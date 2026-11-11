networkanalysisClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "networkanalysisClass",
  inherit = networkanalysisBase,
  private = list(
    
    .network_warning = function(message) {
      notice <- self$results$get("networkWarnings")
      previous <- notice$content
      if (length(previous) != 1 || is.na(previous)) previous <- ""
      notice$setContent(paste(c(previous[nzchar(previous)], message), collapse = "\n"))
      notice$setVisible(TRUE)
    },

    .report_network_groups = function(successful, errors) {
      if (length(errors) > 0) {
        private$.network_warning(paste0("Omitted groups: ", paste(unlist(errors), collapse = "; "), ". Retained groups: ", paste(successful, collapse = ", "), "."))
      }
    },

    .split_network_groups = function(data, variables, grouping) {
      if (nrow(data) == 0) jmvcore::reject("No cases available for network estimation.", code = "no_network_cases")
      if (is.null(grouping) || !nzchar(grouping)) return(list(Overall = data[, variables, drop = FALSE]))
      factor <- as.factor(data[[grouping]])
      if (all(is.na(factor))) jmvcore::reject("No valid grouping values.", code = "no_group_levels")
      split(data[, variables, drop = FALSE], factor, drop = FALSE)
    },

    .populate_network_weights = function(table, graphs, variables, grouping, node_column, enabled) {
      table$deleteRows()
      table$setVisible(isTRUE(enabled))
      if (!isTRUE(enabled)) return()
      for (group_name in names(graphs)) {
        graph <- graphs[[group_name]]
        for (index in seq_along(variables)) {
          values <- list()
          if (!is.null(grouping) && nzchar(grouping)) values$group <- group_name
          values[[node_column]] <- variables[index]
          for (column in seq_along(variables)) values[[paste0("weight_", column)]] <- as.numeric(graph[index, column])
          table$addRow(rowKey = paste(match(group_name, names(graphs)), index, sep = ":"), values = values)
        }
      }
    },

    .node_communities = function(assignments, variables) {
      if (length(assignments) == 0) return(NULL)
      labels <- character()
      assigned <- character()
      groups <- list()
      for (assignment in assignments) {
        label <- trimws(assignment$label)
        nodes <- unlist(assignment$variables, use.names = FALSE)
        if (length(label) != 1 || is.na(label) || !nzchar(label) || length(nodes) == 0) jmvcore::reject("Each node community requires a non-empty name and at least one selected node.", code = "invalid_node_communities")
        if (label %in% labels || anyNA(nodes) || anyDuplicated(nodes) || any(nodes %in% assigned)) jmvcore::reject("Community names must be unique and each node must belong to exactly one community.", code = "invalid_node_communities")
        if (any(!nodes %in% variables)) jmvcore::reject("Community assignments contain variables not selected for this network; update the assignments.", code = "invalid_node_communities")
        groups[[label]] <- match(nodes, variables)
        labels <- c(labels, label)
        assigned <- c(assigned, nodes)
      }
      if (!setequal(assigned, variables)) jmvcore::reject("Assign every selected network node to exactly one community, or clear all assignments to use automatic community detection.", code = "invalid_node_communities")
      groups
    },

    .fit_network = function(data, variables, estimator, correlation) {
      defaults <- c(EBICglasso = "EBICglasso", correlation = "cor", partial_correlation = "pcor")
      if (!estimator %in% names(defaults)) stop("Unsupported network estimator.")
      if (nrow(data) < 5 || length(variables) < 2) stop("Network estimation requires at least five observations and two variables.")
      args <- list(data = data[, variables, drop = FALSE], default = unname(defaults[estimator]), corMethod = if (correlation == "auto") "cor_auto" else correlation, missing = "pairwise", verbose = FALSE)
      recovered <- FALSE
      result <- tryCatch(do.call(bootnet::estimateNetwork, args), error = function(error) {
        if (identical(error$code, "restart")) stop(error)
        if (correlation != "auto" || !grepl("positive.?definite", conditionMessage(error), ignore.case = TRUE)) stop(error)
        args$corArgs <- list(forcePD = TRUE)
        recovered <<- TRUE
        do.call(bootnet::estimateNetwork, args)
      })
      graph <- result$graph
      if (!is.matrix(graph) || !identical(dim(graph), c(length(variables), length(variables))) || any(!is.finite(graph)) || !isSymmetric(unname(graph), tol = 1e-8)) stop("The requested estimator did not return a finite symmetric graph with the selected node dimensions.")
      dimnames(result$graph) <- list(variables, variables)
      attr(result, "pd_recovery") <- recovered
      result
    },

    .resampling_diagnostic = function(group, procedure, requested, completed, failed, status, regularized = 0L) {
      table <- self$results$resamplingDiagnostics_net
      table$addRow(rowKey = table$rowCount + 1L, values = list(group = group, procedure = procedure, requested = as.integer(requested), completed = as.integer(completed), failed = as.integer(failed), unattempted = as.integer(max(0, requested - completed - failed)), regularized = as.integer(regularized), status = status))
    },
    .with_seed = function(seed, code) {
      seed_present <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      previous_seed <- if (seed_present) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
      on.exit({
        if (seed_present) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(list = ".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(seed)
      force(code)
    },

    .getColumnNames = function(table) {
      vapply(table$columns, function(column) column$name, character(1))
    },
    
    .computeCommunitiesAndBridge = function(adjMatrix, nodeLabels, nodeGroupsFromVar, communityAlgo) {
      communities_res <- NULL
      bridge_res <- NULL
      node_to_comm <- NULL
      if (!is.null(nodeGroupsFromVar) && length(nodeGroupsFromVar) > 0) {
        node_to_comm <- setNames(rep(NA_character_, length(nodeLabels)), nodeLabels)
        for (community in names(nodeGroupsFromVar)) node_to_comm[nodeGroupsFromVar[[community]]] <- community
        communities_res <- data.frame(node = nodeLabels, community = unname(node_to_comm), stringsAsFactors = FALSE)
      } else if (communityAlgo != "none") {
        tryCatch({
          g <- igraph::graph_from_adjacency_matrix(abs(adjMatrix), mode = "undirected", weighted = TRUE, diag = FALSE)
          cl <- if (communityAlgo == "louvain") igraph::cluster_louvain(g) else igraph::cluster_walktrap(g)
          mem <- as.character(igraph::membership(cl))
          names(mem) <- nodeLabels
          node_to_comm <- mem
          communities_res <- data.frame(node = nodeLabels, community = paste0("Community ", mem), stringsAsFactors = FALSE)
        }, error = function(e) { private$.network_warning(conditionMessage(e)) })
      }
      comm_for_bridge <- node_to_comm
      if (!is.null(comm_for_bridge) && !anyNA(comm_for_bridge) && length(unique(comm_for_bridge)) >= 2) {
        tryCatch({
          bc <- networktools::bridge(adjMatrix, communities = comm_for_bridge)
          bridge_res <- data.frame(
            node = nodeLabels,
            community = as.character(comm_for_bridge),
            bridgeStrength = as.numeric(bc$`Bridge Strength`),
            bridgeBetweenness = as.numeric(bc$`Bridge Betweenness`),
            bridgeCloseness = as.numeric(bc$`Bridge Closeness`),
            bridgeEI1 = as.numeric(bc$`Bridge Expected Influence (1-step)`),
            stringsAsFactors = FALSE
          )
        }, error = function(e) { private$.network_warning(conditionMessage(e)) })
      }
      return(list(communities = communities_res, bridge = bridge_res, nodeToComm = node_to_comm))
    },

    .sync_weight_columns = function(table, variables, base_columns) {
      column_ids <- paste0("weight_", seq_along(variables))
      for (column in setdiff(private$.getColumnNames(table), c(base_columns, column_ids))) table$getColumn(column)$setVisible(FALSE)
      for (index in seq_along(variables)) {
        table$addColumn(name = column_ids[index], title = variables[index], type = "number")
      }
    },

    .init = function() {
      if (is.null(self$results$get("networkWarnings"))) self$results$add(jmvcore::Notice$new(options = self$options, name = "networkWarnings", title = "Network estimation notices", type = jmvcore::NoticeType$WARNING, visible = FALSE))
      private$.sync_weight_columns(self$results$weights_net, self$options$vars_net, c("group", "node"))
      private$.sync_weight_columns(self$results$weights_mixed, c(self$options$varsCont_mixed, self$options$varsCat_mixed, self$options$varsCount_mixed), c("group", "rowname"))
      self$results$mixedLimitations$setContent("Mixed-data networks (mgm) do not support bootstrap accuracy/stability or network comparison testing (NCT) in this module. Bootstrap and NCT are available only in the continuous-data workflow. Directed causal discovery (DAG) is not implemented in any workflow.")
      
      if (isTRUE(self$options$plotNetwork_net)) {
        width <- self$options$plotWidth_net
        height <- self$options$plotHeight_net
        if (!is.null(self$results$networkPlot_net)) {
          self$results$networkPlot_net$setSize(width, height)
        }
      }
      if (!is.null(self$results$centralityPlot_net)) {
        self$results$centralityPlot_net$setSize(width = 500, height = 800)
      }
      if (!is.null(self$results$clusteringPlot_net)) {
        self$results$clusteringPlot_net$setSize(width = 500, height = 600)
      }
      
      if (isTRUE(self$options$plotNetwork_mixed)) {
        width <- self$options$plotWidth_mixed
        height <- self$options$plotHeight_mixed
        if (!is.null(self$results$networkPlot_mixed)) {
          self$results$networkPlot_mixed$setSize(width, height)
        }
      }
      if (!is.null(self$results$centralityPlot_mixed)) {
        self$results$centralityPlot_mixed$setSize(width = 600, height = 800)
      }
      if (!is.null(self$results$clusteringPlot_mixed)) {
        self$results$clusteringPlot_mixed$setSize(width = 600, height = 800)
      }
      
      if (length(self$options$vars_net) < 3) {
        if (!is.null(self$results$instructions_net)) {
          self$results$instructions_net$setContent("Welcome to Network Analysis for Continuous Data!\n\nPlease select at least 3 variables in the 'Variables' box to begin.")
        }
      }
      if (is.null(self$options$varsCont_mixed) && is.null(self$options$varsCat_mixed) && is.null(self$options$varsCount_mixed)) {
        if (!is.null(self$results$instructions_mixed)) {
          self$results$instructions_mixed$setContent("Welcome to Mixed Data Network Analysis (mgm)!\n\nPlease select at least 3 variables in total (Continuous, Categorical, or Count) to begin.")
        }
      }
    },
    
    .run = function() {
      self$results$get("networkWarnings")$setVisible(FALSE)
      self$results$get("networkWarnings")$setContent("")
      for (name in c("edgeStability_net", "csStability_net", "nctGlobal_net", "nctEdges_net", "resamplingDiagnostics_net")) self$results$get(name)$deleteRows()
      mode <- self$options$mode
      if (mode == "continuous") {
        private$.runContinuous()
      } else if (mode == "mixed") {
        private$.runMixed()
      }
    },
    
    .runContinuous = function() {
      
      if (length(self$options$vars_net) < 3) {
        instructions <- self$results$instructions_net
        instructions$setContent("Welcome to Network Analysis!\n\nPlease select at least 3 variables in the 'Variables' box to begin.")
        instructions$setVisible(TRUE)
        self$results$summary_net$setVisible(FALSE); self$results$weights_net$setVisible(FALSE)
        self$results$centrality_net$setVisible(FALSE); self$results$clustering_net$setVisible(FALSE)
        self$results$networkPlot_net$setVisible(FALSE); self$results$centralityPlot_net$setVisible(FALSE)
        self$results$clusteringPlot_net$setVisible(FALSE)
        self$results$bridge_net$setVisible(FALSE); self$results$communities_net$setVisible(FALSE)
        return()
      } else {
        self$results$instructions_net$setVisible(FALSE)
        self$results$summary_net$setVisible(TRUE)
      }
      
      vars <- self$options$vars_net
      groupVarName <- self$options$groupVar_net
      nodeCommunities <- self$options$nodeCommunities_net
      estimator <- self$options$estimator_net
      corMethod <- self$options$corMethod_net
      
      data_full <- self$data
      columnsToSelect <- vars
      if (!is.null(groupVarName)) {
        columnsToSelect <- c(columnsToSelect, groupVarName)
      }
      
      data_subset <- data_full[, columnsToSelect, drop=FALSE]
      if (nrow(data_subset) == 0) { jmvcore::reject("Dataset is empty.", code = "empty_data"); return() }
      
      for (var in vars) {
        if (!is.numeric(data_subset[[var]])) {
          jmvcore::reject(paste0("Variable '", var, "' is not numeric."), code = "var_not_numeric"); return()
        }
        if (all(is.na(data_subset[[var]]))) {
          jmvcore::reject(paste0("Variable '", var, "' contains only missing values."), code = "var_all_na"); return()
        }
      }
      
      na_note <- NULL
      if (!is.null(groupVarName) && anyNA(data_subset[[groupVarName]])) {
        excluded <- is.na(data_subset[[groupVarName]])
        na_note <- paste(sum(excluded), "cases excluded due to missing grouping values.")
        data_subset <- data_subset[!excluded, , drop = FALSE]
      }
      dataList <- private$.split_network_groups(data_subset, vars, groupVarName)
      groupLevels <- names(dataList)

      qgraph_groups <- private$.node_communities(nodeCommunities, vars)

      estimates <- private$.estimate_continuous_groups(dataList, groupLevels, vars, estimator, corMethod, qgraph_groups, na_note)
      networkGraphsList <- estimates$graphs
      centralityDataList <- estimates$centrality
      clusteringDataList <- estimates$clustering
      bridgeDataList <- estimates$bridge
      communitiesDataList <- estimates$communities
      commMembershipList <- estimates$membership
      estimation_errors <- estimates$errors
      na_note <- estimates$na_note
      nodeLabels <- vars
      successful_groups <- names(networkGraphsList)
      if (length(successful_groups) == 0) {
        jmvcore::reject("Network estimation failed for all groups.", code="all_groups_failed"); return()
      }
      
      private$.report_network_groups(successful_groups, estimation_errors)
      private$.continuous_summary(successful_groups, networkGraphsList, groupVarName, na_note)
      
      private$.populate_network_weights(self$results$weights_net, networkGraphsList, vars, groupVarName, "node", self$options$tableWeights_net)
      
      allCentralityDataList <- private$.continuous_centrality(successful_groups, centralityDataList, groupVarName)
      
      allClusteringDataList <- private$.continuous_clustering(successful_groups, clusteringDataList, groupVarName)

      private$.continuous_bridge(successful_groups, bridgeDataList, groupVarName)

      private$.continuous_communities(successful_groups, communitiesDataList, groupVarName)

      if (isTRUE(self$options$plotNetwork_net) && length(networkGraphsList) > 0) {
        commonLayout <- NULL
        tryCatch({
          graphs_for_layout <- networkGraphsList[successful_groups]
          if (length(graphs_for_layout) > 0) {
            commonLayout <- private$.with_seed(self$options$layoutSeed_net, do.call(qgraph::averageLayout, c(graphs_for_layout, list(layout = self$options$layoutAlgorithm_net))))
            if (!is.null(commonLayout)) {
              rownames(commonLayout) <- nodeLabels
            }
          }
        }, error = function(e) {
          jmvcore::reject(paste("Layout calculation failed:", e$message), code="layout_error")
        })

        final_node_groups <- qgraph_groups
        if (is.null(final_node_groups) && length(commMembershipList) > 0) {
          first_mem <- commMembershipList[[1]]
          if (!is.null(first_mem)) {
            final_node_groups <- list()
            for (u_c in unique(first_mem)) {
              final_node_groups[[paste0("Comm ", u_c)]] <- which(first_mem == u_c)
            }
          }
        }
        plotState <- list(
          graphs = networkGraphsList,
          layoutSeed = self$options$layoutSeed_net,
          layout = commonLayout,
          labels = if(isTRUE(self$options$showLabels_net)) nodeLabels else FALSE,
          groupNames = successful_groups,
          nodeGroups = final_node_groups
        )
        self$results$networkPlot_net$setState(plotState)
      }
      
      if (isTRUE(self$options$plotCentrality_net) && length(allCentralityDataList) > 0) {
        combinedCentralityData <- do.call(rbind, c(allCentralityDataList, make.row.names = FALSE))
        if (!is.null(combinedCentralityData) && !"group" %in% names(combinedCentralityData)) {
          combinedCentralityData$group <- "Overall"
        }
        self$results$centralityPlot_net$setState(combinedCentralityData)
      }
      
      if (isTRUE(self$options$plotClustering_net) && length(allClusteringDataList) > 0) {
        combinedClusteringData <- do.call(rbind, c(allClusteringDataList, make.row.names = FALSE))
        if (!is.null(combinedClusteringData) && !"group" %in% names(combinedClusteringData)) {
          combinedClusteringData$group <- "Overall"
        }
        self$results$clusteringPlot_net$setState(combinedClusteringData)
      }
      
      if (isTRUE(self$options$bootStability_net)) {
        for (group_index in seq_along(successful_groups)) {
          group <- successful_groups[group_index]
          private$.runBootStability(dataList[[group]], vars, estimator, corMethod, self$options$bootN_net, self$results$edgeStability_net, self$results$csStability_net, group, group_index)
        }
      }

      if (isTRUE(self$options$nctTest_net)) {
        if (length(successful_groups) == 2) {
          d_g1 <- dataList[[successful_groups[1]]]
          d_g2 <- dataList[[successful_groups[2]]]
          private$.runNCT(
            d_g1, d_g2, vars, estimator, corMethod,
            self$options$nctPermutations_net,
            self$results$nctGlobal_net,
            self$results$nctEdges_net,
            groups = successful_groups
          )
        } else {
          private$.network_warning("Network Comparison Test (NCT) was not run: it requires exactly 2 successfully estimated groups.")
        }
      }
      

    },
    
    .runMixed = function() {
      
      if (length(self$options$varsCont_mixed) == 0 && length(self$options$varsCat_mixed) == 0 && length(self$options$varsCount_mixed) == 0) {
        instructions <- self$results$instructions_mixed
        instructions$setContent("Welcome to Mixed Data Network Analysis (mgm)!\n\nPlease select at least 3 variables in total (Continuous, Categorical, or Count) to begin.")
        instructions$setVisible(TRUE)
        self$results$summary_mixed$setVisible(FALSE); self$results$weights_mixed$setVisible(FALSE)
        self$results$centrality_mixed$setVisible(FALSE); self$results$clustering_mixed$setVisible(FALSE)
        self$results$networkPlot_mixed$setVisible(FALSE); self$results$centralityPlot_mixed$setVisible(FALSE)
        self$results$clusteringPlot_mixed$setVisible(FALSE)
        self$results$bridge_mixed$setVisible(FALSE); self$results$communities_mixed$setVisible(FALSE)
        return()
      } else {
        self$results$instructions_mixed$setVisible(FALSE)
        self$results$summary_mixed$setVisible(TRUE)
        self$results$weights_mixed$setVisible(isTRUE(self$options$tableWeights_mixed))
        self$results$centrality_mixed$setVisible(isTRUE(self$options$tableCentrality_mixed))
        self$results$clustering_mixed$setVisible(isTRUE(self$options$tableClustering_mixed))
        self$results$networkPlot_mixed$setVisible(isTRUE(self$options$plotNetwork_mixed))
        self$results$centralityPlot_mixed$setVisible(isTRUE(self$options$plotCentrality_mixed))
        self$results$clusteringPlot_mixed$setVisible(isTRUE(self$options$plotClustering_mixed))
      }
      
      varsContNames <- self$options$varsCont_mixed
      varsCatNames <- self$options$varsCat_mixed
      varsCountNames <- self$options$varsCount_mixed
      groupVarName <- self$options$groupVar_mixed
      nodeCommunities <- self$options$nodeCommunities_mixed
      
      allVarNames <- c(varsContNames, varsCatNames, varsCountNames)
      if (length(allVarNames) < 3) {
        jmvcore::reject("Please select at least 3 variables in total for Mixed Network estimation (mgm).", code = "not_enough_variables"); return()
      }
      
      lambdaSelMethod <- self$options$lambdaSelMethod_mixed
      kFolds <- self$options$kFolds_mixed
      lambdaGam <- self$options$lambdaGam_mixed
      ruleReg <- self$options$ruleReg_mixed
      
      data_full <- self$data
      columnsToSelect <- allVarNames
      if (!is.null(groupVarName) && nchar(groupVarName) > 0) {
        columnsToSelect <- unique(c(allVarNames, groupVarName))
      }
      
      data_subset <- data_full[, columnsToSelect, drop=FALSE]
      if (nrow(data_subset) == 0) {
        jmvcore::reject("Dataset is empty after selecting variables.", code = "empty_data_subset"); return()
      }
      
      varTypes <- character(length(allVarNames))
      varLevels <- numeric(length(allVarNames))
      for (i in seq_along(allVarNames)) {
        varName <- allVarNames[i]
        if (all(is.na(data_subset[[varName]]))) {
          jmvcore::reject(paste0("Variable '", varName, "' contains only missing values."), code = "var_all_na"); return()
        }
        if (varName %in% varsContNames) {
          varTypes[i] <- "g"
          varLevels[i] <- 1
          if (!is.numeric(data_subset[[varName]])) {
            jmvcore::reject(paste0("Continuous variable '", varName, "' is not numeric."), code = "cont_not_numeric"); return()
          }
        } else if (varName %in% varsCatNames) {
          varTypes[i] <- "c"
          if (!is.factor(data_subset[[varName]])) {
            data_subset[[varName]] <- as.factor(data_subset[[varName]])
          }
          varLevels[i] <- nlevels(data_subset[[varName]])
          if (varLevels[i] < 2) {
            jmvcore::reject(paste0("Categorical variable '", varName, "' must have at least 2 levels."), code = "cat_not_enough_levels"); return()
          }
        } else if (varName %in% varsCountNames) {
          varTypes[i] <- "p"
          varLevels[i] <- 1
          if (!is.numeric(data_subset[[varName]])) {
            jmvcore::reject(paste0("Count variable '", varName, "' is not numeric."), code = "count_not_numeric"); return()
          }
          if (any(data_subset[[varName]] < 0 | data_subset[[varName]] != round(data_subset[[varName]]), na.rm = TRUE)) {
            jmvcore::reject(paste0("Count variable '", varName, "' must contain non-negative integers."), code = "count_invalid_values"); return()
          }
        }
      }
      
      na_note_global <- NULL
      n_total_initial <- nrow(data_subset)
      cols_for_na_omit <- allVarNames
      if (!is.null(groupVarName) && nchar(groupVarName) > 0) {
        cols_for_na_omit <- c(allVarNames, groupVarName)
      }
      data_subset_for_na <- data_subset[, cols_for_na_omit, drop = FALSE]
      complete_cases_indices <- stats::complete.cases(data_subset_for_na)
      data_subset <- data_subset[complete_cases_indices, , drop = FALSE]
      n_after_na_global <- nrow(data_subset)
      
      if (n_after_na_global < n_total_initial) {
        na_note_global <- paste(n_total_initial - n_after_na_global, "cases excluded due to missing values (listwise deletion based on selected variables and group variable).")
      }
      if (nrow(data_subset) < 10) {
        jmvcore::reject("Insufficient data after listwise deletion of missing values (n < 10).", code = "insufficient_data_after_na"); return()
      }
      
      dataList <- private$.split_network_groups(data_subset, allVarNames, groupVarName)
      groupLevels <- names(dataList)

      qgraph_groups <- private$.node_communities(nodeCommunities, allVarNames)

      if (!requireNamespace("mgm", quietly = TRUE)) { jmvcore::reject("Package 'mgm' is required. Please install it.", code = "mgm_missing"); return() }
      if (!requireNamespace("qgraph", quietly = TRUE)) { jmvcore::reject("Package 'qgraph' is required. Please install it.", code = "qgraph_missing"); return() }
      if (!requireNamespace("igraph", quietly = TRUE)) { jmvcore::reject("Package 'igraph' is required. Please install it.", code = "igraph_missing"); return() }
      if (!requireNamespace("dplyr", quietly = TRUE)) { jmvcore::reject("Package 'dplyr' is required. Please install it.", code = "dplyr_missing"); return() }
      if (!requireNamespace("gtools", quietly = TRUE)) { jmvcore::reject("Package 'gtools' is required. Please install it.", code = "gtools_missing"); return() }
      if (!requireNamespace("reshape2", quietly = TRUE)) { jmvcore::reject("Package 'reshape2' is required. Please install it.", code = "reshape2_missing"); return() }

      estimates <- private$.estimate_mixed_groups(dataList, groupLevels, allVarNames, varsCatNames, varTypes, varLevels, lambdaSelMethod, ruleReg, kFolds, lambdaGam, qgraph_groups)
      networkResultsList <- estimates$results
      weightedAdjMatrices <- estimates$graphs
      centralityDataList <- estimates$centrality
      clusteringDataList <- estimates$clustering
      bridgeDataList <- estimates$bridge
      communitiesDataList <- estimates$communities
      commMembershipList <- estimates$membership
      estimation_errors <- estimates$errors
      if (length(estimation_errors) > 0) {
        if (length(weightedAdjMatrices) == 0) {
          jmvcore::reject(paste("Network estimation failed for all groups. Summary of errors:\n", paste(estimation_errors, collapse="\n")), code="all_groups_failed_with_errors"); return()
        }
      }
      successful_groups <- names(weightedAdjMatrices)
      if (length(successful_groups) == 0) {
        jmvcore::reject("No successful network estimations to report.", code="no_successful_groups"); return()
      }

      private$.report_network_groups(successful_groups, estimation_errors)
      private$.mixed_summary(successful_groups, weightedAdjMatrices, groupVarName, groupLevels, na_note_global)

      private$.populate_network_weights(self$results$weights_mixed, weightedAdjMatrices, allVarNames, groupVarName, "rowname", self$options$tableWeights_mixed)

      allCentralityDataForPlot <- private$.mixed_centrality(successful_groups, centralityDataList, groupVarName, groupLevels)

      allClusteringDataForPlot <- private$.mixed_clustering(successful_groups, clusteringDataList, groupVarName, groupLevels)

      private$.mixed_bridge(successful_groups, bridgeDataList, groupVarName)

      private$.mixed_communities(successful_groups, communitiesDataList, groupVarName)

      if (isTRUE(self$options$plotNetwork_mixed) && length(weightedAdjMatrices) > 0) {
        commonLayout <- NULL
        graphs_for_layout <- weightedAdjMatrices[successful_groups]
        if (length(graphs_for_layout) > 1) {
          tryCatch({
            commonLayout <- private$.with_seed(self$options$layoutSeed_mixed, do.call(qgraph::averageLayout, c(graphs_for_layout, list(layout = self$options$layoutAlgorithm_mixed))))
            if (!is.null(commonLayout) && !is.null(allVarNames) && length(allVarNames) == nrow(commonLayout)) {
              rownames(commonLayout) <- allVarNames
            }
          }, error = function(e) {
            commonLayout <- NULL
          })
        }
        if (is.null(commonLayout) && length(graphs_for_layout) > 0) {
          first_graph_name <- names(graphs_for_layout)[1]
          tryCatch({
            q_obj <- private$.with_seed(self$options$layoutSeed_mixed, qgraph::qgraph(graphs_for_layout[[first_graph_name]], layout = self$options$layoutAlgorithm_mixed, DoNotPlot = TRUE))
            commonLayout <- q_obj$layout
            if (!is.null(commonLayout) && !is.null(allVarNames) && length(allVarNames) == nrow(commonLayout)) {
              rownames(commonLayout) <- allVarNames
            }
          }, error = function(e) {
            tryCatch({
              q_obj_circ <- qgraph::qgraph(graphs_for_layout[[first_graph_name]], layout = "circle", DoNotPlot = TRUE)
              commonLayout <- q_obj_circ$layout
              if (!is.null(commonLayout) && !is.null(allVarNames) && length(allVarNames) == nrow(commonLayout)) {
                rownames(commonLayout) <- allVarNames
              }
            }, error = function(e2) {
              commonLayout <- NULL
            })
          })
        }

        final_node_groups_mixed <- qgraph_groups
        if (is.null(final_node_groups_mixed) && length(commMembershipList) > 0) {
          first_mem_m <- commMembershipList[[1]]
          if (!is.null(first_mem_m)) {
            final_node_groups_mixed <- list()
            for (u_c in unique(first_mem_m)) {
              final_node_groups_mixed[[paste0("Comm ", u_c)]] <- which(first_mem_m == u_c)
            }
          }
        }
        plotStateNetwork <- list(
          graphs = graphs_for_layout,
          layoutSeed = self$options$layoutSeed_mixed,
          edgeColors = lapply(networkResultsList[successful_groups], function(fit) if(!is.null(fit) && !is.null(fit$pairwise) && !is.null(fit$pairwise$edgecolor_cb)) fit$pairwise$edgecolor_cb else if(!is.null(fit) && !is.null(fit$pairwise)) fit$pairwise$edgecolor else NULL),
          layout = commonLayout,
          labels = if(isTRUE(self$options$showLabels_mixed)) allVarNames else FALSE,
          groupNames = successful_groups,
          layoutAlgorithm = self$options$layoutAlgorithm_mixed,
          nodeGroups = final_node_groups_mixed
        )
        self$results$networkPlot_mixed$setState(plotStateNetwork)
      }

      if (isTRUE(self$options$plotCentrality_mixed) && length(allCentralityDataForPlot) > 0) {
        valid_cent_data_for_plot <- Filter(Negate(is.null), allCentralityDataForPlot)
        if (length(valid_cent_data_for_plot) > 0) {
          combinedCentralityData <- do.call(rbind, c(valid_cent_data_for_plot, make.row.names = FALSE))
          if (!is.null(combinedCentralityData) && nrow(combinedCentralityData) > 0) {
            self$results$centralityPlot_mixed$setState(combinedCentralityData)
          }
        }
      }

      if (isTRUE(self$options$plotClustering_mixed) && length(allClusteringDataForPlot) > 0) {
        valid_clust_data_for_plot <- Filter(Negate(is.null), allClusteringDataForPlot)
        if (length(valid_clust_data_for_plot) > 0) {
          combinedClusteringData <- do.call(rbind, c(valid_clust_data_for_plot, make.row.names = FALSE))
          if (!is.null(combinedClusteringData) && nrow(combinedClusteringData) > 0) {
            self$results$clusteringPlot_mixed$setState(combinedClusteringData)
          }
        }
      }
    },

    .estimate_continuous_groups = function(dataList, groupLevels, vars, estimator, corMethod, qgraph_groups, na_note) {
      networkGraphsList <- list()
      centralityDataList <- list()
      clusteringDataList <- list()
      bridgeDataList <- list()
      communitiesDataList <- list()
      commMembershipList <- list()
      estimation_errors <- list()

      if (!requireNamespace("bootnet", quietly = TRUE)) { jmvcore::reject("Package 'bootnet' required.", code = "bootnet_missing"); return() }
      if (!requireNamespace("qgraph", quietly = TRUE)) { jmvcore::reject("Package 'qgraph' required.", code = "qgraph_missing"); return() }
      

      for (groupName in groupLevels) {
        private$.checkpoint(flush = FALSE)
        currentData <- dataList[[groupName]]
        if (nrow(currentData) < 10) { estimation_errors[[groupName]] <- paste0("Group \"", groupName, "\": Insufficient data (n=", nrow(currentData), ", required >=10)"); next }
        networkGraph <- NULL; nodeLabels <- vars
        tryCatch({
          networkResults <- private$.fit_network(currentData, vars, estimator, corMethod)
          if (isTRUE(attr(networkResults, "pd_recovery"))) {
            recovery_note <- paste0("Auto-correlation positive-definite adjustment for group \"", groupName, "\": qgraph forcePD applied to the raw-data correlation calculation; requested estimator and original sample size retained.")
            na_note <- paste(c(na_note, recovery_note), collapse = " ")
          }
          if (!is.null(networkResults) && "graph" %in% names(networkResults)) {
            networkGraph <- networkResults$graph
            if(nrow(networkGraph) != length(nodeLabels) || ncol(networkGraph) != length(nodeLabels)) { stop("Estimated graph dimensions mismatch.") }
            colnames(networkGraph) <- nodeLabels; rownames(networkGraph) <- nodeLabels
          } else { stop("Could not extract graph matrix.") }
          networkGraphsList[[groupName]] <- networkGraph
          
          cent <- qgraph::centrality(networkGraph); manual_ei <- rowSums(networkGraph, na.rm = TRUE)
          centralityDataRaw <- data.frame(variable = nodeLabels, strength = cent$OutDegree, closeness = cent$Closeness, betweenness = cent$Betweenness / 2, expectedInfluence = manual_ei)
          rownames(centralityDataRaw) <- NULL; centralityDataNorm <- centralityDataRaw
          cols_to_norm_cent <- c("strength", "closeness", "betweenness", "expectedInfluence")
          for (col in cols_to_norm_cent) {
            if (col %in% names(centralityDataNorm)) {
              valid_vals <- centralityDataNorm[[col]][is.finite(centralityDataNorm[[col]])]
              if (length(valid_vals) > 1) {
                mean_val <- mean(valid_vals)
                sd_val <- stats::sd(valid_vals)
                if (!is.na(sd_val) && sd_val > 1e-10) {
                  finite_indices <- which(is.finite(centralityDataNorm[[col]]))
                  centralityDataNorm[[col]][finite_indices] <- (centralityDataNorm[[col]][finite_indices] - mean_val) / sd_val
                } else {
                  finite_indices <- which(is.finite(centralityDataNorm[[col]]))
                  if (length(unique(valid_vals)) == 1) {
                    centralityDataNorm[[col]][finite_indices] <- 0
                  } else {
                    centralityDataNorm[[col]][finite_indices] <- centralityDataNorm[[col]][finite_indices] - mean_val
                  }
                }
              } else if (length(valid_vals) == 1) {
                finite_indices <- which(is.finite(centralityDataNorm[[col]]))
                centralityDataNorm[[col]][finite_indices] <- 0
              }
            }
          }
          centralityDataList[[groupName]] <- centralityDataNorm

          clusteringDataNorm <- NULL
          tryCatch({
            clust_res <- qgraph::clustcoef_auto(networkGraph)
            get_clust <- function(name) {
              if (name %in% names(clust_res)) return(as.numeric(clust_res[[name]]))
              if (paste0("clust", name) %in% names(clust_res)) return(as.numeric(clust_res[[paste0("clust", name)]]))
              return(rep(NA_real_, length(nodeLabels)))
            }
            clusteringDataRaw <- data.frame(
              node = nodeLabels,
              Barrat = get_clust("Barrat"),
              Onnela = get_clust("Onnela"),
              WS = get_clust("WS"),
              Zhang = get_clust("Zhang")
            )
            rownames(clusteringDataRaw) <- NULL
            clusteringDataNorm <- clusteringDataRaw
            cols_to_norm_clust <- c("Barrat", "Onnela", "WS", "Zhang")
            for (col in cols_to_norm_clust) {
              if (col %in% names(clusteringDataNorm)) {
                valid_vals <- clusteringDataNorm[[col]][is.finite(clusteringDataNorm[[col]])]
                if (length(valid_vals) > 1) {
                  mean_val <- mean(valid_vals)
                  sd_val <- stats::sd(valid_vals)
                  if (!is.na(sd_val) && sd_val > 1e-10) {
                    finite_indices <- which(is.finite(clusteringDataNorm[[col]]))
                    clusteringDataNorm[[col]][finite_indices] <- (clusteringDataNorm[[col]][finite_indices] - mean_val) / sd_val
                  } else {
                    finite_indices <- which(is.finite(clusteringDataNorm[[col]]))
                    if (length(unique(valid_vals)) == 1) {
                      clusteringDataNorm[[col]][finite_indices] <- 0
                    } else {
                      clusteringDataNorm[[col]][finite_indices] <- clusteringDataNorm[[col]][finite_indices] - mean_val
                    }
                  }
                } else if (length(valid_vals) == 1) {
                  finite_indices <- which(is.finite(clusteringDataNorm[[col]]))
                  clusteringDataNorm[[col]][finite_indices] <- 0
                }
              }
            }
          }, error = function(e) { private$.network_warning(conditionMessage(e)) })
          clusteringDataList[[groupName]] <- clusteringDataNorm

          commAlgo <- self$options$communityAlgorithm_net
          cb_res <- private$.computeCommunitiesAndBridge(networkGraph, nodeLabels, qgraph_groups, commAlgo)
          bridgeDataList[[groupName]] <- cb_res$bridge
          communitiesDataList[[groupName]] <- cb_res$communities
          if (!is.null(cb_res$nodeToComm)) {
            commMembershipList[[groupName]] <- cb_res$nodeToComm
          }

        }, error = function(e) {
          err_msg <- paste("Group", groupName, "-", e$message)
          if (identical(e$code, "restart")) stop(e)
          if (grepl("matrix is not positive definite", e$message, ignore.case = TRUE)) {
            err_msg <- paste(err_msg, "(Pairwise deletion issue?)")
          }
          estimation_errors[[groupName]] <<- err_msg
          networkGraphsList[[groupName]] <<- NULL; centralityDataList[[groupName]] <<- NULL; clusteringDataList[[groupName]] <<- NULL
        })
      }
      list(graphs = networkGraphsList, centrality = centralityDataList, clustering = clusteringDataList, bridge = bridgeDataList, communities = communitiesDataList, membership = commMembershipList, errors = estimation_errors, na_note = na_note)
    },

    .estimate_mixed_groups = function(dataList, groupLevels, allVarNames, varsCatNames, varTypes, varLevels, lambdaSelMethod, ruleReg, kFolds, lambdaGam, qgraph_groups) {
      networkResultsList <- list(); weightedAdjMatrices <- list(); centralityDataList <- list(); clusteringDataList <- list(); bridgeDataList <- list(); communitiesDataList <- list(); commMembershipList <- list(); estimation_errors <- character()
      
      for (groupName in groupLevels) {
        private$.checkpoint(flush = FALSE)
        currentDataGroup_for_estimation <- dataList[[groupName]][, allVarNames, drop=FALSE]
        min_n_group <- max(length(allVarNames) * 2, 20)
        if (nrow(currentDataGroup_for_estimation) < min_n_group) {
          msg <- paste0("Group '", groupName, "': Insufficient data (n=", nrow(currentDataGroup_for_estimation), ", required >=", min_n_group, ") for stable estimation.")
          estimation_errors <- c(estimation_errors, msg)
          next
        }
        
        mgmFit <- NULL; adjMatrix <- NULL
        tryCatch({
        mat_for_mgm <- matrix(0, nrow = nrow(currentDataGroup_for_estimation), ncol = length(allVarNames))
        colnames(mat_for_mgm) <- allVarNames
        for (v_idx in seq_along(allVarNames)) {
          v_name <- allVarNames[v_idx]
          if (v_name %in% varsCatNames) {
            fac_col <- currentDataGroup_for_estimation[[v_name]]
            if (!is.factor(fac_col)) fac_col <- as.factor(fac_col)
            mat_for_mgm[, v_idx] <- as.integer(fac_col) - 1L
          } else {
            mat_for_mgm[, v_idx] <- as.numeric(currentDataGroup_for_estimation[[v_name]])
          }
        }
        mgm_args <- list(data = mat_for_mgm, type = varTypes, level = varLevels, 
                         lambdaSel = lambdaSelMethod, ruleReg = ruleReg, 
                         binarySign = FALSE, pbar = FALSE, warnings = TRUE)
          if (lambdaSelMethod == "CV") {
            mgm_args$k <- kFolds; mgm_args$lambdaGam <- NULL
          } else if (lambdaSelMethod == "EBIC") {
            mgm_args$lambdaGam <- lambdaGam; mgm_args$k <- NULL
          }
          
          if (!is.null(self$results$instructions_mixed)) {
            self$results$instructions_mixed$setContent(paste0("Estimating network for group: ", groupName, "..."))
            self$results$instructions_mixed$setVisible(TRUE)
          }
          mgmFit <- do.call(mgm::mgm, mgm_args)
          if (!is.null(self$results$instructions_mixed)) self$results$instructions_mixed$setVisible(FALSE)
          
          if (is.null(mgmFit) || !("pairwise" %in% names(mgmFit)) || 
              is.null(mgmFit$pairwise$wadj) || is.null(mgmFit$pairwise$signs)) {
            stop("mgm estimation did not return a valid 'pairwise$wadj' or 'pairwise$signs' object.")
          }
          
          adjMatrix <- mgmFit$pairwise$wadj
          if(is.null(adjMatrix) || !is.matrix(adjMatrix) || nrow(adjMatrix) != length(allVarNames) || ncol(adjMatrix) != length(allVarNames)) {
            stop("Adjacency matrix (wadj * signs) is invalid or has wrong dimensions.")
          }
          colnames(adjMatrix) <- allVarNames; rownames(adjMatrix) <- allVarNames
          
          adjMatrix <- (adjMatrix + t(adjMatrix)) / 2
          adjMatrix[lower.tri(adjMatrix)] <- t(adjMatrix)[lower.tri(adjMatrix)]
          
          if (any(!is.finite(adjMatrix))) {
            adjMatrix[!is.finite(adjMatrix)] <- 0
          }
          
          networkResultsList[[groupName]] <- mgmFit
          weightedAdjMatrices[[groupName]] <- adjMatrix
          
          num_vars <- length(allVarNames)
          strength_vals <- rep(NA_real_, num_vars); closeness_vals <- rep(NA_real_, num_vars); betweenness_vals <- rep(NA_real_, num_vars); ei_vals_final <- rep(NA_real_, num_vars)
          cent_obj_qgraph <- NULL
          tryCatch({
            cent_obj_qgraph <- qgraph::centrality(adjMatrix, weighted = TRUE, signed = TRUE, all.shortest.paths = FALSE)
          }, error = function(e_cent) {
            private$.network_warning(paste("Centrality for group", groupName, ":", conditionMessage(e_cent)))
            na_vec <- rep(NA_real_, num_vars)
            cent_obj_qgraph <<- list( Betweenness = na_vec, Closeness = na_vec, OutDegree = na_vec )
          })
          if (!is.null(cent_obj_qgraph)) {
            if (!is.null(cent_obj_qgraph$Betweenness) && length(cent_obj_qgraph$Betweenness) == num_vars) {
              betweenness_vals <- cent_obj_qgraph$Betweenness
            }
            if (!is.null(cent_obj_qgraph$Closeness) && length(cent_obj_qgraph$Closeness) == num_vars) {
              closeness_vals <- cent_obj_qgraph$Closeness; closeness_vals[is.infinite(closeness_vals)] <- NA_real_
            }
            if (!is.null(cent_obj_qgraph$OutDegree) && length(cent_obj_qgraph$OutDegree) == num_vars) {
              strength_vals <- cent_obj_qgraph$OutDegree
            }
          }
          signedAdjMatrix <- adjMatrix
          if (!is.null(mgmFit$pairwise$signs)) {
            has_signs <- is.finite(mgmFit$pairwise$signs)
            signedAdjMatrix[has_signs] <- adjMatrix[has_signs] * mgmFit$pairwise$signs[has_signs]
          }
          temp_ei <- rowSums(signedAdjMatrix, na.rm = TRUE)
          if (length(temp_ei) == num_vars) {
            ei_vals_final <- temp_ei
          }
          
          centralityDataRaw <- data.frame( variable = allVarNames, strength = strength_vals, closeness = closeness_vals, betweenness = betweenness_vals, expectedInfluence = ei_vals_final )
          centralityDataList[[groupName]] <- centralityDataRaw
          
          tryCatch({
            clust_res <- qgraph::clustcoef_auto(adjMatrix)
            get_clust <- function(name) {
              if (name %in% names(clust_res)) return(as.numeric(clust_res[[name]]))
              if (paste0("clust", name) %in% names(clust_res)) return(as.numeric(clust_res[[paste0("clust", name)]]))
              return(rep(NA_real_, num_vars))
            }
            clusteringDataRaw <- data.frame(
              node = allVarNames,
              Barrat = get_clust("Barrat"),
              Onnela = get_clust("Onnela"),
              WS = get_clust("WS"),
              Zhang = get_clust("Zhang")
            )
            rownames(clusteringDataRaw) <- NULL
            clusteringDataList[[groupName]] <- clusteringDataRaw
          }, error = function(e) { private$.network_warning(conditionMessage(e)) })

          commAlgo_mixed <- self$options$communityAlgorithm_mixed
          cb_res_mixed <- private$.computeCommunitiesAndBridge(adjMatrix, allVarNames, qgraph_groups, commAlgo_mixed)
          bridgeDataList[[groupName]] <- cb_res_mixed$bridge
          communitiesDataList[[groupName]] <- cb_res_mixed$communities
          if (!is.null(cb_res_mixed$nodeToComm)) {
            commMembershipList[[groupName]] <- cb_res_mixed$nodeToComm
          }

        }, error = function(e) {
          err_msg <- paste0("Group '", groupName, "': Error during estimation - ", e$message)
          estimation_errors <<- c(estimation_errors, err_msg)
          networkResultsList[[groupName]] <<- NULL; weightedAdjMatrices[[groupName]] <<- NULL; centralityDataList[[groupName]] <<- NULL; clusteringDataList[[groupName]] <<- NULL
          if (!is.null(self$results$instructions_mixed)) self$results$instructions_mixed$setVisible(FALSE)
        })
      }
      list(results = networkResultsList, graphs = weightedAdjMatrices, centrality = centralityDataList, clustering = clusteringDataList, bridge = bridgeDataList, communities = communitiesDataList, membership = commMembershipList, errors = estimation_errors)
    },

    .continuous_summary = function(successful_groups, networkGraphsList, groupVarName, na_note) {
      summaryTable <- self$results$summary_net
      summaryTable$setState(NULL)
      summaryTable$deleteRows()
      summaryTable$setNote("na_handling", NULL)
      if (!is.null(na_note)) {
        summaryTable$setNote("na_handling", na_note)
      }

      for (groupName in successful_groups) {
        networkGraph <- networkGraphsList[[groupName]]
        tryCatch({
          nNodes <- ncol(networkGraph)
          adjMatrix <- networkGraph
          adjMatrix[is.na(adjMatrix)] <- 0
          nonZeroEdges <- sum(abs(adjMatrix[upper.tri(adjMatrix)]) > 1e-10)
          possibleEdges <- nNodes * (nNodes - 1) / 2
          sparsity <- if(possibleEdges > 0) 1 - (nonZeroEdges / possibleEdges) else 1
          summaryRow <- list(nodes = nNodes, edges = paste(nonZeroEdges, "/", possibleEdges), sparsity = sparsity)
          if (!is.null(groupVarName)) {
            summaryRow <- c(list(group = groupName), summaryRow)
          }
          summaryTable$addRow(rowKey = groupName, values = summaryRow)
        }, error = function(e) {
          jmvcore::reject(paste0("Failed to populate summary row for group ", groupName, ": ", e$message), code="summary_group_error")
        })
      }
    },

    .continuous_centrality = function(successful_groups, centralityDataList, groupVarName) {
      centralityTable <- self$results$centrality_net
      centralityTable$setState(NULL)
      centralityTable$deleteRows()
      centralityTable$setNote("norm_z", NULL)
      allCentralityDataList <- list()
      data_found_for_cent_table = FALSE

      if (isTRUE(self$options$tableCentrality_net) || isTRUE(self$options$plotCentrality_net)) {
        for (groupName in successful_groups) {
          centralityData <- centralityDataList[[groupName]]
          if (!is.null(centralityData)) {
            data_found_for_cent_table = TRUE
            if (isTRUE(self$options$tableCentrality_net) && nrow(centralityData) > 0) {
              for (i in 1:nrow(centralityData)) {
                row_key <- paste(groupName, i, sep="_")
                row_values <- list(
                  variable = centralityData$variable[i],
                  strength = centralityData$strength[i],
                  closeness = centralityData$closeness[i],
                  betweenness = centralityData$betweenness[i],
                  expectedInfluence = centralityData$expectedInfluence[i]
                )
                if (!is.null(groupVarName)) {
                  row_values <- c(list(group = groupName), row_values)
                }
                centralityTable$addRow(rowKey = row_key, values = row_values)
              }
            }
            if (!is.null(groupVarName)) {
              centralityData$group <- groupName
            }
            allCentralityDataList[[groupName]] <- centralityData
          }
        }
        if (data_found_for_cent_table) {
          centralityTable$setNote("norm_z", "Centrality values are z-score normalized.")
        }
      } else {
        centralityTable$setState(NULL)
      }
      allCentralityDataList
    },

    .continuous_clustering = function(successful_groups, clusteringDataList, groupVarName) {
      clusteringTable <- self$results$clustering_net
      clusteringTable$setState(NULL)
      clusteringTable$deleteRows()
      clusteringTable$setNote("clust_norm_z", NULL)
      allClusteringDataList <- list()
      data_found_for_clust_table = FALSE

      if (isTRUE(self$options$tableClustering_net) || isTRUE(self$options$plotClustering_net)) {
        for (groupName in successful_groups) {
          clusteringData <- clusteringDataList[[groupName]]
          if (!is.null(clusteringData)) {
            data_found_for_clust_table = TRUE
            if (isTRUE(self$options$tableClustering_net) && nrow(clusteringData) > 0) {
              for (i in 1:nrow(clusteringData)) {
                row_node_name <- clusteringData$node[i]
                row_key <- paste(groupName, row_node_name, sep="_")
                row_values <- list(
                  node = clusteringData$node[i],
                  Barrat = clusteringData$Barrat[i],
                  Onnela = clusteringData$Onnela[i],
                  WS = clusteringData$WS[i],
                  Zhang = clusteringData$Zhang[i]
                )
                if (!is.null(groupVarName)) {
                  row_values <- c(list(group = groupName), row_values)
                }
                clusteringTable$addRow(rowKey = row_key, values = row_values)
              }
            }
            if (!is.null(groupVarName)) {
              clusteringData$group <- groupName
            }
            allClusteringDataList[[groupName]] <- clusteringData
          }
        }
        if (data_found_for_clust_table) {
          clusteringTable$setNote("clust_norm_z", "Clustering values are z-score normalized.")
        }
      } else {
        clusteringTable$setState(NULL)
      }
      allClusteringDataList
    },

    .continuous_bridge = function(successful_groups, bridgeDataList, groupVarName) {
      bridgeTable <- self$results$bridge_net
      bridgeTable$setState(NULL)
      bridgeTable$deleteRows()
      bridgeTable$setNote("bridge_unavailable", NULL)
      if (isTRUE(self$options$tableBridge_net)) {
        unavailable_groups <- successful_groups[vapply(successful_groups, function(groupName) is.null(bridgeDataList[[groupName]]) || nrow(bridgeDataList[[groupName]]) == 0, logical(1))]
        if (length(unavailable_groups) > 0) bridgeTable$setNote("bridge_unavailable", paste0("Bridge centrality unavailable for: ", paste(unavailable_groups, collapse = ", "), ". It requires a complete assignment of nodes to at least two communities (node groups or detected communities) and a successful networktools calculation."))
        for (groupName in successful_groups) {
          bData <- bridgeDataList[[groupName]]
          if (!is.null(bData) && nrow(bData) > 0) {
            for (i in seq_len(nrow(bData))) {
              row_key <- paste(groupName, bData$node[i], sep="_")
              row_vals <- list(
                node = bData$node[i],
                community = bData$community[i],
                bridgeStrength = bData$bridgeStrength[i],
                bridgeBetweenness = bData$bridgeBetweenness[i],
                bridgeCloseness = bData$bridgeCloseness[i],
                bridgeEI1 = bData$bridgeEI1[i]
              )
              if (!is.null(groupVarName)) row_vals <- c(list(group = groupName), row_vals)
              bridgeTable$addRow(rowKey = row_key, values = row_vals)
            }
          }
        }
      }
    },

    .continuous_communities = function(successful_groups, communitiesDataList, groupVarName) {
      commTable <- self$results$communities_net
      commTable$setState(NULL)
      commTable$deleteRows()
      commTable$setNote("source", "Complete manual assignments take precedence over community detection and are shared by membership, bridge centrality and network colors; without an assignment, the selected detection algorithm is used.")
      if (isTRUE(self$options$tableCommunities_net)) {
        for (groupName in successful_groups) {
          cData <- communitiesDataList[[groupName]]
          if (!is.null(cData) && nrow(cData) > 0) {
            for (i in seq_len(nrow(cData))) {
              row_key <- paste(groupName, cData$node[i], sep="_")
              row_vals <- list(
                node = cData$node[i],
                community = cData$community[i]
              )
              if (!is.null(groupVarName)) row_vals <- c(list(group = groupName), row_vals)
              commTable$addRow(rowKey = row_key, values = row_vals)
            }
          }
        }
      }
    },

    .mixed_summary = function(successful_groups, weightedAdjMatrices, groupVarName, groupLevels, na_note_global) {
      summaryTable <- self$results$summary_mixed
      summaryTable$setState(NULL)
      summaryTable$deleteRows()
      if (!is.null(na_note_global)) {
        summaryTable$setNote("na_handling_summary", na_note_global)
      } else {
        summaryTable$setNote("na_handling_summary", NULL)
      }
      
      for (groupName in successful_groups) {
        adjMat <- weightedAdjMatrices[[groupName]]
        nNodes <- ncol(adjMat)
        nonZeroEdges <- sum(abs(adjMat[upper.tri(adjMat)]) > 1e-10, na.rm = TRUE)
        possibleEdges <- nNodes * (nNodes - 1) / 2
        sparsity_val <- if(possibleEdges > 0) 1 - (nonZeroEdges / possibleEdges) else 1
        summaryRow <- list(nodes = nNodes, edges = paste(nonZeroEdges, "/", possibleEdges), sparsity = sparsity_val)
        if (!is.null(groupVarName)) {
          summaryRow <- c(list(group = groupName), summaryRow)
        }
        summaryTable$addRow(rowKey = groupName, values = summaryRow)
      }
    },

    .mixed_centrality = function(successful_groups, centralityDataList, groupVarName, groupLevels) {
      centralityTable <- self$results$centrality_mixed
      centralityTable$setState(NULL)
      centralityTable$deleteRows()
      allCentralityDataForPlot <- list()
      
      if (isTRUE(self$options$tableCentrality_mixed) || isTRUE(self$options$plotCentrality_mixed)) {
        data_found_for_cent_table = FALSE
        for (groupName in successful_groups) {
          centralityData_raw <- centralityDataList[[groupName]]
          if (!is.null(centralityData_raw) && nrow(centralityData_raw) > 0) {
            data_found_for_cent_table = TRUE
            centralityData_Zscores <- centralityData_raw
            cols_to_norm_cent <- c("strength", "closeness", "betweenness", "expectedInfluence")
            for (col in cols_to_norm_cent) {
              if (col %in% names(centralityData_Zscores) && is.numeric(centralityData_Zscores[[col]])) {
                valid_vals <- centralityData_Zscores[[col]][is.finite(centralityData_Zscores[[col]])]
                if (length(valid_vals) > 1) {
                  mean_val <- mean(valid_vals, na.rm = TRUE)
                  sd_val <- stats::sd(valid_vals, na.rm = TRUE)
                  if (!is.na(sd_val) && sd_val > 1e-10) {
                    centralityData_Zscores[[col]] <- (centralityData_Zscores[[col]] - mean_val) / sd_val
                  } else {
                    centralityData_Zscores[[col]] <- centralityData_Zscores[[col]] - mean_val
                  }
                } else if (length(valid_vals) == 1) {
                  centralityData_Zscores[[col]] <- 0
                } else {
                  centralityData_Zscores[[col]] <- NA_real_
                }
              }
            }
            tempDataForPlot <- centralityData_Zscores
            if (!is.null(groupVarName)) {
              tempDataForPlot$group <- groupName
            }
            allCentralityDataForPlot[[groupName]] <- tempDataForPlot
            
            if (isTRUE(self$options$tableCentrality_mixed)) {
              for (i in 1:nrow(centralityData_Zscores)) {
                row_values <- as.list(centralityData_Zscores[i, ])
                if (!is.null(groupVarName)) {
                  row_values <- c(list(group = groupName), row_values)
                }
                centralityTable$addRow(rowKey = paste(groupName, centralityData_Zscores$variable[i], sep="_"), values = row_values)
              }
            }
          }
        }
        if (data_found_for_cent_table && isTRUE(self$options$tableCentrality_mixed)) {
          centralityTable$setNote("centrality_info", "Values in table and plot are Z-score normalized if multiple variables/values exist within a group.")
        }
      }
      allCentralityDataForPlot
    },

    .mixed_clustering = function(successful_groups, clusteringDataList, groupVarName, groupLevels) {
      clusteringTable <- self$results$clustering_mixed
      clusteringTable$setState(NULL)
      clusteringTable$deleteRows()
      allClusteringDataForPlot <- list()
      
      if (isTRUE(self$options$tableClustering_mixed) || isTRUE(self$options$plotClustering_mixed)) {
        data_found_for_clust_table = FALSE
        for (groupName in successful_groups) {
          clusteringData_raw <- clusteringDataList[[groupName]]
          if (!is.null(clusteringData_raw) && nrow(clusteringData_raw) > 0) {
            data_found_for_clust_table = TRUE
            clusteringData_Zscores <- clusteringData_raw
            cols_to_norm_clust <- c("Barrat", "Onnela", "WS", "Zhang")
            for (col in cols_to_norm_clust) {
              if (col %in% names(clusteringData_Zscores) && is.numeric(clusteringData_Zscores[[col]])) {
                valid_vals <- clusteringData_Zscores[[col]][is.finite(clusteringData_Zscores[[col]])]
                if (length(valid_vals) > 1) {
                  mean_val <- mean(valid_vals, na.rm = TRUE)
                  sd_val <- stats::sd(valid_vals, na.rm = TRUE)
                  if (!is.na(sd_val) && sd_val > 1e-10) {
                    clusteringData_Zscores[[col]] <- (clusteringData_Zscores[[col]] - mean_val) / sd_val
                  } else {
                    clusteringData_Zscores[[col]] <- clusteringData_Zscores[[col]] - mean_val
                  }
                } else if (length(valid_vals) == 1) {
                  clusteringData_Zscores[[col]] <- 0
                } else {
                  clusteringData_Zscores[[col]] <- NA_real_
                }
              }
            }
            tempDataForPlot_Clust <- clusteringData_Zscores
            if (!is.null(groupVarName)) {
              tempDataForPlot_Clust$group <- groupName
            }
            allClusteringDataForPlot[[groupName]] <- tempDataForPlot_Clust
            
            if (isTRUE(self$options$tableClustering_mixed)) {
              for (i in 1:nrow(clusteringData_Zscores)) {
                row_values <- as.list(clusteringData_Zscores[i, ])
                if (!is.null(groupVarName)) {
                  row_values <- c(list(group = groupName), row_values)
                }
                clusteringTable$addRow(rowKey = paste(groupName, clusteringData_Zscores$node[i], sep="_"), values = row_values)
              }
            }
          }
        }
        if (data_found_for_clust_table && isTRUE(self$options$tableClustering_mixed)) {
          clusteringTable$setNote("clustering_info", "Values in table and plot are Z-score normalized if multiple variables/values exist within a group.")
        }
      }
      allClusteringDataForPlot
    },

    .mixed_bridge = function(successful_groups, bridgeDataList, groupVarName) {
      bridgeTable_m <- self$results$bridge_mixed
      bridgeTable_m$setState(NULL)
      bridgeTable_m$deleteRows()
      bridgeTable_m$setNote("bridge_unavailable", NULL)
      if (isTRUE(self$options$tableBridge_mixed)) {
        unavailable_groups <- successful_groups[vapply(successful_groups, function(groupName) is.null(bridgeDataList[[groupName]]) || nrow(bridgeDataList[[groupName]]) == 0, logical(1))]
        if (length(unavailable_groups) > 0) bridgeTable_m$setNote("bridge_unavailable", paste0("Bridge centrality unavailable for: ", paste(unavailable_groups, collapse = ", "), ". It requires a complete assignment of nodes to at least two communities (node groups or detected communities) and a successful networktools calculation."))
        for (groupName in successful_groups) {
          bData <- bridgeDataList[[groupName]]
          if (!is.null(bData) && nrow(bData) > 0) {
            for (i in seq_len(nrow(bData))) {
              row_key <- paste(groupName, bData$node[i], sep="_")
              row_vals <- list(
                node = bData$node[i],
                community = bData$community[i],
                bridgeStrength = bData$bridgeStrength[i],
                bridgeBetweenness = bData$bridgeBetweenness[i],
                bridgeCloseness = bData$bridgeCloseness[i],
                bridgeEI1 = bData$bridgeEI1[i]
              )
              if (!is.null(groupVarName)) row_vals <- c(list(group = groupName), row_vals)
              bridgeTable_m$addRow(rowKey = row_key, values = row_vals)
            }
          }
        }
      }
    },

    .mixed_communities = function(successful_groups, communitiesDataList, groupVarName) {
      commTable_m <- self$results$communities_mixed
      commTable_m$setState(NULL)
      commTable_m$deleteRows()
      commTable_m$setNote("source", "Complete manual assignments take precedence over community detection and are shared by membership, bridge centrality and network colors; without an assignment, the selected detection algorithm is used.")
      if (isTRUE(self$options$tableCommunities_mixed)) {
        for (groupName in successful_groups) {
          cData <- communitiesDataList[[groupName]]
          if (!is.null(cData) && nrow(cData) > 0) {
            for (i in seq_len(nrow(cData))) {
              row_key <- paste(groupName, cData$node[i], sep="_")
              row_vals <- list(
                node = cData$node[i],
                community = cData$community[i]
              )
              if (!is.null(groupVarName)) row_vals <- c(list(group = groupName), row_vals)
              commTable_m$addRow(rowKey = row_key, values = row_vals)
            }
          }
        }
      }
    },

    .plotNetwork_net = function(image, ggtheme, theme, ...) {
      plotState <- image$state
      if (is.null(plotState) || is.null(plotState$graphs) || length(plotState$graphs) == 0 || is.null(plotState$layout)) return(FALSE)
      if (!requireNamespace("qgraph", quietly = TRUE)) {
        plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "qgraph package missing."); return(FALSE)
      }
      
      graphsToPlot <- plotState$graphs
      layout <- plotState$layout
      labels <- plotState$labels
      groupNames <- plotState$groupNames
      nodeGroups <- plotState$nodeGroups
      numPlots <- length(graphsToPlot)
      
      old_par <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old_par))
      if (numPlots > 1) { graphics::par(mfrow = c(1, numPlots)) }
      graphics::par(bg = "transparent")
      plotOk <- TRUE
      
      seed_present <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      previous_seed <- if (seed_present) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
      on.exit({
        if (seed_present) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(list = ".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)
      if (!is.null(plotState$layoutSeed)) set.seed(plotState$layoutSeed)
      for (i in 1:numPlots) { 
        groupName <- groupNames[i]
        currentGraph <- graphsToPlot[[groupName]]
        if (is.null(currentGraph)) next
        
        qArgs <- list(
          input = currentGraph, 
          layout = layout, 
          labels = labels, 
          theme = "colorblind", 
          DoNotPlot = FALSE, 
          title = groupName
        )
        if (!is.null(nodeGroups)) {
          qArgs$groups <- nodeGroups
          qArgs$legend <- TRUE
          qArgs$legend.cex <- 0.5
          qArgs$GLratio <- 2.5
        }
        
        tryCatch({
          do.call(qgraph::qgraph, qArgs)
        }, error = function(e) { 
          jmvcore::reject(paste0("Network plotting failed for group ", groupName, ": ", e$message), code = "network_plot_error")
          plot(1, type="n", axes=FALSE, xlab="", ylab="")
          text(1, 1, paste0("Plot Error (", groupName, "):\n", e$message), col="red")
          plotOk <<- FALSE 
        }) 
      }
      return(plotOk)
    },
    
    .plotCentrality_net = function(image, ggtheme, theme, ...) {
      plotData <- image$state; if (is.null(plotData) || nrow(plotData) == 0) return(FALSE)
      if (!requireNamespace("ggplot2", quietly = TRUE)) { stop("ggplot2 package required.") }
      if (!requireNamespace("reshape2", quietly = TRUE)) { stop("reshape2 package required.") }
      if (!requireNamespace("gtools", quietly = TRUE)) { stop("gtools package required.") }
      
      id_vars <- intersect(c("variable", "group"), names(plotData))
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, variable.name = "measure", value.name = "value") }, error = function(e) jmvcore::reject(conditionMessage(e), code = "network_plot_data"))
      if (is.null(plotDataLong)) return(FALSE)
      plotDataLong <- plotDataLong[!is.na(plotDataLong$value), ]
      if (nrow(plotDataLong) == 0) return(FALSE)
      
      measure_order <- c("betweenness", "closeness", "strength", "expectedInfluence")
      measure_labels <- c("Betweenness", "Closeness", "Strength", "Expected Influence")
      names(measure_labels) <- measure_order
      
      plotDataLong$measure <- factor(plotDataLong$measure, levels = intersect(measure_order, unique(as.character(plotDataLong$measure))), labels = measure_labels[intersect(measure_order, unique(as.character(plotDataLong$measure)))])
      plotDataLong$variable <- factor(plotDataLong$variable, levels = gtools::mixedsort(unique(plotDataLong$variable), decreasing = TRUE))
      
      if ("group" %in% names(plotDataLong)) {
        p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable, colour = group, linetype = group, group = group))
      } else {
        p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable, group = 1))
      }
      
      p <- p + ggplot2::geom_path(colour = "gray60") +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::facet_wrap(~ measure, scales = "free_x") +
        ggtheme +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)) +
        ggplot2::labs(x = "Value (Z-score)", y = "Variable", colour = "Group", linetype = "Group") +
        ggplot2::ggtitle("Centrality Plot")
      
      if (!"group" %in% names(plotDataLong) || length(unique(plotDataLong$group)) <= 1) {
        p <- p + ggplot2::theme(legend.position = "none")
      }
      
      print(p); TRUE
    },
    
    .plotClustering_net = function(image, ggtheme, theme, ...) {
      plotData <- image$state; if (is.null(plotData) || nrow(plotData) == 0) return(FALSE)
      if (!requireNamespace("ggplot2", quietly = TRUE)) { stop("ggplot2 package required.") }
      if (!requireNamespace("gtools", quietly = TRUE)) { stop("gtools package required.") }
      if (!requireNamespace("reshape2", quietly = TRUE)) { stop("reshape2 package required.") }
      
      id_vars <- intersect(c("node", "group"), names(plotData))
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, variable.name = "measure", value.name = "value") }, error = function(e) jmvcore::reject(conditionMessage(e), code = "network_plot_data"))
      if (is.null(plotDataLong)) return(FALSE)
      plotDataLong <- plotDataLong[!is.na(plotDataLong$value), ]
      if (nrow(plotDataLong) == 0) return(FALSE)
      
      measure_order <- c("Barrat", "Onnela", "WS", "Zhang")
      plotDataLong$measure <- factor(plotDataLong$measure, levels = intersect(measure_order, unique(as.character(plotDataLong$measure))))
      plotDataLong$node <- factor(plotDataLong$node, levels = gtools::mixedsort(unique(plotDataLong$node), decreasing = TRUE))
      
      if ("group" %in% names(plotDataLong)) {
        p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = node, colour = group, shape = group, group = group))
      } else {
        p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = node, group = 1))
      }
      
      p <- p + ggplot2::geom_path(colour = "gray60") +
        ggplot2::geom_point(size=2.5) +
        ggplot2::facet_wrap(~ measure, scales = "free_x") +
        ggtheme +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)) +
        ggplot2::labs(x = "Value (Z-score)", y = "Variable", colour = "Group", shape = "Group") +
        ggplot2::ggtitle("Clustering Measures (Z-score Normalized)")
      
      if (!"group" %in% names(plotDataLong) || length(unique(plotDataLong$group)) <= 1) {
        p <- p + ggplot2::theme(legend.position = "none")
      }
      
      print(p); TRUE
    },
    
    .plotNetwork_mixed = function(image, ggtheme, theme, ...) {
      seed_present <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      previous_seed <- if (seed_present) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
      on.exit({
        if (seed_present) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(list = ".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)

      plotState <- image$state
      if (!is.null(plotState$layoutSeed)) set.seed(plotState$layoutSeed)
      if (is.null(plotState) || is.null(plotState$graphs) || length(plotState$graphs) == 0) { return(FALSE) }
      if (!requireNamespace("qgraph", quietly = TRUE)) {
        plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "qgraph package missing.", col="red"); return(FALSE)
      }
      
      graphsToPlot <- plotState$graphs
      edgeColorsList <- plotState$edgeColors
      layout <- plotState$layout
      labels <- plotState$labels
      groupNames <- plotState$groupNames
      nodeGroups <- plotState$nodeGroups
      numPlots <- length(graphsToPlot)
      
      fncLayoutAlgorithm <- "spring"
      if (!is.null(plotState$layoutAlgorithm)) fncLayoutAlgorithm <- plotState$layoutAlgorithm
      if (is.null(layout) && length(graphsToPlot) > 0 && !is.null(graphsToPlot[[1]])) {
        tryCatch({
          q_obj <- qgraph::qgraph(graphsToPlot[[1]], layout = fncLayoutAlgorithm, DoNotPlot = TRUE)
          layout <- q_obj$layout
          if (!is.null(layout) && is.character(labels) && length(labels) == nrow(layout)) {
            rownames(layout) <- labels
          }
        }, error = function(e_layout) {
          plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "Layout Error.", col="red"); return(FALSE)
        })
      }
      if (is.null(layout)) {
        plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "Layout Unavailable.", col="red"); return(FALSE)
      }
      
      old_par <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old_par))
      if (numPlots > 1) {
        nc <- if (numPlots > 3) 2 else numPlots
        nr <- ceiling(numPlots / nc)
        graphics::par(mfrow = c(nr, nc), mar = c(1,1,2,1))
      } else {
        graphics::par(mar = c(2,2,2,2))
      }
      graphics::par(bg = "transparent")
      plotOk <- TRUE
      
      for (i in 1:numPlots) { 
        groupName <- groupNames[i]
        currentGraph <- graphsToPlot[[groupName]]
        currentEdgeColors <- NULL
        if (!is.null(edgeColorsList) && length(edgeColorsList) >= i && !is.null(edgeColorsList[[i]])) {
          currentEdgeColors <- edgeColorsList[[i]]
        }
        if (is.null(currentGraph)) next
        
        currentLabels <- labels
        if (is.character(labels) && length(labels) != ncol(currentGraph)) {
          currentLabels <- 1:ncol(currentGraph)
        }
        max_val_abs <- max(abs(unlist(graphsToPlot)), na.rm=TRUE)
        if (!is.finite(max_val_abs) || max_val_abs == 0) max_val_abs <- 1
        
        qArgs <- list(
          input = currentGraph, 
          layout = layout, 
          labels = currentLabels, 
          edge.color = currentEdgeColors, 
          theme = "colorblind", 
          DoNotPlot = FALSE, 
          title = groupName, 
          mar = graphics::par("mar"), 
          legend = FALSE, 
          cut = 0, 
          maximum = max_val_abs
        )
        
        if (!is.null(nodeGroups)) {
          qArgs$groups <- nodeGroups
          qArgs$legend <- TRUE
          qArgs$legend.cex <- 0.5
          qArgs$GLratio <- 2.5
        }
        
        tryCatch({ 
          do.call(qgraph::qgraph, qArgs) 
        }, error = function(e) {
          plot(1, type="n", axes=FALSE, xlab="", ylab="")
          text(1, 1, paste0("Plot Error (", groupName, ")"), col="red", cex=0.8)
          plotOk <<- FALSE
        }) 
      }
      return(plotOk)
    },
    
    .plotCentrality_mixed = function(image, ggtheme, theme, ...) {
      plotData <- image$state; if (is.null(plotData) || nrow(plotData) == 0) return(FALSE)
      id_vars <- intersect(c("variable", "group"), names(plotData))
      if (length(id_vars) == 0 || !"variable" %in% id_vars) { return(FALSE) }
      measure_vars <- intersect(c("strength", "closeness", "betweenness", "expectedInfluence"), names(plotData))
      if (length(measure_vars) == 0) { return(FALSE) }
      
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, measure.vars = measure_vars, variable.name = "measure", value.name = "value") }, error = function(e) jmvcore::reject(conditionMessage(e), code = "network_plot_data"))
      if (is.null(plotDataLong) || nrow(plotDataLong) == 0) return(FALSE)
      plotDataLong <- plotDataLong[!is.na(plotDataLong$value), ]
      if (nrow(plotDataLong) == 0) return(FALSE)
      
      measure_order <- c("strength", "closeness", "betweenness", "expectedInfluence")
      measure_labels_map <- c(strength="Strength", closeness="Closeness", betweenness="Betweenness", expectedInfluence="Expected Influence")
      present_measures <- intersect(measure_order, unique(as.character(plotDataLong$measure)))
      plotDataLong$measure <- factor(plotDataLong$measure, levels = present_measures, labels = measure_labels_map[present_measures])
      
      y_axis_levels_ordered <- gtools::mixedsort(unique(as.character(plotDataLong$variable)), decreasing = FALSE)
      plotDataLong$variable <- factor(plotDataLong$variable, levels = rev(y_axis_levels_ordered))
      
      use_group <- "group" %in% names(plotDataLong) && length(unique(plotDataLong$group)) > 1
      if (use_group) {
        plotDataLong <- dplyr::arrange(plotDataLong, measure, group, variable)
      } else {
        plotDataLong <- dplyr::arrange(plotDataLong, measure, variable)
      }
      
      if (use_group) {
        p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable, colour = group, linetype = group))
      } else {
        p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable))
      }
      
      path_group_aes <- if (use_group) ggplot2::aes(group = interaction(measure, group)) else ggplot2::aes(group = measure)
      gg_version <- utils::packageVersion("ggplot2")
      path_param <- if (gg_version >= "3.4.0") list(linewidth = 0.8) else list(size = 0.8)
      
      p <- p + do.call(ggplot2::geom_path, c(list(mapping = path_group_aes, alpha = 0.6), path_param))
      point_aes <- if (use_group) ggplot2::aes(shape = group) else ggplot2::aes()
      p <- p + ggplot2::geom_point(mapping = point_aes, size = 2.5)
      
      if (use_group) {
        p <- p + ggplot2::labs(colour = "Group", linetype = "Group", shape = "Group")
      }
      p <- p + ggplot2::facet_wrap(~ measure, scales = "free_x") + ggtheme + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA), 
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA), 
                       strip.background = ggplot2::element_rect(fill="grey90", color="grey50")) + 
        ggplot2::labs(x = "Value (Z-score)", y = "Variable") + 
        ggplot2::ggtitle("Centrality Measures")
      
      if (!use_group) {
        p <- p + ggplot2::theme(legend.position = "none")
      }
      print(p); TRUE
    },
    
    .plotClustering_mixed = function(image, ggtheme, theme, ...) {
      plotData <- image$state
      if (is.null(plotData) || nrow(plotData) == 0) return(FALSE)
      if ("node" %in% names(plotData) && !"variable" %in% names(plotData)) {
        plotData$variable <- plotData$node; plotData$node <- NULL
      }
      id_vars <- intersect(c("variable", "group"), names(plotData))
      measure_vars_clust <- intersect(c("Barrat", "Onnela", "WS", "Zhang"), names(plotData))
      
      if (length(id_vars) == 0 || !"variable" %in% id_vars) { return(FALSE) }
      if (length(measure_vars_clust) == 0) { return(FALSE) }
      
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, measure.vars = measure_vars_clust, variable.name = "measure", value.name = "value") }, error = function(e) jmvcore::reject(conditionMessage(e), code = "network_plot_data"))
      if (is.null(plotDataLong) || nrow(plotDataLong) == 0) return(FALSE)
      plotDataLong <- plotDataLong[!is.na(plotDataLong$value), ]
      if (nrow(plotDataLong) == 0) return(FALSE)
      
      clust_measure_order <- c("Barrat", "Onnela", "WS", "Zhang")
      clust_measure_labels_map <- c(Barrat="Barrat", Onnela="Onnela", WS="WS", Zhang="Zhang")
      present_clust_measures <- intersect(clust_measure_order, unique(as.character(plotDataLong$measure)))
      plotDataLong$measure <- factor(plotDataLong$measure, levels = present_clust_measures, labels = clust_measure_labels_map[present_clust_measures])
      
      y_axis_levels_ordered <- gtools::mixedsort(unique(as.character(plotDataLong$variable)), decreasing = FALSE)
      plotDataLong$variable <- factor(plotDataLong$variable, levels = rev(y_axis_levels_ordered))
      
      use_group <- "group" %in% names(plotDataLong) && length(unique(plotDataLong$group)) > 1
      if (use_group) {
        plotDataLong <- dplyr::arrange(plotDataLong, measure, group, variable)
      } else {
        plotDataLong <- dplyr::arrange(plotDataLong, measure, variable)
      }
      
      if (use_group) {
        p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable, colour = group, linetype = group))
      } else {
        p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable))
      }
      
      path_group_aes <- if (use_group) ggplot2::aes(group = interaction(measure, group)) else ggplot2::aes(group = measure)
      gg_version <- utils::packageVersion("ggplot2")
      path_param <- if (gg_version >= "3.4.0") list(linewidth = 0.8) else list(size = 0.8)
      
      p <- p + do.call(ggplot2::geom_path, c(list(mapping = path_group_aes, alpha = 0.6), path_param))
      point_aes <- if (use_group) ggplot2::aes(shape = group) else ggplot2::aes()
      p <- p + ggplot2::geom_point(mapping = point_aes, size = 2.5)
      
      if (use_group) {
        p <- p + ggplot2::labs(colour = "Group", linetype = "Group", shape = "Group")
      }
      p <- p + ggplot2::facet_wrap(~ measure, scales = "free_x") + ggtheme + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA), 
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA), 
                       strip.background = ggplot2::element_rect(fill="grey90", color="grey50")) + 
        ggplot2::labs(x = "Clustering Coefficient (Z-score)", y = "Variable") + 
        ggplot2::ggtitle("Clustering Measures")
      
      if (!use_group) {
        p <- p + ggplot2::theme(legend.position = "none")
      }
      print(p); TRUE
    },

    .estimateGraphMatrix = function(data_df, vars, estimator, corMethod) {
      tryCatch({
        result <- private$.fit_network(data_df, vars, estimator, corMethod)
        graph <- result$graph
        attr(graph, "pd_recovery") <- isTRUE(attr(result, "pd_recovery"))
        graph
      }, error = function(error) {
        if (identical(error$code, "restart")) stop(error)
        NULL
      })
    },

    .bootstrap_result = function(data, vars, estimator, corMethod, bootN, type, group) {
      completed <- 0L
      failed <- 0L
      regularized <- 0L
      restart <- NULL
      sampling <- FALSE
      fit <- function(data, ...) {
        tryCatch({
          private$.checkpoint(flush = FALSE)
          result <- private$.fit_network(data, vars, estimator, corMethod)
          graph <- result$graph
          if (sampling && isTRUE(attr(result, "pd_recovery"))) regularized <<- regularized + 1L
          if (sampling) completed <<- completed + 1L
          graph
        }, error = function(error) {
          if (identical(error$code, "restart")) restart <<- error
          if (sampling && is.null(restart)) failed <<- failed + 1L
          stop(error)
        })
      }
      procedure <- if (type == "case") "Case-dropping bootstrap (CS)" else "Edge bootstrap"
      result <- tryCatch({
        sample <- bootnet::estimateNetwork(data[, vars, drop = FALSE], default = "none", fun = fit, labels = vars, weighted = TRUE, signed = TRUE, directed = FALSE, verbose = FALSE)
        sampling <- TRUE
        bootstrap <- bootnet::bootnet(sample, nBoots = bootN, type = type, nCores = 1, statistics = if (type == "case") c("strength", "closeness", "betweenness", "expectedInfluence") else "edge", caseMin = 0.05, caseMax = 0.75, caseN = 10, maxErrors = 0, verbose = FALSE)
        private$.checkpoint(flush = FALSE)
        bootstrap
      }, error = function(error) {
        if (!is.null(restart)) stop(restart)
        if (identical(error$code, "restart")) stop(error)
        private$.network_warning(paste0(procedure, " unavailable for group \"", group, "\": ", conditionMessage(error), ". No estimator substitution or failed-replicate removal was used."))
        NULL
      })
      private$.resampling_diagnostic(group, procedure, bootN, completed, failed, if (is.null(result)) "Aborted; no interval or CS reported" else "Completed with requested estimator", regularized)
      result
    },

    .runBootStability = function(currentData, vars, estimator, corMethod, bootN, tableEdgeCI, tableCS, group = "Overall", group_index = 1L) {
      private$.with_seed(as.integer((self$options$bootSeed_net + group_index - 1) %% .Machine$integer.max), {
        if (isTRUE(self$options$bootEdgeCI_net)) {
          result <- private$.bootstrap_result(currentData, vars, estimator, corMethod, bootN, "nonparametric", group)
          if (!is.null(result)) {
            summary <- as.data.frame(summary(result, statistics = "edge"))
            for (index in seq_len(nrow(summary))) {
              row <- summary[index, ]
              tableEdgeCI$addRow(rowKey = paste(group_index, index, sep = ":"), values = list(group = group, edge = paste(row$node1, row$node2, sep = " -- "), weight = as.numeric(row$sample), boot_mean = as.numeric(row$mean), boot_se = as.numeric(row$sd), ci_lower = as.numeric(row$q2.5), ci_upper = as.numeric(row$q97.5)))
            }
          }
          tableEdgeCI$setNote("method", "bootnet nonparametric bootstrap, requested estimator and correlation policy retained; 95% percentile intervals use type-6 quantiles. Each group has its own bootstrap; diagnostics report completion and failures. A failed estimation aborts that group's procedure, without reporting a conditional interval from only successful draws.")
        }
        if (isTRUE(self$options$bootCS_net)) {
          result <- private$.bootstrap_result(currentData, vars, estimator, corMethod, bootN, "case", group)
          if (!is.null(result)) {
            stability <- bootnet::corStability(result, cor = 0.7, statistics = "all", verbose = FALSE)
            labels <- c(strength = "Strength", closeness = "Closeness", betweenness = "Betweenness", expectedInfluence = "Expected Influence")
            for (metric in names(labels)) {
              values <- result$sampleTable$value[result$sampleTable$type == metric]
              estimable <- length(values) > 1 && all(is.finite(values)) && stats::sd(values) > 0
              coefficient <- if (estimable && metric %in% names(stability)) as.numeric(stability[[metric]]) else NA_real_
              interpretation <- if (!is.finite(coefficient)) "Not estimable (constant or non-finite sample centrality)" else if (coefficient >= 0.5) "Stable (CS >= 0.5)" else if (coefficient >= 0.25) "Acceptable (0.25 <= CS < 0.5)" else "Unstable (CS < 0.25)"
              tableCS$addRow(rowKey = paste(group_index, metric, sep = ":"), values = list(group = group, measure = unname(labels[metric]), cs_coefficient = coefficient, interpretation = interpretation))
            }
          }
          tableCS$setNote("method", "bootnet case-dropping bootstrap and corStability(cor=0.7), independently for each group. Case fractions range from 0.05 to 0.75 on 10 candidate levels; CS is limited to sampled levels. Increase replicates for Monte Carlo precision. Undefined sample centralities have no CS coefficient. Group seeds are bootstrap seed plus group index minus one; caller RNG is restored.")
        }
      })
    },
    .runNCT = function(dataGroup1, dataGroup2, vars, estimator, corMethod, permutations, tableGlobal, tableEdges, groups = c("Group 1", "Group 2")) {
      tableGlobal$deleteRows()
      tableEdges$deleteRows()
      private$.with_seed(self$options$nctSeed_net, {
        group_label <- paste(groups, collapse = " vs ")
        graph1 <- private$.estimateGraphMatrix(dataGroup1, vars, estimator, corMethod)
        graph2 <- private$.estimateGraphMatrix(dataGroup2, vars, estimator, corMethod)
        if (is.null(graph1) || is.null(graph2)) {
          private$.resampling_diagnostic(group_label, "NCT permutation", permutations, 0L, 0L, "Aborted: observed network estimation failed")
          private$.network_warning("NCT unavailable: at least one observed network could not be estimated with the requested estimator. No substitute estimator was used.")
          return()
        }
        positions <- which(upper.tri(graph1), arr.ind = TRUE)
        edge1 <- graph1[positions]
        edge2 <- graph2[positions]
        observed_edges <- abs(edge1 - edge2)
        observed_maximum <- max(observed_edges)
        observed_strength <- abs(sum(abs(edge1)) - sum(abs(edge2)))
        combined <- rbind(dataGroup1[, vars, drop = FALSE], dataGroup2[, vars, drop = FALSE])
        first_n <- nrow(dataGroup1)
        maximum <- strength <- rep(NA_real_, permutations)
        edge_differences <- matrix(NA_real_, permutations, nrow(positions))
        regularized <- 0L
        for (replicate in seq_len(permutations)) {
          private$.checkpoint(flush = FALSE)
          order <- sample(nrow(combined))
          first <- private$.estimateGraphMatrix(combined[order[seq_len(first_n)], , drop = FALSE], vars, estimator, corMethod)
          second <- private$.estimateGraphMatrix(combined[order[-seq_len(first_n)], , drop = FALSE], vars, estimator, corMethod)
          regularized <- regularized + as.integer(isTRUE(attr(first, "pd_recovery"))) + as.integer(isTRUE(attr(second, "pd_recovery")))
          if (is.null(first) || is.null(second)) {
            private$.resampling_diagnostic(group_label, "NCT permutation", permutations, replicate - 1L, 1L, "Aborted on first failed permutation; no p-values reported", regularized)
            private$.network_warning(paste0("NCT aborted at permutation ", replicate, " of ", permutations, ": the requested estimator failed. No p-values are reported; failures were not replaced by zero or another estimator."))
            return()
          }
          edge_differences[replicate, ] <- abs(first[positions] - second[positions])
          maximum[replicate] <- max(edge_differences[replicate, ])
          strength[replicate] <- abs(sum(abs(first[positions])) - sum(abs(second[positions])))
        }
        private$.resampling_diagnostic(group_label, "NCT permutation", permutations, permutations, 0L, "Completed with requested estimator", regularized)
        tableGlobal$addRow(rowKey = "M", values = list(test = "Maximum Edge Difference (M) [Network Invariance]", statistic = observed_maximum, p_value = (sum(maximum >= observed_maximum) + 1) / (permutations + 1)))
        tableGlobal$addRow(rowKey = "S", values = list(test = "Global Strength Difference (S)", statistic = observed_strength, p_value = (sum(strength >= observed_strength) + 1) / (permutations + 1)))
        for (index in seq_len(nrow(positions))) {
          tableEdges$addRow(rowKey = index, values = list(edge = paste(vars[positions[index, ]], collapse = " -- "), weight_g1 = edge1[index], weight_g2 = edge2[index], diff = observed_edges[index], p_value = (sum(edge_differences[, index] >= observed_edges[index]) + 1) / (permutations + 1)))
        }
        note <- paste0("Independent-group permutation test: ", groups[1], " (Group 1) versus ", groups[2], " (Group 2). Requested estimator/correlation policy retained; all ", permutations, " permutations succeeded, including valid zero statistics. Two-tailed absolute differences, plus-one correction; edge p-values are unadjusted. Any failed permutation aborts the procedure without p-values.")
        tableGlobal$setNote("method", note)
        tableEdges$setNote("method", note)
      })
    }


  )
)
