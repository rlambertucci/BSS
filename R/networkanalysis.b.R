networkanalysisClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "networkanalysisClass",
  inherit = networkanalysisBase,
  private = list(
    
    .getColumnNames = function(table) {
      tryCatch(sapply(table$columns, function(col) col$name), error = function(e) character(0))
    },
    
    .init = function() {
      message("networkanalysisClass$.init() called")
      
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
      
      summaryTable_net <- self$results$summary_net
      if (!"group" %in% private$.getColumnNames(summaryTable_net)) {
        try(summaryTable_net$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar_net)"), silent = TRUE)
      }
      centralityTable_net <- self$results$centrality_net
      if (!"group" %in% private$.getColumnNames(centralityTable_net)) {
        try(centralityTable_net$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar_net)"), silent = TRUE)
      }
      if (!"variable" %in% private$.getColumnNames(centralityTable_net)) {
        try(centralityTable_net$addColumn(name = "variable", title = "Variable", type = 'text'), silent = TRUE)
      }
      clusteringTable_net <- self$results$clustering_net
      if (!"group" %in% private$.getColumnNames(clusteringTable_net)) {
        try(clusteringTable_net$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar_net)"), silent = TRUE)
      }
      if (!"node" %in% private$.getColumnNames(clusteringTable_net)) {
        try(clusteringTable_net$addColumn(name = "node", title = "Variable", type = 'text'), silent = TRUE)
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
      
      summaryTable_mixed <- self$results$summary_mixed
      if (!"group" %in% private$.getColumnNames(summaryTable_mixed)) {
        try(summaryTable_mixed$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar_mixed)"), silent = TRUE)
      }
      centralityTable_mixed <- self$results$centrality_mixed
      if (!"group" %in% private$.getColumnNames(centralityTable_mixed)) {
        try(centralityTable_mixed$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar_mixed)"), silent = TRUE)
      }
      if (!"variable" %in% private$.getColumnNames(centralityTable_mixed)) {
        try(centralityTable_mixed$addColumn(name = "variable", title = "Variable", type = 'text'), silent = TRUE)
      }
      clusteringTable_mixed <- self$results$clustering_mixed
      if (!"group" %in% private$.getColumnNames(clusteringTable_mixed)) {
        try(clusteringTable_mixed$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar_mixed)"), silent = TRUE)
      }
      if (!"node" %in% private$.getColumnNames(clusteringTable_mixed)) {
        try(clusteringTable_mixed$addColumn(name = "node", title = "Variable", type = 'text'), silent = TRUE)
      }
      
      if (length(self$options$vars_net) < 3) {
        if (!is.null(self$results$instructions_net)) {
          self$results$instructions_net$setContent("Welcome to Network Analysis for Continuous Data!\n\nPlease select at least 3 variables in the 'Variables' box to begin.")
        }
      }
      if (is.null(self$options$varsCont_mixed) && is.null(self$options$varsCat_mixed) && is.null(self$options$varsCount_mixed)) {
        if (!is.null(self$results$instructions_mixed)) {
          self$results$instructions_mixed$setContent("Welcome to Mixed Data Network Analysis (mgm)!\n\nPlease select at least two variables (Continuous, Categorical, or Count) to begin.")
        }
      }
    },
    
    .run = function() {
      message("networkanalysisClass$.run() called")
      mode <- self$options$mode
      if (mode == "continuous") {
        private$.runContinuous()
      } else if (mode == "mixed") {
        private$.runMixed()
      }
    },
    
    .runContinuous = function() {
      message("networkanalysisClass$.runContinuous() called")
      
      if (length(self$options$vars_net) < 3) {
        instructions <- self$results$instructions_net
        instructions$setContent("Welcome to Network Analysis!\n\nPlease select at least 3 variables in the 'Variables' box to begin.")
        instructions$setVisible(TRUE)
        try(self$results$summary_net$setVisible(FALSE), silent=TRUE); try(self$results$weights_net$setVisible(FALSE), silent=TRUE)
        try(self$results$centrality_net$setVisible(FALSE), silent=TRUE); try(self$results$clustering_net$setVisible(FALSE), silent=TRUE)
        try(self$results$networkPlot_net$setVisible(FALSE), silent=TRUE); try(self$results$centralityPlot_net$setVisible(FALSE), silent=TRUE)
        try(self$results$clusteringPlot_net$setVisible(FALSE), silent=TRUE)
        return()
      } else {
        self$results$instructions_net$setVisible(FALSE)
        try(self$results$summary_net$setVisible(TRUE), silent=TRUE)
      }
      
      vars <- self$options$vars_net
      groupVarName <- self$options$groupVar_net
      nodeGroupVarName <- self$options$nodeGroupVar_net
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
          jmvcore::reject(paste("Variable '", var, "' is not numeric."), code = "var_not_numeric"); return()
        }
        if (all(is.na(data_subset[[var]]))) {
          jmvcore::reject(paste("Variable '", var, "' contains only missing values."), code = "var_all_na"); return()
        }
      }
      
      dataList <- list()
      groupLevels <- NULL
      na_note <- NULL
      
      if (!is.null(groupVarName)) {
        if (anyNA(data_subset[[groupVarName]])) {
          n_total <- nrow(data_subset)
          data_subset <- data_subset[!is.na(data_subset[[groupVarName]]), , drop=FALSE]
          n_after_na_group <- nrow(data_subset)
          na_note <- paste(n_total - n_after_na_group, "cases excluded due to missing values in the grouping variable.")
        }
        if (nrow(data_subset) == 0) { jmvcore::reject("No cases remaining after removing missing values in grouping variable.", code = "no_data_after_na_group"); return() }
        if (!is.factor(data_subset[[groupVarName]])) { data_subset[[groupVarName]] <- as.factor(data_subset[[groupVarName]]) }
        groupLevels <- levels(data_subset[[groupVarName]])
        if (length(groupLevels) < 1) { jmvcore::reject("Grouping variable has no levels.", code="no_group_levels"); return() }
        if (length(groupLevels) == 1) { jmvcore::reject("Grouping variable must have at least two levels.", code="only_one_group"); return() }
        dataList <- split(data_subset[, vars, drop=FALSE], data_subset[[groupVarName]], drop=TRUE)
      } else {
        dataList <- list(Overall = data_subset[, vars, drop=FALSE])
        groupLevels <- "Overall"
        na_note <- "Missing data handled using pairwise deletion."
      }
      
      qgraph_groups <- NULL
      if (!is.null(nodeGroupVarName)) {
        tryCatch({
          raw_labels <- as.character(data_full[[nodeGroupVarName]])
          raw_labels <- raw_labels[!is.na(raw_labels) & raw_labels != ""]
          node_group_mapping <- rep(NA, length(vars))
          names(node_group_mapping) <- vars
          matched_any <- FALSE
          
          for (label_str in raw_labels) {
            parts <- strsplit(label_str, "=")[[1]]
            if (length(parts) >= 2) {
              v_name <- trimws(parts[1])
              g_name <- trimws(paste(parts[-1], collapse="="))
              if (v_name %in% vars) {
                node_group_mapping[v_name] <- g_name
                matched_any <- TRUE
              }
            }
          }
          if (matched_any) {
            unique_groups <- unique(na.omit(node_group_mapping))
            qgraph_groups <- list()
            for (g in unique_groups) {
              indices <- which(node_group_mapping == g)
              qgraph_groups[[g]] <- indices
            }
          }
        }, error = function(e) {
          qgraph_groups <- NULL
        })
      }
      
      networkGraphsList <- list()
      centralityDataList <- list()
      clusteringDataList <- list()
      estimation_errors <- list()
      
      if (!requireNamespace("bootnet", quietly = TRUE)) { jmvcore::reject("Package 'bootnet' required.", code = "bootnet_missing"); return() }
      if (!requireNamespace("qgraph", quietly = TRUE)) { jmvcore::reject("Package 'qgraph' required.", code = "qgraph_missing"); return() }
      
      bootnet_default <- switch(estimator, "EBICglasso" = "EBICglasso", "correlation" = "cor", "partial_correlation" = "pcor", "EBICglasso")
      bootnet_corMethod <- ifelse(corMethod == "auto", "cor_auto", corMethod)
      
      for (groupName in groupLevels) {
        currentData <- dataList[[groupName]]
        if (nrow(currentData) < 10) { estimation_errors[[groupName]] <- "Insufficient data (n < 10)"; next }
        args <- list(data = currentData, default = bootnet_default, corMethod = bootnet_corMethod, missing = "pairwise")
        networkGraph <- NULL; nodeLabels <- vars
        tryCatch({
          networkResults <- do.call(bootnet::estimateNetwork, args)
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
          }, error = function(e) {})
          clusteringDataList[[groupName]] <- clusteringDataNorm
          
        }, error = function(e) {
          err_msg <- paste("Group", groupName, "-", e$message)
          if (grepl("matrix is not positive definite", e$message, ignore.case = TRUE)) {
            err_msg <- paste(err_msg, "(Pairwise deletion issue?)")
          }
          estimation_errors[[groupName]] <- err_msg
          networkGraphsList[[groupName]] <- NULL; centralityDataList[[groupName]] <- NULL; clusteringDataList[[groupName]] <- NULL
        })
      }
      
      if (length(estimation_errors) > 0) {
        jmvcore::reject(paste("Errors occurred during estimation for some groups:\n", paste(estimation_errors, collapse="\n")), code="group_estimation_error")
      }
      successful_groups <- names(networkGraphsList)
      if (length(successful_groups) == 0) {
        jmvcore::reject("Network estimation failed for all groups.", code="all_groups_failed"); return()
      }
      
      summaryTable <- self$results$summary_net
      try(summaryTable$setState(NULL), silent=TRUE)
      try(summaryTable$deleteRows(), silent=TRUE)
      try(summaryTable$deleteNote("na_handling"), silent=TRUE)
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
          jmvcore::reject(paste("Failed to populate summary row for group", groupName, ":", e$message), code="summary_group_error")
        })
      }
      
      weightsTable <- self$results$weights_net
      if (isTRUE(self$options$tableWeights_net) && is.null(groupVarName)) {
        try(weightsTable$setVisible(TRUE), silent=TRUE)
        tryCatch({
          networkGraph <- networkGraphsList[[1]]
          nodeNames <- rownames(networkGraph)
          current_table_cols <- private$.getColumnNames(weightsTable)
          cols_to_remove <- setdiff(current_table_cols, c("node", nodeNames))
          for(col_name in cols_to_remove) {
            if (col_name %in% private$.getColumnNames(weightsTable)) {
              try(weightsTable$deleteColumn(name = col_name), silent=TRUE)
            }
          }
          if (!"node" %in% private$.getColumnNames(weightsTable)) {
            try(weightsTable$addColumn(name = "node", title = "", type = 'text'), silent = TRUE)
          }
          for (nodeName in nodeNames) {
            if (!nodeName %in% private$.getColumnNames(weightsTable)) {
              try(weightsTable$addColumn(name = nodeName, title = nodeName, type = 'number'), silent=TRUE)
            }
          }
          try(weightsTable$setState(NULL), silent = TRUE)
          try(weightsTable$deleteRows(), silent = TRUE)
          if (length(nodeNames) > 0) {
            adjMatrix <- networkGraph
            for (row_node in nodeNames) {
              row_values <- list(node = row_node)
              for (col_node in nodeNames) {
                row_values[[col_node]] <- adjMatrix[row_node, col_node]
              }
              try(weightsTable$addRow(rowKey = row_node, values = row_values), silent = TRUE)
            }
          }
        }, error = function(e) {
          jmvcore::reject(paste("Failed to populate weights table:", e$message), code="weights_table_error")
          try(weightsTable$setState(NULL), silent=TRUE)
          try(weightsTable$setVisible(FALSE), silent=TRUE)
        })
      } else {
        try(weightsTable$setState(NULL), silent=TRUE)
        try(weightsTable$setVisible(FALSE), silent=TRUE)
      }
      
      centralityTable <- self$results$centrality_net
      try(centralityTable$setState(NULL), silent=TRUE)
      try(centralityTable$deleteRows(), silent=TRUE)
      try(centralityTable$deleteNote("norm_z"), silent=TRUE)
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
                try(centralityTable$addRow(rowKey = row_key, values = row_values), silent = TRUE)
              }
            }
            if (!is.null(groupVarName)) {
              centralityData$group <- groupName
            }
            allCentralityDataList[[groupName]] <- centralityData
          }
        }
        if (data_found_for_cent_table) {
          try(centralityTable$setNote("norm_z", "Centrality values are z-score normalized."), silent=TRUE)
        }
      } else {
        try(centralityTable$setState(NULL), silent=TRUE)
      }
      
      clusteringTable <- self$results$clustering_net
      try(clusteringTable$setState(NULL), silent=TRUE)
      try(clusteringTable$deleteRows(), silent=TRUE)
      try(clusteringTable$deleteNote("clust_norm_z"), silent=TRUE)
      allClusteringDataList <- list()
      data_found_for_clust_table = FALSE
      
      if (isTRUE(self$options$tableClustering_net) || isTRUE(self$options$plotClustering_net)) {
        current_table_cols <- private$.getColumnNames(clusteringTable)
        required_cols <- c("group", "node", "Barrat", "Onnela", "WS", "Zhang")
        cols_to_remove <- setdiff(current_table_cols, required_cols)
        for(col_name in cols_to_remove) {
          if (col_name %in% private$.getColumnNames(clusteringTable)) {
            try(clusteringTable$deleteColumn(name = col_name), silent=TRUE)
          }
        }
        current_table_cols_after_remove <- private$.getColumnNames(clusteringTable)
        cols_to_add <- setdiff(required_cols, current_table_cols_after_remove)
        if ("group" %in% cols_to_add) {
          try(clusteringTable$addColumn(name = "group", title = "Group", type = 'text', visible="(groupVar_net)"), silent = TRUE)
        }
        if ("node" %in% cols_to_add) {
          try(clusteringTable$addColumn(name = "node", title = "Variable", type = 'text'), silent = TRUE)
        }
        for(col_name in setdiff(cols_to_add, c("node", "group"))) {
          try(clusteringTable$addColumn(name = col_name, title = col_name, type = 'number'), silent = TRUE)
        }
        
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
                try(clusteringTable$addRow(rowKey = row_key, values = row_values), silent = TRUE)
              }
            }
            if (!is.null(groupVarName)) {
              clusteringData$group <- groupName
            }
            allClusteringDataList[[groupName]] <- clusteringData
          }
        }
        if (data_found_for_clust_table) {
          try(clusteringTable$setNote("clust_norm_z", "Clustering values are z-score normalized."), silent=TRUE)
        }
      } else {
        try(clusteringTable$setState(NULL), silent=TRUE)
      }
      
      if (isTRUE(self$options$plotNetwork_net) && length(networkGraphsList) > 0) {
        commonLayout <- NULL
        tryCatch({
          graphs_for_layout <- networkGraphsList[successful_groups]
          if (length(graphs_for_layout) > 0) {
            commonLayout <- do.call(qgraph::averageLayout, c(graphs_for_layout, list(layout = self$options$layoutAlgorithm_net)))
            if (!is.null(commonLayout)) {
              rownames(commonLayout) <- nodeLabels
            }
          }
        }, error = function(e) {
          jmvcore::reject(paste("Layout calculation failed:", e$message), code="layout_error")
          tryCatch({
            commonLayout <- qgraph::averageLayout(networkGraphsList[[1]], layout = "circle")
            if (!is.null(commonLayout)) {
              rownames(commonLayout) <- nodeLabels
            }
          }, error=function(e2){})
        })
        
        plotState <- list(
          graphs = networkGraphsList, 
          layout = commonLayout, 
          labels = if(isTRUE(self$options$showLabels_net)) nodeLabels else FALSE, 
          groupNames = successful_groups,
          nodeGroups = qgraph_groups
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
    },
    
    .runMixed = function() {
      message("networkanalysisClass$.runMixed() called")
      
      if (length(self$options$varsCont_mixed) == 0 && length(self$options$varsCat_mixed) == 0 && length(self$options$varsCount_mixed) == 0) {
        instructions <- self$results$instructions_mixed
        instructions$setContent("Welcome to Mixed Data Network Analysis (mgm)!\n\nPlease select at least two variables (Continuous, Categorical, or Count) to begin.")
        instructions$setVisible(TRUE)
        try(self$results$summary_mixed$setVisible(FALSE), silent=TRUE); try(self$results$weights_mixed$setVisible(FALSE), silent=TRUE)
        try(self$results$centrality_mixed$setVisible(FALSE), silent=TRUE); try(self$results$clustering_mixed$setVisible(FALSE), silent=TRUE)
        try(self$results$networkPlot_mixed$setVisible(FALSE), silent=TRUE); try(self$results$centralityPlot_mixed$setVisible(FALSE), silent=TRUE)
        try(self$results$clusteringPlot_mixed$setVisible(FALSE), silent=TRUE)
        return()
      } else {
        self$results$instructions_mixed$setVisible(FALSE)
        try(self$results$summary_mixed$setVisible(TRUE), silent=TRUE)
        try(self$results$weights_mixed$setVisible(isTRUE(self$options$tableWeights_mixed)), silent=TRUE)
        try(self$results$centrality_mixed$setVisible(isTRUE(self$options$tableCentrality_mixed)), silent=TRUE)
        try(self$results$clustering_mixed$setVisible(isTRUE(self$options$tableClustering_mixed)), silent=TRUE)
        try(self$results$networkPlot_mixed$setVisible(isTRUE(self$options$plotNetwork_mixed)), silent=TRUE)
        try(self$results$centralityPlot_mixed$setVisible(isTRUE(self$options$plotCentrality_mixed)), silent=TRUE)
        try(self$results$clusteringPlot_mixed$setVisible(isTRUE(self$options$plotClustering_mixed)), silent=TRUE)
      }
      
      varsContNames <- self$options$varsCont_mixed
      varsCatNames <- self$options$varsCat_mixed
      varsCountNames <- self$options$varsCount_mixed
      groupVarName <- self$options$groupVar_mixed
      nodeGroupVarName <- self$options$nodeGroupVar_mixed
      
      allVarNames <- c(varsContNames, varsCatNames, varsCountNames)
      if (length(allVarNames) < 2) {
        jmvcore::reject("Please select at least 2 variables in total.", code = "not_enough_variables"); return()
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
          jmvcore::reject(paste("Variable '", varName, "' contains only missing values."), code = "var_all_na"); return()
        }
        if (varName %in% varsContNames) {
          varTypes[i] <- "g"
          varLevels[i] <- 1
          if (!is.numeric(data_subset[[varName]])) {
            jmvcore::reject(paste("Continuous variable '", varName, "' is not numeric."), code = "cont_not_numeric"); return()
          }
        } else if (varName %in% varsCatNames) {
          varTypes[i] <- "c"
          if (!is.factor(data_subset[[varName]])) {
            data_subset[[varName]] <- as.factor(data_subset[[varName]])
          }
          varLevels[i] <- nlevels(data_subset[[varName]])
          if (varLevels[i] < 2) {
            jmvcore::reject(paste("Categorical variable '", varName, "' must have at least 2 levels."), code = "cat_not_enough_levels"); return()
          }
        } else if (varName %in% varsCountNames) {
          varTypes[i] <- "p"
          varLevels[i] <- 1
          if (!is.numeric(data_subset[[varName]])) {
            jmvcore::reject(paste("Count variable '", varName, "' is not numeric."), code = "count_not_numeric"); return()
          }
          if (any(data_subset[[varName]] < 0 | data_subset[[varName]] != round(data_subset[[varName]]), na.rm = TRUE)) {
            jmvcore::reject(paste("Count variable '", varName, "' must contain non-negative integers."), code = "count_invalid_values"); return()
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
      
      dataList <- list()
      groupLevels <- NULL
      if (!is.null(groupVarName) && nchar(groupVarName) > 0) {
        if (!is.factor(data_subset[[groupVarName]])) {
          data_subset[[groupVarName]] <- as.factor(data_subset[[groupVarName]])
        }
        groupLevels <- levels(data_subset[[groupVarName]])
        if (length(groupLevels) < 1) {
          jmvcore::reject("Grouping variable has no levels after NA removal.", code="no_group_levels_after_na"); return()
        }
        dataList <- split(data_subset[, allVarNames, drop=FALSE], data_subset[[groupVarName]], drop=TRUE)
      } else {
        dataList <- list(Overall = data_subset[, allVarNames, drop=FALSE])
        groupLevels <- "Overall"
      }
      
      qgraph_groups <- NULL
      if (!is.null(nodeGroupVarName)) {
        tryCatch({
          raw_labels <- as.character(data_full[[nodeGroupVarName]])
          raw_labels <- raw_labels[!is.na(raw_labels) & raw_labels != ""]
          node_group_mapping <- rep(NA, length(allVarNames))
          names(node_group_mapping) <- allVarNames
          matched_any <- FALSE
          
          for (label_str in raw_labels) {
            parts <- strsplit(label_str, "=")[[1]]
            if (length(parts) >= 2) {
              v_name <- trimws(parts[1])
              g_name <- trimws(paste(parts[-1], collapse="="))
              if (v_name %in% allVarNames) {
                node_group_mapping[v_name] <- g_name
                matched_any <- TRUE
              }
            }
          }
          if (matched_any) {
            unique_groups <- unique(na.omit(node_group_mapping))
            qgraph_groups <- list()
            for (g in unique_groups) {
              indices <- which(node_group_mapping == g)
              qgraph_groups[[g]] <- indices
            }
          }
        }, error = function(e) {
          qgraph_groups <- NULL
        })
      }
      
      if (!requireNamespace("mgm", quietly = TRUE)) { jmvcore::reject("Package 'mgm' is required. Please install it.", code = "mgm_missing"); return() }
      if (!requireNamespace("qgraph", quietly = TRUE)) { jmvcore::reject("Package 'qgraph' is required. Please install it.", code = "qgraph_missing"); return() }
      if (!requireNamespace("igraph", quietly = TRUE)) { jmvcore::reject("Package 'igraph' is required. Please install it.", code = "igraph_missing"); return() }
      if (!requireNamespace("dplyr", quietly = TRUE)) { jmvcore::reject("Package 'dplyr' is required. Please install it.", code = "dplyr_missing"); return() }
      if (!requireNamespace("gtools", quietly = TRUE)) { jmvcore::reject("Package 'gtools' is required. Please install it.", code = "gtools_missing"); return() }
      if (!requireNamespace("reshape2", quietly = TRUE)) { jmvcore::reject("Package 'reshape2' is required. Please install it.", code = "reshape2_missing"); return() }
      
      networkResultsList <- list(); weightedAdjMatrices <- list(); centralityDataList <- list(); clusteringDataList <- list(); estimation_errors <- character()
      
      for (groupName in groupLevels) {
        currentDataGroup_for_estimation <- dataList[[groupName]][, allVarNames, drop=FALSE]
        min_n_group <- max(length(allVarNames) * 2, 20)
        if (nrow(currentDataGroup_for_estimation) < min_n_group) {
          msg <- paste0("Group '", groupName, "': Insufficient data (n=", nrow(currentDataGroup_for_estimation), ", required >=", min_n_group, ") for stable estimation.")
          estimation_errors <- c(estimation_errors, msg)
          warning(msg); next
        }
        
        mgmFit <- NULL; adjMatrix <- NULL
        tryCatch({
          mgm_args <- list(data = as.matrix(currentDataGroup_for_estimation), type = varTypes, level = varLevels, 
                           lambdaSel = lambdaSelMethod, ruleReg = ruleReg, 
                           binarySign = FALSE, pbar = FALSE, warnings = TRUE)
          if (lambdaSelMethod == "CV") {
            mgm_args$k <- kFolds; mgm_args$lambdaGam <- NULL
          } else if (lambdaSelMethod == "EBIC") {
            mgm_args$lambdaGam <- lambdaGam; mgm_args$k <- NULL
          }
          
          if (!is.null(self$results$instructions_mixed)) {
            self$results$instructions_mixed$setContent(paste("Estimating network for group:", groupName, "..."))
            self$results$instructions_mixed$setVisible(TRUE)
          }
          mgmFit <- do.call(mgm::mgm, mgm_args)
          if (!is.null(self$results$instructions_mixed)) self$results$instructions_mixed$setVisible(FALSE)
          
          if (is.null(mgmFit) || !("pairwise" %in% names(mgmFit)) || 
              is.null(mgmFit$pairwise$wadj) || is.null(mgmFit$pairwise$signs)) {
            stop("mgm estimation did not return a valid 'pairwise$wadj' or 'pairwise$signs' object.")
          }
          
          adjMatrix <- mgmFit$pairwise$wadj * mgmFit$pairwise$signs
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
          temp_ei <- rowSums(adjMatrix, na.rm = TRUE)
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
          }, error = function(e) {})
          
        }, error = function(e) {
          err_msg <- paste0("Group '", groupName, "': Error during estimation - ", e$message)
          estimation_errors <<- c(estimation_errors, err_msg)
          networkResultsList[[groupName]] <<- NULL; weightedAdjMatrices[[groupName]] <<- NULL; centralityDataList[[groupName]] <<- NULL; clusteringDataList[[groupName]] <<- NULL
          if (!is.null(self$results$instructions_mixed)) self$results$instructions_mixed$setVisible(FALSE)
        })
      }
      
      if (length(estimation_errors) > 0) {
        if (length(weightedAdjMatrices) == 0) {
          jmvcore::reject(paste("Network estimation failed for all groups. Summary of errors:\n", paste(estimation_errors, collapse="\n")), code="all_groups_failed_with_errors"); return()
        }
      }
      successful_groups <- names(weightedAdjMatrices)
      if (length(successful_groups) == 0) {
        jmvcore::reject("No successful network estimations to report.", code="no_successful_groups"); return()
      }
      
      summaryTable <- self$results$summary_mixed
      try(summaryTable$setState(NULL), silent=TRUE)
      try(summaryTable$deleteRows(), silent=TRUE)
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
        if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) {
          summaryRow <- c(list(group = groupName), summaryRow)
        }
        summaryTable$addRow(rowKey = groupName, values = summaryRow)
      }
      
      weightsTable <- self$results$weights_mixed
      try(weightsTable$setState(NULL), silent=TRUE)
      try(weightsTable$deleteRows(), silent=TRUE)
      current_cols <- private$.getColumnNames(weightsTable)
      cols_to_remove <- setdiff(current_cols, c("group", "rowname"))
      for(col_name in cols_to_remove) {
        try(weightsTable$deleteColumn(name = col_name), silent=TRUE)
      }
      for (varName in allVarNames) {
        col_id_name <- paste0("var_", make.names(varName))
        if (!col_id_name %in% private$.getColumnNames(weightsTable)) {
          weightsTable$addColumn(name = col_id_name, title = varName, type = 'number', format = ".3f")
        }
      }
      
      if (isTRUE(self$options$tableWeights_mixed)) {
        for (groupName in successful_groups) {
          adjMat <- weightedAdjMatrices[[groupName]]
          for (row_node in allVarNames) {
            row_values <- list()
            if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) {
              row_values[["group"]] <- groupName
            }
            row_values[["rowname"]] <- row_node
            for (col_node in allVarNames) {
              row_values[[paste0("var_", make.names(col_node))]] <- adjMat[row_node, col_node]
            }
            weightsTable$addRow(rowKey = paste(groupName, row_node, sep="_"), values = row_values)
          }
        }
      }
      
      centralityTable <- self$results$centrality_mixed
      try(centralityTable$setState(NULL), silent=TRUE)
      try(centralityTable$deleteRows(), silent=TRUE)
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
            if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) {
              tempDataForPlot$group <- groupName
            }
            allCentralityDataForPlot[[groupName]] <- tempDataForPlot
            
            if (isTRUE(self$options$tableCentrality_mixed)) {
              for (i in 1:nrow(centralityData_Zscores)) {
                row_values <- as.list(centralityData_Zscores[i, ])
                if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) {
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
      
      clusteringTable <- self$results$clustering_mixed
      try(clusteringTable$setState(NULL), silent=TRUE)
      try(clusteringTable$deleteRows(), silent=TRUE)
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
            if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) {
              tempDataForPlot_Clust$group <- groupName
            }
            allClusteringDataForPlot[[groupName]] <- tempDataForPlot_Clust
            
            if (isTRUE(self$options$tableClustering_mixed)) {
              for (i in 1:nrow(clusteringData_Zscores)) {
                row_values <- as.list(clusteringData_Zscores[i, ])
                if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) {
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
      
      if (isTRUE(self$options$plotNetwork_mixed) && length(weightedAdjMatrices) > 0) {
        commonLayout <- NULL
        graphs_for_layout <- weightedAdjMatrices[successful_groups]
        if (length(graphs_for_layout) > 1) {
          tryCatch({
            commonLayout <- do.call(qgraph::averageLayout, c(graphs_for_layout, list(layout = self$options$layoutAlgorithm_mixed)))
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
            q_obj <- qgraph::qgraph(graphs_for_layout[[first_graph_name]], layout = self$options$layoutAlgorithm_mixed, DoNotPlot = TRUE)
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
        
        plotStateNetwork <- list(
          graphs = graphs_for_layout, 
          edgeColors = lapply(networkResultsList[successful_groups], function(fit) if(!is.null(fit) && !is.null(fit$pairwise) && !is.null(fit$pairwise$edgecolor)) fit$pairwise$edgecolor else NULL), 
          layout = commonLayout, 
          labels = if(isTRUE(self$options$showLabels_mixed)) allVarNames else FALSE, 
          groupNames = successful_groups, 
          layoutAlgorithm = self$options$layoutAlgorithm_mixed,
          nodeGroups = qgraph_groups
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
          jmvcore::reject(paste("Network plotting failed for group", groupName, ":", e$message), code = "network_plot_error")
          plot(1, type="n", axes=FALSE, xlab="", ylab="")
          text(1, 1, paste("Plot Error (", groupName, "):\n", e$message), col="red")
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
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, variable.name = "measure", value.name = "value") }, error = function(e) { NULL })
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
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, variable.name = "measure", value.name = "value") }, error = function(e) { NULL })
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
      plotState <- image$state
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
          text(1, 1, paste("Plot Error (", groupName, ")"), col="red", cex=0.8)
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
      
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, measure.vars = measure_vars, variable.name = "measure", value.name = "value") }, error = function(e) { return(NULL) })
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
      
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, measure.vars = measure_vars_clust, variable.name = "measure", value.name = "value") }, error = function(e) { return(NULL) })
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
    }
  ) 
)
