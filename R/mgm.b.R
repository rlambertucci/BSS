mgmClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "mgmClass", 
  inherit = mgmBase, 
  private = list(
    
    .getColumnNames = function(table) {
      tryCatch(sapply(table$columns, function(col) col$name), error = function(e) character(0))
    },
    
    .init = function() {
      message("mgmClass$.init() called") 
      if (isTRUE(self$options$plotNetwork)) { width <- self$options$plotWidth; height <- self$options$plotHeight; if (!is.null(self$results$networkPlot)) self$results$networkPlot$setSize(width, height) }
      if (!is.null(self$results$centralityPlot)) self$results$centralityPlot$setSize(width = 600, height = 800) 
      if (!is.null(self$results$clusteringPlot)) self$results$clusteringPlot$setSize(width = 600, height = 800) 
      if (is.null(self$options$varsCont) && is.null(self$options$varsCat) && is.null(self$options$varsCount)) { if (!is.null(self$results$instructions)) { self$results$instructions$setContent( "Welcome to Mixed Data Network Analysis (mgm)!\n\n" ) } }
    },
    
    .run = function() {
      message("mgmClass$.run() called") 
      
      if (length(self$options$varsCont) == 0 && length(self$options$varsCat) == 0 && length(self$options$varsCount) == 0) {
        if (!is.null(self$results$instructions)) { self$results$instructions$setContent( "Welcome to Mixed Data Network Analysis (mgm)!\n\nPlease select at least two variables (Continuous, Categorical, or Count) to begin."); self$results$instructions$setVisible(TRUE) }; try(self$results$summary$setVisible(FALSE), silent=TRUE); try(self$results$weights$setVisible(FALSE), silent=TRUE); try(self$results$centrality$setVisible(FALSE), silent=TRUE); try(self$results$clustering$setVisible(FALSE), silent=TRUE); try(self$results$networkPlot$setVisible(FALSE), silent=TRUE); try(self$results$centralityPlot$setVisible(FALSE), silent=TRUE); try(self$results$clusteringPlot$setVisible(FALSE), silent=TRUE); return()
      } else {
        if (!is.null(self$results$instructions)) self$results$instructions$setVisible(FALSE)
        try(self$results$summary$setVisible(TRUE), silent=TRUE); try(self$results$weights$setVisible(isTRUE(self$options$tableWeights)), silent=TRUE); try(self$results$centrality$setVisible(isTRUE(self$options$tableCentrality)), silent=TRUE); try(self$results$clustering$setVisible(isTRUE(self$options$tableClustering)), silent=TRUE); try(self$results$networkPlot$setVisible(isTRUE(self$options$plotNetwork)), silent=TRUE); try(self$results$centralityPlot$setVisible(isTRUE(self$options$plotCentrality)), silent=TRUE); try(self$results$clusteringPlot$setVisible(isTRUE(self$options$plotClustering)), silent=TRUE)
      }
      varsContNames <- self$options$varsCont; varsCatNames <- self$options$varsCat; varsCountNames <- self$options$varsCount; groupVarName <- self$options$groupVar; allVarNames <- c(varsContNames, varsCatNames, varsCountNames); if (length(allVarNames) < 2) { jmvcore::reject("Please select at least 2 variables in total.", code = "not_enough_variables"); return() }; lambdaSelMethod <- self$options$lambdaSelMethod; kFolds <- self$options$kFolds; lambdaGam <- self$options$lambdaGam; ruleReg <- self$options$ruleReg
      data_full <- self$data; columnsToSelect <- allVarNames; if (!is.null(groupVarName) && nchar(groupVarName) > 0) { columnsToSelect <- unique(c(allVarNames, groupVarName)) }; data_subset <- data_full[, columnsToSelect, drop=FALSE]; if (nrow(data_subset) == 0) { jmvcore::reject("Dataset is empty after selecting variables.", code = "empty_data_subset"); return() }; varTypes <- character(length(allVarNames)); varLevels <- numeric(length(allVarNames)); for (i in seq_along(allVarNames)) { varName <- allVarNames[i]; if (varName %in% varsContNames) { varTypes[i] <- "g"; varLevels[i] <- 1; if (!is.numeric(data_subset[[varName]])) { jmvcore::reject(paste("Continuous variable '", varName, "' is not numeric."), code = "cont_not_numeric"); return() } } else if (varName %in% varsCatNames) { varTypes[i] <- "c"; if (!is.factor(data_subset[[varName]])) { data_subset[[varName]] <- as.factor(data_subset[[varName]]) }; varLevels[i] <- nlevels(data_subset[[varName]]); if (varLevels[i] < 2) { jmvcore::reject(paste("Categorical variable '", varName, "' must have at least 2 levels."), code = "cat_not_enough_levels"); return() } } else if (varName %in% varsCountNames) { varTypes[i] <- "p"; varLevels[i] <- 1; if (!is.numeric(data_subset[[varName]])) { jmvcore::reject(paste("Count variable '", varName, "' is not numeric."), code = "count_not_numeric"); return() }; if (any(data_subset[[varName]] < 0 | data_subset[[varName]] != round(data_subset[[varName]]), na.rm = TRUE)) { jmvcore::reject(paste("Count variable '", varName, "' must contain non-negative integers."), code = "count_invalid_values"); return() } } }; na_note_global <- NULL; n_total_initial <- nrow(data_subset); cols_for_na_omit <- allVarNames; if (!is.null(groupVarName) && nchar(groupVarName) > 0) { cols_for_na_omit <- c(allVarNames, groupVarName) }; data_subset_for_na <- data_subset[, cols_for_na_omit, drop = FALSE]; complete_cases_indices <- stats::complete.cases(data_subset_for_na); data_subset <- data_subset[complete_cases_indices, , drop = FALSE]; n_after_na_global <- nrow(data_subset); if (n_after_na_global < n_total_initial) { na_note_global <- paste(n_total_initial - n_after_na_global, "cases excluded due to missing values (listwise deletion based on selected variables and group variable).") }; if (nrow(data_subset) < 10) { jmvcore::reject("Insufficient data after listwise deletion of missing values (n < 10).", code = "insufficient_data_after_na"); return() }; dataList <- list(); groupLevels <- NULL; if (!is.null(groupVarName) && nchar(groupVarName) > 0) { if (!is.factor(data_subset[[groupVarName]])) { data_subset[[groupVarName]] <- as.factor(data_subset[[groupVarName]]) }; groupLevels <- levels(data_subset[[groupVarName]]); if (length(groupLevels) < 1) { jmvcore::reject("Grouping variable has no levels after NA removal.", code="no_group_levels_after_na"); return() }; dataList <- split(data_subset[, allVarNames, drop=FALSE], data_subset[[groupVarName]], drop=TRUE) } else { dataList <- list(Overall = data_subset[, allVarNames, drop=FALSE]); groupLevels <- "Overall" }
      if (!requireNamespace("mgm", quietly = TRUE)) { jmvcore::reject("Package 'mgm' is required. Please install it.", code = "mgm_missing"); return() }; if (!requireNamespace("qgraph", quietly = TRUE)) { jmvcore::reject("Package 'qgraph' is required. Please install it.", code = "qgraph_missing"); return() }; if (!requireNamespace("igraph", quietly = TRUE)) { jmvcore::reject("Package 'igraph' is required. Please install it.", code = "igraph_missing"); return() }; if (!requireNamespace("dplyr", quietly = TRUE)) { jmvcore::reject("Package 'dplyr' is required. Please install it.", code = "dplyr_missing"); return() }; if (!requireNamespace("gtools", quietly = TRUE)) { jmvcore::reject("Package 'gtools' is required. Please install it.", code = "gtools_missing"); return() }; if (!requireNamespace("reshape2", quietly = TRUE)) { jmvcore::reject("Package 'reshape2' is required. Please install it.", code = "reshape2_missing"); return() }
      
      networkResultsList <- list(); weightedAdjMatrices <- list(); centralityDataList <- list(); clusteringDataList <- list(); estimation_errors <- character() 
      
      for (groupName in groupLevels) {
        message(paste("Processing group:", groupName)); currentDataGroup_for_estimation <- dataList[[groupName]][, allVarNames, drop=FALSE]; min_n_group <- max(length(allVarNames) * 2, 20); if (nrow(currentDataGroup_for_estimation) < min_n_group ) { msg <- paste0("Group '", groupName, "': Insufficient data (n=", nrow(currentDataGroup_for_estimation), ", required >=", min_n_group, ") for stable estimation."); estimation_errors <- c(estimation_errors, msg); warning(msg); next }
        
        mgmFit <- NULL; adjMatrix <- NULL;
        
        tryCatch({ 
          message(paste("Group:", groupName, "Using mgm::mgm for estimation."))
          mgm_args <- list(data = as.matrix(currentDataGroup_for_estimation), type = varTypes, level = varLevels, 
                           lambdaSel = self$options$lambdaSelMethod, ruleReg = self$options$ruleReg, 
                           binarySign = FALSE, pbar = FALSE, warnings = TRUE)
          if (self$options$lambdaSelMethod == "CV") { mgm_args$k <- self$options$kFolds; mgm_args$lambdaGam <- NULL } 
          else if (self$options$lambdaSelMethod == "EBIC") { mgm_args$lambdaGam <- self$options$lambdaGam; mgm_args$k <- NULL }
          
          if (!is.null(self$results$instructions)) { self$results$instructions$setContent(paste("Estimating network for group:", groupName, "...")); self$results$instructions$setVisible(TRUE) }
          mgmFit <- do.call(mgm::mgm, mgm_args)
          if (!is.null(self$results$instructions)) self$results$instructions$setVisible(FALSE)
          
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
          message(paste("Group:", groupName, "Applied two-step symmetrization to adjMatrix."))
          
          if (any(!is.finite(adjMatrix))) {
            warning(paste("Group:", groupName, "adjMatrix contains NA/NaN/Inf values after symmetrization. Replacing with 0 for igraph compatibility."))
            adjMatrix[!is.finite(adjMatrix)] <- 0
          }
          
          networkResultsList[[groupName]] <- mgmFit 
          weightedAdjMatrices[[groupName]] <- adjMatrix
          
          message(paste("Final Symmetrized Adjacency matrix for group", groupName, "BEFORE igraph:"))
          if (nrow(adjMatrix) < 10 && ncol(adjMatrix) < 10) { print(round(adjMatrix,4)) } else { print(summary(as.vector(adjMatrix))); message(paste("Number of non-zero edges:", sum(adjMatrix != 0, na.rm = TRUE))) }
          if (!isSymmetric(adjMatrix, tol = .Machine$double.eps^0.5)) { warning(paste("Group:", groupName, "adjMatrix is STILL NOT perfectly symmetric before igraph call. This is problematic.")) }
          
          num_vars <- length(allVarNames)
          strength_vals <- rep(NA_real_, num_vars); closeness_vals <- rep(NA_real_, num_vars); betweenness_vals <- rep(NA_real_, num_vars); ei_vals_final <- rep(NA_real_, num_vars)
          cent_obj_qgraph <- NULL; 
          tryCatch({ 
            cent_obj_qgraph <- qgraph::centrality(adjMatrix, weighted = TRUE, signed = TRUE, all.shortest.paths = FALSE) 
          }, error = function(e_cent) { 
            warning(paste("qgraph::centrality failed for group", groupName, ":", e_cent$message)); 
            na_vec <- rep(NA_real_, num_vars); 
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
          temp_ei <- rowSums(adjMatrix, na.rm = TRUE); 
          if (length(temp_ei) == num_vars) { 
            ei_vals_final <- temp_ei 
          } else { 
            warning(paste("rowSums(adjMatrix) for EI resulted in incorrect length for group", groupName)) 
          }
          centralityDataRaw <- data.frame( variable = allVarNames, strength = strength_vals, closeness = closeness_vals, betweenness = betweenness_vals, expectedInfluence = ei_vals_final ); centralityDataList[[groupName]] <- centralityDataRaw 
          
          message(paste("Calculating clustering for group", groupName)); 
          igraph_obj <- NULL
          tryCatch({
            igraph_obj <- igraph::graph_from_adjacency_matrix( adjMatrix, mode = "undirected", weighted = TRUE, diag = FALSE )
          }, error = function(e_igraph) { stop(paste("Failed to create igraph object for group", groupName, ":", e_igraph$message, "\nMatrix isSymmetric check:", isSymmetric(adjMatrix, tol = .Machine$double.eps^0.5) )) })
          
          barrat_raw <- rep(NA_real_, num_vars); ws_raw <- rep(NA_real_, num_vars); 
          edge_weights_for_igraph <- NULL
          if (!is.null(igraph_obj) && igraph::ecount(igraph_obj) > 0 && igraph::is_weighted(igraph_obj)) { edge_weights_for_igraph <- abs(igraph::E(igraph_obj)$weight) }
          
          if (!is.null(igraph_obj)){
            tryCatch({ barrat_raw <- igraph::transitivity(igraph_obj, type = "barrat", weights = edge_weights_for_igraph) }, error = function(e) { warning(paste("Barrat clustering failed for group", groupName, ":", e$message)) })
            tryCatch({ ws_raw     <- igraph::transitivity(igraph_obj, type = "local",    weights = edge_weights_for_igraph) }, error = function(e) { warning(paste("Local (WS) clustering failed for group", groupName, ":", e$message)) })
          }
          fix_coeffs <- function(coeffs, expected_len) { if (length(coeffs) != expected_len) coeffs <- rep(NA_real_, expected_len); coeffs[is.na(coeffs)] <- 0; return(coeffs) }; 
          barrat_raw <- fix_coeffs(barrat_raw, num_vars); 
          ws_raw <- fix_coeffs(ws_raw, num_vars); 
          clusteringDataRaw <- data.frame(node = allVarNames, barrat = barrat_raw, ws = ws_raw); rownames(clusteringDataRaw) <- NULL; clusteringDataList[[groupName]] <- clusteringDataRaw 
          
        }, error = function(e) {
          err_msg <- paste0("Group '", groupName, "': Error during estimation/processing - ", e$message); estimation_errors <<- c(estimation_errors, err_msg); message(paste("ERROR captured for group", groupName, ":", e$message)); networkResultsList[[groupName]] <<- NULL; weightedAdjMatrices[[groupName]] <<- NULL; centralityDataList[[groupName]] <<- NULL; clusteringDataList[[groupName]] <<- NULL; if (!is.null(self$results$instructions)) self$results$instructions$setVisible(FALSE)
        }) 
      } 
      
      if (length(estimation_errors) > 0) { if (length(weightedAdjMatrices) == 0) { jmvcore::reject(paste("Network estimation failed for all groups. Check R console for specific group errors. Summary of errors:\n", paste(estimation_errors, collapse="\n")), code="all_groups_failed_with_errors"); return() } else { warning(paste("Errors occurred for some groups during network estimation. Results are for successful groups only. Errors:\n", paste(estimation_errors, collapse="\n"))) } }; if (length(weightedAdjMatrices) == 0 && length(estimation_errors) == 0) { jmvcore::reject("Network estimation did not succeed for any group (no specific errors reported).", code="all_groups_failed_silently"); return() }; successful_groups <- names(weightedAdjMatrices); if (length(successful_groups) == 0) { jmvcore::reject("No successful network estimations to report.", code="no_successful_groups"); return() }
      summaryTable <- self$results$summary; summaryTable$setState(NULL); if (!is.null(na_note_global)) { summaryTable$setNote("na_handling_summary", na_note_global) } else { summaryTable$setNote("na_handling_summary", NULL) }; for (groupName in successful_groups) { adjMat <- weightedAdjMatrices[[groupName]]; nNodes <- ncol(adjMat); nonZeroEdges <- sum(abs(adjMat[upper.tri(adjMat)]) > 1e-10, na.rm = TRUE); possibleEdges <- nNodes * (nNodes - 1) / 2; sparsity_val <- if(possibleEdges > 0) 1 - (nonZeroEdges / possibleEdges) else 1; summaryRow <- list(nodes = nNodes, edges = paste(nonZeroEdges, "/", possibleEdges), sparsity = sparsity_val); if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) { summaryRow <- c(list(group = groupName), summaryRow) }; summaryTable$addRow(rowKey = groupName, values = summaryRow) }
      weightsTable <- self$results$weights; weightsTable$setState(NULL); current_cols <- private$.getColumnNames(weightsTable); cols_to_remove <- setdiff(current_cols, c("group", "rowname")); for(col_name in cols_to_remove) { try(weightsTable$deleteColumn(name = col_name), silent=TRUE) }; for (varName in allVarNames) { col_id_name <- paste0("var_", make.names(varName)); if (!col_id_name %in% private$.getColumnNames(weightsTable)) { weightsTable$addColumn(name = col_id_name, title = varName, type = 'number', format = ".3f") } }; if (isTRUE(self$options$tableWeights)) { for (groupName in successful_groups) { adjMat <- weightedAdjMatrices[[groupName]]; for (row_node in allVarNames) { row_values <- list(); if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) { row_values[["group"]] <- groupName }; row_values[["rowname"]] <- row_node; for (col_node in allVarNames) { row_values[[paste0("var_", make.names(col_node))]] <- adjMat[row_node, col_node] }; weightsTable$addRow(rowKey = paste(groupName, row_node, sep="_"), values = row_values) } } }
      centralityTable <- self$results$centrality; centralityTable$setState(NULL); allCentralityDataForPlot <- list(); if (isTRUE(self$options$tableCentrality) || isTRUE(self$options$plotCentrality)) { data_found_for_cent_table = FALSE; for (groupName in successful_groups) { centralityData_raw <- centralityDataList[[groupName]]; if (!is.null(centralityData_raw) && nrow(centralityData_raw) > 0) { data_found_for_cent_table = TRUE; centralityData_Zscores <- centralityData_raw; cols_to_norm_cent <- c("strength", "closeness", "betweenness", "expectedInfluence"); for (col in cols_to_norm_cent) { if (col %in% names(centralityData_Zscores) && is.numeric(centralityData_Zscores[[col]])) { valid_vals <- centralityData_Zscores[[col]][is.finite(centralityData_Zscores[[col]])]; if (length(valid_vals) > 1) { mean_val <- mean(valid_vals, na.rm = TRUE); sd_val <- stats::sd(valid_vals, na.rm = TRUE); if (!is.na(sd_val) && sd_val > 1e-10) { centralityData_Zscores[[col]] <- (centralityData_Zscores[[col]] - mean_val) / sd_val } else { centralityData_Zscores[[col]] <- centralityData_Zscores[[col]] - mean_val } } else if (length(valid_vals) == 1) { centralityData_Zscores[[col]] <- 0 } else { centralityData_Zscores[[col]] <- NA_real_ } } }; tempDataForPlot <- centralityData_Zscores; if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) { tempDataForPlot$group <- groupName }; allCentralityDataForPlot[[groupName]] <- tempDataForPlot; if (isTRUE(self$options$tableCentrality)) { for (i in 1:nrow(centralityData_Zscores)) { row_values <- as.list(centralityData_Zscores[i, ]); if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) { row_values <- c(list(group = groupName), row_values) }; centralityTable$addRow(rowKey = paste(groupName, centralityData_Zscores$variable[i], sep="_"), values = row_values) } } } else { allCentralityDataForPlot[[groupName]] <- NULL } }; if (data_found_for_cent_table && isTRUE(self$options$tableCentrality)) { centralityTable$setNote("centrality_info", "Values in table and plot are Z-score normalized if multiple variables/values exist within a group.") } else { centralityTable$setNote("centrality_info", NULL) } }
      clusteringTable <- self$results$clustering; clusteringTable$setState(NULL); allClusteringDataForPlot <- list(); if (isTRUE(self$options$tableClustering) || isTRUE(self$options$plotClustering)) { data_found_for_clust_table = FALSE; for (groupName in successful_groups) { clusteringData_raw <- clusteringDataList[[groupName]]; if (!is.null(clusteringData_raw) && nrow(clusteringData_raw) > 0) { data_found_for_clust_table = TRUE; clusteringData_Zscores <- clusteringData_raw; 
      cols_to_norm_clust <- c("barrat", "ws"); # MODIFICADO: "onnela" removido
      for (col in cols_to_norm_clust) { if (col %in% names(clusteringData_Zscores) && is.numeric(clusteringData_Zscores[[col]])) { valid_vals <- clusteringData_Zscores[[col]][is.finite(clusteringData_Zscores[[col]])]; if (length(valid_vals) > 1) { mean_val <- mean(valid_vals, na.rm = TRUE); sd_val <- stats::sd(valid_vals, na.rm = TRUE); if (!is.na(sd_val) && sd_val > 1e-10) { clusteringData_Zscores[[col]] <- (clusteringData_Zscores[[col]] - mean_val) / sd_val } else { clusteringData_Zscores[[col]] <- clusteringData_Zscores[[col]] - mean_val } } else if (length(valid_vals) == 1) { clusteringData_Zscores[[col]] <- 0 } else { clusteringData_Zscores[[col]] <- NA_real_ } } }; tempDataForPlot_Clust <- clusteringData_Zscores; if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) { tempDataForPlot_Clust$group <- groupName }; allClusteringDataForPlot[[groupName]] <- tempDataForPlot_Clust; if (isTRUE(self$options$tableClustering)) { for (i in 1:nrow(clusteringData_Zscores)) { row_values <- as.list(clusteringData_Zscores[i, ]); if ( (length(groupLevels) > 1 || !is.null(groupVarName)) && groupLevels[1] != "Overall" ) { row_values <- c(list(group = groupName), row_values) }; clusteringTable$addRow(rowKey = paste(groupName, clusteringData_Zscores$node[i], sep="_"), values = row_values) } } } else { allClusteringDataForPlot[[groupName]] <- NULL } }; if (data_found_for_clust_table && isTRUE(self$options$tableClustering)) { clusteringTable$setNote("clustering_info", "Values in table and plot are Z-score normalized if multiple variables/values exist within a group.") } else { clusteringTable$setNote("clustering_info", NULL) } }
      
      if (isTRUE(self$options$plotNetwork) && length(weightedAdjMatrices) > 0) { message("Preparing state for Network Plot"); commonLayout <- NULL; graphs_for_layout <- weightedAdjMatrices[successful_groups]; if (length(graphs_for_layout) > 1) { tryCatch({ commonLayout <- qgraph::averageLayout(graphs_for_layout, layout = self$options$layoutAlgorithm); if (!is.null(commonLayout) && !is.null(allVarNames) && length(allVarNames) == nrow(commonLayout)) rownames(commonLayout) <- allVarNames }, error = function(e) { warning(paste("Could not compute average layout:", e$message)); commonLayout <- NULL }) }; if (is.null(commonLayout) && length(graphs_for_layout) > 0) { first_graph_name <- names(graphs_for_layout)[1]; tryCatch({ q_obj <- qgraph::qgraph(graphs_for_layout[[first_graph_name]], layout = self$options$layoutAlgorithm, DoNotPlot = TRUE); commonLayout <- q_obj$layout; if (!is.null(commonLayout) && !is.null(allVarNames) && length(allVarNames) == nrow(commonLayout)) rownames(commonLayout) <- allVarNames }, error = function(e) { warning(paste("Layout for first graph failed:", e$message, "- using circle.")); tryCatch({ q_obj_circ <- qgraph::qgraph(graphs_for_layout[[first_graph_name]], layout = "circle", DoNotPlot = TRUE); commonLayout <- q_obj_circ$layout; if (!is.null(commonLayout) && !is.null(allVarNames) && length(allVarNames) == nrow(commonLayout)) rownames(commonLayout) <- allVarNames }, error = function(e2) { warning(paste("Circle layout fallback also failed:", e2$message)); commonLayout <- NULL }) }) }; plotStateNetwork <- list(graphs = graphs_for_layout, edgeColors = lapply(networkResultsList[successful_groups], function(fit) if(!is.null(fit) && !is.null(fit$pairwise) && !is.null(fit$pairwise$edgecolor)) fit$pairwise$edgecolor else NULL), layout = commonLayout, labels = if(isTRUE(self$options$showLabels)) allVarNames else FALSE, groupNames = successful_groups, layoutAlgorithm = self$options$layoutAlgorithm); self$results$networkPlot$setState(plotStateNetwork) }
      if (isTRUE(self$options$plotCentrality) && length(allCentralityDataForPlot) > 0) { valid_cent_data_for_plot <- Filter(Negate(is.null), allCentralityDataForPlot); if (length(valid_cent_data_for_plot) > 0) { combinedCentralityData <- do.call(rbind, c(valid_cent_data_for_plot, make.row.names = FALSE)); if (!is.null(combinedCentralityData) && nrow(combinedCentralityData) > 0) { self$results$centralityPlot$setState(combinedCentralityData) } else { self$results$centralityPlot$setState(NULL) } } else { self$results$centralityPlot$setState(NULL) } } else { if (!is.null(self$results$centralityPlot)) self$results$centralityPlot$setState(NULL) }
      if (isTRUE(self$options$plotClustering) && length(allClusteringDataForPlot) > 0) { valid_clust_data_for_plot <- Filter(Negate(is.null), allClusteringDataForPlot); if (length(valid_clust_data_for_plot) > 0) { combinedClusteringData <- do.call(rbind, c(valid_clust_data_for_plot, make.row.names = FALSE)); if (!is.null(combinedClusteringData) && nrow(combinedClusteringData) > 0) { self$results$clusteringPlot$setState(combinedClusteringData) } else { self$results$clusteringPlot$setState(NULL) } } else { self$results$clusteringPlot$setState(NULL) } } else { if (!is.null(self$results$clusteringPlot)) self$results$clusteringPlot$setState(NULL) }
      
      message("mgmClass$.run() completed")
    }, 
    
    .plotNetwork = function(image, ggtheme, theme, ...) { plotState <- image$state; if (is.null(plotState) || is.null(plotState$graphs) || length(plotState$graphs) == 0) { return(FALSE) }; if (!requireNamespace("qgraph", quietly = TRUE)) { plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "qgraph package missing.", col="red"); return(FALSE) }; graphsToPlot <- plotState$graphs; edgeColorsList <- plotState$edgeColors; layout <- plotState$layout; labels <- plotState$labels; groupNames <- plotState$groupNames; numPlots <- length(graphsToPlot); fncLayoutAlgorithm <- "spring"; if (!is.null(plotState$layoutAlgorithm)) fncLayoutAlgorithm <- plotState$layoutAlgorithm; if (is.null(layout) && length(graphsToPlot) > 0 && !is.null(graphsToPlot[[1]])) { warning(".plotNetwork: Layout was NULL, attempting default."); tryCatch({ q_obj <- qgraph::qgraph(graphsToPlot[[1]], layout = fncLayoutAlgorithm, DoNotPlot = TRUE); layout <- q_obj$layout; if (!is.null(layout) && is.character(labels) && length(labels) == nrow(layout)) rownames(layout) <- labels }, error = function(e_layout) { warning(paste("Failed to compute fallback layout in .plotNetwork:", e_layout$message)); plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "Layout Error.", col="red"); return(FALSE) }) }; if (is.null(layout)) { warning("Layout is still NULL in .plotNetwork."); plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "Layout Unavailable.", col="red"); return(FALSE) }; old_par <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old_par)); if (numPlots > 1) { nc <- if (numPlots > 3) 2 else numPlots; nr <- ceiling(numPlots / nc); graphics::par(mfrow = c(nr, nc), mar = c(1,1,2,1)) } else { graphics::par(mar = c(2,2,2,2)) }; graphics::par(bg = "transparent"); plotOk <- TRUE; for (i in 1:numPlots) { groupName <- groupNames[i]; currentGraph <- graphsToPlot[[groupName]]; currentEdgeColors <- NULL; if (!is.null(edgeColorsList) && length(edgeColorsList) >= i && !is.null(edgeColorsList[[i]])) { currentEdgeColors <- edgeColorsList[[i]] }; if (is.null(currentGraph)) next; currentLabels <- labels; if (is.character(labels) && length(labels) != ncol(currentGraph)) { warning(paste("Label length mismatch for group", groupName, ". Using node numbers.")); currentLabels <- 1:ncol(currentGraph) }; max_val_abs <- max(abs(unlist(graphsToPlot)), na.rm=TRUE); if (!is.finite(max_val_abs) || max_val_abs == 0) max_val_abs <- 1; tryCatch({ qgraph::qgraph(input = currentGraph, layout = layout, labels = currentLabels, edge.color = currentEdgeColors, theme = "colorblind", DoNotPlot = FALSE, title = groupName, mar = graphics::par("mar"), legend = FALSE, cut = 0, maximum = max_val_abs) }, error = function(e) { warning(paste("Network plotting failed for group", groupName, ":", e$message)); plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, paste("Plot Error (", groupName, ")"), col="red", cex=0.8); plotOk <<- FALSE }) }; return(plotOk) },
    .plotCentrality = function(image, ggtheme, theme, ...) { plotData <- image$state; if (is.null(plotData) || nrow(plotData) == 0) return(FALSE); id_vars <- intersect(c("variable", "group"), names(plotData)); if (length(id_vars) == 0 || !"variable" %in% id_vars) { warning("Centrality plot data missing 'variable'."); return(FALSE) }; measure_vars <- intersect(c("strength", "closeness", "betweenness", "expectedInfluence"), names(plotData)); if (length(measure_vars) == 0) { warning("No centrality measures in plot data."); return(FALSE) }; plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, measure.vars = measure_vars, variable.name = "measure", value.name = "value") }, error = function(e) { warning(paste("Error melting centrality data:", e$message)); return(NULL) }); if (is.null(plotDataLong) || nrow(plotDataLong) == 0) return(FALSE); plotDataLong <- plotDataLong[!is.na(plotDataLong$value), ]; if (nrow(plotDataLong) == 0) return(FALSE); measure_order <- c("strength", "closeness", "betweenness", "expectedInfluence"); measure_labels_map <- c(strength="Strength", closeness="Closeness", betweenness="Betweenness", expectedInfluence="Expected Influence"); present_measures <- intersect(measure_order, unique(as.character(plotDataLong$measure))); plotDataLong$measure <- factor(plotDataLong$measure, levels = present_measures, labels = measure_labels_map[present_measures]); y_axis_levels_ordered <- gtools::mixedsort(unique(as.character(plotDataLong$variable)), decreasing = FALSE); plotDataLong$variable <- factor(plotDataLong$variable, levels = rev(y_axis_levels_ordered)); use_group <- "group" %in% names(plotDataLong) && length(unique(plotDataLong$group)) > 1; if (use_group) { plotDataLong <- dplyr::arrange(plotDataLong, measure, group, variable) } else { plotDataLong <- dplyr::arrange(plotDataLong, measure, variable) }; if (use_group) { p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable, colour = group, linetype = group)) } else { p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable)) }; path_group_aes <- if (use_group) ggplot2::aes(group = interaction(measure, group)) else ggplot2::aes(group = measure); gg_version <- utils::packageVersion("ggplot2"); path_param <- if (gg_version >= "3.4.0") list(linewidth = 0.8) else list(size = 0.8); p <- p + do.call(ggplot2::geom_path, c(list(mapping = path_group_aes, alpha = 0.6), path_param)); point_aes <- if (use_group) ggplot2::aes(shape = group) else ggplot2::aes(); p <- p + ggplot2::geom_point(mapping = point_aes, size = 2.5); if (use_group) { p <- p + ggplot2::labs(colour = "Group", linetype = "Group", shape = "Group") }; p <- p + ggplot2::facet_wrap(~ measure, scales = "free_x") + ggtheme + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA), plot.background = ggplot2::element_rect(fill = "transparent", colour = NA), strip.background = ggplot2::element_rect(fill="grey90", color="grey50")) + ggplot2::labs(x = "Value (Z-score)", y = "Variable") + ggplot2::ggtitle("Centrality Measures"); if (!use_group) { p <- p + ggplot2::theme(legend.position = "none") }; print(p); TRUE },
    .plotClustering = function(image, ggtheme, theme, ...) { 
      plotData <- image$state; 
      if (is.null(plotData) || nrow(plotData) == 0) return(FALSE); 
      if ("node" %in% names(plotData) && !"variable" %in% names(plotData)) { plotData$variable <- plotData$node; plotData$node <- NULL }; 
      id_vars <- intersect(c("variable", "group"), names(plotData)); 
      measure_vars_clust <- intersect(c("barrat", "ws"), names(plotData)); # MODIFICADO: "onnela" removido
      if (length(id_vars) == 0 || !"variable" %in% id_vars) { warning("Clustering plot data missing 'variable' (node)."); return(FALSE) }; 
      if (length(measure_vars_clust) == 0) { warning("No clustering measures (barrat, ws) found in plot data."); return(FALSE) }; 
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, measure.vars = measure_vars_clust, variable.name = "measure", value.name = "value") }, error = function(e) { warning(paste("Error melting clustering data:", e$message)); return(NULL) }); 
      if (is.null(plotDataLong) || nrow(plotDataLong) == 0) return(FALSE); 
      plotDataLong <- plotDataLong[!is.na(plotDataLong$value), ]; 
      if (nrow(plotDataLong) == 0) return(FALSE); 
      clust_measure_order <- c("barrat", "ws"); # MODIFICADO: "onnela" removido
      clust_measure_labels_map <- c(barrat="Barrat", ws="Local (WS)"); # MODIFICADO: "onnela" removido
      present_clust_measures <- intersect(clust_measure_order, unique(as.character(plotDataLong$measure))); 
      plotDataLong$measure <- factor(plotDataLong$measure, levels = present_clust_measures, labels = clust_measure_labels_map[present_clust_measures]); 
      y_axis_levels_ordered <- gtools::mixedsort(unique(as.character(plotDataLong$variable)), decreasing = FALSE); 
      plotDataLong$variable <- factor(plotDataLong$variable, levels = rev(y_axis_levels_ordered)); 
      use_group <- "group" %in% names(plotDataLong) && length(unique(plotDataLong$group)) > 1; 
      if (use_group) { plotDataLong <- dplyr::arrange(plotDataLong, measure, group, variable) } else { plotDataLong <- dplyr::arrange(plotDataLong, measure, variable) }; 
      if (use_group) { p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable, colour = group, linetype = group)) } else { p <- ggplot2::ggplot(plotDataLong, ggplot2::aes(x = value, y = variable)) }; 
      path_group_aes <- if (use_group) ggplot2::aes(group = interaction(measure, group)) else ggplot2::aes(group = measure); 
      gg_version <- utils::packageVersion("ggplot2"); 
      path_param <- if (gg_version >= "3.4.0") list(linewidth = 0.8) else list(size = 0.8); 
      p <- p + do.call(ggplot2::geom_path, c(list(mapping = path_group_aes, alpha = 0.6), path_param)); 
      point_aes <- if (use_group) ggplot2::aes(shape = group) else ggplot2::aes(); 
      p <- p + ggplot2::geom_point(mapping = point_aes, size = 2.5); 
      if (use_group) { p <- p + ggplot2::labs(colour = "Group", linetype = "Group", shape = "Group") }; 
      p <- p + ggplot2::facet_wrap(~ measure, scales = "free_x") + ggtheme + 
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA), 
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA), 
                       strip.background = ggplot2::element_rect(fill="grey90", color="grey50")) + 
        ggplot2::labs(x = "Clustering Coefficient (Z-score)", y = "Variable") + 
        ggplot2::ggtitle("Clustering Measures"); 
      if (!use_group) { p <- p + ggplot2::theme(legend.position = "none") }; 
      print(p); 
      TRUE 
    }
  ) 
)