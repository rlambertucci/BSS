NetworkClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "NetworkClass",
  inherit = NetworkBase,
  private = list(
    
    .getColumnNames = function(table) {
      tryCatch(sapply(table$columns, function(col) col$name), error = function(e) character(0))
    },
    
    .init = function() {
      if (isTRUE(self$options$plotNetwork)) {
        width <- self$options$plotWidth
        height <- self$options$plotHeight
        self$results$networkPlot$setSize(width, height)
      }
      self$results$centralityPlot$setSize(width = 500, height = 800)
      self$results$clusteringPlot$setSize(width = 500, height = 600)
      
      summaryTable <- self$results$summary
      if (!"group" %in% private$.getColumnNames(summaryTable)) {
        try(summaryTable$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar)"), silent = TRUE)
      }
      centralityTable <- self$results$centrality
      if (!"group" %in% private$.getColumnNames(centralityTable)) {
        try(centralityTable$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar)"), silent = TRUE)
      }
      if (!"variable" %in% private$.getColumnNames(centralityTable)) {
        try(centralityTable$addColumn(name = "variable", title = "Variable", type = 'text'), silent = TRUE)
      }
      clusteringTable <- self$results$clustering
      if (!"group" %in% private$.getColumnNames(clusteringTable)) {
        try(clusteringTable$insertColumn(1, name = "group", title = "Group", type = 'text', visible="(groupVar)"), silent = TRUE)
      }
      if (!"node" %in% private$.getColumnNames(clusteringTable)) {
        try(clusteringTable$addColumn(name = "node", title = "Variable", type = 'text'), silent = TRUE)
      }
      if (is.null(self$results$instructions)) {
      }
      
    },
    
    .run = function() {
      
      if (length(self$options$vars) < 3) {
        instructions <- self$results$instructions
        instructions$setContent("Welcome to Network Analysis!\n\nPlease select at least 3 variables in the 'Variables' box to begin.")
        instructions$setVisible(TRUE)
        try(self$results$summary$setVisible(FALSE), silent=TRUE); try(self$results$weights$setVisible(FALSE), silent=TRUE)
        try(self$results$centrality$setVisible(FALSE), silent=TRUE); try(self$results$clustering$setVisible(FALSE), silent=TRUE)
        try(self$results$networkPlot$setVisible(FALSE), silent=TRUE); try(self$results$centralityPlot$setVisible(FALSE), silent=TRUE)
        try(self$results$clusteringPlot$setVisible(FALSE), silent=TRUE)
        return()
      } else {
        self$results$instructions$setVisible(FALSE)
        try(self$results$summary$setVisible(TRUE), silent=TRUE)
      }
      
      vars <- self$options$vars
      groupVarName <- self$options$groupVar
      estimator <- self$options$estimator
      corMethod <- self$options$corMethod
      
      data_full <- self$data
      
      dataList <- list()
      groupLevels <- NULL
      na_note <- NULL
      
      columnsToSelect <- vars
      if (!is.null(groupVarName)) {
        columnsToSelect <- c(vars, groupVarName)
      }
      
      data_subset <- data_full[, columnsToSelect, drop=FALSE]
      
      if (nrow(data_subset) == 0) { jmvcore::reject("Dataset is empty.", code = "empty_data"); return() }
      
      for (var in vars) {
        if (!is.numeric(data_subset[[var]])) {
          jmvcore::reject(paste("Variable '", var, "' is not numeric."), code = "var_not_numeric"); return()
        }
      }
      
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
      
      networkGraphsList <- list()
      centralityDataList <- list()
      clusteringDataList <- list()
      estimation_errors <- list()
      
      if (!requireNamespace("bootnet", quietly = TRUE)) { jmvcore::reject("Package 'bootnet' required.", code = "bootnet_missing"); return() }
      if (!requireNamespace("qgraph", quietly = TRUE)) { jmvcore::reject("Package 'qgraph' required.", code = "qgraph_missing"); return() }
      if (!requireNamespace("igraph", quietly = TRUE)) { jmvcore::reject("Package 'igraph' required.", code = "igraph_missing"); return() }
      
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
                if (!is.na(sd_val) && sd_val > 1e-10) { finite_indices <- which(is.finite(centralityDataNorm[[col]])); centralityDataNorm[[col]][finite_indices] <- (centralityDataNorm[[col]][finite_indices] - mean_val) / sd_val }
                else { finite_indices <- which(is.finite(centralityDataNorm[[col]])); if (length(unique(valid_vals)) == 1) { centralityDataNorm[[col]][finite_indices] <- 0 } else { centralityDataNorm[[col]][finite_indices] <- centralityDataNorm[[col]][finite_indices] - mean_val } }
              } else if (length(valid_vals) == 1) { finite_indices <- which(is.finite(centralityDataNorm[[col]])); centralityDataNorm[[col]][finite_indices] <- 0 }
            }
          }
          centralityDataList[[groupName]] <- centralityDataNorm
          
          igraph_obj <- igraph::graph_from_adjacency_matrix(networkGraph, mode = "undirected", weighted = TRUE, diag = FALSE)
          edge_weights <- igraph::E(igraph_obj)$weight
          barrat_raw <- suppressWarnings(igraph::transitivity(igraph_obj, type = "barrat", weights = edge_weights))
          onnela_raw <- suppressWarnings(igraph::transitivity(igraph_obj, type = "weighted", weights = edge_weights))
          ws_raw     <- suppressWarnings(igraph::transitivity(igraph_obj, type = "local", weights = edge_weights))
          clusteringDataRaw <- data.frame(node = nodeLabels, Barrat = barrat_raw, Onnela = onnela_raw, WS = ws_raw)
          rownames(clusteringDataRaw) <- NULL; clusteringDataNorm <- clusteringDataRaw
          cols_to_norm_clust <- c("Barrat", "Onnela", "WS")
          for (col in cols_to_norm_clust) {
            if (col %in% names(clusteringDataNorm)) {
              valid_vals <- clusteringDataNorm[[col]][is.finite(clusteringDataNorm[[col]])]
              if (length(valid_vals) > 1) {
                mean_val <- mean(valid_vals)
                sd_val <- stats::sd(valid_vals)
                if (!is.na(sd_val) && sd_val > 1e-10) { finite_indices <- which(is.finite(clusteringDataNorm[[col]])); clusteringDataNorm[[col]][finite_indices] <- (clusteringDataNorm[[col]][finite_indices] - mean_val) / sd_val }
                else { finite_indices <- which(is.finite(clusteringDataNorm[[col]])); if (length(unique(valid_vals)) == 1) { clusteringDataNorm[[col]][finite_indices] <- 0 } else { clusteringDataNorm[[col]][finite_indices] <- clusteringDataNorm[[col]][finite_indices] - mean_val } }
              } else if (length(valid_vals) == 1) { finite_indices <- which(is.finite(clusteringDataNorm[[col]])); clusteringDataNorm[[col]][finite_indices] <- 0 }
            }
          }
          clusteringDataList[[groupName]] <- clusteringDataNorm
          
        }, error = function(e) {
          err_msg <- paste("Group", groupName, "-", e$message); if (grepl("matrix is not positive definite", e$message, ignore.case = TRUE)) { err_msg <- paste(err_msg, "(Pairwise deletion issue?)") }
          estimation_errors[[groupName]] <- err_msg
          networkGraphsList[[groupName]] <- NULL; centralityDataList[[groupName]] <- NULL; clusteringDataList[[groupName]] <- NULL
        })
      }
      
      if (length(estimation_errors) > 0) { jmvcore::reject(paste("Errors occurred during estimation for some groups:\n", paste(estimation_errors, collapse="\n")), code="group_estimation_error") }
      successful_groups <- names(networkGraphsList); if (length(successful_groups) == 0) { jmvcore::reject("Network estimation failed for all groups.", code="all_groups_failed"); return() }
      
      summaryTable <- self$results$summary; try(summaryTable$setState(NULL), silent=TRUE); try(summaryTable$deleteNote("na_handling"), silent=TRUE); try(summaryTable$deleteNote("na_removed"), silent=TRUE)
      if (!is.null(na_note)) { summaryTable$setNote("na_handling", na_note) }
      for (groupName in successful_groups) { networkGraph <- networkGraphsList[[groupName]]; tryCatch({ nNodes <- ncol(networkGraph); adjMatrix <- networkGraph; adjMatrix[is.na(adjMatrix)] <- 0; nonZeroEdges <- sum(abs(adjMatrix[upper.tri(adjMatrix)]) > 1e-10); possibleEdges <- nNodes * (nNodes - 1) / 2; sparsity <- if(possibleEdges > 0) 1 - (nonZeroEdges / possibleEdges) else 1; summaryRow <- list(nodes = nNodes, edges = paste(nonZeroEdges, "/", possibleEdges), sparsity = sparsity); if (!is.null(groupVarName)) { summaryRow <- c(list(group = groupName), summaryRow) }; summaryTable$addRow(rowKey = groupName, values = summaryRow) }, error = function(e) { jmvcore::reject(paste("Failed to populate summary row for group", groupName, ":", e$message), code="summary_group_error") }) }
      
      weightsTable <- self$results$weights; if (isTRUE(self$options$tableWeights) && is.null(groupVarName)) { try(weightsTable$setVisible(TRUE), silent=TRUE); tryCatch({ networkGraph <- networkGraphsList[[1]]; nodeNames <- rownames(networkGraph); current_table_cols <- private$.getColumnNames(weightsTable); cols_to_remove <- setdiff(current_table_cols, c("node", nodeNames)); for(col_name in cols_to_remove) { if (col_name %in% private$.getColumnNames(weightsTable)) try(weightsTable$deleteColumn(name = col_name), silent=TRUE) }; if (!"node" %in% private$.getColumnNames(weightsTable)) { try(weightsTable$addColumn(name = "node", title = "", type = 'text'), silent = TRUE) }; for (nodeName in nodeNames) { if (!nodeName %in% private$.getColumnNames(weightsTable)) { try(weightsTable$addColumn(name = nodeName, title = nodeName, type = 'number'), silent=TRUE) } }; try(weightsTable$setState(NULL), silent = TRUE); if (length(nodeNames) > 0) { adjMatrix <- networkGraph; for (row_node in nodeNames) { row_values <- list(node = row_node); for (col_node in nodeNames) { row_values[[col_node]] <- adjMatrix[row_node, col_node] }; try(weightsTable$addRow(rowKey = row_node, values = row_values), silent = TRUE) } } }, error = function(e) { jmvcore::reject(paste("Failed to populate weights table:", e$message), code="weights_table_error"); try(weightsTable$setState(NULL), silent=TRUE); try(weightsTable$setVisible(FALSE), silent=TRUE) }) } else { try(weightsTable$setState(NULL), silent=TRUE); try(weightsTable$setVisible(FALSE), silent=TRUE) }
      
      centralityTable <- self$results$centrality; try(centralityTable$setState(NULL), silent=TRUE); try(centralityTable$deleteNote("norm_z"), silent=TRUE); allCentralityDataList <- list(); data_found_for_cent_table = FALSE
      if (isTRUE(self$options$tableCentrality) || isTRUE(self$options$plotCentrality)) { for (groupName in successful_groups) { centralityData <- centralityDataList[[groupName]]; if (!is.null(centralityData)) { data_found_for_cent_table = TRUE; if (isTRUE(self$options$tableCentrality) && nrow(centralityData) > 0) { for (i in 1:nrow(centralityData)) { row_key <- paste(groupName, i, sep="_"); row_values <- list(variable = centralityData$variable[i], strength = centralityData$strength[i], closeness = centralityData$closeness[i], betweenness = centralityData$betweenness[i], expectedInfluence = centralityData$expectedInfluence[i]); if (!is.null(groupVarName)) { row_values <- c(list(group = groupName), row_values) }; try(centralityTable$addRow(rowKey = row_key, values = row_values), silent = TRUE) } }; if (!is.null(groupVarName)) centralityData$group <- groupName; allCentralityDataList[[groupName]] <- centralityData } }; if (data_found_for_cent_table) try(centralityTable$setNote("norm_z", "Centrality values are z-score normalized."), silent=TRUE) } else { try(centralityTable$setState(NULL), silent=TRUE) }
      
      clusteringTable <- self$results$clustering; try(clusteringTable$setState(NULL), silent=TRUE); try(clusteringTable$deleteNote("clust_norm_z"), silent=TRUE); allClusteringDataList <- list(); data_found_for_clust_table = FALSE
      if (isTRUE(self$options$tableClustering) || isTRUE(self$options$plotClustering)) { current_table_cols <- private$.getColumnNames(clusteringTable); required_cols <- c("group", "node", "Barrat", "Onnela", "WS"); cols_to_remove <- setdiff(current_table_cols, required_cols); for(col_name in cols_to_remove) { if (col_name %in% private$.getColumnNames(clusteringTable)) try(clusteringTable$deleteColumn(name = col_name), silent=TRUE) }; current_table_cols_after_remove <- private$.getColumnNames(clusteringTable); cols_to_add <- setdiff(required_cols, current_table_cols_after_remove); if ("group" %in% cols_to_add) { try(clusteringTable$addColumn(name = "group", title = "Group", type = 'text', visible="(groupVar)"), silent = TRUE) }; if ("node" %in% cols_to_add) { try(clusteringTable$addColumn(name = "node", title = "Variable", type = 'text'), silent = TRUE) }; for(col_name in setdiff(cols_to_add, c("node", "group"))) { try(clusteringTable$addColumn(name = col_name, title = col_name, type = 'number'), silent = TRUE) }; for (groupName in successful_groups) { clusteringData <- clusteringDataList[[groupName]]; if (!is.null(clusteringData)) { data_found_for_clust_table = TRUE; if (isTRUE(self$options$tableClustering) && nrow(clusteringData) > 0) { for (i in 1:nrow(clusteringData)) { row_node_name <- clusteringData$node[i]; row_key <- paste(groupName, row_node_name, sep="_"); row_values <- list(node = clusteringData$node[i], Barrat = clusteringData$Barrat[i], Onnela = clusteringData$Onnela[i], WS = clusteringData$WS[i]); if (!is.null(groupVarName)) { row_values <- c(list(group = groupName), row_values) }; try(clusteringTable$addRow(rowKey = row_key, values = row_values), silent = TRUE) } }; if (!is.null(groupVarName)) clusteringData$group <- groupName; allClusteringDataList[[groupName]] <- clusteringData } }; if (data_found_for_clust_table) try(clusteringTable$setNote("clust_norm_z", "Clustering values are z-score normalized."), silent=TRUE) } else { try(clusteringTable$setState(NULL), silent=TRUE) }
      
      if (isTRUE(self$options$plotNetwork) && length(networkGraphsList) > 0) { commonLayout <- NULL; tryCatch({ graphs_for_layout <- networkGraphsList[successful_groups]; if (length(graphs_for_layout) > 0) {
        commonLayout <- qgraph::averageLayout(graphs_for_layout, layout = self$options$layoutAlgorithm); if (!is.null(commonLayout)) rownames(commonLayout) <- nodeLabels } }, error = function(e) { jmvcore::reject(paste("Layout calculation failed:", e$message), code="layout_error"); tryCatch({
          commonLayout <- qgraph::averageLayout(networkGraphsList[[1]], layout = "circle"); if (!is.null(commonLayout)) rownames(commonLayout) <- nodeLabels }, error=function(e2){}) }); plotState <- list(graphs = networkGraphsList, layout = commonLayout, labels = if(isTRUE(self$options$showLabels)) nodeLabels else FALSE, groupNames = successful_groups); self$results$networkPlot$setState(plotState) }
      
      if (isTRUE(self$options$plotCentrality) && length(allCentralityDataList) > 0) {
        combinedCentralityData <- do.call(rbind, c(allCentralityDataList, make.row.names = FALSE))
        if (!is.null(combinedCentralityData) && !"group" %in% names(combinedCentralityData)) {
          combinedCentralityData$group <- "Overall"
        }
        self$results$centralityPlot$setState(combinedCentralityData)
      }
      if (isTRUE(self$options$plotClustering) && length(allClusteringDataList) > 0) {
        combinedClusteringData <- do.call(rbind, c(allClusteringDataList, make.row.names = FALSE))
        if (!is.null(combinedClusteringData) && !"group" %in% names(combinedClusteringData)) {
          combinedClusteringData$group <- "Overall"
        }
        self$results$clusteringPlot$setState(combinedClusteringData)
      }
      
    },
    
    .plotNetwork = function(image, ggtheme, theme, ...) {
      plotState <- image$state; if (is.null(plotState) || is.null(plotState$graphs) || length(plotState$graphs) == 0 || is.null(plotState$layout)) return(FALSE)
      if (!requireNamespace("qgraph", quietly = TRUE)) { plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, "qgraph package missing."); return(FALSE) }
      graphsToPlot <- plotState$graphs; layout <- plotState$layout; labels <- plotState$labels; groupNames <- plotState$groupNames; numPlots <- length(graphsToPlot)
      old_par <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old_par))
      if (numPlots > 1) { graphics::par(mfrow = c(1, numPlots)) }
      graphics::par(bg = "transparent")
      plotOk <- TRUE
      for (i in 1:numPlots) { groupName <- groupNames[i]; currentGraph <- graphsToPlot[[groupName]]; if (is.null(currentGraph)) next; qArgs <- list(input = currentGraph, layout = layout, labels = labels, theme = "colorblind", DoNotPlot = FALSE, title = groupName); tryCatch({
        qgraph::qgraph(qArgs$input, layout=qArgs$layout, labels=qArgs$labels, theme=qArgs$theme, title=qArgs$title) }, error = function(e) { jmvcore::reject(paste("Network plotting failed for group", groupName, ":", e$message), code = "network_plot_error"); plot(1, type="n", axes=FALSE, xlab="", ylab=""); text(1, 1, paste("Plot Error (", groupName, "):\n", e$message), col="red"); plotOk <<- FALSE }) }
      return(plotOk)
    },
    
    .plotCentrality = function(image, ggtheme, theme, ...) {
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
    
    .plotClustering = function(image, ggtheme, theme, ...) {
      plotData <- image$state; if (is.null(plotData) || nrow(plotData) == 0) return(FALSE)
      if (!requireNamespace("ggplot2", quietly = TRUE)) { stop("ggplot2 package required.") }
      if (!requireNamespace("gtools", quietly = TRUE)) { stop("gtools package required.") }
      if (!requireNamespace("reshape2", quietly = TRUE)) { stop("reshape2 package required.") }
      
      id_vars <- intersect(c("node", "group"), names(plotData))
      plotDataLong <- tryCatch({ reshape2::melt(plotData, id.vars = id_vars, variable.name = "measure", value.name = "value") }, error = function(e) { NULL })
      if (is.null(plotDataLong)) return(FALSE)
      plotDataLong <- plotDataLong[!is.na(plotDataLong$value), ]
      if (nrow(plotDataLong) == 0) return(FALSE)
      
      measure_order <- c("Barrat", "Onnela", "WS")
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
    }
    
  )
)