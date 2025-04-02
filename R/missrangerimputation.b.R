missRangerImputationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "missRangerImputationClass",
  inherit = missRangerImputationBase,
  private = list(
    .run = function() {
      
      data <- self$data
      vars_to_impute <- self$options$vars
      maxiter <- self$options$maxiter
      num_trees <- self$options$num_trees
      pmm_k <- self$options$pmm_k
      seed <- self$options$seed
      show_pattern_plot <- self$options$show_pattern_plot
      show_correlation_plot <- self$options$show_correlation_plot
      
      if (is.null(data)) {
        return()
      }
      
      if (is.null(vars_to_impute) || length(vars_to_impute) == 0) {
        return()
      }
      
      vars_to_impute <- vars_to_impute[!sapply(data[, vars_to_impute, drop = FALSE], inherits, "jmvcore.IdVariable")]
      if(length(vars_to_impute) == 0){
        return()
      }
      
      data_to_impute <- data[, vars_to_impute, drop = FALSE]
      
      if (seed != "" && !is.na(as.numeric(seed))) {
        set.seed(as.numeric(seed))
      }
      
      imputed_data <- tryCatch({
        imputed_data <- missRanger::missRanger(
          data_to_impute,
          maxiter = maxiter,
          num.trees = num_trees,
          pmm.k = pmm_k,
          verbose = 0
        )
        imputed_data
      }, error = function(e) {
        msg <- paste("Error during imputation:", e$message)
        self$results$text$setContent(msg)
        self$results$text$setVisible(TRUE)
        return(NULL)
      })
      
      if (is.null(imputed_data)) {
        return()
      }
      
      for (var_name in names(imputed_data)) {
        if (is.numeric(imputed_data[[var_name]])) {
          imputed_data[[var_name]] <- round(imputed_data[[var_name]], digits = 3)
        }
      }
      
      table <- self$results$imputedData
      row_num <- 1
      
      for (var_name in vars_to_impute) {
        original_values <- data[[var_name]]
        imputed_values <- imputed_data[[var_name]]
        na_indices <- which(is.na(original_values))
        
        for (i in na_indices) {
          table$addRow(rowKey = row_num, values = list(
            variable = var_name,
            original = if (is.na(original_values[i])) "NA" else as.character(original_values[i]),
            imputed = as.character(imputed_values[i]),
            rowNumber = i
          ))
          row_num <- row_num + 1
        }
      }
      
      self$results$text$setContent("Imputation completed. Imputed values are shown in the table.")
      self$results$text$setVisible(TRUE)
      
      
      if (show_pattern_plot) {
        self$results$patternPlot$setState(data)
      }
      
      if (show_correlation_plot) {
        missing_indicators <- as.data.frame(lapply(data, function(x) as.integer(is.na(x))))
        cor_matrix <- stats::cor(missing_indicators, use = "pairwise.complete.obs")
        self$results$correlationPlot$setState(cor_matrix)
        
      }
    },
    
    .combinationsPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      plotData <- image$state
      
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par))
      graphics::par(bg = "transparent")
      
      VIM::aggr(plotData,
                numbers = TRUE,
                sortVars = TRUE,
                prop = c(TRUE, TRUE),
                gap = 3,
                col = c("skyblue", "red"),
                cex.axis = 0.8,
                ylab = c("Proportion of Missings", "Proportion of Combinations")
      )
      
      TRUE
    },
    
    .correlationPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      plotData <- image$state
      p <- GGally::ggcorr(data = NULL, cor_matrix = plotData,
                          label = TRUE, label_round = 2,
                          hjust = 1, layout.exp = 5) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
                       axis.text.y = ggplot2::element_text(size = 12),
                       panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))
      
      print(p)
      TRUE
    }
  )
)