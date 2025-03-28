ModifiedZClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "ModifiedZClass",
  inherit = ModifiedZBase,
  private = list(
    .run = function() {
      
      if (is.null(self$options$vars) || length(self$options$vars) == 0) {
        self$results$text$setContent("No variables selected.")
        return()
      }
      
      data <- self$data
      vars <- self$options$vars
      groups <- self$options$groups
      threshold <- self$options$threshold
      
      results <- data.frame()  
      
      if (!is.null(groups) && length(groups) > 0) {
        grouping_var <- groups[[1]]  
        if (!grouping_var %in% colnames(data)) {
          self$results$text$setContent("Grouping variable not found in the dataset.")
          return()
        }
        data$group <- as.factor(data[[grouping_var]])
      } else {
        data$group <- factor("All Data")  
      }
      
      for (var in vars) {
        if (!is.numeric(data[[var]]))  
          next
        
        grouped_data <- split(data, data$group)  
        
        for (group_name in names(grouped_data)) {
          group_data <- grouped_data[[group_name]]
          x <- na.omit(group_data[[var]])  
          
          if (length(x) == 0)  
            next
          
          med <- median(x)  
          mad_val <- median(abs(x - med))  
          
          if (mad_val == 0)  
            next
          
          modified_z <- 0.6745 * (x - med) / mad_val
          is_outlier <- abs(modified_z) > threshold  
          
          outlier_rows <- which(is_outlier)
          if (length(outlier_rows) > 0) {
            variable_results <- data.frame(
              variable = rep(var, length(outlier_rows)),
              group = rep(group_name, length(outlier_rows)),
              row_number = as.numeric(rownames(group_data)[outlier_rows]),
              modified_z_score = modified_z[outlier_rows],
              is_outlier = rep("Yes", length(outlier_rows))
            )
            results <- rbind(results, variable_results)
          }
        }
      }
      
      table <- self$results$outlierTable
      for (i in seq_len(nrow(results))) {
        table$addRow(rowKey = i, values = list(
          variable = results$variable[i],
          group = results$group[i],
          row_number = results$row_number[i],
          modified_z_score = results$modified_z_score[i],
          is_outlier = results$is_outlier[i]
        ))
      }
      
      if (nrow(results) == 0) {
        self$results$text$setContent("No outliers detected.")
      } else {
        self$results$text$setContent("Modified Z-Score Outlier Detection completed.")
      }
    }
  )
)