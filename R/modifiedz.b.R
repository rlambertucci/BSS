ModifiedZClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "ModifiedZClass",
  inherit = ModifiedZBase,
  private = list(
    
    
    .init = function() {
      info_html <- paste0(
        '<div>', 
        '<ul>',
        '<li><b>Threshold = 3.0</b> → Used for more conservative contexts (e.g., psychology, social sciences). <i>Leys, C., Klein, O., Dominicy, Y., & Ley, C. (2018).</i></li>',
        '<li><b>Threshold = 3.5</b> → The most widely recommended standard (robust statistics). <i>Iglewicz, B., & Hoaglin, D. C. (1993).</i></li>',
        '<li><b>Threshold = 4.0</b> → Suitable for large samples, preventing excessive removal of naturally extreme values. <i>Iglewicz, B., & Hoaglin, D. C. (1993).</i></li>',
        '<li><b>Threshold = 2.5 to 4.5</b> → In machine learning or exploratory analysis, the threshold can be adjusted as needed. <i>Hodge, V. J., & Austin, J. (2004).</i></li>',
        '</ul>',
        '</div>'
      )
      self$results$thresholdInfo$setContent(info_html)
    },  
    
    
    .run = function() {
      
      if (is.null(self$options$vars) || length(self$options$vars) == 0) {
        self$results$text$setContent("No variables selected.")
        return()
      }
      
      data <- self$data
      vars <- self$options$vars
      groups <- self$options$groups
      threshold <- self$options$threshold
      
      results <- data.frame(
        variable = character(),
        group = character(),
        row_number = integer(),
        modified_z_score = numeric(),
        stringsAsFactors = FALSE
      )
      
      grouping_variable_selected <- !is.null(groups) && length(groups) > 0
      grouping_var_name <- NULL
      
      if (grouping_variable_selected) {
        grouping_var_name <- groups[[1]]
        if (!grouping_var_name %in% colnames(data)) {
          self$results$text$setContent(paste("Grouping variable '", grouping_var_name, "' not found in the dataset."))
          return()
        }
        data$.internal_group_col <- as.factor(data[[grouping_var_name]])
      } else {
        data$.internal_group_col <- factor(rep("Overall", nrow(data)))
      }
      
      for (var in vars) {
        if (!is.numeric(data[[var]])) {
          jmvcore::reject(paste("Variable '", var, "' is not numeric and will be skipped."), code = "var_not_numeric")
          next
        }
        
        grouped_data <- split(data, data$.internal_group_col, drop = FALSE)
        
        for (group_name in names(grouped_data)) {
          group_data <- grouped_data[[group_name]]
          x_named <- group_data[[var]]
          names(x_named) <- rownames(group_data)
          x <- na.omit(x_named)
          
          if (length(x) == 0) {
            next
          }
          
          med <- median(x)
          mad_val <- stats::mad(x, center = med, constant = 1)
          
          if (mad_val == 0) {
            next
          }
          modified_z <- (x - med) / (mad_val / 0.6745)
          
          is_outlier_logical <- abs(modified_z) > threshold
          outlier_indices_in_x <- which(is_outlier_logical)
          
          if (length(outlier_indices_in_x) > 0) {
            outlier_scores <- modified_z[outlier_indices_in_x]
            original_row_numbers <- as.numeric(names(x)[outlier_indices_in_x])
            
            variable_results <- data.frame(
              variable = rep(var, length(original_row_numbers)),
              group = rep(group_name, length(original_row_numbers)),
              row_number = original_row_numbers,
              modified_z_score = outlier_scores,
              stringsAsFactors = FALSE
            )
            results <- rbind(results, variable_results)
          }
        }
      }
      
      table <- self$results$outlierTable
      
      if (nrow(results) > 0) {
        results <- results[order(results$variable, results$group, results$row_number), ]
      }
      
      for (i in seq_len(nrow(results))) {
        row_data <- list(
          variable = results$variable[i],
          group = results$group[i],
          row_number = results$row_number[i],
          modified_z_score = results$modified_z_score[i]
        )
        table$addRow(rowKey = i, values = row_data)
      }
      
      if (nrow(results) == 0) {
        self$results$text$setContent("No outliers detected with the specified threshold.")
      } else {
        count_label <- ifelse(nrow(results) == 1, "outlier", "outliers")
        group_info <- if (grouping_variable_selected) paste("across groups based on", grouping_var_name) else "overall"
        self$results$text$setContent(paste("Outlier detection complete. Found", nrow(results), count_label, group_info, "."))
      }
      
    }
  )
)