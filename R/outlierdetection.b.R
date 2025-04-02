outlierDetectionClass <- R6::R6Class(
  "outlierDetectionClass",
  inherit = outlierDetectionBase,
  private = list(
    .run = function() {
      if (is.null(self$options$vars))
        return()
      
      table <- self$results$outlierTable
      all_plot_data <- list()
      has_group <- !is.null(self$options$group)
      group_var <- self$options$group
      
      row_counter <- 1
      
      for (var in self$options$vars) {
        if (!var %in% names(self$data))
          next
        
        tryCatch({
          if (has_group) {
            if (!group_var %in% names(self$data)) {
              stop(paste("Grouping variable", group_var, "not found in data."))
            }
            data_grouped <- split(self$data, as.factor(self$data[[group_var]]))
          } else {
            data_grouped <- list("All Data" = self$data)
          }
          
          for (group_name in names(data_grouped)) {
            data_group <- data_grouped[[group_name]]
            
            data_var_full <- data_group[[var]]
            
            non_na_indices_in_group <- which(!is.na(data_var_full))
            
            data_var <- as.numeric(data_var_full[non_na_indices_in_group])
            
            if(length(data_var) == 0) next
            
            original_row_numbers_group <- as.numeric(rownames(data_group))
            if(anyNA(original_row_numbers_group)) {
              warning(paste("Could not reliably get original row numbers for group", group_name, "variable", var, "- using relative indices instead."))
              original_row_numbers_group <- 1:nrow(data_group)
            }
            
            iqr_value <- stats::IQR(data_var, na.rm = TRUE)
            q1 <- stats::quantile(data_var, 0.25, na.rm = TRUE)
            q3 <- stats::quantile(data_var, 0.75, na.rm = TRUE)
            
            outlierLevel_full <- rep("Non-Outlier", nrow(data_group))
            rowIndex_full <- rep(NA_integer_, nrow(data_group))
            
            row_values_for_table <- list()
            
            if (self$options$iqr15) {
              lower_1.5 <- q1 - (1.5 * iqr_value)
              upper_1.5 <- q3 + (1.5 * iqr_value)
              outliers_1.5_indices_in_datavar <- which(data_var < lower_1.5 | data_var > upper_1.5)
              outliers_1.5_indices_in_group <- non_na_indices_in_group[outliers_1.5_indices_in_datavar]
              
              outlierLevel_full[outliers_1.5_indices_in_group] <- "1.5"
              rowIndex_full[outliers_1.5_indices_in_group] <- original_row_numbers_group[outliers_1.5_indices_in_group]
              
              row_values_for_table$lowerFence_1.5 <- lower_1.5
              row_values_for_table$upperFence_1.5 <- upper_1.5
              row_values_for_table$numOutliers_1.5 <- length(outliers_1.5_indices_in_datavar)
            }
            
            if (self$options$iqr22) {
              lower_2.2 <- q1 - (2.2 * iqr_value)
              upper_2.2 <- q3 + (2.2 * iqr_value)
              outliers_2.2_indices_in_datavar <- which(data_var < lower_2.2 | data_var > upper_2.2)
              outliers_2.2_indices_in_group <- non_na_indices_in_group[outliers_2.2_indices_in_datavar]
              
              outlierLevel_full[outliers_2.2_indices_in_group] <- "2.2"
              rowIndex_full[outliers_2.2_indices_in_group] <- original_row_numbers_group[outliers_2.2_indices_in_group]
              
              row_values_for_table$lowerFence_2.2 <- lower_2.2
              row_values_for_table$upperFence_2.2 <- upper_2.2
              row_values_for_table$numOutliers_2.2 <- length(outliers_2.2_indices_in_datavar)
            }
            
            if (self$options$iqr30) {
              lower_3.0 <- q1 - (3.0 * iqr_value)
              upper_3.0 <- q3 + (3.0 * iqr_value)
              outliers_3.0_indices_in_datavar <- which(data_var < lower_3.0 | data_var > upper_3.0)
              outliers_3.0_indices_in_group <- non_na_indices_in_group[outliers_3.0_indices_in_datavar]
              
              outlierLevel_full[outliers_3.0_indices_in_group] <- "3.0"
              rowIndex_full[outliers_3.0_indices_in_group] <- original_row_numbers_group[outliers_3.0_indices_in_group]
              
              row_values_for_table$lowerFence_3.0 <- lower_3.0
              row_values_for_table$upperFence_3.0 <- upper_3.0
              row_values_for_table$numOutliers_3.0 <- length(outliers_3.0_indices_in_datavar)
            }
            
            row_values_for_table$variable <- var
            row_values_for_table$group_level <- group_name
            table$addRow(rowKey=row_counter, values=row_values_for_table)
            row_counter <- row_counter + 1
            
            if (self$options$dispBox) {
              plot_data <- data.frame(
                variable = var,
                value = data_var_full,
                outlierLevel = factor(outlierLevel_full, levels = c("Non-Outlier", "1.5", "2.2", "3.0")),
                rowIndex = rowIndex_full,
                x = if (has_group) as.numeric(as.factor(data_group[[group_var]])) else 0,
                group = if (has_group) as.character(data_group[[group_var]]) else "All Data"
              )
              all_plot_data[[paste(var, group_name, sep = "_")]] <- plot_data
            }
          }
          
        }, error = function(e) {
          jmvcore::reject(paste("Error processing variable", var, ":", e$message), code="error_processing_variable")
        })
      }
      
      if (self$options$dispBox && length(all_plot_data) > 0) {
        image <- self$results$plot
        image$setState(all_plot_data)
      } else if (self$options$dispBox) {
        image <- self$results$plot
        image$setState(NULL)
      }
    },
    
    .plot = function(image, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      plot_list <- list()
      
      shapes <- c("Non-Outlier" = 16,
                  "1.5" = 16,
                  "2.2" = 15,
                  "3.0" = 17)
      
      colors <- c("Non-Outlier" = "black",
                  "1.5" = "black",
                  "2.2" = "black",
                  "3.0" = "black")
      
      for (var in self$options$vars) {
        all_data_for_var <- NULL
        
        for (var_group_name in names(image$state)) {
          plotData <- image$state[[var_group_name]]
          if (plotData$variable[1] == var) {
            all_data_for_var <- rbind(all_data_for_var, plotData)
          }
        }
        
        if (is.null(all_data_for_var)) next
        
        has_group <- "group" %in% names(all_data_for_var) && length(unique(all_data_for_var$group)) > 1
        
        active_levels <- c("Non-Outlier")
        if (self$options$iqr15) active_levels <- c(active_levels, "1.5")
        if (self$options$iqr22) active_levels <- c(active_levels, "2.2")
        if (self$options$iqr30) active_levels <- c(active_levels, "3.0")
        
        active_colors <- colors[active_levels]
        active_shapes <- shapes[active_levels]
        
        if (has_group) {
          outliers_data <- subset(all_data_for_var, outlierLevel != "Non-Outlier")
          
          p <- ggplot2::ggplot(all_data_for_var, ggplot2::aes(x = group, y = value)) +
            ggplot2::geom_boxplot(ggplot2::aes(group = group), outlier.shape = NA) +
            ggplot2::geom_point(data = outliers_data,
                                ggplot2::aes(x = group, y = value, color = outlierLevel, shape = outlierLevel),
                                size = 3,
                                position = ggplot2::position_dodge(width = 0.2)) +
            ggplot2::geom_text(
              data = outliers_data,
              ggplot2::aes(x = group, y = value, label = paste("", rowIndex), color = outlierLevel),
              vjust = 0.4,
              hjust = -0.1,
              size = 4,
              show.legend = FALSE,
              position = ggplot2::position_dodge(width = 0.2)
            ) +
            ggplot2::scale_color_manual(values = active_colors, drop = FALSE) +
            ggplot2::scale_shape_manual(values = active_shapes, drop = FALSE) +
            ggplot2::labs(
              title = paste("Boxplot for", var),
              x = self$options$group,
              y = "Value",
              color = "Outlier Level",
              shape = "Outlier Level"
            ) +
            ggplot2::theme_classic() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.x = ggplot2::element_text(size=12),
              axis.text.x = ggplot2::element_text(size=10),
              axis.title.y = ggplot2::element_text(size = 12),
              axis.text.y = ggplot2::element_text(size = 10),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              legend.position = "bottom",
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              legend.background = ggplot2::element_rect(fill = "transparent", color = NA)
            )
          
        } else {
          all_data_for_var$x_axis_label <- "Data"
          outliers_data <- subset(all_data_for_var, outlierLevel != "Non-Outlier")
          if (nrow(outliers_data) > 0) {
            outliers_data$x_axis_label <- "Data"
          }
          
          p <- ggplot2::ggplot(all_data_for_var, ggplot2::aes(x = x_axis_label, y = value)) +
            ggplot2::geom_boxplot(outlier.shape = NA) +
            ggplot2::geom_point(data = outliers_data,
                                ggplot2::aes(x = x_axis_label, color = outlierLevel, shape = outlierLevel),
                                size = 3) +
            ggplot2::geom_text(
              data = outliers_data,
              ggplot2::aes(x = x_axis_label, y = value, label = paste("", rowIndex), color = outlierLevel),
              vjust = 0.4,
              hjust = -0.1,
              size = 4,
              show.legend = FALSE
            ) +
            ggplot2::scale_color_manual(values = active_colors, drop = FALSE) +
            ggplot2::scale_shape_manual(values = active_shapes, drop = FALSE) +
            ggplot2::labs(
              title = paste("Boxplot for", var),
              x = "",
              y = "Value",
              color = "Outlier Level",
              shape = "Outlier Level"
            ) +
            ggplot2::theme_classic() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.x = ggplot2::element_blank(),
              axis.text.x = ggplot2::element_blank(),
              axis.ticks.x = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_text(size = 12),
              axis.text.y = ggplot2::element_text(size = 10),
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              legend.position = "bottom",
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              legend.background = ggplot2::element_rect(fill = "transparent", color = NA)
            )
        }
        
        plot_list[[var]] <- p
      }
      
      if (length(plot_list) > 1) {
        n_plots <- length(plot_list)
        n_cols <- min(3, n_plots)
        n_rows <- ceiling(n_plots / n_cols)
        
        final_plot <- gridExtra::grid.arrange(
          grobs = plot_list,
          ncol = n_cols,
          nrow = n_rows
        )
        print(final_plot)
        
      } else if (length(plot_list) == 1) {
        print(plot_list[[1]])
      }
      
      TRUE
    }
  )
)