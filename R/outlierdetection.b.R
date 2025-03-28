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
            data_var <- data_group[[var]]
            data_var <- as.numeric(data_var[!is.na(data_var)])
            
            if(length(data_var) == 0) next
            
            original_indices <- which(!is.na(data_group[[var]]))
            
            iqr_value <- IQR(data_var, na.rm = TRUE)
            q1 <- quantile(data_var, 0.25, na.rm = TRUE)
            q3 <- quantile(data_var, 0.75, na.rm = TRUE)
            
            row_values <- list()
            outlier_level <- rep("Non-Outlier", length(data_var))
            row_indices <- rep(NA, length(data_var))
            
            if (self$options$iqr15) {
              lower_1.5 <- q1 - (1.5 * iqr_value)
              upper_1.5 <- q3 + (1.5 * iqr_value)
              outliers_1.5 <- data_var < lower_1.5 | data_var > upper_1.5
              
              row_values$lowerFence_1.5 <- lower_1.5
              row_values$upperFence_1.5 <- upper_1.5
              row_values$numOutliers_1.5 <- sum(outliers_1.5)
              
              outlier_level[outliers_1.5] <- "1.5"
              row_indices[outliers_1.5] <- original_indices[outliers_1.5]
            }
            
            if (self$options$iqr22) {
              lower_2.2 <- q1 - (2.2 * iqr_value)
              upper_2.2 <- q3 + (2.2 * iqr_value)
              outliers_2.2 <- data_var < lower_2.2 | data_var > upper_2.2
              
              row_values$lowerFence_2.2 <- lower_2.2
              row_values$upperFence_2.2 <- upper_2.2
              row_values$numOutliers_2.2 <- sum(outliers_2.2)
              
              outlier_level[outliers_2.2] <- "2.2"
              row_indices[outliers_2.2] <- original_indices[outliers_2.2]
            }
            
            if (self$options$iqr30) {
              lower_3.0 <- q1 - (3.0 * iqr_value)
              upper_3.0 <- q3 + (3.0 * iqr_value)
              outliers_3.0 <- data_var < lower_3.0 | data_var > upper_3.0
              
              row_values$lowerFence_3.0 <- lower_3.0
              row_values$upperFence_3.0 <- upper_3.0
              row_values$numOutliers_3.0 <- sum(outliers_3.0)
              
              outlier_level[outliers_3.0] <- "3.0"
              row_indices[outliers_3.0] <- original_indices[outliers_3.0]
            }
            
            row_values$variable <- var
            row_values$group_level <- group_name
            table$addRow(rowKey=row_counter, values=row_values)
            row_counter <- row_counter + 1
            
            
            if (self$options$dispBox) {
              plot_data <- data.frame(
                variable = var,
                value = data_var,
                outlierLevel = factor(outlier_level, levels = c("Non-Outlier", "1.5", "2.2", "3.0")),
                rowIndex = row_indices,
                x = if (has_group) as.numeric(as.factor(data_group[[group_var]])) else 0,
                group = if (has_group) as.character(data_group[[group_var]]) else "All Data"
              )
              all_plot_data[[paste(var, group_name, sep = "_")]] <- plot_data
            }
          }
          
        }, error = function(e) {
          print(paste("Erro ao processar variável", var, ":", e$message))
          print(e)
        })
      }
      
      if (self$options$dispBox && length(all_plot_data) > 0) {
        image <- self$results$plot
        image$setState(all_plot_data)
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
        
        has_group <- "group" %in% names(all_data_for_var)
        
        active_levels <- c("Non-Outlier")
        if (self$options$iqr15) active_levels <- c(active_levels, "1.5")
        if (self$options$iqr22) active_levels <- c(active_levels, "2.2")
        if (self$options$iqr30) active_levels <- c(active_levels, "3.0")
        
        active_colors <- colors[active_levels]
        active_shapes <- shapes[active_levels]
        
        outliers_data <- subset(all_data_for_var, outlierLevel != "Non-Outlier")
        
        if (has_group) {
          p <- ggplot(all_data_for_var, aes(x = group, y = value)) +
            geom_boxplot(aes(group = group), outlier.shape = NA) +
            geom_point(data = outliers_data,
                       aes(x = group, y = value, color = outlierLevel, shape = outlierLevel),
                       size = 3,
                       position = position_dodge(width = 0.2)) +
            geom_text(
              data = outliers_data,
              aes(x = group, y = value, label = paste("", rowIndex), color = outlierLevel),
              vjust = 0.4,
              hjust = -0.1,
              size = 4,
              show.legend = FALSE,
              position = position_dodge(width = 0.2)
            ) +
            scale_color_manual(values = active_colors, drop = FALSE) +
            scale_shape_manual(values = active_shapes, drop = FALSE) +
            labs(
              title = paste("Boxplot for", var),
              x = self$options$group,
              y = "Value",
              color = "Outlier Level",
              shape = "Outlier Level"
            ) +
            theme_classic() +
            theme(
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.x = element_text(size=12),
              axis.text.x = element_text(size=10),
              axis.title.y = element_text(size = 12),
              axis.text.y = element_text(size = 10),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom"
            )
          
        } else {
          p <- ggplot(all_data_for_var, aes(x = factor(1), y = value)) +
            geom_boxplot(outlier.shape = NA) +
            geom_point(data = outliers_data,
                       aes(x = x, color = outlierLevel, shape = outlierLevel),
                       size = 3,
                       position = position_dodge(width = 0.2)) +
            geom_text(
              data = outliers_data,
              aes(x = x, y = value, label = paste("", rowIndex), color = outlierLevel),
              vjust = 0.4,
              hjust = -0.1,
              size = 4,
              show.legend = FALSE,
              position = position_dodge(width = 0.2)
            ) +
            scale_color_manual(values = active_colors, drop = FALSE) +
            scale_shape_manual(values = active_shapes, drop = FALSE) +
            labs(
              title = paste("Boxplot for", var),
              x = "",
              y = "Value",
              color = "Outlier Level",
              shape = "Outlier Level"
            ) +
            theme_classic() +
            theme(
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_text(size = 12),
              axis.text.y = element_text(size = 10),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom"
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