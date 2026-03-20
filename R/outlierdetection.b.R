outlierDetectionClass <- R6::R6Class(
  "outlierDetectionClass",
  inherit = outlierDetectionBase,
  private = list(
    
    .init = function() {
      info_html <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.5;">', 
        '<ul>',
        '<li><b>1.5 IQR</b> &rarr; Standard criterion; ideal for general outlier detection and common boxplot visualization. <i>Tukey, J. W. (1977).</i></li>',
        '<li><b>2.2 IQR</b> &rarr; Balanced approach; recommended for a more conservative detection while preserving more data variance. <i>Hoaglin & Iglewicz (1987).</i></li>',
        '<li><b>3.0 IQR</b> &rarr; Extreme criterion; identifies only blatant anomalies, highly effective for removing extreme outliers. <i>Tukey, J. W. (1977).</i></li>',
        '</ul>',
        '<p style="font-style: italic; padding-top: 10px;">Note: Lower multipliers are more aggressive in identifying outliers, while higher multipliers are more tolerant.</p>',
        '</div>'
      )
      self$results$iqrInfo$setContent(info_html)
    },
    
    .run = function() {
      if (is.null(self$options$vars) || length(self$options$vars) == 0)
        return()
      
      table         <- self$results$outlierTable
      all_plot_data <- list()
      has_group     <- !is.null(self$options$group) && length(self$options$group) > 0
      group_vars    <- self$options$group
      
      mult_map <- c("iqr15" = 1.5, "iqr22" = 2.2, "iqr30" = 3.0)
      iqr_mult <- mult_map[[self$options$iqrMultiplier]]
      
      treatment_type <- self$options$treatment
      row_counter    <- 1
      
      # Acumula os vetores tratados de todas as variáveis
      all_treated_vectors <- list()
      
      for (var in self$options$vars) {
        if (!var %in% names(self$data)) next
        
        tryCatch({
          if (has_group) {
            # Cruza múltiplas variáveis de grupo com interaction()
            if (length(group_vars) == 1) {
              combined_group <- as.factor(self$data[[group_vars]])
            } else {
              group_cols     <- lapply(group_vars, function(g) self$data[[g]])
              combined_group <- interaction(group_cols, sep = "_", drop = TRUE)
            }
            data_grouped <- split(self$data, combined_group)
          } else {
            data_grouped <- list("All Data" = self$data)
          }
          
          # Inicializa o vetor tratado com o comprimento total do dataset
          full_treated_data <- rep(NA_real_, nrow(self$data))
          full_treated_data[seq_along(self$data[[var]])] <- self$data[[var]]
          
          for (group_name in names(data_grouped)) {
            data_group    <- data_grouped[[group_name]]
            data_var_full <- data_group[[var]]
            
            non_na_indices_in_group <- which(!is.na(data_var_full))
            data_var <- as.numeric(data_var_full[non_na_indices_in_group])
            
            if(length(data_var) == 0) next
            
            original_row_numbers_group <- as.numeric(rownames(data_group))
            
            iqr_value <- stats::IQR(data_var, na.rm = TRUE)
            q1 <- stats::quantile(data_var, 0.25, na.rm = TRUE)
            q3 <- stats::quantile(data_var, 0.75, na.rm = TRUE)
            
            lower_fence <- q1 - (iqr_mult * iqr_value)
            upper_fence <- q3 + (iqr_mult * iqr_value)
            
            outliers_indices_in_datavar <- which(data_var < lower_fence | data_var > upper_fence)
            outliers_indices_in_group   <- non_na_indices_in_group[outliers_indices_in_datavar]
            
            outlierLevel_full <- rep("Non-Outlier", nrow(data_group))
            outlierLevel_full[outliers_indices_in_group] <- as.character(iqr_mult)
            
            rowIndex_full <- rep(NA_integer_, nrow(data_group))
            rowIndex_full[outliers_indices_in_group] <- original_row_numbers_group[outliers_indices_in_group]
            
            treated_values <- data_var_full
            modified_n     <- length(non_na_indices_in_group)
            applied_treatment_text <- "None"
            
            # Aplicação dos Tratamentos
            if (treatment_type != "none") {
              if (treatment_type == "trim_asym") {
                treated_values[outliers_indices_in_group] <- NA
                modified_n <- modified_n - length(outliers_indices_in_group)
                applied_treatment_text <- "Asymmetric Trimming"
              } else if (treatment_type == "trim_sym") {
                trim_prop      <- self$options$trimPercent / 100
                lower_quantile <- stats::quantile(data_var, trim_prop, na.rm = TRUE)
                upper_quantile <- stats::quantile(data_var, 1 - trim_prop, na.rm = TRUE)
                trim_indices   <- which(!is.na(treated_values) & (treated_values < lower_quantile | treated_values > upper_quantile))
                treated_values[trim_indices] <- NA
                modified_n <- length(which(!is.na(treated_values)))
                applied_treatment_text <- paste0("Symmetric Trimming (", self$options$trimPercent, "%)")
              } else if (treatment_type == "win_valid") {
                valid_data <- data_var[data_var >= lower_fence & data_var <= upper_fence]
                min_valid  <- min(valid_data, na.rm = TRUE)
                max_valid  <- max(valid_data, na.rm = TRUE)
                low_outliers  <- which(!is.na(treated_values) & treated_values < lower_fence)
                high_outliers <- which(!is.na(treated_values) & treated_values > upper_fence)
                treated_values[low_outliers]  <- min_valid
                treated_values[high_outliers] <- max_valid
                applied_treatment_text <- "Winsorized (Valid values)"
              } else if (treatment_type == "win_fence") {
                low_outliers  <- which(!is.na(treated_values) & treated_values < lower_fence)
                high_outliers <- which(!is.na(treated_values) & treated_values > upper_fence)
                treated_values[low_outliers]  <- lower_fence
                treated_values[high_outliers] <- upper_fence
                applied_treatment_text <- "Winsorized (Fence limits)"
              }
            }
            
            # Insere os dados tratados deste grupo no vetor principal
            full_treated_data[original_row_numbers_group] <- treated_values
            
            row_values_for_table <- list(
              variable    = var,
              group_level = group_name,
              lowerFence  = lower_fence,
              upperFence  = upper_fence,
              numOutliers = length(outliers_indices_in_datavar)
            )
            
            if (treatment_type != "none") {
              row_values_for_table$treatmentApplied <- applied_treatment_text
              row_values_for_table$modifiedN        <- modified_n
            }
            
            table$addRow(rowKey = row_counter, values = row_values_for_table)
            row_counter <- row_counter + 1
            
            if (self$options$dispBox) {
              plot_data <- data.frame(
                variable     = var,
                value        = data_var_full,
                outlierLevel = factor(outlierLevel_full, levels = c("Non-Outlier", "1.5", "2.2", "3.0")),
                rowIndex     = rowIndex_full,
                group        = if (has_group) rep(group_name, nrow(data_group)) else rep("All Data", nrow(data_group)),
                stringsAsFactors = FALSE
              )
              all_plot_data[[paste(var, group_name, sep = "_")]] <- plot_data
            }
          } # Fim do loop de agrupamento
          
          # Acumula o vetor tratado para envio posterior
          if (treatment_type != "none") {
            all_treated_vectors[[var]] <- full_treated_data
          }
          
        }, error = function(e) {
          jmvcore::reject(paste("Error processing variable", var, ":", e$message), code = "error_processing_variable")
        })
      } # Fim do loop de variáveis
      
      # --- BLOCO DE OUTPUT PARA A PLANILHA ---
      if (treatment_type != "none" &&
          length(all_treated_vectors) > 0 &&
          self$results$treatedData$isNotFilled()) {
        
        output_keys          <- character()
        output_titles        <- character()
        output_measure_types <- character()
        
        for (var in names(all_treated_vectors)) {
          output_keys          <- c(output_keys,          var)
          output_titles        <- c(output_titles,        paste0("treated_", var))
          output_measure_types <- c(output_measure_types, "continuous")
        }
        
        self$results$treatedData$set(
          keys         = output_keys,
          titles       = output_titles,
          descriptions = "Treated variable (outlier)",
          measureTypes = output_measure_types
        )
        
        for (var in names(all_treated_vectors)) {
          self$results$treatedData$setValues(
            key    = var,
            values = all_treated_vectors[[var]]
          )
        }
        
        self$results$treatedData$setRowNums(1:nrow(self$data))
      }
      # --- FIM DO BLOCO DE OUTPUT ---
      
      if (self$options$dispBox && length(all_plot_data) > 0) {
        image <- self$results$plot
        image$setState(list(plot_data = all_plot_data, iqr_mult = iqr_mult))
      } else if (self$options$dispBox) {
        image <- self$results$plot
        image$setState(NULL)
      }
    },
    
    .plot = function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      
      plot_list  <- list()
      iqr_mult   <- image$state$iqr_mult
      group_vars <- self$options$group
      
      shapes <- c("Non-Outlier" = 16, "1.5" = 16, "2.2" = 15, "3.0" = 17)
      colors <- c("Non-Outlier" = "black", "1.5" = "red", "2.2" = "blue", "3.0" = "purple")
      
      for (var in self$options$vars) {
        all_data_for_var <- NULL
        
        for (var_group_name in names(image$state$plot_data)) {
          plotData <- image$state$plot_data[[var_group_name]]
          if (plotData$variable[1] == var) {
            all_data_for_var <- rbind(all_data_for_var, plotData)
          }
        }
        
        if (is.null(all_data_for_var)) next
        
        has_group <- "group" %in% names(all_data_for_var) && length(unique(all_data_for_var$group)) > 1
        
        if (!has_group) {
          all_data_for_var$x_axis_label <- "Data"
        }
        
        active_levels <- c("Non-Outlier", as.character(iqr_mult))
        active_colors <- colors[active_levels]
        active_shapes <- shapes[active_levels]
        
        outliers_data <- subset(all_data_for_var, outlierLevel != "Non-Outlier")
        
        legend_breaks <- as.character(iqr_mult)
        legend_labels <- paste("Outlier (", iqr_mult, " IQR)", sep = "")
        
        if (has_group) {
          x_axis_title <- paste(group_vars, collapse = " x ")
          
          p <- ggplot2::ggplot(all_data_for_var, ggplot2::aes(x = group, y = value)) +
            ggplot2::geom_boxplot(ggplot2::aes(group = group), outlier.shape = NA) +
            ggplot2::geom_point(data = outliers_data,
                                ggplot2::aes(x = group, y = value, color = outlierLevel, shape = outlierLevel),
                                size = 3, position = ggplot2::position_dodge(width = 0.2)) +
            ggplot2::geom_text(data = outliers_data,
                               ggplot2::aes(x = group, y = value, label = paste("", rowIndex), color = outlierLevel),
                               vjust = 0.4, hjust = -0.1, size = 4, show.legend = FALSE,
                               position = ggplot2::position_dodge(width = 0.2)) +
            ggplot2::scale_color_manual(values = active_colors, breaks = legend_breaks, labels = legend_labels) +
            ggplot2::scale_shape_manual(values = active_shapes, breaks = legend_breaks, labels = legend_labels) +
            ggplot2::labs(title = paste("Boxplot for", var), x = x_axis_title, y = "Value", color = "", shape = "") +
            ggplot2::theme_classic() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                           legend.position = "bottom")
          
        } else {
          p <- ggplot2::ggplot(all_data_for_var, ggplot2::aes(x = x_axis_label, y = value)) +
            ggplot2::geom_boxplot(outlier.shape = NA) +
            ggplot2::geom_point(data = outliers_data,
                                ggplot2::aes(x = x_axis_label, color = outlierLevel, shape = outlierLevel),
                                size = 3) +
            ggplot2::geom_text(data = outliers_data,
                               ggplot2::aes(x = x_axis_label, y = value, label = paste("", rowIndex), color = outlierLevel),
                               vjust = 0.4, hjust = -0.1, size = 4, show.legend = FALSE) +
            ggplot2::scale_color_manual(values = active_colors, breaks = legend_breaks, labels = legend_labels) +
            ggplot2::scale_shape_manual(values = active_shapes, breaks = legend_breaks, labels = legend_labels) +
            ggplot2::labs(title = paste("Boxplot for", var), x = "", y = "Value", color = "", shape = "") +
            ggplot2::theme_classic() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                           axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
                           legend.position = "bottom")
        }
        plot_list[[var]] <- p
      }
      
      if (length(plot_list) > 1) {
        n_plots <- length(plot_list)
        n_cols  <- min(3, n_plots)
        n_rows  <- ceiling(n_plots / n_cols)
        final_plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = n_cols, nrow = n_rows)
        print(final_plot)
      } else if (length(plot_list) == 1) {
        print(plot_list[[1]])
      }
      TRUE
    }
  )
)