ModifiedZClass <- R6::R6Class(
  "ModifiedZClass",
  inherit = ModifiedZBase,
  private = list(
    
    .init = function() {
      info_html <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.5;">',
        '<ul>',
        '<li><b>2.5</b> &rarr; High sensitivity; recommended for small samples or experimental designs where outliers significantly bias central tendency and dispersion estimates. <i>Leys et al. (2013).</i></li>',
        '<li><b>3.0</b> &rarr; Conservative approach; ideal for rigorous experimental environments where data stability is paramount and variability must be strictly accounted for. <i>Wilcox (2017).</i></li>',
        '<li><b>3.5</b> &rarr; Academic standard; the most widely accepted criterion in robust statistics for general outlier identification. <i>Iglewicz & Hoaglin (1993).</i></li>',
        '<li><b>4.0</b> &rarr; Low sensitivity; suitable for large datasets where naturally occurring extreme values are expected and should be preserved to maintain population heterogeneity. <i>Iglewicz & Hoaglin (1993).</i></li>',
        '</ul>',
        '<p style="font-style: italic; padding-top: 10px;">',
        'Modified Z-Score: M<sub>i</sub> = 0.6745 &times; (x<sub>i</sub> - median) / MAD. ',
        'Values with |M<sub>i</sub>| greater than the threshold are considered outliers.',
        '</p>',
        '</div>'
      )
      self$results$iqrInfo$setContent(info_html)
    },
    
    .run = function() {
      if (is.null(self$options$vars) || length(self$options$vars) == 0)
        return()
      
      table         <- self$results$outlierTable
      outlierList   <- self$results$outlierList
      all_plot_data <- list()
      has_group     <- !is.null(self$options$group) && length(self$options$group) > 0
      group_vars    <- self$options$group
      
      # Mapeamento do valor numérico e do rótulo de texto (evita o R transformar 3.0 em 3)
      thresh_map <- c("t25" = 2.5, "t30" = 3.0, "t35" = 3.5, "t40" = 4.0)
      label_map  <- c("t25" = "2.5", "t30" = "3.0", "t35" = "3.5", "t40" = "4.0")
      
      threshold    <- thresh_map[[self$options$threshold]]
      thresh_label <- label_map[[self$options$threshold]]
      
      treatment_type  <- self$options$treatment
      row_counter     <- 1
      outlier_counter <- 1
      
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
            
            if (length(data_var) == 0) next
            
            original_row_numbers_group <- as.numeric(rownames(data_group))
            
            # Cálculo do Modified Z-Score via MAD
            med <- stats::median(data_var, na.rm = TRUE)
            mad <- stats::mad(data_var, na.rm = TRUE)
            
            # Evita divisão por zero quando MAD = 0
            if (mad == 0) mad <- stats::mean(abs(data_var - med), na.rm = TRUE)
            if (mad == 0) mad <- 1
            
            modified_z <- 0.6745 * (data_var - med) / mad
            
            # Limites equivalentes em unidades originais
            lower_threshold <- med - (threshold * mad / 0.6745)
            upper_threshold <- med + (threshold * mad / 0.6745)
            
            outliers_indices_in_datavar <- which(abs(modified_z) > threshold)
            outliers_indices_in_group   <- non_na_indices_in_group[outliers_indices_in_datavar]
            
            outlierLevel_full <- rep("Non-Outlier", nrow(data_group))
            outlierLevel_full[outliers_indices_in_group] <- thresh_label # Usa o texto fixo "3.0", "4.0", etc.
            
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
                valid_data <- data_var[abs(modified_z) <= threshold]
                min_valid  <- min(valid_data, na.rm = TRUE)
                max_valid  <- max(valid_data, na.rm = TRUE)
                low_outliers  <- which(!is.na(treated_values) & treated_values < lower_threshold)
                high_outliers <- which(!is.na(treated_values) & treated_values > upper_threshold)
                treated_values[low_outliers]  <- min_valid
                treated_values[high_outliers] <- max_valid
                applied_treatment_text <- "Winsorized (Valid values)"
              } else if (treatment_type == "win_fence") {
                low_outliers  <- which(!is.na(treated_values) & treated_values < lower_threshold)
                high_outliers <- which(!is.na(treated_values) & treated_values > upper_threshold)
                treated_values[low_outliers]  <- lower_threshold
                treated_values[high_outliers] <- upper_threshold
                applied_treatment_text <- "Winsorized (Threshold limits)"
              }
            }
            
            # Insere os dados tratados deste grupo no vetor principal
            full_treated_data[original_row_numbers_group] <- treated_values
            
            # --- Tabela de resumo ---
            row_values_for_table <- list(
              variable       = var,
              group_level    = group_name,
              lowerThreshold = lower_threshold,
              upperThreshold = upper_threshold,
              numOutliers    = length(outliers_indices_in_datavar)
            )
            
            if (treatment_type != "none") {
              row_values_for_table$treatmentApplied <- applied_treatment_text
              row_values_for_table$modifiedN        <- modified_n
            }
            
            table$addRow(rowKey = row_counter, values = row_values_for_table)
            row_counter <- row_counter + 1
            
            # --- Tabela de outliers identificados ---
            if (length(outliers_indices_in_datavar) > 0) {
              for (i in seq_along(outliers_indices_in_datavar)) {
                idx_in_group <- outliers_indices_in_group[i]
                row_num      <- original_row_numbers_group[idx_in_group]
                z_score      <- modified_z[outliers_indices_in_datavar[i]]
                
                outlierList$addRow(
                  rowKey = outlier_counter,
                  values = list(
                    variable    = var,
                    group_level = group_name,
                    rowNumber   = row_num,
                    modifiedZ   = round(z_score, 4)
                  )
                )
                outlier_counter <- outlier_counter + 1
              }
            }
            
            if (self$options$dispBox) {
              plot_data <- data.frame(
                variable     = var,
                value        = data_var_full,
                outlierLevel = factor(outlierLevel_full, levels = c("Non-Outlier", "2.5", "3.0", "3.5", "4.0")),
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
        
        # Define o sufixo baseado no tratamento escolhido
        suffix <- ""
        if (treatment_type %in% c("trim_asym", "trim_sym")) {
          suffix <- "_trimmed"
        } else if (treatment_type %in% c("win_valid", "win_fence")) {
          suffix <- "_winsorzd"
        }
        
        for (var in names(all_treated_vectors)) {
          output_keys          <- c(output_keys,          var)
          output_titles        <- c(output_titles,        paste0(var, suffix)) # Aplica o sufixo condicional
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
        image$setState(list(plot_data = all_plot_data, threshold = threshold, thresh_label = thresh_label))
      } else if (self$options$dispBox) {
        image <- self$results$plot
        image$setState(NULL)
      }
    },
    
    .plot = function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      
      plot_list    <- list()
      threshold    <- image$state$threshold
      thresh_label <- image$state$thresh_label # Recupera o rótulo fixo (ex: "3.0")
      group_vars   <- self$options$group
      
      shapes <- c("Non-Outlier" = 16, "2.5" = 16, "3.0" = 17, "3.5" = 15, "4.0" = 18)
      colors <- c("Non-Outlier" = "black", "2.5" = "orange", "3.0" = "red", "3.5" = "blue", "4.0" = "purple")
      
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
        
        active_levels <- c("Non-Outlier", thresh_label)
        active_colors <- colors[active_levels]
        active_shapes <- shapes[active_levels]
        
        outliers_data <- subset(all_data_for_var, outlierLevel != "Non-Outlier")
        
        legend_breaks <- thresh_label
        legend_labels <- paste0("Outlier (|Z*| > ", thresh_label, ")")
        
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
            ggplot2::labs(title = paste("Modified Z-Score Boxplot for", var), x = x_axis_title, y = "Value", color = "", shape = "") +
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
            ggplot2::labs(title = paste("Modified Z-Score Boxplot for", var), x = "", y = "Value", color = "", shape = "") +
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