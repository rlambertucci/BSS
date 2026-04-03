dixonQTestClass <- R6::R6Class(
  "dixonQTestClass",
  inherit = dixonQTestBase,
  private = list(
    
    .init = function() {
      info_html <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.5;">',
        '<ul>',
        '<li><b>Dixon\'s Q Test</b> &rarr; A statistical test for detecting a single outlier in a ',
        'small dataset. The Q statistic is the ratio of the gap between the suspect value and its ',
        'nearest neighbor to the range of the dataset. Designed for sample sizes between 3 and 30. ',
        '<i>Dixon, W. J. (1950).</i></li>',
        '<li><b>Two-sided test</b> &rarr; Both the minimum and maximum values are tested as potential ',
        'outliers. The value producing the largest Q statistic is reported as the suspect outlier.</li>',
        '<li><b>Critical values</b> &rarr; Based on Dixon (1950) and Rorabacher (1991) tables, ',
        'varying by sample size and significance level.</li>',
        '</ul>',
        '<p style="font-style: italic; padding-top: 10px;">',
        'Note: Dixon\'s Q Test assumes normality and is sensitive to sample size. ',
        'It should only be applied to datasets with N between 3 and 30.',
        '</p>',
        '</div>'
      )
      self$results$dixonInfo$setContent(info_html)
    },
    
    .run = function() {
      if (is.null(self$options$var)) return()
      
      var        <- self$options$var
      has_group  <- !is.null(self$options$group) && length(self$options$group) > 0
      group_vars <- self$options$group
      treatment_type <- self$options$treatment
      alpha_key  <- self$options$alpha
      
      # Tabela de valores críticos de Dixon (two-sided)
      q_critical_table <- list(
        alpha10 = c(
          "3"=0.941, "4"=0.765, "5"=0.642, "6"=0.560, "7"=0.507,
          "8"=0.554, "9"=0.512, "10"=0.477, "11"=0.576, "12"=0.546,
          "13"=0.521, "14"=0.546, "15"=0.525, "16"=0.507, "17"=0.490,
          "18"=0.475, "19"=0.462, "20"=0.450, "21"=0.440, "22"=0.430,
          "23"=0.421, "24"=0.413, "25"=0.406, "26"=0.399, "27"=0.393,
          "28"=0.387, "29"=0.381, "30"=0.376
        ),
        alpha05 = c(
          "3"=0.970, "4"=0.829, "5"=0.710, "6"=0.625, "7"=0.568,
          "8"=0.608, "9"=0.564, "10"=0.530, "11"=0.619, "12"=0.590,
          "13"=0.565, "14"=0.590, "15"=0.568, "16"=0.548, "17"=0.531,
          "18"=0.516, "19"=0.503, "20"=0.491, "21"=0.480, "22"=0.470,
          "23"=0.461, "24"=0.452, "25"=0.443, "26"=0.435, "27"=0.428,
          "28"=0.421, "29"=0.415, "30"=0.409
        ),
        alpha01 = c(
          "3"=0.994, "4"=0.926, "5"=0.821, "6"=0.740, "7"=0.680,
          "8"=0.717, "9"=0.672, "10"=0.635, "11"=0.713, "12"=0.675,
          "13"=0.649, "14"=0.674, "15"=0.647, "16"=0.624, "17"=0.605,
          "18"=0.589, "19"=0.575, "20"=0.562, "21"=0.551, "22"=0.541,
          "23"=0.532, "24"=0.524, "25"=0.516, "26"=0.508, "27"=0.501,
          "28"=0.495, "29"=0.489, "30"=0.483
        )
      )
      
      # Função para calcular Q estatístico
      calc_q <- function(sorted, n, side) {
        if (side == "lower") {
          if (n >= 3 && n <= 7) {
            q <- (sorted[2] - sorted[1]) / (sorted[n] - sorted[1])
          } else if (n >= 8 && n <= 10) {
            q <- (sorted[3] - sorted[1]) / (sorted[n] - sorted[1])
          } else if (n >= 11 && n <= 13) {
            q <- (sorted[3] - sorted[1]) / (sorted[n-2] - sorted[1])
          } else {
            q <- (sorted[4] - sorted[1]) / (sorted[n-3] - sorted[1])
          }
          list(q = q, value = sorted[1])
        } else {
          if (n >= 3 && n <= 7) {
            q <- (sorted[n] - sorted[n-1]) / (sorted[n] - sorted[1])
          } else if (n >= 8 && n <= 10) {
            q <- (sorted[n] - sorted[n-2]) / (sorted[n] - sorted[1])
          } else if (n >= 11 && n <= 13) {
            q <- (sorted[n] - sorted[n-2]) / (sorted[n] - sorted[3])
          } else {
            q <- (sorted[n] - sorted[n-3]) / (sorted[n] - sorted[4])
          }
          list(q = q, value = sorted[n])
        }
      }
      
      # Agrupamento
      if (has_group) {
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
      
      # Inicializa vetor tratado
      full_treated_data <- as.numeric(self$data[[var]])
      all_plot_data     <- list()
      row_counter       <- 1
      outlier_counter   <- 1
      
      for (group_name in names(data_grouped)) {
        data_group <- data_grouped[[group_name]]
        data_var   <- as.numeric(data_group[[var]])
        data_var_clean <- data_var[!is.na(data_var)]
        n <- length(data_var_clean)
        
        original_row_numbers <- as.numeric(rownames(data_group))
        
        # Validação por grupo
        if (n < 3) {
          self$results$dixonTable$addRow(
            rowKey = row_counter,
            values = list(
              variable      = var,
              group_level   = group_name,
              n             = n,
              outlier_value = NA,
              outlier_side  = "—",
              q_statistic   = NA,
              q_critical    = NA,
              significant   = "N < 3 (invalid)"
            )
          )
          row_counter <- row_counter + 1
          next
        }
        
        if (n > 30) {
          self$results$dixonTable$addRow(
            rowKey = row_counter,
            values = list(
              variable      = var,
              group_level   = group_name,
              n             = n,
              outlier_value = NA,
              outlier_side  = "—",
              q_statistic   = NA,
              q_critical    = NA,
              significant   = "N > 30 (invalid)"
            )
          )
          row_counter <- row_counter + 1
          next
        }
        
        q_critical  <- q_critical_table[[alpha_key]][[as.character(n)]]
        sorted_data <- sort(data_var_clean)
        data_range  <- sorted_data[n] - sorted_data[1]
        
        if (data_range == 0) {
          self$results$dixonTable$addRow(
            rowKey = row_counter,
            values = list(
              variable      = var,
              group_level   = group_name,
              n             = n,
              outlier_value = NA,
              outlier_side  = "—",
              q_statistic   = NA,
              q_critical    = NA,
              significant   = "All values identical"
            )
          )
          row_counter <- row_counter + 1
          next
        }
        
        lower_result <- calc_q(sorted_data, n, "lower")
        upper_result <- calc_q(sorted_data, n, "upper")
        
        if (lower_result$q >= upper_result$q) {
          q_stat       <- lower_result$q
          outlier_val  <- lower_result$value
          outlier_side <- "Lower"
        } else {
          q_stat       <- upper_result$q
          outlier_val  <- upper_result$value
          outlier_side <- "Upper"
        }
        
        is_outlier  <- q_stat > q_critical
        significant <- if (is_outlier) "Yes" else "No"
        
        # Tratamento
        treated_values <- data_var
        modified_n     <- n
        applied_treatment_text <- "None"
        
        if (is_outlier && treatment_type != "none") {
          outlier_idx_in_group <- which(data_var == outlier_val)[1]
          
          if (treatment_type == "trim_asym") {
            treated_values[outlier_idx_in_group] <- NA
            modified_n <- n - 1
            applied_treatment_text <- "Asymmetric Trimming"
          } else if (treatment_type == "win_valid") {
            valid_data <- data_var_clean[data_var_clean != outlier_val]
            if (outlier_side == "Lower") {
              treated_values[outlier_idx_in_group] <- min(valid_data, na.rm = TRUE)
            } else {
              treated_values[outlier_idx_in_group] <- max(valid_data, na.rm = TRUE)
            }
            applied_treatment_text <- "Winsorized (Valid values)"
          }
          
          # Atualiza vetor principal
          full_treated_data[original_row_numbers] <- treated_values
        }
        
        # --- Tabela principal ---
        row_values <- list(
          variable      = var,
          group_level   = group_name,
          n             = n,
          outlier_value = outlier_val,
          outlier_side  = outlier_side,
          q_statistic   = round(q_stat, 4),
          q_critical    = round(q_critical, 4),
          significant   = significant
        )
        
        if (treatment_type != "none") {
          row_values$treatmentApplied <- if (is_outlier) applied_treatment_text else "No outlier detected"
          row_values$modifiedN        <- modified_n
        }
        
        self$results$dixonTable$addRow(rowKey = row_counter, values = row_values)
        row_counter <- row_counter + 1
        
        # --- Tabela de outliers identificados ---
        if (is_outlier) {
          outlier_idx_in_group <- which(data_var == outlier_val)[1]
          outlier_row_in_full  <- original_row_numbers[outlier_idx_in_group]
          
          self$results$outlierList$addRow(
            rowKey = outlier_counter,
            values = list(
              variable      = var,
              group_level   = group_name,
              rowNumber     = outlier_row_in_full,
              outlier_value = outlier_val,
              outlier_side  = outlier_side,
              q_statistic   = round(q_stat, 4)
            )
          )
          outlier_counter <- outlier_counter + 1
        }
        
        # --- Boxplot ---
        if (self$options$dispBox) {
          plot_df <- data.frame(
            value        = data_var,
            outlierLevel = ifelse(!is.na(data_var) & is_outlier & data_var == outlier_val,
                                  "Outlier", "Non-Outlier"),
            rowIndex     = original_row_numbers,
            group        = rep(group_name, length(data_var)),
            stringsAsFactors = FALSE
          )
          all_plot_data[[group_name]] <- plot_df
        }
        
      } # Fim do loop de grupos
      
      # --- Output para planilha ---
      if (treatment_type != "none" && self$results$treatedData$isNotFilled()) {
        
        output_title <- if (treatment_type == "trim_asym") {
          paste0(var, "_trimmed")
        } else {
          paste0(var, "_winsorzd")
        }
        
        self$results$treatedData$set(
          keys         = var,
          titles       = output_title,
          descriptions = "Treated variable (Dixon Q Test)",
          measureTypes = "continuous"
        )
        self$results$treatedData$setValues(
          key    = var,
          values = full_treated_data
        )
        self$results$treatedData$setRowNums(1:nrow(self$data))
      }
      
      # --- Estado do plot ---
      if (self$options$dispBox && length(all_plot_data) > 0) {
        self$results$plot$setState(list(
          plot_data  = all_plot_data,
          var_name   = var,
          has_group  = has_group,
          group_vars = group_vars
        ))
      }
    },
    
    .plot = function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      
      state      <- image$state
      plot_data  <- state$plot_data
      var_name   <- state$var_name
      has_group  <- state$has_group
      group_vars <- state$group_vars
      
      all_data    <- do.call(rbind, plot_data)
      outliers_df <- subset(all_data, outlierLevel == "Outlier")
      
      colors <- c("Non-Outlier" = "black", "Outlier" = "red")
      shapes <- c("Non-Outlier" = 16, "Outlier" = 16)
      
      if (has_group && length(unique(all_data$group)) > 1) {
        
        p <- ggplot2::ggplot(all_data, ggplot2::aes(x = group, y = value)) +
          ggplot2::geom_boxplot(ggplot2::aes(group = group), outlier.shape = NA) +
          ggplot2::geom_point(data = outliers_df,
                              ggplot2::aes(x = group, y = value, color = outlierLevel, shape = outlierLevel),
                              size = 3, position = ggplot2::position_dodge(width = 0.2)) +
          ggplot2::geom_text(data = outliers_df,
                             ggplot2::aes(x = group, y = value, label = paste("", rowIndex), color = outlierLevel),
                             vjust = 0.4, hjust = -0.1, size = 4, show.legend = FALSE,
                             position = ggplot2::position_dodge(width = 0.2)) +
          ggplot2::scale_color_manual(values = colors, breaks = "Outlier", labels = "Outlier") +
          ggplot2::scale_shape_manual(values = shapes, breaks = "Outlier", labels = "Outlier") +
          ggplot2::labs(title = paste("Dixon's Q Test Boxplot for", var_name),
                        x = paste(group_vars, collapse = " x "),
                        y = "Value", color = "", shape = "") +
          ggplot2::theme_classic() +
          ggplot2::theme(
            plot.title      = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom"
          )
        
      } else {
        all_data$x_label    <- "Data"
        outliers_df$x_label <- "Data"
        
        p <- ggplot2::ggplot(all_data, ggplot2::aes(x = x_label, y = value)) +
          ggplot2::geom_boxplot(outlier.shape = NA) +
          ggplot2::geom_point(data = outliers_df,
                              ggplot2::aes(x = x_label, color = outlierLevel, shape = outlierLevel),
                              size = 3) +
          ggplot2::geom_text(data = outliers_df,
                             ggplot2::aes(x = x_label, y = value, label = paste("", rowIndex), color = outlierLevel),
                             vjust = 0.4, hjust = -0.1, size = 4, show.legend = FALSE) +
          ggplot2::scale_color_manual(values = colors, breaks = "Outlier", labels = "Outlier") +
          ggplot2::scale_shape_manual(values = shapes, breaks = "Outlier", labels = "Outlier") +
          ggplot2::labs(title = paste("Dixon's Q Test Boxplot for", var_name),
                        x = "", y = "Value", color = "", shape = "") +
          ggplot2::theme_classic() +
          ggplot2::theme(
            plot.title      = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x     = ggplot2::element_blank(),
            axis.ticks.x    = ggplot2::element_blank(),
            legend.position = "bottom"
          )
      }
      
      print(p)
      TRUE
    }
  )
)