outliersClass <- R6::R6Class(
  "outliersClass",
  inherit = outliersBase,
  private = list(
    
    .init = function() {
      
      info_html_dixon <- paste0(
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
      self$results$dixonInfo$setContent(info_html_dixon)
      self$results$dixonTable$setNote(
        "citation", 
        "Citations: Dixon, W. J. (1950). Analysis of extreme values. Annals of Mathematical Statistics, 21(4), 488-506. Rorabacher, D. B. (1991). Statistical treatment for rejection of deviant values: critical values of Dixon's Q parameter. Analytical Chemistry, 63(2), 139-146."
      )
      
      info_html_modz <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.5;">',
        '<ul>',
        '<li><b>2.5</b> &rarr; High sensitivity; recommended for small samples or experimental designs where outliers significantly bias central tendency and dispersion estimates. <i>Leys et al. (2013).</i></li>',
        '<li><b>3.0</b> &rarr; Conservative approach; ideal for rigorous experimental environments where data stability is paramount and variability must be strictly accounted for. <i>Wilcox (2017).</i></li>',
        '<li><b>3.5</b> &rarr; Academic standard; the most widely accepted criterion in robust statistics for general outlier identification. <i>Iglewicz & Hoaglin (1993).</i></li>',
        '<li><b>4.0</b> &rarr; Low sensitivity; suitable for large datasets where naturally occurring extreme values are expected and should be preserved to maintain heterogeneity. <i>Iglewicz & Hoaglin (1993).</i></li>',
        '</ul>',
        '<p style="font-style: italic; padding-top: 10px;">',
        'Modified Z-Score: M<sub>i</sub> = 0.6745 &times; (x<sub>i</sub> - median) / MAD. ',
        'Values with |M<sub>i</sub>| greater than the threshold are considered outliers.',
        '</p>',
        '</div>'
      )
      self$results$modzInfo$setContent(info_html_modz)
      self$results$modzTable$setNote(
        "citation",
        "Citations: Iglewicz, B., & Hoaglin, D. C. (1993). How to detect and handle outliers. ASQC Quality Press. Leys, C., et al. (2013). Detecting outliers: Do not use standard deviation and mean, use absolute deviation around the median. Journal of Experimental Social Psychology, 49(4), 764-766."
      )
      
      info_html_iqr <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.5;">', 
        '<ul>',
        '<li><b>1.5 IQR</b> &rarr; Standard criterion; ideal for general outlier detection and common boxplot visualization. <i>Tukey, J. W. (1977).</i></li>',
        '<li><b>2.2 IQR</b> &rarr; Balanced approach; recommended for a more conservative detection while preserving more data variance. <i>Hoaglin & Iglewicz (1987).</i></li>',
        '<li><b>3.0 IQR</b> &rarr; Extreme criterion; identifies only blatant anomalies, highly effective for removing extreme outliers. <i>Tukey, J. W. (1977).</i></li>',
        '</ul>',
        '<p style="font-style: italic; padding-top: 10px;">Note: Lower multipliers are more aggressive in identifying outliers, while higher multipliers are more tolerant.</p>',
        '</div>'
      )
      self$results$iqrInfo$setContent(info_html_iqr)
      self$results$iqrTable$setNote(
        "citation",
        "Citations: Tukey, J. W. (1977). Exploratory data analysis. Addison-Wesley. Hoaglin, D. C., & Iglewicz, B. (1987). Fine-tuning some outlier diagnostics. Journal of the American Statistical Association, 82(400), 1147-1149."
      )
    },
    
    .run = function() {
      mode <- self$options$mode
      if (mode == "dixon") {
        private$.runDixon()
      } else if (mode == "modz") {
        private$.runModZ()
      } else if (mode == "iqr") {
        private$.runIQR()
      }
    },
    
    .runDixon = function() {
      if (is.null(self$options$var_dixon)) return()
      
      try(self$results$dixonTable$deleteRows(), silent = TRUE)
      try(self$results$dixonOutlierList$deleteRows(), silent = TRUE)
      
      var        <- self$options$var_dixon
      has_group  <- !is.null(self$options$group_dixon) && length(self$options$group_dixon) > 0
      group_vars <- self$options$group_dixon
      treatment_type <- self$options$treatment_dixon
      alpha_key  <- self$options$alpha_dixon
      
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
      
      calc_q <- function(sorted, n, side) {
        if (side == "lower") {
          if (n >= 3 && n <= 7) {
            q <- (sorted[2] - sorted[1]) / (sorted[n] - sorted[1])
          } else if (n >= 8 && n <= 10) {
            q <- (sorted[2] - sorted[1]) / (sorted[n-1] - sorted[1])
          } else if (n >= 11 && n <= 13) {
            q <- (sorted[3] - sorted[1]) / (sorted[n-1] - sorted[1])
          } else {
            q <- (sorted[3] - sorted[1]) / (sorted[n-2] - sorted[1])
          }
          list(q = q, value = sorted[1])
        } else {
          if (n >= 3 && n <= 7) {
            q <- (sorted[n] - sorted[n-1]) / (sorted[n] - sorted[1])
          } else if (n >= 8 && n <= 10) {
            q <- (sorted[n] - sorted[n-1]) / (sorted[n] - sorted[2])
          } else if (n >= 11 && n <= 13) {
            q <- (sorted[n] - sorted[n-2]) / (sorted[n] - sorted[2])
          } else {
            q <- (sorted[n] - sorted[n-2]) / (sorted[n] - sorted[3])
          }
          list(q = q, value = sorted[n])
        }
      }
      
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
            outlier_match_idx <- which(data_var_clean == outlier_val)[1]
            valid_data <- data_var_clean[-outlier_match_idx]
            if (outlier_side == "Lower") {
              treated_values[outlier_idx_in_group] <- min(valid_data, na.rm = TRUE)
            } else {
              treated_values[outlier_idx_in_group] <- max(valid_data, na.rm = TRUE)
            }
            applied_treatment_text = "Winsorized (Valid values)"
          }
          
          full_treated_data[original_row_numbers] <- treated_values
        }
        
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
        
        if (is_outlier) {
          outlier_idx_in_group <- which(data_var == outlier_val)[1]
          outlier_row_in_full  <- original_row_numbers[outlier_idx_in_group]
          
          self$results$dixonOutlierList$addRow(
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
        
        if (self$options$dispBox_dixon) {
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
        
      }
      
      if (treatment_type != "none" && self$results$treatedData_dixon$isNotFilled()) {
        output_title <- if (treatment_type == "trim_asym") {
          paste0(var, "_trimmed")
        } else {
          paste0(var, "_winsorzd")
        }
        
        self$results$treatedData_dixon$set(
          keys         = var,
          titles       = output_title,
          descriptions = "Treated variable (Dixon Q Test)",
          measureTypes = "continuous"
        )
        self$results$treatedData_dixon$setValues(
          key    = var,
          values = full_treated_data
        )
        self$results$treatedData_dixon$setRowNums(1:nrow(self$data))
      }
      
      if (self$options$dispBox_dixon && length(all_plot_data) > 0) {
        self$results$dixonPlot$setState(list(
          plot_data  = all_plot_data,
          var_name   = var,
          has_group  = has_group,
          group_vars = group_vars
        ))
      }
    },
    
    .runModZ = function() {
      if (is.null(self$options$vars_modz) || length(self$options$vars_modz) == 0)
        return()
      
      try(self$results$modzTable$deleteRows(), silent = TRUE)
      try(self$results$modzOutlierList$deleteRows(), silent = TRUE)
      
      table         <- self$results$modzTable
      outlierList   <- self$results$modzOutlierList
      all_plot_data <- list()
      has_group     <- !is.null(self$options$group_modz) && length(self$options$group_modz) > 0
      group_vars    <- self$options$group_modz
      
      thresh_map <- c("t25" = 2.5, "t30" = 3.0, "t35" = 3.5, "t40" = 4.0)
      label_map  <- c("t25" = "2.5", "t30" = "3.0", "t35" = "3.5", "t40" = "4.0")
      
      threshold    <- thresh_map[[self$options$threshold_modz]]
      thresh_label <- label_map[[self$options$threshold_modz]]
      
      treatment_type  <- self$options$treatment_modz
      row_counter     <- 1
      outlier_counter <- 1
      
      all_treated_vectors <- list()
      
      for (var in self$options$vars_modz) {
        if (!var %in% names(self$data)) next
        
        tryCatch({
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
          
          full_treated_data <- rep(NA_real_, nrow(self$data))
          full_treated_data[seq_along(self$data[[var]])] <- self$data[[var]]
          
          for (group_name in names(data_grouped)) {
            data_group    <- data_grouped[[group_name]]
            data_var_full <- data_group[[var]]
            
            non_na_indices_in_group <- which(!is.na(data_var_full))
            data_var <- as.numeric(data_var_full[non_na_indices_in_group])
            
            if (length(data_var) == 0) next
            
            original_row_numbers_group <- as.numeric(rownames(data_group))
            
            med <- stats::median(data_var, na.rm = TRUE)
            mad <- stats::mad(data_var, na.rm = TRUE)
            
            if (mad == 0) mad <- stats::mean(abs(data_var - med), na.rm = TRUE)
            if (mad == 0) mad <- 1
            
            modified_z <- 0.6745 * (data_var - med) / mad
            
            lower_threshold <- med - (threshold * mad / 0.6745)
            upper_threshold <- med + (threshold * mad / 0.6745)
            
            outliers_indices_in_datavar <- which(abs(modified_z) > threshold)
            outliers_indices_in_group   <- non_na_indices_in_group[outliers_indices_in_datavar]
            
            outlierLevel_full <- rep("Non-Outlier", nrow(data_group))
            outlierLevel_full[outliers_indices_in_group] <- thresh_label
            
            rowIndex_full <- rep(NA_integer_, nrow(data_group))
            rowIndex_full[outliers_indices_in_group] <- original_row_numbers_group[outliers_indices_in_group]
            
            treated_values <- data_var_full
            modified_n     <- length(non_na_indices_in_group)
            applied_treatment_text <- "None"
            
            if (treatment_type != "none") {
              if (treatment_type == "trim_asym") {
                treated_values[outliers_indices_in_group] <- NA
                modified_n <- modified_n - length(outliers_indices_in_group)
                applied_treatment_text <- "Asymmetric Trimming"
              } else if (treatment_type == "trim_sym") {
                trim_prop      <- self$options$trimPercent_modz / 100
                lower_quantile <- stats::quantile(data_var, trim_prop, na.rm = TRUE)
                upper_quantile <- stats::quantile(data_var, 1 - trim_prop, na.rm = TRUE)
                trim_indices   <- which(!is.na(treated_values) & (treated_values < lower_quantile | treated_values > upper_quantile))
                treated_values[trim_indices] <- NA
                modified_n <- length(which(!is.na(treated_values)))
                applied_treatment_text <- paste0("Symmetric Trimming (", self$options$trimPercent_modz, "%)")
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
            
            full_treated_data[original_row_numbers_group] <- treated_values
            
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
            
            if (self$options$dispBox_modz) {
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
          }
          
          if (treatment_type != "none") {
            all_treated_vectors[[var]] <- full_treated_data
          }
          
        }, error = function(e) {
          jmvcore::reject(paste("Error processing variable", var, ":", e$message), code = "error_processing_variable")
        })
      }
      
      if (treatment_type != "none" && length(all_treated_vectors) > 0 && self$results$treatedData_modz$isNotFilled()) {
        output_keys          <- character()
        output_titles        <- character()
        output_measure_types <- character()
        
        suffix <- ""
        if (treatment_type %in% c("trim_asym", "trim_sym")) {
          suffix <- "_trimmed"
        } else if (treatment_type %in% c("win_valid", "win_fence")) {
          suffix <- "_winsorzd"
        }
        
        for (var in names(all_treated_vectors)) {
          output_keys          <- c(output_keys,          var)
          output_titles        <- c(output_titles,        paste0(var, suffix))
          output_measure_types <- c(output_measure_types, "continuous")
        }
        
        self$results$treatedData_modz$set(
          keys         = output_keys,
          titles       = output_titles,
          descriptions = "Treated variable (Modified Z)",
          measureTypes = output_measure_types
        )
        
        for (var in names(all_treated_vectors)) {
          self$results$treatedData_modz$setValues(
            key    = var,
            values = all_treated_vectors[[var]]
          )
        }
        self$results$treatedData_modz$setRowNums(1:nrow(self$data))
      }
      
      if (self$options$dispBox_modz && length(all_plot_data) > 0) {
        self$results$modzPlot$setState(list(plot_data = all_plot_data, threshold = threshold, thresh_label = thresh_label))
      } else if (self$options$dispBox_modz) {
        self$results$modzPlot$setState(NULL)
      }
    },
    
    .runIQR = function() {
      if (is.null(self$options$vars_iqr) || length(self$options$vars_iqr) == 0)
        return()
      
      try(self$results$iqrTable$deleteRows(), silent = TRUE)
      
      table         <- self$results$iqrTable
      all_plot_data <- list()
      has_group     <- !is.null(self$options$group_iqr) && length(self$options$group_iqr) > 0
      group_vars    <- self$options$group_iqr
      
      mult_map  <- c("iqr15" = 1.5, "iqr22" = 2.2, "iqr30" = 3.0)
      label_map <- c("iqr15" = "1.5", "iqr22" = "2.2", "iqr30" = "3.0")
      
      iqr_mult  <- mult_map[[self$options$iqrMultiplier_iqr]]
      iqr_label <- label_map[[self$options$iqrMultiplier_iqr]]
      
      treatment_type <- self$options$treatment_iqr
      row_counter    <- 1
      
      all_treated_vectors <- list()
      
      for (var in self$options$vars_iqr) {
        if (!var %in% names(self$data)) next
        
        tryCatch({
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
          
          full_treated_data <- rep(NA_real_, nrow(self$data))
          full_treated_data[seq_along(self$data[[var]])] <- self$data[[var]]
          
          for (group_name in names(data_grouped)) {
            data_group    <- data_grouped[[group_name]]
            data_var_full <- data_group[[var]]
            
            non_na_indices_in_group <- which(!is.na(data_var_full))
            data_var <- as.numeric(data_var_full[non_na_indices_in_group])
            
            if (length(data_var) == 0) next
            
            original_row_numbers_group <- as.numeric(rownames(data_group))
            
            iqr_value <- stats::IQR(data_var, na.rm = TRUE)
            q1 <- stats::quantile(data_var, 0.25, na.rm = TRUE)
            q3 <- stats::quantile(data_var, 0.75, na.rm = TRUE)
            
            lower_fence <- q1 - (iqr_mult * iqr_value)
            upper_fence <- q3 + (iqr_mult * iqr_value)
            
            outliers_indices_in_datavar <- which(data_var < lower_fence | data_var > upper_fence)
            outliers_indices_in_group   <- non_na_indices_in_group[outliers_indices_in_datavar]
            
            outlierLevel_full <- rep("Non-Outlier", nrow(data_group))
            outlierLevel_full[outliers_indices_in_group] <- iqr_label
            
            rowIndex_full <- rep(NA_integer_, nrow(data_group))
            rowIndex_full[outliers_indices_in_group] <- original_row_numbers_group[outliers_indices_in_group]
            
            treated_values <- data_var_full
            modified_n     <- length(non_na_indices_in_group)
            applied_treatment_text <- "None"
            
            if (treatment_type != "none") {
              if (treatment_type == "trim_asym") {
                treated_values[outliers_indices_in_group] <- NA
                modified_n <- modified_n - length(outliers_indices_in_group)
                applied_treatment_text <- "Asymmetric Trimming"
              } else if (treatment_type == "trim_sym") {
                trim_prop      <- self$options$trimPercent_iqr / 100
                lower_quantile <- stats::quantile(data_var, trim_prop, na.rm = TRUE)
                upper_quantile <- stats::quantile(data_var, 1 - trim_prop, na.rm = TRUE)
                trim_indices   <- which(!is.na(treated_values) & (treated_values < lower_quantile | treated_values > upper_quantile))
                treated_values[trim_indices] <- NA
                modified_n <- length(which(!is.na(treated_values)))
                applied_treatment_text <- paste0("Symmetric Trimming (", self$options$trimPercent_iqr, "%)")
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
            
            if (self$options$dispBox_iqr) {
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
          }
          
          if (treatment_type != "none") {
            all_treated_vectors[[var]] <- full_treated_data
          }
          
        }, error = function(e) {
          jmvcore::reject(paste("Error processing variable", var, ":", e$message), code = "error_processing_variable")
        })
      }
      
      if (treatment_type != "none" && length(all_treated_vectors) > 0 && self$results$treatedData_iqr$isNotFilled()) {
        output_keys          <- character()
        output_titles        <- character()
        output_measure_types <- character()
        
        suffix <- ""
        if (treatment_type %in% c("trim_asym", "trim_sym")) {
          suffix <- "_trimmed"
        } else if (treatment_type %in% c("win_valid", "win_fence")) {
          suffix <- "_winsorzd"
        }
        
        for (var in names(all_treated_vectors)) {
          output_keys          <- c(output_keys,          var)
          output_titles        <- c(output_titles,        paste0(var, suffix))
          output_measure_types <- c(output_measure_types, "continuous")
        }
        
        self$results$treatedData_iqr$set(
          keys         = output_keys,
          titles       = output_titles,
          descriptions = "Treated variable (IQR)",
          measureTypes = output_measure_types
        )
        
        for (var in names(all_treated_vectors)) {
          self$results$treatedData_iqr$setValues(
            key    = var,
            values = all_treated_vectors[[var]]
          )
        }
        self$results$treatedData_iqr$setRowNums(1:nrow(self$data))
      }
      
      if (self$options$dispBox_iqr && length(all_plot_data) > 0) {
        self$results$iqrPlot$setState(list(plot_data = all_plot_data, iqr_mult = iqr_mult, iqr_label = iqr_label))
      } else if (self$options$dispBox_iqr) {
        self$results$iqrPlot$setState(NULL)
      }
    },
    
    .plot = function(image, ...) {
      mode <- self$options$mode
      if (mode == "dixon") {
        return(private$.plotDixon(image, ...))
      } else if (mode == "modz") {
        return(private$.plotModZ(image, ...))
      } else if (mode == "iqr") {
        return(private$.plotIQR(image, ...))
      }
      FALSE
    },
    
    .plotDixon = function(image, ...) {
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
    },
    
    .plotModZ = function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      
      plot_list    <- list()
      threshold    <- image$state$threshold
      thresh_label <- image$state$thresh_label
      group_vars   <- self$options$group_modz
      
      shapes <- c("Non-Outlier" = 16, "2.5" = 16, "3.0" = 17, "3.5" = 15, "4.0" = 18)
      colors <- c("Non-Outlier" = "black", "2.5" = "orange", "3.0" = "red", "3.5" = "blue", "4.0" = "purple")
      
      for (var in self$options$vars_modz) {
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
    },
    
    .plotIQR = function(image, ...) {
      if (is.null(image$state)) return(FALSE)
      
      plot_list  <- list()
      iqr_label  <- image$state$iqr_label
      group_vars <- self$options$group_iqr
      
      shapes <- c("Non-Outlier" = 16, "1.5" = 16, "2.2" = 15, "3.0" = 17)
      colors <- c("Non-Outlier" = "black", "1.5" = "red", "2.2" = "blue", "3.0" = "purple")
      
      for (var in self$options$vars_iqr) {
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
        
        active_levels <- c("Non-Outlier", iqr_label)
        active_colors <- colors[active_levels]
        active_shapes <- shapes[active_levels]
        
        outliers_data <- subset(all_data_for_var, outlierLevel != "Non-Outlier")
        
        legend_breaks <- iqr_label
        legend_labels <- paste0("Outlier (", iqr_label, " IQR)")
        
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
