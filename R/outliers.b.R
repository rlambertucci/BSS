outliersClass <- R6::R6Class(
  "outliersClass",
  inherit = outliersBase,
  private = list(
    
    .to_numeric_vec = function(x) {
      if (is.null(x)) return(NULL)
      if (is.numeric(x)) return(as.numeric(x))
      if (is.factor(x)) {
        num_try <- suppressWarnings(as.numeric(as.character(x)))
        if (!all(is.na(num_try)) || all(is.na(x))) return(num_try)
        return(as.numeric(x))
      }
      if (is.character(x)) {
        return(suppressWarnings(as.numeric(x)))
      }
      suppressWarnings(as.numeric(x))
    },
    
    .medcouple = function(x) {
      x <- sort(na.omit(x))
      n <- length(x)
      if (n < 3) return(0)
      if (n > 1000) {
        idx <- round(seq(1, n, length.out = 1000))
        x <- x[idx]
        n <- length(x)
      }
      m <- stats::median(x)
      left <- x[x <= m]
      right <- x[x >= m]
      grid <- expand.grid(l = left, r = right)
      valid <- grid$r > grid$l
      if (!any(valid)) return(0)
      grid_valid <- grid[valid, ]
      h_vals <- ((grid_valid$r - m) - (m - grid_valid$l)) / (grid_valid$r - grid_valid$l)
      mc <- stats::median(h_vals)
      max(min(mc, 1), -1)
    },
    
    .update_help = function() {
      mode <- self$options$mode
      configured <- list(dixon = length(self$options$var_dixon) > 0, modz = length(self$options$vars_modz) > 0, iqr = length(self$options$vars_iqr) > 0, mah = length(self$options$vars_mah) > 0)
      for (name in names(configured)) {
        result <- self$results[[paste0(name, "Info")]]
        active_mode <- if (identical(name, "mah")) "mahalanobis" else name
        if (!is.null(result)) result$setVisible(identical(mode, active_mode) && !configured[[name]])
      }
    },

    .init = function() {
      for (variable in self$options$vars_modz) self$results$modzPlot$addItem(key = variable)
      for (variable in self$options$vars_iqr) self$results$iqrPlot$addItem(key = variable)
      private$.update_help()
      
      info_html_dixon <- paste0(
        '<div>',
        '<ul>',
        '<li><b>Dixon\'s Q Test</b> \u2192 A statistical test for detecting a single outlier in a ',
        'small dataset. The Q statistic is the ratio of the gap between the suspect value and its ',
        'nearest neighbor to the range of the dataset. Designed for sample sizes between 3 and 30. ',
        '<i>Dixon, W. J. (1950).</i></li>',
        '<li><b>Two-sided test</b> \u2192 Both the minimum and maximum values are tested as potential ',
        'outliers. The value producing the largest Q statistic is reported as the suspect outlier.</li>',
        '<li><b>Critical values</b> \u2192 Based on Dixon (1950) and Rorabacher (1991) tables, ',
        'varying by sample size and significance level.</li>',
        '</ul>',
        '<p>',
        'Note: Dixon\'s Q Test assumes normality and is sensitive to sample size. ',
        'It should only be applied to datasets with N between 3 and 30.',
        '</p>',
        '</div>'
      )
      self$results$dixonInfo$setContent(info_html_dixon)
      
      info_html_modz <- paste0(
        '<div>',
        '<ul>',
        '<li><b>2.5</b> \u2192 High sensitivity; recommended for small samples or experimental designs where outliers significantly bias central tendency and dispersion estimates. <i>Leys et al. (2013).</i></li>',
        '<li><b>3.0</b> \u2192 Conservative approach; ideal for rigorous experimental environments where data stability is paramount and variability must be strictly accounted for. <i>Wilcox (2017).</i></li>',
        '<li><b>3.5</b> \u2192 Academic standard; the most widely accepted criterion in robust statistics for general outlier identification. <i>Iglewicz & Hoaglin (1993).</i></li>',
        '<li><b>4.0</b> \u2192 Low sensitivity; suitable for large datasets where naturally occurring extreme values are expected and should be preserved to maintain heterogeneity. <i>Iglewicz & Hoaglin (1993).</i></li>',
        '</ul>',
        '<p>',
        'Modified Z-Score: M<sub>i</sub> = 0.6745 \u00d7 (x<sub>i</sub> - median) / MAD. ',
        'Values with |M<sub>i</sub>| greater than the threshold are considered outliers. ',
        'When MAD is zero, the fallback is (x<sub>i</sub> - median) / (1.253314 \u00d7 MeanAD), ',
        'where MeanAD is the mean absolute deviation from the median (IBM SPSS Modeler Algorithms Guide). ',
        'Groups with no dispersion cannot be evaluated; their data are preserved and omitted from this method\'s plot.',
        '</p>',
        '</div>'
      )
      self$results$modzInfo$setContent(info_html_modz)
      
      info_html_iqr <- paste0(
        '<div>',
        '<ul>',
        '<li><b>1.5 IQR</b> \u2192 Standard criterion; ideal for general outlier detection and common boxplot visualization. <i>Tukey, J. W. (1977).</i></li>',
        '<li><b>2.2 IQR</b> \u2192 Balanced approach; recommended for a more conservative detection while preserving more data variance. <i>Hoaglin & Iglewicz (1987).</i></li>',
        '<li><b>3.0 IQR</b> \u2192 Extreme criterion; identifies only blatant anomalies, highly effective for removing extreme outliers. <i>Tukey, J. W. (1977).</i></li>',
        '<li><b>Adjusted Boxplot:</b> Modifies fences via Medcouple (MC) for skewed data. <i>Hubert & Vandervieren (2008).</i></li>',
        '</ul>',
        '<p>Note: Lower multipliers are more aggressive in identifying outliers, while higher multipliers are more tolerant.</p>',
        '</div>'
      )
      self$results$iqrInfo$setContent(info_html_iqr)
      
      info_html_mah <- paste0(
        '<div>',
        '<ul>',
        '<li><b>Classical Mahalanobis Distance:</b> Measures distance from multivariate centroid taking covariance structure into account. Follows a Chi-square distribution under multivariate normality. <i>Mahalanobis (1936).</i></li>',
        '<li><b>Robust MCD (Minimum Covariance Determinant):</b> Highly robust against masking and swamping by computing location and scatter from the most concentrated subset of observations. <i>Rousseeuw & Van Driessen (1999).</i></li>',
        '</ul>',
        '<p>',
        'Observations with D\u00b2 exceeding the critical \u03c7\u00b2 value at the selected \u03b1 level are identified as multivariate outliers.',
        '</p>',
        '</div>'
      )
      self$results$mahInfo$setContent(info_html_mah)
    },
    
    .set_variable_plots = function(plots, variables, plot_data, metadata = list()) {
      for (variable in variables) {
        item <- plots$get(key = variable)
        item$setState(NULL)
        selected <- Filter(function(rows) nrow(rows) > 0 && identical(as.character(rows$variable[1]), variable), plot_data)
        if (length(selected) > 0) item$setState(c(list(plot_data = selected), metadata))
      }
    },

    .run = function() {
      private$.update_help()
      mode <- self$options$mode
      if (mode == "dixon") {
        private$.runDixon()
      } else if (mode == "modz") {
        private$.runModZ()
      } else if (mode == "iqr") {
        private$.runIQR()
      } else if (mode == "mahalanobis") {
        private$.runMahalanobis()
      }
    },
    
    .runDixon = function() {
      if (!is.null(self$results$dixonPlot)) self$results$dixonPlot$setState(NULL)
      plot_data <- list()
      if (is.null(self$options$var_dixon)) return()
      
      if (self$results$dixonTable$rowCount > 0) try(self$results$dixonTable$deleteRows(), silent = TRUE)
      if (self$results$dixonOutlierList$rowCount > 0) try(self$results$dixonOutlierList$deleteRows(), silent = TRUE)
      
      var            <- self$options$var_dixon
      has_group      <- !is.null(self$options$group_dixon) && length(self$options$group_dixon) > 0
      group_vars     <- self$options$group_dixon
      treatment_type <- self$options$treatment_dixon
      alpha_key      <- self$options$alpha_dixon
      
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
      
      process_group <- function(data_sub, group_name) {
        vec_raw <- private$.to_numeric_vec(data_sub[[var]])
        valid_idx <- which(!is.na(vec_raw))
        vec <- vec_raw[valid_idx]
        n <- length(vec)
        plot_data[[group_name]] <<- data.frame(group = group_name, value = vec_raw, rowIndex = as.integer(rownames(data_sub)), outlierLevel = "Non-Outlier", stringsAsFactors = FALSE)
        
        if (n < 3 || n > 30) {
          self$results$dixonTable$addRow(rowKey = group_name, values = list(
            variable = var, group_level = group_name, n = n,
            significant = if (n < 3) "N < 3" else "N > 30"
          ))
          return(NULL)
        }
        if (length(unique(vec)) <= 1) {
          self$results$dixonTable$addRow(rowKey = group_name, values = list(
            variable = var, group_level = group_name, n = n,
            significant = "All values identical"
          ))
          return(NULL)
        }
        
        sorted_vec <- sort(vec)
        lower_res <- calc_q(sorted_vec, n, "lower")
        upper_res <- calc_q(sorted_vec, n, "upper")
        
        suspect <- if (lower_res$q >= upper_res$q) lower_res else upper_res
        suspect_side <- if (lower_res$q >= upper_res$q) "Low" else "High"
        
        q_crit <- q_critical_table[[alpha_key]][[as.character(n)]]
        is_sig <- suspect$q > q_crit
        if (is_sig) plot_data[[group_name]]$outlierLevel[which(vec_raw == suspect$value)] <<- "Outlier"
        
        row_vals <- list(
          variable = var, group_level = group_name, n = n,
          outlier_value = suspect$value, outlier_side = suspect_side,
          q_statistic = suspect$q, q_critical = q_crit,
          significant = if (is_sig) "Yes" else "No"
        )
        
        treated_vec <- vec_raw
        if (is_sig && treatment_type != "none") {
          out_idx <- which(vec_raw == suspect$value)
          if (treatment_type == "trim_asym") {
            treated_vec[out_idx] <- NA_real_
            row_vals$treatmentApplied <- "Asymmetric Trimming"
            row_vals$modifiedN <- n - length(out_idx)
          } else if (treatment_type == "win_valid") {
            valid_vals <- vec[vec != suspect$value]
            replacement <- if (suspect_side == "Low") min(valid_vals) else max(valid_vals)
            treated_vec[out_idx] <- replacement
            row_vals$treatmentApplied <- "Winsorized"
            row_vals$modifiedN <- n
          }
        }
        
        self$results$dixonTable$addRow(rowKey = group_name, values = row_vals)
        
        if (is_sig) {
          out_row_indices <- which(vec_raw == suspect$value)
          for (r_i in out_row_indices) {
            self$results$dixonOutlierList$addRow(rowKey = paste(group_name, r_i, sep="_"), values = list(
              variable = var, group_level = group_name, rowNumber = as.integer(rownames(data_sub)[r_i]),
              outlier_value = suspect$value, outlier_side = suspect_side, q_statistic = suspect$q
            ))
          }
        }
        treated_vec
      }
      
      df_work <- self$data
      if (has_group) {
        group_factor <- interaction(df_work[group_vars], drop = TRUE, sep = " * ")
        groups_list <- split(df_work, group_factor)
      } else {
        groups_list <- list("All Data" = df_work)
      }
      
      treated_full <- private$.to_numeric_vec(df_work[[var]])
      for (grp_name in names(groups_list)) {
        sub_df <- groups_list[[grp_name]]
        treated_sub <- process_group(sub_df, grp_name)
        if (!is.null(treated_sub)) {
          idx_sub <- which(if (has_group) group_factor == grp_name else rep(TRUE, nrow(df_work)))
          treated_full[idx_sub] <- treated_sub
        }
      }
      
      if (isTRUE(self$options$dispBox_dixon) && length(plot_data) > 0) {
        self$results$dixonPlot$setState(list(plot_data = plot_data, var_name = var, has_group = has_group, group_vars = group_vars))
      }

      if (treatment_type != "none" && self$results$treatedData_dixon$isNotFilled()) {
        self$results$treatedData_dixon$set(keys = var, titles = paste0(var, "_treated"), descriptions = "Treated variable (Dixon)", measureTypes = "continuous")
        self$results$treatedData_dixon$setValues(key = var, values = treated_full)
        self$results$treatedData_dixon$setRowNums(rownames(self$data))
      }
    },
    
    .runModZ = function() {
      if (is.null(self$options$vars_modz) || length(self$options$vars_modz) == 0) return()
      
      table <- self$results$modzTable
      outlierList <- self$results$modzOutlierList
      if (table$rowCount > 0) try(table$deleteRows(), silent = TRUE)
      if (outlierList$rowCount > 0) try(outlierList$deleteRows(), silent = TRUE)
      try(table$deleteNote("modz_scale"), silent = TRUE)
      private$.set_variable_plots(self$results$modzPlot, self$options$vars_modz, list())
      
      all_plot_data <- list()
      has_group     <- !is.null(self$options$group_modz) && length(self$options$group_modz) > 0
      fallback_groups <- character()
      unevaluated_groups <- character()
      group_vars    <- self$options$group_modz
      
      thresh_map <- c("t25" = 2.5, "t30" = 3.0, "t35" = 3.5, "t40" = 4.0)
      label_map  <- c("t25" = "2.5", "t30" = "3.0", "t35" = "3.5", "t40" = "4.0")
      
      threshold    <- thresh_map[[self$options$threshold_modz]]
      thresh_label <- label_map[[self$options$threshold_modz]]
      
      treatment_type  <- self$options$treatment_modz
      row_counter     <- 1
      outlier_counter <- 1
      
      all_treated_vectors <- list()
      df_work <- self$data
      df_work$.row_id <- seq_len(nrow(df_work))
      
      for (var in self$options$vars_modz) {
        if (!var %in% names(self$data)) next
        
        tryCatch({
          if (has_group) {
            combined_group <- interaction(lapply(group_vars, function(g) df_work[[g]]), sep = "_", drop = TRUE)
            data_grouped <- split(df_work, combined_group)
          } else {
            data_grouped <- list("All Data" = df_work)
          }
          
          full_treated_data <- private$.to_numeric_vec(self$data[[var]])
          
          for (group_name in names(data_grouped)) {
            data_group    <- data_grouped[[group_name]]
            data_var_full <- private$.to_numeric_vec(data_group[[var]])
            non_na_indices_in_group <- which(!is.na(data_var_full))
            data_var <- data_var_full[non_na_indices_in_group]
            if (length(data_var) == 0) next
            
            original_row_numbers_group <- data_group$.row_id
            med <- stats::median(data_var, na.rm = TRUE)
            mad <- stats::mad(data_var, constant = 1, na.rm = TRUE)
            is_evaluable <- TRUE
            if (is.na(mad) || mad == 0) {
              mean_ad <- mean(abs(data_var - med), na.rm = TRUE)
              is_evaluable <- !is.na(mean_ad) && mean_ad > 0
              if (is_evaluable) {
                scale <- 1.253314 * mean_ad
                modified_z <- (data_var - med) / scale
                lower_threshold <- med - threshold * scale
                upper_threshold <- med + threshold * scale
                fallback_groups <- c(fallback_groups, paste0(var, " [", group_name, "]"))
              } else {
                modified_z <- rep(NA_real_, length(data_var))
                lower_threshold <- NA_real_
                upper_threshold <- NA_real_
                unevaluated_groups <- c(unevaluated_groups, paste0(var, " [", group_name, "]"))
              }
            } else {
              modified_z <- 0.6745 * (data_var - med) / mad
              lower_threshold <- med - (threshold * mad / 0.6745)
              upper_threshold <- med + (threshold * mad / 0.6745)
            }
            
            outliers_indices_in_datavar <- which(abs(modified_z) > threshold)
            outliers_indices_in_group   <- non_na_indices_in_group[outliers_indices_in_datavar]
            
            outlierLevel_full <- rep("Non-Outlier", nrow(data_group))
            outlierLevel_full[outliers_indices_in_group] <- thresh_label
            
            rowIndex_full <- rep(NA_integer_, nrow(data_group))
            rowIndex_full[outliers_indices_in_group] <- as.integer(rownames(data_group)[outliers_indices_in_group])
            
            treated_values <- data_var_full
            modified_n     <- length(non_na_indices_in_group)
            applied_treatment_text <- if (is_evaluable) "None" else "Not applied (no dispersion)"
            
            if (is_evaluable && treatment_type != "none") {
              if (treatment_type == "trim_asym") {
                treated_values[outliers_indices_in_group] <- NA_real_
                modified_n <- modified_n - length(outliers_indices_in_group)
                applied_treatment_text <- "Asymmetric Trimming"
              } else if (treatment_type == "trim_sym") {
                trim_prop      <- self$options$trimPercent_modz / 100
                lower_quantile <- stats::quantile(data_var, trim_prop, na.rm = TRUE)
                upper_quantile <- stats::quantile(data_var, 1 - trim_prop, na.rm = TRUE)
                trim_indices   <- which(!is.na(treated_values) & (treated_values < lower_quantile | treated_values > upper_quantile))
                treated_values[trim_indices] <- NA_real_
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
              numOutliers    = if (is_evaluable) length(outliers_indices_in_datavar) else NA_integer_
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
                    rowNumber   = as.integer(rownames(self$data)[row_num]),
                    modifiedZ   = round(z_score, 4)
                  )
                )
                outlier_counter <- outlier_counter + 1
              }
            }
            
            if (self$options$dispBox_modz && is_evaluable) {
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
          if (treatment_type != "none") all_treated_vectors[[var]] <- full_treated_data
        }, error = function(e) {
          jmvcore::reject(paste0("Error processing variable ", var, ": ", e$message), code = "error_processing_variable")
        })
      }
      
      if (treatment_type != "none" && length(all_treated_vectors) > 0 && self$results$treatedData_modz$isNotFilled()) {
        output_keys   <- names(all_treated_vectors)
        suffix <- if (treatment_type %in% c("trim_asym", "trim_sym")) "_trimmed" else "_winsorized"
        self$results$treatedData_modz$set(
          keys = output_keys,
          titles = paste0(output_keys, suffix),
          descriptions = rep("Treated variable (Modified Z)", length(output_keys)),
          measureTypes = rep("continuous", length(output_keys))
        )
        for (var in output_keys) {
          self$results$treatedData_modz$setValues(key = var, values = all_treated_vectors[[var]])
        }
        self$results$treatedData_modz$setRowNums(rownames(self$data))
      }
      
      scale_notes <- character()
      if (length(fallback_groups) > 0) {
        scale_notes <- c(scale_notes, paste0("MAD = 0: MeanAD fallback used for ", paste(fallback_groups, collapse = "; "), ". Scores use (x - median) / (1.253314 * MeanAD). This fallback is less robust to extreme values than MAD."))
      }
      if (length(unevaluated_groups) > 0) {
        scale_notes <- c(scale_notes, paste0("Scores, thresholds and outlier counts are not calculable for ", paste(unevaluated_groups, collapse = "; "), ": no dispersion. Data are unchanged, no treatment is applied, and these groups are omitted from this method's plot."))
      }
      if (length(scale_notes) > 0) table$setNote("modz_scale", paste(scale_notes, collapse = " "))

      if (self$options$dispBox_modz && length(all_plot_data) > 0) {
        private$.set_variable_plots(self$results$modzPlot, self$options$vars_modz, all_plot_data, list(threshold = threshold, thresh_label = thresh_label))
      }
    },
    
    .runIQR = function() {
      if (is.null(self$options$vars_iqr) || length(self$options$vars_iqr) == 0) return()
      
      private$.set_variable_plots(self$results$iqrPlot, self$options$vars_iqr, list())
      table <- self$results$iqrTable
      if (table$rowCount > 0) try(table$deleteRows(), silent = TRUE)
      try(table$deleteNote("adj_box_note"), silent = TRUE)
      
      all_plot_data <- list()
      has_group     <- !is.null(self$options$group_iqr) && length(self$options$group_iqr) > 0
      group_vars    <- self$options$group_iqr
      use_adj_box   <- isTRUE(self$options$adj_box_iqr)
      
      mult_map  <- c("iqr15" = 1.5, "iqr22" = 2.2, "iqr30" = 3.0)
      label_map <- c("iqr15" = "1.5", "iqr22" = "2.2", "iqr30" = "3.0")
      
      iqr_mult  <- mult_map[[self$options$iqrMultiplier_iqr]]
      iqr_label <- label_map[[self$options$iqrMultiplier_iqr]]
      
      treatment_type <- self$options$treatment_iqr
      row_counter    <- 1
      
      all_treated_vectors <- list()
      df_work <- self$data
      df_work$.row_id <- seq_len(nrow(df_work))
      
      if (use_adj_box) {
        table$setNote("adj_box_note", "Fences adjusted for distribution skewness using an approximate medcouple. Samples above 1,000 observations are reduced to 1,000 ordered values; pairs tied at the median are omitted.")
      }
      
      for (var in self$options$vars_iqr) {
        if (!var %in% names(self$data)) next
        
        tryCatch({
          if (has_group) {
            combined_group <- interaction(lapply(group_vars, function(g) df_work[[g]]), sep = "_", drop = TRUE)
            data_grouped <- split(df_work, combined_group)
          } else {
            data_grouped <- list("All Data" = df_work)
          }
          
          full_treated_data <- private$.to_numeric_vec(self$data[[var]])
          
          for (group_name in names(data_grouped)) {
            data_group    <- data_grouped[[group_name]]
            data_var_full <- private$.to_numeric_vec(data_group[[var]])
            non_na_indices_in_group <- which(!is.na(data_var_full))
            data_var <- data_var_full[non_na_indices_in_group]
            if (length(data_var) == 0) next
            
            original_row_numbers_group <- data_group$.row_id
            
            q1 <- stats::quantile(data_var, 0.25, na.rm = TRUE)
            q3 <- stats::quantile(data_var, 0.75, na.rm = TRUE)
            iqr_value <- q3 - q1
            
            if (use_adj_box) {
              mc <- private$.medcouple(data_var)
              if (mc >= 0) {
                lower_fence <- q1 - (iqr_mult * exp(-4 * mc) * iqr_value)
                upper_fence <- q3 + (iqr_mult * exp(3 * mc) * iqr_value)
              } else {
                lower_fence <- q1 - (iqr_mult * exp(-3 * mc) * iqr_value)
                upper_fence <- q3 + (iqr_mult * exp(4 * mc) * iqr_value)
              }
            } else {
              lower_fence <- q1 - (iqr_mult * iqr_value)
              upper_fence <- q3 + (iqr_mult * iqr_value)
            }
            
            outliers_indices_in_datavar <- which(data_var < lower_fence | data_var > upper_fence)
            outliers_indices_in_group   <- non_na_indices_in_group[outliers_indices_in_datavar]
            
            outlierLevel_full <- rep("Non-Outlier", nrow(data_group))
            outlierLevel_full[outliers_indices_in_group] <- iqr_label
            
            rowIndex_full <- rep(NA_integer_, nrow(data_group))
            rowIndex_full[outliers_indices_in_group] <- as.integer(rownames(data_group)[outliers_indices_in_group])
            
            treated_values <- data_var_full
            modified_n     <- length(non_na_indices_in_group)
            applied_treatment_text <- "None"
            
            if (treatment_type != "none") {
              if (treatment_type == "trim_asym") {
                treated_values[outliers_indices_in_group] <- NA_real_
                modified_n <- modified_n - length(outliers_indices_in_group)
                applied_treatment_text <- "Asymmetric Trimming"
              } else if (treatment_type == "trim_sym") {
                trim_prop      <- self$options$trimPercent_iqr / 100
                lower_quantile <- stats::quantile(data_var, trim_prop, na.rm = TRUE)
                upper_quantile <- stats::quantile(data_var, 1 - trim_prop, na.rm = TRUE)
                trim_indices   <- which(!is.na(treated_values) & (treated_values < lower_quantile | treated_values > upper_quantile))
                treated_values[trim_indices] <- NA_real_
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
          if (treatment_type != "none") all_treated_vectors[[var]] <- full_treated_data
        }, error = function(e) {
          jmvcore::reject(paste0("Error processing variable ", var, ": ", e$message), code = "error_processing_variable")
        })
      }
      
      if (treatment_type != "none" && length(all_treated_vectors) > 0 && self$results$treatedData_iqr$isNotFilled()) {
        output_keys   <- names(all_treated_vectors)
        suffix <- if (treatment_type %in% c("trim_asym", "trim_sym")) "_trimmed" else "_winsorized"
        self$results$treatedData_iqr$set(
          keys = output_keys,
          titles = paste0(output_keys, suffix),
          descriptions = rep("Treated variable (IQR)", length(output_keys)),
          measureTypes = rep("continuous", length(output_keys))
        )
        for (var in output_keys) {
          self$results$treatedData_iqr$setValues(key = var, values = all_treated_vectors[[var]])
        }
        self$results$treatedData_iqr$setRowNums(rownames(self$data))
      }
      
      if (self$options$dispBox_iqr && length(all_plot_data) > 0) {
        private$.set_variable_plots(self$results$iqrPlot, self$options$vars_iqr, all_plot_data, list(iqr_mult = iqr_mult, iqr_label = iqr_label))
      }
    },
    
    .runMahalanobis = function() {
      vars <- self$options$vars_mah
      if (is.null(vars) || length(vars) < 2) return()
      
      table       <- self$results$mahTable
      table$setNote("insufficient_cases", NULL)
      outlierList <- self$results$mahOutlierList
      table$setRow(rowNo = 1, values = list())
      if (outlierList$rowCount > 0) try(outlierList$deleteRows(), silent = TRUE)
      
      method    <- self$options$method_mah
      alpha_opt <- self$options$alpha_mah
      alpha     <- switch(alpha_opt, p001 = 0.001, p01 = 0.01, p05 = 0.05, 0.001)
      
      df_sub <- self$data[vars]
      complete_idx <- which(stats::complete.cases(df_sub))
      if (length(complete_idx) <= length(vars)) {
        table$setNote("insufficient_cases", paste0("Mahalanobis analysis not performed: ", length(complete_idx), " complete cases for ", length(vars), " variables. The number of complete cases must exceed the number of variables."))
        table$setRow(rowNo = 1, values = list(
          method = if (method == "classic") "Classical" else "Robust MCD",
          numVars = length(vars),
          n_analyzed = length(complete_idx),
          critCutoff = NA_real_,
          numOutliers = NA_integer_,
          pctOutliers = NA_real_
        ))
        return()
      }
      
      X <- as.matrix(df_sub[complete_idx, , drop = FALSE])
      n <- nrow(X)
      p <- ncol(X)
      
      d2 <- NULL
      method_label <- "Classical Mahalanobis"
      if (method == "classic") {
        cov_mat <- stats::cov(X)
        center_vec <- colMeans(X)
        inv_cov <- tryCatch(solve(cov_mat), error = function(e) MASS::ginv(cov_mat))
        d2 <- stats::mahalanobis(X, center = center_vec, cov = inv_cov, inverted = TRUE)
      } else if (method == "robust_mcd") {
        method_label <- "Robust MCD (FastMCD)"
        fit_mcd <- tryCatch(MASS::cov.rob(X, method = "mcd"), error = function(e) NULL)
        if (is.null(fit_mcd)) {
          fit_mcd <- MASS::cov.rob(X, method = "classical")
          method_label <- "MCD Fallback (Cov.rob classical)"
        }
        inv_cov <- tryCatch(solve(fit_mcd$cov), error = function(e) MASS::ginv(fit_mcd$cov))
        d2 <- stats::mahalanobis(X, center = fit_mcd$center, cov = inv_cov, inverted = TRUE)
      }
      
      crit_val     <- stats::qchisq(1 - alpha, df = p)
      p_vals       <- 1 - stats::pchisq(d2, df = p)
      outlier_mask <- (d2 > crit_val)
      n_outliers   <- sum(outlier_mask)
      pct_outliers <- (n_outliers / n) * 100
      
      table$setRow(rowNo = 1, values = list(
        method      = method_label,
        numVars     = as.integer(p),
        n_analyzed  = as.integer(n),
        critCutoff  = crit_val,
        numOutliers = as.integer(n_outliers),
        pctOutliers = pct_outliers
      ))
      
      if (n_outliers > 0) {
        out_indices <- which(outlier_mask)
        for (i in seq_along(out_indices)) {
          orig_row <- complete_idx[out_indices[i]]
          outlierList$addRow(rowKey = i, values = list(
            rowNumber = as.integer(rownames(self$data)[orig_row]),
            d_squared = d2[out_indices[i]],
            p_value   = p_vals[out_indices[i]]
          ))
        }
      }
      
      if (isTRUE(self$options$dispPlot_mah)) {
        theo_quant <- stats::qchisq((seq_len(n) - 0.5) / n, df = p)
        emp_quant  <- sort(d2)
        qq_df <- data.frame(theoretical = theo_quant, empirical = emp_quant)
        self$results$mahPlot$setState(list(df = qq_df, crit = crit_val, p = p, method = method_label))
      }
      
      if (self$results$treatedData_mah$isNotFilled()) {
        self$results$treatedData_mah$set(
          keys = "mah_outlier",
          titles = "mah_outlier",
          descriptions = "Multivariate outlier flag (1 = Outlier, 0 = Normal)",
          measureTypes = "nominal"
        )
        flag_vec <- rep(NA_integer_, nrow(self$data))
        flag_vec[complete_idx] <- as.integer(outlier_mask)
        self$results$treatedData_mah$setValues(key = "mah_outlier", values = flag_vec)
        self$results$treatedData_mah$setRowNums(rownames(self$data))
      }
    },
    
    .plotMahalanobis = function(image, ggtheme, theme, ...) {
      state <- image$state
      if (is.null(state) || is.null(state$df)) return(FALSE)
      
      df   <- state$df
      crit <- state$crit
      p_df <- state$p
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = theoretical, y = empirical)) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50", linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = crit, linetype = "dotted", color = "#c0392b", linewidth = 0.9) +
        ggplot2::geom_point(color = "#2c3e50", size = 2.5, alpha = 0.8) +
        ggplot2::labs(
          title = paste0("Chi-Square QQ-Plot (Mahalanobis D\u00b2, df = ", p_df, ")"),
          x = "Theoretical Chi-Square Quantiles",
          y = "Empirical Mahalanobis D\u00b2",
          caption = paste0("Dotted red line: Critical \u03c7\u00b2 cutoff = ", round(crit, 2))
        ) +
        ggtheme +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 12),
          panel.grid.minor = ggplot2::element_blank()
        )
      
      print(p)
      TRUE
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
    
    .plotDixon = function(image, ggtheme, ...) {
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
          ggtheme +
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
          ggtheme +
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
    
    .plotModZ = function(image, ggtheme, ...) {
      if (is.null(image$state)) return(FALSE)
      
      plot_list    <- list()
      threshold    <- image$state$threshold
      thresh_label <- image$state$thresh_label
      group_vars   <- self$options$group_modz
      
      shapes <- c("Non-Outlier" = 16, "2.5" = 16, "3.0" = 17, "3.5" = 15, "4.0" = 18)
      colors <- c("Non-Outlier" = "black", "2.5" = "orange", "3.0" = "red", "3.5" = "blue", "4.0" = "purple")
      
      for (var in unique(vapply(image$state$plot_data, function(rows) as.character(rows$variable[1]), character(1)))) {
        all_data_for_var <- NULL
        for (var_group_name in names(image$state$plot_data)) {
          plotData <- image$state$plot_data[[var_group_name]]
          if (plotData$variable[1] == var) {
            all_data_for_var <- rbind(all_data_for_var, plotData)
          }
        }
        if (is.null(all_data_for_var)) next
        has_group <- "group" %in% names(all_data_for_var) && length(unique(all_data_for_var$group)) > 1
        if (!has_group) all_data_for_var$x_axis_label <- "Data"
        
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
            ggtheme +
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
            ggtheme +
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
    
    .plotIQR = function(image, ggtheme, ...) {
      if (is.null(image$state)) return(FALSE)
      
      plot_list  <- list()
      iqr_label  <- image$state$iqr_label
      group_vars <- self$options$group_iqr
      
      shapes <- c("Non-Outlier" = 16, "1.5" = 16, "2.2" = 15, "3.0" = 17)
      colors <- c("Non-Outlier" = "black", "1.5" = "red", "2.2" = "blue", "3.0" = "purple")
      
      for (var in unique(vapply(image$state$plot_data, function(rows) as.character(rows$variable[1]), character(1)))) {
        all_data_for_var <- NULL
        for (var_group_name in names(image$state$plot_data)) {
          plotData <- image$state$plot_data[[var_group_name]]
          if (plotData$variable[1] == var) {
            all_data_for_var <- rbind(all_data_for_var, plotData)
          }
        }
        if (is.null(all_data_for_var)) next
        has_group <- "group" %in% names(all_data_for_var) && length(unique(all_data_for_var$group)) > 1
        if (!has_group) all_data_for_var$x_axis_label <- "Data"
        
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
            ggtheme +
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
            ggtheme +
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
