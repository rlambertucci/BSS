effectSizeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "effectSizeClass",
  inherit = effectSizeBase,
  private = list(
    .is_defined = function(x) {
      !is.null(x) && length(x) > 0 && all(nzchar(x))
    },
    
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
    
    .init = function() {
      info_html <- paste0(
        '<div style="font-size:0.9em; line-height:1.6; padding:4px;">',
        '<h4 style="margin-bottom:6px;">&#128200; Pairwise Effect Size</h4>',
        '<p style="margin-bottom:8px;">Computes effect sizes and confidence intervals. ',
        'All possible group or time-point combinations are generated <b>automatically</b>.</p>',
        '<ul style="margin-top:0px; padding-left:20px;">',
        '<li><b>Independent Samples:</b> Uses categorical IVs and continuous DVs.</li>',
        '<li><b>Longitudinal:</b> Compare effect sizes across time points (Repeated Measures).</li>',
        '</ul>',
        '<hr style="margin:8px 0;"/>',
        '<p><b>&#9654; Interpretation Benchmarks</b></p>',
        '<ul style="margin-top:0px; padding-left:20px;">',
        '<li><b>Cohen (1988):</b> &lt; 0.2 (Negligible) | &ge; 0.2 (Small) | &ge; 0.5 (Medium) | &ge; 0.8 (Large)</li>',
        '<li><b>Sawilowsky (2009):</b> &lt; 0.01 (Negligible) | 0.2 (Very small) | 0.5 (Small) | 0.8 (Medium) | 1.2 (Large) | 2.0 (Very large) | &ge; 2.0 (Huge)</li>',
        '<li><b>Rank Biserial (Correlation):</b> &lt; 0.1 (Negligible) | &ge; 0.1 (Small) | &ge; 0.3 (Medium) | &ge; 0.5 (Large)</li>',
        '</ul>',
        '</div>'
      )
      self$results$analysisInfo$setContent(info_html)
      
      ame_html <- paste0(
        '<div style="font-size:0.9em; line-height:1.6; padding:8px; background-color:#f4f6f8; border-left:4px solid #2c3e50; margin-bottom:10px;">',
        '<h4 style="margin-top:0; margin-bottom:6px;">&#128161; AME (Average Marginal Effect) Interpretation</h4>',
        '<p style="margin-bottom:0;">The AME represents the discrete difference in the dependent variable\'s original metric between the two groups on the response scale. ',
        'For the <b>Interpretation</b> column, the AME is internally standardized (divided by the standard deviation of the DV) ',
        'so that effect size benchmarks can be applied.</p>',
        '</div>'
      )
      self$results$ameBenchmark$setContent(ame_html)
    },
    
    .run = function() {
      data                 <- self$data
      mode                 <- self$options$mode
      run_analysis         <- self$options$run_analysis
      include_interactions <- self$options$include_interactions
      benchmark_std        <- if (!is.null(self$options$benchmark_standard)) self$options$benchmark_standard else "cohen"
      show_conversions     <- isTRUE(self$options$show_conversions)
      show_es_plot         <- isTRUE(self$options$show_es_plot)
      plot_w               <- if (!is.null(self$options$plot_width)) self$options$plot_width else 650
      plot_h               <- if (!is.null(self$options$plot_height)) self$options$plot_height else 450
      
      table <- self$results$effectSizeTable
      
      if (mode == "cross_sectional") {
        table$setTitle("Pairwise Effect Size Results (Independent Samples)")
      } else {
        table$setTitle("Within-Subjects Effect Sizes (Time Effects)")
      }
      table$setNote("engine", "Note. Computations powered by the 'effectsize' R package. Variance is pooled locally between the compared pairs, not globally across the entire model.")
      
      if (mode == "cross_sectional") {
        dep_vars            <- self$options$dep_vars_cs
        indep_vars          <- self$options$indep_vars_cs
        es_method           <- self$options$es_method
        show_cles           <- self$options$show_cles
        cov_var             <- self$options$cov
        
        long_format    <- "wide"
        time_var       <- NULL
        wave_vars      <- NULL
        subject_id_var <- NULL
        es_method_long <- "cohens_dz"
      } else {
        dep_vars            <- self$options$dep_vars_long
        indep_vars          <- self$options$indep_vars_long
        es_method_long      <- self$options$es_method_long
        show_cles_long      <- self$options$show_cles_long
        cov_var             <- self$options$cov_long
        
        long_format    <- self$options$long_format
        time_var       <- self$options$time_var
        wave_vars      <- self$options$wave_vars
        subject_id_var <- self$options$subject_id_var
        
        es_method <- es_method_long
        show_cles <- show_cles_long
      }
      
      reference_group      <- self$options$reference_group
      reference_group_long <- self$options$reference_group_long
      use_glm              <- self$options$use_glm
      es_glm_dist          <- self$options$es_glm
      ci_method            <- self$options$ci_method
      n_bootstrap          <- self$options$n_bootstrap
      ci_level_opt         <- self$options$ci_level
      
      has_cov <- private$.is_defined(cov_var)
      
      if (is.null(data)) return()
      
      if (!isTRUE(run_analysis)) {
        self$results$text$setContent("Check 'Run Analysis' to compute effect sizes.")
        self$results$text$setVisible(TRUE)
        return()
      }
      if (!requireNamespace("effectsize", quietly = TRUE)) {
        self$results$text$setContent("Error: The 'effectsize' package is not installed. Please add it to your DESCRIPTION file.")
        self$results$text$setVisible(TRUE)
        return()
      }
      if (has_cov && !requireNamespace("emmeans", quietly = TRUE)) {
        self$results$text$setContent("Error: The 'emmeans' package is required for covariate-adjusted calculations. Please install it.")
        self$results$text$setVisible(TRUE)
        return()
      }
      if (mode == "cross_sectional") {
        if (!private$.is_defined(dep_vars) || !private$.is_defined(indep_vars)) {
          self$results$text$setContent("Cross-sectional analysis requires at least one Dependent Variable (continuous) and one Independent Variable (categorical).")
          self$results$text$setVisible(TRUE)
          return()
        }
      } else {
        if (long_format == "wide") {
          if (!private$.is_defined(wave_vars) || length(wave_vars) < 2) {
            self$results$text$setContent("Wide format selected. Please assign at least 2 Wave Columns.")
            self$results$text$setVisible(TRUE)
            return()
          }
        } else if (long_format == "long") {
          if (!private$.is_defined(dep_vars) || !private$.is_defined(time_var)) {
            self$results$text$setContent("Long format selected. Please assign a Dependent Variable and a Time/Wave Variable.")
            self$results$text$setVisible(TRUE)
            return()
          }
        }
      }
      
      ci_level <- switch(ci_level_opt, ci_90 = 0.90, ci_95 = 0.95, ci_99 = 0.99, 0.95)
      
      table_long_between <- self$results$effectSizeTableLongBetween
      clesTable          <- self$results$clesTable
      clesLongTable      <- self$results$clesLongTable
      conversionTable    <- self$results$conversionTable
      
      if (table$rowCount > 0) try(table$deleteRows(), silent = TRUE)
      if (table_long_between$rowCount > 0) try(table_long_between$deleteRows(), silent = TRUE)
      if (clesTable$rowCount > 0) try(clesTable$deleteRows(), silent = TRUE)
      if (clesLongTable$rowCount > 0) try(clesLongTable$deleteRows(), silent = TRUE)
      if (conversionTable$rowCount > 0) try(conversionTable$deleteRows(), silent = TRUE)
      
      try(table$deleteNote("covariate_note"), silent = TRUE)
      try(table_long_between$deleteNote("covariate_note"), silent = TRUE)
      if (has_cov) {
        table$setNote("covariate_note", "Note. Adjusted statistics (Estimated Marginal Means) are computed controlling for the covariate using the 'emmeans' R package (Lenth, 2024).")
        table_long_between$setNote("covariate_note", "Note. Adjusted statistics (Estimated Marginal Means) are computed controlling for the covariate using the 'emmeans' R package (Lenth, 2024).")
      }
      
      n_rows <- 0
      n_cles <- 0
      n_rows_long_between <- 0
      n_cles_long <- 0
      n_conv <- 0
      
      # Tracking for Forest Plot
      plot_entries <- list()
      
      grouping_factors <- list()
      if (private$.is_defined(indep_vars)) {
        for (iv in indep_vars) {
          grouping_factors[[iv]] <- factor(data[[iv]])
        }
        if (isTRUE(include_interactions) && length(indep_vars) > 1) {
          int_name <- paste(indep_vars, collapse = " * ")
          grouping_factors[[int_name]] <- interaction(data[indep_vars], drop = TRUE, sep = " * ")
        }
      }
      
      if (mode == "longitudinal" && length(grouping_factors) == 0) {
        table_long_between$setVisible(FALSE)
      } else if (mode == "longitudinal") {
        table_long_between$setNote("engine", "Note. Computations powered by the 'effectsize' R package. Variance is pooled locally between the compared pairs.")
      }
      
      if (mode == "cross_sectional") {
        cov_col_full <- if (has_cov) private$.to_numeric_vec(data[[cov_var]]) else NULL
        for (iv_name in names(grouping_factors)) {
          iv_col_raw <- grouping_factors[[iv_name]]
          for (dv in dep_vars) {
            if (iv_name == dv) next
            dv_col_raw <- private$.to_numeric_vec(data[[dv]])
            if (is.null(dv_col_raw) || all(is.na(dv_col_raw))) next
            
            valid <- !is.na(iv_col_raw) & !is.na(dv_col_raw)
            if (has_cov) {
              valid <- valid & !is.na(cov_col_full)
            }
            iv_col  <- droplevels(iv_col_raw[valid])
            dv_col  <- dv_col_raw[valid]
            cov_col <- if (has_cov) cov_col_full[valid] else NULL
            n       <- sum(valid)
            if (n < 3) next
            
            groups      <- levels(iv_col)
            if (length(groups) < 2) next
            group_pairs <- combn(groups, 2, simplify = FALSE)
            groups_note <- paste0("[Available groups: ", paste(groups, collapse = ", "), "]")
            
            for (gp in group_pairs) {
              ga    <- dv_col[iv_col == gp[[1]]]
              gb    <- dv_col[iv_col == gp[[2]]]
              n_obs <- length(ga) + length(gb)
              if (length(ga) < 2 || length(gb) < 2) next
              cov_a <- if (has_cov) cov_col[iv_col == gp[[1]]] else NULL
              cov_b <- if (has_cov) cov_col[iv_col == gp[[2]]] else NULL
              
              if (isTRUE(use_glm)) {
                result <- tryCatch({
                  private$.compute_glm(iv_col, dv_col, gp[[1]], gp[[2]], es_glm_dist, ci_method, ci_level, n_bootstrap, cov_col, benchmark = benchmark_std)
                }, error = function(e) {
                  list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = "AME (error)", interpretation = paste0("GLM error: ", e$message))
                })
              } else {
                ref_grp <- if (es_method == "glass_delta") {
                  rg <- trimws(reference_group)
                  if (nchar(rg) > 0 && rg %in% c(gp[[1]], gp[[2]])) rg else gp[[2]]
                } else gp[[2]]
                result <- tryCatch({
                  private$.compute_2groups(ga, gb, gp[[1]], gp[[2]], ref_grp, es_method, ci_method, ci_level, n_bootstrap, cov_a, cov_b, benchmark = benchmark_std)
                }, error = function(e) {
                  list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = es_method, interpretation = paste0("Computation error: ", e$message))
                })
              }
              
              interp_note <- result$interpretation
              if (es_method == "glass_delta" && !isTRUE(use_glm)) interp_note <- paste0(interp_note, " ", groups_note)
              n_rows <- n_rows + 1
              comp_str <- paste0(gp[[1]], " vs ", gp[[2]])
              table$addRow(rowKey = n_rows, values = list(
                dv             = dv,
                iv             = iv_name,
                comparison     = comp_str,
                method         = result$method,
                es_value       = result$es_value,
                ci_lower       = result$ci_lower,
                ci_upper       = result$ci_upper,
                interpretation = interp_note,
                n_obs          = n_obs
              ))
              
              # Conversions
              if (show_conversions && is.finite(result$es_value)) {
                conv_res <- private$.convert_metrics(result$es_value, result$method, benchmark_std)
                n_conv <- n_conv + 1
                conversionTable$addRow(rowKey = n_conv, values = list(
                  dv             = paste0(dv, " (", iv_name, ")"),
                  comparison     = comp_str,
                  cohen_d        = conv_res$cohen_d,
                  correlation_r  = conv_res$correlation_r,
                  eta_squared    = conv_res$eta_squared,
                  odds_ratio     = conv_res$odds_ratio,
                  auc            = conv_res$auc,
                  interpretation = conv_res$interpretation
                ))
              }
              
              # Forest plot entry
              if (show_es_plot && is.finite(result$es_value)) {
                plot_entries[[length(plot_entries) + 1]] <- list(
                  label    = paste0(dv, " [", iv_name, ": ", comp_str, "]"),
                  estimate = result$es_value,
                  ci_lower = result$ci_lower,
                  ci_upper = result$ci_upper,
                  method   = result$method
                )
              }
              
              if (isTRUE(show_cles)) {
                cles_result <- private$.compute_cles_independent(ga, gb)
                n_cles <- n_cles + 1
                clesTable$addRow(
                  rowKey = n_cles,
                  values = list(
                    dv         = dv,
                    iv         = iv_name,
                    comparison = comp_str,
                    cles       = cles_result$cles
                  )
                )
              }
            }
          }
        }
      }
      
      if (mode == "longitudinal") {
        subsets <- list(list(label = "Overall Sample", mask = rep(TRUE, nrow(data))))
        if (length(grouping_factors) > 0) {
          for (iv_name in names(grouping_factors)) {
            grp_col <- grouping_factors[[iv_name]]
            valid_levels <- levels(droplevels(grp_col[!is.na(grp_col)]))
            for (lvl in valid_levels) {
              sub_label <- paste0(iv_name, " [", lvl, "]")
              subsets[[sub_label]] <- list(
                label = sub_label,
                mask  = (!is.na(grp_col) & grp_col == lvl)
              )
            }
          }
        }
        
        if (long_format == "wide") {
          wave_pairs <- combn(wave_vars, 2, simplify = FALSE)
          for (pair in wave_pairs) {
            w1   <- pair[[1]]
            w2   <- pair[[2]]
            col1_full <- private$.to_numeric_vec(data[[w1]])
            col2_full <- private$.to_numeric_vec(data[[w2]])
            cov_col_full <- if (has_cov) private$.to_numeric_vec(data[[cov_var]]) else NULL
            if (is.null(col1_full) || is.null(col2_full)) next
            
            for (sub in subsets) {
              mask <- sub$mask
              col1 <- col1_full[mask]
              col2 <- col2_full[mask]
              cov_col <- if (has_cov) cov_col_full[mask] else NULL
              valid  <- !is.na(col1) & !is.na(col2)
              if (has_cov) {
                valid <- valid & !is.na(cov_col)
              }
              col1    <- col1[valid]
              col2    <- col2[valid]
              cov_col <- if (has_cov) cov_col[valid] else NULL
              n_pair  <- sum(valid)
              if (n_pair < 3) next
              
              ref_wave <- if (es_method_long == "glass_dz") {
                rg <- trimws(reference_group_long)
                if (nchar(rg) > 0 && rg %in% c(w1, w2)) rg else w1
              } else w1
              
              result <- tryCatch({
                private$.compute_dependent_paired(col1, col2, w1, w2, ref_wave, es_method_long, ci_method, ci_level, n_bootstrap, cov_col, benchmark = benchmark_std)
              }, error = function(e) {
                list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = es_method_long, interpretation = paste0("Paired error: ", e$message))
              })
              
              n_rows <- n_rows + 1
              comp_str <- paste0(w1, " vs ", w2)
              table$addRow(rowKey = n_rows, values = list(
                dv             = comp_str,
                iv             = sub$label,
                comparison     = comp_str,
                method         = result$method,
                es_value       = result$es_value,
                ci_lower       = result$ci_lower,
                ci_upper       = result$ci_upper,
                interpretation = result$interpretation,
                n_obs          = n_pair
              ))
              
              if (show_conversions && is.finite(result$es_value)) {
                conv_res <- private$.convert_metrics(result$es_value, result$method, benchmark_std)
                n_conv <- n_conv + 1
                conversionTable$addRow(rowKey = n_conv, values = list(
                  dv             = sub$label,
                  comparison     = comp_str,
                  cohen_d        = conv_res$cohen_d,
                  correlation_r  = conv_res$correlation_r,
                  eta_squared    = conv_res$eta_squared,
                  odds_ratio     = conv_res$odds_ratio,
                  auc            = conv_res$auc,
                  interpretation = conv_res$interpretation
                ))
              }
              
              if (show_es_plot && is.finite(result$es_value)) {
                plot_entries[[length(plot_entries) + 1]] <- list(
                  label    = paste0(comp_str, " (", sub$label, ")"),
                  estimate = result$es_value,
                  ci_lower = result$ci_lower,
                  ci_upper = result$ci_upper,
                  method   = result$method
                )
              }
              
              if (isTRUE(show_cles_long)) {
                cles_result <- private$.compute_cles_paired(col1, col2)
                n_cles_long <- n_cles_long + 1
                clesLongTable$addRow(
                  rowKey = n_cles_long,
                  values = list(
                    dv         = sub$label,
                    comparison = comp_str,
                    cles       = cles_result$cles
                  )
                )
              }
            }
          }
          
          if (length(grouping_factors) > 0) {
            for (wave in wave_vars) {
              dv_col <- private$.to_numeric_vec(data[[wave]])
              if (is.null(dv_col) || all(is.na(dv_col))) next
              for (iv_name in names(grouping_factors)) {
                iv_col_raw <- grouping_factors[[iv_name]]
                cov_col_raw <- if (has_cov) private$.to_numeric_vec(data[[cov_var]]) else NULL
                valid <- !is.na(iv_col_raw) & !is.na(dv_col)
                if (has_cov) {
                  valid <- valid & !is.na(cov_col_raw)
                }
                iv_col     <- droplevels(iv_col_raw[valid])
                dv_col_sub <- dv_col[valid]
                cov_col    <- if (has_cov) cov_col_raw[valid] else NULL
                groups     <- levels(iv_col)
                if (length(groups) < 2) next
                group_pairs <- combn(groups, 2, simplify = FALSE)
                for (gp in group_pairs) {
                  ga <- dv_col_sub[iv_col == gp[[1]]]
                  gb <- dv_col_sub[iv_col == gp[[2]]]
                  n_obs <- length(ga) + length(gb)
                  if (length(ga) < 2 || length(gb) < 2) next
                  cov_a <- if (has_cov) cov_col[iv_col == gp[[1]]] else NULL
                  cov_b <- if (has_cov) cov_col[iv_col == gp[[2]]] else NULL
                  method_to_use <- switch(es_method,
                                          cohens_dz = "cohens_d",
                                          hedges_gz = "hedges_g",
                                          glass_dz  = "glass_delta",
                                          rank_biserial_paired = "rank_biserial",
                                          es_method)
                  result <- tryCatch({
                    private$.compute_2groups(ga, gb, gp[[1]], gp[[2]], gp[[2]], method_to_use, ci_method, ci_level, n_bootstrap, cov_a, cov_b, benchmark = benchmark_std)
                  }, error = function(e) {
                    list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method_to_use, interpretation = paste0("Error: ", e$message))
                  })
                  n_rows_long_between <- n_rows_long_between + 1
                  comp_str <- paste0(gp[[1]], " vs ", gp[[2]])
                  table_long_between$addRow(rowKey = n_rows_long_between, values = list(
                    dv             = wave,
                    iv             = iv_name,
                    comparison     = comp_str,
                    method         = result$method,
                    es_value       = result$es_value,
                    ci_lower       = result$ci_lower,
                    ci_upper       = result$ci_upper,
                    interpretation = result$interpretation,
                    n_obs          = n_obs
                  ))
                }
              }
            }
          }
        } else if (long_format == "long") {
          time_col    <- factor(data[[time_var]])
          time_levels <- levels(time_col)
          if (length(time_levels) < 2) {
            self$results$text$setContent(paste0("Time variable '", time_var, "' must have at least 2 distinct levels."))
            self$results$text$setVisible(TRUE)
            return()
          }
          for (dv in dep_vars) {
            dv_col_full <- private$.to_numeric_vec(data[[dv]])
            cov_col_full <- if (has_cov) private$.to_numeric_vec(data[[cov_var]]) else NULL
            if (is.null(dv_col_full) || all(is.na(dv_col_full))) next
            time_pairs <- combn(time_levels, 2, simplify = FALSE)
            for (pair in time_pairs) {
              t1 <- pair[[1]]
              t2 <- pair[[2]]
              for (sub in subsets) {
                mask <- sub$mask
                if (private$.is_defined(subject_id_var)) {
                  id_col <- data[[subject_id_var]]
                  if (has_cov) {
                    df_all <- data.frame(id = id_col, val = dv_col_full, time = time_col, mask = mask, cov = cov_col_full)
                  } else {
                    df_all <- data.frame(id = id_col, val = dv_col_full, time = time_col, mask = mask)
                  }
                  df_t1  <- df_all[df_all$time == t1 & df_all$mask == TRUE & !is.na(df_all$val), ]
                  df_t2  <- df_all[df_all$time == t2 & df_all$mask == TRUE & !is.na(df_all$val), ]
                  if (has_cov) {
                    df_t1 <- df_t1[!is.na(df_t1$cov), ]
                    df_t2 <- df_t2[!is.na(df_t2$cov), ]
                  }
                  
                  # Deduplicate subject IDs if multiple measurements exist per wave
                  if (anyDuplicated(df_t1$id)) {
                    if (has_cov) {
                      df_t1 <- stats::aggregate(cbind(val, cov) ~ id, data = df_t1, FUN = mean)
                    } else {
                      df_t1 <- stats::aggregate(val ~ id, data = df_t1, FUN = mean)
                    }
                  }
                  if (anyDuplicated(df_t2$id)) {
                    if (has_cov) {
                      df_t2 <- stats::aggregate(cbind(val, cov) ~ id, data = df_t2, FUN = mean)
                    } else {
                      df_t2 <- stats::aggregate(val ~ id, data = df_t2, FUN = mean)
                    }
                  }
                  
                  matched <- merge(df_t1, df_t2, by = "id")
                  col1   <- matched$val.x
                  col2   <- matched$val.y
                  cov_col <- if (has_cov) {
                    (matched$cov.x + matched$cov.y) / 2
                  } else NULL
                  n_pair <- nrow(matched)
                  if (n_pair < 3) next
                  ref_wave <- if (es_method_long == "glass_dz") {
                    rg <- trimws(reference_group_long)
                    if (nchar(rg) > 0 && rg %in% c(t1, t2)) rg else t1
                  } else t1
                  result <- tryCatch({
                    private$.compute_dependent_paired(col1, col2, t1, t2, ref_wave, es_method_long, ci_method, ci_level, n_bootstrap, cov_col, benchmark = benchmark_std)
                  }, error = function(e) {
                    list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = es_method_long, interpretation = paste0("Paired error: ", e$message))
                  })
                  n_rows <- n_rows + 1
                  comp_str <- paste0(t1, " vs ", t2)
                  table$addRow(rowKey = n_rows, values = list(
                    dv             = dv,
                    iv             = sub$label,
                    comparison     = comp_str,
                    method         = result$method,
                    es_value       = result$es_value,
                    ci_lower       = result$ci_lower,
                    ci_upper       = result$ci_upper,
                    interpretation = result$interpretation,
                    n_obs          = n_pair
                  ))
                  
                  if (show_conversions && is.finite(result$es_value)) {
                    conv_res <- private$.convert_metrics(result$es_value, result$method, benchmark_std)
                    n_conv <- n_conv + 1
                    conversionTable$addRow(rowKey = n_conv, values = list(
                      dv             = paste0(dv, " (", sub$label, ")"),
                      comparison     = comp_str,
                      cohen_d        = conv_res$cohen_d,
                      correlation_r  = conv_res$correlation_r,
                      eta_squared    = conv_res$eta_squared,
                      odds_ratio     = conv_res$odds_ratio,
                      auc            = conv_res$auc,
                      interpretation = conv_res$interpretation
                    ))
                  }
                  
                  if (show_es_plot && is.finite(result$es_value)) {
                    plot_entries[[length(plot_entries) + 1]] <- list(
                      label    = paste0(dv, " [", sub$label, ": ", comp_str, "]"),
                      estimate = result$es_value,
                      ci_lower = result$ci_lower,
                      ci_upper = result$ci_upper,
                      method   = result$method
                    )
                  }
                  
                  if (isTRUE(show_cles_long)) {
                    cles_result <- private$.compute_cles_paired(col1, col2)
                    n_cles_long <- n_cles_long + 1
                    clesLongTable$addRow(
                      rowKey = n_cles_long,
                      values = list(
                        dv         = if (sub$label == "Overall Sample") dv else paste0(dv, " (", sub$label, ")"),
                        comparison = comp_str,
                        cles       = cles_result$cles
                      )
                    )
                  }
                } else {
                  g1 <- dv_col_full[time_col == t1 & mask == TRUE & !is.na(dv_col_full)]
                  g2 <- dv_col_full[time_col == t2 & mask == TRUE & !is.na(dv_col_full)]
                  cov_g1 <- if (has_cov) cov_col_full[time_col == t1 & mask == TRUE & !is.na(dv_col_full)] else NULL
                  cov_g2 <- if (has_cov) cov_col_full[time_col == t2 & mask == TRUE & !is.na(dv_col_full)] else NULL
                  if (has_cov) {
                    valid_g1 <- !is.na(cov_g1)
                    g1 <- g1[valid_g1]
                    cov_g1 <- cov_g1[valid_g1]
                    valid_g2 <- !is.na(cov_g2)
                    g2 <- g2[valid_g2]
                    cov_g2 <- cov_g2[valid_g2]
                  }
                  n_pair <- length(g1) + length(g2)
                  if (length(g1) < 2 || length(g2) < 2) next
                  method_to_use_unpaired <- switch(es_method_long,
                                                   cohens_dz = "cohens_d",
                                                   hedges_gz = "hedges_g",
                                                   glass_dz  = "glass_delta",
                                                   rank_biserial_paired = "rank_biserial",
                                                   es_method_long)
                  result <- tryCatch({
                    private$.compute_2groups(g1, g2, t1, t2, t1, method_to_use_unpaired, ci_method, ci_level, n_bootstrap, cov_g1, cov_g2, benchmark = benchmark_std)
                  }, error = function(e) {
                    list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method_to_use_unpaired, interpretation = paste0("Error: ", e$message))
                  })
                  n_rows <- n_rows + 1
                  comp_str <- paste0(t1, " vs ", t2, " (unpaired)")
                  table$addRow(rowKey = n_rows, values = list(
                    dv             = dv,
                    iv             = sub$label,
                    comparison     = comp_str,
                    method         = result$method,
                    es_value       = result$es_value,
                    ci_lower       = result$ci_lower,
                    ci_upper       = result$ci_upper,
                    interpretation = result$interpretation,
                    n_obs          = n_pair
                  ))
                  
                  if (show_conversions && is.finite(result$es_value)) {
                    conv_res <- private$.convert_metrics(result$es_value, result$method, benchmark_std)
                    n_conv <- n_conv + 1
                    conversionTable$addRow(rowKey = n_conv, values = list(
                      dv             = paste0(dv, " (", sub$label, ")"),
                      comparison     = comp_str,
                      cohen_d        = conv_res$cohen_d,
                      correlation_r  = conv_res$correlation_r,
                      eta_squared    = conv_res$eta_squared,
                      odds_ratio     = conv_res$odds_ratio,
                      auc            = conv_res$auc,
                      interpretation = conv_res$interpretation
                    ))
                  }
                  
                  if (show_es_plot && is.finite(result$es_value)) {
                    plot_entries[[length(plot_entries) + 1]] <- list(
                      label    = paste0(dv, " [", sub$label, ": ", comp_str, "]"),
                      estimate = result$es_value,
                      ci_lower = result$ci_lower,
                      ci_upper = result$ci_upper,
                      method   = result$method
                    )
                  }
                  
                  if (isTRUE(show_cles_long)) {
                    cles_result <- private$.compute_cles_independent(g1, g2)
                    n_cles <- n_cles + 1
                    clesTable$addRow(
                      rowKey = n_cles,
                      values = list(
                        dv         = dv,
                        iv         = sub$label,
                        comparison = comp_str,
                        cles       = cles_result$cles
                      )
                    )
                  }
                }
              }
            }
            if (length(grouping_factors) > 0) {
              for (t1 in time_levels) {
                time_mask <- (time_col == t1)
                for (iv_name in names(grouping_factors)) {
                  iv_col_raw <- grouping_factors[[iv_name]]
                  cov_col_raw <- if (has_cov) cov_col_full else NULL
                  valid  <- !is.na(iv_col_raw) & !is.na(dv_col_full) & time_mask
                  if (has_cov) {
                    valid <- valid & !is.na(cov_col_raw)
                  }
                  iv_col     <- droplevels(iv_col_raw[valid])
                  dv_col_sub <- dv_col_full[valid]
                  cov_col    <- if (has_cov) cov_col_raw[valid] else NULL
                  groups     <- levels(iv_col)
                  if (length(groups) < 2) next
                  group_pairs <- combn(groups, 2, simplify = FALSE)
                  for (gp in group_pairs) {
                    ga    <- dv_col_sub[iv_col == gp[[1]]]
                    gb    <- dv_col_sub[iv_col == gp[[2]]]
                    n_obs <- length(ga) + length(gb)
                    if (length(ga) < 2 || length(gb) < 2) next
                    cov_a <- if (has_cov) cov_col[iv_col == gp[[1]]] else NULL
                    cov_b <- if (has_cov) cov_col[iv_col == gp[[2]]] else NULL
                    method_to_use <- switch(es_method,
                                            cohens_dz = "cohens_d",
                                            hedges_gz = "hedges_g",
                                            glass_dz  = "glass_delta",
                                            rank_biserial_paired = "rank_biserial",
                                            es_method)
                    result <- tryCatch({
                      private$.compute_2groups(ga, gb, gp[[1]], gp[[2]], gp[[2]], method_to_use, ci_method, ci_level, n_bootstrap, cov_a, cov_b, benchmark = benchmark_std)
                    }, error = function(e) {
                      list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method_to_use, interpretation = paste0("Error: ", e$message))
                    })
                    n_rows_long_between <- n_rows_long_between + 1
                    comp_str <- paste0(gp[[1]], " vs ", gp[[2]])
                    table_long_between$addRow(rowKey = n_rows_long_between, values = list(
                      dv             = paste0(dv, " (At Time: ", t1, ")"),
                      iv             = iv_name,
                      comparison     = comp_str,
                      method         = result$method,
                      es_value       = result$es_value,
                      ci_lower       = result$ci_lower,
                      ci_upper       = result$ci_upper,
                      interpretation = result$interpretation,
                      n_obs          = n_obs
                    ))
                  }
                }
              }
            }
          }
        }
      }
      
      # Forest Plot Setup
      if (show_es_plot) {
        image <- self$results$esPlot
        image$setSize(plot_w, plot_h)
        if (length(plot_entries) > 0) {
          plot_df <- do.call(rbind, lapply(plot_entries, as.data.frame, stringsAsFactors = FALSE))
          image$setState(plot_df)
        } else {
          image$setState(NULL)
        }
      }
      
      if (n_rows == 0) {
        self$results$text$setContent("No valid pairs found. Check variable assignments, types, and missing data.")
      } else {
        self$results$text$setContent(paste0(
          (n_rows + n_rows_long_between), " effect size(s) computed successfully."
        ))
      }
      self$results$text$setVisible(TRUE)
    },
    
    .interpret = function(value, method, benchmark = "cohen") {
      if (is.na(value) || !is.finite(value)) return("—")
      abs_val <- abs(value)
      
      # Rank biserial always uses correlation thresholds
      if (method %in% c("rank_biserial", "rank_biserial_paired")) {
        if (abs_val < 0.1) return("Negligible")
        if (abs_val < 0.3) return("Small")
        if (abs_val < 0.5) return("Medium")
        return("Large")
      }
      
      # Standard Cohen (1988)
      if (benchmark == "cohen") {
        if (abs_val < 0.2) return("Negligible")
        if (abs_val < 0.5) return("Small")
        if (abs_val < 0.8) return("Medium")
        return("Large")
      }
      
      # Sawilowsky (2009) expanded benchmark
      if (benchmark == "sawilowsky") {
        if (abs_val < 0.01) return("Negligible (< 0.01)")
        if (abs_val < 0.20) return("Very small (0.01 - 0.2)")
        if (abs_val < 0.50) return("Small (0.2 - 0.5)")
        if (abs_val < 0.80) return("Medium (0.5 - 0.8)")
        if (abs_val < 1.20) return("Large (0.8 - 1.2)")
        if (abs_val < 2.00) return("Very large (1.2 - 2.0)")
        return("Huge (≥ 2.0)")
      }
      
      if (abs_val < 0.2) return("Negligible")
      if (abs_val < 0.5) return("Small")
      if (abs_val < 0.8) return("Medium")
      return("Large")
    },
    
    .convert_metrics = function(es_val, method, benchmark = "cohen") {
      # Determine Cohen's d equivalent
      d <- if (method %in% c("rank_biserial", "rank_biserial_paired")) {
        r_clamped <- max(min(es_val, 0.999), -0.999)
        (2 * r_clamped) / sqrt(1 - r_clamped^2)
      } else {
        es_val
      }
      
      # Asymptotic conversions (Borenstein et al., 2009; Rosenthal, 1994)
      r <- d / sqrt(d^2 + 4)
      eta_sq <- r^2
      or_val <- exp(1.813799 * d)
      auc_val <- stats::pnorm(d / sqrt(2))
      interp <- private$.interpret(d, "cohens_d", benchmark)
      
      list(
        cohen_d       = d,
        correlation_r = r,
        eta_squared   = eta_sq,
        odds_ratio    = or_val,
        auc           = auc_val,
        interpretation= interp
      )
    },
    
    .compute_cles_independent = function(ga, gb) {
      ps <- try(effectsize::p_superiority(ga, gb, parametric = FALSE), silent = TRUE)
      if (inherits(ps, "try-error")) return(list(cles = NA_real_))
      p <- as.numeric(ps$p_superiority)
      list(cles = p)
    },
    
    .compute_cles_paired = function(col1, col2) {
      diff_vec <- col1 - col2
      valid_diff <- na.omit(diff_vec)
      if (length(valid_diff) == 0) return(list(cles = NA_real_))
      p <- mean(diff_vec > 0) + 0.5 * mean(diff_vec == 0)
      list(cles = p)
    },
    
    .compute_2groups = function(ga, gb, name_a, name_b, ref_grp, method, ci_method, ci_level, n_bootstrap, cov_a = NULL, cov_b = NULL, benchmark = "cohen") {
      es_val <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
      has_cov <- !is.null(cov_a) && !is.null(cov_b)
      n1 <- length(ga)
      n2 <- length(gb)
      s1 <- sd(ga, na.rm = TRUE)
      s2 <- sd(gb, na.rm = TRUE)
      
      if (is.na(s1) || is.na(s2) || (s1 == 0 && s2 == 0)) {
        return(list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method, interpretation = "Zero variance"))
      }
      
      df_pair <- data.frame(
        y = c(ga, gb),
        group = factor(c(rep(name_a, n1), rep(name_b, n2)))
      )
      
      if (has_cov) {
        raw_sd_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
        if (is.na(raw_sd_pooled) || raw_sd_pooled == 0) {
          return(list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method, interpretation = "Zero variance"))
        }
        df_cov <- data.frame(
          y = c(ga, gb),
          group = factor(c(rep(name_a, n1), rep(name_b, n2))),
          cov = c(cov_a, cov_b)
        )
        
        # Calculate sample point estimate with covariate
        if (method == "rank_biserial") {
          res_fit <- lm(y ~ cov, data = df_cov)
          resids <- residuals(res_fit)
          res_a <- resids[df_cov$group == name_a]
          res_b <- resids[df_cov$group == name_b]
          rb_res <- tryCatch({
            effectsize::rank_biserial(res_a, res_b, ci = ci_level)
          }, error = function(e) NULL)
          if (!is.null(rb_res)) {
            es_val <- as.numeric(rb_res[[1]])
            if (ci_method == "analytical") {
              ci_lo <- as.numeric(rb_res$CI_low)
              ci_hi <- as.numeric(rb_res$CI_high)
            }
          }
        } else {
          fit <- lm(y ~ group + cov, data = df_cov)
          emm <- emmeans::emmeans(fit, "group")
          emm_df <- as.data.frame(emm)
          emm_a <- emm_df$emmean[emm_df$group == name_a]
          emm_b <- emm_df$emmean[emm_df$group == name_b]
          if (length(emm_a) > 0 && length(emm_b) > 0) {
            adj_diff <- emm_a - emm_b
            contr_df <- as.data.frame(pairs(emm))
            se_diff <- contr_df$SE[1]
            alpha <- 1 - ci_level
            df_res <- df.residual(fit)
            t_crit <- qt(1 - alpha / 2, df = df_res)
            ci_lo_diff <- adj_diff - t_crit * se_diff
            ci_hi_diff <- adj_diff + t_crit * se_diff
            
            if (method == "cohens_d") {
              es_val <- adj_diff / raw_sd_pooled
              if (ci_method == "analytical") {
                ci_lo <- ci_lo_diff / raw_sd_pooled
                ci_hi <- ci_hi_diff / raw_sd_pooled
              }
            } else if (method == "hedges_g") {
              df_tot <- n1 + n2 - 2
              correction <- if (df_tot > 1) 1 - (3 / (4 * df_tot - 1)) else 1
              es_val <- (adj_diff / raw_sd_pooled) * correction
              if (ci_method == "analytical") {
                ci_lo <- (ci_lo_diff / raw_sd_pooled) * correction
                ci_hi <- (ci_hi_diff / raw_sd_pooled) * correction
              }
            } else if (method == "glass_delta") {
              s_ref <- if (ref_grp == name_a) s1 else s2
              if (is.na(s_ref) || s_ref == 0) return(list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method, interpretation = "Zero variance in reference group"))
              diff_glass <- if (ref_grp == name_a) -adj_diff else adj_diff
              lo_glass   <- if (ref_grp == name_a) -ci_hi_diff else ci_lo_diff
              hi_glass   <- if (ref_grp == name_a) -ci_lo_diff else ci_hi_diff
              df_ref <- if (ref_grp == name_a) n1 - 1 else n2 - 1
              corr_glass <- if (df_ref > 1) 1 - (3 / (4 * df_ref - 1)) else 1
              es_val <- (diff_glass / s_ref) * corr_glass
              if (ci_method == "analytical") {
                ci_lo <- (lo_glass / s_ref) * corr_glass
                ci_hi <- (hi_glass / s_ref) * corr_glass
              }
            }
          }
        }
        
        # Bootstrap CI with covariate
        if (ci_method == "bootstrap") {
          calc_adj_es <- function(b_ga, b_gb, b_cov_a, b_cov_b) {
            b_n1 <- length(b_ga); b_n2 <- length(b_gb)
            b_df <- data.frame(
              y = c(b_ga, b_gb),
              group = factor(c(rep(name_a, b_n1), rep(name_b, b_n2))),
              cov = c(b_cov_a, b_cov_b)
            )
            b_fit <- try(lm(y ~ group + cov, data = b_df), silent = TRUE)
            if (inherits(b_fit, "try-error")) return(NA_real_)
            b_emm <- try(emmeans::emmeans(b_fit, "group"), silent = TRUE)
            if (inherits(b_emm, "try-error")) return(NA_real_)
            b_emm_df <- as.data.frame(b_emm)
            b_emm_a <- b_emm_df$emmean[b_emm_df$group == name_a]
            b_emm_b <- b_emm_df$emmean[b_emm_df$group == name_b]
            if (length(b_emm_a) == 0 || length(b_emm_b) == 0) return(NA_real_)
            b_adj_diff <- b_emm_a - b_emm_b
            b_s1 <- sd(b_ga, na.rm = TRUE)
            b_s2 <- sd(b_gb, na.rm = TRUE)
            b_sd_pooled <- sqrt(((b_n1 - 1) * b_s1^2 + (b_n2 - 1) * b_s2^2) / (b_n1 + b_n2 - 2))
            if (method == "cohens_d") {
              return(b_adj_diff / b_sd_pooled)
            } else if (method == "hedges_g") {
              b_corr <- 1 - (3 / (4 * (b_n1 + b_n2 - 2) - 1))
              return((b_adj_diff / b_sd_pooled) * b_corr)
            } else if (method == "glass_delta") {
              b_s_ref <- if (ref_grp == name_a) b_s1 else b_s2
              if (is.na(b_s_ref) || b_s_ref == 0) return(NA_real_)
              b_diff_glass <- if (ref_grp == name_a) -b_adj_diff else b_adj_diff
              b_df_ref <- if (ref_grp == name_a) b_n1 - 1 else b_n2 - 1
              b_corr_glass <- if (b_df_ref > 1) 1 - (3 / (4 * b_df_ref - 1)) else 1
              return((b_diff_glass / b_s_ref) * b_corr_glass)
            }
            return(NA_real_)
          }
          calc_adj_rb <- function(b_ga, b_gb, b_cov_a, b_cov_b) {
            b_n1 <- length(b_ga); b_n2 <- length(b_gb)
            b_df <- data.frame(y = c(b_ga, b_gb), cov = c(b_cov_a, b_cov_b))
            b_fit <- try(lm(y ~ cov, data = b_df), silent = TRUE)
            if (inherits(b_fit, "try-error")) return(NA_real_)
            b_resids <- residuals(b_fit)
            b_res_a <- b_resids[1:b_n1]
            b_res_b <- b_resids[(b_n1 + 1):(b_n1 + b_n2)]
            rb_res <- try(effectsize::rank_biserial(b_res_a, b_res_b, ci = NULL)[[1]], silent = TRUE)
            if (inherits(rb_res, "try-error")) return(NA_real_)
            as.numeric(rb_res)
          }
          boot_fn <- function() {
            idx_a <- sample(seq_len(n1), n1, replace = TRUE)
            idx_b <- sample(seq_len(n2), n2, replace = TRUE)
            b_ga <- ga[idx_a]; b_cov_a <- cov_a[idx_a]
            b_gb <- gb[idx_b]; b_cov_b <- cov_b[idx_b]
            if (method == "rank_biserial") {
              calc_adj_rb(b_ga, b_gb, b_cov_a, b_cov_b)
            } else {
              calc_adj_es(b_ga, b_gb, b_cov_a, b_cov_b)
            }
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if (length(boot_vals) >= 10) {
            alpha <- 1 - ci_level
            q     <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
          }
        }
      } else {
        # Without covariate: compute sample point estimate and analytical CI
        res <- tryCatch({
          switch(method,
                 cohens_d      = effectsize::cohens_d(ga, gb, ci = ci_level),
                 hedges_g      = effectsize::hedges_g(ga, gb, ci = ci_level),
                 glass_delta   = effectsize::glass_delta(y ~ group, data = df_pair, reference = ref_grp, ci = ci_level),
                 rank_biserial = effectsize::rank_biserial(ga, gb, ci = ci_level)
          )
        }, error = function(e) NULL)
        if (!is.null(res)) {
          es_val <- as.numeric(res[[1]])
          if (ci_method == "analytical") {
            ci_lo  <- as.numeric(res$CI_low)
            ci_hi  <- as.numeric(res$CI_high)
          }
        }
        
        # Bootstrap without covariate
        if (ci_method == "bootstrap") {
          boot_fn <- function() {
            b1 <- sample(ga, n1, replace = TRUE); b2 <- sample(gb, n2, replace = TRUE)
            res_b <- tryCatch({
              switch(method,
                     cohens_d      = effectsize::cohens_d(b1, b2, ci = NULL)[[1]],
                     hedges_g      = effectsize::hedges_g(b1, b2, ci = NULL)[[1]],
                     glass_delta   = {
                       b_df <- data.frame(y = c(b1, b2), group = factor(c(rep(name_a, n1), rep(name_b, n2))))
                       effectsize::glass_delta(y ~ group, data = b_df, reference = ref_grp, ci = NULL)[[1]]
                     },
                     rank_biserial = effectsize::rank_biserial(b1, b2, ci = NULL)[[1]]
              )
            }, error = function(e) NA_real_)
            as.numeric(res_b)
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if (length(boot_vals) >= 10) {
            alpha <- 1 - ci_level
            q     <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
          }
        }
      }
      list(es_value = es_val, ci_lower = ci_lo, ci_upper = ci_hi, method = method, interpretation = private$.interpret(es_val, method, benchmark))
    },
    
    .compute_dependent_paired = function(col1, col2, name1, name2, ref_wave, method, ci_method, ci_level, n_bootstrap, cov_col = NULL, benchmark = "cohen") {
      es_val <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
      has_cov <- !is.null(cov_col)
      n <- length(col1)
      sd1 <- sd(col1, na.rm = TRUE)
      sd2 <- sd(col2, na.rm = TRUE)
      diff_vec <- col1 - col2
      sd_diff <- sd(diff_vec, na.rm = TRUE)
      
      if (is.na(sd_diff) || sd_diff == 0) {
        return(list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method, interpretation = "Zero variance in difference score"))
      }
      
      if (has_cov) {
        s_ref <- if (ref_wave == name1) sd1 else sd2
        df <- data.frame(diff = diff_vec, cov = cov_col)
        if (method == "rank_biserial_paired") {
          fit <- lm(diff ~ cov, data = df)
          resids <- residuals(fit)
          res <- tryCatch({
            effectsize::rank_biserial(resids, mu = 0, ci = ci_level)
          }, error = function(e) NULL)
          if (!is.null(res)) {
            es_val <- as.numeric(res[[1]])
            if (ci_method == "analytical") {
              ci_lo  <- as.numeric(res$CI_low)
              ci_hi  <- as.numeric(res$CI_high)
            }
          }
        } else {
          fit <- lm(diff ~ cov, data = df)
          emm <- emmeans::emmeans(fit, ~ 1)
          emm_df <- as.data.frame(emm)
          if (nrow(emm_df) > 0) {
            adj_diff <- emm_df$emmean[1]
            se_diff <- emm_df$SE[1]
            alpha <- 1 - ci_level
            df_res <- df.residual(fit)
            t_crit <- qt(1 - alpha / 2, df = df_res)
            ci_lo_diff <- adj_diff - t_crit * se_diff
            ci_hi_diff <- adj_diff + t_crit * se_diff
            
            if (method == "cohens_dz") {
              es_val <- adj_diff / sd_diff
              if (ci_method == "analytical") {
                ci_lo <- ci_lo_diff / sd_diff
                ci_hi <- ci_hi_diff / sd_diff
              }
            } else if (method == "hedges_gz") {
              df_mod <- n - 2
              correction <- if (df_mod > 1) 1 - (3 / (4 * df_mod - 1)) else 1
              es_val <- (adj_diff / sd_diff) * correction
              if (ci_method == "analytical") {
                ci_lo <- (ci_lo_diff / sd_diff) * correction
                ci_hi <- (ci_hi_diff / sd_diff) * correction
              }
            } else if (method == "glass_dz") {
              if (is.na(s_ref) || s_ref == 0) return(list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method, interpretation = "Zero variance in reference wave"))
              diff_glass <- if (ref_wave == name1) -adj_diff else adj_diff
              lo_glass   <- if (ref_wave == name1) -ci_hi_diff else ci_lo_diff
              hi_glass   <- if (ref_wave == name1) -ci_lo_diff else ci_hi_diff
              df_ref <- n - 2
              corr_glass <- if (df_ref > 1) 1 - (3 / (4 * df_ref - 1)) else 1
              es_val <- (diff_glass / s_ref) * corr_glass
              if (ci_method == "analytical") {
                ci_lo <- (lo_glass / s_ref) * corr_glass
                ci_hi <- (hi_glass / s_ref) * corr_glass
              }
            }
          }
        }
        
        # Bootstrap paired with covariate
        if (ci_method == "bootstrap") {
          calc_adj_paired_es <- function(b_col1, b_col2, b_cov) {
            b_n <- length(b_col1)
            b_diff <- b_col1 - b_col2
            b_df <- data.frame(diff = b_diff, cov = b_cov)
            b_fit <- try(lm(diff ~ cov, data = b_df), silent = TRUE)
            if (inherits(b_fit, "try-error")) return(NA_real_)
            b_emm <- try(emmeans::emmeans(b_fit, ~ 1), silent = TRUE)
            if (inherits(b_emm, "try-error")) return(NA_real_)
            b_adj_diff <- as.data.frame(b_emm)$emmean[1]
            b_sd_diff <- sd(b_diff, na.rm = TRUE)
            if (method == "cohens_dz") {
              return(b_adj_diff / b_sd_diff)
            } else if (method == "hedges_gz") {
              b_corr <- 1 - (3 / (4 * (b_n - 2) - 1))
              return((b_adj_diff / b_sd_diff) * b_corr)
            } else if (method == "glass_dz") {
              b_s_ref <- if (ref_wave == name1) sd(b_col1, na.rm = TRUE) else sd(b_col2, na.rm = TRUE)
              if (is.na(b_s_ref) || b_s_ref == 0) return(NA_real_)
              b_diff_glass <- if (ref_wave == name1) -b_adj_diff else b_adj_diff
              b_corr_glass <- 1 - (3 / (4 * (b_n - 2) - 1))
              return((b_diff_glass / b_s_ref) * b_corr_glass)
            } else if (method == "rank_biserial_paired") {
              b_resids <- residuals(b_fit)
              rb_res <- try(effectsize::rank_biserial(b_resids, mu = 0, ci = NULL)[[1]], silent = TRUE)
              if (inherits(rb_res, "try-error")) return(NA_real_)
              return(as.numeric(rb_res))
            }
            return(NA_real_)
          }
          boot_fn <- function() {
            idx <- sample(seq_len(n), n, replace = TRUE)
            b_col1 <- col1[idx]
            b_col2 <- col2[idx]
            b_cov <- cov_col[idx]
            calc_adj_paired_es(b_col1, b_col2, b_cov)
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if (length(boot_vals) >= 10) {
            alpha <- 1 - ci_level
            q     <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
          }
        }
      } else {
        # Without covariate: paired analytical point estimate and CI
        res <- tryCatch({
          switch(method,
                 cohens_dz            = effectsize::cohens_d(col1, col2, paired = TRUE, ci = ci_level),
                 hedges_gz            = effectsize::hedges_g(col1, col2, paired = TRUE, ci = ci_level),
                 glass_dz             = {
                   if (ref_wave == name1) {
                     effectsize::repeated_measures_d(col2, col1, method = "b", ci = ci_level)
                   } else {
                     effectsize::repeated_measures_d(col1, col2, method = "b", ci = ci_level)
                   }
                 },
                 rank_biserial_paired = effectsize::rank_biserial(col1, col2, paired = TRUE, ci = ci_level)
          )
        }, error = function(e) NULL)
        if (!is.null(res)) {
          es_val <- as.numeric(res[[1]])
          if (ci_method == "analytical") {
            ci_lo  <- as.numeric(res$CI_low)
            ci_hi  <- as.numeric(res$CI_high)
          }
        }
        
        # Bootstrap paired without covariate
        if (ci_method == "bootstrap") {
          boot_fn <- function() {
            idx <- sample(seq_len(n), n, replace = TRUE)
            b1 <- col1[idx]; b2 <- col2[idx]
            res_b <- tryCatch({
              switch(method,
                     cohens_dz            = effectsize::cohens_d(b1, b2, paired = TRUE, ci = NULL)[[1]],
                     hedges_gz            = effectsize::hedges_g(b1, b2, paired = TRUE, ci = NULL)[[1]],
                     glass_dz             = {
                       if (ref_wave == name1) {
                         effectsize::repeated_measures_d(b2, b1, method = "b", ci = NULL)[[1]]
                       } else {
                         effectsize::repeated_measures_d(b1, b2, method = "b", ci = NULL)[[1]]
                       }
                     },
                     rank_biserial_paired = effectsize::rank_biserial(b1, b2, paired = TRUE, ci = NULL)[[1]]
              )
            }, error = function(e) NA_real_)
            as.numeric(res_b)
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if (length(boot_vals) >= 10) {
            alpha <- 1 - ci_level
            q     <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
          }
        }
      }
      list(es_value = es_val, ci_lower = ci_lo, ci_upper = ci_hi, method = method, interpretation = private$.interpret(es_val, method, benchmark))
    },
    
    .compute_glm = function(iv_col, dv_col, name_a, name_b, dist, ci_method, ci_level, n_bootstrap, cov_col = NULL, benchmark = "cohen") {
      family_fn <- switch(dist, 
                          glm_gaussian     = gaussian(link = "identity"),
                          glm_gamma        = Gamma(link = "log"), 
                          glm_inv_gaussian = inverse.gaussian(link = "log"), 
                          Gamma(link = "log"))
      
      mask <- iv_col %in% c(name_a, name_b)
      iv_sub <- droplevels(iv_col[mask])
      dv_sub <- dv_col[mask]
      n <- length(dv_sub)
      has_cov <- !is.null(cov_col)
      
      # Positive value check for Gamma and Inverse Gaussian
      if (dist %in% c("glm_gamma", "glm_inv_gaussian")) {
        if (any(dv_sub <= 0, na.rm = TRUE)) {
          return(list(
            es_value = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            method = paste0("AME (", dist, ")"),
            interpretation = "Error: DV must be strictly positive (> 0) for Gamma/Inverse Gaussian distribution"
          ))
        }
      }
      
      cov_sub <- if (has_cov) cov_col[mask] else NULL
      df_glm <- if (has_cov) {
        data.frame(y = dv_sub, x = factor(iv_sub, levels = c(name_a, name_b)), cov = cov_sub)
      } else {
        data.frame(y = dv_sub, x = factor(iv_sub, levels = c(name_a, name_b)))
      }
      formula_glm <- if (has_cov) y ~ x + cov else y ~ x
      
      fit <- glm(formula_glm, data = df_glm, family = family_fn)
      emm <- emmeans::emmeans(fit, "x", type = "response")
      pairs_df <- as.data.frame(pairs(emmeans::regrid(emm)))
      
      ame <- pairs_df$estimate[1]
      se_ame <- pairs_df$SE[1]
      alpha <- 1 - ci_level
      z <- qnorm(1 - alpha / 2)
      ci_lo <- ame - z * se_ame
      ci_hi <- ame + z * se_ame
      
      if (ci_method == "bootstrap") {
        boot_fn <- function() {
          idx <- sample(seq_len(n), n, replace = TRUE)
          b_df <- df_glm[idx, ]
          b_fit <- try(glm(formula_glm, data = b_df, family = family_fn), silent = TRUE)
          if (inherits(b_fit, "try-error")) return(NA_real_)
          b_emm <- try(emmeans::emmeans(b_fit, "x", type = "response"), silent = TRUE)
          if (inherits(b_emm, "try-error")) return(NA_real_)
          b_pairs <- try(as.data.frame(pairs(emmeans::regrid(b_emm))), silent = TRUE)
          if (inherits(b_pairs, "try-error") || nrow(b_pairs) == 0) return(NA_real_)
          b_pairs$estimate[1]
        }
        boot_vals <- replicate(n_bootstrap, boot_fn())
        boot_vals <- boot_vals[is.finite(boot_vals)]
        if (length(boot_vals) >= 10) {
          q <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
          ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
        }
      }
      
      sd_dv <- sd(dv_sub, na.rm = TRUE)
      interp_val <- if (is.finite(sd_dv) && sd_dv > 0) private$.interpret(abs(ame) / sd_dv, "cohens_d", benchmark) else "—"
      list(
        es_value = ame,
        ci_lower = ci_lo,
        ci_upper = ci_hi,
        method = paste0("AME (", dist, ")"),
        interpretation = interp_val
      )
    },
    
    .renderESPlot = function(image, ggtheme, theme, ...) {
      plot_df <- image$state
      if (is.null(plot_df) || nrow(plot_df) == 0) return(FALSE)
      
      # Reverse factor levels so first computed row appears at top of plot
      plot_df$label <- factor(plot_df$label, levels = rev(unique(plot_df$label)))
      
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(y = label, x = estimate)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.7) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.25, color = "#2c3e50", linewidth = 0.8) +
        ggplot2::geom_point(color = "#2980b9", size = 3) +
        ggplot2::labs(
          x = "Effect Size & Confidence Interval",
          y = "Comparison",
          title = "Forest Plot of Effect Sizes"
        ) +
        ggtheme +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 9, face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
        )
      
      print(p)
      TRUE
    }
  )
)
