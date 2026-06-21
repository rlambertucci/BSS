effectSizeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "effectSizeClass",
  inherit = effectSizeBase,
  private = list(
    .is_defined = function(x) {
      !is.null(x) && length(x) > 0 && all(nzchar(x))
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
        '<p><b>&#9654; Interpretation Benchmarks</b> (Cohen, 1988)</p>',
        '<ul style="margin-top:0px; padding-left:20px;">',
        '<li><b>Cohen / Hedge / Glass:</b> &lt; 0.2 (Negligible) | &ge; 0.2 (Small) | &ge; 0.5 (Medium) | &ge; 0.8 (Large)</li>',
        '<li><b>Rank Biserial (Correlation):</b> &lt; 0.1 (Negligible) | &ge; 0.1 (Small) | &ge; 0.3 (Medium) | &ge; 0.5 (Large)</li>',
        '</ul>',
        '</div>'
      )
      self$results$analysisInfo$setContent(info_html)
      
      ame_html <- paste0(
        '<div style="font-size:0.9em; line-height:1.6; padding:8px; background-color:#f4f6f8; border-left:4px solid #2c3e50; margin-bottom:10px;">',
        '<h4 style="margin-top:0; margin-bottom:6px;">&#128161; AME (Average Marginal Effect) Interpretation</h4>',
        '<p style="margin-bottom:0;">The AME represents the absolute difference in the dependent variable\'s original metric between the two groups. ',
        'For the <b>Interpretation</b> column, the AME is internally standardized (divided by the standard deviation of the DV) ',
        'so that Cohen\'s (1988) benchmarks can be applied.</p>',
        '</div>'
      )
      self$results$ameBenchmark$setContent(ame_html)
    },
    .run = function() {
      data                 <- self$data
      mode                 <- self$options$mode
      run_analysis         <- self$options$run_analysis
      include_interactions <- self$options$include_interactions
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
      
      try(table$deleteRows(), silent = TRUE)
      try(table_long_between$deleteRows(), silent = TRUE)
      try(clesTable$deleteRows(), silent = TRUE)
      try(clesLongTable$deleteRows(), silent = TRUE)
      
      try(table$deleteNote("covariate_note"), silent = TRUE)
      try(table_long_between$deleteNote("covariate_note"), silent = TRUE)
      if (has_cov) {
        table$setNote("covariate_note", "Note. Adjusted statistics (Estimated Marginal Means) are computed controlling for the covariate using the 'emmeans' R package (Lenth, 2024).")
        table_long_between$setNote("covariate_note", "Note. Adjusted statistics (Estimated Marginal Means) are computed controlling for the covariate using the 'emmeans' R package (Lenth, 2024).")
      }
      
      n_rows <- 0
      n_rows_long_between <- 0
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
        for (iv_name in names(grouping_factors)) {
          iv_col_raw <- grouping_factors[[iv_name]]
          for (dv in dep_vars) {
            if (iv_name == dv) next
            dv_col <- data[[dv]]
            if (!is.numeric(dv_col)) next
            cov_col_raw <- if (has_cov) data[[cov_var]] else NULL
            valid  <- !is.na(iv_col_raw) & !is.na(dv_col)
            if (has_cov) {
              valid <- valid & !is.na(cov_col_raw)
            }
            iv_col <- droplevels(iv_col_raw[valid])
            dv_col <- dv_col[valid]
            cov_col <- if (has_cov) cov_col_raw[valid] else NULL
            n      <- sum(valid)
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
                  private$.compute_glm(iv_col, dv_col, gp[[1]], gp[[2]], es_glm_dist, ci_method, ci_level, n_bootstrap, cov_col)
                }, error = function(e) {
                  list(es_value = NA, ci_lower = NA, ci_upper = NA, method = "AME (error)", interpretation = paste0("GLM error: ", e$message))
                })
              } else {
                ref_grp <- if (es_method == "glass_delta") {
                  rg <- trimws(reference_group)
                  if (nchar(rg) > 0 && rg %in% groups) rg else gp[[1]]
                } else gp[[1]]
                result <- tryCatch({
                  private$.compute_2groups(ga, gb, gp[[1]], gp[[2]], ref_grp, es_method, ci_method, ci_level, n_bootstrap, cov_a, cov_b)
                }, error = function(e) {
                  list(es_value = NA, ci_lower = NA, ci_upper = NA, method = es_method, interpretation = paste0("Computation error: ", e$message))
                })
              }
              interp_note <- result$interpretation
              if (es_method == "glass_delta" && !isTRUE(use_glm)) interp_note <- paste0(interp_note, " ", groups_note)
              n_rows <- n_rows + 1
              table$addRow(rowKey = n_rows, values = list(
                dv             = dv,
                iv             = iv_name,
                comparison     = paste0(gp[[1]], " vs ", gp[[2]]),
                method         = result$method,
                es_value       = result$es_value,
                ci_lower       = result$ci_lower,
                ci_upper       = result$ci_upper,
                interpretation = interp_note,
                n_obs          = n_obs
              ))
              if (isTRUE(show_cles)) {
                cles_result <- private$.compute_cles_independent(ga, gb)
                clesTable$addRow(
                  rowKey = paste0(iv_name, dv, gp[[1]], gp[[2]]),
                  values = list(
                    dv         = dv,
                    iv         = iv_name,
                    comparison = paste0(gp[[1]], " vs ", gp[[2]]),
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
            col1_full <- data[[w1]]
            col2_full <- data[[w2]]
            cov_col_full <- if (has_cov) data[[cov_var]] else NULL
            if (!is.numeric(col1_full) || !is.numeric(col2_full)) next
            for (sub in subsets) {
              mask <- sub$mask
              col1 <- col1_full[mask]
              col2 <- col2_full[mask]
              cov_col <- if (has_cov) cov_col_full[mask] else NULL
              valid  <- !is.na(col1) & !is.na(col2)
              if (has_cov) {
                valid <- valid & !is.na(cov_col)
              }
              col1   <- col1[valid]
              col2   <- col2[valid]
              cov_col <- if (has_cov) cov_col[valid] else NULL
              n_pair <- sum(valid)
              if (n_pair < 3) next
              ref_wave <- if (es_method_long == "glass_dz") {
                rg <- trimws(reference_group_long)
                if (nchar(rg) > 0 && rg %in% c(w1, w2)) rg else w1
              } else w1
              result <- tryCatch({
                private$.compute_dependent_paired(col1, col2, w1, w2, ref_wave, es_method_long, ci_method, ci_level, n_bootstrap, cov_col)
              }, error = function(e) {
                list(es_value = NA, ci_lower = NA, ci_upper = NA, method = es_method_long, interpretation = paste0("Paired error: ", e$message))
              })
              n_rows <- n_rows + 1
              table$addRow(rowKey = n_rows, values = list(
                dv             = paste0(w1, " vs ", w2),
                iv             = sub$label,
                comparison     = paste0(w1, " vs ", w2),
                method         = result$method,
                es_value       = result$es_value,
                ci_lower       = result$ci_lower,
                ci_upper       = result$ci_upper,
                interpretation = result$interpretation,
                n_obs          = n_pair
              ))
              if (isTRUE(show_cles_long)) {
                cles_result <- private$.compute_cles_paired(col1, col2)
                clesLongTable$addRow(
                  rowKey = paste0(sub$label, w1, w2),
                  values = list(
                    dv         = sub$label,
                    comparison = paste0(w1, " vs ", w2),
                    cles       = cles_result$cles
                  )
                )
              }
            }
          }
          if (length(grouping_factors) > 0) {
            for (wave in wave_vars) {
              dv_col <- data[[wave]]
              if (!is.numeric(dv_col)) next
              for (iv_name in names(grouping_factors)) {
                iv_col_raw <- grouping_factors[[iv_name]]
                cov_col_raw <- if (has_cov) data[[cov_var]] else NULL
                valid <- !is.na(iv_col_raw) & !is.na(dv_col)
                if (has_cov) {
                  valid <- valid & !is.na(cov_col_raw)
                }
                iv_col <- droplevels(iv_col_raw[valid])
                dv_col_sub <- dv_col[valid]
                cov_col <- if (has_cov) cov_col_raw[valid] else NULL
                groups <- levels(iv_col)
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
                    private$.compute_2groups(ga, gb, gp[[1]], gp[[2]], gp[[1]], method_to_use, ci_method, ci_level, n_bootstrap, cov_a, cov_b)
                  }, error = function(e) {
                    list(es_value = NA, ci_lower = NA, ci_upper = NA, method = method_to_use, interpretation = paste0("Error: ", e$message))
                  })
                  n_rows_long_between <- n_rows_long_between + 1
                  table_long_between$addRow(rowKey = n_rows_long_between, values = list(
                    dv             = wave,
                    iv             = iv_name,
                    comparison     = paste0(gp[[1]], " vs ", gp[[2]]),
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
            dv_col_full <- data[[dv]]
            cov_col_full <- if (has_cov) data[[cov_var]] else NULL
            if (!is.numeric(dv_col_full)) next
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
                    private$.compute_dependent_paired(col1, col2, t1, t2, ref_wave, es_method_long, ci_method, ci_level, n_bootstrap, cov_col)
                  }, error = function(e) {
                    list(es_value = NA, ci_lower = NA, ci_upper = NA, method = es_method_long, interpretation = paste0("Paired error: ", e$message))
                  })
                  n_rows <- n_rows + 1
                  table$addRow(rowKey = n_rows, values = list(
                    dv             = dv,
                    iv             = sub$label,
                    comparison     = paste0(t1, " vs ", t2),
                    method         = result$method,
                    es_value       = result$es_value,
                    ci_lower       = result$ci_lower,
                    ci_upper       = result$ci_upper,
                    interpretation = result$interpretation,
                    n_obs          = n_pair
                  ))
                  if (isTRUE(show_cles_long)) {
                    cles_result <- private$.compute_cles_paired(col1, col2)
                    clesLongTable$addRow(
                      rowKey = paste0(sub$label, dv, t1, t2),
                      values = list(
                        dv         = if (sub$label == "Overall Sample") dv else paste0(dv, " (", sub$label, ")"),
                        comparison = paste0(t1, " vs ", t2),
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
                    private$.compute_2groups(g1, g2, t1, t2, t1, method_to_use_unpaired, ci_method, ci_level, n_bootstrap, cov_g1, cov_g2)
                  }, error = function(e) {
                    list(es_value = NA, ci_lower = NA, ci_upper = NA, method = method_to_use_unpaired, interpretation = paste0("Error: ", e$message))
                  })
                  n_rows <- n_rows + 1
                  table$addRow(rowKey = n_rows, values = list(
                    dv             = dv,
                    iv             = sub$label,
                    comparison     = paste0(t1, " vs ", t2, " (unpaired)"),
                    method         = result$method,
                    es_value       = result$es_value,
                    ci_lower       = result$ci_lower,
                    ci_upper       = result$ci_upper,
                    interpretation = result$interpretation,
                    n_obs          = n_pair
                  ))
                  if (isTRUE(show_cles_long)) {
                    cles_result <- private$.compute_cles_independent(g1, g2)
                    clesTable$addRow(
                      rowKey = paste0(sub$label, dv, t1, t2),
                      values = list(
                        dv         = dv,
                        iv         = sub$label,
                        comparison = paste0(t1, " vs ", t2, " (unpaired)"),
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
                  iv_col <- droplevels(iv_col_raw[valid])
                  dv_col_sub <- dv_col_full[valid]
                  cov_col <- if (has_cov) cov_col_raw[valid] else NULL
                  groups <- levels(iv_col)
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
                      private$.compute_2groups(ga, gb, gp[[1]], gp[[2]], gp[[1]], method_to_use, ci_method, ci_level, n_bootstrap, cov_a, cov_b)
                    }, error = function(e) {
                      list(es_value = NA, ci_lower = NA, ci_upper = NA, method = method_to_use, interpretation = paste0("Error: ", e$message))
                    })
                    n_rows_long_between <- n_rows_long_between + 1
                    table_long_between$addRow(rowKey = n_rows_long_between, values = list(
                      dv             = paste0(dv, " (At Time: ", t1, ")"),
                      iv             = iv_name,
                      comparison     = paste0(gp[[1]], " vs ", gp[[2]]),
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
      if (n_rows == 0) {
        self$results$text$setContent("No valid pairs found. Check variable assignments, types, and missing data.")
      } else {
        self$results$text$setContent(paste0(
          (n_rows + n_rows_long_between), " effect size(s) computed successfully."
        ))
      }
      self$results$text$setVisible(TRUE)
    },
    .interpret = function(value, method) {
      if (is.na(value)) return("—")
      abs_val <- abs(value)
      thresholds <- switch(method,
                           cohens_d = , hedges_g = , glass_delta = ,
                           cohens_dz = , hedges_gz = , glass_dz =
                             list(small = 0.2, medium = 0.5, large = 0.8),
                           rank_biserial = , rank_biserial_paired =
                             list(small = 0.1, medium = 0.3, large = 0.5),
                           list(small = 0.2, medium = 0.5, large = 0.8)
      )
      if (abs_val < thresholds$small)  return("Negligible")
      if (abs_val < thresholds$medium) return("Small")
      if (abs_val < thresholds$large)  return("Medium")
      return("Large")
    },
    .compute_cles_independent = function(ga, gb) {
      ps <- try(effectsize::p_superiority(ga, gb, parametric = FALSE), silent = TRUE)
      if (inherits(ps, "try-error")) return(list(cles = NA))
      p <- as.numeric(ps$p_superiority)
      cles_val <- if (p < 0.5) 1 - p else p
      list(cles = cles_val)
    },
    .compute_cles_paired = function(col1, col2) {
      diff_vec <- col1 - col2
      if (length(na.omit(diff_vec)) == 0) return(list(cles = NA))
      p <- mean(diff_vec > 0, na.rm = TRUE) + 0.5 * mean(diff_vec == 0, na.rm = TRUE)
      cles_val <- if (p < 0.5) 1 - p else p
      list(cles = cles_val)
    },
    .compute_2groups = function(ga, gb, name_a, name_b, ref_grp, method, ci_method, ci_level, n_bootstrap, cov_a = NULL, cov_b = NULL) {
      es_val <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
      has_cov <- !is.null(cov_a) && !is.null(cov_b)
      if (has_cov) {
        n1 <- length(ga)
        n2 <- length(gb)
        s1 <- sd(ga)
        s2 <- sd(gb)
        raw_sd_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
        df <- data.frame(
          y = c(ga, gb),
          group = factor(c(rep(name_a, n1), rep(name_b, n2))),
          cov = c(cov_a, cov_b)
        )
        if (ci_method == "analytical") {
          if (method == "rank_biserial") {
            res_fit <- lm(y ~ cov, data = df)
            resids <- residuals(res_fit)
            res_a <- resids[df$group == name_a]
            res_b <- resids[df$group == name_b]
            res <- tryCatch({
              effectsize::rank_biserial(res_a, res_b, ci = ci_level)
            }, error = function(e) NULL)
            if (!is.null(res)) {
              es_val <- as.numeric(res[[1]])
              ci_lo  <- as.numeric(res$CI_low)
              ci_hi  <- as.numeric(res$CI_high)
            }
          } else {
            fit <- lm(y ~ group + cov, data = df)
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
                ci_lo <- ci_lo_diff / raw_sd_pooled
                ci_hi <- ci_hi_diff / raw_sd_pooled
              } else if (method == "hedges_g") {
                correction <- 1 - (3 / (4 * (n1 + n2) - 9))
                es_val <- (adj_diff / raw_sd_pooled) * correction
                ci_lo <- (ci_lo_diff / raw_sd_pooled) * correction
                ci_hi <- (ci_hi_diff / raw_sd_pooled) * correction
              } else if (method == "glass_delta") {
                s_ref <- if (ref_grp == name_a) s1 else s2
                es_val <- adj_diff / s_ref
                ci_lo <- ci_lo_diff / s_ref
                ci_hi <- ci_hi_diff / s_ref
              }
            }
          }
        } else {
          calc_adj_es <- function(b_ga, b_gb, b_cov_a, b_cov_b) {
            b_n1 <- length(b_ga)
            b_n2 <- length(b_gb)
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
            b_s1 <- sd(b_ga)
            b_s2 <- sd(b_gb)
            b_sd_pooled <- sqrt(((b_n1 - 1) * b_s1^2 + (b_n2 - 1) * b_s2^2) / (b_n1 + b_n2 - 2))
            if (method == "cohens_d") {
              return(b_adj_diff / b_sd_pooled)
            } else if (method == "hedges_g") {
              b_correction <- 1 - (3 / (4 * (b_n1 + b_n2) - 9))
              return((b_adj_diff / b_sd_pooled) * b_correction)
            } else if (method == "glass_delta") {
              b_s_ref <- if (ref_grp == name_a) b_s1 else b_s2
              return(b_adj_diff / b_s_ref)
            }
            return(NA_real_)
          }
          calc_adj_rb <- function(b_ga, b_gb, b_cov_a, b_cov_b) {
            b_n1 <- length(b_ga)
            b_n2 <- length(b_gb)
            b_df <- data.frame(
              y = c(b_ga, b_gb),
              cov = c(b_cov_a, b_cov_b)
            )
            b_fit <- try(lm(y ~ cov, data = b_df), silent = TRUE)
            if (inherits(b_fit, "try-error")) return(NA_real_)
            b_resids <- residuals(b_fit)
            b_res_a <- b_resids[1:b_n1]
            b_res_b <- b_resids[(b_n1 + 1):(b_n1 + b_n2)]
            rb_res <- try(effectsize::rank_biserial(b_res_a, b_res_b)[[1]], silent = TRUE)
            if (inherits(rb_res, "try-error")) return(NA_real_)
            as.numeric(rb_res)
          }
          boot_fn <- function() {
            idx_a <- sample(seq_len(n1), n1, replace = TRUE)
            idx_b <- sample(seq_len(n2), n2, replace = TRUE)
            b_ga <- ga[idx_a]
            b_cov_a <- cov_a[idx_a]
            b_gb <- gb[idx_b]
            b_cov_b <- cov_b[idx_b]
            if (method == "rank_biserial") {
              calc_adj_rb(b_ga, b_gb, b_cov_a, b_cov_b)
            } else {
              calc_adj_es(b_ga, b_gb, b_cov_a, b_cov_b)
            }
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if (length(boot_vals) >= 10) {
            es_val <- mean(boot_vals)
            alpha <- 1 - ci_level
            q     <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- q[[1]]; ci_hi <- q[[2]]
          }
        }
      } else {
        if (ci_method == "analytical") {
          res <- tryCatch({
            switch(method,
                   cohens_d      = effectsize::cohens_d(ga, gb, ci = ci_level),
                   hedges_g      = effectsize::hedges_g(ga, gb, ci = ci_level),
                   glass_delta   = effectsize::glass_delta(ga, gb, ci = ci_level),
                   rank_biserial = effectsize::rank_biserial(ga, gb, ci = ci_level)
            )
          }, error = function(e) NULL)
          if (!is.null(res)) {
            es_val <- as.numeric(res[[1]])
            ci_lo  <- as.numeric(res$CI_low)
            ci_hi  <- as.numeric(res$CI_high)
          }
        } else {
          n1 <- length(ga); n2 <- length(gb)
          boot_fn <- function() {
            b1 <- sample(ga, n1, replace = TRUE); b2 <- sample(gb, n2, replace = TRUE)
            res_b <- tryCatch({
              switch(method,
                     cohens_d      = effectsize::cohens_d(b1, b2, ci = ci_level)[[1]],
                     hedges_g      = effectsize::hedges_g(b1, b2, ci = ci_level)[[1]],
                     glass_delta   = effectsize::glass_delta(b1, b2, ci = ci_level)[[1]],
                     rank_biserial = effectsize::rank_biserial(b1, b2, ci = ci_level)[[1]]
              )
            }, error = function(e) NA_real_)
            as.numeric(res_b)
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if (length(boot_vals) >= 10) {
            es_val <- mean(boot_vals)
            alpha <- 1 - ci_level
            q     <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- q[[1]]; ci_hi <- q[[2]]
          }
        }
      }
      list(es_value = es_val, ci_lower = ci_lo, ci_upper = ci_hi, method = method, interpretation = private$.interpret(es_val, method))
    },
    .compute_dependent_paired = function(col1, col2, name1, name2, ref_wave, method, ci_method, ci_level, n_bootstrap, cov_col = NULL) {
      es_val <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
      has_cov <- !is.null(cov_col)
      if (has_cov) {
        n <- length(col1)
        diff_vec <- col1 - col2
        raw_sd_diff <- sd(diff_vec, na.rm = TRUE)
        s_ref <- if (ref_wave == name1) sd(col1, na.rm = TRUE) else sd(col2, na.rm = TRUE)
        df <- data.frame(diff = diff_vec, cov = cov_col)
        if (ci_method == "analytical") {
          if (method == "rank_biserial_paired") {
            fit <- lm(diff ~ cov, data = df)
            resids <- residuals(fit)
            res <- tryCatch({
              effectsize::rank_biserial(resids, rep(0, length(resids)), paired = TRUE, ci = ci_level)
            }, error = function(e) NULL)
            if (!is.null(res)) {
              es_val <- as.numeric(res[[1]])
              ci_lo  <- as.numeric(res$CI_low)
              ci_hi  <- as.numeric(res$CI_high)
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
                es_val <- adj_diff / raw_sd_diff
                ci_lo <- ci_lo_diff / raw_sd_diff
                ci_hi <- ci_hi_diff / raw_sd_diff
              } else if (method == "hedges_gz") {
                correction <- 1 - (3 / (4 * n - 9))
                es_val <- (adj_diff / raw_sd_diff) * correction
                ci_lo <- (ci_lo_diff / raw_sd_diff) * correction
                ci_hi <- (ci_hi_diff / raw_sd_diff) * correction
              } else if (method == "glass_dz") {
                es_val <- adj_diff / s_ref
                ci_lo <- ci_lo_diff / s_ref
                ci_hi <- ci_hi_diff / s_ref
              }
            }
          }
        } else {
          calc_adj_paired_es <- function(b_col1, b_col2, b_cov) {
            b_n <- length(b_col1)
            b_diff <- b_col1 - b_col2
            b_df <- data.frame(diff = b_diff, cov = b_cov)
            b_fit <- try(lm(diff ~ cov, data = b_df), silent = TRUE)
            if (inherits(b_fit, "try-error")) return(NA_real_)
            b_emm <- try(emmeans::emmeans(b_fit, ~ 1), silent = TRUE)
            if (inherits(b_emm, "try-error")) return(NA_real_)
            b_adj_diff <- as.data.frame(b_emm)$emmean[1]
            b_sd_diff <- sd(b_diff)
            if (method == "cohens_dz") {
              return(b_adj_diff / b_sd_diff)
            } else if (method == "hedges_gz") {
              b_correction <- 1 - (3 / (4 * b_n - 9))
              return((b_adj_diff / b_sd_diff) * b_correction)
            } else if (method == "glass_dz") {
              b_s_ref <- if (ref_wave == name1) sd(b_col1) else sd(b_col2)
              return(b_adj_diff / b_s_ref)
            } else if (method == "rank_biserial_paired") {
              b_resids <- residuals(b_fit)
              rb_res <- try(effectsize::rank_biserial(b_resids, rep(0, length(b_resids)), paired = TRUE)[[1]], silent = TRUE)
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
            es_val <- mean(boot_vals)
            alpha <- 1 - ci_level
            q     <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- q[[1]]; ci_hi <- q[[2]]
          }
        }
      } else {
        if (ci_method == "analytical") {
          res <- tryCatch({
            switch(method,
                   cohens_dz            = effectsize::cohens_d(col1, col2, paired = TRUE, ci = ci_level),
                   hedges_gz            = effectsize::hedges_g(col1, col2, paired = TRUE, ci = ci_level),
                   glass_dz             = effectsize::glass_delta(col1, col2, ci = ci_level),
                   rank_biserial_paired = effectsize::rank_biserial(col1, col2, paired = TRUE, ci = ci_level)
            )
          }, error = function(e) NULL)
          if (!is.null(res)) {
            es_val <- as.numeric(res[[1]])
            ci_lo  <- as.numeric(res$CI_low)
            ci_hi  <- as.numeric(res$CI_high)
          }
        } else {
          n <- length(col1)
          boot_fn <- function() {
            idx <- sample(seq_len(n), n, replace = TRUE)
            b1 <- col1[idx]; b2 <- col2[idx]
            res_b <- tryCatch({
              switch(method,
                     cohens_dz            = effectsize::cohens_d(b1, b2, paired = TRUE, ci = ci_level)[[1]],
                     hedges_gz            = effectsize::hedges_g(b1, b2, paired = TRUE, ci = ci_level)[[1]],
                     glass_dz             = effectsize::glass_delta(b1, b2, ci = ci_level)[[1]],
                     rank_biserial_paired = effectsize::rank_biserial(b1, b2, paired = TRUE, ci = ci_level)[[1]]
              )
            }, error = function(e) NA_real_)
            as.numeric(res_b)
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if (length(boot_vals) >= 10) {
            es_val <- mean(boot_vals)
            alpha <- 1 - ci_level
            q     <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- q[[1]]; ci_hi <- q[[2]]
          }
        }
      }
      list(es_value = es_val, ci_lower = ci_lo, ci_upper = ci_hi, method = method, interpretation = private$.interpret(es_val, method))
    },
    .compute_glm = function(iv_col, dv_col, name_a, name_b, dist, ci_method, ci_level, n_bootstrap, cov_col = NULL) {
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
      if (has_cov) {
        cov_sub <- cov_col[mask]
        df_glm <- data.frame(y = dv_sub, x = relevel(iv_sub, ref = name_a), cov = cov_sub)
        fit <- glm(y ~ x + cov, data = df_glm, family = family_fn)
        coef_x <- coef(fit)[2]
        if (dist == "glm_gaussian") { ame <- coef_x } else { ame <- coef_x * mean(dv_sub, na.rm = TRUE) }
        if (ci_method == "analytical") {
          se_coef <- summary(fit)$coefficients[2, "Std. Error"]
          if (dist == "glm_gaussian") { se_ame <- se_coef } else { se_ame <- se_coef * mean(dv_sub, na.rm = TRUE) }
          z <- qnorm(1 - (1 - ci_level) / 2)
          ci_lo <- ame - z * se_ame
          ci_hi <- ame + z * se_ame
        } else {
          boot_fn <- function() {
            idx <- sample(seq_len(n), n, replace = TRUE)
            b_fit <- try(glm(y ~ x + cov, data = df_glm[idx, ], family = family_fn), silent = TRUE)
            if(inherits(b_fit, "try-error")) return(NA)
            if (dist == "glm_gaussian") { coef(b_fit)[2] } else { coef(b_fit)[2] * mean(df_glm$y[idx], na.rm = TRUE) }
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if(length(boot_vals)<10) { ci_lo <- NA; ci_hi <- NA } else {
            alpha <- 1 - ci_level
            q <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- q[[1]]; ci_hi <- q[[2]]
          }
        }
      } else {
        df_glm <- data.frame(y = dv_sub, x = relevel(iv_sub, ref = name_a))
        fit <- glm(y ~ x, data = df_glm, family = family_fn)
        coef_x <- coef(fit)[2]
        if (dist == "glm_gaussian") { ame <- coef_x } else { ame <- coef_x * mean(dv_sub, na.rm = TRUE) }
        if (ci_method == "analytical") {
          se_coef <- summary(fit)$coefficients[2, "Std. Error"]
          if (dist == "glm_gaussian") { se_ame <- se_coef } else { se_ame <- se_coef * mean(dv_sub, na.rm = TRUE) }
          z <- qnorm(1 - (1 - ci_level) / 2)
          ci_lo <- ame - z * se_ame
          ci_hi <- ame + z * se_ame
        } else {
          boot_fn <- function() {
            idx <- sample(seq_len(n), n, replace = TRUE)
            b_fit <- try(glm(y ~ x, data = df_glm[idx, ], family = family_fn), silent = TRUE)
            if(inherits(b_fit, "try-error")) return(NA)
            if (dist == "glm_gaussian") { coef(b_fit)[2] } else { coef(b_fit)[2] * mean(dv_sub[idx], na.rm = TRUE) }
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if(length(boot_vals)<10) { ci_lo <- NA; ci_hi <- NA } else {
            alpha <- 1 - ci_level
            q <- quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- q[[1]]; ci_hi <- q[[2]]
          }
        }
      }
      interp_val <- private$.interpret(abs(ame) / sd(dv_sub, na.rm = TRUE), "cohens_d")
      list(es_value = ame, ci_lower = ci_lo, ci_upper = ci_hi, method = paste0("AME (", dist, ")"), interpretation = interp_val)
    }
  )
)