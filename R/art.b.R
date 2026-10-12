artClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "artClass",
  inherit = artBase,
  private = list(
    
    .is_defined = function(x) {
      !is.null(x) && length(x) > 0 && all(nzchar(x))
    },
    
    .safe_extract = function(df, row_idx, possible_names) {
      for (cn in possible_names) {
        if (cn %in% colnames(df)) {
          val <- df[row_idx, cn]
          if (!is.null(val) && length(val) == 1) {
            return(as.numeric(val))
          }
        }
      }
      return(NA_real_)
    },
    
    .to_numeric_vec = function(x) {
      if (is.numeric(x)) return(x)
      if (is.factor(x)) {
        num_val <- suppressWarnings(as.numeric(as.character(x)))
        if (!all(is.na(num_val))) return(num_val)
        return(as.numeric(x))
      }
      suppressWarnings(as.numeric(x))
    },
    
    .unmangle_term = function(term_str, reverse_map) {
      parts <- strsplit(term_str, ":")[[1]]
      unmangled_parts <- sapply(parts, function(p) {
        if (p %in% names(reverse_map)) reverse_map[[p]] else p
      })
      paste(unmangled_parts, collapse = ":")
    },
    
    .is_contrast_paired = function(contrast_str, term, between_vars, within_vars) {
      if (length(within_vars) == 0) {
        return(FALSE)
      }
      sides <- strsplit(contrast_str, " - ")[[1]]
      if (length(sides) != 2) {
        return(FALSE)
      }
      left_levels <- trimws(strsplit(sides[1], ",")[[1]])
      right_levels <- trimws(strsplit(sides[2], ",")[[1]])
      term_factors <- strsplit(term, ":")[[1]]
      if (length(left_levels) != length(term_factors) || length(right_levels) != length(term_factors)) {
        return(FALSE)
      }
      diff_factors <- term_factors[left_levels != right_levels]
      any_between_diff <- any(diff_factors %in% between_vars)
      any_within_diff <- any(diff_factors %in% within_vars)
      return(any_within_diff && !any_between_diff)
    },
    
    .init = function() {
      info_html <- paste0(
        '<div style=\"font-size:0.9em; line-height:1.6; padding:4px;\">',
        '<h4 style=\"margin-bottom:6px;\">&#128200; Aligned Rank Transform (ART)</h4>',
        '<p style=\"margin-bottom:8px;\">A robust nonparametric approach for analyzing data with multiple factors or repeated measures. ',
        'It aligns the data before ranking, allowing for the accurate evaluation of main effects and interactions.</p>',
        '<ul style=\"margin-top:0px; padding-left:20px;\">',
        '<li><b>Cross-sectional:</b> For independent samples (Factorial Nonparametric ANOVA).</li>',
        '<li><b>Longitudinal / Mixed:</b> For repeated measures, mixed designs, or up to 3 pure within-subject factors (A \u00d7 B \u00d7 C).</li>',
        '</ul>',
        '<p style=\"margin-bottom:4px; font-size:0.85em; color:#555;\"><i>Powered by the R package <b>ARTool</b> (Wobbrock et al., 2011).</i></p>',
        '</div>'
      )
      self$results$analysisInfo$setContent(info_html)
    },
    
    .run = function() {
      data <- self$data
      if (is.null(data)) {
        return()
      }
      
      mode <- self$options$mode
      run_analysis <- self$options$run_analysis
      run_posthoc <- self$options$run_posthoc
      posthoc_correction <- self$options$posthoc_correction
      calc_es <- self$options$calc_es
      es_method <- self$options$es_method
      calc_nonparam_es <- self$options$calc_nonparametric_es
      show_plot <- self$options$show_plot
      
      if (!isTRUE(run_analysis)) {
        self$results$text$setContent("Check 'Run ART Analysis' to compute results.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      if (mode == "cross_sectional") {
        dep_var <- self$options$dep_var_cs
        fixed_factors <- self$options$fixed_factors_cs
        between_vars_opt <- fixed_factors
        within_vars_opt <- character()
        long_format <- "long"
        subject_id_var <- NULL
        time_var <- NULL
        wave_vars <- NULL
      } else {
        dep_var <- self$options$dep_var_long
        between_vars_opt <- if (private$.is_defined(self$options$fixed_factors_long)) self$options$fixed_factors_long else character()
        long_format <- self$options$long_format
        subject_id_var <- self$options$subject_id_var
        time_var <- self$options$time_var
        
        within_vars_opt <- character()
        if (private$.is_defined(self$options$within_factors_long)) {
          within_vars_opt <- c(within_vars_opt, self$options$within_factors_long)
        }
        if (private$.is_defined(time_var) && !(time_var %in% within_vars_opt)) {
          within_vars_opt <- c(within_vars_opt, time_var)
        }
        wave_vars <- self$options$wave_vars
      }
      
      if (mode == "cross_sectional") {
        if (!private$.is_defined(dep_var) || !private$.is_defined(fixed_factors)) {
          self$results$text$setContent("Error: Cross-sectional analysis requires a Dependent Variable and at least one Fixed Factor.")
          self$results$text$setVisible(TRUE)
          return()
        }
      } else {
        if (long_format == "wide") {
          if (!private$.is_defined(wave_vars) || length(wave_vars) < 2 || !private$.is_defined(subject_id_var)) {
            self$results$text$setContent("Error: Wide format requires at least 2 Wave Columns and a Subject ID Variable.")
            self$results$text$setVisible(TRUE)
            return()
          }
        } else if (long_format == "long") {
          if (!private$.is_defined(dep_var) || length(within_vars_opt) == 0 || !private$.is_defined(subject_id_var)) {
            self$results$text$setContent("Error: Long format requires a Dependent Variable, at least one Within-Subject Factor (or Time Variable), and a Subject ID Variable.")
            self$results$text$setVisible(TRUE)
            return()
          }
        }
      }
      
      if (!requireNamespace("ARTool", quietly = TRUE)) {
        self$results$text$setContent("Error: The 'ARTool' package is not installed. Please add it to your DESCRIPTION file.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      if (isTRUE(run_posthoc) && !requireNamespace("emmeans", quietly = TRUE)) {
        self$results$text$setContent("Error: The 'emmeans' package is required for Post-Hoc tests. Please add it to your DESCRIPTION file.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      process_data <- data
      reshape_failed <- FALSE
      if (mode == "longitudinal" && long_format == "wide") {
        process_data <- process_data[!is.na(process_data[[subject_id_var]]), ]
        for (wv in wave_vars) {
          process_data[[wv]] <- private$.to_numeric_vec(process_data[[wv]])
        }
        tryCatch({
          process_data <- stats::reshape(
            process_data,
            varying = wave_vars,
            v.names = ".dv_internal",
            timevar = ".time_internal",
            times = wave_vars,
            direction = "long",
            idvar = subject_id_var
          )
          dep_var <- ".dv_internal"
          within_vars_opt <- ".time_internal"
        }, error = function(e) {
          reshape_failed <<- TRUE
        })
        
        if (reshape_failed) {
          self$results$text$setContent("Error: Reshaping wide data to long format failed. Please check column names and data consistency.")
          self$results$text$setVisible(TRUE)
          return()
        }
      }
      
      predictors <- if (mode == "cross_sectional") fixed_factors else c(between_vars_opt, within_vars_opt)
      predictors <- Filter(Negate(is.null), predictors)
      predictors <- unique(predictors)
      
      vars_in_model <- unique(c(dep_var, predictors, subject_id_var))
      vars_in_model <- Filter(Negate(is.null), vars_in_model)
      
      missing_cols <- setdiff(vars_in_model, names(process_data))
      if (length(missing_cols) > 0) {
        self$results$text$setContent(paste("Error: Missing column(s) in dataset:", paste(missing_cols, collapse = ", ")))
        self$results$text$setVisible(TRUE)
        return()
      }
      
      process_data <- process_data[, vars_in_model, drop = FALSE]
      process_data <- stats::na.omit(process_data)
      
      if (nrow(process_data) == 0) {
        self$results$text$setContent("Error: No complete cases remaining after removing missing values.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      process_data[[dep_var]] <- private$.to_numeric_vec(process_data[[dep_var]])
      if (all(is.na(process_data[[dep_var]]))) {
        self$results$text$setContent("Error: The Dependent Variable cannot be converted to numeric.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      process_data <- droplevels(process_data)
      
      safe_name_map <- setNames(make.names(vars_in_model), vars_in_model)
      reverse_name_map <- setNames(names(safe_name_map), unname(safe_name_map))
      
      for (orig_col in names(safe_name_map)) {
        names(process_data)[names(process_data) == orig_col] <- safe_name_map[[orig_col]]
      }
      
      dep_var_safe <- safe_name_map[[dep_var]]
      predictors_safe <- unname(safe_name_map[predictors])
      subject_id_safe <- if (private$.is_defined(subject_id_var)) safe_name_map[[subject_id_var]] else NULL
      
      for (p in predictors_safe) {
        process_data[[p]] <- as.factor(process_data[[p]])
      }
      
      if (!is.null(subject_id_safe)) {
        process_data[[subject_id_safe]] <- as.factor(process_data[[subject_id_safe]])
      }
      
      main_effects <- paste(predictors_safe, collapse = " * ")
      if (mode == "longitudinal" && !is.null(subject_id_safe)) {
        random_effect <- paste0(" + (1|", subject_id_safe, ")")
      } else {
        random_effect <- ""
      }
      
      formula_str <- paste0(dep_var_safe, " ~ ", main_effects, random_effect)
      art_formula <- as.formula(formula_str)
      
      model_error <- NULL
      model <- tryCatch({
        ARTool::art(art_formula, data = process_data)
      }, error = function(e) {
        model_error <<- e$message
        NULL
      })
      
      if (is.null(model)) {
        self$results$text$setContent(paste("ART model failed. Reason:", model_error, "\nFormula attempted:", formula_str))
        self$results$text$setVisible(TRUE)
        return()
      }
      
      anova_error <- NULL
      anova_res <- tryCatch({
        anova(model)
      }, error = function(e) {
        anova_error <<- e$message
        NULL
      })
      
      if (is.null(anova_res)) {
        self$results$text$setContent(paste("Failed to generate ANOVA table from the ART model. Reason:", anova_error))
        self$results$text$setVisible(TRUE)
        return()
      }
      
      anova_table <- self$results$anovaTable
      if (anova_table$rowCount > 0) anova_table$deleteRows()
      
      anova_table$setNote(
        "citation",
        "Note. Computations powered by ARTool (Wobbrock et al., 2011). Partial \u03b7\u00b2 calculated as (F * df) / (F * df + df.res). Partial \u03c9\u00b2 calculated as df * (F - 1) / (df * (F - 1) + df.res + 1), truncated at 0."
      )
      
      num_rows_anova <- nrow(anova_res)
      if (num_rows_anova == 0) {
        return()
      }
      
      for (i in 1:num_rows_anova) {
        term_internal <- anova_res$Term[i]
        term_display <- private$.unmangle_term(term_internal, reverse_name_map)
        df_num <- private$.safe_extract(anova_res, i, c("Df", "df"))
        df_res <- private$.safe_extract(anova_res, i, c("Df.res", "Res.Df", "Denom"))
        f_val <- private$.safe_extract(anova_res, i, c("F", "F value", "F-value"))
        p_val <- private$.safe_extract(anova_res, i, c("Pr(>F)", "p-value", "p.value", "p"))
        
        pes <- NA_real_
        pos <- NA_real_
        if (!is.na(f_val) && !is.na(df_num) && !is.na(df_res) && df_res > 0) {
          pes <- (f_val * df_num) / ((f_val * df_num) + df_res)
          val_pos <- (df_num * (f_val - 1)) / (df_num * (f_val - 1) + df_res + 1)
          pos <- max(0, val_pos)
        }
        
        anova_table$addRow(rowKey = i, values = list(
          factor        = term_display,
          df            = df_num,
          df_res        = df_res,
          f_value       = f_val,
          p_value       = p_val,
          part_eta_sq   = pes,
          part_omega_sq = pos
        ))
      }
      
      if (isTRUE(run_posthoc)) {
        posthoc_table <- self$results$posthocTable
        if (posthoc_table$rowCount > 0) posthoc_table$deleteRows()
        
        posthoc_table$setNote(
          "citation",
          "Note. Pairwise contrast comparisons computed using the ART-C procedure (Elkin et al., 2021) via ARTool, with p-value adjustments powered by emmeans (Lenth, 2024). Nonparametric effect sizes: Vargha & Delaney's A (Vargha & Delaney, 2000) and Kerby's rank-biserial correlation r_rb (Kerby, 2014; r_rb = 2A - 1)."
        )
        
        es_table <- self$results$effectSizeTable
        if (es_table$rowCount > 0) es_table$deleteRows()
        
        es_table$setNote(
          "citation",
          "Note. Pairwise effect sizes: Cohen's d / Hedges' g approximated from t-ratios: d = 2*t/sqrt(df) for unpaired contrasts, and d = t/sqrt(df) for paired/within-subjects contrasts. Vargha-Delaney A represents the probability of stochastic superiority; r_rb is the rank-biserial correlation (Kerby, 2014)."
        )
        
        terms_to_test <- anova_res$Term[1:num_rows_anova]
        row_ph <- 1
        row_es <- 1
        
        between_vars_safe <- unname(safe_name_map[between_vars_opt])
        within_vars_safe <- unname(safe_name_map[within_vars_opt])
        
        adjust_arg <- switch(
          posthoc_correction,
          "holm"       = "holm",
          "bonferroni" = "bonferroni",
          "tukey"      = "tukey",
          "fdr"        = "fdr",
          "none"       = "none",
          "holm"
        )
        
        for (term in terms_to_test) {
          term_display <- private$.unmangle_term(term, reverse_name_map)
          ph_res <- tryCatch({
            ARTool::art.con(model, term, adjust = adjust_arg)
          }, error = function(e) {
            NULL
          })
          
          if (is.null(ph_res)) {
            next
          }
          
          ph_df <- as.data.frame(ph_res)
          col_contrast <- names(ph_df)[1]
          
          for (j in seq_len(nrow(ph_df))) {
            comp <- as.character(ph_df[j, col_contrast])
            est <- private$.safe_extract(ph_df, j, c("estimate", "Estimate"))
            se <- private$.safe_extract(ph_df, j, c("SE", "se", "Std. Error"))
            df <- private$.safe_extract(ph_df, j, c("df", "Df"))
            t_r <- private$.safe_extract(ph_df, j, c("t.ratio", "z.ratio", "t value", "z value"))
            p_v <- private$.safe_extract(ph_df, j, c("p.value", "Pr(>|t|)", "Pr(>|z|)"))
            
            # Nonparametric effect sizes: Vargha-Delaney A and Rank-Biserial r_rb
            a_val <- NA_real_
            r_rb_val <- NA_real_
            sides <- strsplit(comp, " - ")[[1]]
            is_paired <- private$.is_contrast_paired(comp, term, between_vars_safe, within_vars_safe)
            
            if (length(sides) == 2) {
              g1_levels <- trimws(strsplit(sides[1], ",")[[1]])
              g2_levels <- trimws(strsplit(sides[2], ",")[[1]])
              term_safe_factors <- strsplit(term, ":")[[1]]
              
              if (length(g1_levels) == length(term_safe_factors) && length(g2_levels) == length(term_safe_factors)) {
                idx1 <- rep(TRUE, nrow(process_data))
                idx2 <- rep(TRUE, nrow(process_data))
                for (f_idx in seq_along(term_safe_factors)) {
                  f_col <- term_safe_factors[f_idx]
                  idx1 <- idx1 & (as.character(process_data[[f_col]]) == g1_levels[f_idx])
                  idx2 <- idx2 & (as.character(process_data[[f_col]]) == g2_levels[f_idx])
                }
                y1 <- process_data[[dep_var_safe]][idx1]
                y2 <- process_data[[dep_var_safe]][idx2]
                
                if (is_paired && !is.null(subject_id_safe)) {
                  s1 <- process_data[[subject_id_safe]][idx1]
                  s2 <- process_data[[subject_id_safe]][idx2]
                  m_sub <- intersect(s1, s2)
                  if (length(m_sub) >= 2) {
                    y1_paired <- y1[match(m_sub, s1)]
                    y2_paired <- y2[match(m_sub, s2)]
                    diffs <- y1_paired - y2_paired
                    diffs <- diffs[diffs != 0]
                    if (length(diffs) > 0) {
                      r_diff <- rank(abs(diffs))
                      w_pos <- sum(r_diff[diffs > 0])
                      w_neg <- sum(r_diff[diffs < 0])
                      w_tot <- w_pos + w_neg
                      if (w_tot > 0) {
                        r_rb_val <- (w_pos - w_neg) / w_tot
                        a_val <- (r_rb_val + 1) / 2
                      }
                    }
                  }
                } else {
                  if (length(y1) > 0 && length(y2) > 0) {
                    comp_mat <- outer(y1, y2, function(a, b) ifelse(a > b, 1, ifelse(a == b, 0.5, 0)))
                    a_val <- mean(comp_mat)
                    r_rb_val <- 2 * a_val - 1
                  }
                }
              }
            }
            
            # Analytical fallback if empirical bootstrap did not converge
            if (is.na(a_val) && !is.na(t_r) && !is.na(df) && df > 0) {
              scale_fac <- if (is_paired) 1 else 2
              d_approx <- (scale_fac * t_r) / sqrt(df)
              a_val <- stats::pnorm(d_approx / sqrt(2))
              r_rb_val <- 2 * a_val - 1
            }
            
            posthoc_table$addRow(rowKey = row_ph, values = list(
              factor   = term_display,
              contrast = comp,
              estimate = est,
              se       = se,
              df       = df,
              t_ratio  = t_r,
              p_value  = p_v,
              vd_a     = a_val,
              r_rb     = r_rb_val
            ))
            row_ph <- row_ph + 1
            
            if (isTRUE(calc_es)) {
              scale_factor <- if (is_paired) 1 else 2
              d_val <- if (!is.na(t_r) && !is.na(df) && df > 0) (scale_factor * t_r) / sqrt(df) else NA_real_
              j_corr <- if (!is.na(df) && df > 1) (1 - 3 / (4 * df - 1)) else 1
              g_val <- if (!is.na(d_val)) d_val * j_corr else NA_real_
              
              add_es_row <- function(m_lbl, val) {
                es_table$addRow(rowKey = row_es, values = list(
                  factor = term_display,
                  contrast = comp,
                  method = m_lbl,
                  es_value = val
                ))
                row_es <<- row_es + 1
              }
              
              if (es_method == "cohens_d") {
                add_es_row("Cohen's d", d_val)
              } else if (es_method == "hedges_g") {
                add_es_row("Hedges' g", g_val)
              } else if (es_method == "vargha_delaney_a") {
                add_es_row("Vargha & Delaney's A", a_val)
              } else if (es_method == "rank_biserial") {
                add_es_row("Rank-Biserial r_rb", r_rb_val)
              } else if (es_method == "all") {
                add_es_row("Cohen's d", d_val)
                add_es_row("Hedges' g", g_val)
                add_es_row("Vargha & Delaney's A", a_val)
                add_es_row("Rank-Biserial r_rb", r_rb_val)
              }
            }
          }
        }
      }
      
      if (isTRUE(show_plot)) {
        image <- self$results$artPlot
        plot_df <- process_data
        for (cn in names(safe_name_map)) {
          names(plot_df)[names(plot_df) == safe_name_map[[cn]]] <- cn
        }
        image$setState(list(
          data = plot_df,
          dep_var = dep_var,
          predictors = predictors
        ))
      }
    },
    
    .renderArtPlot = function(image, ggtheme, ...) {
      state <- image$state
      if (is.null(state)) return(FALSE)
      
      df <- state$data
      dep_var <- state$dep_var
      predictors <- state$predictors
      
      if (length(predictors) == 0) return(FALSE)
      
      p1 <- predictors[1]
      p2 <- if (length(predictors) >= 2) predictors[2] else NULL
      p3 <- if (length(predictors) >= 3) predictors[3] else NULL
      
      if (is.null(p2)) {
        p <- ggplot2::ggplot(df, ggplot2::aes_string(x = p1, y = dep_var)) +
          ggplot2::stat_summary(fun = mean, geom = "point", size = 3, color = "#2c3e50") +
          ggplot2::stat_summary(fun.data = ggplot2::mean_cl_boot, geom = "errorbar", width = 0.2, color = "#2c3e50") +
          ggplot2::labs(title = paste("Marginal Means of", dep_var, "by", p1)) +
          ggtheme
      } else {
        p <- ggplot2::ggplot(df, ggplot2::aes_string(x = p1, y = dep_var, color = p2, group = p2)) +
          ggplot2::stat_summary(fun = mean, geom = "point", size = 3) +
          ggplot2::stat_summary(fun = mean, geom = "line", linewidth = 0.8) +
          ggplot2::stat_summary(fun.data = ggplot2::mean_cl_boot, geom = "errorbar", width = 0.2) +
          ggplot2::labs(title = paste("Interaction of", p1, "and", p2, "on", dep_var)) +
          ggtheme
        
        if (!is.null(p3)) {
          p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", p3)))
        }
      }
      
      print(p)
      TRUE
    }
  )
)

