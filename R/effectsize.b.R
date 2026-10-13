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
    
    .winsorize = function(x, trim = 0.20) {
      ql <- stats::quantile(x, trim, na.rm = TRUE)
      qu <- stats::quantile(x, 1 - trim, na.rm = TRUE)
      pmin(pmax(x, ql), qu)
    },
    
    .init = function() {
      info_html <- paste0(
        '<div style="font-size:0.9em; line-height:1.6; padding:4px;">',
        '<h4 style="margin-bottom:6px;">&#128200; Pairwise Effect Size & Test Statistic Calculator</h4>',
        '<p style="margin-bottom:8px;">Computes standardized, robust, and categorical effect sizes with analytical and bootstrap confidence intervals. ',
        'Includes direct converter from test statistics (t, F, z, &chi;&sup2;).</p>',
        '<ul style="margin-top:0px; padding-left:20px;">',
        '<li><b>Independent Samples:</b> Cohen\'s d, Hedges\' g, Glass\' &Delta;, Rank Biserial, Algina-Keselman-Penfield d_R, and Akritas-Arnold &delta;&#770;.</li>',
        '<li><b>Longitudinal / Paired:</b> Cohen\'s dz, Hedges\' gz, Glass\' &Delta;z, Paired Rank Biserial.</li>',
        '<li><b>Categorical:</b> Cram&eacute;r\'s V, Odds Ratio (OR), and Relative Risk (RR).</li>',
        '<li><b>Direct Calculator:</b> Convert directly from test statistics without raw data.</li>',
        '</ul>',
        '<hr style="margin:8px 0;"/>',
        '<p><b>&#9654; Interpretation Benchmarks</b></p>',
        '<ul style="margin-top:0px; padding-left:20px;">',
        '<li><b>Cohen (1988):</b> &lt; 0.2 (Negligible) | &ge; 0.2 (Small) | &ge; 0.5 (Medium) | &ge; 0.8 (Large)</li>',
        '<li><b>Sawilowsky (2009):</b> &lt; 0.01 (Negligible) | 0.2 (Very small) | 0.5 (Small) | 0.8 (Medium) | 1.2 (Large) | 2.0 (Very large) | &ge; 2.0 (Huge)</li>',
        '<li><b>Rank Biserial & Relative Effects:</b> &lt; 0.1 (Negligible) | &ge; 0.1 (Small) | &ge; 0.3 (Medium) | &ge; 0.5 (Large)</li>',
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
      benchmark_std        <- if (!is.null(self$options$benchmark_standard)) self$options$benchmark_standard else "cohen"
      calc_from_test_stat  <- isTRUE(self$options$calc_from_test_stat)
      show_categorical_es  <- isTRUE(self$options$show_categorical_es)
      ci_level_raw         <- if (!is.null(self$options$ci_level)) self$options$ci_level else "ci_95"
      ci_level             <- switch(ci_level_raw, ci_90 = 0.90, ci_99 = 0.99, 0.95)
      ci_method            <- if (!is.null(self$options$ci_method)) self$options$ci_method else "analytical"
      n_bootstrap          <- if (!is.null(self$options$n_bootstrap)) self$options$n_bootstrap else 1000
      
      if (calc_from_test_stat) {
        dt_table <- self$results$directCalcTable
        if (dt_table$rowCount > 0) dt_table$deleteRows()
        
        stat_type <- if (!is.null(self$options$stat_type)) self$options$stat_type else "stat_t"
        s_val <- if (!is.null(self$options$stat_val)) self$options$stat_val else 2.5
        df1 <- if (!is.null(self$options$stat_df1)) self$options$stat_df1 else 30
        df2 <- if (!is.null(self$options$stat_df2)) self$options$stat_df2 else 60
        n_sample <- if (!is.null(self$options$stat_n) && self$options$stat_n > 0) self$options$stat_n else (df1 + 2)
        
        dt_table$setNote(
          "direct_note",
          "Note. Instant effect size conversions computed from user-specified test statistics without raw data. Citations: Cohen (1988); Borenstein et al. (2009)."
        )
        
        row_dt <- 1
        if (stat_type == "stat_t") {
          t_stat <- s_val
          d_val <- (2 * t_stat) / sqrt(df1)
          g_val <- if (df1 > 1) d_val * (1 - 3 / (4 * df1 - 1)) else d_val
          r_val <- t_stat / sqrt(t_stat^2 + df1)
          eta2_val <- t_stat^2 / (t_stat^2 + df1)
          or_val <- exp(d_val * pi / sqrt(3))
          auc_val <- stats::pnorm(d_val / sqrt(2))
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("t(", df1, ") = ", round(t_stat, 3)),
            metric = "Cohen's d",
            value = d_val,
            interpretation = private$.interpret(d_val, "cohens_d", benchmark_std),
            formula_ref = "d = 2*t / sqrt(df)"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("t(", df1, ") = ", round(t_stat, 3)),
            metric = "Hedges' g",
            value = g_val,
            interpretation = private$.interpret(g_val, "hedges_g", benchmark_std),
            formula_ref = "g = d * (1 - 3/(4*df - 1))"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("t(", df1, ") = ", round(t_stat, 3)),
            metric = "Point-Biserial r",
            value = r_val,
            interpretation = private$.interpret(r_val, "rank_biserial", benchmark_std),
            formula_ref = "r = t / sqrt(t^2 + df)"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("t(", df1, ") = ", round(t_stat, 3)),
            metric = "Eta-squared (\u03b7\u00b2)",
            value = eta2_val,
            interpretation = private$.interpret(d_val, "cohens_d", benchmark_std),
            formula_ref = "\u03b7\u00b2 = t^2 / (t^2 + df)"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("t(", df1, ") = ", round(t_stat, 3)),
            metric = "Odds Ratio (OR)",
            value = or_val,
            interpretation = private$.interpret_ratio(or_val),
            formula_ref = "OR = exp(d * \u03c0 / sqrt(3))"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("t(", df1, ") = ", round(t_stat, 3)),
            metric = "AUC / Prob. Superiority",
            value = auc_val,
            interpretation = paste0(round(auc_val * 100, 1), "% chance of superiority"),
            formula_ref = "AUC = \u03a6(d / sqrt(2))"
          )); row_dt <- row_dt + 1
          
        } else if (stat_type == "stat_f") {
          f_stat <- s_val
          eta_p <- (f_stat * df1) / (f_stat * df1 + df2)
          omega_p <- max(0, (df1 * (f_stat - 1)) / (df1 * (f_stat - 1) + df2 + 1))
          f_cohen <- sqrt(eta_p / max(1e-6, 1 - eta_p))
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("F(", df1, ", ", df2, ") = ", round(f_stat, 3)),
            metric = "Partial \u03b7\u00b2",
            value = eta_p,
            interpretation = private$.interpret_eta(eta_p),
            formula_ref = "\u03b7_p\u00b2 = (F*df1) / (F*df1 + df2)"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("F(", df1, ", ", df2, ") = ", round(f_stat, 3)),
            metric = "Partial \u03c9\u00b2",
            value = omega_p,
            interpretation = private$.interpret_eta(omega_p),
            formula_ref = "\u03c9_p\u00b2 = max(0, df1*(F-1) / (df1*(F-1) + df2 + 1))"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("F(", df1, ", ", df2, ") = ", round(f_stat, 3)),
            metric = "Cohen's f",
            value = f_cohen,
            interpretation = private$.interpret_f(f_cohen),
            formula_ref = "f = sqrt(\u03b7_p\u00b2 / (1 - \u03b7_p\u00b2))"
          )); row_dt <- row_dt + 1
          
          if (df1 == 1) {
            d_from_f <- 2 * f_cohen
            dt_table$addRow(rowKey = row_dt, values = list(
              input_stat = paste0("F(1, ", df2, ") = ", round(f_stat, 3)),
              metric = "Equivalent Cohen's d (df1=1)",
              value = d_from_f,
              interpretation = private$.interpret(d_from_f, "cohens_d", benchmark_std),
              formula_ref = "d = 2 * f"
            )); row_dt <- row_dt + 1
          }
          
        } else if (stat_type == "stat_z") {
          z_stat <- s_val
          d_z <- (2 * z_stat) / sqrt(n_sample)
          r_z <- z_stat / sqrt(n_sample)
          auc_z <- stats::pnorm(z_stat / sqrt(2))
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("z = ", round(z_stat, 3), " (N = ", n_sample, ")"),
            metric = "Cohen's d equivalent",
            value = d_z,
            interpretation = private$.interpret(d_z, "cohens_d", benchmark_std),
            formula_ref = "d = 2*z / sqrt(N)"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("z = ", round(z_stat, 3), " (N = ", n_sample, ")"),
            metric = "Correlation r",
            value = r_z,
            interpretation = private$.interpret(r_z, "rank_biserial", benchmark_std),
            formula_ref = "r = z / sqrt(N)"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("z = ", round(z_stat, 3)),
            metric = "AUC / Prob. Superiority",
            value = auc_z,
            interpretation = paste0(round(auc_z * 100, 1), "% chance of superiority"),
            formula_ref = "AUC = \u03a6(z / sqrt(2))"
          )); row_dt <- row_dt + 1
          
        } else if (stat_type == "stat_chisq") {
          chisq_val <- s_val
          phi_val <- sqrt(chisq_val / n_sample)
          k_factor <- max(1, df1)
          cramer_val <- sqrt(chisq_val / (n_sample * k_factor))
          or_approx <- exp(sqrt(chisq_val) * pi / sqrt(3 * n_sample))
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("\u03c7\u00b2(", df1, ") = ", round(chisq_val, 3), " (N = ", n_sample, ")"),
            metric = "Cram\u00e9r's V",
            value = cramer_val,
            interpretation = private$.interpret_cramer(cramer_val),
            formula_ref = "V = sqrt(\u03c7\u00b2 / (N * min(r-1, c-1)))"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("\u03c7\u00b2 = ", round(chisq_val, 3), " (N = ", n_sample, ")"),
            metric = "Phi (\u03c6)",
            value = phi_val,
            interpretation = private$.interpret_cramer(phi_val),
            formula_ref = "\u03c6 = sqrt(\u03c7\u00b2 / N)"
          )); row_dt <- row_dt + 1
          
          dt_table$addRow(rowKey = row_dt, values = list(
            input_stat = paste0("\u03c7\u00b2 = ", round(chisq_val, 3), " (N = ", n_sample, ")"),
            metric = "Odds Ratio approx. (2\u00d72)",
            value = or_approx,
            interpretation = private$.interpret_ratio(or_approx),
            formula_ref = "OR \u2248 exp(sqrt(\u03c7\u00b2) * \u03c0 / sqrt(3*N))"
          )); row_dt <- row_dt + 1
        }
      }
      
      data <- self$data
      
      if (show_categorical_es && !is.null(data)) {
        cat_tbl <- self$results$categoricalTable
        if (cat_tbl$rowCount > 0) cat_tbl$deleteRows()
        
        cat1 <- self$options$cat_var1
        cat2 <- self$options$cat_var2
        
        if (private$.is_defined(cat1) && private$.is_defined(cat2) &&
            cat1 %in% names(data) && cat2 %in% names(data)) {
          
          cat_tbl$setNote(
            "cat_note",
            "Note. Cram\u00e9r's V computed following Cram\u00e9r (1946). Odds Ratio (OR) and Relative Risk (RR) with Wald asymptotic confidence intervals following Fleiss et al. (2003)."
          )
          
          v1 <- factor(data[[cat1]])
          v2 <- factor(data[[cat2]])
          valid <- !is.na(v1) & !is.na(v2)
          v1 <- droplevels(v1[valid])
          v2 <- droplevels(v2[valid])
          n_cat <- length(v1)
          
          if (n_cat >= 5 && nlevels(v1) >= 2 && nlevels(v2) >= 2) {
            tab <- table(v1, v2)
            chi_res <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
            chi_stat <- as.numeric(chi_res$statistic)
            r_dim <- nrow(tab)
            c_dim <- ncol(tab)
            k_min <- min(r_dim - 1, c_dim - 1)
            v_est <- sqrt(chi_stat / (n_cat * max(1, k_min)))
            
            z_crit <- stats::qnorm(1 - (1 - ci_level) / 2)
            se_v <- sqrt((1 - v_est^2) / n_cat)
            ci_lo_v <- max(0, v_est - z_crit * se_v)
            ci_hi_v <- min(1, v_est + z_crit * se_v)
            
            row_c <- 1
            cat_tbl$addRow(rowKey = row_c, values = list(
              pair = paste(cat1, "\u00d7", cat2),
              metric = "Cram\u00e9r's V",
              value = v_est,
              ci_lower = ci_lo_v,
              ci_upper = ci_hi_v,
              interpretation = private$.interpret_cramer(v_est),
              n_obs = n_cat
            )); row_c <- row_c + 1
            
            if (r_dim == 2 && c_dim == 2) {
              a <- tab[1, 1]; b <- tab[1, 2]
              c_cell <- tab[2, 1]; d <- tab[2, 2]
              
              if (any(c(a, b, c_cell, d) == 0)) {
                a <- a + 0.5; b <- b + 0.5
                c_cell <- c_cell + 0.5; d <- d + 0.5
              }
              
              or_val <- (a * d) / (b * c_cell)
              se_ln_or <- sqrt(1 / a + 1 / b + 1 / c_cell + 1 / d)
              ci_lo_or <- exp(log(or_val) - z_crit * se_ln_or)
              ci_hi_or <- exp(log(or_val) + z_crit * se_ln_or)
              
              cat_tbl$addRow(rowKey = row_c, values = list(
                pair = paste(cat1, "\u00d7", cat2),
                metric = "Odds Ratio (OR)",
                value = or_val,
                ci_lower = ci_lo_or,
                ci_upper = ci_hi_or,
                interpretation = private$.interpret_ratio(or_val),
                n_obs = n_cat
              )); row_c <- row_c + 1
              
              p1 <- a / (a + b)
              p2 <- c_cell / (c_cell + d)
              rr_val <- if (p2 > 0) p1 / p2 else NA_real_
              se_ln_rr <- sqrt(1 / a - 1 / (a + b) + 1 / c_cell - 1 / (c_cell + d))
              ci_lo_rr <- exp(log(rr_val) - z_crit * se_ln_rr)
              ci_hi_rr <- exp(log(rr_val) + z_crit * se_ln_rr)
              
              cat_tbl$addRow(rowKey = row_c, values = list(
                pair = paste(cat1, "\u00d7", cat2),
                metric = "Relative Risk (RR)",
                value = rr_val,
                ci_lower = ci_lo_rr,
                ci_upper = ci_hi_rr,
                interpretation = private$.interpret_ratio(rr_val),
                n_obs = n_cat
              )); row_c <- row_c + 1
            }
          }
        }
      }
      
      if (is.null(data)) {
        if (calc_from_test_stat) {
          self$results$text$setContent("Direct test statistic calculations computed successfully without raw data.")
          self$results$text$setVisible(TRUE)
        }
        return()
      }
      
      mode                 <- self$options$mode
      run_analysis         <- self$options$run_analysis
      include_interactions <- self$options$include_interactions
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
      table$setNote("engine", "Note. Computations powered by effectsize and robust algorithms. Robust d_R follows Algina et al. (2005) with 20% trimmed means and Winsorized variances. Akritas-Arnold relative effect \u03b4\u0302 follows Akritas & Arnold (1994).")
      
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
      
      use_glm          <- self$options$use_glm
      es_glm_dist      <- self$options$es_glm
      reference_group  <- self$options$reference_group
      reference_group_long <- self$options$reference_group_long
      
      if (table$rowCount > 0) table$deleteRows()
      clesTable <- if (mode == "cross_sectional") self$results$clesTable else self$results$clesLongTable
      if (clesTable$rowCount > 0) clesTable$deleteRows()
      
      table_long_between <- self$results$effectSizeTableLongBetween
      if (table_long_between$rowCount > 0) table_long_between$deleteRows()
      
      conversion_tbl <- self$results$conversionTable
      if (conversion_tbl$rowCount > 0) conversion_tbl$deleteRows()
      
      if (!isTRUE(run_analysis)) {
        if (!calc_from_test_stat && !show_categorical_es) {
          self$results$text$setContent("Check 'Run Analysis (Compute Results)' to begin.")
          self$results$text$setVisible(TRUE)
        }
        return()
      }
      
      has_cov <- private$.is_defined(cov_var) && (cov_var %in% names(data))
      
      n_rows <- 0
      n_cles <- 0
      n_rows_long_between <- 0
      n_cles_long <- 0
      n_conv <- 0
      
      plot_entries <- list()
      grouping_factors <- list()
      if (private$.is_defined(indep_vars)) {
        for (iv in indep_vars) {
          if (iv %in% names(data)) grouping_factors[[iv]] <- factor(data[[iv]])
        }
        if (isTRUE(include_interactions) && length(indep_vars) > 1) {
          valid_ivs <- intersect(indep_vars, names(data))
          if (length(valid_ivs) > 1) {
            int_name <- paste(valid_ivs, collapse = " * ")
            grouping_factors[[int_name]] <- interaction(data[valid_ivs], drop = TRUE, sep = " * ")
          }
        }
      }
      
      if (mode == "longitudinal" && length(grouping_factors) == 0) {
        table_long_between$setVisible(FALSE)
      } else if (mode == "longitudinal") {
        table_long_between$setNote("engine", "Note. Computations powered by effectsize and robust algorithms. Variance is pooled locally between the compared pairs.")
      }
      
      if (mode == "cross_sectional") {
        cov_col_full <- if (has_cov) private$.to_numeric_vec(data[[cov_var]]) else NULL
        for (iv_name in names(grouping_factors)) {
          iv_col_raw <- grouping_factors[[iv_name]]
          for (dv in dep_vars) {
            if (iv_name == dv || !(dv %in% names(data))) next
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
              
              if (is.finite(result$es_value)) {
                plot_entries[[length(plot_entries) + 1]] <- list(
                  label    = paste0(dv, ": ", comp_str),
                  estimate = result$es_value,
                  ci_lower = result$ci_lower,
                  ci_upper = result$ci_upper
                )
              }
              
              if (isTRUE(show_conversions) && !is.na(result$es_value)) {
                convs <- private$.convert_metrics(result$es_value, es_method, benchmark_std)
                n_conv <- n_conv + 1
                conversion_tbl$addRow(rowKey = n_conv, values = list(
                  dv             = dv,
                  comparison     = paste0(iv_name, " (", comp_str, ")"),
                  cohen_d        = convs$cohen_d,
                  correlation_r  = convs$correlation_r,
                  eta_squared    = convs$eta_squared,
                  odds_ratio     = convs$odds_ratio,
                  auc            = convs$auc,
                  interpretation = convs$interpretation
                ))
              }
              
              if (isTRUE(show_cles)) {
                cles_result <- private$.compute_cles_independent(ga, gb)
                n_cles <- n_cles + 1
                clesTable$addRow(rowKey = n_cles, values = list(
                  dv         = dv,
                  iv         = iv_name,
                  comparison = comp_str,
                  cles       = cles_result$cles
                ))
              }
            }
          }
        }
      } else {
        if (long_format == "wide") {
          if (private$.is_defined(wave_vars) && length(wave_vars) >= 2) {
            wave_pairs <- combn(wave_vars, 2, simplify = FALSE)
            for (wp in wave_pairs) {
              w1 <- wp[[1]]; w2 <- wp[[2]]
              if (!(w1 %in% names(data)) || !(w2 %in% names(data))) next
              c1 <- private$.to_numeric_vec(data[[w1]])
              c2 <- private$.to_numeric_vec(data[[w2]])
              valid <- !is.na(c1) & !is.na(c2)
              c1 <- c1[valid]; c2 <- c2[valid]
              n_obs <- length(c1)
              if (n_obs < 3) next
              
              result <- tryCatch({
                private$.compute_dependent_paired(c1, c2, w1, w2, w1, es_method_long, ci_method, ci_level, n_bootstrap, cov_col = NULL, benchmark = benchmark_std)
              }, error = function(e) {
                list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = es_method_long, interpretation = paste0("Error: ", e$message))
              })
              
              n_rows <- n_rows + 1
              comp_str <- paste0(w1, " vs ", w2)
              table$addRow(rowKey = n_rows, values = list(
                dv             = "Repeated Measures",
                iv             = "Time Waves",
                comparison     = comp_str,
                method         = result$method,
                es_value       = result$es_value,
                ci_lower       = result$ci_lower,
                ci_upper       = result$ci_upper,
                interpretation = result$interpretation,
                n_obs          = n_obs
              ))
              
              if (is.finite(result$es_value)) {
                plot_entries[[length(plot_entries) + 1]] <- list(
                  label    = comp_str,
                  estimate = result$es_value,
                  ci_lower = result$ci_lower,
                  ci_upper = result$ci_upper
                )
              }
              
              if (isTRUE(show_conversions) && !is.na(result$es_value)) {
                convs <- private$.convert_metrics(result$es_value, es_method_long, benchmark_std)
                n_conv <- n_conv + 1
                conversion_tbl$addRow(rowKey = n_conv, values = list(
                  dv             = "Wide Wave",
                  comparison     = comp_str,
                  cohen_d        = convs$cohen_d,
                  correlation_r  = convs$correlation_r,
                  eta_squared    = convs$eta_squared,
                  odds_ratio     = convs$odds_ratio,
                  auc            = convs$auc,
                  interpretation = convs$interpretation
                ))
              }
              
              if (isTRUE(show_cles_long)) {
                cles_result <- private$.compute_cles_paired(c1, c2)
                n_cles_long <- n_cles_long + 1
                clesTable$addRow(rowKey = n_cles_long, values = list(
                  dv         = "Wide Wave",
                  comparison = comp_str,
                  cles       = cles_result$cles
                ))
              }
            }
          }
        } else if (long_format == "long") {
          if (private$.is_defined(dep_vars) && private$.is_defined(time_var) && private$.is_defined(subject_id_var)) {
            if (time_var %in% names(data) && subject_id_var %in% names(data)) {
              time_lvls <- sort(unique(stats::na.omit(as.character(data[[time_var]]))))
              if (length(time_lvls) >= 2) {
                t_pairs <- combn(time_lvls, 2, simplify = FALSE)
                for (dv in dep_vars) {
                  if (!(dv %in% names(data))) next
                  for (tp in t_pairs) {
                    t1 <- tp[[1]]; t2 <- tp[[2]]
                    d1 <- data[as.character(data[[time_var]]) == t1 & !is.na(data[[time_var]]), c(subject_id_var, dv)]
                    d2 <- data[as.character(data[[time_var]]) == t2 & !is.na(data[[time_var]]), c(subject_id_var, dv)]
                    m_df <- merge(d1, d2, by = subject_id_var, suffixes = c("_1", "_2"))
                    c1 <- private$.to_numeric_vec(m_df[[paste0(dv, "_1")]])
                    c2 <- private$.to_numeric_vec(m_df[[paste0(dv, "_2")]])
                    valid <- !is.na(c1) & !is.na(c2)
                    c1 <- c1[valid]; c2 <- c2[valid]
                    n_obs <- length(c1)
                    if (n_obs < 3) next
                    result <- tryCatch({
                      private$.compute_dependent_paired(c1, c2, t1, t2, t1, es_method_long, ci_method, ci_level, n_bootstrap, cov_col = NULL, benchmark = benchmark_std)
                    }, error = function(e) {
                      list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = es_method_long, interpretation = paste0("Error: ", e$message))
                    })
                    n_rows <- n_rows + 1
                    comp_str <- paste0(t1, " vs ", t2)
                    table$addRow(rowKey = n_rows, values = list(
                      dv             = dv,
                      iv             = time_var,
                      comparison     = comp_str,
                      method         = result$method,
                      es_value       = result$es_value,
                      ci_lower       = result$ci_lower,
                      ci_upper       = result$ci_upper,
                      interpretation = result$interpretation,
                      n_obs          = n_obs
                    ))
                    if (is.finite(result$es_value)) {
                      plot_entries[[length(plot_entries) + 1]] <- list(
                        label    = paste0(dv, ": ", comp_str),
                        estimate = result$es_value,
                        ci_lower = result$ci_lower,
                        ci_upper = result$ci_upper
                      )
                    }
                    if (isTRUE(show_conversions) && !is.na(result$es_value)) {
                      convs <- private$.convert_metrics(result$es_value, es_method_long, benchmark_std)
                      n_conv <- n_conv + 1
                      conversion_tbl$addRow(rowKey = n_conv, values = list(
                        dv             = dv,
                        comparison     = comp_str,
                        cohen_d        = convs$cohen_d,
                        correlation_r  = convs$correlation_r,
                        eta_squared    = convs$eta_squared,
                        odds_ratio     = convs$odds_ratio,
                        auc            = convs$auc,
                        interpretation = convs$interpretation
                      ))
                    }
                    if (isTRUE(show_cles_long)) {
                      cles_result <- private$.compute_cles_paired(c1, c2)
                      n_cles_long <- n_cles_long + 1
                      clesTable$addRow(rowKey = n_cles_long, values = list(
                        dv         = dv,
                        comparison = comp_str,
                        cles       = cles_result$cles
                      ))
                    }
                  }
                }
              }
            }
          }
        }
      }
      
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
      
      self$results$text$setContent(paste0(
        (n_rows + n_rows_long_between), " pairwise effect size(s) computed."
      ))
      self$results$text$setVisible(TRUE)
    },
    
    .interpret = function(val, method, benchmark = "cohen") {
      if (is.null(val) || is.na(val) || !is.finite(val)) return("Inconclusive")
      abs_val <- abs(val)
      
      if (method %in% c("rank_biserial", "rank_biserial_paired", "akritas_arnold_delta")) {
        if (abs_val < 0.1) return("Negligible (< 0.1)")
        if (abs_val < 0.3) return("Small (0.1 - 0.3)")
        if (abs_val < 0.5) return("Medium (0.3 - 0.5)")
        return("Large (\u2265 0.5)")
      }
      
      if (benchmark == "sawilowsky") {
        if (abs_val < 0.01) return("Negligible (< 0.01)")
        if (abs_val < 0.20) return("Very small (0.01 - 0.2)")
        if (abs_val < 0.50) return("Small (0.2 - 0.5)")
        if (abs_val < 0.80) return("Medium (0.5 - 0.8)")
        if (abs_val < 1.20) return("Large (0.8 - 1.2)")
        if (abs_val < 2.00) return("Very large (1.2 - 2.0)")
        return("Huge (\u2265 2.0)")
      }
      
      if (abs_val < 0.2) return("Negligible (< 0.2)")
      if (abs_val < 0.5) return("Small (0.2 - 0.5)")
      if (abs_val < 0.8) return("Medium (0.5 - 0.8)")
      return("Large (\u2265 0.8)")
    },
    
    .interpret_ratio = function(r) {
      if (is.null(r) || is.na(r) || !is.finite(r)) return("Inconclusive")
      eff <- if (r < 1 && r > 0) 1 / r else r
      if (eff < 1.5) return("Small / Negligible (< 1.5)")
      if (eff < 3.5) return("Medium (1.5 - 3.5)")
      return("Large (\u2265 3.5)")
    },
    
    .interpret_eta = function(eta) {
      if (is.null(eta) || is.na(eta) || !is.finite(eta)) return("Inconclusive")
      if (eta < 0.01) return("Negligible (< 0.01)")
      if (eta < 0.06) return("Small (0.01 - 0.06)")
      if (eta < 0.14) return("Medium (0.06 - 0.14)")
      return("Large (\u2265 0.14)")
    },
    
    .interpret_f = function(f) {
      if (is.null(f) || is.na(f) || !is.finite(f)) return("Inconclusive")
      if (f < 0.10) return("Negligible (< 0.10)")
      if (f < 0.25) return("Small (0.10 - 0.25)")
      if (f < 0.40) return("Medium (0.25 - 0.40)")
      return("Large (\u2265 0.40)")
    },
    
    .interpret_cramer = function(v) {
      if (is.null(v) || is.na(v) || !is.finite(v)) return("Inconclusive")
      if (v < 0.10) return("Negligible (< 0.10)")
      if (v < 0.30) return("Small / Weak (0.10 - 0.30)")
      if (v < 0.50) return("Moderate (0.30 - 0.50)")
      return("Large / Strong (\u2265 0.50)")
    },
    
    .convert_metrics = function(es_val, method, benchmark = "cohen") {
      d <- if (method %in% c("rank_biserial", "rank_biserial_paired", "akritas_arnold_delta")) {
        r_clamped <- max(min(es_val, 0.999), -0.999)
        (2 * r_clamped) / sqrt(1 - r_clamped^2)
      } else {
        es_val
      }
      
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
      valid_diff <- stats::na.omit(diff_vec)
      if (length(valid_diff) == 0) return(list(cles = NA_real_))
      p <- mean(diff_vec > 0) + 0.5 * mean(diff_vec == 0)
      list(cles = p)
    },
    
    .compute_2groups = function(ga, gb, name_a, name_b, ref_grp, method, ci_method, ci_level, n_bootstrap, cov_a = NULL, cov_b = NULL, benchmark = "cohen") {
      es_val <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
      has_cov <- !is.null(cov_a) && !is.null(cov_b)
      n1 <- length(ga)
      n2 <- length(gb)
      s1 <- stats::sd(ga, na.rm = TRUE)
      s2 <- stats::sd(gb, na.rm = TRUE)
      
      if (is.na(s1) || is.na(s2) || (s1 == 0 && s2 == 0)) {
        return(list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method, interpretation = "Zero variance"))
      }
      
      if (method == "algina_keselman_d") {
        m_ta <- mean(ga, trim = 0.20)
        m_tb <- mean(gb, trim = 0.20)
        wa <- private$.winsorize(ga, 0.20)
        wb <- private$.winsorize(gb, 0.20)
        sw <- sqrt(((n1 - 1) * stats::var(wa) + (n2 - 1) * stats::var(wb)) / (n1 + n2 - 2))
        es_val <- if (sw > 0) 0.642 * (m_ta - m_tb) / sw else NA_real_
        
        boot_dr <- function() {
          b1 <- sample(ga, n1, replace = TRUE)
          b2 <- sample(gb, n2, replace = TRUE)
          b_wa <- private$.winsorize(b1, 0.20)
          b_wb <- private$.winsorize(b2, 0.20)
          b_sw <- sqrt(((n1 - 1) * stats::var(b_wa) + (n2 - 1) * stats::var(b_wb)) / (n1 + n2 - 2))
          if (b_sw > 0) 0.642 * (mean(b1, trim = 0.20) - mean(b2, trim = 0.20)) / b_sw else NA_real_
        }
        b_reps <- if (ci_method == "bootstrap") n_bootstrap else 400
        b_vals <- replicate(b_reps, boot_dr())
        b_vals <- b_vals[is.finite(b_vals)]
        if (length(b_vals) >= 10) {
          alpha <- 1 - ci_level
          q <- stats::quantile(b_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
          ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
        }
        return(list(es_value = es_val, ci_lower = ci_lo, ci_upper = ci_hi, method = "Robust d (Algina-Keselman-Penfield)", interpretation = private$.interpret(es_val, "cohens_d", benchmark)))
      }
      
      if (method == "akritas_arnold_delta") {
        comp_mat <- outer(ga, gb, function(a, b) ifelse(a > b, 1, ifelse(a == b, 0.5, 0)))
        p_hat <- mean(comp_mat)
        es_val <- 2 * p_hat - 1
        
        r_all <- rank(c(ga, gb))
        r_a <- r_all[1:n1]; r_b <- r_all[(n1 + 1):(n1 + n2)]
        r_int_a <- rank(ga); r_int_b <- rank(gb)
        v1 <- stats::var(r_a - r_int_a)
        v2 <- stats::var(r_b - r_int_b)
        se_delta <- 2 * sqrt(v1 / (n1 * n2^2) + v2 / (n2 * n1^2))
        z_crit <- stats::qnorm(1 - (1 - ci_level) / 2)
        ci_lo <- max(-1, es_val - z_crit * se_delta)
        ci_hi <- min(1, es_val + z_crit * se_delta)
        
        return(list(es_value = es_val, ci_lower = ci_lo, ci_upper = ci_hi, method = "Akritas-Arnold Relative Effect (\u03b4\u0302)", interpretation = private$.interpret(es_val, "akritas_arnold_delta", benchmark)))
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
        
        if (method == "rank_biserial") {
          res_fit <- stats::lm(y ~ cov, data = df_cov)
          resids <- stats::residuals(res_fit)
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
          fit <- stats::lm(y ~ group + cov, data = df_cov)
          emm <- emmeans::emmeans(fit, "group")
          emm_df <- as.data.frame(emm)
          emm_a <- emm_df$emmean[emm_df$group == name_a]
          emm_b <- emm_df$emmean[emm_df$group == name_b]
          if (length(emm_a) > 0 && length(emm_b) > 0) {
            adj_diff <- emm_a - emm_b
            contr_df <- as.data.frame(pairs(emm))
            se_diff <- contr_df$SE[1]
            alpha <- 1 - ci_level
            df_res <- stats::df.residual(fit)
            t_crit <- stats::qt(1 - alpha / 2, df = df_res)
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
        
        if (ci_method == "bootstrap") {
          boot_fn <- function() {
            idx_a <- sample(seq_len(n1), n1, replace = TRUE)
            idx_b <- sample(seq_len(n2), n2, replace = TRUE)
            b_ga <- ga[idx_a]; b_cov_a <- cov_a[idx_a]
            b_gb <- gb[idx_b]; b_cov_b <- cov_b[idx_b]
            b_n1 <- length(b_ga); b_n2 <- length(b_gb)
            b_df <- data.frame(y = c(b_ga, b_gb), group = factor(c(rep(name_a, b_n1), rep(name_b, b_n2))), cov = c(b_cov_a, b_cov_b))
            b_fit <- try(stats::lm(y ~ group + cov, data = b_df), silent = TRUE)
            if (inherits(b_fit, "try-error")) return(NA_real_)
            b_emm <- try(emmeans::emmeans(b_fit, "group"), silent = TRUE)
            if (inherits(b_emm, "try-error")) return(NA_real_)
            b_emm_df <- as.data.frame(b_emm)
            b_emm_a <- b_emm_df$emmean[b_emm_df$group == name_a]
            b_emm_b <- b_emm_df$emmean[b_emm_df$group == name_b]
            if (length(b_emm_a) == 0 || length(b_emm_b) == 0) return(NA_real_)
            b_adj_diff <- b_emm_a - b_emm_b
            b_s1 <- stats::sd(b_ga, na.rm = TRUE); b_s2 <- stats::sd(b_gb, na.rm = TRUE)
            b_sd_pooled <- sqrt(((b_n1 - 1) * b_s1^2 + (b_n2 - 1) * b_s2^2) / (b_n1 + b_n2 - 2))
            if (method == "cohens_d") return(b_adj_diff / b_sd_pooled)
            if (method == "hedges_g") return((b_adj_diff / b_sd_pooled) * (1 - 3 / (4 * (b_n1 + b_n2 - 2) - 1)))
            NA_real_
          }
          boot_vals <- replicate(n_bootstrap, boot_fn())
          boot_vals <- boot_vals[is.finite(boot_vals)]
          if (length(boot_vals) >= 10) {
            alpha <- 1 - ci_level
            q     <- stats::quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
          }
        }
      } else {
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
            q     <- stats::quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
            ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
          }
        }
      }
      list(es_value = es_val, ci_lower = ci_lo, ci_upper = ci_hi, method = method, interpretation = private$.interpret(es_val, method, benchmark))
    },
    
    .compute_dependent_paired = function(col1, col2, name1, name2, ref_wave, method, ci_method, ci_level, n_bootstrap, cov_col = NULL, benchmark = "cohen") {
      es_val <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_
      n <- length(col1)
      sd1 <- stats::sd(col1, na.rm = TRUE)
      sd2 <- stats::sd(col2, na.rm = TRUE)
      diff_vec <- col1 - col2
      sd_diff <- stats::sd(diff_vec, na.rm = TRUE)
      
      if (is.na(sd_diff) || sd_diff == 0) {
        return(list(es_value = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, method = method, interpretation = "Zero variance in difference score"))
      }
      
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
      
      if (ci_method == "bootstrap") {
        boot_fn <- function() {
          idx <- sample(seq_len(n), n, replace = TRUE)
          b1 <- col1[idx]; b2 <- col2[idx]
          res_b <- tryCatch({
            switch(method,
                   cohens_dz            = effectsize::cohens_d(b1, b2, paired = TRUE, ci = NULL)[[1]],
                   hedges_gz            = effectsize::hedges_g(b1, b2, paired = TRUE, ci = NULL)[[1]],
                   rank_biserial_paired = effectsize::rank_biserial(b1, b2, paired = TRUE, ci = NULL)[[1]]
            )
          }, error = function(e) NA_real_)
          as.numeric(res_b)
        }
        boot_vals <- replicate(n_bootstrap, boot_fn())
        boot_vals <- boot_vals[is.finite(boot_vals)]
        if (length(boot_vals) >= 10) {
          alpha <- 1 - ci_level
          q     <- stats::quantile(boot_vals, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
          ci_lo <- as.numeric(q[[1]]); ci_hi <- as.numeric(q[[2]])
        }
      }
      list(es_value = es_val, ci_lower = ci_lo, ci_upper = ci_hi, method = method, interpretation = private$.interpret(es_val, method, benchmark))
    },
    
    .compute_glm = function(iv_col, dv_col, name_a, name_b, dist, ci_method, ci_level, n_bootstrap, cov_col = NULL, benchmark = "cohen") {
      family_fn <- switch(dist, 
                          glm_gaussian     = stats::gaussian(link = "identity"),
                          glm_gamma        = stats::Gamma(link = "log"), 
                          glm_inv_gaussian = stats::inverse.gaussian(link = "log"), 
                          stats::Gamma(link = "log"))
      
      mask <- iv_col %in% c(name_a, name_b)
      iv_sub <- droplevels(iv_col[mask])
      dv_sub <- dv_col[mask]
      n <- length(dv_sub)
      has_cov <- !is.null(cov_col)
      
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
      
      fit <- stats::glm(formula_glm, data = df_glm, family = family_fn)
      emm <- emmeans::emmeans(fit, "x", type = "response")
      pairs_df <- as.data.frame(pairs(emmeans::regrid(emm)))
      
      ame <- pairs_df$estimate[1]
      se_ame <- pairs_df$SE[1]
      alpha <- 1 - ci_level
      z <- stats::qnorm(1 - alpha / 2)
      ci_lo <- ame - z * se_ame
      ci_hi <- ame + z * se_ame
      
      sd_dv <- stats::sd(dv_sub, na.rm = TRUE)
      interp_val <- if (is.finite(sd_dv) && sd_dv > 0) private$.interpret(abs(ame) / sd_dv, "cohens_d", benchmark) else "\u2014"
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


