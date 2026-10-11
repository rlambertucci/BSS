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
        '<li><b>Longitudinal:</b> For repeated measures or mixed designs. Automatically handles subject tracking.</li>',
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
      show_plot <- self$options$show_plot
      
      if (!isTRUE(run_analysis)) {
        self$results$text$setContent("Check 'Run ART Analysis' to compute results.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      if (mode == "cross_sectional") {
        dep_var <- self$options$dep_var_cs
        fixed_factors <- self$options$fixed_factors_cs
        long_format <- "long"
        subject_id_var <- NULL
        time_var <- NULL
        wave_vars <- NULL
      } else {
        dep_var <- self$options$dep_var_long
        fixed_factors <- self$options$fixed_factors_long
        long_format <- self$options$long_format
        subject_id_var <- self$options$subject_id_var
        time_var <- self$options$time_var
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
          if (!private$.is_defined(dep_var) || !private$.is_defined(time_var) || !private$.is_defined(subject_id_var)) {
            self$results$text$setContent("Error: Long format requires a Dependent Variable, a Time Variable, and a Subject ID Variable.")
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
          time_var <- ".time_internal"
        }, error = function(e) {
          reshape_failed <<- TRUE
        })
        
        if (reshape_failed) {
          self$results$text$setContent("Error: Reshaping wide data to long format failed. Please check column names and data consistency.")
          self$results$text$setVisible(TRUE)
          return()
        }
      }
      
      predictors <- if (mode == "cross_sectional") fixed_factors else c(fixed_factors, time_var)
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
        "Note. Computations powered by the R package ARTool (Wobbrock et al., 2011). Partial η² calculated as (F * df) / (F * df + df.res). Partial ω² calculated as df * (F - 1) / (df * (F - 1) + df.res + 1), truncated at 0."
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
          "Note. Pairwise contrast comparisons computed using the ART-C procedure (Elkin et al., 2021) via the R package ARTool, with p-value adjustments powered by the emmeans package (Lenth, 2024)."
        )
        
        es_table <- self$results$effectSizeTable
        if (es_table$rowCount > 0) es_table$deleteRows()
        
        es_table$setNote(
          "citation",
          "Note. Pairwise effect sizes (Cohen's d or Hedges' g) approximated from t-ratios and degrees of freedom: d = 2 * t / sqrt(df) for unpaired contrasts, and d = t / sqrt(df) for paired/within-subjects contrasts. Hedges' g includes correction: g = d * (1 - 3 / (4 * df - 1))."
        )
        
        terms_to_test <- anova_res$Term[1:num_rows_anova]
        row_ph <- 1
        row_es <- 1
        
        between_vars_safe <- unname(safe_name_map[fixed_factors])
        within_vars_safe <- if (mode == "longitudinal") unname(safe_name_map[time_var]) else character()
        
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
            
            posthoc_table$addRow(rowKey = row_ph, values = list(
              factor = term_display,
              contrast = comp,
              estimate = est,
              se = se,
              df = df,
              t_ratio = t_r,
              p_value = p_v
            ))
            row_ph <- row_ph + 1
            
            if (isTRUE(calc_es)) {
              es_final <- NA_real_
              m_label <- if (es_method == "hedges_g") "Hedges' g" else "Cohen's d"
              
              if (!is.na(t_r) && !is.na(df) && df > 0 && is.finite(df)) {
                is_paired <- private$.is_contrast_paired(comp, term, between_vars_safe, within_vars_safe)
                scale_factor <- if (is_paired) 1 else 2
                d_val <- (scale_factor * t_r) / sqrt(df)
                
                if (es_method == "hedges_g") {
                  j_corr <- if (df > 1) (1 - 3 / (4 * df - 1)) else 1
                  es_final <- d_val * j_corr
                } else {
                  es_final <- d_val
                }
              }
              
              es_table$addRow(rowKey = row_es, values = list(
                factor = term_display,
                contrast = comp,
                method = m_label,
                es_value = es_final
              ))
              row_es <- row_es + 1
            }
          }
        }
      }
      
      if (isTRUE(show_plot)) {
        image <- self$results$artPlot
        w <- self$options$plot_width
        h <- self$options$plot_height
        if (!is.null(w) && !is.null(h) && w > 0 && h > 0) {
          image$setSize(w, h)
        }
        image$setState(list(
          data          = process_data,
          dep_var       = dep_var_safe,
          dep_var_title = dep_var,
          predictors    = predictors_safe,
          reverse_map   = reverse_name_map
        ))
      }
      
      display_formula <- paste0("`", dep_var, "` ~ ", paste(paste0("`", predictors, "`"), collapse = " * "))
      if (mode == "longitudinal" && private$.is_defined(subject_id_var)) {
        display_formula <- paste0(display_formula, " + (1|`", subject_id_var, "`)\")")
      }
      self$results$text$setContent(paste("ART Analysis completed successfully.\nFormula used:", display_formula))
      self$results$text$setVisible(TRUE)
    },
    
    .renderArtPlot = function(image, ggtheme, theme, ...) {
      state <- image$state
      if (is.null(state) || is.null(state$data)) return(FALSE)
      
      df          <- state$data
      dep_var     <- state$dep_var
      dep_title   <- state$dep_var_title
      predictors  <- state$predictors
      reverse_map <- state$reverse_map
      
      if (is.null(dep_var) || length(predictors) == 0) return(FALSE)
      
      pred1 <- predictors[1]
      pred1_label <- if (!is.null(reverse_map[[pred1]])) reverse_map[[pred1]] else pred1
      
      if (length(predictors) == 1) {
        plot_df <- stats::aggregate(df[[dep_var]], by = list(P1 = df[[pred1]]), FUN = function(x) {
          c(Mean = mean(x, na.rm = TRUE),
            SE   = stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
        })
        plot_df <- data.frame(
          P1   = plot_df$P1,
          Mean = plot_df$x[, "Mean"],
          SE   = plot_df$x[, "SE"]
        )
        plot_df$CI_lower <- plot_df$Mean - 1.96 * plot_df$SE
        plot_df$CI_upper <- plot_df$Mean + 1.96 * plot_df$SE
        
        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = P1, y = Mean, group = 1)) +
          ggplot2::geom_line(color = "#2b5c8f", linewidth = 0.8) +
          ggplot2::geom_point(color = "#2b5c8f", size = 3) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), width = 0.15, color = "#2b5c8f", linewidth = 0.7) +
          ggplot2::labs(title = paste("Marginal Means:", dep_title), x = pred1_label, y = dep_title) +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA))
        print(p)
        return(TRUE)
      } else {
        pred2 <- predictors[2]
        pred2_label <- if (!is.null(reverse_map[[pred2]])) reverse_map[[pred2]] else pred2
        
        plot_df <- stats::aggregate(df[[dep_var]], by = list(P1 = df[[pred1]], P2 = df[[pred2]]), FUN = function(x) {
          c(Mean = mean(x, na.rm = TRUE),
            SE   = stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
        })
        plot_df <- data.frame(
          P1   = plot_df$P1,
          P2   = plot_df$P2,
          Mean = plot_df$x[, "Mean"],
          SE   = plot_df$x[, "SE"]
        )
        plot_df$CI_lower <- plot_df$Mean - 1.96 * plot_df$SE
        plot_df$CI_upper <- plot_df$Mean + 1.96 * plot_df$SE
        
        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = P1, y = Mean, color = P2, group = P2)) +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_lower, ymax = CI_upper), width = 0.15, linewidth = 0.7) +
          ggplot2::labs(
            title = paste("Interaction Plot:", pred1_label, "×", pred2_label),
            x = pred1_label,
            y = dep_title,
            color = pred2_label
          ) +
          ggplot2::scale_color_brewer(palette = "Set1") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            legend.position  = "right",
            panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
          )
        print(p)
        return(TRUE)
      }
    }
  )
)
