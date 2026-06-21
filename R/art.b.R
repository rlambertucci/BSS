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
        '<div style="font-size:0.9em; line-height:1.6; padding:4px;">',
        '<h4 style="margin-bottom:6px;">&#128200; Aligned Rank Transform (ART)</h4>',
        '<p style="margin-bottom:8px;">A robust nonparametric approach for analyzing data with multiple factors or repeated measures. ',
        'It aligns the data before ranking, allowing for the accurate evaluation of main effects and interactions.</p>',
        '<ul style="margin-top:0px; padding-left:20px;">',
        '<li><b>Cross-sectional:</b> For independent samples (Factorial Nonparametric ANOVA).</li>',
        '<li><b>Longitudinal:</b> For repeated measures or mixed designs. Automatically handles subject tracking.</li>',
        '</ul>',
        '<p style="margin-bottom:4px; font-size:0.85em; color:#555;"><i>Powered by the R package <b>ARTool</b> (Wobbrock et al., 2011).</i></p>',
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
          rownames(process_data) <- NULL
          dep_var <- ".dv_internal"
          time_var <- ".time_internal"
          process_data[[time_var]] <- as.factor(process_data[[time_var]])
        }, error = function(e) {
          self$results$text$setContent(paste("Error converting Wide to Long format:", e$message))
          self$results$text$setVisible(TRUE)
          reshape_failed <<- TRUE
        })
      }
      if (reshape_failed) {
        return()
      }
      
      predictors <- character()
      if (private$.is_defined(fixed_factors)) {
        predictors <- c(predictors, fixed_factors)
      }
      if (mode == "longitudinal" && private$.is_defined(time_var)) {
        predictors <- c(predictors, time_var)
      }
      
      if (length(predictors) == 0) {
        self$results$text$setContent("Error: No valid factors found to build the model.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      for (p in predictors) {
        process_data[[p]] <- as.factor(process_data[[p]])
      }
      
      if (private$.is_defined(subject_id_var)) {
        process_data[[subject_id_var]] <- as.factor(process_data[[subject_id_var]])
      }
      
      main_effects <- paste(predictors, collapse = " * ")
      if (mode == "longitudinal" && private$.is_defined(subject_id_var)) {
        random_effect <- paste0(" + (1|", subject_id_var, ")")
      } else {
        random_effect <- ""
      }
      
      formula_str <- paste0("`", dep_var, "` ~ ", main_effects, random_effect)
      art_formula <- as.formula(formula_str)
      
      vars_in_model <- c(dep_var, predictors, subject_id_var)
      vars_in_model <- unique(vars_in_model[private$.is_defined(vars_in_model)])
      process_data <- na.omit(process_data[, vars_in_model, drop = FALSE])
      
      if (nrow(process_data) == 0) {
        self$results$text$setContent("Error: No complete cases remaining after removing missing values.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
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
      anova_table$setNote(
        "citation",
        "Note. Computations powered by the R package ARTool (Wobbrock et al., 2011). Citation: Wobbrock, J. O., Findlater, L., Gergle, D., and Higgins, J. J. (2011). The Aligned Rank Transform for Nonparametric Factorial Analyses Using Only ANOVA Procedures. In Proceedings of the ACM Conference on Human Factors in Computing Systems (CHI '11), pp. 143-146. https://doi.org/10.1145/1978942.1978963"
      )
      
      num_rows_anova <- nrow(anova_res)
      if (num_rows_anova == 0) {
        return()
      }
      
      for (i in 1:num_rows_anova) {
        term <- anova_res$Term[i]
        df_num <- private$.safe_extract(anova_res, i, c("Df", "df"))
        df_res <- private$.safe_extract(anova_res, i, c("Df.res", "Res.Df", "Denom"))
        f_val <- private$.safe_extract(anova_res, i, c("F", "F value", "F-value"))
        p_val <- private$.safe_extract(anova_res, i, c("Pr(>F)", "p-value", "p.value", "p"))
        
        pes <- NA_real_
        if (!is.na(f_val) && !is.na(df_num) && !is.na(df_res) && df_res > 0) {
          pes <- (f_val * df_num) / ((f_val * df_num) + df_res)
        }
        
        anova_table$addRow(rowKey = i, values = list(
          factor = term,
          df = df_num,
          df_res = df_res,
          f_value = f_val,
          p_value = p_val,
          part_eta_sq = pes
        ))
      }
      
      if (isTRUE(run_posthoc)) {
        posthoc_table <- self$results$posthocTable
        posthoc_table$setNote(
          "citation",
          "Note. Pairwise contrast comparisons computed using the ART-C procedure (Elkin et al., 2021) via the R package ARTool, with p-value adjustments powered by the emmeans package (Lenth, 2024). Citations: (1) Elkin, L. A. et al. (2021). An Aligned Rank Transform Procedure for Multifactor Contrast Tests. In Proceedings of the ACM Symposium on User Interface Software and Technology (UIST '21), pp. 754-768. (2) Lenth, R. V. (2024). emmeans: Estimated Marginal Means, aka Least-Squares Means."
        )
        
        es_table <- self$results$effectSizeTable
        es_table$setNote(
          "citation",
          "Note. Pairwise effect sizes (Cohen's d or Hedges' g) approximated from t-ratios and degrees of freedom: d = 2 * t / sqrt(df) for unpaired contrasts, and d = t / sqrt(df) for paired/within-subjects contrasts. Hedges' g includes correction: g = d * (1 - 3 / (4 * df - 1))."
        )
        
        terms_to_test <- anova_res$Term[1:num_rows_anova]
        row_ph <- 1
        row_es <- 1
        
        between_vars <- fixed_factors
        within_vars <- if (mode == "longitudinal") time_var else character()
        
        for (term in terms_to_test) {
          ph_res <- tryCatch({
            ARTool::art.con(model, term, adjust = posthoc_correction)
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
              factor = term,
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
              m_label <- if (es_method == "hedges_g") "Hedge's g" else "Cohen's d"
              
              if (!is.na(t_r) && !is.na(df) && df > 0 && is.finite(df)) {
                is_paired <- private$.is_contrast_paired(comp, term, between_vars, within_vars)
                scale_factor <- if (is_paired) 1 else 2
                d_val <- (scale_factor * t_r) / sqrt(df)
                
                if (es_method == "hedges_g") {
                  j_corr <- 1 - (3 / (4 * df - 1))
                  es_final <- d_val * j_corr
                } else {
                  es_final <- d_val
                }
              }
              
              es_table$addRow(rowKey = row_es, values = list(
                factor = term,
                contrast = comp,
                method = m_label,
                es_value = es_final
              ))
              row_es <- row_es + 1
            }
          }
        }
      }
      
      self$results$text$setContent(paste("ART Analysis completed successfully.\nFormula used:", formula_str))
      self$results$text$setVisible(TRUE)
    }
  )
)