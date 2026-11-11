dataimputationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "dataimputationClass",
  inherit = dataimputationBase,
  private = list(
    
    
    .update_help = function() {
      configured <- list(mice_cs = length(self$options$vars_mice_cs) > 0, mice_long = length(c(self$options$vars_mice_long, self$options$wave_vars_mice_long)) > 0, missranger = length(self$options$vars_mr) > 0)
      result_names <- c(mice_cs = "imputationInfo_mice_cs", mice_long = "imputationInfo_mice_long", missranger = "imputationInfo_mr")
      for (mode in names(configured)) {
        result <- self$results[[result_names[[mode]]]]
        if (!is.null(result)) result$setVisible(identical(self$options$mode, mode) && !configured[[mode]])
        suffix <- if (identical(mode, "missranger")) "mr" else mode
        table <- self$results[[paste0("imputedData_", suffix)]]
        notice <- if (identical(self$options$mode, mode) && configured[[mode]]) "Saved columns contain one completed dataset, using mean/mode aggregation when multiple imputations are used. Analyses of saved columns alone do not propagate between-imputation variability into standard errors or confidence intervals; use appropriate multiple-imputation inference instead." else NULL
        if (!is.null(table)) table$setNote("aggregation_uncertainty", notice)
      }
    },

    .init = function() {
      private$.update_help()
      info_cs <- paste0(
        '<div>',
        '<h4>Multivariate Imputation by Chained Equations (Cross-sectional)</h4>',
        '<ul>',
        '<li><b>Methodology:</b> Imputes missing values by modeling each incomplete variable conditionally on the others ',
        'using univariate regression models iteratively <i>(van Buuren & Groothuis-Oudshoorn, 2011)</i>.</li>',
        '<li><b>Algorithms:</b> <b>pmm</b> (Predictive Mean Matching \u2014 robust, default), ',
        '<b>norm</b> (Bayesian linear regression), <b>logreg</b> (binary logistic regression), ',
        'and <b>polyreg</b> (polytomous regression for nominal categories).</li>',
        '<li><b>Aggregation:</b> Combines corresponding missing cells across <i>m</i> completed datasets into one saved dataset, using the <b>Mean</b> for numeric ',
        'variables and the <b>Mode</b> for categorical variables. This is a single aggregated dataset, separate from the inferential summaries.</li>',
        '</ul>',
        '</div>'
      )
      info_cs <- paste0(info_cs, "<p><b>Saved dataset:</b> A single aggregated dataset does not represent imputation uncertainty. Analyses of saved columns alone do not propagate between-imputation variability into standard errors or confidence intervals; use appropriate multiple-imputation inference instead.</p>")
      self$results$imputationInfo_mice_cs$setContent(info_cs)
      
      info_long <- paste0(
        '<div>',
        '<h4>Multilevel Imputation by Chained Equations (Longitudinal)</h4>',
        '<ul>',
        '<li><b>Methodology:</b> Supports longitudinal and nested datasets by utilizing random-effects ',
        'linear mixed models <i>(miceadds package)</i>. Clustered variance is correctly accounted for.</li>',
        '<li><b>Algorithms:</b> <b>2l.pmm</b> (Two-level Predictive Mean Matching) and ',
        '<b>2l.continuous</b> (Two-level normal imputation for continuous variables).</li>',
        '<li><b>Formats:</b> Automatically reshapes <b>Wide</b> data (repeated measures) into <b>Long</b> format ',
        'for multi-level modeling, and converts it back for seamless spreadsheet saving.</li>',
        '<li><b>Wave coding (wide):</b> Wave columns are modeled as nominal fixed effects, without assuming equal spacing or a linear time trend.</li>',
        '</ul>',
        '</div>'
      )
      info_long <- paste0(info_long, "<p><b>Saved dataset:</b> A single aggregated dataset does not represent imputation uncertainty. Analyses of saved columns alone do not propagate between-imputation variability into standard errors or confidence intervals; use appropriate multiple-imputation inference instead.</p>")
      self$results$imputationInfo_mice_long$setContent(info_long)
      
      info_mr <- paste0(
        '<div>',
        '<h4>Non-parametric Random Forest Imputation (missRanger)</h4>',
        '<ul>',
        '<li><b>Methodology:</b> A fast, non-parametric imputation method that combines Breiman\'s Random Forest ',
        'with the iterative chained equations framework of MICE <i>(Mayer, 2019)</i>.</li>',
        '<li><b>Predictive Mean Matching:</b> Integrates PMM donors (nearest <i>k</i> cases) to prevent out-of-bounds ',
        'predictions and ensure natural, realistic imputed distributions.</li>',
        '<li><b>Multiple Imputation:</b> Runs <i>N</i> independent forests with deterministic seeds (Seed + i) to ',
        'create an ensemble. Final values are aggregated via Mean/Mode.</li>',
        '</ul>',
        '</div>'
      )
      info_mr <- paste0(info_mr, "<p><b>Saved dataset:</b> A single aggregated dataset does not represent imputation uncertainty. Analyses of saved columns alone do not propagate between-imputation variability into standard errors or confidence intervals; use appropriate multiple-imputation inference instead.</p>")
      self$results$imputationInfo_mr$setContent(info_mr)
    },
    
    .run = function() {
      private$.update_help()
      mode <- self$options$mode
      if (is.null(self$data)) return()
      
      if (mode == "mice_cs") {
        private$.runMiceCS()
      } else if (mode == "mice_long") {
        private$.runMiceLong()
      } else if (mode == "missranger") {
        private$.runMissRanger()
      }
    },
    
    .runMiceCS = function() {
      data           <- self$data
      vars_to_impute <- self$options$vars_mice_cs
      predictor_vars <- self$options$predictor_vars_mice_cs
      imp_method     <- self$options$imputation_method_mice_cs
      num_imp        <- self$options$num_imputations_mice_cs
      maxiter        <- self$options$maxiter_mice_cs
      run_imp        <- self$options$run_imputation_mice_cs
      
      show_pattern        <- self$options$show_pattern_plot_mice_cs
      show_upset          <- isTRUE(self$options$show_upset_plot_mice_cs)
      show_pattern_table  <- self$options$show_pattern_table_mice_cs
      show_correlation    <- self$options$show_correlation_plot_mice_cs
      show_trace          <- self$options$show_trace_plot_mice_cs
      show_density        <- self$options$show_density_plot_mice_cs
      show_strip          <- self$options$show_strip_plot_mice_cs
      show_pooled_summary <- self$options$show_pooled_summary_mice_cs
      
      if (is.null(vars_to_impute) || length(vars_to_impute) == 0) return()
      
      private$.check_imputation_targets(data, vars_to_impute)
      vars_to_impute <- Filter(function(v) {
        v %in% names(data) && !inherits(data[[v]], "jmvcore.IdVariable")
      }, vars_to_impute)
      if (length(vars_to_impute) == 0) return()
      
      miss_table <- self$results$missingnessTable_mice_cs
      miss_table$setNote(
        "citation",
        "Imputations are performed using the multivariate imputation by chained equations (MICE) methodology. Powered by the R package mice."
      )
      
      for (var_name in vars_to_impute) {
        col <- data[[var_name]]
        n_total <- length(col)
        n_miss  <- sum(is.na(col))
        miss_table$addRow(rowKey = var_name, values = list(
          variable    = var_name,
          n_missing   = n_miss,
          pct_missing = (n_miss / n_total) * 100,
          n_valid     = n_total - n_miss,
          method_used = private$.resolve_method(var_name, data, imp_method)
        ))
      }
      
      if (isTRUE(show_pattern_table)) {
        private$.fill_pattern_table(data, vars_to_impute, self$results$patternTable_mice_cs)
      }
      
      if (show_pattern) {
        self$results$patternPlot_mice_cs$setState(list(vars = vars_to_impute))
      }
      if (show_upset) {
        self$results$upsetPlot_mice_cs$setState(list(vars = vars_to_impute))
      }
      if (nrow(data) > 10000) {
        miss_table$setNote("large_sample", paste0("Large sample size detected (N = ", nrow(data), "). Recommended settings: PMM with k <= 5 or num.trees <= 100."))
      }
      if (show_correlation) {
        private$.set_correlation_state(data, vars_to_impute, self$results$correlationPlot_mice_cs, self$results$correlationInfo_mice_cs)
      }
      
      if (!isTRUE(run_imp)) return()
      
      all_vars <- unique(c(vars_to_impute, predictor_vars))
      all_vars <- intersect(all_vars, names(data))
      data_to_impute <- data[, all_vars, drop = FALSE]
      
      if (ncol(data_to_impute) < 2) {
        jmvcore::reject("MICE requires at least 2 variables to perform imputation. Add variables to 'Variables' or 'Predictors'.")
      }
      
      mice_seed <- if (isTRUE(self$options$use_seed_mice_cs)) self$options$seed_mice_cs else NA_integer_
      
      method_vec <- private$.build_method_vector(data_to_impute, vars_to_impute, imp_method, NULL, NULL)
      pred_matrix <- mice::make.predictorMatrix(data_to_impute)
      
      mice_obj <- tryCatch({
        mice::mice(
          data            = data_to_impute,
          m               = num_imp,
          method          = method_vec,
          predictorMatrix = pred_matrix,
          maxit           = maxiter,
          seed            = mice_seed,
          printFlag       = FALSE
        )
      }, error = function(e) {
        jmvcore::reject(paste("MICE Cross-sectional execution error:", e$message))
      })
      
      
      if (show_trace) self$results$tracePlot_mice_cs$setState(list(chainMean = mice_obj$chainMean, chainVar = mice_obj$chainVar, vars = names(which(mice_obj$method != ""))))
      if (show_density || show_strip) {
        completed_plots <- lapply(seq_len(mice_obj$m), function(index) mice::complete(mice_obj, action = index))
        plot_rows <- private$.imputation_plot_data(mice_obj$data, completed_plots, if (show_density) names(mice_obj$data) else vars_to_impute)
        if (show_density) self$results$densityPlot_mice_cs$setState(private$.density_plot_data(plot_rows))
        if (show_strip) self$results$stripPlot_mice_cs$setState(plot_rows[plot_rows$variable %in% vars_to_impute, , drop = FALSE])
      }
      
      pooled_data <- private$.pool_imputations(mice_obj, data_to_impute, vars_to_impute)
      
      completed_list <- NULL
      if (isTRUE(show_pooled_summary) || isTRUE(self$options$enable_model_pooling_mice_cs)) {
        completed_list <- lapply(seq_len(mice_obj$m), function(i) mice::complete(mice_obj, action = i))
      }
      
      if (isTRUE(show_pooled_summary) && !is.null(completed_list)) {
        private$.fill_rubin_summary_table_datasets(
          completed_list, data, vars_to_impute, self$results$pooledSummaryTable_mice_cs
        )
      }
      
      if (isTRUE(self$options$enable_model_pooling_mice_cs)) {
        private$.runModelPooling(
          completed_list, data, self$options$model_dep_mice_cs, self$options$model_indep_mice_cs,
          self$options$model_type_mice_cs, self$results$modelPoolingTable_mice_cs
        )
      }
      
      private$.fill_imputed_table(data, pooled_data, vars_to_impute, self$results$imputedData_mice_cs)
      
      private$.write_output(pooled_data, vars_to_impute, data, self$results$imputedColsOutput_mice_cs)
    },
    
    .runMiceLong = function() {
      data           <- self$data
      vars_to_impute <- self$options$vars_mice_long
      predictor_vars <- self$options$predictor_vars_mice_long
      imp_method     <- self$options$imputation_method_mice_long
      num_imp        <- self$options$num_imputations_mice_long
      maxiter        <- self$options$maxiter_mice_long
      run_imp        <- self$options$run_imputation_mice_long
      
      data_format      <- self$options$data_format_mice_long
      subject_id_var   <- self$options$subject_id_var_mice_long
      time_var         <- if (data_format == "long") self$options$time_var_mice_long else NULL
      group_var        <- self$options$group_var_mice_long
      fixed_covariates <- self$options$fixed_covariates_mice_long
      wave_vars        <- self$options$wave_vars_mice_long
      
      show_pattern        <- self$options$show_pattern_plot_mice_long
      show_upset          <- isTRUE(self$options$show_upset_plot_mice_long)
      show_pattern_table  <- self$options$show_pattern_table_mice_long
      show_correlation    <- self$options$show_correlation_plot_mice_long
      show_trace          <- self$options$show_trace_plot_mice_long
      show_density        <- self$options$show_density_plot_mice_long
      show_strip          <- self$options$show_strip_plot_mice_long
      show_pooled_summary <- self$options$show_pooled_summary_mice_long
      
      vars_for_miss_table <- if (data_format == "wide") wave_vars else vars_to_impute
      
      if (!is.null(vars_for_miss_table) && length(vars_for_miss_table) > 0) {
        private$.check_imputation_targets(data, vars_for_miss_table, subject_id_var)
        vars_for_miss_table <- Filter(function(v) {
          v %in% names(data) && !inherits(data[[v]], "jmvcore.IdVariable")
        }, vars_for_miss_table)
      }
      
      if (is.null(vars_for_miss_table) || length(vars_for_miss_table) == 0) return()
      
      miss_table <- self$results$missingnessTable_mice_long
      miss_table$setNote(
        "citation",
        "Longitudinal imputations are performed using multilevel multivariate imputation by chained equations. Powered by the R packages mice and miceadds."
      )
      
      for (var_name in vars_for_miss_table) {
        col <- data[[var_name]]
        n_total <- length(col)
        n_miss  <- sum(is.na(col))
        miss_table$addRow(rowKey = var_name, values = list(
          variable    = var_name,
          n_missing   = n_miss,
          pct_missing = (n_miss / n_total) * 100,
          n_valid     = n_total - n_miss,
          method_used = imp_method
        ))
      }
      
      if (isTRUE(show_pattern_table)) {
        private$.fill_pattern_table(data, vars_for_miss_table, self$results$patternTable_mice_long)
      }
      
      if (show_pattern) {
        self$results$patternPlot_mice_long$setState(list(vars = vars_for_miss_table))
      }
      if (show_upset) {
        self$results$upsetPlot_mice_long$setState(list(vars = vars_for_miss_table))
      }
      if (nrow(data) > 10000) {
        miss_table$setNote("large_sample", paste0("Large sample size detected (N = ", nrow(data), "). Recommended settings: PMM with k <= 5 or num.trees <= 100."))
      }
      if (show_correlation) {
        private$.set_correlation_state(data, vars_for_miss_table, self$results$correlationPlot_mice_long, self$results$correlationInfo_mice_long)
      }
      
      wide_to_long_map <- NULL
      self$results$missingnessTable_mice_long$setNote("wide_req", NULL)
      
      if (data_format == "wide") {
        if (length(wave_vars) < 2) {
          self$results$missingnessTable_mice_long$setNote("wide_req", "Wide format requires at least two columns in 'Wave Columns'.")
          return()
        }
        if (is.null(subject_id_var) || nchar(subject_id_var) == 0) {
          self$results$missingnessTable_mice_long$setNote("wide_req", "Wide format requires a Subject ID variable to link repeated measures.")
          return()
        }
        if (anyNA(data[[subject_id_var]])) {
          jmvcore::reject("Subject ID variable cannot contain missing values.", code = "subject_id_has_na")
          return()
        }
        
        if (anyDuplicated(data[[subject_id_var]])) {
          jmvcore::reject("Subject ID variable must contain unique values in wide format.", code = "subject_id_duplicated")
          return()
        }
        reshape_names <- tail(make.unique(c(names(data), "wave_value", ".wave", ".row_id")), 3)
        while (any(paste0(reshape_names[1], ".", wave_vars) %in% names(data))) {
          reshape_names[1] <- tail(make.unique(c(names(data), reshape_names[1], reshape_names[1])), 1)
        }
        wide_to_long_map <- list(wave_vars = wave_vars, subject_id_var = subject_id_var, original_data = data,
                                 value_var = reshape_names[1], time_var = reshape_names[2], row_var = reshape_names[3])
        data_wide         <- data
        data_wide[[wide_to_long_map$row_var]] <- seq_len(nrow(data_wide))
        
        data_long <- tryCatch({
          stats::reshape(
            data_wide,
            varying   = wave_vars,
            v.names   = wide_to_long_map$value_var,
            timevar   = wide_to_long_map$time_var,
            times     = wave_vars,
            idvar     = wide_to_long_map$row_var,
            direction = "long"
          )
        }, error = function(e) {
          jmvcore::reject(paste("Reshape Wide-to-Long error:", e$message))
        })
        data_long <- data_long[order(data_long[[subject_id_var]], data_long[[wide_to_long_map$time_var]]), ]
        rownames(data_long) <- NULL
        data_long[[wide_to_long_map$time_var]] <- factor(data_long[[wide_to_long_map$time_var]], levels = wave_vars, ordered = FALSE)
        
        data           <- data_long
        vars_to_impute <- wide_to_long_map$value_var
      }
      
      if (!isTRUE(run_imp)) return()
      
      all_vars <- unique(c(
        vars_to_impute,
        if (!is.null(predictor_vars)) predictor_vars,
        if (!is.null(fixed_covariates)) fixed_covariates,
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0) subject_id_var,
        if (!is.null(time_var) && nchar(time_var) > 0) time_var,
        if (!is.null(group_var) && nchar(group_var) > 0) group_var,
        if (!is.null(wide_to_long_map)) wide_to_long_map$time_var
      ))
      all_vars <- intersect(all_vars, names(data))
      data_to_impute <- data[, all_vars, drop = FALSE]
      
      if (ncol(data_to_impute) < 2) {
        jmvcore::reject("MICE requires at least 2 variables (including predictors/structurals) to perform imputation.")
      }
      
      if (!is.null(subject_id_var) && nchar(subject_id_var) > 0 && subject_id_var %in% names(data_to_impute)) {
        if (anyNA(data_to_impute[[subject_id_var]])) {
          jmvcore::reject("Subject ID variable cannot contain missing values.", code = "subject_id_has_na")
          return()
        }
        data_to_impute[[subject_id_var]] <- as.integer(as.factor(data_to_impute[[subject_id_var]]))
      }
      
      if (!is.null(wide_to_long_map)) {
        wide_to_long_map$subject_codes <- integer(nrow(wide_to_long_map$original_data))
        wide_to_long_map$subject_codes[data[[wide_to_long_map$row_var]]] <- data_to_impute[[subject_id_var]]
      }
      mice_seed <- if (isTRUE(self$options$use_seed_mice_long)) self$options$seed_mice_long else NA_integer_
      
      method_vec <- private$.build_method_vector(data_to_impute, vars_to_impute, imp_method, subject_id_var, group_var)
      effective_time_var <- if (!is.null(wide_to_long_map)) wide_to_long_map$time_var else time_var
      pred_matrix <- private$.build_predictor_matrix(
        data_to_impute, vars_to_impute, subject_id_var, effective_time_var, group_var, fixed_covariates, imp_method
      )
      engine_data <- data_to_impute
      engine_names <- make.names(names(engine_data), unique = TRUE)
      names(engine_data) <- engine_names
      names(method_vec) <- engine_names
      dimnames(pred_matrix) <- list(engine_names, engine_names)
      
      mice_obj <- private$.with_mice_method(imp_method, tryCatch({
        mice::mice(
          data            = engine_data,
          m               = num_imp,
          method          = method_vec,
          predictorMatrix = pred_matrix,
          maxit           = maxiter,
          seed            = mice_seed,
          printFlag       = FALSE
        )
      }, error = function(e) {
        jmvcore::reject(paste("MICE Longitudinal multilevel error:", e$message))
      }))
      
      
      completed_list <- lapply(seq_len(mice_obj$m), function(index) {
        completed <- mice::complete(mice_obj, action = index)
        names(completed) <- names(data_to_impute)
        completed
      })
      if (show_trace) {
        chain_mean <- mice_obj$chainMean
        chain_var <- mice_obj$chainVar
        dimnames(chain_mean)[[1]] <- names(data_to_impute)[match(dimnames(chain_mean)[[1]], engine_names)]
        dimnames(chain_var)[[1]] <- names(data_to_impute)[match(dimnames(chain_var)[[1]], engine_names)]
        self$results$tracePlot_mice_long$setState(list(chainMean = chain_mean, chainVar = chain_var, vars = names(data_to_impute)[mice_obj$method != ""]))
      }
      if (show_density || show_strip) {
        plot_rows <- private$.imputation_plot_data(data_to_impute, completed_list, if (show_density) names(data_to_impute) else vars_to_impute)
        if (show_density) self$results$densityPlot_mice_long$setState(private$.density_plot_data(plot_rows))
        if (show_strip) self$results$stripPlot_mice_long$setState(plot_rows[plot_rows$variable %in% vars_to_impute, , drop = FALSE])
      }
      
      pooled_data <- private$.pool_imputations(mice_obj, data_to_impute, vars_to_impute, completed_list = completed_list)
      
      if (!is.null(wide_to_long_map)) {
        completed_wide_list <- list()
        if (isTRUE(show_pooled_summary) || isTRUE(self$options$enable_model_pooling_mice_long)) {
          for (i in seq_len(mice_obj$m)) {
            comp_long <- completed_list[[i]]
            wide_res_i <- stats::reshape(comp_long, idvar = subject_id_var, timevar = wide_to_long_map$time_var, v.names = wide_to_long_map$value_var, direction = "wide")
            for (wv in wave_vars) {
              col_new <- paste0(wide_to_long_map$value_var, ".", wv)
              if (col_new %in% names(wide_res_i)) {
                names(wide_res_i)[names(wide_res_i) == col_new] <- wv
              }
            }
            completed_wide_list[[i]] <- private$.restore_wide_order(wide_res_i, wide_to_long_map)
          }
        }
        
        pooled_data <- tryCatch({
          wide_res <- stats::reshape(pooled_data, idvar = subject_id_var, timevar = wide_to_long_map$time_var, v.names = wide_to_long_map$value_var, direction = "wide")
          for (wv in wave_vars) {
            col_new <- paste0(wide_to_long_map$value_var, ".", wv)
            if (col_new %in% names(wide_res)) {
              names(wide_res)[names(wide_res) == col_new] <- wv
            }
          }
          private$.restore_wide_order(wide_res, wide_to_long_map)
        }, error = function(e) {
          jmvcore::reject(paste("Reshape Long-to-Wide error:", e$message))
        })
        
        vars_to_impute <- wave_vars
        data           <- wide_to_long_map$original_data
        
        if (isTRUE(show_pooled_summary)) {
          private$.fill_rubin_summary_table_datasets(
            completed_wide_list, data, wave_vars, self$results$pooledSummaryTable_mice_long
          )
        }
      } else {
        if (isTRUE(show_pooled_summary)) {
          private$.fill_rubin_summary_table_datasets(
            completed_list, data, vars_to_impute, self$results$pooledSummaryTable_mice_long
          )
        }
      }
      
      if (isTRUE(self$options$enable_model_pooling_mice_long)) {
        models_list <- if (!is.null(wide_to_long_map)) completed_wide_list else completed_list
        if (!is.null(models_list)) {
          private$.runModelPooling(
            models_list, data, self$options$model_dep_mice_long, self$options$model_indep_mice_long,
            self$options$model_type_mice_long, self$results$modelPoolingTable_mice_long
          )
        }
      }
      private$.fill_imputed_table(data, pooled_data, vars_to_impute, self$results$imputedData_mice_long)
      
      private$.write_output(pooled_data, vars_to_impute, data, self$results$imputedColsOutput_mice_long)
    },
    
    .runMissRanger = function() {
      data           <- self$data
      vars_to_impute <- self$options$vars_mr
      predictor_vars <- self$options$predictor_vars_mr
      use_mi         <- self$options$use_multiple_imputation_mr
      num_imp        <- self$options$num_imputations_mr
      maxiter        <- self$options$maxiter_mr
      num_trees      <- self$options$num_trees_mr
      pmm_k          <- self$options$pmm_k_mr
      run_imp        <- self$options$run_imputation_mr
      
      show_pattern        <- self$options$show_pattern_plot_mr
      show_upset          <- isTRUE(self$options$show_upset_plot_mr)
      show_pattern_table  <- self$options$show_pattern_table_mr
      show_correlation    <- self$options$show_correlation_plot_mr
      show_density        <- self$options$show_density_plot_mr
      show_strip          <- self$options$show_strip_plot_mr
      show_pooled_summary <- self$options$show_pooled_summary_mr
      
      if (is.null(vars_to_impute) || length(vars_to_impute) == 0) return()
      private$.check_imputation_targets(data, vars_to_impute)
      vars_to_impute <- Filter(function(v) {
        v %in% names(data) && !inherits(data[[v]], "jmvcore.IdVariable")
      }, vars_to_impute)
      if (length(vars_to_impute) == 0) return()
      
      miss_table <- self$results$missingnessTable_mr
      miss_table$setNote(
        "citation",
        "Random forest imputations are performed using non-parametric predictive mean matching. Powered by the R package missRanger."
      )
      
      for (var_name in vars_to_impute) {
        col <- data[[var_name]]
        n_total <- length(col)
        n_miss  <- sum(is.na(col))
        miss_table$addRow(rowKey = var_name, values = list(
          variable    = var_name,
          n_missing   = n_miss,
          pct_missing = (n_miss / n_total) * 100,
          n_valid     = n_total - n_miss,
          method_used = "Random Forest (missRanger)"
        ))
      }
      
      if (isTRUE(show_pattern_table)) {
        private$.fill_pattern_table(data, vars_to_impute, self$results$patternTable_mr)
      }
      
      if (show_pattern) {
        self$results$patternPlot_mr$setState(list(vars = vars_to_impute))
      }
      if (show_upset) {
        self$results$upsetPlot_mr$setState(list(vars = vars_to_impute))
      }
      if (nrow(data) > 10000) {
        miss_table$setNote("large_sample", paste0("Large sample size detected (N = ", nrow(data), "). Recommended settings: PMM with k <= 5 or num.trees <= 100."))
      }
      if (show_correlation) {
        private$.set_correlation_state(data, vars_to_impute, self$results$correlationPlot_mr, self$results$correlationInfo_mr)
      }
      
      if (!isTRUE(run_imp)) return()
      
      all_vars <- unique(c(vars_to_impute, predictor_vars))
      all_vars <- intersect(all_vars, names(data))
      data_to_impute <- data[, all_vars, drop = FALSE]
      
      if (ncol(data_to_impute) < 2) {
        jmvcore::reject("missRanger requires at least 2 variables to perform imputation. Add variables to 'Variables' or 'Predictors'.")
      }
      
      base_seed <- if (isTRUE(self$options$use_seed_mr)) self$options$seed_mr else NULL
      
      m <- if (isTRUE(use_mi)) num_imp else 1
      if (!is.null(base_seed) && as.double(base_seed) + m > .Machine$integer.max) jmvcore::reject("The missRanger seed plus the number of imputations must not exceed 2147483647.", code = "invalid_seed")
      imp_list <- list()
      
      imputed_data_result <- tryCatch({
        vars_formula_bt <- paste0("`", vars_to_impute, "`")
        formula_str <- paste(paste(vars_formula_bt, collapse = " + "), "~ .")
        
        for (i in 1:m) {
          current_seed <- if (!is.null(base_seed)) base_seed + i else NULL
          
          res <- missRanger::missRanger(
            data      = data_to_impute,
            formula   = as.formula(formula_str),
            maxiter   = maxiter,
            num.trees = num_trees,
            pmm.k     = pmm_k,
            seed      = current_seed,
            verbose   = 0
          )
          imp_list[[i]] <- res
        }
        
        agg_res <- data_to_impute
        get_mode <- function(x) {
          ux <- unique(x[!is.na(x)])
          if (length(ux) == 0) return(NA)
          ux[which.max(tabulate(match(x, ux)))]
        }
        
        for (var in vars_to_impute) {
          na_indices <- which(is.na(data_to_impute[[var]]))
          if (length(na_indices) == 0) next
          
          if (is.numeric(data_to_impute[[var]])) {
            mat <- sapply(imp_list, function(df) df[[var]][na_indices])
            if (!is.matrix(mat)) mat <- matrix(mat, nrow = length(na_indices))
            agg_res[[var]][na_indices] <- rowMeans(mat, na.rm = TRUE)
          } else {
            mat <- sapply(imp_list, function(df) as.character(df[[var]][na_indices]))
            if (!is.matrix(mat)) mat <- matrix(mat, nrow = length(na_indices))
            agg_res[[var]][na_indices] <- apply(mat, 1, get_mode)
            
            if (is.factor(data_to_impute[[var]])) {
              agg_res[[var]] <- factor(agg_res[[var]], levels = levels(data_to_impute[[var]]), ordered = is.ordered(data_to_impute[[var]]))
            }
          }
        }
        agg_res
      }, error = function(e) {
        jmvcore::reject(paste("Error running missRanger:", e$message))
      })
      
      if (show_density || show_strip) {
        plot_rows <- private$.imputation_plot_data(data_to_impute, imp_list, vars_to_impute)
        if (show_density) self$results$densityPlot_mr$setState(private$.density_plot_data(plot_rows))
        if (show_strip) self$results$stripPlot_mr$setState(plot_rows)
      }
      
      if (isTRUE(show_pooled_summary)) {
        private$.fill_mr_summary_table(data_to_impute, imputed_data_result, vars_to_impute, self$results$pooledSummaryTable_mr)
      }
      self$results$modelPoolingTable_mr$setNote("single_imputation", NULL)
      if (isTRUE(self$options$enable_model_pooling_mr) && length(imp_list) < 2) {
        if (self$results$modelPoolingTable_mr$rowCount > 0) self$results$modelPoolingTable_mr$deleteRows()
        self$results$modelPoolingTable_mr$setNote("single_imputation", "Model pooling requires at least two completed datasets. Enable multiple imputation with at least two imputations; a single forest imputation is not multiple-imputation inference.")
      }
      if (isTRUE(self$options$enable_model_pooling_mr) && isTRUE(use_mi) && length(imp_list) >= 2) {
        private$.runModelPooling(
          imp_list, data, self$options$model_dep_mr, self$options$model_indep_mr,
          self$options$model_type_mr, self$results$modelPoolingTable_mr
        )
      }
      
      private$.fill_imputed_table(data, imputed_data_result, vars_to_impute, self$results$imputedData_mr)
      
      private$.write_output(imputed_data_result, vars_to_impute, data, self$results$imputedColsOutput_mr)
    },
    
    .fill_pattern_table = function(data, vars_for_pattern, table_ref) {
      if (is.null(vars_for_pattern) || length(vars_for_pattern) == 0) return()
      sub_data <- data[, intersect(vars_for_pattern, names(data)), drop = FALSE]
      if (ncol(sub_data) == 0 || nrow(sub_data) == 0) return()
      
      if (table_ref$rowCount > 0) table_ref$deleteRows()
      
      is_na_mat <- is.na(sub_data)
      pattern_strs <- apply(is_na_mat, 1, function(r) paste(as.integer(r), collapse = ""))
      tab <- sort(table(pattern_strs), decreasing = TRUE)
      
      for (k in seq_along(tab)) {
        pat_key   <- names(tab)[k]
        n_cases   <- as.integer(tab[k])
        pct_cases <- (n_cases / nrow(sub_data)) * 100
        bits      <- as.integer(strsplit(pat_key, "")[[1]])
        miss_vars <- names(sub_data)[bits == 1]
        n_obs     <- sum(bits == 0)
        miss_desc <- if (length(miss_vars) == 0) "None (Complete cases)" else paste(miss_vars, collapse = ", ")
        
        table_ref$addRow(rowKey = k, values = list(
          pattern      = paste0("Pattern ", k),
          n_cases      = n_cases,
          pct_cases    = pct_cases,
          n_observed   = n_obs,
          missing_vars = miss_desc
        ))
      }
    },
    
    .fill_rubin_summary_table_datasets = function(completed_list, original_data, vars_to_impute, table_ref) {
      if (table_ref$rowCount > 0) table_ref$deleteRows()
      table_ref$setNote("summary_unavailable", NULL)
      table_ref$setNote("incomplete_imputations", NULL)
      if (is.null(completed_list) || length(completed_list) == 0) return()
      if (length(completed_list) < 2) {
        table_ref$setNote("summary_unavailable", "Rubin mean summaries require at least two completed datasets.")
        return()
      }
      cont_vars <- Filter(function(v) {
        v %in% names(original_data) && is.numeric(original_data[[v]]) && anyNA(original_data[[v]])
      }, vars_to_impute)
      if (length(cont_vars) == 0) return()
      
      m <- length(completed_list)
      unavailable <- character()
      incomplete <- character()
      for (v in cont_vars) {
        col_orig <- original_data[[v]]
        obs_vals <- col_orig[!is.na(col_orig)]
        n_obs    <- length(obs_vals)
        n_miss   <- sum(is.na(col_orig))
        mean_obs <- if (n_obs > 0) mean(obs_vals) else NA_real_
        sd_obs   <- if (n_obs > 1) stats::sd(obs_vals) else NA_real_
        
        q_vec <- numeric(m)
        u_vec <- numeric(m)
        valid_summary <- TRUE
        for (i in seq_len(m)) {
          y_i      <- completed_list[[i]][[v]]
          if (!is.numeric(y_i) || is.complex(y_i) || !is.null(dim(y_i)) || any(!is.finite(y_i[!is.na(y_i)])) || sum(!is.na(y_i)) < 2) {
            unavailable <- c(unavailable, paste0(v, " (dataset ", i, ")"))
            valid_summary <- FALSE
            break
          }
          q_vec[i] <- mean(y_i, na.rm = TRUE)
          n_comp   <- sum(!is.na(y_i))
          u_vec[i] <- stats::var(y_i, na.rm = TRUE) / n_comp
          if (anyNA(y_i)) incomplete <- c(incomplete, paste0(v, " (dataset ", i, ", N = ", n_comp, ")"))
        }
        if (!valid_summary) next
        
        q_bar     <- mean(q_vec)
        u_bar     <- mean(u_vec)
        b_var     <- if (m > 1) stats::var(q_vec) else 0
        t_var     <- u_bar + (1 + 1/m) * b_var
        se_pooled <- sqrt(max(t_var, 0))
        fmi       <- if (t_var > 0) ((1 + 1/m) * b_var) / t_var else 0
        
        df_rubin  <- if (b_var > 0 && m > 1) (m - 1) * (1 + u_bar / ((1 + 1/m) * b_var))^2 else 1000
        df_used   <- max(df_rubin, 2)
        t_crit    <- stats::qt(0.975, df = df_used)
        ci_lower  <- q_bar - t_crit * se_pooled
        ci_upper  <- q_bar + t_crit * se_pooled
        
        table_ref$addRow(rowKey = v, values = list(
          variable    = v,
          n_obs       = n_obs,
          n_miss      = n_miss,
          mean_obs    = mean_obs,
          sd_obs      = sd_obs,
          mean_pooled = q_bar,
          se_pooled   = se_pooled,
          var_within  = u_bar,
          var_between = b_var,
          fmi         = fmi,
          ci_lower    = ci_lower,
          ci_upper    = ci_upper
        ))
      }
      if (length(unavailable) > 0) table_ref$setNote("summary_unavailable", paste0("Mean summaries omitted for invalid values or fewer than two available observations: ", paste(unavailable, collapse = "; "), "."))
      if (length(incomplete) > 0) table_ref$setNote("incomplete_imputations", paste0("Missing values remain; means and within-imputation variances use the available observations in each dataset, not a fully completed sample: ", paste(incomplete, collapse = "; "), "."))
    },
    
    .fill_mr_summary_table = function(original_data, final_data, vars_to_impute, table_ref) {
      cont_vars <- Filter(function(v) {
        v %in% names(original_data) && is.numeric(original_data[[v]]) && anyNA(original_data[[v]])
      }, vars_to_impute)
      if (length(cont_vars) == 0) return()
      
      if (table_ref$rowCount > 0) table_ref$deleteRows()
      
      for (v in cont_vars) {
        col_orig   <- original_data[[v]]
        obs_vals   <- col_orig[!is.na(col_orig)]
        n_obs      <- length(obs_vals)
        n_miss     <- sum(is.na(col_orig))
        mean_obs   <- if (n_obs > 0) mean(obs_vals) else NA_real_
        sd_obs     <- if (n_obs > 1) stats::sd(obs_vals) else NA_real_
        
        final_vals <- final_data[[v]]
        mean_imp   <- mean(final_vals, na.rm = TRUE)
        sd_imp     <- if (length(final_vals) > 1) stats::sd(final_vals, na.rm = TRUE) else NA_real_
        diff_mean  <- mean_imp - mean_obs
        
        table_ref$addRow(rowKey = v, values = list(
          variable     = v,
          n_obs        = n_obs,
          n_miss       = n_miss,
          mean_obs     = mean_obs,
          sd_obs       = sd_obs,
          mean_imputed = mean_imp,
          sd_imputed   = sd_imp,
          diff_mean    = diff_mean
        ))
      }
    },
    
    .resolve_method = function(var_name, data, imp_method) {
      if (imp_method %in% c("2l.pmm", "2l.continuous")) return(imp_method)
      col <- data[[var_name]]
      if (is.numeric(col)) return(imp_method)
      if (is.factor(col)) {
        if (nlevels(col) == 2) return("logreg")
        return("polyreg")
      }
      return(imp_method)
    },
    
    .build_method_vector = function(data_to_impute, vars_to_impute, imp_method, subject_id_var, group_var) {
      method_vec <- rep("", ncol(data_to_impute))
      names(method_vec) <- names(data_to_impute)
      
      for (v in vars_to_impute) {
        if (!v %in% names(method_vec)) next
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0 && v == subject_id_var) next
        if (!is.null(group_var) && nchar(group_var) > 0 && v == group_var) next
        
        col <- data_to_impute[[v]]
        if (imp_method %in% c("2l.pmm", "2l.continuous")) {
          method_vec[v] <- imp_method
        } else if (is.numeric(col)) {
          method_vec[v] <- imp_method
        } else if (is.factor(col)) {
          method_vec[v] <- if (nlevels(col) == 2) "logreg" else "polyreg"
        } else {
          method_vec[v] <- imp_method
        }
      }
      method_vec
    },
    
    .build_predictor_matrix = function(data_to_impute, vars_to_impute, subject_id_var, time_var, group_var, fixed_covariates, imp_method) {
      pred_matrix <- mice::make.predictorMatrix(data_to_impute)
      
      struct_vars <- unique(c(
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0) subject_id_var,
        if (!is.null(time_var) && nchar(time_var) > 0) time_var,
        if (!is.null(group_var) && nchar(group_var) > 0) group_var
      ))
      struct_vars <- intersect(struct_vars, rownames(pred_matrix))
      if (length(struct_vars) > 0) pred_matrix[struct_vars, ] <- 0
      
      if (imp_method %in% c("2l.pmm", "2l.continuous")) {
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0 && subject_id_var %in% colnames(pred_matrix)) {
          pred_matrix[vars_to_impute, subject_id_var] <- -2
        }
        if (!is.null(time_var) && nchar(time_var) > 0 && time_var %in% colnames(pred_matrix)) {
          pred_matrix[vars_to_impute, time_var] <- 1
        }
      }
      
      if (!is.null(fixed_covariates) && length(fixed_covariates) > 0) {
        fix_vars <- intersect(fixed_covariates, rownames(pred_matrix))
        if (length(fix_vars) > 0) pred_matrix[fix_vars, ] <- 0
      }
      pred_matrix
    },
    
    .restore_wide_order = function(wide_data, wide_to_long_map) {
      subject_codes <- wide_to_long_map$subject_codes
      reconstructed_codes <- wide_data[[wide_to_long_map$subject_id_var]]
      original_data <- wide_to_long_map$original_data
      if (nrow(wide_data) != nrow(original_data) || is.null(reconstructed_codes) ||
          anyNA(reconstructed_codes) || anyDuplicated(reconstructed_codes)) {
        jmvcore::reject("Reconstructed wide participants do not match the original participants.", code = "wide_subject_mismatch")
      }
      original_order <- match(subject_codes, reconstructed_codes)
      if (anyNA(original_order) || anyDuplicated(original_order)) {
        jmvcore::reject("Reconstructed wide participants do not match the original participants.", code = "wide_subject_mismatch")
      }
      wide_data <- wide_data[original_order, , drop = FALSE]
      rownames(wide_data) <- rownames(original_data)
      wide_data
    },

    .check_imputation_targets = function(data, targets, subject_id = NULL) {
      targets <- intersect(targets, names(data))
      id_targets <- targets[vapply(targets, function(variable) {
        identical(attr(data[[variable]], 'jmv-id'), TRUE) ||
          inherits(data[[variable]], 'jmvcore.IdVariable') || variable %in% subject_id
      }, logical(1))]
      if (length(id_targets) > 0) {
        jmvcore::reject('ID variables cannot be imputation targets: {variables}.',
          code = 'id_imputation_target', variables = paste(id_targets, collapse = ', '))
      }
    },

    .with_mice_method = function(method, code) {
      name <- paste0("mice.impute.", method)
      implementation <- utils::getFromNamespace(name, "miceadds")
      present <- exists(name, envir = globalenv(), inherits = FALSE)
      if (present && (bindingIsActive(name, globalenv()) || bindingIsLocked(name, globalenv()))) {
        jmvcore::reject("Cannot temporarily register a miceadds method over an active or locked global binding.", code = "protected_mice_binding")
      }
      previous <- if (present) get(name, envir = globalenv(), inherits = FALSE) else NULL
      adapter <- function(...) {
        arguments <- list(...)
        colnames(arguments$x) <- make.names(colnames(arguments$x), unique = TRUE)
        if (!is.null(names(arguments$type))) names(arguments$type) <- colnames(arguments$x)
        do.call(implementation, arguments)
      }
      assign(name, adapter, envir = globalenv())
      on.exit({
        if (present) {
          assign(name, previous, envir = globalenv())
        } else if (exists(name, envir = globalenv(), inherits = FALSE)) {
          rm(list = name, envir = globalenv())
        }
      }, add = TRUE)
      force(code)
    },

    .pool_imputations = function(mice_obj, data_original, vars_to_impute, completed_list = NULL) {
      get_mode <- function(x) {
        ux <- unique(x[!is.na(x)])
        if (length(ux) == 0) return(NA)
        ux[which.max(tabulate(match(x, ux)))]
      }
      
      if (is.null(completed_list)) completed_list <- lapply(seq_len(mice_obj$m), function(i) mice::complete(mice_obj, action = i))
      pooled <- data_original
      
      for (v in vars_to_impute) {
        na_idx <- which(is.na(data_original[[v]]))
        if (length(na_idx) == 0) next
        
        col_orig <- data_original[[v]]
        if (is.numeric(col_orig)) {
          mat <- sapply(completed_list, function(d) d[[v]][na_idx])
          if (!is.matrix(mat)) mat <- matrix(mat, nrow = length(na_idx))
          pooled[[v]][na_idx] <- rowMeans(mat, na.rm = TRUE)
        } else {
          mat <- sapply(completed_list, function(d) as.character(d[[v]][na_idx]))
          if (!is.matrix(mat)) mat <- matrix(mat, nrow = length(na_idx))
          pooled[[v]][na_idx] <- apply(mat, 1, get_mode)
          if (is.factor(col_orig)) {
            pooled[[v]] <- factor(pooled[[v]], levels = levels(col_orig), ordered = is.ordered(col_orig))
          }
        }
      }
      pooled
    },
    
    .fill_imputed_table = function(original_data, pooled_data, vars_to_impute, table_result_ref) {
      if (table_result_ref$rowCount > 0) table_result_ref$deleteRows()
      
      table_result_ref$setNote("missing_cells", "Only originally missing cells are listed. Numeric aggregates retain full precision; categorical imputations appear in Category. These aggregates are not pooled parameter estimates.")
      row_num <- 1
      for (var_name in vars_to_impute) {
        if (!var_name %in% names(original_data) || !var_name %in% names(pooled_data)) next
        original_col <- original_data[[var_name]]
        imputed_col  <- pooled_data[[var_name]]
        na_idx       <- which(is.na(original_col))
        
        for (i in na_idx) {
          val <- imputed_col[i]
          if (row_num <= 1000) {
            table_result_ref$addRow(rowKey = row_num, values = list(
              variable  = var_name,
              imputed   = if (is.numeric(val)) as.numeric(val) else NA_real_,
              imputedLabel = if (is.numeric(val)) "" else as.character(val),
              rowNumber = as.integer(rownames(original_data)[i])
            ))
            row_num <- row_num + 1
          } else if (row_num == 1001) {
            table_result_ref$addRow(rowKey = row_num, values = list(
              variable  = "...",
              imputed   = NA_real_,
              imputedLabel = "Table truncated (max 1000 rows)",
              rowNumber = NA_integer_
            ))
            row_num <- row_num + 1
          }
        }
      }
    },
    
    .set_correlation_state = function(data, vars_to_impute, plot_ref, info_ref) {
      plot_ref$setState(NULL)
      plot_ref$setVisible(TRUE)
      info_ref$setContent("Missingness correlation heatmap unavailable: at least two variables must each have both observed and missing values, with at least two rows.")
      info_ref$setVisible(TRUE)
      sub_data <- data[, intersect(vars_to_impute, names(data)), drop = FALSE]
      if (nrow(sub_data) < 2 || ncol(sub_data) < 2) return()
      miss_ind <- as.data.frame(lapply(sub_data, function(x) as.integer(is.na(x))))
      valid_cols <- vapply(miss_ind, function(x) stats::var(x) > 0, logical(1))
      if (sum(valid_cols) < 2) return()
      cor_mat <- stats::cor(miss_ind[, valid_cols, drop = FALSE])
      if (!all(is.finite(cor_mat))) return()
      plot_ref$setState(cor_mat)
      info_ref$setContent("")
      info_ref$setVisible(FALSE)
    },
    
    .write_output = function(pooled_data, vars_to_impute, original_data, output_ref) {
      if (!output_ref$isNotFilled()) return()
      
      keys   <- character()
      titles <- character()
      mtypes <- character()
      
      for (v in vars_to_impute) {
        if (!v %in% names(pooled_data)) next
        keys   <- c(keys, v)
        titles <- c(titles, paste0(v, "_imputed"))
        
        orig_col <- original_data[[v]]
        mtype <- if (is.numeric(orig_col)) "continuous"
        else if (is.factor(orig_col) && is.ordered(orig_col)) "ordinal"
        else if (is.factor(orig_col)) "nominal"
        else "continuous"
        mtypes <- c(mtypes, mtype)
      }
      
      if (length(keys) == 0) return()
      
      output_ref$set(
        keys         = keys,
        titles       = titles,
        descriptions = rep("Single aggregated dataset; numeric means and categorical modes; imputation uncertainty is not represented", length(keys)),
        measureTypes = mtypes
      )
      
      for (k in keys) {
        vec      <- pooled_data[[k]]
        orig_col <- original_data[[k]]
        if (is.factor(orig_col) && is.factor(vec)) {
          vec <- factor(as.character(vec), levels = levels(orig_col), ordered = is.ordered(orig_col))
        }
        output_ref$setValues(key = k, values = vec)
      }
      
      if (nrow(original_data) > 0) {
        output_ref$setRowNums(rownames(original_data))
      }
    },
    
    .renderPatternPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_vim_pattern(image)
    },
    .renderCorrelationPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_ggcorr_heatmap(image, ggtheme)
    },
    .renderTracePlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_mice_trace(image)
    },
    .renderDensityPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_mice_density(image, ggtheme)
    },
    .renderStripPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_mice_strip(image, ggtheme)
    },
    
    .renderPatternPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_vim_pattern(image)
    },
    .renderCorrelationPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_ggcorr_heatmap(image, ggtheme)
    },
    .renderTracePlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_mice_trace(image)
    },
    .renderDensityPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_mice_density(image, ggtheme)
    },
    .renderStripPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_mice_strip(image, ggtheme)
    },
    
    .renderPatternPlotMR = function(image, ggtheme, theme, ...) {
      private$.draw_vim_pattern(image)
    },
    .renderCorrelationPlotMR = function(image, ggtheme, theme, ...) {
      private$.draw_ggcorr_heatmap(image, ggtheme)
    },
    .renderDensityPlotMR = function(image, ggtheme, theme, ...) {
      private$.draw_mr_density(image, ggtheme)
    },
    .renderStripPlotMR = function(image, ggtheme, theme, ...) {
      private$.draw_mr_strip(image, ggtheme)
    },

    .renderUpsetPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_upset_plot(image, ggtheme)
    },
    .renderUpsetPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_upset_plot(image, ggtheme)
    },
    .renderUpsetPlotMR = function(image, ggtheme, theme, ...) {
      private$.draw_upset_plot(image, ggtheme)
    },
    
    .draw_upset_plot = function(image, ggtheme) {
      if (is.null(image$state)) return(FALSE)
      state <- image$state
      data <- self$data
      vars <- intersect(state$vars, names(data))
      if (length(vars) < 2) return(FALSE)
      
      sub_data <- data[, vars, drop = FALSE]
      is_na_mat <- is.na(sub_data)
      if (!any(is_na_mat)) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 1, y = 1, label = "No missing values observed in the selected variables.", size = 4.5, color = "#555555") +
          ggtheme + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
        print(p)
        return(TRUE)
      }
      
      patterns <- apply(is_na_mat, 1, function(row) paste(as.integer(row), collapse = ""))
      pat_counts <- sort(table(patterns), decreasing = TRUE)
      
      top_n <- min(15, length(pat_counts))
      top_pats <- names(pat_counts)[1:top_n]
      counts <- as.integer(pat_counts[1:top_n])
      
      top_df <- data.frame(
        Pattern = factor(seq_len(top_n)),
        Count   = counts
      )
      
      p_list <- list()
      line_list <- list()
      for (i in seq_len(top_n)) {
        bits <- as.integer(strsplit(top_pats[i], "")[[1]])
        active_vars <- which(bits == 1)
        for (v_idx in seq_along(vars)) {
          p_list[[length(p_list) + 1]] <- data.frame(
            Pattern = factor(i, levels = seq_len(top_n)),
            Variable = factor(vars[v_idx], levels = rev(vars)),
            IsMissing = (bits[v_idx] == 1)
          )
        }
        if (length(active_vars) > 1) {
          line_list[[length(line_list) + 1]] <- data.frame(
            Pattern = factor(i, levels = seq_len(top_n)),
            ymin = factor(vars[max(active_vars)], levels = rev(vars)),
            ymax = factor(vars[min(active_vars)], levels = rev(vars))
          )
        }
      }
      mat_df <- do.call(rbind, p_list)
      
      p_top <- ggplot2::ggplot(top_df, ggplot2::aes(x = Pattern, y = Count)) +
        ggplot2::geom_col(fill = "#1a5f7a", width = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.4, size = 3.2) +
        ggplot2::labs(title = "Missing Data Pattern Intersections (UpSet Plot)", y = "Intersection Size") +
        ggtheme +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold", size = 12, color = "#1a5f7a"),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
      
      p_bot <- ggplot2::ggplot(mat_df, ggplot2::aes(x = Pattern, y = Variable)) +
        ggplot2::geom_point(ggplot2::aes(color = IsMissing, size = IsMissing)) +
        ggplot2::scale_color_manual(values = c("TRUE" = "#1a5f7a", "FALSE" = "#d3d3d3"), guide = "none") +
        ggplot2::scale_size_manual(values = c("TRUE" = 4.2, "FALSE" = 2.5), guide = "none") +
        ggplot2::labs(x = "Pattern Combinations", y = NULL) +
        ggtheme +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = "#f0f0f0"),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(face = "bold", size = 10)
        )
      
      if (length(line_list) > 0) {
        lines_df <- do.call(rbind, line_list)
        p_bot <- p_bot + ggplot2::geom_linerange(data = lines_df, ggplot2::aes(x = Pattern, ymin = ymin, ymax = ymax), color = "#1a5f7a", linewidth = 1.1, inherit.aes = FALSE)
      }
      
      combined <- cowplot::plot_grid(p_top, p_bot, ncol = 1, align = "v", rel_heights = c(1.2, 1))
      print(combined)
      TRUE
    },
    
    .runModelPooling = function(completed_list, original_data, dep_var, indep_vars, model_type, table_ref) {
      if (table_ref$rowCount > 0) table_ref$deleteRows()
      table_ref$setNote("pooling_unavailable", NULL)
      table_ref$setNote("citation", NULL)
      table_ref$setNote("logistic_response", NULL)
      unavailable <- function(message) table_ref$setNote("pooling_unavailable", paste0("Model pooling unavailable: ", message))
      if (is.null(completed_list) || length(completed_list) == 0) {
        unavailable("no completed datasets are available.")
        return()
      }
      if (is.null(dep_var) || length(dep_var) != 1 || is.null(indep_vars) || length(indep_vars) == 0) {
        unavailable("select one dependent variable and at least one predictor.")
        return()
      }
      m <- length(completed_list)
      if (m < 2) {
        unavailable("at least two completed datasets are required for multiple-imputation inference.")
        return()
      }
      required_vars <- unique(c(dep_var, indep_vars))
      for (index in seq_len(m)) {
        missing_vars <- setdiff(required_vars, names(completed_list[[index]]))
        if (length(missing_vars) > 0) {
          unavailable(paste0("dataset ", index, " is missing variables: ", paste(missing_vars, collapse = ", "), "."))
          return()
        }
      }
      if (model_type == "logit") {
        binary_kind <- function(response) {
          if (!is.null(dim(response))) return("")
          if (is.factor(response)) return("factor")
          if (is.logical(response)) return("logical")
          if (is.numeric(response) && !is.complex(response)) return("numeric")
          ""
        }
        observed_values <- function(response) unique(as.character(response[!is.na(response)]))
        original_response <- original_data[[dep_var]]
        response_kind <- binary_kind(original_response)
        response_levels <- switch(response_kind,
          factor = levels(droplevels(original_response)),
          logical = c("FALSE", "TRUE"),
          numeric = c("0", "1"),
          character()
        )
        if (length(response_levels) != 2 || anyNA(response_levels) || !setequal(observed_values(original_response), response_levels)) {
          unavailable("binary logistic regression requires two observed response categories in the original data: a two-category factor, logical FALSE/TRUE, or numeric 0/1.")
          return()
        }
      }
      quote_names <- function(names) vapply(names, function(name) paste(deparse(as.name(name), backtick = TRUE), collapse = ""), character(1))
      indep_bt <- quote_names(indep_vars)
      dep_bt <- quote_names(dep_var)
      fmla <- stats::as.formula(paste(dep_bt, "~", paste(indep_bt, collapse = " + ")))
      
      q_list <- list()
      u_list <- list()
      term_names <- NULL
      nu_com <- NULL
      
      for (i in seq_len(m)) {
        df_i <- completed_list[[i]]
        if (model_type == "logit") {
          response <- df_i[[dep_var]]
          if (!identical(binary_kind(response), response_kind) || !setequal(observed_values(response), response_levels)) {
            unavailable(paste0("binary response categories and type must match the original data in dataset ", i, "."))
            return()
          }
          if (response_kind == "factor") df_i[[dep_var]] <- factor(as.character(response), levels = response_levels)
          fitting_rows <- stats::complete.cases(df_i[, required_vars, drop = FALSE])
          if (length(unique(df_i[[dep_var]][fitting_rows])) != 2) {
            unavailable(paste0("binary logistic regression requires both response categories among the complete model cases in dataset ", i, "."))
            return()
          }
        }
        fit_i <- tryCatch({
          if (model_type == "logit") {
            stats::glm(fmla, family = stats::binomial(), data = df_i)
          } else {
            stats::lm(fmla, data = df_i)
          }
        }, error = function(e) NULL)
        
        if (is.null(fit_i) || (model_type == "logit" && !isTRUE(fit_i$converged))) {
          unavailable(paste0("model fitting failed or did not converge in dataset ", i, "."))
          return()
        }
        residual_df <- stats::df.residual(fit_i)
        if (length(residual_df) != 1 || !is.finite(residual_df) || residual_df <= 0) {
          unavailable(paste0("positive residual degrees of freedom are unavailable in dataset ", i, "."))
          return()
        }
        if (is.null(nu_com)) nu_com <- residual_df
        if (residual_df != nu_com) {
          unavailable(paste0("residual degrees of freedom differ across completed datasets (dataset ", i, ")."))
          return()
        }
        coef_tab <- tryCatch(stats::coef(summary(fit_i)), error = function(e) NULL)
        if (is.null(coef_tab) || nrow(coef_tab) == 0 || any(!is.finite(coef_tab[, 1:2, drop = FALSE]))) {
          unavailable(paste0("coefficient estimates or standard errors are unavailable in dataset ", i, "."))
          return()
        }
        if (is.null(term_names)) term_names <- rownames(coef_tab)
        if (!identical(term_names, rownames(coef_tab))) {
          unavailable(paste0("model terms differ across completed datasets (dataset ", i, ")."))
          return()
        }
        
        q_list[[i]] <- coef_tab[, 1]
        u_list[[i]] <- (coef_tab[, 2])^2
      }
      
      q_mat <- do.call(rbind, q_list)
      u_mat <- do.call(rbind, u_list)
      
      for (k in seq_along(term_names)) {
        t_name <- term_names[k]
        q_k <- q_mat[, k]
        u_k <- u_mat[, k]
        
        q_bar <- mean(q_k)
        u_bar <- mean(u_k)
        b_var <- if (m > 1) stats::var(q_k) else 0
        t_var <- u_bar + (1 + 1/m) * b_var
        if (!is.finite(t_var) || t_var <= 0) {
          if (table_ref$rowCount > 0) table_ref$deleteRows()
          unavailable(paste0("a positive finite total variance is required for term ", t_name, "."))
          return()
        }
        se_pooled <- sqrt(max(t_var, 0))
        
        rvi <- if (u_bar > 0) ((1 + 1/m) * b_var) / u_bar else 0
        lambda <- if (t_var > 0) ((1 + 1/m) * b_var) / t_var else 0
        
        observed_component <- (1 - lambda) * (1 + nu_com) * nu_com
        nu_adj <- (m - 1) * observed_component / ((nu_com + 3) * (m - 1) + lambda^2 * observed_component)
        if (!is.finite(nu_adj) || nu_adj <= 0) {
          if (table_ref$rowCount > 0) table_ref$deleteRows()
          unavailable(paste0("positive finite pooled degrees of freedom are unavailable for term ", t_name, "."))
          return()
        }
        
        stat_val <- if (se_pooled > 0) q_bar / se_pooled else NA_real_
        p_val <- if (!is.na(stat_val)) 2 * stats::pt(abs(stat_val), df = nu_adj, lower.tail = FALSE) else NA_real_
        
        crit_t <- stats::qt(0.975, df = nu_adj)
        ci_lower <- q_bar - crit_t * se_pooled
        ci_upper <- q_bar + crit_t * se_pooled
        
        fmi <- (rvi + 2 / (nu_adj + 3)) / (1 + rvi)
        
        table_ref$addRow(rowKey = t_name, values = list(
          term      = t_name,
          estimate  = q_bar,
          std_error = se_pooled,
          statistic = stat_val,
          df        = nu_adj,
          p_value   = p_val,
          ci_lower  = ci_lower,
          ci_upper  = ci_upper,
          rvi       = rvi,
          fmi       = fmi
        ))
      }
      
      table_ref$setNote(
        "citation",
        "Pooled parameter estimates and standard errors computed across m imputations using Rubin's rules with small-sample degrees of freedom adjustment."
      )
      if (model_type == "logit") {
        table_ref$setNote("logistic_response", paste0("Binary logistic response ", dep_var, ": event (1) = ", response_levels[2], "; reference (0) = ", response_levels[1], ". Coding is held constant across imputations; estimates and intervals are on the log-odds scale."))
      }
    },
    
    .draw_vim_pattern = function(image) {
      if (is.null(image$state)) return(FALSE)
      state <- image$state
      plot_data <- self$data[, intersect(state$vars, names(self$data)), drop = FALSE]
      if (ncol(plot_data) == 0) return(FALSE)
      
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par))
      graphics::par(bg = "transparent")
      
      VIM::aggr(
        plot_data,
        numbers  = TRUE,
        sortVars = TRUE,
        prop     = c(TRUE, TRUE),
        gap      = 3,
        col      = c("#9ecae1", "#fc9272"),
        cex.axis = 0.8,
        ylab     = c("Proportion Missing", "Combination Pattern")
      )
      TRUE
    },
    
    .draw_ggcorr_heatmap = function(image, ggtheme) {
      if (is.null(image$state) || !is.matrix(image$state)) return(FALSE)
      cor_matrix <- image$state
      if (nrow(cor_matrix) < 2 || ncol(cor_matrix) < 2) return(FALSE)
      
      p <- GGally::ggcorr(
        data = NULL, cor_matrix = cor_matrix,
        label = TRUE, label_round = 2,
        hjust = 0.9, layout.exp = 3, label_size = 3.5
      ) +
        ggtheme +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y      = ggplot2::element_text(size = 10),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    },
    
    .imputation_plot_data = function(original, completed, vars) {
      plot_rows <- list()
      for (variable in intersect(vars, names(original))) {
        values <- original[[variable]]
        if (!is.numeric(values) || !anyNA(values)) next
        missing <- is.na(values)
        plot_rows[[length(plot_rows) + 1L]] <- data.frame(variable = rep(variable, sum(!missing)), value = values[!missing], imp = rep(0L, sum(!missing)), type = rep("Observed", sum(!missing)))
        for (index in seq_along(completed)) {
          plot_rows[[length(plot_rows) + 1L]] <- data.frame(variable = variable, value = completed[[index]][[variable]][missing], imp = index, type = "Imputed")
        }
      }
      if (length(plot_rows) == 0) return(data.frame(variable = character(), value = numeric(), imp = integer(), type = character()))
      do.call(rbind, plot_rows)
    },

    .density_plot_data = function(plot_rows) {
      vars <- unique(plot_rows$variable)
      keep <- vapply(vars, function(variable) {
        types <- plot_rows$type[plot_rows$variable == variable]
        sum(types == "Observed") >= 2 && sum(types == "Imputed") >= 2
      }, logical(1))
      plot_rows[plot_rows$variable %in% vars[keep], c("value", "type", "variable"), drop = FALSE]
    },

    .draw_mice_trace = function(image) {
      if (is.null(image$state)) return(FALSE)
      trace <- image$state
      imputed_vars <- trace$vars
      if (length(imputed_vars) == 0) return(FALSE)
      
      n_vars <- length(imputed_vars)
      n_cols <- 2
      n_rows <- ceiling(n_vars / n_cols)
      
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par))
      graphics::par(mfrow = c(n_rows * 2, n_cols), mar = c(3, 3, 2, 1), bg = "transparent")
      
      chainMean <- trace$chainMean
      chainVar  <- trace$chainVar
      
      for (v in imputed_vars) {
        if (!v %in% dimnames(chainMean)[[1]]) next
        mean_mat <- matrix(chainMean[v, , , drop = FALSE], nrow = dim(chainMean)[2], ncol = dim(chainMean)[3])
        var_mat <- matrix(chainVar[v, , , drop = FALSE], nrow = dim(chainVar)[2], ncol = dim(chainVar)[3])
        
        matplot(
          mean_mat, type = "l", lty = 1,
          col = grDevices::rainbow(ncol(mean_mat)),
          main = paste0(v, " \u2014 Iteration Mean"),
          xlab = "Iteration", ylab = "Mean", cex.main = 0.9
        )
        
        sd_mat <- sqrt(pmax(var_mat, 0))
        if (any(is.finite(sd_mat))) {
          matplot(
            sd_mat, type = "l", lty = 1,
            col = grDevices::rainbow(ncol(sd_mat)),
            main = paste0(v, " \u2014 Iteration SD"),
            xlab = "Iteration", ylab = "SD", cex.main = 0.9
          )
        } else {
          graphics::plot.new()
          graphics::title(main = paste0(v, " \u2014 SD N/A"), cex.main = 0.9)
        }
      }
      TRUE
    },
    
    .draw_mice_density = function(image, ggtheme) {
      if (is.null(image$state)) return(FALSE)
      plot_data <- image$state
      if (nrow(plot_data) == 0) return(FALSE)
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, colour = type, fill = type)) +
        ggplot2::geom_density(alpha = 0.25, linewidth = 0.7) +
        ggplot2::facet_wrap(~ variable, scales = "free") +
        ggplot2::scale_colour_manual(values = c("Observed" = "#3182bd", "Imputed" = "#de2d26")) +
        ggplot2::scale_fill_manual(values = c("Observed" = "#3182bd", "Imputed" = "#de2d26")) +
        ggplot2::labs(x = "Value", y = "Density", colour = NULL, fill = NULL) +
        ggtheme +
        ggplot2::theme(
          legend.position  = "top",
          strip.background = ggplot2::element_rect(fill = "#f0f0f0"),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    },
    
    .draw_mice_strip = function(image, ggtheme) {
      if (is.null(image$state)) return(FALSE)
      plot_data <- image$state
      if (nrow(plot_data) == 0) return(FALSE)
      plot_data$imp  <- factor(plot_data$imp)
      plot_data$type <- factor(plot_data$type, levels = c("Observed", "Imputed"))
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = imp, y = value, colour = type, shape = type)) +
        ggplot2::geom_jitter(width = 0.18, alpha = 0.65, size = 1.6) +
        ggplot2::facet_wrap(~ variable, scales = "free_y") +
        ggplot2::scale_colour_manual(values = c("Observed" = "#3182bd", "Imputed" = "#de2d26")) +
        ggplot2::scale_shape_manual(values = c("Observed" = 21, "Imputed" = 20)) +
        ggplot2::labs(x = "Imputation (0 = Observed)", y = "Value", colour = NULL, shape = NULL) +
        ggtheme +
        ggplot2::theme(
          legend.position  = "top",
          strip.background = ggplot2::element_rect(fill = "#f0f0f0"),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    },
    
    .draw_mr_density = function(image, ggtheme) {
      private$.draw_mice_density(image, ggtheme)
    },

    .draw_mr_strip = function(image, ggtheme) {
      private$.draw_mice_strip(image, ggtheme)
    }
  )
)

