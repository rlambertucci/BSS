dataimputationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "dataimputationClass",
  inherit = dataimputationBase,
  private = list(
    
    .mice_cs_result = NULL,
    .mice_long_result = NULL,
    
    .init = function() {
      info_cs <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.6; font-family: sans-serif;">',
        '<h4 style="margin-top: 0; color: #1a5f7a;">Multivariate Imputation by Chained Equations (Cross-sectional)</h4>',
        '<ul>',
        '<li><b>Methodology:</b> Imputes missing values by modeling each incomplete variable conditionally on the others ',
        'using univariate regression models iteratively <i>(van Buuren & Groothuis-Oudshoorn, 2011)</i>.</li>',
        '<li><b>Algorithms:</b> <b>pmm</b> (Predictive Mean Matching — robust, default), ',
        '<b>norm</b> (Bayesian linear regression), <b>logreg</b> (binary logistic regression), ',
        'and <b>polyreg</b> (polytomous regression for nominal categories).</li>',
        '<li><b>Aggregation:</b> Pools <i>m</i> simulated datasets by calculating the <b>Mean</b> for continuous ',
        'variables and the <b>Mode</b> for categorical variables across iterations.</li>',
        '</ul>',
        '</div>'
      )
      self$results$imputationInfo_mice_cs$setContent(info_cs)
      
      info_long <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.6; font-family: sans-serif;">',
        '<h4 style="margin-top: 0; color: #1a5f7a;">Multilevel Imputation by Chained Equations (Longitudinal)</h4>',
        '<ul>',
        '<li><b>Methodology:</b> Supports longitudinal and nested datasets by utilizing random-effects ',
        'linear mixed models <i>(miceadds package)</i>. Clustered variance is correctly accounted for.</li>',
        '<li><b>Algorithms:</b> <b>2l.pmm</b> (Two-level Predictive Mean Matching) and ',
        '<b>2l.continuous</b> (Two-level normal imputation for continuous variables).</li>',
        '<li><b>Formats:</b> Automatically reshapes <b>Wide</b> data (repeated measures) into <b>Long</b> format ',
        'for multi-level modeling, and converts it back for seamless planilha saving.</li>',
        '</ul>',
        '</div>'
      )
      self$results$imputationInfo_mice_long$setContent(info_long)
      
      info_mr <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.6; font-family: sans-serif;">',
        '<h4 style="margin-top: 0; color: #1a5f7a;">Non-parametric Random Forest Imputation (missRanger)</h4>',
        '<ul>',
        '<li><b>Methodology:</b> A fast, non-parametric imputation method that combines Breimans Random Forest ',
        'with the iterative chained equations framework of MICE <i>(Mayer, 2019)</i>.</li>',
        '<li><b>Predictive Mean Matching:</b> Integrates PMM donors (nearest <i>k</i> cases) to prevent out-of-bounds ',
        'predictions and ensure natural, realistic imputed distributions.</li>',
        '<li><b>Multiple Imputation:</b> Runs <i>N</i> independent forests with deterministic seeds (Seed + i) to ',
        'create an ensemble. Final values are aggregated via Mean/Mode.</li>',
        '</ul>',
        '</div>'
      )
      self$results$imputationInfo_mr$setContent(info_mr)
    },
    
    .run = function() {
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
      seed_str       <- self$options$seed_mice_cs
      run_imp        <- self$options$run_imputation_mice_cs
      
      show_pattern     <- self$options$show_pattern_plot_mice_cs
      show_correlation <- self$options$show_correlation_plot_mice_cs
      show_trace       <- self$options$show_trace_plot_mice_cs
      show_density     <- self$options$show_density_plot_mice_cs
      show_strip       <- self$options$show_strip_plot_mice_cs
      
      if (is.null(vars_to_impute) || length(vars_to_impute) == 0) return()
      
      vars_to_impute <- Filter(function(v) {
        v %in% names(data) && !inherits(data[[v]], "jmvcore.IdVariable")
      }, vars_to_impute)
      if (length(vars_to_impute) == 0) return()
      
      miss_table <- self$results$missingnessTable_mice_cs
      miss_table$setNote(
        "citation",
        "Note. Imputations are performed using the multivariate imputation by chained equations (MICE) methodology. Powered by the R package mice. Citation: van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. https://doi.org/10.18637/jss.v045.i03"
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
      
      if (show_pattern) {
        self$results$patternPlot_mice_cs$setState(list(data = data, vars = vars_to_impute))
      }
      if (show_correlation) {
        private$.set_correlation_state(data, vars_to_impute, self$results$correlationPlot_mice_cs)
      }
      
      if (!isTRUE(run_imp)) return()
      
      all_vars <- unique(c(vars_to_impute, predictor_vars))
      all_vars <- intersect(all_vars, names(data))
      data_to_impute <- data[, all_vars, drop = FALSE]
      
      if (ncol(data_to_impute) < 2) {
        jmvcore::reject("MICE requires at least 2 variables to perform imputation. Add variables to 'Variables' or 'Predictors'.")
      }
      
      base_seed <- suppressWarnings(as.numeric(seed_str))
      mice_seed <- if (!is.na(base_seed)) base_seed else NA
      
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
      
      private$.mice_cs_result <- mice_obj
      
      if (show_trace)   self$results$tracePlot_mice_cs$setState(mice_obj)
      if (show_density) self$results$densityPlot_mice_cs$setState(mice_obj)
      if (show_strip)   self$results$stripPlot_mice_cs$setState(list(mice_obj = mice_obj, vars = vars_to_impute))
      
      pooled_data <- private$.pool_imputations(mice_obj, data_to_impute, vars_to_impute)
      
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
      seed_str       <- self$options$seed_mice_long
      run_imp        <- self$options$run_imputation_mice_long
      
      data_format      <- self$options$data_format_mice_long
      subject_id_var   <- self$options$subject_id_var_mice_long
      time_var         <- self$options$time_var_mice_long
      group_var        <- self$options$group_var_mice_long
      fixed_covariates <- self$options$fixed_covariates_mice_long
      wave_vars        <- self$options$wave_vars_mice_long
      
      show_pattern     <- self$options$show_pattern_plot_mice_long
      show_correlation <- self$options$show_correlation_plot_mice_long
      show_trace       <- self$options$show_trace_plot_mice_long
      show_density     <- self$options$show_density_plot_mice_long
      show_strip       <- self$options$show_strip_plot_mice_long
      
      vars_for_miss_table <- if (data_format == "wide") wave_vars else vars_to_impute
      
      if (!is.null(vars_for_miss_table) && length(vars_for_miss_table) > 0) {
        vars_for_miss_table <- Filter(function(v) {
          v %in% names(data) && !inherits(data[[v]], "jmvcore.IdVariable")
        }, vars_for_miss_table)
      }
      
      if (is.null(vars_for_miss_table) || length(vars_for_miss_table) == 0) return()
      
      miss_table <- self$results$missingnessTable_mice_long
      miss_table$setNote(
        "citation",
        "Note. Longitudinal imputations are performed using multilevel multivariate imputation by chained equations. Powered by the R packages mice and miceadds. Citations: (1) van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. (2) Robitzsch, A., & Grund, S. (2024). miceadds: Some Additional Multiple Imputation Functions for mice."
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
      
      if (show_pattern) {
        self$results$patternPlot_mice_long$setState(list(data = data, vars = vars_for_miss_table))
      }
      if (show_correlation) {
        private$.set_correlation_state(data, vars_for_miss_table, self$results$correlationPlot_mice_long)
      }
      
      wide_to_long_map <- NULL
      self$results$missingnessTable_mice_long$setNote("wide_req", NULL)
      
      if (data_format == "wide") {
        if (length(wave_vars) < 2) {
          self$results$missingnessTable_mice_long$setNote("wide_req", "Note. Wide format requires at least two columns in 'Wave Columns'.")
          return()
        }
        if (is.null(subject_id_var) || nchar(subject_id_var) == 0) {
          self$results$missingnessTable_mice_long$setNote("wide_req", "Note. Wide format requires a Subject ID variable to link repeated measures.")
          return()
        }
        
        wide_to_long_map <- list(wave_vars = wave_vars, subject_id_var = subject_id_var, original_data = data)
        data_wide         <- data
        data_wide$.row_id <- seq_len(nrow(data_wide))
        
        data_long <- tryCatch({
          stats::reshape(
            data_wide,
            varying   = wave_vars,
            v.names   = "wave_value",
            timevar   = ".wave",
            times     = wave_vars,
            direction = "long"
          )
        }, error = function(e) {
          jmvcore::reject(paste("Reshape Wide-to-Long error:", e$message))
        })
        data_long <- data_long[order(data_long[[subject_id_var]], data_long$.wave), ]
        rownames(data_long) <- NULL
        
        data           <- data_long
        vars_to_impute <- "wave_value"
      }
      
      if (!isTRUE(run_imp)) return()
      
      all_vars <- unique(c(
        vars_to_impute,
        if (!is.null(predictor_vars)) predictor_vars,
        if (!is.null(fixed_covariates)) fixed_covariates,
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0) subject_id_var,
        if (!is.null(time_var) && nchar(time_var) > 0) time_var,
        if (!is.null(group_var) && nchar(group_var) > 0) group_var,
        if (!is.null(wide_to_long_map)) ".wave"
      ))
      all_vars <- intersect(all_vars, names(data))
      data_to_impute <- data[, all_vars, drop = FALSE]
      
      if (ncol(data_to_impute) < 2) {
        jmvcore::reject("MICE requires at least 2 variables (including predictors/structurals) to perform imputation.")
      }
      
      if (!is.null(subject_id_var) && nchar(subject_id_var) > 0 && subject_id_var %in% names(data_to_impute)) {
        data_to_impute[[subject_id_var]] <- as.integer(data_to_impute[[subject_id_var]])
      }
      
      base_seed <- suppressWarnings(as.numeric(seed_str))
      mice_seed <- if (!is.na(base_seed)) base_seed else NA
      
      method_vec <- private$.build_method_vector(data_to_impute, vars_to_impute, imp_method, subject_id_var, group_var)
      effective_time_var <- if (!is.null(wide_to_long_map)) ".wave" else time_var
      pred_matrix <- private$.build_predictor_matrix(
        data_to_impute, vars_to_impute, subject_id_var, effective_time_var, group_var, fixed_covariates, imp_method
      )
      
      mice_obj <- tryCatch({
        if (requireNamespace("miceadds", quietly = TRUE)) {
          fn_name <- paste0("mice.impute.", imp_method)
          fn <- tryCatch(utils::getFromNamespace(fn_name, "miceadds"), error = function(e) NULL)
          if (!is.null(fn)) assign(fn_name, fn, envir = globalenv())
        }
        
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
        jmvcore::reject(paste("MICE Longitudinal multilevel error:", e$message))
      })
      
      private$.mice_long_result <- mice_obj
      
      if (show_trace)   self$results$tracePlot_mice_long$setState(mice_obj)
      if (show_density) self$results$densityPlot_mice_long$setState(mice_obj)
      if (show_strip)   self$results$stripPlot_mice_long$setState(list(mice_obj = mice_obj, vars = vars_to_impute))
      
      pooled_data <- private$.pool_imputations(mice_obj, data_to_impute, vars_to_impute)
      
      if (!is.null(wide_to_long_map)) {
        pooled_data <- tryCatch({
          wide_res <- stats::reshape(pooled_data, idvar = subject_id_var, timevar = ".wave", v.names = "wave_value", direction = "wide")
          for (wv in wave_vars) {
            col_new <- paste0("wave_value.", wv)
            if (col_new %in% names(wide_res)) {
              names(wide_res)[names(wide_res) == col_new] <- wv
            }
          }
          wide_res
        }, error = function(e) {
          jmvcore::reject(paste("Reshape Long-to-Wide error:", e$message))
        })
        vars_to_impute <- wave_vars
        data           <- wide_to_long_map$original_data
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
      seed_str       <- self$options$seed_mr
      run_imp        <- self$options$run_imputation_mr
      
      show_pattern     <- self$options$show_pattern_plot_mr
      show_correlation <- self$options$show_correlation_plot_mr
      
      if (is.null(vars_to_impute) || length(vars_to_impute) == 0) return()
      vars_to_impute <- Filter(function(v) {
        v %in% names(data) && !inherits(data[[v]], "jmvcore.IdVariable")
      }, vars_to_impute)
      if (length(vars_to_impute) == 0) return()
      
      miss_table <- self$results$missingnessTable_mr
      miss_table$setNote(
        "citation",
        "Note. Random forest imputations are performed using non-parametric predictive mean matching. Powered by the R package missRanger. Citation: Mayer, M. (2019). missRanger: Fast Imputation of Missing Values. R package version 2.1.0."
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
      
      if (show_pattern) {
        self$results$patternPlot_mr$setState(list(data = data, vars = vars_to_impute))
      }
      if (show_correlation) {
        private$.set_correlation_state(data, vars_to_impute, self$results$correlationPlot_mr)
      }
      
      if (!isTRUE(run_imp)) return()
      
      all_vars <- unique(c(vars_to_impute, predictor_vars))
      all_vars <- intersect(all_vars, names(data))
      data_to_impute <- data[, all_vars, drop = FALSE]
      
      if (ncol(data_to_impute) < 2) {
        jmvcore::reject("missRanger requires at least 2 variables to perform imputation. Add variables to 'Variables' or 'Predictors'.")
      }
      
      base_seed <- suppressWarnings(as.numeric(seed_str))
      base_seed <- if (!is.na(base_seed)) base_seed else NULL
      
      imputed_data_result <- tryCatch({
        vars_formula_bt <- paste0("`", vars_to_impute, "`")
        formula_str <- paste(paste(vars_formula_bt, collapse = " + "), "~ .")
        
        m <- if (isTRUE(use_mi)) num_imp else 1
        imp_list <- list()
        
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
      
      private$.fill_imputed_table(data, imputed_data_result, vars_to_impute, self$results$imputedData_mr)
      
      private$.write_output(imputed_data_result, vars_to_impute, data, self$results$imputedColsOutput_mr)
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
        if (!is.null(group_var) && nchar(group_var) > 0) group_var,
        ".wave", ".row_id", "id"
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
        if (".wave" %in% colnames(pred_matrix)) {
          pred_matrix[vars_to_impute, ".wave"] <- 1
        }
      }
      
      if (!is.null(fixed_covariates) && length(fixed_covariates) > 0) {
        fix_vars <- intersect(fixed_covariates, rownames(pred_matrix))
        if (length(fix_vars) > 0) pred_matrix[fix_vars, ] <- 0
      }
      pred_matrix
    },
    
    .pool_imputations = function(mice_obj, data_original, vars_to_impute) {
      get_mode <- function(x) {
        ux <- unique(x[!is.na(x)])
        if (length(ux) == 0) return(NA)
        ux[which.max(tabulate(match(x, ux)))]
      }
      
      completed_list <- lapply(seq_len(mice_obj$m), function(i) mice::complete(mice_obj, action = i))
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
      if (table_result_ref$rowCount > 0) table_result_ref$clear()
      
      row_num <- 1
      for (var_name in vars_to_impute) {
        if (!var_name %in% names(original_data) || !var_name %in% names(pooled_data)) next
        original_col <- original_data[[var_name]]
        imputed_col  <- pooled_data[[var_name]]
        na_idx       <- which(is.na(original_col))
        
        for (i in na_idx) {
          if (row_num <= 1000) {
            table_result_ref$addRow(rowKey = row_num, values = list(
              variable  = var_name,
              original  = "NA",
              imputed   = as.character(round(suppressWarnings(as.numeric(imputed_col[i])), 3) %||% as.character(imputed_col[i])),
              rowNumber = i
            ))
            row_num <- row_num + 1
          } else if (row_num == 1001) {
            table_result_ref$addRow(rowKey = row_num, values = list(
              variable  = "...",
              original  = "...",
              imputed   = "Table truncated (max 1000 rows)",
              rowNumber = NA_integer_
            ))
            row_num <- row_num + 1
          }
        }
      }
    },
    
    .set_correlation_state = function(data, vars_to_impute, plot_ref) {
      sub_data   <- data[, intersect(vars_to_impute, names(data)), drop = FALSE]
      miss_ind   <- as.data.frame(lapply(sub_data, function(x) as.integer(is.na(x))))
      valid_cols <- sapply(miss_ind, function(x) var(x, na.rm = TRUE) > 0)
      
      if (sum(valid_cols) > 1) {
        cor_mat <- stats::cor(miss_ind[, valid_cols, drop = FALSE], use = "pairwise.complete.obs")
        if (any(is.finite(cor_mat))) {
          plot_ref$setState(cor_mat)
          plot_ref$setVisible(TRUE)
          return()
        }
      }
      plot_ref$setVisible(FALSE)
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
        descriptions = "Pooled imputed variable output",
        measureTypes = mtypes
      )
      
      for (k in keys) {
        vec      <- pooled_data[[k]]
        orig_col <- original_data[[k]]
        if (is.factor(orig_col) && is.factor(vec)) {
          vec <- factor(as.character(vec), levels = levels(orig_col), ordered = is.ordered(orig_col))
        } else if (is.numeric(vec)) {
          vec <- round(vec, 3)
        }
        output_ref$setValues(key = k, values = vec)
      }
      
      if (nrow(original_data) > 0) {
        output_ref$setRowNums(seq_len(nrow(original_data)))
      }
    },
    
    .renderPatternPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_vim_pattern(image)
    },
    .renderCorrelationPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_ggcorr_heatmap(image)
    },
    .renderTracePlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_mice_trace(image)
    },
    .renderDensityPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_mice_density(image)
    },
    .renderStripPlotMiceCS = function(image, ggtheme, theme, ...) {
      private$.draw_mice_strip(image)
    },
    
    .renderPatternPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_vim_pattern(image)
    },
    .renderCorrelationPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_ggcorr_heatmap(image)
    },
    .renderTracePlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_mice_trace(image)
    },
    .renderDensityPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_mice_density(image)
    },
    .renderStripPlotMiceLong = function(image, ggtheme, theme, ...) {
      private$.draw_mice_strip(image)
    },
    
    .renderPatternPlotMR = function(image, ggtheme, theme, ...) {
      private$.draw_vim_pattern(image)
    },
    .renderCorrelationPlotMR = function(image, ggtheme, theme, ...) {
      private$.draw_ggcorr_heatmap(image)
    },
    
    .draw_vim_pattern = function(image) {
      if (is.null(image$state)) return(FALSE)
      state <- image$state
      plot_data <- state$data[, intersect(state$vars, names(state$data)), drop = FALSE]
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
    
    .draw_ggcorr_heatmap = function(image) {
      if (is.null(image$state) || !is.matrix(image$state)) return(FALSE)
      cor_matrix <- image$state
      if (nrow(cor_matrix) < 2 || ncol(cor_matrix) < 2) return(FALSE)
      
      p <- GGally::ggcorr(
        data = NULL, cor_matrix = cor_matrix,
        label = TRUE, label_round = 2,
        hjust = 0.9, layout.exp = 3, label_size = 3.5
      ) +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y      = ggplot2::element_text(size = 10),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    },
    
    .draw_mice_trace = function(image) {
      if (is.null(image$state)) return(FALSE)
      mice_obj <- image$state
      if (!inherits(mice_obj, "mids")) return(FALSE)
      
      imputed_vars <- names(which(mice_obj$method != ""))
      if (length(imputed_vars) == 0) return(FALSE)
      
      n_vars <- length(imputed_vars)
      n_cols <- 2
      n_rows <- ceiling(n_vars / n_cols)
      
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par))
      graphics::par(mfrow = c(n_rows * 2, n_cols), mar = c(3, 3, 2, 1), bg = "transparent")
      
      chainMean <- mice_obj$chainMean
      chainVar  <- mice_obj$chainVar
      
      for (v in imputed_vars) {
        if (!v %in% dimnames(chainMean)[[1]]) next
        mean_mat <- chainMean[v, , ]
        var_mat  <- chainVar[v, , ]
        
        matplot(
          t(mean_mat), type = "l", lty = 1,
          col = grDevices::rainbow(ncol(mean_mat)),
          main = paste0(v, " — Iteration Mean"),
          xlab = "Iteration", ylab = "Mean", cex.main = 0.9
        )
        
        sd_mat <- sqrt(pmax(var_mat, 0))
        if (any(is.finite(sd_mat))) {
          matplot(
            t(sd_mat), type = "l", lty = 1,
            col = grDevices::rainbow(ncol(sd_mat)),
            main = paste0(v, " — Iteration SD"),
            xlab = "Iteration", ylab = "SD", cex.main = 0.9
          )
        } else {
          graphics::plot.new()
          graphics::title(main = paste0(v, " — SD N/A"), cex.main = 0.9)
        }
      }
      TRUE
    },
    
    .draw_mice_density = function(image) {
      if (is.null(image$state)) return(FALSE)
      mice_obj <- image$state
      
      vars_with_miss <- names(which(sapply(mice_obj$data, function(x) is.numeric(x) && anyNA(x))))
      if (length(vars_with_miss) == 0) return(FALSE)
      
      completed_list <- lapply(seq_len(mice_obj$m), function(i) mice::complete(mice_obj, action = i))
      plot_list <- list()
      for (v in vars_with_miss) {
        obs <- mice_obj$data[[v]]
        obs <- obs[!is.na(obs)]
        if (length(obs) < 2) next
        
        imp <- unlist(lapply(completed_list, function(d) {
          d[[v]][is.na(mice_obj$data[[v]])]
        }))
        if (length(imp) < 2) next
        
        plot_list[[v]] <- rbind(
          data.frame(value = obs, type = "Observed", variable = v),
          data.frame(value = imp, type = "Imputed",  variable = v)
        )
      }
      
      if (length(plot_list) == 0) return(FALSE)
      plot_data <- do.call(rbind, plot_list)
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, colour = type, fill = type)) +
        ggplot2::geom_density(alpha = 0.25, linewidth = 0.7) +
        ggplot2::facet_wrap(~ variable, scales = "free") +
        ggplot2::scale_colour_manual(values = c("Observed" = "#3182bd", "Imputed" = "#de2d26")) +
        ggplot2::scale_fill_manual(values = c("Observed" = "#3182bd", "Imputed" = "#de2d26")) +
        ggplot2::labs(x = "Value", y = "Density", colour = NULL, fill = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position  = "top",
          strip.background = ggplot2::element_rect(fill = "#f0f0f0"),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    },
    
    .draw_mice_strip = function(image) {
      if (is.null(image$state)) return(FALSE)
      mice_obj <- image$state$mice_obj
      vars     <- image$state$vars
      
      num_vars <- Filter(function(v) v %in% names(mice_obj$data) && is.numeric(mice_obj$data[[v]]) && anyNA(mice_obj$data[[v]]), vars)
      if (length(num_vars) == 0) return(FALSE)
      
      completed_list <- lapply(seq_len(mice_obj$m), function(i) mice::complete(mice_obj, action = i))
      plot_rows <- list()
      for (v in num_vars) {
        na_orig <- is.na(mice_obj$data[[v]])
        plot_rows[[paste(v, 0)]] <- data.frame(variable = v, value = mice_obj$data[[v]][!na_orig], imp = 0, type = "Observed")
        
        for (i in seq_len(mice_obj$m)) {
          d <- completed_list[[i]]
          plot_rows[[paste(v, i)]] <- data.frame(variable = v, value = d[[v]][na_orig], imp = i, type = "Imputed")
        }
      }
      
      plot_data      <- do.call(rbind, plot_rows)
      plot_data$imp  <- factor(plot_data$imp)
      plot_data$type <- factor(plot_data$type, levels = c("Observed", "Imputed"))
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = imp, y = value, colour = type, shape = type)) +
        ggplot2::geom_jitter(width = 0.18, alpha = 0.65, size = 1.6) +
        ggplot2::facet_wrap(~ variable, scales = "free_y") +
        ggplot2::scale_colour_manual(values = c("Observed" = "#3182bd", "Imputed" = "#de2d26")) +
        ggplot2::scale_shape_manual(values = c("Observed" = 21, "Imputed" = 20)) +
        ggplot2::labs(x = "Imputation (0 = Observed)", y = "Value", colour = NULL, shape = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position  = "top",
          strip.background = ggplot2::element_rect(fill = "#f0f0f0"),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    }
  )
)