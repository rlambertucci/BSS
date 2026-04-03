miceImputationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "miceImputationClass",
  inherit = miceImputationBase,
  private = list(
    
    .mice_result = NULL,
    
    # ════════════════════════════════════════════════════════════
    # .init — HTML informativo
    # ════════════════════════════════════════════════════════════
    .init = function() {
      info_html <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.6;">',
        '<ul>',
        '<li><b>MICE</b> (Multiple Imputation by Chained Equations) &rarr; Imputes missing values ',
        'by modeling each incomplete variable conditionally on the others. Each variable is imputed ',
        'using its own model, iterating until convergence. ',
        '<i>van Buuren & Groothuis-Oudshoorn (2011).</i></li>',
        '<li><b>Methods available:</b> ',
        '<b>pmm</b> (Predictive Mean Matching, default — works for any distribution), ',
        '<b>norm</b> (Bayesian linear regression), ',
        '<b>logreg</b> (logistic regression for binary variables), ',
        '<b>polyreg</b> (polytomous regression for nominal variables), ',
        '<b>2l.pmm</b> and <b>2l.continuous</b> (multilevel methods for longitudinal/clustered data).</li>',
        '<li><b>Pooled Output</b> &rarr; Runs <b>m</b> independent imputations. ',
        'The final saved value is the <b>Mean</b> across imputations for continuous variables ',
        'and the <b>Mode</b> for categorical variables.</li>',
        '<li><b>Longitudinal data</b> &rarr; Use <b>2l.pmm</b> or <b>2l.continuous</b> and fill in the ',
        'Longitudinal Settings section (Subject ID, Time, Group/Cluster). ',
        'Wide format is automatically converted to long for imputation and back to wide for output.</li>',
        '</ul>',
        '<p style="font-style: italic; padding-top: 6px;">',
        'Note: If no Predictor Variables are selected, all variables in the dataset will be used. ',
        'For reproducible results, set a numeric seed value.',
        '</p>',
        '</div>'
      )
      self$results$imputationInfo$setContent(info_html)
    },
    
    # ════════════════════════════════════════════════════════════
    # .run — lógica principal
    # ════════════════════════════════════════════════════════════
    .run = function() {
      
      # ── Opções ──────────────────────────────────────────────
      data              <- self$data
      vars_to_impute    <- self$options$vars
      predictor_vars    <- self$options$predictor_vars
      num_imputations   <- self$options$num_imputations
      maxiter           <- self$options$maxiter
      seed_str          <- self$options$seed
      run_imputation    <- self$options$run_imputation
      data_format       <- self$options$data_format
      subject_id_var    <- self$options$subject_id_var
      time_var          <- self$options$time_var
      group_var         <- self$options$group_var
      fixed_covariates  <- self$options$fixed_covariates
      wave_vars         <- self$options$wave_vars
      
      show_pattern_plot     <- self$options$show_pattern_plot
      show_trace_plot       <- self$options$show_trace_plot
      show_density_plot     <- self$options$show_density_plot
      show_strip_plot       <- self$options$show_strip_plot
      show_correlation_plot <- self$options$show_correlation_plot
      
      # ── Seleciona método ─────────────────────────────────────
      imp_method <- self$options$imputation_method
      
      if (is.null(data)) return()
      
      # ── Validação de variáveis ───────────────────────────────
      if (is.null(vars_to_impute) || length(vars_to_impute) == 0) {
        self$results$text$setContent("Please select variables to impute.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      # Filtra ID variables
      vars_to_impute <- Filter(function(v) {
        v %in% names(data) && !inherits(data[[v]], "jmvcore.IdVariable")
      }, vars_to_impute)
      
      if (length(vars_to_impute) == 0) {
        self$results$text$setContent("No suitable variables found (ID variables are excluded).")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      # ── Seed ────────────────────────────────────────────────
      base_seed <- suppressWarnings(as.numeric(seed_str))
      if (is.na(base_seed)) base_seed <- NULL
      
      # ── Conversão wide → long ────────────────────────────────
      wide_to_long_map <- NULL
      
      if (data_format == "wide" && !is.null(wave_vars) && length(wave_vars) >= 2) {
        
        if (is.null(subject_id_var) || nchar(subject_id_var) == 0) {
          self$results$text$setContent(
            "Wide format requires a Subject ID variable in Longitudinal Settings.")
          self$results$text$setVisible(TRUE)
          return()
        }
        
        wide_to_long_map <- list(
          wave_vars      = wave_vars,
          subject_id_var = subject_id_var,
          original_data  = data
        )
        
        data_wide         <- data
        data_wide$.row_id <- seq_len(nrow(data_wide))
        
        tryCatch({
          data_long <- stats::reshape(
            data_wide,
            varying   = wave_vars,
            v.names   = "wave_value",
            timevar   = ".wave",
            times     = wave_vars,
            direction = "long"
          )
          data_long <- data_long[order(data_long[[subject_id_var]], data_long$.wave), ]
          rownames(data_long) <- NULL
          data           <- data_long
          vars_to_impute <- "wave_value"
        }, error = function(e) {
          self$results$text$setContent(paste("Wide-to-long conversion error:", e$message))
          self$results$text$setVisible(TRUE)
        })
      }
      
      # ── Tabela de Missingness ────────────────────────────────
      miss_table <- self$results$missingnessTable
      
      vars_for_miss_table <- if (!is.null(wide_to_long_map)) {
        wide_to_long_map$wave_vars
      } else {
        vars_to_impute
      }
      
      for (var_name in vars_for_miss_table) {
        orig_data_ref <- if (!is.null(wide_to_long_map)) wide_to_long_map$original_data else data
        if (!var_name %in% names(orig_data_ref)) next
        col         <- orig_data_ref[[var_name]]
        n_total     <- length(col)
        n_missing   <- sum(is.na(col))
        n_valid     <- n_total - n_missing
        pct_missing <- round((n_missing / n_total) * 100, 1)
        method_label <- imp_method
        
        miss_table$addRow(
          rowKey = var_name,
          values = list(
            variable    = var_name,
            n_missing   = n_missing,
            pct_missing = pct_missing,
            n_valid     = n_valid,
            method_used = method_label
          )
        )
      }
      
      # ── Gráficos que não dependem do Run ─────────────────────
      if (show_pattern_plot) {
        self$results$patternPlot$setState(
          list(data = data, vars = vars_to_impute)
        )
      } else {
        self$results$patternPlot$setVisible(FALSE)
      }
      
      if (show_correlation_plot) {
        private$.set_correlation_state(data, vars_to_impute)
      } else {
        self$results$correlationPlot$setVisible(FALSE)
      }
      
      # ── Para aqui se Run não marcado ─────────────────────────
      if (!isTRUE(run_imputation)) {
        self$results$text$setContent("Check 'Run Imputation' to execute the algorithm.")
        self$results$text$setVisible(TRUE)
        return()
      }
      
      # ── Construção do dataset para imputação ─────────────────
      all_vars <- unique(c(
        vars_to_impute,
        if (!is.null(predictor_vars))                                  predictor_vars,
        if (!is.null(fixed_covariates))                                fixed_covariates,
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0)    subject_id_var,
        if (!is.null(time_var)       && nchar(time_var) > 0)          time_var,
        if (!is.null(group_var)      && nchar(group_var) > 0)         group_var,
        if (!is.null(wide_to_long_map))                                ".wave"
      ))
      all_vars       <- intersect(all_vars, names(data))
      data_to_impute <- data[, all_vars, drop = FALSE]
      
      # Converte subject_id para inteiro (exigido por 2l.*)
      if (!is.null(subject_id_var) && nchar(subject_id_var) > 0 &&
          subject_id_var %in% names(data_to_impute)) {
        data_to_impute[[subject_id_var]] <- as.integer(data_to_impute[[subject_id_var]])
      }
      
      # ── Método por coluna ────────────────────────────────────
      method_vec <- private$.build_method_vector(
        data_to_impute, vars_to_impute, imp_method,
        subject_id_var, group_var
      )
      
      # ── Predictor matrix ─────────────────────────────────────
      effective_time_var <- if (!is.null(wide_to_long_map)) ".wave" else time_var
      
      pred_matrix <- private$.build_predictor_matrix(
        data_to_impute, vars_to_impute,
        subject_id_var, effective_time_var, group_var,
        fixed_covariates, imp_method
      )
      
      # ── Execução do MICE ─────────────────────────────────────
      mice_obj <- tryCatch({
        mice_seed <- if (!is.null(base_seed)) base_seed else NA
        
        if (imp_method %in% c("2l.pmm", "2l.continuous")) {
          requireNamespace("miceadds", quietly = TRUE)
          fn_name <- paste0("mice.impute.", imp_method)
          fn <- tryCatch(
            utils::getFromNamespace(fn_name, "miceadds"),
            error = function(e) NULL
          )
          if (is.null(fn)) {
            stop(paste0(
              "Function '", fn_name, "' not found in miceadds. ",
              "Please update miceadds: install.packages('miceadds')"
            ))
          }
          assign(fn_name, fn, envir = globalenv())
        }
        
        mice::mice(
          data            = data_to_impute,
          m               = num_imputations,
          method          = method_vec,
          predictorMatrix = pred_matrix,
          maxit           = maxiter,
          seed            = mice_seed,
          printFlag       = FALSE
        )
      }, error = function(e) {
        self$results$text$setContent(paste("MICE error:", e$message))
        self$results$text$setVisible(TRUE)
        return(NULL)
      })
      
      if (is.null(mice_obj)) return()
      
      private$.mice_result <- mice_obj
      
      # ── Gráficos que dependem do Run ─────────────────────────
      if (show_trace_plot) {
        self$results$tracePlot$setState(mice_obj)
      } else {
        self$results$tracePlot$setVisible(FALSE)
      }
      
      if (show_density_plot) {
        self$results$densityPlot$setState(mice_obj)
      } else {
        self$results$densityPlot$setVisible(FALSE)
      }
      
      if (show_strip_plot) {
        self$results$stripPlot$setState(
          list(mice_obj = mice_obj, vars = vars_to_impute)
        )
      } else {
        self$results$stripPlot$setVisible(FALSE)
      }
      
      # ── Pooling: Média / Moda ────────────────────────────────
      pooled_data <- private$.pool_imputations(
        mice_obj, data_to_impute, vars_to_impute
      )
      
      # ── Reconversão long → wide ──────────────────────────────
      if (!is.null(wide_to_long_map)) {
        pooled_data <- private$.long_to_wide(
          pooled_data, wide_to_long_map, subject_id_var
        )
        vars_to_impute <- wide_to_long_map$wave_vars
        data           <- wide_to_long_map$original_data
      }
      
      # ── Arredondamento ───────────────────────────────────────
      for (v in names(pooled_data)) {
        if (is.numeric(pooled_data[[v]])) {
          pooled_data[[v]] <- round(pooled_data[[v]], digits = 3)
        }
      }
      
      # ── Tabela de valores imputados ──────────────────────────
      table         <- self$results$imputedData
      row_num       <- 1
      total_na_orig <- 0
      
      for (var_name in vars_to_impute) {
        if (!var_name %in% names(data) || !var_name %in% names(pooled_data)) next
        original_col  <- data[[var_name]]
        imputed_col   <- pooled_data[[var_name]]
        na_idx        <- which(is.na(original_col))
        total_na_orig <- total_na_orig + length(na_idx)
        
        for (i in na_idx) {
          if (row_num <= 1000) {
            table$addRow(rowKey = row_num, values = list(
              variable  = var_name,
              original  = "NA",
              imputed   = as.character(imputed_col[i]),
              rowNumber = i
            ))
            row_num <- row_num + 1
          } else if (row_num == 1001) {
            table$addRow(rowKey = row_num, values = list(
              variable  = "...",
              original  = "...",
              imputed   = "Table truncated (max 1000 rows)",
              rowNumber = NA_integer_
            ))
            row_num <- row_num + 1
          }
        }
      }
      
      # ── Texto de resumo ──────────────────────────────────────
      long_note <- if (imp_method %in% c("2l.pmm", "2l.continuous"))
        paste0(" | Multilevel method: ", imp_method, ".") else ""
      wide_note <- if (data_format == "wide")
        " | Wide format: converted to long and back." else ""
      self$results$text$setContent(paste0(
        "MICE completed (m = ", num_imputations,
        ", method = ", imp_method,
        ", maxit = ", maxiter, "). ",
        total_na_orig, " NA(s) handled.",
        long_note, wide_note
      ))
      self$results$text$setVisible(TRUE)
      
      # ── Output para a planilha ───────────────────────────────
      private$.write_output(pooled_data, vars_to_impute, data)
    },
    
    # ════════════════════════════════════════════════════════════
    # HELPERS
    # ════════════════════════════════════════════════════════════
    
    .resolve_method = function(var_name, data, imp_method) {
      if (imp_method %in% c("2l.pmm", "2l.continuous")) return(imp_method)
      col <- data[[var_name]]
      if (is.numeric(col))  return(imp_method)
      if (is.factor(col)) {
        if (nlevels(col) == 2) return("logreg")
        return("polyreg")
      }
      return(imp_method)
    },
    
    .build_method_vector = function(data_to_impute, vars_to_impute,
                                    imp_method, subject_id_var, group_var) {
      method_vec <- rep("", ncol(data_to_impute))
      names(method_vec) <- names(data_to_impute)
      
      for (v in vars_to_impute) {
        if (!v %in% names(method_vec)) next
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0 && v == subject_id_var) next
        if (!is.null(group_var)      && nchar(group_var) > 0      && v == group_var)      next
        
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
    
    .build_predictor_matrix = function(data_to_impute, vars_to_impute,
                                       subject_id_var, time_var, group_var,
                                       fixed_covariates, imp_method) {
      pred_matrix <- mice::make.predictorMatrix(data_to_impute)
      
      # Estruturais não são alvo
      struct_vars <- unique(c(
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0) subject_id_var,
        if (!is.null(time_var)       && nchar(time_var) > 0)       time_var,
        if (!is.null(group_var)      && nchar(group_var) > 0)      group_var,
        ".wave", ".row_id", "id"
      ))
      struct_vars <- intersect(struct_vars, rownames(pred_matrix))
      if (length(struct_vars) > 0) {
        pred_matrix[struct_vars, ] <- 0
      }
      
      # Métodos multinível: subject_id = cluster (-2)
      if (imp_method %in% c("2l.pmm", "2l.continuous")) {
        if (!is.null(subject_id_var) && nchar(subject_id_var) > 0 &&
            subject_id_var %in% colnames(pred_matrix)) {
          pred_matrix[vars_to_impute, subject_id_var] <- -2
        }
        if (!is.null(time_var) && nchar(time_var) > 0 &&
            time_var %in% colnames(pred_matrix)) {
          pred_matrix[vars_to_impute, time_var] <- 1
        }
        if (".wave" %in% colnames(pred_matrix)) {
          pred_matrix[vars_to_impute, ".wave"] <- 1
        }
      }
      
      # Covariáveis fixas: nunca alvo
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
      
      completed_list <- lapply(
        seq_len(mice_obj$m),
        function(i) mice::complete(mice_obj, action = i)
      )
      
      pooled <- data_original
      
      for (v in vars_to_impute) {
        if (!v %in% names(data_original)) next
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
            pooled[[v]] <- factor(
              pooled[[v]],
              levels  = levels(col_orig),
              ordered = is.ordered(col_orig)
            )
          }
        }
      }
      pooled
    },
    
    .long_to_wide = function(long_data, wide_map, subject_id_var) {
      tryCatch({
        wide_result <- stats::reshape(
          long_data,
          idvar     = subject_id_var,
          timevar   = ".wave",
          v.names   = "wave_value",
          direction = "wide"
        )
        wave_vars <- wide_map$wave_vars
        for (wv in wave_vars) {
          col_new <- paste0("wave_value.", wv)
          if (col_new %in% names(wide_result)) {
            names(wide_result)[names(wide_result) == col_new] <- wv
          }
        }
        wide_result
      }, error = function(e) {
        self$results$text$setContent(paste("Long-to-wide conversion error:", e$message))
        self$results$text$setVisible(TRUE)
        return(long_data)
      })
    },
    
    .set_correlation_state = function(data, vars_to_impute) {
      sub_data   <- data[, intersect(vars_to_impute, names(data)), drop = FALSE]
      miss_ind   <- as.data.frame(lapply(sub_data, function(x) as.integer(is.na(x))))
      valid_cols <- sapply(miss_ind, function(x) var(x, na.rm = TRUE) > 0)
      if (sum(valid_cols) > 1) {
        cor_mat <- stats::cor(miss_ind[, valid_cols, drop = FALSE],
                              use = "pairwise.complete.obs")
        if (any(is.finite(cor_mat))) {
          self$results$correlationPlot$setState(cor_mat)
          self$results$correlationPlot$setVisible(TRUE)
          return()
        }
      }
      self$results$correlationPlot$setVisible(FALSE)
    },
    
    .write_output = function(pooled_data, vars_to_impute, original_data) {
      if (!self$results$imputedColsOutput$isNotFilled()) return()
      
      keys   <- character()
      titles <- character()
      mtypes <- character()
      
      for (v in vars_to_impute) {
        if (!v %in% names(pooled_data)) next
        keys   <- c(keys,   v)
        titles <- c(titles, paste0(v, "_imputed"))
        
        orig_col <- original_data[[v]]
        mtype <- if (is.numeric(orig_col))                             "continuous"
        else if (is.factor(orig_col) && is.ordered(orig_col)) "ordinal"
        else if (is.factor(orig_col))                         "nominal"
        else                                                   "continuous"
        mtypes <- c(mtypes, mtype)
      }
      
      if (length(keys) == 0) return()
      
      self$results$imputedColsOutput$set(
        keys         = keys,
        titles       = titles,
        descriptions = "MICE pooled imputed variable",
        measureTypes = mtypes
      )
      
      for (k in keys) {
        vec      <- pooled_data[[k]]
        orig_col <- original_data[[k]]
        if (is.factor(orig_col) && is.factor(vec)) {
          vec <- factor(as.character(vec),
                        levels  = levels(orig_col),
                        ordered = is.ordered(orig_col))
        }
        self$results$imputedColsOutput$setValues(key = k, values = vec)
      }
      
      if (nrow(original_data) > 0) {
        self$results$imputedColsOutput$setRowNums(seq_len(nrow(original_data)))
      }
    },
    
    # ════════════════════════════════════════════════════════════
    # RENDER FUNCTIONS — Gráficos
    # ════════════════════════════════════════════════════════════
    
    .patternPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return(FALSE)
      state <- image$state
      data  <- state$data
      vars  <- state$vars
      
      plot_data <- data[, intersect(vars, names(data)), drop = FALSE]
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
        col      = c("skyblue", "tomato"),
        cex.axis = 0.8,
        ylab     = c("Proportion Missing", "Combination Pattern")
      )
      TRUE
    },
    
    .tracePlot = function(image, ggtheme, theme, ...) {
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
      graphics::par(
        mfrow = c(n_rows * 2, n_cols),
        mar   = c(3, 3, 2, 1),
        bg    = "transparent"
      )
      
      chainMean <- mice_obj$chainMean
      chainVar  <- mice_obj$chainVar
      
      for (v in imputed_vars) {
        if (!v %in% dimnames(chainMean)[[1]]) next
        
        mean_mat <- chainMean[v, , ]
        var_mat  <- chainVar[v, , ]
        
        matplot(
          t(mean_mat),
          type = "l", lty = 1,
          col  = grDevices::rainbow(ncol(mean_mat)),
          main = paste0(v, " — Mean"),
          xlab = "Iteration", ylab = "Mean",
          cex.main = 0.9
        )
        
        sd_mat    <- sqrt(pmax(var_mat, 0))
        sd_finite <- is.finite(sd_mat)
        
        if (any(sd_finite)) {
          ylim_sd <- range(sd_mat[sd_finite], na.rm = TRUE)
          if (diff(ylim_sd) == 0) ylim_sd <- ylim_sd + c(-0.1, 0.1)
          matplot(
            t(sd_mat),
            type = "l", lty = 1,
            col  = grDevices::rainbow(ncol(sd_mat)),
            main = paste0(v, " — SD"),
            xlab = "Iteration", ylab = "SD",
            ylim = ylim_sd,
            cex.main = 0.9
          )
        } else {
          graphics::plot.new()
          graphics::title(main = paste0(v, " — SD (not available)"), cex.main = 0.9)
        }
      }
      TRUE
    },
    
    .densityPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return(FALSE)
      mice_obj <- image$state
      if (!inherits(mice_obj, "mids")) return(FALSE)
      
      vars_with_missing <- names(which(
        sapply(mice_obj$data, function(x) is.numeric(x) && anyNA(x))
      ))
      if (length(vars_with_missing) == 0) return(FALSE)
      
      plot_list <- list()
      for (v in vars_with_missing) {
        obs_vals <- mice_obj$data[[v]]
        obs_vals <- obs_vals[!is.na(obs_vals)]
        if (length(obs_vals) < 2) next
        
        imp_vals <- unlist(lapply(seq_len(mice_obj$m), function(i) {
          d <- mice::complete(mice_obj, action = i)
          orig_na <- is.na(mice_obj$data[[v]])
          d[[v]][orig_na]
        }))
        if (length(imp_vals) < 2) next
        
        df_v <- rbind(
          data.frame(value = obs_vals, type = "Observed", variable = v),
          data.frame(value = imp_vals, type = "Imputed",  variable = v)
        )
        plot_list[[v]] <- df_v
      }
      
      if (length(plot_list) == 0) return(FALSE)
      plot_data <- do.call(rbind, plot_list)
      
      p <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = value, colour = type, fill = type)) +
        ggplot2::geom_density(alpha = 0.3, linewidth = 0.8) +
        ggplot2::facet_wrap(~ variable, scales = "free") +
        ggplot2::scale_colour_manual(values = c("Observed" = "steelblue",
                                                "Imputed"  = "tomato")) +
        ggplot2::scale_fill_manual(values = c("Observed" = "steelblue",
                                              "Imputed"  = "tomato")) +
        ggplot2::labs(title  = "Observed vs Imputed Distribution",
                      x = "Value", y = "Density",
                      colour = NULL, fill = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position  = "top",
          strip.background = ggplot2::element_rect(fill = "grey92"),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    },
    
    .stripPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return(FALSE)
      state    <- image$state
      mice_obj <- state$mice_obj
      vars     <- state$vars
      if (!inherits(mice_obj, "mids")) return(FALSE)
      
      num_vars <- Filter(function(v) {
        v %in% names(mice_obj$data) &&
          is.numeric(mice_obj$data[[v]]) &&
          anyNA(mice_obj$data[[v]])
      }, vars)
      if (length(num_vars) == 0) return(FALSE)
      
      plot_rows <- list()
      for (v in num_vars) {
        orig_na <- is.na(mice_obj$data[[v]])
        
        obs_df <- data.frame(
          variable = v,
          value    = mice_obj$data[[v]][!orig_na],
          imp      = 0,
          type     = "Observed"
        )
        
        for (i in seq_len(mice_obj$m)) {
          d <- mice::complete(mice_obj, action = i)
          imp_df <- data.frame(
            variable = v,
            value    = d[[v]][orig_na],
            imp      = i,
            type     = "Imputed"
          )
          plot_rows[[paste(v, i)]] <- imp_df
        }
        plot_rows[[paste(v, 0)]] <- obs_df
      }
      
      plot_data      <- do.call(rbind, plot_rows)
      plot_data$imp  <- factor(plot_data$imp)
      plot_data$type <- factor(plot_data$type, levels = c("Observed", "Imputed"))
      
      p <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = imp, y = value, colour = type, shape = type)) +
        ggplot2::geom_jitter(width = 0.2, alpha = 0.7, size = 1.8) +
        ggplot2::facet_wrap(~ variable, scales = "free_y") +
        ggplot2::scale_colour_manual(values = c("Observed" = "steelblue",
                                                "Imputed"  = "tomato")) +
        ggplot2::scale_shape_manual(values = c("Observed" = 21, "Imputed" = 20)) +
        ggplot2::labs(title  = "Strip Plot — Observed vs Imputed",
                      x = "Imputation", y = "Value",
                      colour = NULL, shape = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position  = "top",
          strip.background = ggplot2::element_rect(fill = "grey92"),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    },
    
    .correlationPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state) || !is.matrix(image$state)) return(FALSE)
      cor_mat <- image$state
      if (nrow(cor_mat) < 2 || !any(is.finite(cor_mat))) return(FALSE)
      
      p <- GGally::ggcorr(
        data        = NULL,
        cor_matrix  = cor_mat,
        label       = TRUE,
        label_round = 2,
        hjust       = 1,
        layout.exp  = 5
      ) +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
          axis.text.y      = ggplot2::element_text(size = 12),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    }
  )
)