
DataPresentationClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "DataPresentationClass",
  inherit = DataPresentationBase,
  private = list(
    .group_names = NULL,

    .groupColumns = function(groups) {
      reserved <- c("variable", "n", "mean", "sd", "se", "ci_lower", "ci_upper", "median", "min", "max")
      stats::setNames(tail(make.unique(c(reserved, groups)), length(groups)), groups)
    },

    .groupLabels = function(groups) {
      if (is.null(private$.group_names)) return(groups)
      unname(private$.group_names[groups])
    },

    .groupColumn = function(group) {
      columns <- private$.groupColumns(unname(private$.group_names))
      unname(columns[private$.groupLabels(group)])
    },
    
    .quote_names = function(names) {
      vapply(names, function(name) paste(deparse(as.name(name), backtick = TRUE), collapse = ""), character(1), USE.NAMES = FALSE)
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
    
    .fitDistribution = function(values, family, ci_prop) {
      values <- values[!is.na(values)]
      count <- length(values)
      if (count < 2) stop("At least two non-missing observations are required.")
      if (any(!is.finite(values))) stop("All non-missing observations must be finite.")
      if (any(values <= 0)) stop("All non-missing observations must be strictly positive; no observations were removed.")
      if (all(values == values[1])) stop("The group is constant; dispersion cannot be estimated.")
      if (!family %in% c('gamma', 'inv_gaussian')) stop("Unsupported distribution.")
      average <- mean(values)
      scaled <- values / average
      if (any(!is.finite(scaled)) || any(scaled <= 0)) stop("The data range exceeds numerical precision.")
      if (family == 'gamma') {
        mean_log <- mean(log(scaled))
        shape_for <- function(gap) {
          if (!is.finite(gap) || gap <= 0) stop("Gamma shape cannot be estimated at numerical precision.")
          equation <- function(log_shape) {
            shape <- exp(log_shape)
            difference <- if (shape > 1e5) 1 / (2 * shape) + 1 / (12 * shape^2) else log_shape - digamma(shape)
            difference - gap
          }
          exp(stats::uniroot(equation, c(-30, 36), tol = 1e-10)$root)
        }
        shape <- shape_for(-mean_log)
        fitted_loglik <- sum(stats::dgamma(scaled, shape = shape, scale = 1 / shape, log = TRUE))
        likelihood_ratio <- function(log_ratio) {
          inverse_mean <- exp(-log_ratio)
          gap <- inverse_mean + log_ratio - 1 - mean_log
          profile_shape <- shape_for(gap)
          profile_loglik <- sum(stats::dgamma(scaled, shape = profile_shape, scale = exp(log_ratio) / profile_shape, log = TRUE))
          2 * (fitted_loglik - profile_loglik)
        }
        deviation <- average / sqrt(shape)
      } else {
        inverse_shape <- mean((scaled - 1)^2 / scaled)
        if (!is.finite(inverse_shape) || inverse_shape <= 0) stop("Inverse Gaussian shape cannot be estimated at numerical precision.")
        likelihood_ratio <- function(log_ratio) count * log1p((exp(-log_ratio) - 1)^2 / inverse_shape)
        deviation <- average * sqrt(inverse_shape)
        if (count * log1p(1 / inverse_shape) <= stats::qchisq(ci_prop, df = 1)) {
          stop("The profile-likelihood confidence interval has an unbounded upper limit; no finite interval is available.")
        }
      }
      cutoff <- stats::qchisq(ci_prop, df = 1)
      confidence_limit <- function(direction) {
        distance <- 0.25
        while (distance <= 512) {
          boundary <- direction * distance
          value <- likelihood_ratio(boundary) - cutoff
          if (is.finite(value) && value >= 0) {
            limits <- sort(c(0, boundary))
            return(average * exp(stats::uniroot(function(log_ratio) likelihood_ratio(log_ratio) - cutoff, limits, tol = 1e-9)$root))
          }
          distance <- distance * 2
        }
        stop("The profile-likelihood confidence limit could not be bracketed at numerical precision.")
      }
      lower <- confidence_limit(-1)
      upper <- confidence_limit(1)
      if (any(!is.finite(c(average, deviation, lower, upper))) || deviation <= 0 || lower <= 0 || lower >= average || upper <= average) {
        stop("The fitted summary is outside numerical precision.")
      }
      list(n = count, mean = average, sd = deviation, se = deviation / sqrt(count), ci_lower = lower, ci_upper = upper,
           median = stats::median(values), min = min(values), max = max(values))
    },

    .presentationMessages = function(messages = character()) {
      if (self$options$plotType == 'density' && self$options$theo_dist != 'none') {
        messages <- c(messages, "Saved-analysis migration: theoretical density curves have been retired. This plot now shows empirical density only. The previous curve selection has not been converted into a summary distribution.")
      }
      self$results$messages$setContent(paste(messages, collapse = "\n"))
      self$results$messages$setVisible(length(messages) > 0)
    },

    .plotGroups = function(data, groups, occasion = NULL) {
      if (!is.null(occasion)) {
        x_var <- occasion
        fill_var <- if (length(groups) > 0) interaction(data[groups], sep = " - ") else occasion
        leg_title <- if (length(groups) > 0) paste(private$.groupLabels(groups), collapse = " & ") else "Time"
      } else if (length(groups) == 1) {
        x_var <- data[[groups[1]]]
        fill_var <- x_var
        leg_title <- private$.groupLabels(groups[1])
      } else if (self$options$intStyle == 'legend') {
        x_var <- data[[groups[1]]]
        fill_var <- interaction(data[groups[-1]], sep = " - ")
        leg_title <- paste(private$.groupLabels(groups[-1]), collapse = " & ")
      } else {
        x_var <- interaction(data[groups], sep = " - ")
        fill_var <- x_var
        leg_title <- "Groups"
      }
      list(x_var = x_var, fill_var = fill_var, leg_title = rep(leg_title, length(x_var)))
    },

    .get_rm_dim_name = function(fit, rm_vars) {
      tryCatch({
        rg <- emmeans::ref_grid(fit)
        for (vname in names(rg@levels)) {
          if (all(rm_vars %in% rg@levels[[vname]])) return(vname)
        }
        'rep.meas'
      }, error = function(e) 'rep.meas')
    },
    
    .init = function() {
      grouping_vars <- if (self$options$analysisType == 'cross_sectional') self$options$group else self$options$group_long
      private$.syncTableColumns(self$results$descriptiveTable, grouping_vars)
      private$.formatTableByNorm(self$results$descriptiveTable)
      num_vars <- if (self$options$analysisType == 'cross_sectional') self$options$deps else self$options$rm_vars
      private$.syncCorrTableColumns(self$results$corrMatrixTable, num_vars)
      if (self$results$corrMatrixTable$rowCount > 0) self$results$corrMatrixTable$deleteRows()
      for (index in seq_along(num_vars)) self$results$corrMatrixTable$addRow(rowKey = index, values = list(num = as.character(index), var = num_vars[index]))
      plots <- self$results$plots
      if (self$options$analysisType == 'cross_sectional') {
        keys <- self$options$deps
      } else {
        keys <- c("Repeated_Measures")
      }
      
      if (length(keys) > 0) {
        for (k in keys) {
          plots$addItem(key = k)
          if (self$options$analysisType == 'longitudinal') plots$get(key = k)$setTitle("Repeated Measures")
        }
      }
    },
    
    .syncTableColumns = function(table, grouping_vars) {
      get_col_names <- function(tbl) {
        tryCatch(sapply(tbl$columns, function(col) col$name), error = function(e) character(0))
      }
      base_cols <- c("variable", "n", "mean", "sd", "se", "ci_lower", "ci_upper", "median", "min", "max")
      group_columns <- private$.groupColumns(grouping_vars)
      current_cols <- get_col_names(table)
      cols_to_remove <- setdiff(current_cols, c(base_cols, unname(group_columns)))
      for (col_name in cols_to_remove) {
        table$getColumn(col_name)$setVisible(FALSE)
      }
      current_cols_after_remove <- get_col_names(table)
      for (i in seq_along(grouping_vars)) {
        g_var <- unname(group_columns[i])
        if (g_var %in% current_cols_after_remove) {
          table$getColumn(g_var)$setTitle(grouping_vars[[i]])
          table$getColumn(g_var)$setVisible(TRUE)
        } else {
          table$addColumn(name = g_var, title = grouping_vars[[i]], type = 'text', combineBelow = TRUE, index = i + 1)
        }
      }
    },
    
    .run = function() {
      if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
      if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required.")
      `%>%` <- dplyr::`%>%`
      
      analysisType <- self$options$analysisType
      ciWidth      <- self$options$ciWidth / 100
      showTable    <- self$options$showTable
      data         <- self$data
      
      if (is.null(data) || nrow(data) == 0) return()
      self$results$descriptiveTable$setNote('distribution', NULL)
      self$results$descriptiveTable$setNote('distribution_failures', NULL)
      grouping_vars <- if (analysisType == 'cross_sectional') self$options$group else self$options$group_long
      missing_group <- if (length(grouping_vars) > 0) !complete.cases(data[, grouping_vars, drop = FALSE]) else rep(FALSE, nrow(data))
      self$results$descriptiveTable$setNote('missing_groups', if (any(missing_group)) paste(sum(missing_group), 'rows with missing grouping values excluded from both table and plots.') else NULL)
      data <- data[!missing_group, , drop = FALSE]
      if (nrow(data) == 0) jmvcore::reject('No cases with complete grouping values.', code = 'missing_groups')
      
      has_cov <- FALSE
      cov_var <- NULL
      if (analysisType == 'cross_sectional') {
        has_cov <- !is.null(self$options$cov) && self$options$cov != ""
        cov_var <- if (has_cov) self$options$cov else NULL
      } else {
        has_cov <- !is.null(self$options$cov_long) && self$options$cov_long != ""
        cov_var <- if (has_cov) self$options$cov_long else NULL
      }
      if (has_cov) {
        cov_source <- cov_var
        cov_var <- tail(make.unique(c(names(data), ".bss_covariate")), 1)
        data[[cov_var]] <- data[[cov_source]]
      }

      private$.group_names <- NULL
      if (length(grouping_vars) > 0) {
        group_keys <- tail(make.unique(c(names(data), paste0(".bss_group_", seq_along(grouping_vars)))), length(grouping_vars))
        private$.group_names <- stats::setNames(grouping_vars, group_keys)
        names(data)[match(grouping_vars, names(data))] <- group_keys
        grouping_vars <- group_keys
      }
      
      if (self$options$plotType %in% c('bar', 'meandp') && self$options$summary_distribution != 'normal') {
        private$.runDistributionPresentation(data, grouping_vars, has_cov)
        return()
      }

      plot_df_list <- list() 
      plot_stats_list <- list()
      
      if (analysisType == 'cross_sectional') {
        deps        <- self$options$deps
        groups_vars <- grouping_vars
        
        if (!isTRUE(length(deps) > 0) || !isTRUE(length(groups_vars) > 0)) {
          self$results$messages$setContent("Cross-Sectional: Assign at least one DV and one Grouping Variable.")
          self$results$messages$setVisible(TRUE)
          return()
        }
        self$results$messages$setVisible(FALSE)
        
        for (dep in deps) data[[dep]] <- private$.to_numeric_vec(data[[dep]])
        if (has_cov) data[[cov_var]] <- private$.to_numeric_vec(data[[cov_var]])
        for (v in groups_vars) data[[v]] <- as.factor(data[[v]])
        
        for (dep in deps) {
          cols_needed <- c(dep, groups_vars)
          if (has_cov) cols_needed <- c(cols_needed, cov_var)
          df_temp <- data[, cols_needed, drop = FALSE]
          df_clean <- df_temp[complete.cases(df_temp), , drop = FALSE]
          aesthetics <- private$.plotGroups(df_clean, groups_vars)
          plot_df_list[[dep]] <- data.frame(y = df_clean[[dep]], x_var = aesthetics$x_var,
                                          fill_var = aesthetics$fill_var, leg_title = aesthetics$leg_title)
          
          if (has_cov && nrow(df_clean) > 0) {
            formula_str <- paste(private$.quote_names(dep), "~", paste(private$.quote_names(groups_vars), collapse = " * "), "+", private$.quote_names(cov_var))
            fit <- tryCatch({
              stats::lm(as.formula(formula_str), data = df_clean)
            }, error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
            
            if (!is.null(fit)) {
              emm <- tryCatch({
                emmeans::emmeans(fit, specs = groups_vars, level = ciWidth)
              }, error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
              
              if (!is.null(emm)) {
                emm_df <- as.data.frame(emm)
                
                raw_summary <- df_clean %>%
                  dplyr::group_by(dplyr::across(dplyr::all_of(groups_vars))) %>%
                  dplyr::summarise(
                    sd = sd(.data[[dep]], na.rm = TRUE),
                    .groups = 'drop'
                  )
                
                merged_df <- merge(emm_df, raw_summary, by = groups_vars, all.x = TRUE)
                
                aesthetics <- private$.plotGroups(merged_df, groups_vars)
                
                lower_col <- grep("lower|LCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
                upper_col <- grep("upper|UCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
                ci_half <- (merged_df[[upper_col]] - merged_df[[lower_col]]) / 2
                
                plot_stats_list[[dep]] <- data.frame(
                  x_var = factor(aesthetics$x_var),
                  fill_var = factor(aesthetics$fill_var),
                  mean = merged_df$emmean,
                  sd = merged_df$sd,
                  se = merged_df$SE,
                  ci_half = ci_half
                )
              }
            }
          }
        }
      }
      
      else if (analysisType == 'longitudinal') {
        rm_vars    <- self$options$rm_vars
        group_long <- grouping_vars
        
        if (!isTRUE(length(rm_vars) > 1)) {
          self$results$messages$setContent("Longitudinal: Assign at least TWO Repeated Measures Variables.")
          self$results$messages$setVisible(TRUE)
          return()
        }
        self$results$messages$setVisible(FALSE)
        
        for (v in rm_vars) data[[v]] <- private$.to_numeric_vec(data[[v]])
        if (has_cov) data[[cov_var]] <- private$.to_numeric_vec(data[[cov_var]])
        if (length(group_long) > 0) {
          for (v in group_long) data[[v]] <- as.factor(data[[v]])
        }
        
        cols_needed <- rm_vars
        if (length(group_long) > 0) {
          cols_needed <- c(cols_needed, group_long)
        }
        if (has_cov) {
          cols_needed <- c(cols_needed, cov_var)
        }
        data_clean <- data[complete.cases(data[, cols_needed, drop = FALSE]), , drop = FALSE]
        
        stacked <- stack(data_clean[rm_vars])
        colnames(stacked) <- c("y", "x_var")
        stacked$x_var <- factor(stacked$x_var, levels = rm_vars)
        
        info_df <- data_clean[rep(seq_len(nrow(data_clean)), times = length(rm_vars)), group_long, drop = FALSE]
        aesthetics <- private$.plotGroups(info_df, group_long, stacked$x_var)
        df_temp <- data.frame(y = stacked$y, x_var = aesthetics$x_var,
                              fill_var = aesthetics$fill_var, leg_title = aesthetics$leg_title)
        plot_df_list[["Repeated_Measures"]] <- df_temp[complete.cases(df_temp), ]
        
        if (has_cov && nrow(data_clean) > 0) {
          if (length(group_long) > 0) {
            formula_str <- paste0("cbind(", paste(private$.quote_names(rm_vars), collapse = ", "), ") ~ ", paste(private$.quote_names(group_long), collapse = " * "), " + ", private$.quote_names(cov_var))
          } else {
            formula_str <- paste0("cbind(", paste(private$.quote_names(rm_vars), collapse = ", "), ") ~ ", private$.quote_names(cov_var))
          }
          
          fit <- tryCatch({
            stats::lm(as.formula(formula_str), data = data_clean)
          }, error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
          
          if (!is.null(fit)) {
            rm_dim_name <- private$.get_rm_dim_name(fit, rm_vars)
            if (length(group_long) > 0) {
              specs_formula <- as.formula(paste("~", private$.quote_names(rm_dim_name), "|", paste(private$.quote_names(group_long), collapse = " + ")))
            } else {
              specs_formula <- as.formula(paste("~", private$.quote_names(rm_dim_name)))
            }
            
            emm <- tryCatch({
              emmeans::emmeans(fit, specs = specs_formula, level = ciWidth)
            }, error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
            
            if (!is.null(emm)) {
              emm_df <- as.data.frame(emm)
              
              rm_col_name <- NULL
              for (col in names(emm_df)) {
                if (any(as.character(emm_df[[col]]) %in% rm_vars)) {
                  rm_col_name <- col
                  break
                }
              }
              if (!is.null(rm_col_name)) {
                emm_df$variable <- factor(emm_df[[rm_col_name]], levels = rm_vars)
              } else {
                emm_df$variable <- factor(emm_df[[rm_dim_name]], levels = rm_vars)
              }
              
              group_cols_summary <- if (length(group_long) > 0) c("variable", group_long) else "variable"
              
              raw_summary <- data_clean[, c(rm_vars, group_long), drop = FALSE] %>%
                tidyr::pivot_longer(cols = dplyr::all_of(rm_vars), names_to = "variable", values_to = "value") %>%
                dplyr::mutate(variable = factor(variable, levels = rm_vars)) %>%
                dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_summary))) %>%
                dplyr::summarise(
                  sd = sd(value, na.rm = TRUE),
                  .groups = 'drop'
                )
              
              emm_df$variable <- as.character(emm_df$variable)
              raw_summary$variable <- as.character(raw_summary$variable)
              
              merge_cols <- "variable"
              if (length(group_long) > 0) merge_cols <- c(merge_cols, group_long)
              merged_df <- merge(emm_df, raw_summary, by = merge_cols, all.x = TRUE)
              
              aesthetics <- private$.plotGroups(merged_df, group_long, factor(merged_df$variable, levels = rm_vars))
              
              lower_col <- grep("lower|LCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
              upper_col <- grep("upper|UCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
              ci_half <- (merged_df[[upper_col]] - merged_df[[lower_col]]) / 2
              
              plot_stats_list[["Repeated_Measures"]] <- data.frame(
                x_var = factor(aesthetics$x_var, levels = rm_vars),
                fill_var = factor(aesthetics$fill_var),
                mean = merged_df$emmean,
                sd = merged_df$sd,
                se = merged_df$SE,
                ci_half = ci_half
              )
            }
          }
        }
      }
      
      if (isTRUE(showTable)) {
        table <- self$results$descriptiveTable
        table$deleteRows()
        
        if (has_cov) {
          if (analysisType == 'cross_sectional') {
            deps <- self$options$deps
            row_index <- 1
            for (dep in deps) {
              cols_needed <- c(dep, groups_vars, cov_var)
              df_temp <- data[, cols_needed, drop = FALSE]
              df_temp <- df_temp[complete.cases(df_temp), ]
              if (nrow(df_temp) == 0) next
              for (v in groups_vars) df_temp[[v]] <- as.factor(df_temp[[v]])
              
              formula_str <- paste(private$.quote_names(dep), "~", paste(private$.quote_names(groups_vars), collapse = " * "), "+", private$.quote_names(cov_var))
              fit <- tryCatch({
                stats::lm(as.formula(formula_str), data = df_temp)
              }, error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
              
              if (!is.null(fit)) {
                emm <- tryCatch({
                  emmeans::emmeans(fit, specs = groups_vars, level = ciWidth)
                }, error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
                
                if (!is.null(emm)) {
                  emm_df <- as.data.frame(emm)
                  
                  raw_summary <- df_temp %>%
                    dplyr::group_by(dplyr::across(dplyr::all_of(groups_vars))) %>%
                    dplyr::summarise(
                      n = sum(!is.na(.data[[dep]])),
                      sd = sd(.data[[dep]], na.rm = TRUE),
                      median = median(.data[[dep]], na.rm = TRUE),
                      min = min(.data[[dep]], na.rm = TRUE),
                      max = max(.data[[dep]], na.rm = TRUE),
                      .groups = 'drop'
                    )
                  
                  merged_df <- merge(emm_df, raw_summary, by = groups_vars, all.x = TRUE)
                  lower_col <- grep("lower|LCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
                  upper_col <- grep("upper|UCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
                  
                  for (j in 1:nrow(merged_df)) {
                    row_data <- merged_df[j, ]
                    row_values <- list(
                      variable = dep,
                      n = row_data$n,
                      mean = row_data$emmean,
                      sd = row_data$sd,
                      se = row_data$SE,
                      ci_lower = row_data[[lower_col]],
                      ci_upper = row_data[[upper_col]],
                      median = row_data$median,
                      min = row_data$min,
                      max = row_data$max
                    )
                    for (g_var in grouping_vars) {
                      row_values[[private$.groupColumn(g_var)]] <- as.character(row_data[[g_var]])
                    }
                    table$addRow(rowKey = row_index, values = row_values)
                    row_index <- row_index + 1
                  }
                }
              }
            }
          } else {
            rm_vars <- self$options$rm_vars
            cols_needed <- rm_vars
            if (length(group_long) > 0) cols_needed <- c(cols_needed, group_long)
            cols_needed <- c(cols_needed, cov_var)
            
            data_clean <- data[complete.cases(data[, cols_needed, drop = FALSE]), , drop = FALSE]
            if (length(group_long) > 0) {
              for (v in group_long) data_clean[[v]] <- as.factor(data_clean[[v]])
            }
            
            if (length(group_long) > 0) {
              formula_str <- paste0("cbind(", paste(private$.quote_names(rm_vars), collapse = ", "), ") ~ ", paste(private$.quote_names(group_long), collapse = " * "), " + ", private$.quote_names(cov_var))
            } else {
              formula_str <- paste0("cbind(", paste(private$.quote_names(rm_vars), collapse = ", "), ") ~ ", private$.quote_names(cov_var))
            }
            
            fit <- if (nrow(data_clean) == 0) NULL else tryCatch({
              stats::lm(as.formula(formula_str), data = data_clean)
            }, error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
            
            if (!is.null(fit)) {
              rm_dim_name <- private$.get_rm_dim_name(fit, rm_vars)
              if (length(group_long) > 0) {
                specs_formula <- as.formula(paste("~", private$.quote_names(rm_dim_name), "|", paste(private$.quote_names(group_long), collapse = " + ")))
              } else {
                specs_formula <- as.formula(paste("~", private$.quote_names(rm_dim_name)))
              }
              
              emm <- tryCatch({
                emmeans::emmeans(fit, specs = specs_formula, level = ciWidth)
              }, error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
              
              if (!is.null(emm)) {
                emm_df <- as.data.frame(emm)
                
                rm_col_name <- NULL
                for (col in names(emm_df)) {
                  if (any(as.character(emm_df[[col]]) %in% rm_vars)) {
                    rm_col_name <- col
                    break
                  }
                }
                if (!is.null(rm_col_name)) {
                  emm_df$variable <- factor(emm_df[[rm_col_name]], levels = rm_vars)
                } else {
                  emm_df$variable <- factor(emm_df[[rm_dim_name]], levels = rm_vars)
                }
                
                group_cols_summary <- if (length(group_long) > 0) c("variable", group_long) else "variable"
                
                raw_summary <- data_clean[, c(rm_vars, group_long), drop = FALSE] %>%
                  tidyr::pivot_longer(cols = dplyr::all_of(rm_vars), names_to = "variable", values_to = "value") %>%
                  dplyr::mutate(variable = factor(variable, levels = rm_vars)) %>%
                  dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_summary))) %>%
                  dplyr::summarise(
                    n = sum(!is.na(value)),
                    sd = sd(value, na.rm = TRUE),
                    median = median(value, na.rm = TRUE),
                    min = min(value, na.rm = TRUE),
                    max = max(value, na.rm = TRUE),
                    .groups = 'drop'
                  )
                
                emm_df$variable <- as.character(emm_df$variable)
                raw_summary$variable <- as.character(raw_summary$variable)
                
                merge_cols <- "variable"
                if (length(group_long) > 0) merge_cols <- c(merge_cols, group_long)
                merged_df <- merge(emm_df, raw_summary, by = merge_cols, all.x = TRUE)
                
                merged_df$variable <- factor(merged_df$variable, levels = rm_vars)
                if (length(group_long) > 0) {
                  merged_df <- merged_df[order(match(as.character(merged_df$variable), rm_vars), merged_df[[group_long[1]]]), ]
                } else {
                  merged_df <- merged_df[order(match(as.character(merged_df$variable), rm_vars)), ]
                }
                
                lower_col <- grep("lower|LCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
                upper_col <- grep("upper|UCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
                
                for (j in 1:nrow(merged_df)) {
                  row_data <- merged_df[j, ]
                  row_values <- list(
                    variable = as.character(row_data$variable),
                    n = row_data$n,
                    mean = row_data$emmean,
                    sd = row_data$sd,
                    se = row_data$SE,
                    ci_lower = row_data[[lower_col]],
                    ci_upper = row_data[[upper_col]],
                    median = row_data$median,
                    min = row_data$min,
                    max = row_data$max
                  )
                  for (g_var in grouping_vars) {
                    row_values[[private$.groupColumn(g_var)]] <- as.character(row_data[[g_var]])
                  }
                  table$addRow(rowKey = j, values = row_values)
                }
              }
            }
          }
        } else {
          summary_df <- NULL
          if (analysisType == 'cross_sectional') {
            deps <- self$options$deps
            if (length(deps) > 0) {
              grouping_cols <- "variable"
              if (length(grouping_vars) > 0) {
                grouping_cols <- c(grouping_cols, grouping_vars)
              }
              summary_df <- data[, c(deps, grouping_vars), drop = FALSE] %>%
                tidyr::pivot_longer(cols = dplyr::all_of(deps), names_to = "variable", values_to = "value") %>%
                dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
                dplyr::summarise(
                  n = sum(!is.na(value)),
                  mean = mean(value, na.rm = TRUE),
                  sd = sd(value, na.rm = TRUE),
                  median = median(value, na.rm = TRUE),
                  min = min(value, na.rm = TRUE),
                  max = max(value, na.rm = TRUE),
                  .groups = 'drop'
                )
            }
          } else {
            rm_vars <- self$options$rm_vars
            if (length(rm_vars) > 0) {
              grouping_cols <- "variable"
              if (length(grouping_vars) > 0) {
                grouping_cols <- c(grouping_cols, grouping_vars)
              }
              summary_df <- data[, c(rm_vars, grouping_vars), drop = FALSE] %>%
                tidyr::pivot_longer(cols = dplyr::all_of(rm_vars), names_to = "variable", values_to = "value") %>%
                dplyr::mutate(variable = factor(variable, levels = rm_vars)) %>%
                dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
                dplyr::summarise(
                  n = sum(!is.na(value)),
                  mean = mean(value, na.rm = TRUE),
                  sd = sd(value, na.rm = TRUE),
                  median = median(value, na.rm = TRUE),
                  min = min(value, na.rm = TRUE),
                  max = max(value, na.rm = TRUE),
                  .groups = 'drop'
                )
            }
          }
          
          if (!is.null(summary_df) && nrow(summary_df) > 0) {
            for (i in 1:nrow(summary_df)) {
              row_data <- as.list(summary_df[i, ])
              if (row_data$n == 0) {
                row_data[c("mean", "sd", "median", "min", "max")] <- list(NA, NA, NA, NA, NA)
              }
              se <- if (row_data$n > 1) row_data$sd / sqrt(row_data$n) else NA
              t_crit <- if (row_data$n > 1) stats::qt(1 - (1 - ciWidth) / 2, df = row_data$n - 1) else NA
              
              row_values <- list(
                variable = row_data$variable,
                n = row_data$n,
                mean = row_data$mean,
                sd = row_data$sd,
                se = se,
                ci_lower = if (!is.na(t_crit)) row_data$mean - t_crit * se else NA,
                ci_upper = if (!is.na(t_crit)) row_data$mean + t_crit * se else NA,
                median = row_data$median,
                min = row_data$min,
                max = row_data$max
              )
              for (g_var in grouping_vars) {
                row_values[[private$.groupColumn(g_var)]] <- as.character(row_data[[g_var]])
              }
              
              table$addRow(rowKey = i, values = row_values)
            }
          }
        }
      }
      
      for (dep_key in names(plot_df_list)) {
        state <- list(
          data = plot_df_list[[dep_key]][, c("y", "x_var", "fill_var", "leg_title"), drop = FALSE],
          dep = dep_key,
          stats_df = plot_stats_list[[dep_key]]
        )
        self$results$plots$get(key = dep_key)$setState(state)
      }

      num_vars <- if (analysisType == 'cross_sectional') self$options$deps else self$options$rm_vars
      if (length(num_vars) >= 2) {
        private$.runCorrelationMatrix(num_vars, data, self$options$corrMethod)
        private$.setHeatmapState(num_vars, data, self$options$corrMethod, self$options$corrCluster)
      }
      empty_plots <- names(plot_df_list)[vapply(plot_df_list, nrow, integer(1)) == 0]
      private$.presentationMessages(if (length(empty_plots) > 0) paste0("No complete observations for: ", paste(empty_plots, collapse = ", "), ". The affected plots were not generated; check missing values in the selected measures, groups and covariate.") else character())
    },
    .runDistributionPresentation = function(data, grouping_vars, has_cov) {
      longitudinal <- self$options$analysisType == 'longitudinal'
      variables <- if (longitudinal) self$options$rm_vars else self$options$deps
      keys <- if (longitudinal) 'Repeated_Measures' else variables
      table <- self$results$descriptiveTable
      table$deleteRows()
      table$setNote('covariate_note', NULL)
      table$setNote('distribution_failures', NULL)
      for (key in keys) self$results$plots$get(key = key)$setState(NULL)
      family <- self$options$summary_distribution
      family_label <- if (family == 'gamma') 'Gamma' else 'Inverse Gaussian'
      method_note <- paste0(family_label, ' fitted separately by group and occasion using maximum likelihood. SD is fitted data dispersion; SE = fitted SD / sqrt(N) is plug-in asymptotic uncertainty of the mean. ', self$options$ciWidth, '% CI uses profile likelihood with a chi-square(1) likelihood-ratio cutoff. SD/SE bars are symmetric, not coverage intervals; CI limits may be asymmetric. Observed N, median, minimum and maximum are not model adjusted. These summaries do not test changes between occasions or adjust for covariates.')
      table$setNote('distribution', method_note)
      if ((!longitudinal && (length(variables) == 0 || length(grouping_vars) == 0)) || (longitudinal && length(variables) < 2)) {
        private$.presentationMessages(if (longitudinal) 'Longitudinal: Assign at least TWO Repeated Measures Variables.' else 'Cross-Sectional: Assign at least one DV and one Grouping Variable.')
        return()
      }
      for (variable in variables) data[[variable]] <- private$.to_numeric_vec(data[[variable]])
      for (group in grouping_vars) data[[group]] <- as.factor(data[[group]])
      if (length(variables) >= 2) {
        private$.runCorrelationMatrix(variables, data, self$options$corrMethod)
        private$.setHeatmapState(variables, data, self$options$corrMethod, self$options$corrCluster)
      }
      if (has_cov) {
        private$.presentationMessages(paste0(family_label, ': remove the selected covariate to use descriptive distribution fits, or select Normal to retain the existing covariate-adjusted analysis. No covariate was ignored or removed.'))
        return()
      }
      plot_data <- setNames(vector('list', length(keys)), keys)
      plot_stats <- setNames(vector('list', length(keys)), keys)
      failures <- character()
      failed_keys <- character()
      row_index <- 0
      cells <- if (length(grouping_vars) > 0) dplyr::group_rows(dplyr::group_by(data[, grouping_vars, drop = FALSE], dplyr::across(dplyr::all_of(grouping_vars)))) else list(seq_len(nrow(data)))
      for (variable in variables) {
        key <- if (longitudinal) 'Repeated_Measures' else variable
        frame <- data.frame(y = data[[variable]])
        if (longitudinal) {
          frame$x_var <- factor(rep(variable, nrow(frame)), levels = variables)
          frame$fill_var <- if (length(grouping_vars) > 0) interaction(data[grouping_vars], sep = ' - ', drop = TRUE) else frame$x_var
          legend <- if (length(grouping_vars) > 0) paste(private$.groupLabels(grouping_vars), collapse = ' & ') else 'Time'
        } else if (length(grouping_vars) == 1) {
          frame$x_var <- data[[grouping_vars[1]]]
          frame$fill_var <- frame$x_var
          legend <- private$.groupLabels(grouping_vars[1])
        } else if (self$options$intStyle == 'legend') {
          frame$x_var <- data[[grouping_vars[1]]]
          frame$fill_var <- interaction(data[grouping_vars[-1]], sep = ' - ', drop = TRUE)
          legend <- paste(private$.groupLabels(grouping_vars[-1]), collapse = ' & ')
        } else {
          frame$x_var <- interaction(data[grouping_vars], sep = ' - ', drop = TRUE)
          frame$fill_var <- frame$x_var
          legend <- 'Groups'
        }
        frame$leg_title <- legend
        plot_data[[key]] <- dplyr::bind_rows(plot_data[[key]], frame[!is.na(frame$y), c('y', 'x_var', 'fill_var', 'leg_title'), drop = FALSE])
        for (cell_rows in cells) {
          cell <- frame[cell_rows, , drop = FALSE]
          context <- paste(c(paste0(if (longitudinal) 'Occasion=' else 'Variable=', variable), vapply(grouping_vars, function(group) paste0(private$.groupLabels(group), '=', as.character(data[[group]][cell_rows[1]])), character(1))), collapse = '; ')
          fitted <- tryCatch(private$.fitDistribution(cell$y, family, self$options$ciWidth / 100), error = function(error) error)
          if (inherits(fitted, 'error')) {
            failures <- c(failures, paste0(context, ': ', conditionMessage(fitted)))
            failed_keys <- c(failed_keys, key)
            observed <- cell$y[!is.na(cell$y)]
            finite <- length(observed) > 0 && all(is.finite(observed))
            fitted <- list(n = length(observed), mean = NA_real_, sd = NA_real_, se = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, median = if (finite) stats::median(observed) else NA_real_, min = if (finite) min(observed) else NA_real_, max = if (finite) max(observed) else NA_real_)
          } else {
            plot_stats[[key]] <- dplyr::bind_rows(plot_stats[[key]], data.frame(x_var = cell$x_var[1], fill_var = cell$fill_var[1], as.data.frame(fitted[c('mean', 'sd', 'se', 'ci_lower', 'ci_upper')])))
          }
          if (isTRUE(self$options$showTable)) {
            row_index <- row_index + 1
            row_values <- c(list(variable = variable), fitted)
            for (group in grouping_vars) row_values[[private$.groupColumn(group)]] <- as.character(data[[group]][cell_rows[1]])
            table$addRow(rowKey = row_index, values = row_values)
          }
        }
      }
      for (key in setdiff(keys, failed_keys)) {
        self$results$plots$get(key = key)$setState(list(data = plot_data[[key]], dep = key, stats_df = plot_stats[[key]], distribution_label = family_label))
      }
      table$setNote('distribution_failures', if (length(failures) > 0) paste(failures, collapse = '\n') else NULL)
      private$.presentationMessages(if (length(failures) > 0) c('The affected plots were not generated. No observations were silently excluded and no substitute distribution was used.', failures) else character())
    },

    .formatTableByNorm = function(table) {
      format <- self$options$tableFormat
      title <- 'Descriptive statistics by group'
      note <- 'M = mean; SD = standard deviation; SE = standard error; CI = confidence interval; N = sample size.'
      analysisType <- self$options$analysisType
      col_titles <- switch(
        format,
        'abnt' = list(
          variable = if (analysisType == 'longitudinal') 'Tempo' else 'Vari\u00e1vel',
          n = 'N', mean = 'M\u00e9dia', sd = 'DP', se = 'EP',
          ci_lower = 'IC Inferior', ci_upper = 'IC Superior', 
          median = 'Mediana', min = 'M\u00edn', max = 'M\u00e1x'
        ),
        'apa' = list(
          variable = if (analysisType == 'longitudinal') 'Time' else 'Variable',
          n = 'N', mean = 'M', sd = 'SD', se = 'SE', 
          ci_lower = 'LL', ci_upper = 'UL', 
          median = 'Mdn', min = 'Min', max = 'Max'
        ),
        'vancouver' = list(
          variable = if (analysisType == 'longitudinal') 'Time' else 'Variable',
          n = 'n', mean = 'Mean', sd = 'SD', se = 'SE', 
          ci_lower = 'CI Lower', ci_upper = 'CI Upper', 
          median = 'Median', min = 'Min', max = 'Max'
        ),
        'chicago' = list(
          variable = if (analysisType == 'longitudinal') 'Time' else 'Variable',
          n = 'N', mean = 'Mean', sd = 'SD', se = 'SE', 
          ci_lower = 'CI Lower', ci_upper = 'CI Upper', 
          median = 'Mdn', min = 'Min', max = 'Max'
        )
      )
      
      for (col_name in names(col_titles)) {
        if (col_name %in% names(table$columns)) {
          table$getColumn(col_name)$setTitle(col_titles[[col_name]])
        }
      }
      
      title_to_set <- title
      if (title == 'Descriptive statistics by group') {
        title_to_set <- switch(
          format,
          'abnt' = 'Estat\u00edsticas descritivas por grupo',
          'Descriptive statistics by group'
        )
      }
      if (nzchar(title_to_set)) {
        table$setTitle(title_to_set)
      }
      
      note_to_set <- note
      if (note == 'M = mean; SD = standard deviation; SE = standard error; CI = confidence interval; N = sample size.') {
        note_to_set <- switch(
          format,
          'abnt' = 'M\u00e9dia = m\u00e9dia; DP = desvio padr\u00e3o; EP = erro padr\u00e3o; IC = intervalo de confian\u00e7a; N = tamanho da amostra.',
          'apa' = 'M = mean; SD = standard deviation; SE = standard error; CI = confidence interval; N = sample size.',
          'vancouver' = 'Mean = mean; SD = standard deviation; SE = standard error; CI = confidence interval; n = sample size.',
          'chicago' = 'Mean = mean; SD = standard deviation; SE = standard error; CI = confidence interval; N = sample size.'
        )
      }
      
      table$setNote(key = 'general_note', note = note_to_set)
      
      ci_note_text <- switch(
        format,
        'abnt' = paste0('IC = ', self$options$ciWidth, '% Intervalo de Confian\u00e7a.'),
        paste0('CI = ', self$options$ciWidth, '% Confidence Interval.')
      )
      table$setNote(key = 'ci_note', note = ci_note_text)
      
      has_cov <- FALSE
      if (analysisType == 'cross_sectional') {
        has_cov <- !is.null(self$options$cov) && self$options$cov != ""
      } else {
        has_cov <- !is.null(self$options$cov_long) && self$options$cov_long != ""
      }
      if (has_cov) {
        cov_note_text <- switch(
          format,
          'abnt' = "Nota. Estat\u00edsticas ajustadas (M\u00e9dias Marginais Estimadas) s\u00e3o calculadas controlando para a covari\u00e1vel usando o pacote R 'emmeans'.",
          "Adjusted statistics (Estimated Marginal Means) are computed controlling for the covariate using the 'emmeans' R package."
        )
        table$setNote(key = 'covariate_note', note = cov_note_text)
      } else {
        table$setNote(key = 'covariate_note', NULL)
      }
    },
    
    .plotGroup = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return(FALSE)
      
      state     <- image$state
      plot_data <- state$data
      dep       <- state$dep
      opts      <- self$options
      
      if (nrow(plot_data) == 0) return(FALSE)
      
      if (!is.null(state$stats_df)) {
        stats_df <- state$stats_df
      } else {
        stats_df <- private$.calcStats(plot_data, opts$ciWidth / 100)
      }
      n_colors  <- nlevels(plot_data$fill_var)
      colors    <- private$.resolveColors(opts$colorPalette, n_colors, 
                                          list(opts$color1, opts$color2, opts$color3, 
                                               opts$color4, opts$color5, opts$color6,
                                               opts$color7, opts$color8))
      
      custom_theme <- ggplot2::theme(
        plot.title      = ggplot2::element_text(size = opts$fontSizeTitle),
        plot.subtitle   = ggplot2::element_text(size = opts$fontSizeSubtitle),
        axis.title      = ggplot2::element_text(size = opts$fontSizeAxisTitle),
        axis.text       = ggplot2::element_text(size = opts$fontSizeAxis),
        axis.text.x     = ggplot2::element_text(size = opts$fontSizeAxis, angle = opts$xAngle, hjust = if(opts$xAngle > 0) 1 else 0.5),
        axis.text.y     = ggplot2::element_text(size = opts$fontSizeAxis), 
        legend.text     = ggplot2::element_text(size = opts$fontSizeLegend),
        legend.title    = ggplot2::element_text(size = opts$fontSizeLegend),
        legend.position = opts$legendPosition,
        panel.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.grid      = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.border    = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        legend.box.background = ggplot2::element_blank(),
        legend.key      = ggplot2::element_blank(),
        axis.line.x.bottom = ggplot2::element_line(colour = '#333333', linewidth = opts$axisLineWidth),
        axis.line.y.left = ggplot2::element_line(colour = '#333333', linewidth = opts$axisLineWidth),
        axis.line       = ggplot2::element_line(linewidth = opts$axisLineWidth),
        axis.ticks      = ggplot2::element_line(linewidth = opts$axisLineWidth)
      )
      
      error_col <- switch(opts$errorType, sd = 'sd', se = 'se', ci = 'ci_half')
      if (opts$errorType == 'ci' && all(c('ci_lower', 'ci_upper') %in% names(stats_df))) {
        stats_df$ymin <- stats_df$ci_lower
        stats_df$ymax <- stats_df$ci_upper
      } else {
        stats_df$ymin <- stats_df$mean - stats_df[[error_col]]
        stats_df$ymax <- stats_df$mean + stats_df[[error_col]]
      }
      
      dodge_val <- if (opts$plotType == 'meandp') 0.2 else 0.8
      jitter_pos <- ggplot2::position_jitterdodge(jitter.width = 0.1, dodge.width = dodge_val)
      
      group_var <- if (opts$analysisType == 'longitudinal' && length(opts$group_long) > 0) {
        ggplot2::aes(group = fill_var)
      } else {
        ggplot2::aes(group = 1)
      }
      
      p <- switch(opts$plotType,
                  bar = {
                    gg <- ggplot2::ggplot(stats_df, ggplot2::aes(x = x_var, y = mean, fill = fill_var)) +
                      ggplot2::geom_col(position = ggplot2::position_dodge(dodge_val), width = opts$barWidth, color = 'black', linewidth = opts$lineWidth, alpha = opts$barAlpha) +
                      ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax), position = ggplot2::position_dodge(dodge_val), width = opts$barWidth * 0.3, linewidth = opts$errorLineWidth)
                    if (opts$showPoints) gg <- gg + ggplot2::geom_point(data = plot_data, ggplot2::aes(x = x_var, y = y, fill = fill_var), color = "black", shape = 1, position = jitter_pos, size = opts$pointSize, alpha = opts$pointAlpha, inherit.aes = FALSE)
                    gg
                  },
                  boxplot = {
                    gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x_var, y = y, fill = fill_var)) +
                      ggplot2::geom_boxplot(position = ggplot2::position_dodge(dodge_val), width = opts$barWidth, linewidth = opts$lineWidth, alpha = opts$barAlpha, outlier.shape = NA)
                    if (opts$showPoints) gg <- gg + ggplot2::geom_point(ggplot2::aes(fill = fill_var), color = "black", shape = 1, position = jitter_pos, size = opts$pointSize, alpha = opts$pointAlpha)
                    gg
                  },
                  meandp = {
                    gg <- ggplot2::ggplot(stats_df, ggplot2::aes(x = x_var, y = mean, color = fill_var)) +
                      ggplot2::geom_point(position = ggplot2::position_dodge(dodge_val), size = opts$pointSize * 2) +
                      ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax), position = ggplot2::position_dodge(dodge_val), width = 0.2, linewidth = opts$errorLineWidth)
                    
                    if (opts$analysisType == 'longitudinal' && opts$showLines) {
                      gg <- gg + ggplot2::geom_line(group_var, position = ggplot2::position_dodge(dodge_val), linewidth = opts$lineWidth)
                    }
                    if (opts$showPoints) gg <- gg + ggplot2::geom_point(data = plot_data, ggplot2::aes(x = x_var, y = y, fill = fill_var), color = "black", shape = 1, position = jitter_pos, size = opts$pointSize, alpha = opts$pointAlpha, inherit.aes = FALSE)
                    gg
                  },
                  density = {
                    gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = y, fill = fill_var, color = fill_var)) +
                      ggplot2::geom_density(alpha = 0.35, linewidth = opts$lineWidth)
                    gg
                  }
      )
      
      p <- p + ggtheme + custom_theme + ggplot2::scale_fill_manual(values = colors) + ggplot2::scale_color_manual(values = colors)
      y_expand <- if (opts$removeGap) ggplot2::expansion(mult = c(0, 0.05)) else ggplot2::waiver()
      
      if (opts$customYAxis && opts$plotType != 'density') {
        y_breaks <- if (opts$yStep > 0) seq(opts$yMin, opts$yMax, by = opts$yStep) else ggplot2::waiver()
        p <- p + ggplot2::scale_y_continuous(expand = y_expand, limits = c(opts$yMin, opts$yMax), breaks = y_breaks)
      } else {
        p <- p + ggplot2::scale_y_continuous(expand = y_expand)
      }
      
      label_x <- if(opts$xLabel != '') opts$xLabel else (if(opts$plotType == 'density') dep else (if(opts$analysisType == 'longitudinal') "Time" else opts$group[1]))
      plot_title <- if(opts$titleText != '') opts$titleText else NULL
      distribution_caption <- if (!is.null(state$distribution_label)) paste0(state$distribution_label, ' maximum-likelihood fit; ', switch(opts$errorType, sd = 'fitted SD (data dispersion)', se = 'asymptotic SE of the mean', ci = paste0(opts$ciWidth, '% profile-likelihood CI of the mean'))) else NULL
      
      p <- p + ggplot2::labs(title = plot_title,
                                            x = label_x, y = if(opts$yLabel != '') opts$yLabel else (if(opts$plotType == 'density') "Density" else dep),
                                            fill = plot_data$leg_title[1], color = plot_data$leg_title[1], caption = distribution_caption)
      print(p)
      return(TRUE)
    },
    
    .calcStats = function(data, ci_prop) {
      stats <- aggregate(y ~ x_var + fill_var, data = data, function(vals) {
        n <- length(vals); m <- mean(vals)
        sd_v <- if(n > 1) stats::sd(vals) else NA; se_v <- if(n > 1) sd_v/sqrt(n) else NA
        ci_h <- if(n > 1) stats::qt(1 - (1-ci_prop)/2, df = n-1) * se_v else NA
        c(mean = m, sd = sd_v, se = se_v, ci_half = ci_h)
      })
      res <- data.frame(x_var = stats$x_var, fill_var = stats$fill_var, stats$y); res
    },
    
    .resolveColors = function(palette, n, manual) {
      if (palette == 'manual') {
        mc <- unlist(manual)
        mc <- mc[mc != '' & mc != 'none']
        if (length(mc) > 0) return(rep(mc, length.out = n))
        palette <- 'set1'
      }
      if (palette == 'dark2') palette <- 'grayscale'
      if (palette == 'grayscale') {
        if (n <= 1) return('#FFFFFF')
        gray_ramp <- grDevices::colorRampPalette(c('#E0E0E0', '#404040'))(n - 1)
        return(c('#FFFFFF', gray_ramp))
      }
      pals <- list(
        set1 = c('#E41A1C', '#377EB8', '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', '#F781BF'),
        set2 = c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#FFD92F', '#E5C494', '#B3B3B3'),
        set3 = c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#FCCDE5'),
        pastel1 = c('#FBB4AE', '#B3CDE3', '#CCEBC5', '#DECBE4', '#FED9A6', '#FFFFCC', '#E5D8BD', '#FDDAEC'),
        pastel2 = c('#B3E2CD', '#FDCDAC', '#CBD5E8', '#F4CAE4', '#E6F5C9', '#FFF2AE', '#F1E2CC', '#CCCCCC'),
        grayscale = c('#FFFFFF', '#E0E0E0', '#C8C8C8', '#B0B0B0', '#989898', '#808080', '#686868', '#505050'),
        viridis = c('#440154', '#414487', '#2A788E', '#22A884', '#7AD151', '#FDE725'),
        plasma = c('#0D0887', '#6A00A8', '#B12A90', '#E16462', '#FCA636', '#F0F921')
      )
      base <- pals[[palette]]; if (is.null(base)) base <- pals$set1
      if (n <= length(base)) return(base[1:n])
      return(grDevices::colorRampPalette(base)(n))
    },
    .syncCorrTableColumns = function(corr_tbl, num_vars) {
      get_col_names <- function(tbl) {
        tryCatch(sapply(tbl$columns, function(col) col$name), error = function(e) character(0))
      }
      expected_cols <- paste0('col_', seq_along(num_vars))
      for (col_id in setdiff(get_col_names(corr_tbl), c('num', 'var', 'mean', 'sd', expected_cols))) corr_tbl$getColumn(col_id)$setVisible(FALSE)
      current_cols <- get_col_names(corr_tbl)
      for (j in seq_along(num_vars)) {
        col_id <- paste0('col_', j)
        corr_tbl$addColumn(name = col_id, title = as.character(j), type = 'number', format = 'zto')
      }
    },
    
    .runCorrelationMatrix = function(num_vars, data, method) {
      if (!isTRUE(self$options$showCorrMatrix)) return()
      tbl <- self$results$corrMatrixTable
      
      k <- length(num_vars)
      unavailable <- character()
      for (i in seq_len(k)) {
        v_i <- num_vars[i]
        vals_i <- private$.to_numeric_vec(data[[v_i]])
        m <- mean(vals_i, na.rm = TRUE)
        s <- stats::sd(vals_i, na.rm = TRUE)
        row_vals <- list(num = as.character(i), var = v_i, mean = if (is.na(m)) NA else m, sd = if (is.na(s)) NA else s)
        
        symbols <- list()
        for (j in seq_len(k)) {
          col_id <- paste0('col_', j)
          if (j == i) {
            row_vals[[col_id]] <- NA
          } else if (j > i) {
            row_vals[[col_id]] <- NA
          } else {
            v_j <- num_vars[j]
            vals_j <- private$.to_numeric_vec(data[[v_j]])
            ct <- tryCatch(stats::cor.test(vals_i, vals_j, method = method), error = function(e) NULL)
            if (!is.null(ct) && is.finite(ct$estimate) && is.finite(ct$p.value)) {
              r_val <- ct$estimate
              p_val <- ct$p.value
              stars <- if (p_val < 0.001) '***' else if (p_val < 0.01) '**' else if (p_val < 0.05) '*' else ''
              row_vals[[col_id]] <- as.numeric(r_val)
              if (nzchar(stars)) symbols[[col_id]] <- stars
            } else {
              row_vals[[col_id]] <- NA
              unavailable <- c(unavailable, paste(v_i, v_j, sep = ' / '))
            }
          }
        }
        tbl$setRow(rowKey = i, values = row_vals)
        for (col_id in names(symbols)) tbl$addSymbol(col = col_id, symbol = symbols[[col_id]], rowKey = i)
      }
      tbl$setNote('unavailable', if (length(unavailable) > 0) paste('Correlations unavailable (insufficient complete pairs or zero variance):', paste(unavailable, collapse = '; ')) else NULL)
      tbl$setNote('stars_note', '* p < .05, ** p < .01, *** p < .001. Lower triangle contains bivariate correlation coefficients; missing entries there indicate unavailable correlations. Diagonal and upper triangle are intentionally omitted. M and SD represent mean and standard deviation.')
    },
    
    .setHeatmapState = function(num_vars, data, method, cluster) {
      if (!isTRUE(self$options$showCorrHeatmap)) return()
      sub_df <- as.data.frame(lapply(data[, num_vars, drop = FALSE], private$.to_numeric_vec))
      cor_mat <- stats::cor(sub_df, use = 'pairwise.complete.obs', method = method)
      self$results$corrHeatmapPlot$setState(list(cor_mat = cor_mat, method = method, cluster = cluster))
    },
    
    .plotCorrHeatmap = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return(FALSE)
      state <- image$state
      cor_mat <- state$cor_mat
      num_vars <- colnames(cor_mat)
      method <- state$method
      unavailable <- any(!is.finite(cor_mat))
      cluster <- isTRUE(state$cluster) && !unavailable
      subtitle <- if (unavailable) paste(strwrap(paste0('NA = correlation unavailable.', if (isTRUE(state$cluster)) ' Clustering not applied because some correlations are unavailable.' else ''), width = 60), collapse = '\n') else NULL
      
      ordered_vars <- num_vars
      if (isTRUE(cluster) && ncol(cor_mat) > 2) {
        dist_mat <- stats::as.dist(1 - abs(cor_mat))
        hc <- tryCatch(stats::hclust(dist_mat, method = 'complete'), error = function(e) jmvcore::reject(conditionMessage(e), code = 'presentation_estimation_failed'))
        if (!is.null(hc)) {
          ordered_vars <- colnames(cor_mat)[hc$order]
        }
      }
      
      df_heat <- expand.grid(Var1 = ordered_vars, Var2 = ordered_vars, stringsAsFactors = FALSE)
      df_heat$value <- apply(df_heat, 1, function(row) cor_mat[row['Var1'], row['Var2']])
      df_heat$label <- ifelse(is.finite(df_heat$value), sprintf('%.2f', df_heat$value), 'NA')
      df_heat$Var1 <- factor(df_heat$Var1, levels = ordered_vars)
      df_heat$Var2 <- factor(df_heat$Var2, levels = rev(ordered_vars))
      
      p <- ggplot2::ggplot(df_heat, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_tile(color = 'white', linewidth = 0.5) +
        ggplot2::geom_text(ggplot2::aes(label = label), color = ifelse(is.finite(df_heat$value) & abs(df_heat$value) > 0.6, 'white', 'black'), size = 3.5) +
        ggtheme +
        ggplot2::scale_fill_gradient2(low = '#2166AC', mid = '#FFFFFF', high = '#B2182B', midpoint = 0, limits = c(-1, 1), name = 'Correlation', na.value = '#D9D9D9') +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10, face = 'bold'),
          axis.text.y = ggplot2::element_text(size = 10, face = 'bold'),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_text(size = 10, hjust = 0),
          panel.background = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          legend.background = ggplot2::element_blank(),
          legend.box.background = ggplot2::element_blank(),
          legend.key = ggplot2::element_blank(),
          axis.line.x.bottom = ggplot2::element_line(colour = '#333333', linewidth = self$options$axisLineWidth),
          axis.line.y.left = ggplot2::element_line(colour = '#333333', linewidth = self$options$axisLineWidth),
          panel.grid = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        ) +
        ggplot2::labs(title = paste0('Correlation Heatmap (', tools::toTitleCase(method), if (cluster) ', Clustered' else '', ')'), subtitle = subtitle)
      print(p)
      TRUE
    }
  )
)

