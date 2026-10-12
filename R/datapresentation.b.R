
DataPresentationClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "DataPresentationClass",
  inherit = DataPresentationBase,
  private = list(
    
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
      plots <- self$results$plots
      if (self$options$analysisType == 'cross_sectional') {
        keys <- self$options$deps
      } else {
        keys <- c("Repeated_Measures")
      }
      
      if (length(keys) > 0) {
        for (k in keys) plots$addItem(key = k)
      }
    },
    
    .syncTableColumns = function(table, grouping_vars) {
      get_col_names <- function(tbl) {
        tryCatch(sapply(tbl$columns, function(col) col$name), error = function(e) character(0))
      }
      base_cols <- c("variable", "n", "mean", "sd", "se", "ci_lower", "ci_upper", "median", "min", "max")
      current_cols <- get_col_names(table)
      cols_to_remove <- setdiff(current_cols, c(base_cols, grouping_vars))
      for (col_name in cols_to_remove) {
        try(table$deleteColumn(name = col_name), silent = TRUE)
      }
      current_cols_after_remove <- get_col_names(table)
      for (i in seq_along(grouping_vars)) {
        g_var <- grouping_vars[[i]]
        if (!g_var %in% current_cols_after_remove) {
          table$addColumn(name = g_var, title = g_var, type = 'text', combineBelow = TRUE, index = i + 1)
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
      
      has_cov <- FALSE
      cov_var <- NULL
      if (analysisType == 'cross_sectional') {
        has_cov <- !is.null(self$options$cov) && self$options$cov != ""
        cov_var <- if (has_cov) self$options$cov else NULL
      } else {
        has_cov <- !is.null(self$options$cov_long) && self$options$cov_long != ""
        cov_var <- if (has_cov) self$options$cov_long else NULL
      }
      
      plot_df_list <- list() 
      plot_stats_list <- list()
      
      if (analysisType == 'cross_sectional') {
        deps        <- self$options$deps
        groups_vars <- self$options$group
        
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
          colnames(df_temp)[1] <- "y"
          
          if (length(groups_vars) == 1) {
            df_temp$x_var    <- df_temp[[groups_vars[1]]]
            df_temp$fill_var <- df_temp[[groups_vars[1]]]
            leg_title        <- groups_vars[1]
          } else {
            if (self$options$intStyle == 'legend') {
              df_temp$x_var    <- df_temp[[groups_vars[1]]]
              df_temp$fill_var <- interaction(df_temp[groups_vars[-1]], sep=" - ")
              leg_title        <- paste(groups_vars[-1], collapse=" & ")
            } else {
              df_temp$x_var    <- interaction(df_temp[groups_vars], sep=" - ")
              df_temp$fill_var <- df_temp$x_var
              leg_title        <- "Groups"
            }
          }
          df_temp$leg_title <- leg_title
          plot_df_list[[dep]] <- df_temp[complete.cases(df_temp), ]
          
          if (has_cov) {
            df_clean <- plot_df_list[[dep]]
            formula_str <- paste("y ~", paste(groups_vars, collapse = " * "), "+", cov_var)
            fit <- tryCatch({
              stats::lm(as.formula(formula_str), data = df_clean)
            }, error = function(e) NULL)
            
            if (!is.null(fit)) {
              emm <- tryCatch({
                emmeans::emmeans(fit, specs = groups_vars, level = ciWidth)
              }, error = function(e) NULL)
              
              if (!is.null(emm)) {
                emm_df <- as.data.frame(emm)
                
                raw_summary <- df_clean %>%
                  dplyr::group_by(dplyr::across(dplyr::all_of(groups_vars))) %>%
                  dplyr::summarise(
                    sd = sd(y, na.rm = TRUE),
                    .groups = 'drop'
                  )
                
                merged_df <- merge(emm_df, raw_summary, by = groups_vars, all.x = TRUE)
                
                if (length(groups_vars) == 1) {
                  merged_df$x_var    <- merged_df[[groups_vars[1]]]
                  merged_df$fill_var <- merged_df[[groups_vars[1]]]
                } else {
                  if (self$options$intStyle == 'legend') {
                    merged_df$x_var    <- merged_df[[groups_vars[1]]]
                    merged_df$fill_var <- interaction(merged_df[groups_vars[-1]], sep=" - ")
                  } else {
                    merged_df$x_var    <- interaction(merged_df[groups_vars], sep=" - ")
                    merged_df$fill_var <- merged_df$x_var
                  }
                }
                
                lower_col <- grep("lower|LCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
                upper_col <- grep("upper|UCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
                ci_half <- (merged_df[[upper_col]] - merged_df[[lower_col]]) / 2
                
                plot_stats_list[[dep]] <- data.frame(
                  x_var = factor(merged_df$x_var),
                  fill_var = factor(merged_df$fill_var),
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
        group_long <- self$options$group_long
        
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
        
        if (length(group_long) > 0) {
          info_df <- data_clean[rep(seq_len(nrow(data_clean)), times = length(rm_vars)), group_long, drop = FALSE]
          df_temp <- cbind(stacked, info_df)
          
          if (length(group_long) == 1) {
            df_temp$fill_var <- df_temp[[group_long[1]]]
            leg_title <- group_long[1]
          } else {
            df_temp$fill_var <- interaction(df_temp[group_long], sep=" - ")
            leg_title <- paste(group_long, collapse=" & ")
          }
        } else {
          df_temp <- stacked; df_temp$fill_var <- df_temp$x_var; leg_title <- "Time"
        }
        df_temp$leg_title <- leg_title
        plot_df_list[["Repeated_Measures"]] <- df_temp[complete.cases(df_temp), ]
        
        if (has_cov) {
          if (length(group_long) > 0) {
            formula_str <- paste0("cbind(", paste(rm_vars, collapse = ", "), ") ~ ", paste(group_long, collapse = " * "), " + ", cov_var)
          } else {
            formula_str <- paste0("cbind(", paste(rm_vars, collapse = ", "), ") ~ ", cov_var)
          }
          
          fit <- tryCatch({
            stats::lm(as.formula(formula_str), data = data_clean)
          }, error = function(e) NULL)
          
          if (!is.null(fit)) {
            rm_dim_name <- private$.get_rm_dim_name(fit, rm_vars)
            if (length(group_long) > 0) {
              specs_formula <- as.formula(paste("~", rm_dim_name, "|", paste(group_long, collapse = " + ")))
            } else {
              specs_formula <- as.formula(paste("~", rm_dim_name))
            }
            
            emm <- tryCatch({
              emmeans::emmeans(fit, specs = specs_formula, level = ciWidth)
            }, error = function(e) NULL)
            
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
              
              raw_summary <- data_clean %>%
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
              
              merged_df$x_var <- factor(merged_df$variable, levels = rm_vars)
              if (length(group_long) > 0) {
                if (length(group_long) == 1) {
                  merged_df$fill_var <- merged_df[[group_long[1]]]
                } else {
                  merged_df$fill_var <- interaction(merged_df[group_long], sep=" - ")
                }
              } else {
                merged_df$fill_var <- merged_df$x_var
              }
              
              lower_col <- grep("lower|LCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
              upper_col <- grep("upper|UCL", names(merged_df), value = TRUE, ignore.case = TRUE)[1]
              ci_half <- (merged_df[[upper_col]] - merged_df[[lower_col]]) / 2
              
              plot_stats_list[["Repeated_Measures"]] <- data.frame(
                x_var = factor(merged_df$x_var, levels = rm_vars),
                fill_var = factor(merged_df$fill_var),
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
        
        grouping_vars <- if (analysisType == 'cross_sectional') self$options$group else self$options$group_long
        private$.syncTableColumns(table, grouping_vars)
        private$.formatTableByNorm(table)
        
        if (has_cov) {
          if (analysisType == 'cross_sectional') {
            deps <- self$options$deps
            row_index <- 1
            for (dep in deps) {
              cols_needed <- c(dep, groups_vars, cov_var)
              df_temp <- data[, cols_needed, drop = FALSE]
              colnames(df_temp)[1] <- "y"
              df_temp <- df_temp[complete.cases(df_temp), ]
              for (v in groups_vars) df_temp[[v]] <- as.factor(df_temp[[v]])
              
              formula_str <- paste("y ~", paste(groups_vars, collapse = " * "), "+", cov_var)
              fit <- tryCatch({
                stats::lm(as.formula(formula_str), data = df_temp)
              }, error = function(e) NULL)
              
              if (!is.null(fit)) {
                emm <- tryCatch({
                  emmeans::emmeans(fit, specs = groups_vars, level = ciWidth)
                }, error = function(e) NULL)
                
                if (!is.null(emm)) {
                  emm_df <- as.data.frame(emm)
                  
                  raw_summary <- df_temp %>%
                    dplyr::group_by(dplyr::across(dplyr::all_of(groups_vars))) %>%
                    dplyr::summarise(
                      n = sum(!is.na(y)),
                      sd = sd(y, na.rm = TRUE),
                      median = median(y, na.rm = TRUE),
                      min = min(y, na.rm = TRUE),
                      max = max(y, na.rm = TRUE),
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
                      row_values[[g_var]] <- as.character(row_data[[g_var]])
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
              formula_str <- paste0("cbind(", paste(rm_vars, collapse = ", "), ") ~ ", paste(group_long, collapse = " * "), " + ", cov_var)
            } else {
              formula_str <- paste0("cbind(", paste(rm_vars, collapse = ", "), ") ~ ", cov_var)
            }
            
            fit <- tryCatch({
              stats::lm(as.formula(formula_str), data = data_clean)
            }, error = function(e) NULL)
            
            if (!is.null(fit)) {
              rm_dim_name <- private$.get_rm_dim_name(fit, rm_vars)
              if (length(group_long) > 0) {
                specs_formula <- as.formula(paste("~", rm_dim_name, "|", paste(group_long, collapse = " + ")))
              } else {
                specs_formula <- as.formula(paste("~", rm_dim_name))
              }
              
              emm <- tryCatch({
                emmeans::emmeans(fit, specs = specs_formula, level = ciWidth)
              }, error = function(e) NULL)
              
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
                
                raw_summary <- data_clean %>%
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
                    row_values[[g_var]] <- as.character(row_data[[g_var]])
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
              summary_df <- data %>%
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
              summary_df <- data %>%
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
            private$.syncTableColumns(table, grouping_vars)
            private$.formatTableByNorm(table)
            for (i in 1:nrow(summary_df)) {
              row_data <- as.list(summary_df[i, ])
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
                row_values[[g_var]] <- as.character(row_data[[g_var]])
              }
              
              table$addRow(rowKey = i, values = row_values)
            }
          }
        }
      }
      
      for (dep_key in names(plot_df_list)) {
        state <- list(
          data = plot_df_list[[dep_key]],
          dep = dep_key,
          options = self$options,
          stats_df = plot_stats_list[[dep_key]]
        )
        self$results$plots$get(key = dep_key)$setState(state)
      }
      if (isTRUE(self$options$showFormattedTable) && isTRUE(showTable)) {
        private$.generateApaHtml(table)
      }

      num_vars <- if (analysisType == 'cross_sectional') self$options$deps else self$options$rm_vars
      if (length(num_vars) >= 2) {
        private$.runCorrelationMatrix(num_vars, data, self$options$corrMethod)
        private$.setHeatmapState(num_vars, data, self$options$corrMethod, self$options$corrCluster)
      }
    },
    .formatTableByNorm = function(table) {
      format <- self$options$tableFormat
      title <- self$options$tableTitle
      note <- self$options$tableNote
      analysisType <- self$options$analysisType
      col_titles <- switch(
        format,
        'abnt' = list(
          variable = if (analysisType == 'longitudinal') 'Tempo' else 'VariÃƒÂ¡vel',
          n = 'N', mean = 'MÃƒÂ©dia', sd = 'DP', se = 'EP', 
          ci_lower = 'IC Inferior', ci_upper = 'IC Superior', 
          median = 'Mediana', min = 'MÃƒÂ­n', max = 'MÃƒÂ¡x'
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
      if (title == 'Table 1. Descriptive statistics by group') {
        title_to_set <- switch(
          format,
          'abnt' = 'Tabela 1. EstatÃƒÂ­sticas descritivas por grupo',
          'Table 1. Descriptive statistics by group'
        )
      }
      if (nzchar(title_to_set)) {
        table$setTitle(title_to_set)
      }
      
      note_to_set <- note
      if (note == 'Note. M = mean; SD = standard deviation; SE = standard error; CI = confidence interval; N = sample size.') {
        note_to_set <- switch(
          format,
          'abnt' = 'MÃƒÂ©dia = mÃƒÂ©dia; DP = desvio padrÃƒÂ£o; EP = erro padrÃƒÂ£o; IC = intervalo de confianÃƒÂ§a; N = tamanho da amostra.',
          'apa' = 'M = mean; SD = standard deviation; SE = standard error; CI = confidence interval; N = sample size.',
          'vancouver' = 'Mean = mean; SD = standard deviation; SE = standard error; CI = confidence interval; n = sample size.',
          'chicago' = 'Mean = mean; SD = standard deviation; SE = standard error; CI = confidence interval; N = sample size.'
        )
      }
      
      note_prefix <- switch(
        format,
        'abnt' = "Fonte: ",
        "Note. "
      )
      if (nzchar(note_to_set)) {
        table$setNote(key = 'general_note', note = paste0(note_prefix, note_to_set))
      }
      
      ci_note_text <- switch(
        format,
        'abnt' = paste0('IC = ', self$options$ciWidth, '% Intervalo de ConfianÃƒÂ§a.'),
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
          'abnt' = "Nota. EstatÃƒÂ­sticas ajustadas (MÃƒÂ©dias Marginais Estimadas) sÃƒÂ£o calculadas controlando para a covariÃƒÂ¡vel usando o pacote R 'emmeans' (Lenth, 2024).",
          "Note. Adjusted statistics (Estimated Marginal Means) are computed controlling for the covariate using the 'emmeans' R package (Lenth, 2024)."
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
      opts      <- state$options
      
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
      
      base_theme <- switch(opts$themePlot, classic = ggplot2::theme_classic(), minimal = ggplot2::theme_minimal(),
                           bw = ggplot2::theme_bw(), light = ggplot2::theme_light(), ggplot2::theme_classic())
      
      custom_theme <- base_theme + ggplot2::theme(
        plot.title      = ggplot2::element_text(size = opts$fontSizeTitle),
        plot.subtitle   = ggplot2::element_text(size = opts$fontSizeSubtitle),
        axis.title      = ggplot2::element_text(size = opts$fontSizeAxisTitle),
        axis.text       = ggplot2::element_text(size = opts$fontSizeAxis),
        axis.text.x     = ggplot2::element_text(size = opts$fontSizeAxis, angle = opts$xAngle, hjust = if(opts$xAngle > 0) 1 else 0.5),
        axis.text.y     = ggplot2::element_text(size = opts$fontSizeAxis), 
        legend.text     = ggplot2::element_text(size = opts$fontSizeLegend),
        legend.title    = ggplot2::element_text(size = opts$fontSizeLegend),
        legend.position = opts$legendPosition,
        axis.line       = ggplot2::element_line(linewidth = opts$axisLineWidth),
        axis.ticks      = ggplot2::element_line(linewidth = opts$axisLineWidth)
      )
      
      error_col <- switch(opts$errorType, sd = 'sd', se = 'se', ci = 'ci_half')
      stats_df$ymin <- stats_df$mean - stats_df[[error_col]]
      stats_df$ymax <- stats_df$mean + stats_df[[error_col]]
      
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
                  }
      )
      
      p <- p + ggplot2::scale_fill_manual(values = colors) + ggplot2::scale_color_manual(values = colors)
      y_expand <- if (opts$removeGap) ggplot2::expansion(mult = c(0, 0.05)) else ggplot2::waiver()
      
      if (opts$customYAxis) {
        y_breaks <- if (opts$yStep > 0) seq(opts$yMin, opts$yMax, by = opts$yStep) else ggplot2::waiver()
        p <- p + ggplot2::scale_y_continuous(expand = y_expand, limits = c(opts$yMin, opts$yMax), breaks = y_breaks)
      } else {
        p <- p + ggplot2::scale_y_continuous(expand = y_expand)
      }
      
      label_x <- if(opts$xLabel != '') opts$xLabel else (if(opts$analysisType == 'longitudinal') "Time" else opts$group[1])
      plot_title <- if(opts$titleText != '') opts$titleText else NULL
      
      p <- p + custom_theme + ggplot2::labs(title = plot_title,
                                            x = label_x, y = if(opts$yLabel != '') opts$yLabel else dep,
                                            fill = plot_data$leg_title[1], color = plot_data$leg_title[1])
      if (isTRUE(opts$exportPlots)) {
        private$.exportVectorPlot(p, dep, opts)
      }
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
    .generateApaHtml = function(table) {
      df <- tryCatch(table$asDF, error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return()
      
      title_text <- if (nzchar(self$options$tableTitle)) self$options$tableTitle else 'Descriptive Statistics by Group'
      note_text <- if (nzchar(self$options$tableNote)) self$options$tableNote else 'Note. M = mean; SD = standard deviation; SE = standard error; CI = confidence interval; N = sample size.'
      
      col_names <- colnames(df)
      header_html <- paste0('<tr>', paste0(sapply(col_names, function(c) paste0('<th style="padding: 6px 12px; font-weight: bold; border-bottom: 1px solid #000; text-align: left;">', c, '</th>')), collapse = ''), '</tr>')
      
      rows_html <- character(nrow(df))
      for (i in seq_len(nrow(df))) {
        row_vals <- df[i, ]
        cells <- sapply(row_vals, function(v) {
          val_str <- if (is.numeric(v)) sprintf('%.2f', v) else as.character(v)
          paste0('<td style="padding: 5px 12px; text-align: left;">', val_str, '</td>')
        })
        rows_html[i] <- paste0('<tr>', paste0(cells, collapse = ''), '</tr>')
      }
      
      html_content <- paste0(
        '<div style="font-family: Times New Roman, serif; font-size: 13px; color: #111; max-width: 900px; margin: 15px 0;">',
        '<p style="font-weight: bold; margin-bottom: 2px;">Table 1</p>',
        '<p style="font-style: italic; margin-top: 0; margin-bottom: 10px;">', title_text, '</p>',
        '<table style="border-collapse: collapse; width: 100%; border-top: 2px solid #000; border-bottom: 2px solid #000;">',
        '<thead>', header_html, '</thead>',
        '<tbody>', paste0(rows_html, collapse = ''), '</tbody>',
        '</table>',
        '<p style="font-size: 11px; margin-top: 8px; font-style: italic;">', note_text, '</p>',
        '</div>'
      )
      self$results$formattedTable$setContent(html_content)
    },
    
    .syncCorrTableColumns = function(corr_tbl, num_vars) {
      get_col_names <- function(tbl) {
        tryCatch(sapply(tbl$columns, function(col) col$name), error = function(e) character(0))
      }
      current_cols <- get_col_names(corr_tbl)
      for (j in seq_along(num_vars)) {
        col_id <- paste0('col_', j)
        if (!col_id %in% current_cols) {
          corr_tbl$addColumn(name = col_id, title = as.character(j), type = 'text')
        }
      }
    },
    
    .runCorrelationMatrix = function(num_vars, data, method) {
      if (!isTRUE(self$options$showCorrMatrix)) return()
      tbl <- self$results$corrMatrixTable
      tbl$deleteRows()
      private$.syncCorrTableColumns(tbl, num_vars)
      
      k <- length(num_vars)
      for (i in seq_len(k)) {
        v_i <- num_vars[i]
        vals_i <- private$.to_numeric_vec(data[[v_i]])
        m <- mean(vals_i, na.rm = TRUE)
        s <- stats::sd(vals_i, na.rm = TRUE)
        row_vals <- list(num = as.character(i), var = v_i, mean = m, sd = s)
        
        for (j in seq_len(k)) {
          col_id <- paste0('col_', j)
          if (j == i) {
            row_vals[[col_id]] <- '-'
          } else if (j > i) {
            row_vals[[col_id]] <- ''
          } else {
            v_j <- num_vars[j]
            vals_j <- private$.to_numeric_vec(data[[v_j]])
            ct <- tryCatch(stats::cor.test(vals_i, vals_j, method = method), error = function(e) NULL)
            if (!is.null(ct)) {
              r_val <- ct$estimate
              p_val <- ct$p.value
              stars <- if (p_val < 0.001) '***' else if (p_val < 0.01) '**' else if (p_val < 0.05) '*' else ''
              row_vals[[col_id]] <- sprintf('%.2f%s', r_val, stars)
            } else {
              row_vals[[col_id]] <- 'NA'
            }
          }
        }
        tbl$addRow(rowKey = i, values = row_vals)
      }
      tbl$setNote('stars_note', 'Note. * p < .05, ** p < .01, *** p < .001. Lower diagonal contains bivariate correlation coefficients. M and SD represent mean and standard deviation.')
    },
    
    .setHeatmapState = function(num_vars, data, method, cluster) {
      if (!isTRUE(self$options$showCorrHeatmap)) return()
      sub_df <- as.data.frame(lapply(data[, num_vars, drop = FALSE], private$.to_numeric_vec))
      self$results$corrHeatmapPlot$setState(list(vars = num_vars, data = sub_df, method = method, cluster = cluster))
    },
    
    .plotCorrHeatmap = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return(FALSE)
      state <- image$state
      num_vars <- state$vars
      sub_df <- state$data
      method <- state$method
      cluster <- state$cluster
      
      cor_mat <- stats::cor(sub_df, use = 'pairwise.complete.obs', method = method)
      cor_mat[is.na(cor_mat)] <- 0
      
      ordered_vars <- num_vars
      if (isTRUE(cluster) && ncol(cor_mat) > 2) {
        dist_mat <- stats::as.dist(1 - abs(cor_mat))
        hc <- tryCatch(stats::hclust(dist_mat, method = 'complete'), error = function(e) NULL)
        if (!is.null(hc)) {
          ordered_vars <- colnames(cor_mat)[hc$order]
        }
      }
      
      df_heat <- expand.grid(Var1 = ordered_vars, Var2 = ordered_vars, stringsAsFactors = FALSE)
      df_heat$value <- apply(df_heat, 1, function(row) cor_mat[row['Var1'], row['Var2']])
      df_heat$Var1 <- factor(df_heat$Var1, levels = ordered_vars)
      df_heat$Var2 <- factor(df_heat$Var2, levels = rev(ordered_vars))
      
      p <- ggplot2::ggplot(df_heat, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_tile(color = 'white', linewidth = 0.5) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf('%.2f', value)), color = ifelse(abs(df_heat$value) > 0.6, 'white', 'black'), size = 3.5) +
        ggplot2::scale_fill_gradient2(low = '#2166AC', mid = '#FFFFFF', high = '#B2182B', midpoint = 0, limits = c(-1, 1), name = 'Correlation') +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10, face = 'bold'),
          axis.text.y = ggplot2::element_text(size = 10, face = 'bold'),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        ) +
        ggplot2::labs(title = paste0('Correlation Heatmap (', tools::toTitleCase(method), if (cluster) ', Clustered' else '', ')'))
      print(p)
      TRUE
    }
    ,
    .exportVectorPlot = function(p, dep, opts) {
      export_dir <- if (!is.null(opts$exportDir) && nzchar(opts$exportDir)) opts$exportDir else getwd()
      if (!dir.exists(export_dir)) {
        tryCatch(dir.create(export_dir, recursive = TRUE), error = function(e) NULL)
      }
      clean_name <- gsub('[^A-Za-z0-9_-]', '_', dep)
      fmt <- if (!is.null(opts$exportFormat)) opts$exportFormat else 'pdf'
      dpi <- if (!is.null(opts$exportDpi) && opts$exportDpi >= 300) opts$exportDpi else 300
      w <- if (!is.null(opts$exportWidth) && opts$exportWidth > 0) opts$exportWidth else 7.0
      h <- if (!is.null(opts$exportHeight) && opts$exportHeight > 0) opts$exportHeight else 5.0
      saved_files <- character()
      if (fmt %in% c('pdf', 'both')) {
        pdf_path <- file.path(export_dir, paste0('BSS_figure_', clean_name, '.pdf'))
        tryCatch({
          ggplot2::ggsave(filename = pdf_path, plot = p, device = 'pdf', width = w, height = h, dpi = dpi)
          saved_files <- c(saved_files, pdf_path)
        }, error = function(e) NULL)
      }
      if (fmt %in% c('svg', 'both')) {
        svg_path <- file.path(export_dir, paste0('BSS_figure_', clean_name, '.svg'))
        tryCatch({
          dev_svg <- if (requireNamespace('svglite', quietly = TRUE)) 'svg' else grDevices::svg; ggplot2::ggsave(filename = svg_path, plot = p, device = dev_svg, width = w, height = h, dpi = dpi)
          saved_files <- c(saved_files, svg_path)
        }, error = function(e) NULL)
      }
      if (length(saved_files) > 0) {
        msg <- paste0(
          '<div style="padding: 8px 12px; background-color: #f1f8e9; border: 1px solid #c8e6c9; border-radius: 4px; margin: 10px 0;">',
          '<p style="color: #2e7d32; font-weight: bold; margin: 0 0 5px 0;">Vector figure(s) exported successfully (', dpi, ' DPI, Publication Quality):</p>',
          '<ul style="margin: 0; padding-left: 20px;">', paste0('<li><code>', saved_files, '</code></li>', collapse = ''), '</ul>',
          '</div>'
        )
        self$results$exportNotice$setContent(msg)
      }
    }
  )
)

