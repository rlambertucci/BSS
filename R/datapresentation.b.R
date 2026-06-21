DataPresentationClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
  "DataPresentationClass",
  inherit = DataPresentationBase,
  private = list(
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
                  dplyr::group_by(dplyr::across(all_of(groups_vars))) %>%
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
          for (v in group_long) data_clean[[v]] <- as.factor(data_clean[[v]])
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
          for (v in group_long) data_clean[[v]] <- as.factor(data_clean[[v]])
          
          if (length(group_long) > 0) {
            formula_str <- paste0("cbind(", paste(rm_vars, collapse = ", "), ") ~ ", paste(group_long, collapse = " * "), " + ", cov_var)
          } else {
            formula_str <- paste0("cbind(", paste(rm_vars, collapse = ", "), ") ~ ", cov_var)
          }
          
          fit <- tryCatch({
            stats::lm(as.formula(formula_str), data = data_clean)
          }, error = function(e) NULL)
          
          if (!is.null(fit)) {
            if (length(group_long) > 0) {
              specs_formula <- as.formula(paste("~ multivariate_response |", paste(group_long, collapse = " + ")))
            } else {
              specs_formula <- ~ multivariate_response
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
                emm_df$variable <- factor(emm_df$multivariate_response, levels = rm_vars)
              }
              
              raw_summary <- data_clean %>%
                tidyr::pivot_longer(cols = all_of(rm_vars), names_to = "variable", values_to = "value") %>%
                dplyr::mutate(variable = factor(variable, levels = rm_vars)) %>%
                dplyr::group_by(dplyr::across(all_of(c("variable", group_long)))) %>%
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
                    dplyr::group_by(dplyr::across(all_of(groups_vars))) %>%
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
            for (v in group_long) data_clean[[v]] <- as.factor(data_clean[[v]])
            
            if (length(group_long) > 0) {
              formula_str <- paste0("cbind(", paste(rm_vars, collapse = ", "), ") ~ ", paste(group_long, collapse = " * "), " + ", cov_var)
            } else {
              formula_str <- paste0("cbind(", paste(rm_vars, collapse = ", "), ") ~ ", cov_var)
            }
            
            fit <- tryCatch({
              stats::lm(as.formula(formula_str), data = data_clean)
            }, error = function(e) NULL)
            
            if (!is.null(fit)) {
              if (length(group_long) > 0) {
                specs_formula <- as.formula(paste("~ multivariate_response |", paste(group_long, collapse = " + ")))
              } else {
                specs_formula <- ~ multivariate_response
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
                  emm_df$variable <- factor(emm_df$multivariate_response, levels = rm_vars)
                }
                
                raw_summary <- data_clean %>%
                  tidyr::pivot_longer(cols = all_of(rm_vars), names_to = "variable", values_to = "value") %>%
                  dplyr::mutate(variable = factor(variable, levels = rm_vars)) %>%
                  dplyr::group_by(dplyr::across(all_of(c("variable", group_long)))) %>%
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
                  merged_df <- merged_df[order(merged_df$variable, merged_df[[group_long[1]]]), ]
                } else {
                  merged_df <- merged_df[order(merged_df$variable), ]
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
                tidyr::pivot_longer(cols = all_of(deps), names_to = "variable", values_to = "value") %>%
                dplyr::group_by(dplyr::across(all_of(grouping_cols))) %>%
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
                tidyr::pivot_longer(cols = all_of(rm_vars), names_to = "variable", values_to = "value") %>%
                dplyr::mutate(variable = factor(variable, levels = rm_vars)) %>%
                dplyr::group_by(dplyr::across(all_of(grouping_cols))) %>%
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
    },
    
    .formatTableByNorm = function(table) {
      format <- self$options$tableFormat
      title <- self$options$tableTitle
      note <- self$options$tableNote
      analysisType <- self$options$analysisType
      col_titles <- switch(
        format,
        'abnt' = list(
          variable = if (analysisType == 'longitudinal') 'Tempo' else 'Variável',
          n = 'N', mean = 'Média', sd = 'DP', se = 'EP', 
          ci_lower = 'IC Inferior', ci_upper = 'IC Superior', 
          median = 'Mediana', min = 'Mín', max = 'Máx'
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
          'abnt' = 'Tabela 1. Estatísticas descritivas por grupo',
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
          'abnt' = 'Média = média; DP = desvio padrão; EP = erro padrão; IC = intervalo de confiança; N = tamanho da amostra.',
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
        'abnt' = paste0('IC = ', self$options$ciWidth, '% Intervalo de Confiança.'),
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
          'abnt' = "Nota. Estatísticas ajustadas (Médias Marginais Estimadas) são calculadas controlando para a covariável usando o pacote R 'emmeans' (Lenth, 2024).",
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
      }
      pals <- list(
        set1 = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
        set2 = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"),
        set3 = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5"),
        pastel1 = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC"),
        pastel2 = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC"),
        dark2 = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
        viridis = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
        plasma = c("#0D0887", "#6A00A8", "#B12A90", "#E16462", "#FCA636", "#F0F921")
      )
      base <- pals[[palette]]; if (is.null(base)) base <- pals$set1
      if (n <= length(base)) return(base[1:n])
      return(grDevices::colorRampPalette(base)(n))
    }
  )
)