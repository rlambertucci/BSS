DDAClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "DDAClass",
    inherit = DDABase,
    private = list(
      
      .getColumnNames = function(table) {
        vapply(table$columns, function(column) column$name, character(1))
      },
      
      .update_help = function() {
        configured <- length(self$options$dep) > 0 && length(self$options$covs) > 0
        if (!is.null(self$results$ddaInfo)) self$results$ddaInfo$setVisible(!configured)
      },

      .init = function() {
        private$.update_help()
        info_html <- paste0(
          '<div>',
          '<ul>',
          '<li><b>Descriptive Discriminant Analysis (DDA)</b> \u2192 A multivariate technique that identifies ',
          'linear combinations of continuous variables (discriminant functions) that best separate ',
          'two or more groups. Used descriptively to understand group separation after a significant ',
          'MANOVA. <i>Huberty, C. J. & Olejnik, S. (2006).</i></li>',
          '<li><b>Structure Coefficients</b> \u2192 Correlations between each covariate and the discriminant ',
          'function scores across the total analysis sample after missing-data exclusion, not pooled within-group correlations. Preferred over standardized coefficients for interpreting the contribution ',
          'of each variable to group separation. <i>Courville, T. & Thompson, B. (2001).</i></li>',
          '<li><b>Pairwise Effect Sizes</b> \u2192 Cohen\'s d and Hedges\' g computed on LD1 scores for each ',
          'pair of groups, quantifying the magnitude of group separation along the first discriminant ',
          'function. <i>Cohen, J. (1988).</i></li>',
          '<li><b>Classification & LOOCV</b> \u2192 Apparent and leave-one-out cross-validated classification accuracy are reported separately. ',
          'Cohen\'s kappa is based on apparent classifications, not LOOCV. <i>Fisher, R. A. (1936).</i></li>',
          '<li><b>Box\'s M Test</b> \u2192 Formal multivariate test for the homogeneity of group covariance matrices. <i>Box, G. E. P. (1949).</i></li>',
          '</ul>',
          '<p>',
          'Note: DDA assumes multivariate normality and homogeneity of covariance matrices across groups. ',
          'At least two groups and one covariate are required.',
          '</p>',
          '</div>'
        )
        self$results$ddaInfo$setContent(info_html)
        
        if (isTRUE(self$options$plot)) {
          width  <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        
        structCoefTable <- self$results$structCoefTable
        structCoefTable$setNote("definition", "Structure coefficients are correlations between the variables and discriminant scores across the total analysis sample after missing-data exclusion, not pooled within-group correlations.")
        if (!"varName" %in% private$.getColumnNames(structCoefTable)) {
          structCoefTable$addColumn(name = "varName", title = "", type = 'text')
        }
        
        propTable <- self$results$prop
        if (!"name" %in% private$.getColumnNames(propTable)) {
          propTable$addColumn(name = "name", title = "", type = 'text', content="Proportion(%)")
          if (propTable$rowCount == 0) { propTable$addRow(rowKey=1, values=list()) }
        }
        
        gcTable <- self$results$gc
        if (!"name" %in% private$.getColumnNames(gcTable)) {
          gcTable$addColumn(name = "name", title = "Groups", type = 'text')
        }
        
        discScoresTable <- self$results$discScoresTable
        if (!"obs" %in% private$.getColumnNames(discScoresTable)) {
          discScoresTable$addColumn(name = "obs", title = "Original Row", type = 'integer')
        }
        
        pairwiseESTable <- self$results$pairwiseESTable
        required_cols <- c("group1", "group2", "cohenD", "hedgesG")
        current_cols_pairwise <- private$.getColumnNames(pairwiseESTable)
        if (!all(required_cols %in% current_cols_pairwise)) {
          pairwiseESTable$addColumn(name="group1", title="Group 1", type='text')
          pairwiseESTable$addColumn(name="group2", title="Group 2", type='text')
          pairwiseESTable$addColumn(name="cohenD", title="Cohen's d", type='number', format='zto')
          pairwiseESTable$addColumn(name="hedgesG", title="Hedges' g", type='number', format='zto')
        }
      },
      
      .run = function() {
        private$.update_help()
        
        if (is.null(self$options$dep) || length(self$options$covs) < 1) {
          return()
        }
        dep  <- self$options$dep
        covs <- self$options$covs
        
        all_analysis_vars <- unique(c(dep, covs))
        all_analysis_vars <- intersect(all_analysis_vars, names(self$data))
        if (length(all_analysis_vars) < 2) return()
        
        data <- self$data[, all_analysis_vars, drop = FALSE]
        data <- jmvcore::naOmit(data)
        if (nrow(data) == 0) {
          jmvcore::reject("No valid data remaining after handling missing values.", code = "no_valid_data")
          return()
        }
        
        for (cov in covs) data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        
        cov_vars <- sapply(data[, covs, drop = FALSE], function(col) stats::var(col, na.rm = TRUE))
        zero_var_covs <- names(cov_vars)[is.na(cov_vars) | cov_vars == 0]
        if (length(zero_var_covs) > 0) {
          jmvcore::reject(
            paste0("Covariate '", zero_var_covs[1], "' has zero variance (constant values)."),
            code = "zero_variance_covariates"
          )
          return()
        }
        
        data[[dep]] <- droplevels(as.factor(data[[dep]]))
        group_levels <- levels(data[[dep]])
        n_groups <- length(group_levels)
        
        if (n_groups < 2) {
          jmvcore::reject("The dependent variable must have at least two groups with valid data.", code = "not_enough_levels_initial")
          return()
        }
        
        formula <- jmvcore::constructFormula(dep, covs)
        formula <- stats::as.formula(formula)
        lda.results <- tryCatch({ MASS::lda(formula, data = data) }, error = function(e) {
          msg <- paste("LDA calculation failed:", e$message); code <- "lda_error"
          if (grepl("collinear", e$message, ignore.case = TRUE)) {
            msg <- paste("LDA calculation failed due to collinearity among covariates."); code <- "lda_collinearity_error"
          } else if (grepl("group means are identical", e$message, ignore.case = TRUE)) {
            msg <- paste("LDA calculation failed: Group means are identical for the selected covariates."); code <- "lda_identical_means_error"
          } else if (grepl("some group is too small", e$message, ignore.case = TRUE)) {
            msg <- paste("LDA calculation failed: At least one group has too few observations relative to the number of covariates."); code <- "lda_small_group_error"
          }
          jmvcore::reject(msg, code=code); return(NULL)
        })
        if (is.null(lda.results)) return()
        
        scores <- NULL; centroids <- NULL; n_lds <- 0
        tryCatch({
          scores <- stats::predict(lda.results)$x
          if (is.vector(scores)) scores <- matrix(scores, ncol = 1)
          colnames(scores) <- paste0("LD", 1:ncol(scores))
          n_lds <- ncol(scores)
          scores_df <- data.frame(Group = data[[dep]], scores)
          centroids <- stats::aggregate(. ~ Group, data = scores_df, FUN = mean)
        }, error = function(e) {
          jmvcore::reject(paste("Error calculating scores/centroids:", e$message), code="score_centroid_error")
        })
        if (is.null(scores) || is.null(centroids)) return()
        
        if (isTRUE(self$options$coef)) {
          table <- self$results$coef
          tryCatch({
            coef_scaling <- lda.results$scaling
            colnames(coef_scaling) <- paste0("LD", 1:ncol(coef_scaling))
            coef_scaling_df <- as.data.frame(coef_scaling)
            variable_names <- rownames(coef_scaling_df)
            dims <- colnames(coef_scaling_df)
            
            current_table_cols <- private$.getColumnNames(table)
            cols_to_remove <- setdiff(current_table_cols, c("name", dims))
            for(col_name in cols_to_remove) { table$getColumn(col_name)$setVisible(FALSE) }
            existing_cols <- private$.getColumnNames(table)
            for (dim_name in dims) {
              if (dim_name %in% existing_cols) table$getColumn(dim_name)$setVisible(TRUE)
              if (!dim_name %in% existing_cols) {
                table$addColumn(name = dim_name, title = dim_name, type = 'number')
              }
            }
            
            table$setState(NULL)
            table$deleteRows()
            if (length(variable_names) > 0) {
              for (var_name in variable_names) {
                row_values <- list()
                for (dim_name in dims) {
                  if (dim_name %in% names(coef_scaling_df)) {
                    row_values[[dim_name]] <- coef_scaling_df[var_name, dim_name]
                  } else {
                    row_values[[dim_name]] <- NA
                  }
                }
                table$addRow(rowKey = var_name, values = row_values)
              }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating Coef table:", e$message), code="coef_table_error")
            table$setState(NULL)
          })
        } else { self$results$coef$setState(NULL) }
        
        if (isTRUE(self$options$prop)) {
          table <- self$results$prop
          tryCatch({
            prop.lda <- lda.results$svd^2 / sum(lda.results$svd^2)
            dims <- paste0("LD", seq_along(prop.lda))
            
            current_cols <- private$.getColumnNames(table)
            cols_to_remove <- setdiff(current_cols, c("name", dims))
            for(col_name in cols_to_remove) { table$getColumn(col_name)$setVisible(FALSE) }
            existing_cols <- private$.getColumnNames(table)
            for (dim_name in dims) {
              if (dim_name %in% existing_cols) table$getColumn(dim_name)$setVisible(TRUE)
              if (!dim_name %in% existing_cols) {
                table$addColumn(name = dim_name, title=dim_name, type = 'number', format="pc")
              }
            }
            
            row_values <- list(name = "Proportion(%)")
            for (i in seq_along(prop.lda)) { row_values[[dims[i]]] <- prop.lda[i] }
            if (table$rowCount == 0) { table$addRow(rowKey=1, values=list()) }
            table$setRow(rowNo = 1, values = row_values)
          }, error = function(e) {
            jmvcore::reject(paste("Error populating Proportion table:", e$message), code="prop_table_error")
            table$setRow(rowNo=1, values=list(name="Proportion(%)"))
          })
        } else { self$results$prop$setRow(rowNo=1, values=list(name="Proportion(%)")) }
        
        if (isTRUE(self$options$gc)) {
          table <- self$results$gc
          tryCatch({
            dims <- colnames(scores)
            group_names <- as.character(centroids$Group)
            
            current_table_cols <- private$.getColumnNames(table)
            cols_to_remove <- setdiff(current_table_cols, c("name", dims))
            for(col_name in cols_to_remove) { table$getColumn(col_name)$setVisible(FALSE) }
            existing_cols <- private$.getColumnNames(table)
            if (!"name" %in% existing_cols) { table$addColumn(name = "name", title = "Groups", type = 'text') }
            for (dim_name in dims) {
              if (dim_name %in% existing_cols) table$getColumn(dim_name)$setVisible(TRUE)
              if (!dim_name %in% existing_cols) {
                table$addColumn(name = dim_name, title = dim_name, type = 'number')
              }
            }
            
            table$setState(NULL)
            table$deleteRows()
            if (nrow(centroids) > 0) {
              for (i in 1:nrow(centroids)) {
                group_name <- group_names[i]
                row_values <- list()
                row_values[["name"]] <- group_name
                for (dim_name in dims) {
                  if (dim_name %in% names(centroids)) {
                    row_values[[dim_name]] <- centroids[i, dim_name]
                  } else {
                    row_values[[dim_name]] <- NA
                  }
                }
                table$addRow(rowKey = group_name, values = row_values)
              }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating GC table:", e$message), code="gc_table_error")
            table$setState(NULL)
          })
        } else { self$results$gc$setState(NULL) }
        
        if (isTRUE(self$options$boxM)) {
          tableBox <- self$results$boxMTable
          tryCatch({
            tableBox$deleteRows()
            p <- length(covs)
            k <- n_groups
            N <- nrow(data)
            group_counts <- table(data[[dep]])
            
            if (any(group_counts <= p)) {
              tableBox$setNote(
                "sample_size_warn",
                paste0("Box's M test requires group sample sizes strictly greater than the number of covariates (n > ", p, " for all groups).")
              )
            } else {
              cov_list <- list()
              det_vec  <- numeric(k)
              n_vec    <- numeric(k)
              valid_det <- TRUE
              
              for (i in seq_along(group_levels)) {
                g <- group_levels[i]
                sub_cov <- stats::cov(data[data[[dep]] == g, covs, drop = FALSE])
                cov_list[[i]] <- sub_cov
                n_vec[i] <- sum(data[[dep]] == g)
                d_val <- det(sub_cov)
                if (is.na(d_val) || d_val <= 0) {
                  valid_det <- FALSE
                  break
                }
                det_vec[i] <- d_val
              }
              
              if (valid_det) {
                pooled_cov <- matrix(0, nrow = p, ncol = p)
                for (i in seq_along(group_levels)) {
                  pooled_cov <- pooled_cov + (n_vec[i] - 1) * cov_list[[i]]
                }
                pooled_cov <- pooled_cov / (N - k)
                det_pooled <- det(pooled_cov)
                
                if (!is.na(det_pooled) && det_pooled > 0) {
                  m_stat <- (N - k) * log(det_pooled) - sum((n_vec - 1) * log(det_vec))
                  c1 <- (sum(1 / (n_vec - 1)) - 1 / (N - k)) * ((2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (k - 1)))
                  chi_stat <- m_stat * (1 - c1)
                  df_box <- as.integer(round(0.5 * p * (p + 1) * (k - 1)))
                  p_val <- stats::pchisq(chi_stat, df = df_box, lower.tail = FALSE)
                  
                  tableBox$addRow(rowKey = 1, values = list(
                    stat = m_stat,
                    chi  = chi_stat,
                    df   = df_box,
                    p    = p_val
                  ))
                  tableBox$setNote(
                    "citation",
                    "Box's M test evaluates the null hypothesis of equality of group covariance matrices. A non-significant result (p > .05) supports homogeneity of covariance matrices."
                  )
                } else {
                  tableBox$setNote("singular_warn", "Pooled covariance matrix is singular; Box's M test cannot be computed.")
                }
              } else {
                tableBox$setNote("singular_warn", "At least one group covariance matrix is singular (determinant <= 0); Box's M test cannot be computed.")
              }
            }
          }, error = function(e) {
            tableBox$setNote("err", paste("Box's M calculation error:", e$message))
          })
        }
        
        if (isTRUE(self$options$classTable)) {
          tableClass <- self$results$classTable
          tryCatch({
            tableClass$deleteRows()
            
            current_cols <- private$.getColumnNames(tableClass)
            for (g in group_levels) {
              col_id <- paste0("pred_", g)
              if (!col_id %in% current_cols) {
                tableClass$addColumn(name = col_id, title = as.character(g), type = 'integer')
              }
            }
            
            pred_app <- stats::predict(lda.results)$class
            tab_app  <- table(Actual = data[[dep]], Predicted = factor(pred_app, levels = group_levels))
            
            lda_cv <- tryCatch({
              MASS::lda(formula, data = data, CV = TRUE)
            }, error = function(e) NULL)
            
            pred_cv <- if (!is.null(lda_cv)) lda_cv$class else NULL
            tab_cv  <- if (!is.null(pred_cv)) table(Actual = data[[dep]], Predicted = factor(pred_cv, levels = group_levels)) else NULL
            
            for (g in group_levels) {
              row_vals <- list(
                method      = "Original (Apparent)",
                actualGroup = as.character(g),
                total       = as.integer(sum(tab_app[g, ])),
                pctCorrect  = if (sum(tab_app[g, ]) > 0) (tab_app[g, g] / sum(tab_app[g, ])) * 100 else 0
              )
              for (pred_g in group_levels) {
                row_vals[[paste0("pred_", pred_g)]] <- as.integer(tab_app[g, pred_g])
              }
              tableClass$addRow(rowKey = paste0("app_", g), values = row_vals)
            }
            
            if (!is.null(tab_cv)) {
              for (g in group_levels) {
                row_vals <- list(
                  method      = "Cross-validated (LOOCV)",
                  actualGroup = as.character(g),
                  total       = as.integer(sum(tab_cv[g, ])),
                  pctCorrect  = if (sum(tab_cv[g, ]) > 0) (tab_cv[g, g] / sum(tab_cv[g, ])) * 100 else 0
                )
                for (pred_g in group_levels) {
                  row_vals[[paste0("pred_", pred_g)]] <- as.integer(tab_cv[g, pred_g])
                }
                tableClass$addRow(rowKey = paste0("cv_", g), values = row_vals)
              }
            }
            
            total_cases <- sum(tab_app)
            acc_app <- (sum(diag(tab_app)) / total_cases) * 100
            
            p_obs <- sum(diag(tab_app)) / total_cases
            p_exp <- sum(rowSums(tab_app) * colSums(tab_app)) / (total_cases^2)
            kappa_app <- if (p_exp < 1) (p_obs - p_exp) / (1 - p_exp) else 1
            
            acc_cv_str <- if (!is.null(tab_cv)) paste0(round((sum(diag(tab_cv)) / total_cases) * 100, 1), "%") else "N/A"
            tableClass$setNote(
              "summary",
              paste0("Classification accuracy: apparent = ", round(acc_app, 1), "%; LOOCV = ", acc_cv_str, ". Apparent Cohen's kappa = ", round(kappa_app, 3), " (not cross-validated). In cross-validation, each case is classified by the functions derived from all cases other than that case (Lachenbruch's holdout method).")
            )
          }, error = function(e) {
            tableClass$setNote("err", paste("Classification table error:", e$message))
          })
        }
        
        if (isTRUE(self$options$plot)) {
          plotData <- data.frame(Group = data[[dep]], scores)
          centroids_plot <- centroids
          image <- self$results$plot
          show_ellipse <- isTRUE(self$options$showEllipse)
          ellipse_lvl  <- as.numeric(self$options$ellipseLevel)
          if (is.na(ellipse_lvl) || ellipse_lvl <= 0 || ellipse_lvl >= 1) ellipse_lvl <- 0.95
          image$setState(list(
            plotData     = plotData,
            centroids    = centroids_plot,
            n_lds        = n_lds,
            ld_colnames  = colnames(scores),
            showEllipse  = show_ellipse,
            ellipseLevel = ellipse_lvl
          ))
        }
        
        if (isTRUE(self$options$structCoef)) {
          table <- self$results$structCoefTable
          tryCatch({
            dims <- colnames(scores)
            
            current_table_cols <- private$.getColumnNames(table)
            cols_to_remove <- setdiff(current_table_cols, c("varName", dims))
            for(col_name in cols_to_remove) { table$getColumn(col_name)$setVisible(FALSE) }
            existing_cols <- private$.getColumnNames(table)
            if (!"varName" %in% existing_cols) { table$addColumn(name = "varName", title = "", type = 'text') }
            for (dim_name in dims) {
              if (dim_name %in% existing_cols) table$getColumn(dim_name)$setVisible(TRUE)
              if (!dim_name %in% existing_cols) {
                table$addColumn(name = dim_name, title = dim_name, type = 'number')
              }
            }
            
            cormat_df <- NULL
            variable_names <- character(0)
            tryCatch({
              cormat <- stats::cor(data[, covs, drop=FALSE], scores)
              cormat <- as.data.frame(cormat)
              colnames(cormat) <- dims
              cormat_df <- cormat
              variable_names <- rownames(cormat_df)
            }, error = function(e) {
              jmvcore::reject(paste("Error calculating struct coef cor:", e$message), code="structcoef_cor_error")
            })
            
            table$setState(NULL)
            table$deleteRows()
            if (!is.null(cormat_df) && length(variable_names) > 0) {
              for (var_name in variable_names) {
                row_values <- list()
                row_values[["varName"]] <- var_name
                for (dim_name in dims) {
                  if (dim_name %in% names(cormat_df)) {
                    row_values[[dim_name]] <- cormat_df[var_name, dim_name]
                  } else {
                    row_values[[dim_name]] <- NA
                  }
                }
                table$addRow(rowKey = var_name, values = row_values)
              }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating StructCoef table:", e$message), code="structcoef_prep_error")
            table$setState(NULL)
          })
        } else { self$results$structCoefTable$setState(NULL) }

        if (self$results$discScoresOutput$isNotFilled()) {
          output <- self$results$discScoresOutput
          output$set(keys = colnames(scores), titles = paste(colnames(scores), "score"), descriptions = rep("Discriminant function score", ncol(scores)), measureTypes = rep("continuous", ncol(scores)))
          output$setRowNums(rownames(data))
          for (dimension in colnames(scores)) output$setValues(key = dimension, values = as.numeric(scores[, dimension]))
        }
        
        if (isTRUE(self$options$discScores)) {
          table <- self$results$discScoresTable
          tryCatch({
            dims <- colnames(scores)
            n_obs <- nrow(scores)
            
            current_table_cols <- private$.getColumnNames(table)
            cols_to_remove <- setdiff(current_table_cols, c("obs", dims))
            for(col_name in cols_to_remove) { table$getColumn(col_name)$setVisible(FALSE) }
            existing_cols <- private$.getColumnNames(table)
            if (!"obs" %in% existing_cols) { table$addColumn(name = "obs", title = "Original Row", type = 'integer') }
            for (dim_name in dims) {
              if (dim_name %in% existing_cols) table$getColumn(dim_name)$setVisible(TRUE)
              if (!dim_name %in% existing_cols) {
                table$addColumn(name = dim_name, title = dim_name, type = 'number')
              }
            }
            
            table$setState(NULL)
            table$deleteRows()
            if (n_obs > 0) {
              for (i in 1:n_obs) {
                row_values <- list(obs = as.integer(rownames(data)[i]))
                for (j in seq_along(dims)) {
                  row_values[[dims[j]]] <- scores[i, j]
                }
                table$addRow(rowKey = i, values = row_values)
              }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating DiscScores table:", e$message), code="discscores_prep_error")
            table$setState(NULL)
          })
        } else { self$results$discScoresTable$setState(NULL) }
        
        if (isTRUE(self$options$pairwiseES)) {
          tableES <- self$results$pairwiseESTable
          
          rows_to_add <- list()
          footnotes <- list()
          
          if (!requireNamespace("effsize", quietly = TRUE)) {
            jmvcore::reject("effsize package required for pairwise effect sizes.", code="effsize_missing")
          } else {
            tryCatch({
              ld1_scores <- scores[, 1]
              group_var <- data[[dep]]
              group_pairs <- utils::combn(group_levels, 2, simplify = FALSE)
              for (pair in group_pairs) {
                group1_name <- pair[1]
                group2_name <- pair[2]
                rowKey <- paste(group1_name, group2_name, sep = " vs ")
                scores_g1 <- ld1_scores[group_var == group1_name]
                scores_g2 <- ld1_scores[group_var == group2_name]
                row_values <- list(group1 = group1_name, group2 = group2_name, cohenD = NA_real_, hedgesG = NA_real_)
                if (length(scores_g1) < 2 || length(scores_g2) < 2) {
                  footnotes[[rowKey]] <- list(col = "cohenD", message = "Insufficient data (n<2)...")
                } else {
                  tryCatch({
                    cd_est <- effsize::cohen.d(scores_g1, scores_g2, pooled = TRUE, paired = FALSE, na.rm = TRUE, hedges.correction = FALSE)$estimate
                    hg_est <- effsize::cohen.d(scores_g1, scores_g2, pooled = TRUE, paired = FALSE, na.rm = TRUE, hedges.correction = TRUE)$estimate
                    row_values$cohenD <- if (is.finite(cd_est)) cd_est else NA_real_
                    row_values$hedgesG <- if (is.finite(hg_est)) hg_est else NA_real_
                  }, error = function(e) {
                    err_msg <- gsub("[\r\n]", " ", e$message)
                    footnotes[[rowKey]] <- list(col = "cohenD", message = paste("Calc error:", err_msg))
                  })
                }
                rows_to_add[[rowKey]] <- row_values
              }
            }, error = function(e) {
              jmvcore::reject(paste("Error preparing pairwise ES data:", e$message), code="pairwise_prep_error")
              rows_to_add <- list()
            })
          }
          
          tryCatch({
            tableES$setState(NULL)
            tableES$deleteRows()
            final_table_cols <- private$.getColumnNames(tableES)
            if (length(rows_to_add) > 0) {
              for (key in names(rows_to_add)) {
                row_values <- rows_to_add[[key]]
                values_to_add <- row_values[names(row_values) %in% final_table_cols]
                tableES$addRow(rowKey = key, values = values_to_add)
              }
            }
            if (length(footnotes) > 0) {
              for (key in names(footnotes)) {
                fn <- footnotes[[key]]
                tableES$addFootnote(rowKey = key, col = fn$col, note = fn$message)
              }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating Pairwise table:", e$message), code="pairwise_populate_error")
            tableES$setState(NULL)
            })
        } else {
          self$results$pairwiseESTable$setState(NULL)
        }
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        state <- image$state
        if (is.null(state) || is.null(state$plotData) || is.null(state$centroids)) return(FALSE)
        plotData     <- state$plotData
        centroids    <- state$centroids
        n_lds        <- state$n_lds
        ld_colnames  <- state$ld_colnames
        show_ellipse <- if (!is.null(state$showEllipse)) state$showEllipse else TRUE
        ellipse_lvl  <- if (!is.null(state$ellipseLevel)) state$ellipseLevel else 0.95
        
        if (nrow(plotData) == 0 || nrow(centroids) == 0 || n_lds < 1 || is.null(ld_colnames) || length(ld_colnames) < n_lds) return(FALSE)
        if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package required for plot.", call. = FALSE)
        ld1_col <- ld_colnames[1]
        centroid_cols <- colnames(centroids)
        plot <- NULL
        
        tryCatch({
          n_plot_groups <- length(unique(plotData$Group))
          if (n_lds == 1) {
            if (!ld1_col %in% colnames(plotData) || !ld1_col %in% centroid_cols) return(FALSE)
            plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[[ld1_col]], y = 0, color = Group)) +
              ggtheme +
              ggplot2::geom_point(alpha = 0.6, size = 3.5, na.rm=TRUE) +
              ggplot2::geom_point(data = centroids, ggplot2::aes(x = .data[[ld1_col]], y = 0, color = Group), size = 6, shape = 17, na.rm=TRUE) +
              ggplot2::labs(title = "Descriptive Discriminant Analysis Plot", x = ld1_col, y = "") +
              ggplot2::theme(axis.text.y=ggplot2::element_blank(), axis.ticks.y=ggplot2::element_blank())
            if (n_plot_groups <= 8) {
              plot <- plot + ggplot2::scale_color_brewer(palette = "Set2")
            }
          } else {
            ld2_col <- ld_colnames[2]
            if (!ld1_col %in% colnames(plotData) || !ld2_col %in% colnames(plotData) || !ld1_col %in% centroid_cols || !ld2_col %in% centroid_cols) return(FALSE)
            plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[[ld1_col]], y = .data[[ld2_col]], color = Group)) +
              ggtheme +
              ggplot2::geom_point(alpha = 0.6, size = 3.5, na.rm=TRUE)
            
            if (isTRUE(show_ellipse)) {
              plot <- plot + tryCatch(
                ggplot2::stat_ellipse(type = "norm", level = ellipse_lvl, na.rm = TRUE),
                error = function(e) NULL
              )
            }
            
            plot <- plot +
              ggplot2::geom_point(data = centroids, ggplot2::aes(x = .data[[ld1_col]], y = .data[[ld2_col]], color = Group), size = 6, shape = 17, na.rm=TRUE) +
              ggplot2::labs(title = "Descriptive Discriminant Analysis Plot", x = ld1_col, y = ld2_col)
            if (n_plot_groups <= 8) {
              plot <- plot + ggplot2::scale_color_brewer(palette = "Set2")
            }
          }
        }, error = function(e) {
          jmvcore::reject(paste("Error generating plot:", e$message), code="plot_error")
          return(FALSE)
        })
        
        if (!is.null(plot)) { print(plot); return(TRUE) } else { return(FALSE) }
      }
    )
  )



