DDAClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "DDAClass",
    inherit = DDABase,
    private = list(
      .init = function() {
        .getColumnNames <- function(table) { tryCatch(sapply(table$columns, function(col) col$name), error = function(e) character(0)) }
        
        if (isTRUE(self$options$plot)) { width <- self$options$width; height <- self$options$height; self$results$plot$setSize(width, height) }
        
        structCoefTable <- self$results$structCoefTable; if (!"varName" %in% .getColumnNames(structCoefTable)) { try(structCoefTable$addColumn(name = "varName", title = "", type = 'text'), silent = TRUE) }
        propTable <- self$results$prop; if (!"name" %in% .getColumnNames(propTable)) { try(propTable$addColumn(name = "name", title = "", type = 'text', content="Proportion(%)"), silent = TRUE); if (propTable$rowCount == 0) { try(propTable$addRow(rowKey=1, values=list()), silent = TRUE) } }
        gcTable <- self$results$gc; if (!"name" %in% .getColumnNames(gcTable)) { try(gcTable$addColumn(name = "name", title = "Groups", type = 'text'), silent = TRUE) }
        discScoresTable <- self$results$discScoresTable; if (!"obs" %in% .getColumnNames(discScoresTable)) { try(discScoresTable$addColumn(name = "obs", title = "Observation", type = 'text'), silent = TRUE) }
        pairwiseESTable <- self$results$pairwiseESTable; required_cols <- c("group1", "group2", "cohenD", "hedgesG"); current_cols_pairwise <- .getColumnNames(pairwiseESTable)
        if (!all(required_cols %in% current_cols_pairwise)) {
          try(pairwiseESTable$addColumn(name="group1", title="Group 1", type='text'), silent=TRUE); try(pairwiseESTable$addColumn(name="group2", title="Group 2", type='text'), silent=TRUE)
          try(pairwiseESTable$addColumn(name="cohenD", title="Cohen's d", type='number', format='zto'), silent=TRUE); try(pairwiseESTable$addColumn(name="hedgesG", title="Hedges' g", type='number', format='zto'), silent=TRUE)
        }
      }, 
      
      .run = function() {
        
        if (is.null(self$options$dep) || length(self$options$covs) < 1) { 
          return()
        }
        dep <- self$options$dep; covs <- self$options$covs; data <- self$data; data <- jmvcore::naOmit(data)
        if (nrow(data) == 0) { jmvcore::reject("No valid data remaining after handling missing values.", code="no_valid_data"); return() }
        for (cov in covs) data[[cov]] <- jmvcore::toNumeric(data[[cov]])
        data[[dep]] <- as.factor(data[[dep]]); group_levels <- levels(data[[dep]]); n_groups <- length(group_levels)
        
        if (n_groups < 2) { jmvcore::reject("The dependent variable must have at least two groups.", code="not_enough_levels_initial"); return() }
        if (n_groups < 3 && (isTRUE(self$options$pairwiseES) || isTRUE(self$options$plot))) { 
          if (isTRUE(self$options$pairwiseES))
            jmvcore::reject("Pairwise effect sizes require the dependent variable to have at least three groups.", code="not_enough_levels_pairwise")
        }
        
        .getColumnNames <- function(table) { tryCatch(sapply(table$columns, function(col) col$name), error = function(e) character(0)) }
        
        formula <- jmvcore::constructFormula(dep, covs); formula <- as.formula(formula)
        lda.results <- tryCatch({ MASS::lda(formula, data = data) }, error = function(e) { 
          msg <- paste("LDA calculation failed:", e$message); code <- "lda_error"
          if (grepl("collinear", e$message, ignore.case = TRUE)) { msg <- paste("LDA calculation failed due to collinearity among covariates."); code <- "lda_collinearity_error"
          } else if (grepl("group means are identical", e$message, ignore.case = TRUE)) { msg <- paste("LDA calculation failed: Group means are identical for the selected covariates."); code <- "lda_identical_means_error"
          } else if (grepl("some group is too small", e$message, ignore.case = TRUE)) { msg <- paste("LDA calculation failed: At least one group has too few observations relative to the number of covariates."); code <- "lda_small_group_error" }
          jmvcore::reject(msg, code=code); return(NULL) 
        })
        if (is.null(lda.results)) return()
        
        scores <- NULL; centroids <- NULL; n_lds <- 0
        tryCatch({
          scores <- predict(lda.results)$x; if (is.vector(scores)) scores <- matrix(scores, ncol = 1)
          colnames(scores) <- paste0("LD", 1:ncol(scores)); n_lds <- ncol(scores) 
          scores_df <- data.frame(Group = data[[dep]], scores); centroids <- aggregate(. ~ Group, data = scores_df, FUN = mean)
        }, error = function(e) { jmvcore::reject(paste("Error calculating scores/centroids:", e$message), code="score_centroid_error") })
        if (is.null(scores) || is.null(centroids)) return()
        
        if (isTRUE(self$options$coef)) {
          table <- self$results$coef
          tryCatch({
            coef_scaling <- lda.results$scaling
            colnames(coef_scaling) <- paste0("LD", 1:ncol(coef_scaling)) 
            coef_scaling_df <- as.data.frame(coef_scaling)
            variable_names <- rownames(coef_scaling_df) 
            dims <- colnames(coef_scaling_df) 
            
            current_table_cols <- .getColumnNames(table)
            cols_to_remove <- setdiff(current_table_cols, dims) 
            for(col_name in cols_to_remove) { if (col_name %in% .getColumnNames(table)) try(table$deleteColumn(name = col_name), silent=TRUE) }
            for (dim_name in dims) { if (!dim_name %in% .getColumnNames(table)) { try(table$addColumn(name = dim_name, title = dim_name, type = 'number'), silent=TRUE) } }
            
            try(table$setState(NULL), silent = TRUE) 
            
            if (length(variable_names) > 0) {
              for (var_name in variable_names) {
                row_values <- list() 
                for (dim_name in dims) {
                  if (dim_name %in% names(coef_scaling_df)) {
                    row_values[[dim_name]] <- coef_scaling_df[var_name, dim_name]
                  } else { row_values[[dim_name]] <- NA }
                }
                try(table$addRow(rowKey = var_name, values = row_values), silent = TRUE)
              }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating Coef table:", e$message), code="coef_table_error")
            try(table$setState(NULL), silent=TRUE) 
          })
        } else { try(self$results$coef$setState(NULL), silent=TRUE) } 
        
        if (isTRUE(self$options$prop)) { 
          table <- self$results$prop
          tryCatch({
            prop.lda <- lda.results$svd^2 / sum(lda.results$svd^2)
            dims <- paste0("LD", seq_along(prop.lda))
            
            current_cols <- .getColumnNames(table)
            cols_to_remove <- setdiff(current_cols, c("name", dims))
            for(col_name in cols_to_remove) { if (col_name %in% .getColumnNames(table)) try(table$deleteColumn(name = col_name), silent=TRUE) }
            for (dim_name in dims) { if (!dim_name %in% .getColumnNames(table)) { try(table$addColumn(name = dim_name, title=dim_name, type = 'number', format="pc"), silent=TRUE) } }
            
            row_values <- list(name = "Proportion(%)") 
            for (i in seq_along(prop.lda)) { row_values[[dims[i]]] <- prop.lda[i] }
            if (table$rowCount == 0) { try(table$addRow(rowKey=1, values=list()), silent = TRUE) }
            try(table$setRow(rowNo = 1, values = row_values), silent=TRUE) 
          }, error = function(e) {
            jmvcore::reject(paste("Error populating Proportion table:", e$message), code="prop_table_error")
            try(table$setRow(rowNo=1, values=list(name="Proportion(%)")), silent=TRUE) 
          })
        } else { try(self$results$prop$setRow(rowNo=1, values=list(name="Proportion(%)")), silent=TRUE) }
        
        if (isTRUE(self$options$gc)) { 
          table <- self$results$gc
          tryCatch({
            dims <- colnames(scores) 
            group_names <- as.character(centroids$Group)
            
            current_table_cols <- .getColumnNames(table)
            cols_to_remove <- setdiff(current_table_cols, c("name", dims))
            for(col_name in cols_to_remove) { if (col_name %in% .getColumnNames(table)) try(table$deleteColumn(name = col_name), silent=TRUE) }
            if (!"name" %in% .getColumnNames(table)) { try(table$addColumn(name = "name", title = "Groups", type = 'text'), silent = TRUE) }
            for (dim_name in dims) { if (!dim_name %in% .getColumnNames(table)) { try(table$addColumn(name = dim_name, title = dim_name, type = 'number'), silent=TRUE) } }
            
            try(table$setState(NULL), silent = TRUE) 
            
            if (nrow(centroids) > 0) {
              for (i in 1:nrow(centroids)) {
                group_name <- group_names[i]
                row_values <- list()
                row_values[["name"]] <- group_name 
                for (dim_name in dims) {
                  if (dim_name %in% names(centroids)) {
                    row_values[[dim_name]] <- centroids[i, dim_name]
                  } else { row_values[[dim_name]] <- NA }
                }
                try(table$addRow(rowKey = group_name, values = row_values), silent = TRUE)
              }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating GC table:", e$message), code="gc_table_error")
            try(table$setState(NULL), silent=TRUE) 
          })
        } else { try(self$results$gc$setState(NULL), silent=TRUE) }
        
        if (isTRUE(self$options$plot)) { 
          plotData <- data.frame(Group = data[[dep]], scores) 
          centroids_plot <- centroids
          image <- self$results$plot
          image$setState(list(plotData = plotData, centroids = centroids_plot, n_lds = n_lds, ld_colnames = colnames(scores))) 
        }
        
        if (isTRUE(self$options$structCoef)) {
          table <- self$results$structCoefTable
          tryCatch({
            dims <- colnames(scores)
            
            current_table_cols <- .getColumnNames(table)
            cols_to_remove <- setdiff(current_table_cols, c("varName", dims))
            for(col_name in cols_to_remove) { if (col_name %in% .getColumnNames(table)) try(table$deleteColumn(name = col_name), silent=TRUE) }
            if (!"varName" %in% .getColumnNames(table)) { try(table$addColumn(name = "varName", title = "", type = 'text'), silent = TRUE) }
            for (dim_name in dims) { if (!dim_name %in% .getColumnNames(table)) { try(table$addColumn(name = dim_name, title = dim_name, type = 'number'), silent=TRUE) } }
            
            cormat_df <- NULL
            variable_names <- character(0)
            tryCatch({
              cormat <- cor(data[, covs, drop=FALSE], scores); cormat <- as.data.frame(cormat)
              colnames(cormat) <- dims; cormat_df <- cormat; 
              variable_names <- rownames(cormat_df)
            }, error = function(e) { jmvcore::reject(paste("Error calculating struct coef cor:", e$message), code="structcoef_cor_error") })
            
            try(table$setState(NULL), silent = TRUE) 
            
            if (!is.null(cormat_df) && length(variable_names) > 0) {
              for (var_name in variable_names) {
                row_values <- list()
                row_values[["varName"]] <- var_name 
                for (dim_name in dims) {
                  if (dim_name %in% names(cormat_df)) {
                    row_values[[dim_name]] <- cormat_df[var_name, dim_name]
                  } else { row_values[[dim_name]] <- NA }
                }
                try(table$addRow(rowKey = var_name, values = row_values), silent = TRUE)
              }
            }
          }, error = function(e) { 
            jmvcore::reject(paste("Error populating StructCoef table:", e$message), code="structcoef_prep_error")
            try(table$setState(NULL), silent=TRUE)
          })
        } else { try(self$results$structCoefTable$setState(NULL), silent=TRUE) }
        
        if (isTRUE(self$options$discScores)) {
          table <- self$results$discScoresTable
          tryCatch({
            dims <- colnames(scores)
            n_obs <- nrow(scores)
            
            current_table_cols <- .getColumnNames(table)
            cols_to_remove <- setdiff(current_table_cols, c("obs", dims))
            for(col_name in cols_to_remove) { if (col_name %in% .getColumnNames(table)) try(table$deleteColumn(name = col_name), silent=TRUE) }
            if (!"obs" %in% .getColumnNames(table)) { try(table$addColumn(name = "obs", title = "Observation", type = 'text'), silent = TRUE) }
            for (dim_name in dims) { if (!dim_name %in% .getColumnNames(table)) { try(table$addColumn(name = dim_name, title = dim_name, type = 'number'), silent=TRUE) } }
            
            try(table$setState(NULL), silent = TRUE) 
            
            if (n_obs > 0) {
              scores_df_loop <- as.data.frame(scores) 
              for (i in 1:n_obs) {
                row_values <- list()
                row_values[["obs"]] <- i 
                for (j in 1:length(dims)) {
                  dim_name <- dims[j]
                  if (dim_name %in% names(scores_df_loop)) {
                    row_values[[dim_name]] <- scores_df_loop[i, j]
                  } else { row_values[[dim_name]] <- NA }
                }
                try(table$addRow(rowKey = i, values = row_values), silent = TRUE)
              }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating DiscScores table:", e$message), code="discscores_prep_error")
            try(table$setState(NULL), silent=TRUE)
          })
        } else { try(self$results$discScoresTable$setState(NULL), silent=TRUE) }
        
        if (isTRUE(self$options$pairwiseES)) {
          tableES <- self$results$pairwiseESTable 
          try(tableES$clearFootnotes(), silent = TRUE) 
          
          rows_to_add <- list() 
          footnotes <- list()      
          
          if (n_groups < 3) { 
          } else if (!requireNamespace("effsize", quietly = TRUE)) { 
            jmvcore::reject("effsize package required for pairwise effect sizes.", code="effsize_missing")
          } else {
            tryCatch({
              ld1_scores <- scores[, 1]; group_var <- data[[dep]]; group_pairs <- utils::combn(group_levels, 2, simplify = FALSE) 
              for (pair in group_pairs) {
                group1_name <- pair[1]; group2_name <- pair[2]; rowKey <- paste(group1_name, group2_name, sep = " vs ") 
                scores_g1 <- ld1_scores[group_var == group1_name]; scores_g2 <- ld1_scores[group_var == group2_name]
                row_values <- list(group1 = group1_name, group2 = group2_name, cohenD = NA_real_, hedgesG = NA_real_) 
                if(length(scores_g1) < 2 || length(scores_g2) < 2) { footnotes[[rowKey]] <- list(col = "cohenD", message = "Insufficient data (n<2)...")
                } else { tryCatch({ row_values$cohenD <- effsize::cohen.d(scores_g1, scores_g2, pooled = TRUE, paired = FALSE, na.rm = TRUE, hedges.correction = FALSE)$estimate 
                row_values$hedgesG <- effsize::cohen.d(scores_g1, scores_g2, pooled = TRUE, paired = FALSE, na.rm = TRUE, hedges.correction = TRUE)$estimate 
                }, error = function(e) { err_msg <- gsub("[\r\n]", " ", e$message); footnotes[[rowKey]] <- list(col = "cohenD", message = paste("Calc error:", err_msg)) }) }
                rows_to_add[[rowKey]] <- row_values 
              } 
            }, error = function(e) {
              jmvcore::reject(paste("Error preparing pairwise ES data:", e$message), code="pairwise_prep_error")
              rows_to_add <- list() 
            })
          } 
          
          tryCatch({
            try(tableES$setState(NULL), silent = TRUE) 
            final_table_cols <- .getColumnNames(tableES) 
            if (length(rows_to_add) > 0) {
              for (key in names(rows_to_add)) {
                row_values <- rows_to_add[[key]]
                values_to_add <- row_values[names(row_values) %in% final_table_cols]
                try(tableES$addRow(rowKey = key, values = values_to_add), silent = TRUE)
              }
            }
            if (length(footnotes) > 0) {
              for (key in names(footnotes)) { fn <- footnotes[[key]]; try(tableES$addFootnote(rowKey = key, col = fn$col, message = fn$message), silent = TRUE) }
            }
          }, error = function(e) {
            jmvcore::reject(paste("Error populating Pairwise table:", e$message), code="pairwise_populate_error") 
            try(tableES$setState(NULL), silent=TRUE); try(tableES$clearFootnotes(), silent = TRUE)
          })
        } else { 
          try(self$results$pairwiseESTable$setState(NULL), silent=TRUE) 
          try(self$results$pairwiseESTable$clearFootnotes(), silent = TRUE) 
        }
        
      }, 
      
      .plot = function(image, ggtheme, theme, ...) {
        state <- image$state; if (is.null(state) || is.null(state$plotData) || is.null(state$centroids)) return(FALSE)
        plotData <- state$plotData; centroids <- state$centroids; n_lds <- state$n_lds; ld_colnames <- state$ld_colnames
        if (nrow(plotData) == 0 || nrow(centroids) == 0 || n_lds < 1 || is.null(ld_colnames) || length(ld_colnames) < n_lds) return(FALSE)
        if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package required for plot.", call. = FALSE)
        library(ggplot2); ld1_col <- ld_colnames[1]; centroid_cols <- colnames(centroids); plot <- NULL 
        tryCatch({
          if (n_lds == 1) {
            if (!ld1_col %in% colnames(plotData) || !ld1_col %in% centroid_cols) return(FALSE)
            plot <- ggplot(plotData, aes(x = .data[[ld1_col]], y = 0, color = Group)) + ggtheme + geom_point(alpha = 0.6, size = 3.5, na.rm=TRUE) + geom_point(data = centroids, aes(x = .data[[ld1_col]], y = 0, color = Group), size = 6, shape = 17, na.rm=TRUE) + labs(title = "Descriptive Discriminant Analysis Plot", x = ld1_col, y = "") + scale_color_brewer(palette = "Set2") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) 
          } else {
            ld2_col <- ld_colnames[2]
            if (!ld1_col %in% colnames(plotData) || !ld2_col %in% colnames(plotData) || !ld1_col %in% centroid_cols || !ld2_col %in% centroid_cols) return(FALSE)
            plot <- ggplot(plotData, aes(x = .data[[ld1_col]], y = .data[[ld2_col]], color = Group)) + ggtheme + geom_point(alpha = 0.6, size = 3.5, na.rm=TRUE) + tryCatch(stat_ellipse(type = "norm", level = 0.95, na.rm = TRUE), error = function(e) NULL) + geom_point(data = centroids, aes(x = .data[[ld1_col]], y = .data[[ld2_col]], color = Group), size = 6, shape = 17, na.rm=TRUE) + labs(title = "Descriptive Discriminant Analysis Plot", x = ld1_col, y = ld2_col) + scale_color_brewer(palette = "Set2")
          }
        }, error = function(e) { jmvcore::reject(paste("Error generating plot:", e$message), code="plot_error"); return(FALSE) }) 
        if (!is.null(plot)) { print(plot); return(TRUE) } else { return(FALSE) }
      } 
    ) 
  ) 