missRangerImputationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "missRangerImputationClass",
  inherit = missRangerImputationBase,
  private = list(
    
    .init = function() {
      info_html <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.5;">',
        '<ul>',
        '<li><b>MissRanger</b> &rarr; A fast imputation algorithm based on <b>Random Forests</b>, ',
        'combining the iterative imputation framework of MICE with the predictive power of ',
        'Random Forests. Each missing value is imputed using a forest trained on the observed data. ',
        '<i>Mayer, M. (2019).</i></li>',
        '<li><b>Predictive Mean Matching (PMM)</b> &rarr; After forest prediction, PMM selects the ',
        'final imputed value from the k nearest observed values, ensuring imputed values are always ',
        'plausible and within the observed range. Set PMM donors to 0 to disable. ',
        '<i>Little, R. J. A. (1988).</i></li>',
        '</ul>',
        '<p style="font-style: italic; padding-top: 10px;">',
        'Note: If no Predictor Variables are selected, all variables in the dataset will be used ',
        'as predictors. For reproducible results, set a numeric seed value.',
        '</p>',
        '</div>'
      )
      self$results$imputationInfo$setContent(info_html)
    },
    
    .run = function() {
      
      data           <- self$data
      vars_to_impute <- self$options$vars
      predictor_vars <- self$options$predictor_vars
      maxiter        <- self$options$maxiter
      num_trees      <- self$options$num_trees
      pmm_k          <- self$options$pmm_k
      seed           <- self$options$seed
      show_pattern_plot     <- self$options$show_pattern_plot
      show_correlation_plot <- self$options$show_correlation_plot
      
      if (is.null(data)) {
        return()
      }
      
      if (is.null(vars_to_impute) || length(vars_to_impute) == 0) {
        if (is.null(self$results$text$content) || self$results$text$content == "") {
          self$results$text$setContent("Please select variables to impute.")
          self$results$text$setVisible(TRUE)
        }
        return()
      }
      
      # Filtrar IdVariable
      vars_to_impute_filtered <- character()
      for (var_name in vars_to_impute) {
        if (var_name %in% names(data) && !inherits(data[[var_name]], "jmvcore.IdVariable")) {
          vars_to_impute_filtered <- c(vars_to_impute_filtered, var_name)
        }
      }
      
      if (length(vars_to_impute_filtered) == 0) {
        if (is.null(self$results$text$content) || self$results$text$content == "") {
          self$results$text$setContent("No suitable variables selected for imputation (ID variables are excluded or selected variables not found).")
          self$results$text$setVisible(TRUE)
        }
        return()
      }
      vars_to_impute <- vars_to_impute_filtered
      
      # --- TABELA DE MISSINGNESS PRÉ-IMPUTAÇÃO ---
      miss_table <- self$results$missingnessTable
      for (var_name in vars_to_impute) {
        original_values <- data[[var_name]]
        n_total   <- length(original_values)
        n_missing <- sum(is.na(original_values))
        n_valid   <- n_total - n_missing
        pct_missing <- round((n_missing / n_total) * 100, 1)
        
        miss_table$addRow(
          rowKey = var_name,
          values = list(
            variable    = var_name,
            n_missing   = n_missing,
            pct_missing = pct_missing,
            n_valid     = n_valid
          )
        )
      }
      # --- FIM DA TABELA DE MISSINGNESS ---
      
      # --- CONSTRUÇÃO DO DATASET PARA IMPUTAÇÃO ---
      # Se predictor_vars estiver vazio, usa todas as variáveis do dataset
      # Se predictor_vars estiver preenchido, combina vars_to_impute + predictor_vars
      if (is.null(predictor_vars) || length(predictor_vars) == 0) {
        data_to_impute <- data
        vars_formula   <- vars_to_impute
      } else {
        all_vars       <- unique(c(vars_to_impute, predictor_vars))
        data_to_impute <- data[, all_vars, drop = FALSE]
        vars_formula   <- vars_to_impute
      }
      
      # Semente
      if (seed != "" && !is.na(suppressWarnings(as.numeric(seed)))) {
        set.seed(as.numeric(seed))
      }
      
      # Imputação
      imputed_data_result <- tryCatch({
        # Envolve nomes de variáveis com backticks para suportar espaços e caracteres especiais
        vars_formula_bt <- paste0("`", vars_formula, "`")
        formula_str <- paste(paste(vars_formula_bt, collapse = " + "), "~ .")
        res <- missRanger::missRanger(
          data_to_impute,
          formula   = as.formula(formula_str),
          maxiter   = maxiter,
          num.trees = num_trees,
          pmm.k     = pmm_k,
          verbose   = 0
        )
        res
      }, error = function(e) {
        msg <- paste("Error during imputation:", e$message)
        self$results$text$setContent(msg)
        self$results$text$setVisible(TRUE)
        return(NULL)
      })
      
      if (is.null(imputed_data_result)) {
        return()
      }
      
      # Arredondamento
      for (var_name in names(imputed_data_result)) {
        if (is.numeric(imputed_data_result[[var_name]])) {
          imputed_data_result[[var_name]] <- round(imputed_data_result[[var_name]], digits = 3)
        }
      }
      
      # --- TABELA DE VALORES IMPUTADOS ---
      table   <- self$results$imputedData
      if (table$rowCount > 0) {
        table$clear()
      }
      row_num         <- 1
      total_na_original <- 0
      
      for (var_name in vars_to_impute) {
        original_values        <- data[[var_name]]
        imputed_values_for_table <- imputed_data_result[[var_name]]
        na_indices             <- which(is.na(original_values))
        total_na_original      <- total_na_original + length(na_indices)
        
        for (i in na_indices) {
          if (row_num <= 1000) {
            table$addRow(rowKey = row_num, values = list(
              variable  = var_name,
              original  = if (is.na(original_values[i])) "NA" else as.character(original_values[i]),
              imputed   = as.character(imputed_values_for_table[i]),
              rowNumber = i
            ))
            row_num <- row_num + 1
          } else if (row_num == 1001) {
            table$addRow(rowKey = row_num, values = list(
              variable = "...", original = "...", imputed = "Table truncated (max 1000 rows)", rowNumber = NA_integer_
            ))
            row_num <- row_num + 1
          }
        }
      }
      
      # Mensagem de texto
      if (total_na_original > 0) {
        self$results$text$setContent(paste0("Imputation completed. ", total_na_original, " NA(s) handled."))
      } else {
        self$results$text$setContent("Imputation completed. No missing values were found in the selected variables.")
      }
      self$results$text$setVisible(TRUE)
      
      # --- BLOCO DE OUTPUT PARA A PLANILHA ---
      if (self$results$imputedColsOutput$isNotFilled() && !is.null(imputed_data_result)) {
        
        output_keys          <- character()
        output_titles        <- character()
        output_measure_types <- character()
        output_col_levels    <- list()
        output_col_ordered   <- list()
        
        for (var_name_key in vars_to_impute) {
          if (var_name_key %in% names(imputed_data_result)) {
            output_keys   <- c(output_keys,   var_name_key)
            output_titles <- c(output_titles, paste0(var_name_key, "_imp"))
            
            original_column    <- self$data[[var_name_key]]
            current_measure_type <- NULL
            
            if (inherits(original_column, "jmvcore.Variable")) {
              current_measure_type <- original_column$measureType
            }
            
            if (is.null(current_measure_type)) {
              if (is.numeric(original_column)) current_measure_type <- "continuous"
              else if (is.factor(original_column)) {
                if (is.ordered(original_column)) current_measure_type <- "ordinal"
                else current_measure_type <- "nominal"
              } else if (is.character(original_column)) current_measure_type <- "nominal"
              else {
                if (is.numeric(imputed_data_result[[var_name_key]])) current_measure_type <- "continuous"
                else current_measure_type <- "nominal"
              }
            }
            output_measure_types <- c(output_measure_types, current_measure_type)
            
            if (is.factor(original_column)) {
              output_col_levels[[var_name_key]]  <- levels(original_column)
              output_col_ordered[[var_name_key]] <- is.ordered(original_column)
            } else {
              output_col_levels[[var_name_key]]  <- NULL
              output_col_ordered[[var_name_key]] <- FALSE
            }
          }
        }
        
        if (length(output_keys) > 0) {
          self$results$imputedColsOutput$set(
            keys         = output_keys,
            titles       = output_titles,
            descriptions = "Imputed variable",
            measureTypes = output_measure_types
          )
          
          for (key_iter in output_keys) {
            imputed_vector              <- imputed_data_result[[key_iter]]
            original_column_for_type_check <- self$data[[key_iter]]
            
            if (is.factor(original_column_for_type_check) && !is.null(output_col_levels[[key_iter]])) {
              current_levels  <- output_col_levels[[key_iter]]
              current_ordered <- output_col_ordered[[key_iter]]
              
              if (is.character(imputed_vector) || is.numeric(imputed_vector)) {
                imputed_vector <- factor(imputed_vector, levels = current_levels, ordered = current_ordered)
              } else if (is.factor(imputed_vector)) {
                if (!identical(levels(imputed_vector), current_levels) ||
                    is.ordered(imputed_vector) != current_ordered) {
                  imputed_vector <- factor(as.character(imputed_vector), levels = current_levels, ordered = current_ordered)
                }
              }
            }
            self$results$imputedColsOutput$setValues(key = key_iter, values = imputed_vector)
          }
          
          if (nrow(self$data) > 0) {
            self$results$imputedColsOutput$setRowNums(1:nrow(self$data))
          }
        }
      }
      # --- FIM DO BLOCO DE OUTPUT ---
      
      # Plots
      if (show_pattern_plot) {
        self$results$patternPlot$setState(data)
      } else {
        self$results$patternPlot$setVisible(FALSE)
      }
      
      if (show_correlation_plot) {
        if (ncol(data[, vars_to_impute, drop = FALSE]) > 1) {
          missing_indicators <- as.data.frame(lapply(data[, vars_to_impute, drop = FALSE], function(x) as.integer(is.na(x))))
          valid_cols_for_cor <- sapply(missing_indicators, function(x) var(x, na.rm = TRUE) > 0)
          if (sum(valid_cols_for_cor) > 1) {
            cor_matrix <- stats::cor(missing_indicators[, valid_cols_for_cor, drop = FALSE], use = "pairwise.complete.obs")
            if (any(is.finite(cor_matrix))) {
              self$results$correlationPlot$setState(cor_matrix)
              self$results$correlationPlot$setVisible(TRUE)
            } else {
              self$results$correlationPlot$setVisible(FALSE)
            }
          } else {
            self$results$correlationPlot$setVisible(FALSE)
          }
        } else {
          self$results$correlationPlot$setVisible(FALSE)
        }
      } else {
        self$results$correlationPlot$setVisible(FALSE)
      }
    },
    
    .combinationsPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      plotData <- image$state
      
      if (sum(sapply(plotData[, self$options$vars, drop = FALSE], anyNA)) == 0 && self$options$show_pattern_plot) {
        return(FALSE)
      }
      
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par))
      graphics::par(bg = "transparent")
      
      varsForPlot    <- self$options$vars
      if (is.null(varsForPlot) || length(varsForPlot) == 0) {
        varsForPlot <- names(plotData)
      }
      
      plotDataFiltered <- plotData[, intersect(names(plotData), varsForPlot), drop = FALSE]
      if (ncol(plotDataFiltered) == 0) return(FALSE)
      
      VIM::aggr(plotDataFiltered,
                numbers  = TRUE,
                sortVars = TRUE,
                prop     = c(TRUE, TRUE),
                gap      = 3,
                col      = c("skyblue", "red"),
                cex.axis = 0.8,
                ylab     = c("Proportion of Missings", "Proportion of Combinations")
      )
      TRUE
    },
    
    .correlationPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state) || !is.matrix(image$state) || ncol(image$state) < 1 || !any(is.finite(image$state)))
        return(FALSE)
      
      plotData <- image$state
      
      if (nrow(plotData) < 2 || ncol(plotData) < 2) {
        return(FALSE)
      }
      
      p <- GGally::ggcorr(data = NULL, cor_matrix = plotData,
                          label = TRUE, label_round = 2,
                          hjust = 1, layout.exp = 5) +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
          axis.text.y  = ggplot2::element_text(size = 12),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      print(p)
      TRUE
    }
  )
)