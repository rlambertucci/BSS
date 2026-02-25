missRangerImputationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "missRangerImputationClass",
  inherit = missRangerImputationBase,
  private = list(
    .run = function() {
      
      data <- self$data # Corrigido aqui
      vars_to_impute <- self$options$vars # Corrigido aqui
      maxiter <- self$options$maxiter # Corrigido aqui
      num_trees <- self$options$num_trees # Corrigido aqui
      pmm_k <- self$options$pmm_k # Corrigido aqui
      seed <- self$options$seed # Corrigido aqui
      show_pattern_plot <- self$options$show_pattern_plot # Corrigido aqui
      show_correlation_plot <- self$options$show_correlation_plot # Corrigido aqui
      
      if (is.null(data)) {
        return()
      }
      
      if (is.null(vars_to_impute) || length(vars_to_impute) == 0) {
        # Adicionar uma mensagem para o usuário se nenhuma variável for selecionada
        if (is.null(self$results$text$content) || self$results$text$content == "") { 
          self$results$text$setContent("Please select variables to impute.")
          self$results$text$setVisible(TRUE)
        }
        return()
      }
      
      # Filtrar IdVariable (do seu código original)
      vars_to_impute_filtered <- character()
      for(var_name in vars_to_impute) {
        if (var_name %in% names(data) && !inherits(data[[var_name]], "jmvcore.IdVariable")) {
          vars_to_impute_filtered <- c(vars_to_impute_filtered, var_name)
        }
      }
      
      if(length(vars_to_impute_filtered) == 0){
        if (is.null(self$results$text$content) || self$results$text$content == "") {
          self$results$text$setContent("No suitable variables selected for imputation (ID variables are excluded or selected variables not found).")
          self$results$text$setVisible(TRUE)
        }
        return()
      }
      vars_to_impute <- vars_to_impute_filtered # Usar a lista filtrada
      
      data_to_impute <- data[, vars_to_impute, drop = FALSE]
      
      # Semente (do seu código original)
      if (seed != "" && !is.na(as.numeric(seed))) {
        set.seed(as.numeric(seed))
      }
      
      # Imputação (do seu código original)
      imputed_data_result <- tryCatch({ # Renomeado para imputed_data_result
        res <- missRanger::missRanger(
          data_to_impute,
          maxiter = maxiter,
          num.trees = num_trees,
          pmm.k = pmm_k,
          verbose = 0
        )
        res # Retornar o resultado do missRanger
      }, error = function(e) {
        msg <- paste("Error during imputation:", e$message) # Corrigido aqui
        self$results$text$setContent(msg) # Corrigido aqui
        self$results$text$setVisible(TRUE) # Corrigido aqui
        return(NULL)
      })
      
      if (is.null(imputed_data_result)) {
        return()
      }
      
      # Arredondamento (do seu código original, aplicado a imputed_data_result)
      for (var_name in names(imputed_data_result)) {
        if (is.numeric(imputed_data_result[[var_name]])) {
          imputed_data_result[[var_name]] <- round(imputed_data_result[[var_name]], digits = 3)
        }
      }
      
      # Tabela de resumo (do seu código original, com pequenas adaptações)
      table <- self$results$imputedData # Corrigido aqui
      if (table$rowCount > 0) { # Limpar tabela antes de popular
        table$clear() 
      }
      row_num <- 1
      total_na_original <- 0
      
      for (var_name in vars_to_impute) {
        original_values <- data[[var_name]]
        imputed_values_for_table <- imputed_data_result[[var_name]] # Usar a coluna de imputed_data_result
        na_indices <- which(is.na(original_values))
        total_na_original <- total_na_original + length(na_indices)
        
        for (i in na_indices) {
          if (row_num <= 1000) { # Limitar linhas na tabela de resumo
            table$addRow(rowKey = row_num, values = list(
              variable = var_name,
              original = if (is.na(original_values[i])) "NA" else as.character(original_values[i]), # Seu código original
              imputed = as.character(imputed_values_for_table[i]), 
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
      
      # Mensagem de texto (com pequena adaptação)
      if (total_na_original > 0) {
        self$results$text$setContent(paste0("Imputation completed. ", total_na_original, " NA(s) handled. Imputed values are shown in the summary table (if any NAs were present and table limit not exceeded).")) # Corrigido aqui
      } else {
        self$results$text$setContent("Imputation completed. No missing values were found in the selected variables to impute.") # Corrigido aqui
      }
      self$results$text$setVisible(TRUE) # Corrigido aqui
      
      
      # --- BLOCO PARA OUTPUT DAS COLUNAS IMPUTADAS (COM CORREÇÃO DO MEASURETYPE) ---
      if (self$results$imputedColsOutput$isNotFilled() && !is.null(imputed_data_result)) {
        
        output_keys <- character()
        output_titles <- character()
        output_measure_types <- character()
        output_col_levels <- list() 
        output_col_ordered <- list() 
        
        for (var_name_key in vars_to_impute) { 
          if (var_name_key %in% names(imputed_data_result)) { 
            output_keys <- c(output_keys, var_name_key) 
            output_titles <- c(output_titles, paste0(var_name_key, "_imp")) 
            
            original_column <- self$data[[var_name_key]] 
            
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
              output_col_levels[[var_name_key]] <- levels(original_column)
              output_col_ordered[[var_name_key]] <- is.ordered(original_column)
            } else {
              output_col_levels[[var_name_key]] <- NULL
              output_col_ordered[[var_name_key]] <- FALSE 
            }
          }
        }
        
        if (length(output_keys) > 0) {
          self$results$imputedColsOutput$set(
            keys = output_keys,
            titles = output_titles,
            descriptions = "Imputed variable", 
            measureTypes = output_measure_types
          )
          
          for (key_iter in output_keys) { 
            imputed_vector <- imputed_data_result[[key_iter]] 
            original_column_for_type_check <- self$data[[key_iter]]
            
            if (is.factor(original_column_for_type_check) && !is.null(output_col_levels[[key_iter]])) {
              current_levels <- output_col_levels[[key_iter]]
              current_ordered <- output_col_ordered[[key_iter]]
              
              if (is.character(imputed_vector) || is.numeric(imputed_vector)) {
                imputed_vector <- factor(imputed_vector, 
                                         levels = current_levels, 
                                         ordered = current_ordered)
              } 
              else if (is.factor(imputed_vector)) {
                if (!identical(levels(imputed_vector), current_levels) ||
                    is.ordered(imputed_vector) != current_ordered) {
                  imputed_vector <- factor(as.character(imputed_vector), 
                                           levels = current_levels,
                                           ordered = current_ordered)
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
      # --- FIM DO BLOCO PARA OUTPUT DAS COLUNAS IMPUTADAS ---
      
      # Lógica dos plots (SEU CÓDIGO ORIGINAL)
      if (show_pattern_plot) {
        self$results$patternPlot$setState(data) # Usando 'data' como no seu original
      } else {
        self$results$patternPlot$setVisible(FALSE) # Adicionado para esconder se não selecionado
      }
      
      if (show_correlation_plot) {
        # Para o correlation plot, é importante que 'data' contenha as colunas certas.
        # Se 'vars_to_impute' é o subconjunto desejado, use-o. Seu original usava 'data'.
        # Vou manter 'data' como no seu original, mas se quiser apenas das vars imputadas,
        # seria 'data[, vars_to_impute, drop=FALSE]'
        if (ncol(data[, vars_to_impute, drop=FALSE]) > 1) { # Adicionar verificação de mais de uma coluna
          missing_indicators <- as.data.frame(lapply(data[, vars_to_impute, drop=FALSE], function(x) as.integer(is.na(x))))
          
          # Remover colunas sem NAs (variância zero) antes da correlação
          valid_cols_for_cor <- sapply(missing_indicators, function(x) var(x, na.rm = TRUE) > 0)
          if (sum(valid_cols_for_cor) > 1) {
            cor_matrix <- stats::cor(missing_indicators[, valid_cols_for_cor, drop=FALSE], use = "pairwise.complete.obs")
            if(any(is.finite(cor_matrix))) {
              self$results$correlationPlot$setState(cor_matrix)
              self$results$correlationPlot$setVisible(TRUE) # Adicionado para mostrar
            } else {
              self$results$correlationPlot$setVisible(FALSE)
            }
          } else {
            self$results$correlationPlot$setVisible(FALSE) # Esconder se não houver colunas válidas
          }
        } else {
          self$results$correlationPlot$setVisible(FALSE) # Esconder se menos de 2 colunas
        }
      } else {
        self$results$correlationPlot$setVisible(FALSE) # Adicionado para esconder se não selecionado
      }
    },
    
    # .combinationsPlot (SEU CÓDIGO ORIGINAL)
    .combinationsPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state))
        return(FALSE)
      
      plotData <- image$state
      
      # Adicionar verificação se há NAs para VIM::aggr
      if (sum(sapply(plotData[, self$options$vars, drop=FALSE], anyNA)) == 0 && self$options$show_pattern_plot) {
        # Opcional: Notificar o usuário que não há NAs para plotar
        # self$results$text$addFormat("\nNote: No missing values in selected variables for pattern plot.")
        return(FALSE) 
      }
      
      
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par))
      graphics::par(bg = "transparent")
      
      # Usar apenas as variáveis selecionadas para o plot de padrão, se disponível
      varsForPlot <- self$options$vars
      if (is.null(varsForPlot) || length(varsForPlot) == 0) {
        varsForPlot <- names(plotData) # Fallback para todas as colunas se nenhuma selecionada (improvável aqui)
      }
      
      # Garantir que plotData tenha apenas as colunas em varsForPlot
      plotDataFiltered <- plotData[, intersect(names(plotData), varsForPlot), drop = FALSE]
      
      if (ncol(plotDataFiltered) == 0) return(FALSE)
      
      
      VIM::aggr(plotDataFiltered, # Usar plotDataFiltered
                numbers = TRUE,
                sortVars = TRUE,
                prop = c(TRUE, TRUE),
                gap = 3,
                col = c("skyblue", "red"), # Suas cores originais
                cex.axis = 0.8,
                ylab = c("Proportion of Missings", "Proportion of Combinations")
      )
      
      TRUE
    },
    
    # .correlationPlot (SEU CÓDIGO ORIGINAL)
    .correlationPlot = function(image, ggtheme, theme, ...) {
      if (is.null(image$state) || !is.matrix(image$state) || ncol(image$state) < 1 || !any(is.finite(image$state))) # ncol < 1 para permitir uma única variável (não ideal mas evita erro)
        return(FALSE)
      
      plotData <- image$state # plotData aqui é a matriz de correlação
      
      # Se for matriz 1x1, ggcorr pode não gostar, mas o erro original não era este
      if (nrow(plotData) < 2 || ncol(plotData) < 2) {
        # Talvez exibir uma mensagem ou apenas não plotar para matrizes 1x1
        # jmvcore::यरlog("Correlation plot requires at least 2 variables with missingness variance.")
        return(FALSE)
      }
      
      p <- GGally::ggcorr(data = NULL, cor_matrix = plotData,
                          label = TRUE, label_round = 2,
                          hjust = 1, layout.exp = 5) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
                       axis.text.y = ggplot2::element_text(size = 12),
                       panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))
      
      print(p)
      TRUE
    }
  )
)