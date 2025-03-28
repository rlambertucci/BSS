finiteSampleSizeClass <- R6::R6Class(
  "finiteSampleSizeClass",
  inherit = finiteSampleSizeBase,
  private = list(
    .run = function() {
      if (is.null(self$options$method))
        return()
      
      table <- self$results$sampleSizeTable
      results_list <- list()
      
      N <- self$options$population_size
      confidence_level <- self$options$confidence_level / 100
      margin_error <- self$options$margin_error / 100
      p <- self$options$population_proportion
      correction <- self$options$correction
      
      if (N <= 0 || confidence_level <= 0 || margin_error <= 0 || p < 0 || p > 1) {
        stop("Parâmetros de entrada inválidos. Verifique os valores fornecidos.")
      }
      
      z_value <- qnorm(1 - (1 - confidence_level) / 2)
      
      if (self$options$method == "Cochran") {
        sample_size <- (N * z_value^2 * p * (1-p)) / 
          ((margin_error^2 * (N-1)) + (z_value^2 * p * (1-p)))
        
        if (correction) {
          sample_size <- sample_size / (1 + ((sample_size - 1) / N))
        }
        
        cat("Valores Intermediários para Cochran:\n")
        cat("Z-Value:", z_value, "\n")
        cat("Proporção (p):", p, "\n")
        cat("Margem de Erro (E):", margin_error, "\n")
        cat("Tamanho da Amostra:", sample_size, "\n")
        cat("Correção Aplicada:", correction, "\n")
        
        results_list <- list(
          method = "Cochran",
          population_size = N,
          confidence_level = self$options$confidence_level,
          margin_error = self$options$margin_error,
          population_proportion = p,
          sample_size = ceiling(sample_size),
          correction_applied = if (correction) "Yes" else "No"
        )
      }
      
      else if (self$options$method == "Slovin") {
        sample_size <- N / (1 + N * (margin_error^2))
        
        results_list <- list(
          method = "Slovin",
          population_size = N,
          confidence_level = self$options$confidence_level,
          margin_error = self$options$margin_error,
          population_proportion = NA,  
          sample_size = ceiling(sample_size),
          correction_applied = "No"  
        )
      }
      
      table$setRow(rowKey = self$options$method, values = results_list)
      self$results$.update()  
    }
  )
)