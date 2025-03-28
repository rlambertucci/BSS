effectSizeCalculationClass <- R6::R6Class(
  "effectSizeCalculationClass",
  inherit = effectSizeCalculationBase,
  private = list(
    .run = function() {
      if (is.null(self$options$test_type) || self$options$chi_square_value == 0 || self$options$sample_size == 0)
        return()
      
      table <- self$results$effectSizeTable
      results_list <- list()
      
      if (self$options$test_type == "Friedman") {
        chi_square <- self$options$chi_square_value
        df <- self$options$df
        n <- self$options$sample_size
        k <- self$options$num_conditions  
        
        w <- chi_square / (n * (k - 1))
        
        interpretation <- ifelse(w < 0.1, "Small", ifelse(w < 0.3, "Medium", "Large"))
        
        results_list <- list(
          test_type = "Friedman",
          chi_square_value = chi_square,
          df = df,
          sample_size = n,
          num_conditions = k,
          effect_size = w,
          interpretation = interpretation
        )
      }
      
      else if (self$options$test_type == "McNemar") {
        chi_square <- self$options$chi_square_value
        n <- self$options$sample_size
        
        g <- chi_square / n
        
        interpretation <- ifelse(g < 0.2, "Small", ifelse(g < 0.5, "Medium", "Large"))
        
        results_list <- list(
          test_type = "McNemar",
          chi_square_value = chi_square,
          df = NA,  
          sample_size = n,
          num_conditions = NA,  
          effect_size = g,
          interpretation = interpretation
        )
      }
      
      table$setRow(rowKey = self$options$test_type, values = results_list)
      
      self$results$.update()
    }
  )
)