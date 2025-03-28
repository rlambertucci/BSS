correlationComparisonClass <- R6::R6Class(
  "correlationComparisonClass",
  inherit = correlationComparisonBase,
  private = list(
    .run = function() {
      if (is.null(self$options$correlation_type))
        return()
      
      table <- self$results$comparisonTable
      results_list <- list()
      
      if (self$options$correlation_type == "Independent") {
        n1 <- self$options$independent_n1
        n2 <- self$options$independent_n2
        r1 <- self$options$independent_r1
        r2 <- self$options$independent_r2
        
        z1 <- 0.5 * log((1 + r1) / (1 - r1))
        z2 <- 0.5 * log((1 + r2) / (1 - r2))
        se <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))  
        z_diff <- z1 - z2
        p_value <- 2 * (1 - pnorm(abs(z_diff) / se))  
        
        results_list <- list(
          correlation_type = "Independent",
          n1 = n1,
          n2 = n2,
          r1 = r1,
          r2 = r2,
          p_value = p_value
        )
        
      } else if (self$options$correlation_type == "Dependent") {
        r12 <- self$options$dependent_r12
        r13 <- self$options$dependent_r13
        r23 <- self$options$dependent_r23
        
        n <- self$options$independent_n1  
        
        numerator <- (r12 - r13) * sqrt((n - 1) * (1 + r23) / 2)
        denominator <- sqrt(2 * (1 - r12^2 - r13^2 - r23^2 + 2 * r12 * r13 * r23))
        z_diff <- numerator / denominator
        
        p_value <- 2 * (1 - pnorm(abs(z_diff)))
        
        results_list <- list(
          correlation_type = "Dependent",
          n1 = NA,  
          n2 = NA,  
          r1 = r12,
          r2 = r13,
          p_value = p_value
        )
      }
      
      table$setRow(rowKey = self$options$correlation_type, values = results_list)
      self$results$.update()  
    }
  )
)