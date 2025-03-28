resourceEquationSampleSizeClass <- R6::R6Class(
  "resourceEquationSampleSizeClass",
  inherit = resourceEquationSampleSizeBase,
  private = list(
    .run = function() {
      if (is.null(self$options$design))
        return()
      
      table <- self$results$sampleSizeTable
      results_list <- list()
      
      if (self$options$design == "Design 1 (One-way ANOVA)") {
        k <- self$options$nGroups
        
        min_n <- ceiling(10/k + 1)  
        max_n <- floor(20/k + 1)    
        
        min_total <- min_n * k
        max_total <- max_n * k
        
        results_list <- list(
          design = "Design 1 (One-way ANOVA)",
          nGroups = k,
          nMeasures = NA,
          sacrifice = NA,
          minN = min_n,
          maxN = max_n,
          minTotal = min_total,
          maxTotal = max_total
        )
      }
      
      else if (self$options$design == "Design 2 (Repeated Measures)") {
        r <- self$options$nMeasures
        sacrifice <- self$options$sacrifice
        
        min_n <- ceiling(10/(r-1) + 1)  
        max_n <- floor(20/(r-1) + 1)    
        
        if (sacrifice) {
          min_total <- min_n * r
          max_total <- max_n * r
        } else {
          min_total <- min_n
          max_total <- max_n
        }
        
        results_list <- list(
          design = "Design 2 (Repeated Measures)",
          nGroups = NA,
          nMeasures = r,
          sacrifice = if(sacrifice) "Yes" else "No",
          minN = min_n,
          maxN = max_n,
          minTotal = min_total,
          maxTotal = max_total
        )
      }
      
      else if (self$options$design == "Design 3 (Mixed ANOVA)") {
        k <- self$options$nGroups
        r <- self$options$nMeasures
        sacrifice <- self$options$sacrifice
        
        min_n <- ceiling(10/(k*r) + 1)  
        max_n <- floor(20/(k*r) + 1)    
        
        if (max_n < min_n) {
          max_n <- min_n
        }
        
        if (sacrifice) {
          min_total <- min_n * k * r
          max_total <- max_n * k * r
        } else {
          min_total <- min_n * k
          max_total <- max_n * k
        }
        
        results_list <- list(
          design = "Design 3 (Mixed ANOVA)",
          nGroups = k,
          nMeasures = r,
          sacrifice = if(sacrifice) "Yes" else "No",
          minN = min_n,
          maxN = max_n,
          minTotal = min_total,
          maxTotal = max_total
        )
      }
      
      table$setRow(rowKey = self$options$design, values = results_list)
      
      self$results$.update()
    }
  )
)