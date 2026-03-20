resourceEquationSampleSizeClass <- R6::R6Class(
  "resourceEquationSampleSizeClass",
  inherit = resourceEquationSampleSizeBase,
  private = list(
    .run = function() {
      
      design <- self$options$design
      if (is.null(design)) {
        return()
      }
      
      table <- self$results$sampleSizeTable
      
      rowIndex <- 1
      
      addParameterRow <- function(param, value) {
        table$addRow(rowKey = rowIndex, values = list(parameter = param, value = as.character(value)))
        rowIndex <<- rowIndex + 1
      }
      
      addParameterRow("Design Type", design)
      
      if (design == "Design 1 (One-way ANOVA)") {
        k <- self$options$nGroups
        
        if (is.null(k) || k <= 0) {
          self$results$text$setContent("Number of Groups (k) must be a positive integer for Design 1.")
          return()
        }
        
        min_n <- ceiling(10/k + 1)
        max_n <- floor(20/k + 1)
        if (max_n < min_n) max_n <- min_n
        
        min_total <- min_n * k
        max_total <- max_n * k
        
        addParameterRow("Number of Groups (k)", k)
        
        addParameterRow("Minimum Sample Size per Group", min_n)
        addParameterRow("Maximum Sample Size per Group", max_n)
        addParameterRow("Minimum Total Sample Size", min_total)
        addParameterRow("Maximum Total Sample Size", max_total)
        
        self$results$text$setContent("Sample size calculation complete for Design 1 (One-way ANOVA).")
        
      } else if (design == "Design 2 (Repeated Measures)") {
        r <- self$options$nMeasures
        sacrifice <- self$options$sacrifice
        sacrifice_text <- if(sacrifice) "Yes" else "No"
        
        if (is.null(r) || r <= 1) {
          self$results$text$setContent("Number of Repeated Measures (r) must be an integer greater than 1 for Design 2.")
          return()
        }
        
        min_n <- ceiling(10/(r-1) + 1)
        max_n <- floor(20/(r-1) + 1)
        if (max_n < min_n) max_n <- min_n
        
        if (sacrifice) {
          min_total <- min_n * r
          max_total <- max_n * r
        } else {
          min_total <- min_n
          max_total <- max_n
        }
        
        addParameterRow("Number of Repeated Measures (r)", r)
        addParameterRow("Animals Sacrificed", sacrifice_text)
        
        addParameterRow("Minimum Sample Size per Group", min_n)
        addParameterRow("Maximum Sample Size per Group", max_n)
        addParameterRow("Minimum Total Sample Size", min_total)
        addParameterRow("Maximum Total Sample Size", max_total)
        
        self$results$text$setContent("Sample size calculation complete for Design 2 (Repeated Measures).")
        
      } else if (design == "Design 3 (Mixed ANOVA)") {
        k <- self$options$nGroups
        r <- self$options$nMeasures
        sacrifice <- self$options$sacrifice
        sacrifice_text <- if(sacrifice) "Yes" else "No"
        
        if (is.null(k) || k <= 0 || is.null(r) || r <= 0) {
          self$results$text$setContent("Number of Groups (k) and Number of Repeated Measures (r) must be positive integers for Design 3.")
          return()
        }
        
        denominator <- k * r
        
        min_n <- ceiling(10/denominator + 1)
        max_n <- floor(20/denominator + 1)
        
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
        
        addParameterRow("Number of Groups (k)", k)
        addParameterRow("Number of Repeated Measures (r)", r)
        addParameterRow("Animals Sacrificed", sacrifice_text)
        
        addParameterRow("Minimum Sample Size per Group", min_n)
        addParameterRow("Maximum Sample Size per Group", max_n)
        addParameterRow("Minimum Total Sample Size", min_total)
        addParameterRow("Maximum Total Sample Size", max_total)
        
        self$results$text$setContent("Sample size calculation complete for Design 3 (Mixed ANOVA).")
      }
      
    }
  )
)