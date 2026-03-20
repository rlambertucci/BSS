finiteSampleSizeClass <- R6::R6Class(
  "finiteSampleSizeClass",
  inherit = finiteSampleSizeBase,
  private = list(
    .run = function() {
      
      method <- self$options$method
      if (is.null(method)) {
        return()
      }
      
      
      N <- self$options$population_size
      confidence_level_pct <- self$options$confidence_level
      margin_error_pct <- self$options$margin_error
      p <- self$options$population_proportion
      
      
      calculation_possible <- TRUE
      results_list <- list()
      
      if (is.null(confidence_level_pct) || is.null(margin_error_pct)) {
        jmvcore::reject("Confidence Level and Margin of Error must be provided.", code="missing_input_basic")
        calculation_possible <- FALSE
      } else {
        confidence_level <- confidence_level_pct / 100
        margin_error <- margin_error_pct / 100
        
        if (confidence_level <= 0 || confidence_level >= 1) {
          jmvcore::reject("Confidence Level must be between 0 and 100.", code="invalid_confidence")
          calculation_possible <- FALSE
        }
        if (margin_error <= 0 || margin_error >= 1) {
          jmvcore::reject("Margin of Error must be between 0 and 100.", code="invalid_error")
          calculation_possible <- FALSE
        }
      }
      
      
      if (calculation_possible) {
        if (method == "Cochran") {
          if (is.null(N) || N <= 0) {
            jmvcore::reject("A valid Population Size (N > 0) must be provided for the Cochran (FPC) method.", code="missing_n_cochran_fpc")
            calculation_possible <- FALSE
          } else if (is.null(p)) {
            jmvcore::reject("Population Proportion (p) must be provided for Cochran method.", code="missing_p_cochran")
            calculation_possible <- FALSE
          } else if (p < 0 || p > 1) {
            jmvcore::reject("Population Proportion (p) must be between 0 and 1.", code="invalid_p_cochran")
            calculation_possible <- FALSE
          } else if (p == 0 || p == 1) {
            jmvcore::reject("Population Proportion (p) cannot be exactly 0 or 1 for Cochran method.", code="extreme_p_cochran")
            calculation_possible <- FALSE
          }
          
          
        } else if (method == "Slovin") {
          if (is.null(N) || N <= 0) {
            jmvcore::reject("A valid Population Size (N > 0) must be provided for Slovin method.", code="missing_n_slovin")
            calculation_possible <- FALSE
          }
        }
      }
      
      
      if (calculation_possible) {
        sample_size <- NA_real_
        correction_applied_text <- "Yes"
        
        if (method == "Cochran") {
          z_value <- stats::qnorm(1 - (1 - confidence_level) / 2)
          
          n0 <- (z_value^2 * p * (1-p)) / (margin_error^2)
          
          if (n0 > 0) {
            sample_size <- n0 / (1 + (n0 - 1) / N)
            sample_size <- max(1, sample_size)
          } else {
            sample_size <- n0
          }
          
          
          results_list <- list(
            method = "Cochran",
            population_size = N,
            confidence_level = confidence_level_pct,
            margin_error = margin_error_pct,
            population_proportion = p,
            sample_size = ceiling(sample_size),
            correction_applied = correction_applied_text
          )
          
        } else if (method == "Slovin") {
          sample_size <- N / (1 + N * (margin_error^2))
          sample_size <- max(1, sample_size)
          correction_applied_text <- "N/A"
          
          results_list <- list(
            method = "Slovin",
            population_size = N,
            confidence_level = confidence_level_pct,
            margin_error = margin_error_pct,
            population_proportion = NA,
            sample_size = ceiling(sample_size),
            correction_applied = correction_applied_text
          )
        }
      }
      
      
      table <- self$results$sampleSizeTable
      
      
      table$setRow(rowNo = 1, values = list())
      
      if (calculation_possible && length(results_list) > 0) {
        table$setRow(rowNo = 1, values = results_list)
      } else {
      }
      
    }
  )
)