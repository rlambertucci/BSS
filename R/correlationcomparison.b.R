correlationComparisonClass <- R6::R6Class(
  "correlationComparisonClass",
  inherit = correlationComparisonBase,
  private = list(
    .run = function() {
      
      correlation_type <- self$options$correlation_type
      if (is.null(correlation_type)) {
        return()
      }
      
      results_list <- list() 
      calculation_possible <- TRUE
      statistic_to_report <- NA_real_
      p_value <- NA_real_
      df <- NA_integer_ 
      statistic_label <- "Statistic" 
      
      if (correlation_type == "independent") {
        n1 <- self$options$independent_n1
        n2 <- self$options$independent_n2
        r1 <- self$options$independent_r1
        r2 <- self$options$independent_r2
        
        if (is.null(n1) || is.null(n2) || is.null(r1) || is.null(r2) || n1 == 0 || n2 == 0) { 
          jmvcore::reject("Please provide valid inputs (N > 0) for independent comparison.", code = "missing_input_ind")
          calculation_possible <- FALSE
        } else if (n1 < 4 || n2 < 4) {
          jmvcore::reject("Sample sizes (N) must be at least 4 for independent comparison.", code = "n_too_small_ind")
          calculation_possible <- FALSE
        } else if (abs(r1) > 1 || abs(r2) > 1) {
          jmvcore::reject("Correlation coefficients must be between -1 and 1.", code = "r_out_of_bounds_ind")
          calculation_possible <- FALSE
        } else if (abs(r1) == 1 || abs(r2) == 1) {
          jmvcore::reject("Fisher's Z-transformation is undefined for correlations of exactly +/- 1.", code = "r_perfect_ind")
          calculation_possible <- FALSE
        }
        
        if (calculation_possible) {
          z1 <- 0.5 * log((1 + r1) / (1 - r1))
          z2 <- 0.5 * log((1 + r2) / (1 - r2))
          se <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
          statistic_to_report <- (z1 - z2) / se
          statistic_label <- "Z"
          p_value <- 2 * (1 - stats::pnorm(abs(statistic_to_report)))
          
          results_list <- list(
            comparisonType = "Independent",
            n_group1 = n1,
            n_group2 = n2,
            r_group1 = r1,
            r_group2 = r2,
            n_dep = NA,      
            r12_dep = NA,
            r13_dep = NA,
            r23_dep = NA,
            statistic = statistic_to_report,
            p_value = p_value
          )
        } else {
          results_list <- list(
            comparisonType = "Independent", n_group1=n1, n_group2=n2, r_group1=r1, r_group2=r2,
            n_dep=NA, r12_dep=NA, r13_dep=NA, r23_dep=NA, statistic=NA, p_value=NA
          )
        }
        
      } else if (correlation_type == "dependent") {
        n <- self$options$dependent_n
        r12 <- self$options$dependent_r12
        r13 <- self$options$dependent_r13
        r23 <- self$options$dependent_r23
        
        if (is.null(n) || is.null(r12) || is.null(r13) || is.null(r23) || n == 0) { 
          jmvcore::reject("Please provide valid inputs (N > 0) for dependent comparison.", code = "missing_input_dep")
          calculation_possible <- FALSE
        } else if (n < 4) {
          jmvcore::reject("Sample size (N) must be at least 4 for dependent comparison.", code = "n_too_small_dep")
          calculation_possible <- FALSE
        } else if (abs(r12) > 1 || abs(r13) > 1 || abs(r23) > 1) {
          jmvcore::reject("Correlation coefficients must be between -1 and 1.", code = "r_out_of_bounds_dep")
          calculation_possible <- FALSE
        } else if (abs(r12) == 1 || abs(r13) == 1) {
          jmvcore::reject("Comparison test is undefined for correlations (r12, r13) of exactly +/- 1.", code = "r_perfect_dep")
          calculation_possible <- FALSE
        }
        
        if (calculation_possible) {
          det_R <- 1 - r12^2 - r13^2 - r23^2 + 2 * r12 * r13 * r23
          if (det_R <= 1e-10) { 
            jmvcore::reject("The correlation matrix implied by r12, r13, and r23 is not positive definite (determinant <= 0). Check input values.", code = "corr_matrix_invalid")
            calculation_possible <- FALSE
          } else {
            R_sq_avg = (r12^2 + r13^2) / 2
            denominator_term = (2 * ( (n - 1)/(n - 3) ) * det_R + R_sq_avg * (1 - r23)^3)
            if (denominator_term <= 1e-10) { 
              jmvcore::reject("Denominator in t-statistic calculation is near zero. Check input values.", code="t_denom_zero_dep")
              calculation_possible <- FALSE
            } else {
              t_stat = (r12 - r13) * sqrt( ((n - 1) * (1 + r23)) / denominator_term )
              df = n - 3
              p_value <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
              statistic_to_report = t_stat
              statistic_label = "t"
            }
          }
        }
        
        if (calculation_possible) {
          results_list <- list(
            comparisonType = "Dependent",
            n_group1 = NA,    
            n_group2 = NA,
            r_group1 = NA,
            r_group2 = NA,
            n_dep = n,
            r12_dep = r12,
            r13_dep = r13,
            r23_dep = r23,
            statistic = statistic_to_report,
            p_value = p_value
          )
        } else {
          results_list <- list(
            comparisonType = "Dependent", n_group1=NA, n_group2=NA, r_group1=NA, r_group2=NA,
            n_dep=n, r12_dep=r12, r13_dep=r13, r23_dep=r23, statistic=NA, p_value=NA
          )
        }
        
      } else {
        jmvcore::reject("Unknown correlation type selected.", code = "unknown_type")
        return()
      }
      
      table <- self$results$comparisonTable
      
      stat_col_title <- statistic_label 
      if (calculation_possible) {
        if (correlation_type == "dependent") {
          stat_col_title <- paste0(statistic_label, "(", df, ")")
        } else {
          stat_col_title <- statistic_label 
        }
      }
      if ('statistic' %in% names(table$columns)) {
        table$getColumn('statistic')$setTitle(stat_col_title)
      }
      
      
      if (length(results_list) > 0) {
        table$setRow(rowNo = 1, values = results_list)
      } else {
        
      }
      
    } 
  ) 
) 