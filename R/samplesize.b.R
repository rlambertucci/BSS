samplesizeClass <- R6::R6Class(
  "samplesizeClass",
  inherit = samplesizeBase,
  private = list(
    .run = function() {
      mode <- self$options$mode
      if (is.null(mode)) {
        return()
      }
      
      if (mode == "finite") {
        private$.runFinite()
      } else if (mode == "animals") {
        private$.runAnimals()
      }
    },
    
    .runFinite = function() {
      method <- self$options$method_finite
      if (is.null(method)) {
        return()
      }
      
      N <- self$options$population_size_finite
      confidence_level_pct <- self$options$confidence_level_finite
      margin_error_pct <- self$options$margin_error_finite
      p <- self$options$population_proportion_finite
      
      table <- self$results$sampleSizeTable_finite
      table$setRow(rowNo = 1, values = list())
      try(table$deleteNote("fpc_note"), silent = TRUE)
      try(table$deleteNote("slovin_note"), silent = TRUE)
      
      if (is.null(confidence_level_pct) || is.null(margin_error_pct)) {
        self$results$text_finite$setContent("Please provide Confidence Level (%) and Margin of Error (%).")
        return()
      }
      
      confidence_level <- confidence_level_pct / 100
      margin_error <- margin_error_pct / 100
      
      if (confidence_level <= 0 || confidence_level >= 1) {
        self$results$text_finite$setContent("Confidence Level must be strictly between 0% and 100%.")
        return()
      }
      if (margin_error <= 0 || margin_error >= 1) {
        self$results$text_finite$setContent("Margin of Error must be strictly between 0% and 100%.")
        return()
      }
      if (is.null(N) || N <= 0) {
        self$results$text_finite$setContent("Population Size (N) must be a positive integer (N > 0).")
        return()
      }
      
      if (method == "Cochran") {
        if (is.null(p) || p <= 0 || p >= 1) {
          self$results$text_finite$setContent("Population Proportion (p) must be strictly between 0 and 1 for Cochran's method.")
          return()
        }
        
        z_value <- stats::qnorm(1 - (1 - confidence_level) / 2)
        n0 <- (z_value^2 * p * (1 - p)) / (margin_error^2)
        
        if (n0 > 0) {
          sample_size_raw <- n0 / (1 + (n0 - 1) / N)
          sample_size <- min(N, max(1, ceiling(sample_size_raw)))
        } else {
          sample_size <- min(N, max(1, ceiling(n0)))
        }
        
        results_list <- list(
          method = "Cochran",
          population_size = as.integer(N),
          confidence_level = confidence_level_pct,
          margin_error = margin_error_pct,
          population_proportion = p,
          sample_size = as.integer(sample_size)
        )
        table$setRow(rowNo = 1, values = results_list)
        table$setNote("fpc_note", "Note. Finite Population Correction (Cochran, 1977) applied: n = n0 / [1 + (n0 - 1) / N].")
        self$results$text_finite$setContent(paste0("Cochran sample size calculation complete: required sample size is ", sample_size, " subjects."))
        
      } else if (method == "Slovin") {
        sample_size_raw <- N / (1 + N * (margin_error^2))
        sample_size <- min(N, max(1, ceiling(sample_size_raw)))
        
        results_list <- list(
          method = "Slovin",
          population_size = as.integer(N),
          confidence_level = NA,
          margin_error = margin_error_pct,
          population_proportion = NA,
          sample_size = as.integer(sample_size)
        )
        table$setRow(rowNo = 1, values = results_list)
        table$setNote("slovin_note", "Note. Slovin's heuristic formula does not incorporate confidence level (empirically assumes ~95%).")
        self$results$text_finite$setContent(paste0("Slovin sample size calculation complete: required sample size is ", sample_size, " subjects."))
      }
    },
    
    .runAnimals = function() {
      design <- self$options$design_animals
      if (is.null(design)) {
        return()
      }
      
      table <- self$results$sampleSizeTable_animals
      if (table$rowCount > 0) try(table$deleteRows(), silent = TRUE)
      try(table$deleteNote("resource_note"), silent = TRUE)
      
      rowIndex <- 1
      addParameterRow <- function(param, value) {
        table$addRow(rowKey = rowIndex, values = list(parameter = param, value = as.character(value)))
        rowIndex <<- rowIndex + 1
      }
      
      if (design == "Design 1 (One-way ANOVA)") {
        k <- self$options$nGroups_animals
        
        if (is.null(k) || k < 2) {
          self$results$text_animals$setContent("Number of Groups (k) must be an integer >= 2 for Design 1 (One-way ANOVA).")
          return()
        }
        
        min_n <- ceiling(10 / k + 1)
        max_n <- floor(20 / k + 1)
        if (max_n < min_n) max_n <- min_n
        
        min_total <- min_n * k
        max_total <- max_n * k
        
        addParameterRow("Design Type", "One-way ANOVA")
        addParameterRow("Number of Groups (k)", k)
        addParameterRow("Minimum Sample Size per Group", min_n)
        addParameterRow("Maximum Sample Size per Group", max_n)
        addParameterRow("Minimum Total Sample Size", min_total)
        addParameterRow("Maximum Total Sample Size", max_total)
        
        table$setNote("resource_note", "Note. Target degrees of freedom for error term: 10 <= E <= 20 (Charan & Kantharia, 2013; Festing & Altman, 2002).")
        self$results$text_animals$setContent("Sample size calculation complete for Design 1 (One-way ANOVA).")
        
      } else if (design == "Design 2 (Repeated Measures)") {
        r <- self$options$nMeasures_animals
        sacrifice <- isTRUE(self$options$sacrifice_animals)
        sacrifice_text <- if (sacrifice) "Yes" else "No"
        
        if (is.null(r) || r < 2) {
          self$results$text_animals$setContent("Number of Repeated Measures (r) must be an integer >= 2 for Design 2.")
          return()
        }
        
        if (!sacrifice) {
          # True repeated measures on same cohort: E = (n - 1)(r - 1)
          min_n <- ceiling(10 / (r - 1) + 1)
          max_n <- floor(20 / (r - 1) + 1)
          if (max_n < min_n) max_n <- min_n
          
          min_total <- min_n
          max_total <- max_n
          
          addParameterRow("Design Type", "Repeated Measures (Single Cohort)")
          addParameterRow("Number of Repeated Measures (r)", r)
          addParameterRow("Animals Sacrificed", sacrifice_text)
          addParameterRow("Minimum Sample Size per Cohort", min_n)
          addParameterRow("Maximum Sample Size per Cohort", max_n)
          addParameterRow("Minimum Total Sample Size", min_total)
          addParameterRow("Maximum Total Sample Size", max_total)
          
        } else {
          # Animals sacrificed at each measure: r independent cohorts, E = r(n - 1)
          min_n_cohort <- ceiling(10 / r + 1)
          max_n_cohort <- floor(20 / r + 1)
          if (max_n_cohort < min_n_cohort) max_n_cohort <- min_n_cohort
          
          min_total <- min_n_cohort * r
          max_total <- max_n_cohort * r
          
          addParameterRow("Design Type", "Repeated Measures (Sacrificed per Measure)")
          addParameterRow("Number of Time Points (r)", r)
          addParameterRow("Animals Sacrificed", sacrifice_text)
          addParameterRow("Minimum Sample Size per Time Point", min_n_cohort)
          addParameterRow("Maximum Sample Size per Time Point", max_n_cohort)
          addParameterRow("Minimum Total Sample Size", min_total)
          addParameterRow("Maximum Total Sample Size", max_total)
        }
        
        table$setNote("resource_note", "Note. Target degrees of freedom for error term: 10 <= E <= 20 (Arifin & Wan Mohammad, 2017; Festing & Altman, 2002).")
        self$results$text_animals$setContent("Sample size calculation complete for Design 2 (Repeated Measures).")
        
      } else if (design == "Design 3 (Mixed ANOVA)") {
        k <- self$options$nGroups_animals
        r <- self$options$nMeasures_animals
        sacrifice <- isTRUE(self$options$sacrifice_animals)
        sacrifice_text <- if (sacrifice) "Yes" else "No"
        
        if (is.null(k) || k < 2 || is.null(r) || r < 2) {
          self$results$text_animals$setContent("Number of Groups (k) and Number of Repeated Measures (r) must both be integers >= 2 for Design 3.")
          return()
        }
        
        if (!sacrifice) {
          # Mixed ANOVA (k between x r within): E_within = k(n - 1)(r - 1)
          min_n <- ceiling(10 / (k * (r - 1)) + 1)
          max_n <- floor(20 / (k * (r - 1)) + 1)
          if (max_n < min_n) max_n <- min_n
          
          min_total <- min_n * k
          max_total <- max_n * k
          
          addParameterRow("Design Type", "Mixed ANOVA (k Between x r Within)")
          addParameterRow("Number of Groups (k)", k)
          addParameterRow("Number of Repeated Measures (r)", r)
          addParameterRow("Animals Sacrificed", sacrifice_text)
          addParameterRow("Minimum Sample Size per Group", min_n)
          addParameterRow("Maximum Sample Size per Group", max_n)
          addParameterRow("Minimum Total Sample Size", min_total)
          addParameterRow("Maximum Total Sample Size", max_total)
          
        } else {
          # Factorial between-subjects with k x r cells: E = k*r*(n - 1)
          min_n_cell <- ceiling(10 / (k * r) + 1)
          max_n_cell <- floor(20 / (k * r) + 1)
          if (max_n_cell < min_n_cell) max_n_cell <- min_n_cell
          
          min_per_group <- min_n_cell * r
          max_per_group <- max_n_cell * r
          min_total <- min_per_group * k
          max_total <- max_per_group * k
          
          addParameterRow("Design Type", "Factorial Design (k Groups x r Sacrifice Times)")
          addParameterRow("Number of Groups (k)", k)
          addParameterRow("Number of Time Points (r)", r)
          addParameterRow("Animals Sacrificed", sacrifice_text)
          addParameterRow("Minimum Animals per Time Point per Group", min_n_cell)
          addParameterRow("Maximum Animals per Time Point per Group", max_n_cell)
          addParameterRow("Minimum Sample Size per Group (all times)", min_per_group)
          addParameterRow("Maximum Sample Size per Group (all times)", max_per_group)
          addParameterRow("Minimum Total Sample Size", min_total)
          addParameterRow("Maximum Total Sample Size", max_total)
        }
        
        table$setNote("resource_note", "Note. Target degrees of freedom for error term: 10 <= E <= 20 (Arifin & Wan Mohammad, 2017; Festing & Altman, 2002).")
        self$results$text_animals$setContent("Sample size calculation complete for Design 3 (Mixed ANOVA).")
      }
    }
  )
)
