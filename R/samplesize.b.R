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
      } else if (mode == "power_t") {
        private$.runPowerT()
      } else if (mode == "tost") {
        private$.runTOST()
      } else if (mode == "proportions") {
        private$.runProportions()
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
    },
    
    .runPowerT = function() {
      t_type  <- self$options$t_test_type
      d       <- self$options$t_effect_size
      alpha   <- self$options$t_alpha
      power   <- self$options$t_power
      alt     <- self$options$t_alternative
      show_cv <- isTRUE(self$options$show_power_curve)
      n_max   <- if (!is.null(self$options$curve_n_max) && self$options$curve_n_max >= 20) self$options$curve_n_max else 150
      
      table <- self$results$sampleSizeTable_power_t
      table$setRow(rowNo = 1, values = list())
      
      if (is.null(d) || d <= 0) {
        self$results$text_power_t$setContent("Effect size (Cohen's d) must be positive (d > 0).")
        return()
      }
      if (is.null(alpha) || alpha <= 0 || alpha >= 1) {
        self$results$text_power_t$setContent("Significance level (Î±) must be between 0 and 1.")
        return()
      }
      if (is.null(power) || power <= 0 || power >= 1) {
        self$results$text_power_t$setContent("Target power (1 - Î²) must be between 0 and 1.")
        return()
      }
      
      type_arg <- switch(t_type,
                         two_sample = "two.sample",
                         one_sample = "one.sample",
                         paired     = "paired",
                         "two.sample")
      alt_arg  <- switch(alt,
                         two_sided = "two.sided",
                         one_sided = "one.sided",
                         "two.sided")
      
      type_label <- switch(t_type,
                           two_sample = "Independent Two-Sample t-test",
                           one_sample = "One-Sample t-test",
                           paired     = "Paired-Samples t-test",
                           "Two-Sample t-test")
      
      pwr_res <- tryCatch({
        stats::power.t.test(delta = abs(d), sd = 1, sig.level = alpha, power = power, type = type_arg, alternative = alt_arg)
      }, error = function(e) NULL)
      
      if (is.null(pwr_res)) {
        self$results$text_power_t$setContent("Error computing sample size. Check parameters.")
        return()
      }
      
      n_per_group <- ceiling(pwr_res$n)
      total_n     <- if (t_type == "two_sample") n_per_group * 2 else n_per_group
      
      table$setRow(rowNo = 1, values = list(
        test_type   = type_label,
        effect_size = d,
        alpha       = alpha,
        power       = power,
        n_per_group = as.integer(n_per_group),
        total_n     = as.integer(total_n)
      ))
      
      self$results$text_power_t$setContent(paste0(
        "Power Analysis for t-test complete: Required sample size is ",
        n_per_group, if (t_type == "two_sample") " subjects per group (Total N = " else " subjects (Total N = ",
        total_n, ") for ", round(power * 100, 1), "% power at Î± = ", alpha, "."
      ))
      
      if (show_cv) {
        n_seq <- seq(4, n_max, by = 1)
        calc_curve <- function(d_val, label) {
          p_vals <- sapply(n_seq, function(n_i) {
            tryCatch({
              stats::power.t.test(n = n_i, delta = d_val, sd = 1, sig.level = alpha, type = type_arg, alternative = alt_arg)$power
            }, error = function(e) NA_real_)
          })
          data.frame(n = n_seq, power = p_vals, effect = label, stringsAsFactors = FALSE)
        }
        
        df_target <- calc_curve(d, paste0("User effect (d = ", d, ")"))
        df_small  <- calc_curve(0.2, "Small (d = 0.2)")
        df_med    <- calc_curve(0.5, "Medium (d = 0.5)")
        df_large  <- calc_curve(0.8, "Large (d = 0.8)")
        
        all_curves <- rbind(df_target, df_small, df_med, df_large)
        image <- self$results$powerCurvePlot
        image$setState(list(
          curves       = all_curves,
          target_power = power,
          target_n     = n_per_group,
          target_d     = d
        ))
      }
    },
    
    .renderPowerCurve = function(image, ggtheme, theme, ...) {
      state <- image$state
      if (is.null(state) || is.null(state$curves)) return(FALSE)
      
      df <- state$curves
      target_p <- state$target_power
      target_n <- state$target_n
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = n, y = power, color = effect)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_hline(yintercept = target_p, linetype = "dashed", color = "#c0392b", linewidth = 0.8) +
        ggplot2::geom_vline(xintercept = target_n, linetype = "dotted", color = "#2c3e50", linewidth = 0.8) +
        ggplot2::geom_point(data = data.frame(n = target_n, power = target_p), ggplot2::aes(x = n, y = power), color = "#c0392b", size = 3.5, inherit.aes = FALSE) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
        ggplot2::labs(
          title = "Power Sensitivity Curve (t-test)",
          x = "Sample Size per Group (n)",
          y = "Statistical Power (1 - Î²)",
          color = "Effect Size"
        ) +
        ggtheme +
        ggplot2::theme(
          legend.position = "bottom",
          panel.grid.minor = ggplot2::element_blank()
        )
      
      print(p)
      TRUE
    },
    
    .runTOST = function() {
      eq_bound   <- abs(self$options$tost_eq_bound)
      true_diff  <- if (!is.null(self$options$tost_true_diff)) self$options$tost_true_diff else 0.0
      alpha      <- self$options$tost_alpha
      power      <- self$options$tost_power
      tost_type  <- self$options$tost_type
      
      table <- self$results$sampleSizeTable_tost
      table$setRow(rowNo = 1, values = list())
      
      if (is.null(eq_bound) || eq_bound <= 0) {
        self$results$text_tost$setContent("Equivalence Bound (Î”) must be strictly positive (Î” > 0).")
        return()
      }
      if (abs(true_diff) >= eq_bound) {
        self$results$text_tost$setContent("True difference (|Î´|) must be strictly smaller than equivalence bound (Î”).")
        return()
      }
      if (is.null(alpha) || alpha <= 0 || alpha >= 0.5) {
        self$results$text_tost$setContent("Significance level (Î±) must be between 0 and 0.5.")
        return()
      }
      if (is.null(power) || power <= 0 || power >= 1) {
        self$results$text_tost$setContent("Target power must be between 0 and 1.")
        return()
      }
      
      z_alpha <- stats::qnorm(1 - alpha)
      z_beta  <- stats::qnorm(1 - (1 - power) / 2)
      denom   <- (eq_bound - abs(true_diff))
      
      is_paired <- (tost_type == "paired")
      factor_k  <- if (is_paired) 1 else 2
      
      n_raw <- factor_k * ((z_alpha + z_beta) / denom)^2
      n_req <- max(3, ceiling(n_raw))
      total_n <- if (is_paired) n_req else n_req * 2
      
      design_label <- if (is_paired) "Paired-Samples Equivalence (TOST)" else "Two Independent Groups Equivalence (TOST)"
      
      table$setRow(rowNo = 1, values = list(
        design      = design_label,
        eq_bound    = eq_bound,
        true_diff   = true_diff,
        alpha       = alpha,
        power       = power,
        n_per_group = as.integer(n_req),
        total_n     = as.integer(total_n)
      ))
      
      self$results$text_tost$setContent(paste0(
        "TOST Equivalence Sample Size: Required sample size is ",
        n_req, if (!is_paired) " subjects per group (Total N = " else " pairs (Total N = ",
        total_n, ") to establish equivalence within [ -", eq_bound, ", +", eq_bound,
        " ] with ", round(power * 100, 1), "% power at nominal Î± = ", alpha, " (Lakens, 2013; Schuirmann, 1987)."
      ))
    },
    
    .runProportions = function() {
      prop_type <- self$options$prop_test_type
      p1        <- self$options$prop_p1
      p2        <- self$options$prop_p2
      p0        <- self$options$prop_p0
      p12       <- self$options$prop_p12
      p21       <- self$options$prop_p21
      alpha     <- self$options$prop_alpha
      power     <- self$options$prop_power
      alt       <- self$options$prop_alternative
      
      table <- self$results$sampleSizeTable_proportions
      table$setRow(rowNo = 1, values = list())
      
      if (is.null(alpha) || alpha <= 0 || alpha >= 1 || is.null(power) || power <= 0 || power >= 1) {
        self$results$text_proportions$setContent("Î± and target power must be strictly between 0 and 1.")
        return()
      }
      
      z_alpha <- stats::qnorm(1 - (if (alt == "two_sided") alpha / 2 else alpha))
      z_beta  <- stats::qnorm(power)
      
      if (prop_type == "two_prop") {
        if (is.null(p1) || is.null(p2) || p1 <= 0 || p1 >= 1 || p2 <= 0 || p2 >= 1 || p1 == p2) {
          self$results$text_proportions$setContent("Proportions pâ‚ and pâ‚‚ must be between 0 and 1, and pâ‚ â‰  pâ‚‚.")
          return()
        }
        alt_arg <- switch(alt, two_sided = "two.sided", one_sided = "one.sided", "two.sided")
        pwr_res <- tryCatch({
          stats::power.prop.test(p1 = p1, p2 = p2, sig.level = alpha, power = power, alternative = alt_arg)
        }, error = function(e) NULL)
        
        if (is.null(pwr_res)) {
          p_bar <- (p1 + p2) / 2
          n_raw <- ((z_alpha * sqrt(2 * p_bar * (1 - p_bar)) + z_beta * sqrt(p1 * (1 - p1) + p2 * (1 - p2))) / (p1 - p2))^2
          n_per_group <- ceiling(n_raw)
        } else {
          n_per_group <- ceiling(pwr_res$n)
        }
        total_n <- n_per_group * 2
        
        table$setRow(rowNo = 1, values = list(
          test_type   = "Two Independent Proportions",
          p1          = p1,
          p2          = p2,
          alpha       = alpha,
          power       = power,
          n_per_group = as.integer(n_per_group),
          total_n     = as.integer(total_n)
        ))
        self$results$text_proportions$setContent(paste0(
          "Two-proportion test complete: Required sample size is ", n_per_group,
          " subjects per group (Total N = ", total_n, ") for ", round(power * 100, 1),
          "% power at Î± = ", alpha, " (Fleiss et al., 2003)."
        ))
        
      } else if (prop_type == "one_prop") {
        if (is.null(p1) || is.null(p0) || p1 <= 0 || p1 >= 1 || p0 <= 0 || p0 >= 1 || p1 == p0) {
          self$results$text_proportions$setContent("Proportions pâ‚ and null value pâ‚€ must be between 0 and 1, and pâ‚ â‰  pâ‚€.")
          return()
        }
        n_raw <- ((z_alpha * sqrt(p0 * (1 - p0)) + z_beta * sqrt(p1 * (1 - p1))) / (p1 - p0))^2
        n_req <- max(5, ceiling(n_raw))
        
        table$setRow(rowNo = 1, values = list(
          test_type   = "One-Sample Proportion vs. Null",
          p1          = p1,
          p2          = p0,
          alpha       = alpha,
          power       = power,
          n_per_group = as.integer(n_req),
          total_n     = as.integer(n_req)
        ))
        self$results$text_proportions$setContent(paste0(
          "One-sample proportion test complete: Required sample size is ", n_req,
          " subjects for ", round(power * 100, 1), "% power at Î± = ", alpha, "."
        ))
        
      } else if (prop_type == "mcnemar") {
        if (is.null(p12) || is.null(p21) || p12 <= 0 || p21 <= 0 || (p12 + p21) >= 1 || p12 == p21) {
          self$results$text_proportions$setContent("Discordant proportions pâ‚â‚‚ and pâ‚‚â‚ must be positive with (pâ‚â‚‚ + pâ‚‚â‚) < 1, and pâ‚â‚‚ â‰  pâ‚‚â‚.")
          return()
        }
        p_disc <- p12 + p21
        diff   <- p12 - p21
        n_raw  <- ((z_alpha * p_disc + z_beta * sqrt(p_disc - diff^2))^2) / (diff^2 * p_disc)
        n_pairs <- max(5, ceiling(n_raw))
        
        table$setRow(rowNo = 1, values = list(
          test_type   = "Paired Proportions (McNemar)",
          p1          = p12,
          p2          = p21,
          alpha       = alpha,
          power       = power,
          n_per_group = as.integer(n_pairs),
          total_n     = as.integer(n_pairs)
        ))
        self$results$text_proportions$setContent(paste0(
          "McNemar paired proportions test complete: Required sample size is ", n_pairs,
          " matched pairs for ", round(power * 100, 1), "% power at Î± = ", alpha,
          " (McNemar, 1947; Connor, 1987)."
        ))
      }
    }
  )
)
