samplesizeClass <- R6::R6Class(
  "samplesizeClass",
  inherit = samplesizeBase,
  private = list(
    .animal_parameters = function() {
      design <- self$options$design_animals
      if (is.null(design)) return(character())
      dropout <- !is.null(self$options$dropout_animals) && self$options$dropout_animals > 0
      sacrifice <- isTRUE(self$options$sacrifice_animals)
      parameters <- character()
      if (design == "Design 1 (One-way ANOVA)") {
        parameters <- c("Design Type", "Number of Groups (k)", "Minimum Sample Size per Group", "Maximum Sample Size per Group", "Minimum Total Sample Size", "Maximum Total Sample Size")
        if (dropout) parameters <- c(parameters, c("Expected Attrition / Dropout Rate (%)", "Adjusted Minimum n per Group (with Dropout)", "Adjusted Maximum n per Group (with Dropout)", "Adjusted Minimum Total N (with Dropout)", "Adjusted Maximum Total N (with Dropout)"))
      } else if (design == "Design 2 (Repeated Measures)") {
        if (!sacrifice) {
          parameters <- c("Design Type", "Number of Repeated Measures (r)", "Animals Sacrificed", "Minimum Sample Size per Cohort", "Maximum Sample Size per Cohort", "Minimum Total Sample Size", "Maximum Total Sample Size")
          if (dropout) parameters <- c(parameters, c("Expected Attrition / Dropout Rate (%)", "Adjusted Minimum Cohort Size (with Dropout)", "Adjusted Maximum Cohort Size (with Dropout)"))
        } else {
          parameters <- c("Design Type", "Number of Time Points (r)", "Animals Sacrificed", "Minimum Sample Size per Time Point", "Maximum Sample Size per Time Point", "Minimum Total Sample Size", "Maximum Total Sample Size")
          if (dropout) parameters <- c(parameters, c("Expected Attrition / Dropout Rate (%)", "Adjusted Minimum Animals per Time (with Dropout)", "Adjusted Maximum Animals per Time (with Dropout)", "Adjusted Minimum Total N (with Dropout)", "Adjusted Maximum Total N (with Dropout)"))
        }
      } else if (design == "Design 3 (Mixed ANOVA)") {
        if (!sacrifice) {
          parameters <- c("Design Type", "Number of Groups (k)", "Number of Repeated Measures (r)", "Animals Sacrificed", "Minimum Sample Size per Group", "Maximum Sample Size per Group", "Minimum Total Sample Size", "Maximum Total Sample Size")
          if (dropout) parameters <- c(parameters, c("Expected Attrition / Dropout Rate (%)", "Adjusted Minimum n per Group (with Dropout)", "Adjusted Maximum n per Group (with Dropout)", "Adjusted Minimum Total N (with Dropout)", "Adjusted Maximum Total N (with Dropout)"))
        } else {
          parameters <- c("Design Type", "Number of Groups (k)", "Number of Time Points (r)", "Animals Sacrificed", "Minimum Animals per Time Point per Group", "Maximum Animals per Time Point per Group", "Minimum Sample Size per Group (all times)", "Maximum Sample Size per Group (all times)", "Minimum Total Sample Size", "Maximum Total Sample Size")
          if (dropout) parameters <- c(parameters, c("Expected Attrition / Dropout Rate (%)", "Adjusted Animals per Cell (with Dropout)", "Adjusted Minimum Total N (with Dropout)", "Adjusted Maximum Total N (with Dropout)"))
        }
      }
      if (design %in% c("Design 1 (One-way ANOVA)", "Design 3 (Mixed ANOVA)") && isTRUE(self$options$calc_power_anova_animals)) {
        parameters <- c(parameters, c("Power-ANOVA: Effect Size (f)", "Power-ANOVA: Significance Level (α)", "Power-ANOVA: Target Power (1 - β)", "Power-ANOVA: Recommended n per Group", "Power-ANOVA: Recommended Total N"))
        if (dropout) parameters <- c(parameters, c("Power-ANOVA: Adjusted n per Group (with Dropout)", "Power-ANOVA: Adjusted Total N (with Dropout)"))
      }
      parameters
    },

    .init = function() {
      table <- self$results$sampleSizeTable_animals
      if (table$rowCount > 0) table$deleteRows()
      for (parameter in private$.animal_parameters()) table$addRow(rowKey = parameter, values = list(parameter = parameter))
    },

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
      self$results$text_finite$setContent("")
      self$results$text_finite$setVisible(TRUE)
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
      table$setNote("fpc_note", NULL)
      table$setNote("slovin_note", NULL)
      
      if (is.null(margin_error_pct) || (method == "Cochran" && is.null(confidence_level_pct))) {
        jmvcore::reject("Please provide Margin of Error (%) and, for Cochran's method, Confidence Level (%).", code = "invalid_sample_size_parameters")
        return()
      }
      
      confidence_level <- if (method == "Cochran") confidence_level_pct / 100 else NA_real_
      margin_error <- margin_error_pct / 100
      
      if (method == "Cochran" && (confidence_level <= 0 || confidence_level >= 1)) {
        jmvcore::reject("Confidence Level must be strictly between 0% and 100%.", code = "invalid_sample_size_parameters")
        return()
      }
      if (margin_error <= 0 || margin_error >= 1) {
        jmvcore::reject("Margin of Error must be strictly between 0% and 100%.", code = "invalid_sample_size_parameters")
        return()
      }
      if (is.null(N) || N <= 0) {
        jmvcore::reject("Population Size (N) must be a positive integer (N > 0).", code = "invalid_sample_size_parameters")
        return()
      }
      
      if (method == "Cochran") {
        if (is.null(p) || p <= 0 || p >= 1) {
          jmvcore::reject("Population Proportion (p) must be strictly between 0 and 1 for Cochran's method.", code = "invalid_sample_size_parameters")
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
        table$setNote("fpc_note", "Finite Population Correction applied: n = n0 / [1 + (n0 - 1) / N].")
        self$results$text_finite$setContent("")
        self$results$text_finite$setVisible(FALSE)
        
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
        table$setNote("slovin_note", "Slovin's heuristic formula does not incorporate confidence level (empirically assumes ~95%).")
        self$results$text_finite$setContent("")
        self$results$text_finite$setVisible(FALSE)
      }
    },
    
    .runAnimals = function() {
      self$results$text_animals$setContent("")
      self$results$text_animals$setVisible(TRUE)
      design <- self$options$design_animals
      if (is.null(design)) return()
      
      table <- self$results$sampleSizeTable_animals
      table$setNote("resource_note", NULL)
      
      for (parameter in private$.animal_parameters()) table$setRow(rowKey = parameter, values = list(value = NA_integer_, estimate = NA_real_, detail = ""))
      addParameterRow <- function(param, value) {
        values <- list(parameter = param)
        if (is.character(value)) values$detail <- value
        else if (grepl("^Expected Attrition|^Power-ANOVA: (Effect Size|Significance Level|Target Power)", param)) values$estimate <- as.numeric(value)
        else values$value <- as.integer(value)
        table$setRow(rowKey = param, values = values)
      }
      
      dropout <- if (!is.null(self$options$dropout_animals)) max(0, min(50, as.numeric(self$options$dropout_animals))) else 0
      loss_factor <- if (dropout > 0) 1 - (dropout / 100) else 1
      
      if (design == "Design 1 (One-way ANOVA)") {
        k <- self$options$nGroups_animals
        if (is.null(k) || k < 2) {
          jmvcore::reject("Number of Groups (k) must be an integer >= 2 for Design 1 (One-way ANOVA).", code = "invalid_sample_size_parameters")
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
        if (dropout > 0) {
          adj_min_n <- ceiling(min_n / loss_factor)
          adj_max_n <- ceiling(max_n / loss_factor)
          addParameterRow("Expected Attrition / Dropout Rate (%)", dropout)
          addParameterRow("Adjusted Minimum n per Group (with Dropout)", adj_min_n)
          addParameterRow("Adjusted Maximum n per Group (with Dropout)", adj_max_n)
          addParameterRow("Adjusted Minimum Total N (with Dropout)", adj_min_n * k)
          addParameterRow("Adjusted Maximum Total N (with Dropout)", adj_max_n * k)
        }
        if (isTRUE(self$options$calc_power_anova_animals)) {
          p_f <- self$options$power_f_animals
          p_alpha <- self$options$power_alpha_animals
          p_power <- self$options$power_target_animals
          if (!is.null(p_f) && p_f > 0 && !is.null(p_alpha) && !is.null(p_power)) {
            p_res <- tryCatch({
              stats::power.anova.test(groups = k, between.var = p_f^2 * k / (k - 1), within.var = 1, sig.level = p_alpha, power = p_power)
            }, error = function(e) jmvcore::reject(conditionMessage(e), code = "sample_size_calculation_failed"))
            if (!is.null(p_res)) {
              p_n_grp <- ceiling(p_res$n)
              addParameterRow("Power-ANOVA: Effect Size (f)", p_f)
              addParameterRow("Power-ANOVA: Significance Level (α)", p_alpha)
              addParameterRow("Power-ANOVA: Target Power (1 - β)", p_power)
              addParameterRow("Power-ANOVA: Recommended n per Group", p_n_grp)
              addParameterRow("Power-ANOVA: Recommended Total N", p_n_grp * k)
              if (dropout > 0) {
                adj_p_n_grp <- ceiling(p_n_grp / loss_factor)
                addParameterRow("Power-ANOVA: Adjusted n per Group (with Dropout)", adj_p_n_grp)
                addParameterRow("Power-ANOVA: Adjusted Total N (with Dropout)", adj_p_n_grp * k)
              }
            }
          }
        }
        table$setNote("resource_note", "Target degrees of freedom for error term: 10 <= E <= 20. Attrition compensation: N_adj = ceiling(N / (1 - d)).")
        self$results$text_animals$setContent("")
        self$results$text_animals$setVisible(FALSE)
      } else if (design == "Design 2 (Repeated Measures)") {
        r <- self$options$nMeasures_animals
        sacrifice <- isTRUE(self$options$sacrifice_animals)
        sacrifice_text <- if (sacrifice) "Yes" else "No"
        if (is.null(r) || r < 2) {
          jmvcore::reject("Number of Repeated Measures (r) must be an integer >= 2 for Design 2.", code = "invalid_sample_size_parameters")
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
          if (dropout > 0) {
            adj_min_n <- ceiling(min_n / loss_factor)
            adj_max_n <- ceiling(max_n / loss_factor)
            addParameterRow("Expected Attrition / Dropout Rate (%)", dropout)
            addParameterRow("Adjusted Minimum Cohort Size (with Dropout)", adj_min_n)
            addParameterRow("Adjusted Maximum Cohort Size (with Dropout)", adj_max_n)
          }
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
          if (dropout > 0) {
            adj_min_cohort <- ceiling(min_n_cohort / loss_factor)
            adj_max_cohort <- ceiling(max_n_cohort / loss_factor)
            addParameterRow("Expected Attrition / Dropout Rate (%)", dropout)
            addParameterRow("Adjusted Minimum Animals per Time (with Dropout)", adj_min_cohort)
            addParameterRow("Adjusted Maximum Animals per Time (with Dropout)", adj_max_cohort)
            addParameterRow("Adjusted Minimum Total N (with Dropout)", adj_min_cohort * r)
            addParameterRow("Adjusted Maximum Total N (with Dropout)", adj_max_cohort * r)
          }
        }
        table$setNote("resource_note", "Target degrees of freedom for error term: 10 <= E <= 20. Attrition compensation: N_adj = ceiling(N / (1 - d)).")
        self$results$text_animals$setContent("")
        self$results$text_animals$setVisible(FALSE)
      } else if (design == "Design 3 (Mixed ANOVA)") {
        k <- self$options$nGroups_animals
        r <- self$options$nMeasures_animals
        sacrifice <- isTRUE(self$options$sacrifice_animals)
        sacrifice_text <- if (sacrifice) "Yes" else "No"
        if (is.null(k) || k < 2 || is.null(r) || r < 2) {
          jmvcore::reject("Number of Groups (k) and Number of Repeated Measures (r) must both be integers >= 2 for Design 3.", code = "invalid_sample_size_parameters")
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
          if (dropout > 0) {
            adj_min_n <- ceiling(min_n / loss_factor)
            adj_max_n <- ceiling(max_n / loss_factor)
            addParameterRow("Expected Attrition / Dropout Rate (%)", dropout)
            addParameterRow("Adjusted Minimum n per Group (with Dropout)", adj_min_n)
            addParameterRow("Adjusted Maximum n per Group (with Dropout)", adj_max_n)
            addParameterRow("Adjusted Minimum Total N (with Dropout)", adj_min_n * k)
            addParameterRow("Adjusted Maximum Total N (with Dropout)", adj_max_n * k)
          }
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
          if (dropout > 0) {
            adj_min_cell <- ceiling(min_n_cell / loss_factor)
            adj_max_cell <- ceiling(max_n_cell / loss_factor)
            addParameterRow("Expected Attrition / Dropout Rate (%)", dropout)
            addParameterRow("Adjusted Animals per Cell (with Dropout)", adj_min_cell)
            addParameterRow("Adjusted Minimum Total N (with Dropout)", adj_min_cell * k * r)
            addParameterRow("Adjusted Maximum Total N (with Dropout)", adj_max_cell * k * r)
          }
        }
        if (isTRUE(self$options$calc_power_anova_animals)) {
          p_f <- self$options$power_f_animals
          p_alpha <- self$options$power_alpha_animals
          p_power <- self$options$power_target_animals
          if (!is.null(p_f) && p_f > 0 && !is.null(p_alpha) && !is.null(p_power)) {
            p_res <- tryCatch({
              stats::power.anova.test(groups = k, between.var = p_f^2 * k / (k - 1), within.var = 1, sig.level = p_alpha, power = p_power)
            }, error = function(e) jmvcore::reject(conditionMessage(e), code = "sample_size_calculation_failed"))
            if (!is.null(p_res)) {
              p_n_grp <- ceiling(p_res$n)
              addParameterRow("Power-ANOVA: Effect Size (f)", p_f)
              addParameterRow("Power-ANOVA: Significance Level (α)", p_alpha)
              addParameterRow("Power-ANOVA: Target Power (1 - β)", p_power)
              addParameterRow("Power-ANOVA: Recommended n per Group", p_n_grp)
              addParameterRow("Power-ANOVA: Recommended Total N", p_n_grp * k)
              if (dropout > 0) {
                adj_p_n_grp <- ceiling(p_n_grp / loss_factor)
                addParameterRow("Power-ANOVA: Adjusted n per Group (with Dropout)", adj_p_n_grp)
                addParameterRow("Power-ANOVA: Adjusted Total N (with Dropout)", adj_p_n_grp * k)
              }
            }
          }
        }
        table$setNote("resource_note", "Target degrees of freedom for error term: 10 <= E <= 20. Attrition compensation: N_adj = ceiling(N / (1 - d)).")
        self$results$text_animals$setContent("")
        self$results$text_animals$setVisible(FALSE)
      }
    },
    .runPowerT = function() {
      self$results$text_power_t$setContent("")
      self$results$text_power_t$setVisible(TRUE)
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
        jmvcore::reject("Effect size (Cohen's d) must be positive (d > 0).", code = "invalid_sample_size_parameters")
        return()
      }
      if (is.null(alpha) || alpha <= 0 || alpha >= 1) {
        jmvcore::reject("Significance level (\u03b1) must be between 0 and 1.", code = "invalid_sample_size_parameters")
        return()
      }
      if (is.null(power) || power <= 0 || power >= 1) {
        jmvcore::reject("Target power (1 - \u03b2) must be between 0 and 1.", code = "invalid_sample_size_parameters")
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
      }, error = function(e) jmvcore::reject(conditionMessage(e), code = "sample_size_calculation_failed"))
      
      if (is.null(pwr_res)) {
        jmvcore::reject("Error computing sample size. Check parameters.", code = "invalid_sample_size_parameters")
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
      
      self$results$text_power_t$setContent("")
      self$results$text_power_t$setVisible(FALSE)
      
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
          y = "Statistical Power (1 - \u03b2)",
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
      self$results$text_tost$setContent("")
      self$results$text_tost$setVisible(TRUE)
      eq_bound   <- abs(self$options$tost_eq_bound)
      true_diff  <- if (!is.null(self$options$tost_true_diff)) self$options$tost_true_diff else 0.0
      alpha      <- self$options$tost_alpha
      power      <- self$options$tost_power
      tost_type  <- self$options$tost_type
      
      table <- self$results$sampleSizeTable_tost
      table$setRow(rowNo = 1, values = list())
      
      if (length(eq_bound) != 1 || !is.finite(eq_bound) || eq_bound <= 0) {
        jmvcore::reject("Equivalence Bound (\u0394) must be strictly positive (\u0394 > 0).", code = "invalid_sample_size_parameters")
        return()
      }
      if (length(true_diff) != 1 || !is.finite(true_diff) || abs(true_diff) >= eq_bound) {
        jmvcore::reject("True difference (|\u03b4|) must be strictly smaller than equivalence bound (\u0394).", code = "invalid_sample_size_parameters")
        return()
      }
      if (length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 0.5) {
        jmvcore::reject("Significance level (\u03b1) must be between 0 and 0.5.", code = "invalid_sample_size_parameters")
        return()
      }
      if (length(power) != 1 || !is.finite(power) || power <= 0 || power >= 1) {
        jmvcore::reject("Target power must be between 0 and 1.", code = "invalid_sample_size_parameters")
        return()
      }
      
      z_alpha <- stats::qnorm(1 - alpha)
      is_paired <- (tost_type == "paired")
      factor_k <- if (is_paired) 1 else 2
      achieved_power <- function(sample_size) {
        se <- sqrt(factor_k / sample_size)
        lower <- (-eq_bound - true_diff) / se + z_alpha
        upper <- (eq_bound - true_diff) / se - z_alpha
        max(0, stats::pnorm(upper) - stats::pnorm(lower))
      }
      lower_n <- 3
      upper_n <- 3
      maximum_n <- floor(.Machine$integer.max / if (is_paired) 1 else 2)
      while (achieved_power(upper_n) < power && upper_n < maximum_n) upper_n <- min(upper_n * 2, maximum_n)
      if (achieved_power(upper_n) < power) {
        jmvcore::reject("Required sample size exceeds the supported range; review the equivalence bounds and target power.", code = "invalid_sample_size_parameters")
        return()
      }
      while (lower_n < upper_n) {
        midpoint <- floor((lower_n + upper_n) / 2)
        if (achieved_power(midpoint) >= power) upper_n <- midpoint else lower_n <- midpoint + 1
      }
      n_req <- lower_n
      table$setNote("method", "Normal-theory TOST planning approximation with standardized bounds: power is the probability of simultaneously rejecting both one-sided tests at the specified true difference. Paired effects use the standard deviation of paired differences; independent groups assume equal sizes and a common standard deviation. This is not an exact unknown-variance t-based power calculation.")
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
      
      self$results$text_tost$setContent("")
      self$results$text_tost$setVisible(FALSE)
    },
    
    .runProportions = function() {
      self$results$text_proportions$setContent("")
      self$results$text_proportions$setVisible(TRUE)
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
        jmvcore::reject("\u03b1 and target power must be strictly between 0 and 1.", code = "invalid_sample_size_parameters")
        return()
      }
      
      z_alpha <- stats::qnorm(1 - (if (alt == "two_sided") alpha / 2 else alpha))
      z_beta  <- stats::qnorm(power)
      
      if (prop_type == "two_prop") {
        if (is.null(p1) || is.null(p2) || p1 <= 0 || p1 >= 1 || p2 <= 0 || p2 >= 1 || p1 == p2) {
          jmvcore::reject("Proportions p\u2081 and p\u2082 must be between 0 and 1, and p\u2081 \u2260 p\u2082.", code = "invalid_sample_size_parameters")
          return()
        }
        alt_arg <- switch(alt, two_sided = "two.sided", one_sided = "one.sided", "two.sided")
        pwr_res <- tryCatch({
          stats::power.prop.test(p1 = p1, p2 = p2, sig.level = alpha, power = power, alternative = alt_arg)
        }, error = function(e) jmvcore::reject(conditionMessage(e), code = "sample_size_calculation_failed"))
        
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
        self$results$text_proportions$setContent("")
        self$results$text_proportions$setVisible(FALSE)
        
      } else if (prop_type == "one_prop") {
        if (is.null(p1) || is.null(p0) || p1 <= 0 || p1 >= 1 || p0 <= 0 || p0 >= 1 || p1 == p0) {
          jmvcore::reject("Proportions p\u2081 and null value p\u2080 must be between 0 and 1, and p\u2081 \u2260 p\u2080.", code = "invalid_sample_size_parameters")
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
        self$results$text_proportions$setContent("")
        self$results$text_proportions$setVisible(FALSE)
        
      } else if (prop_type == "mcnemar") {
        if (is.null(p12) || is.null(p21) || p12 <= 0 || p21 <= 0 || (p12 + p21) >= 1 || p12 == p21) {
          jmvcore::reject("Discordant proportions p\u2081\u2082 and p\u2082\u2081 must be positive with (p\u2081\u2082 + p\u2082\u2081) < 1, and p\u2081\u2082 \u2260 p\u2082\u2081.", code = "invalid_sample_size_parameters")
          return()
        }
        p_disc <- p12 + p21
        diff   <- p12 - p21
        n_raw  <- (z_alpha * sqrt(p_disc) + z_beta * sqrt(p_disc - diff^2))^2 / diff^2
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
        self$results$text_proportions$setContent("")
        self$results$text_proportions$setVisible(FALSE)
      }
    }
  )
)
