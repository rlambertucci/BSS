
correlationComparisonClass <- R6::R6Class(
  "correlationComparisonClass",
  inherit = correlationComparisonBase,
  private = list(
    
    .update_help = function() {
      mode <- self$options$mode
      if (length(mode) != 1) mode <- ""
      configured <- switch(mode,
        independent = if (identical(self$options$independent_input_type, "raw")) length(c(self$options$indep_var_x, self$options$indep_var_y, self$options$indep_group)) > 0 else TRUE,
        dependent = TRUE,
        dep_nonoverlapping = TRUE,
        multigroup = length(c(self$options$multi_var_x, self$options$multi_var_y, self$options$multi_group)) > 0,
        partial_comparison = length(c(self$options$partial_var_x, self$options$partial_var_y, self$options$partial_group)) > 0,
        jennrich = length(self$options$vars_jennrich) > 0,
        FALSE
      )
      if (!is.null(self$results$comparisonInfo)) self$results$comparisonInfo$setVisible(!configured)
    },

    .init = function() {
      private$.update_help()
      info_html <- paste0(
        '<div>',
        '<ul>',
        '<li><b>Independent Comparison</b> \u2192 Tests whether two correlation coefficients ',
        'obtained from <b>two independent groups</b> differ. <i>Fisher (1921, 1925); Zou (2007).</i></li>',
        '<li><b>Dependent Overlapping</b> \u2192 Tests correlations sharing a variable (r<sub>12</sub> vs. r<sub>13</sub>). <i>Williams (1959); Zou (2007).</i></li>',
        '<li><b>Dependent Non-Overlapping</b> \u2192 Tests correlations without shared variables (r<sub>12</sub> vs. r<sub>34</sub>). <i>Dunn and Clark (1969); Raghunathan et al. (1996); Zou (2007).</i></li>',
        '<li><b>Multi-Group Homogeneity Test</b> \u2192 Tests whether correlations across <b>k \u2265 2 independent groups</b> are homogeneous via Cochran\'s Q / \u03c7\u00b2 test. <i>Cochran (1954); Fisher (1925).</i></li>',
        '<li><b>Partial Correlation Comparison</b> \u2192 Compares partial correlation r<sub>XY.Z</sub> between two independent groups controlling for covariates Z. <i>Steiger (1980).</i></li>',
        '<li><b>Jennrich Matrix Test</b> \u2192 Asymptotic \u03c7\u00b2 test for correlation matrix equality. <i>Jennrich (1970).</i></li>',
        '</ul>',
        '</div>'
      )
      self$results$comparisonInfo$setContent(info_html)
    },
    
    .is_defined = function(x) {
      !is.null(x) && length(x) > 0 && all(!is.na(x)) && all(nzchar(as.character(x)))
    },

    .is_empty = function(x) {
      if (is.null(x) || length(x) == 0) return(TRUE)
      if (length(x) == 1 && is.na(x)) return(TRUE)
      FALSE
    },
    
    .run = function() {
      private$.update_help()
      mode <- self$options$mode
      if (private$.is_empty(mode)) return()
      
      table <- self$results$comparisonTable
      self$results$ciPlot$setState(NULL)
      table$setNote("warning", NULL)
      table$setNote("diff_ci", NULL)
      
      if ('statistic' %in% names(table$columns)) {
        table$getColumn('statistic')$setTitle("Statistic")
      }
      
      if (mode == "independent") {
        private$.runIndependent()
      } else if (mode == "dependent") {
        private$.runDependent()
      } else if (mode == "dep_nonoverlapping") {
        private$.runDepNonoverlapping()
      } else if (mode == "multigroup") {
        private$.runMultiGroup()
      } else if (mode == "partial_comparison") {
        private$.runPartialComparison()
      } else if (mode == "jennrich") {
        private$.runJennrich()
      }
    },
    
    .runIndependent = function() {
      table <- self$results$comparisonTable
      input_type <- self$options$independent_input_type
      table$setNote(
        "citation",
        "Fisher's r-to-Z transformation and Z-test compare two independent correlation coefficients. Confidence interval for the difference based on Zou's method."
      )
      if (input_type == "raw") {
        data <- self$data
        vx <- self$options$indep_var_x
        vy <- self$options$indep_var_y
        vg <- self$options$indep_group
        if (is.null(data) || is.null(vx) || is.null(vy) || is.null(vg)) return()
        if (!(vx %in% names(data)) || !(vy %in% names(data)) || !(vg %in% names(data))) return()
        x_vec <- as.numeric(data[[vx]])
        y_vec <- as.numeric(data[[vy]])
        g_fac <- as.factor(data[[vg]])
        valid <- !is.na(x_vec) & !is.na(y_vec) & !is.na(g_fac)
        x_vec <- x_vec[valid]; y_vec <- y_vec[valid]; g_fac <- droplevels(g_fac[valid])
        lvls <- levels(g_fac)
        if (length(lvls) != 2) {
          jmvcore::reject("Warning: Grouping variable must contain exactly 2 levels for independent comparison.", code = "invalid_correlation_comparison")
          return()
        }
        idx1 <- g_fac == lvls[1]
        idx2 <- g_fac == lvls[2]
        n1 <- sum(idx1)
        n2 <- sum(idx2)
        if (n1 < 4 || n2 < 4) {
          jmvcore::reject("Warning: Sample size in each group must be at least 4.", code = "invalid_correlation_comparison")
          return()
        }
        r1 <- stats::cor(x_vec[idx1], y_vec[idx1])
        r2 <- stats::cor(x_vec[idx2], y_vec[idx2])
        table$setNote("raw_data_note", paste0("Estimated correlations from raw data: ", lvls[1], " (n = ", n1, ", r = ", round(r1, 3), ") vs ", lvls[2], " (n = ", n2, ", r = ", round(r2, 3), ")."))
      } else {
        n1 <- self$options$independent_n1
        n2 <- self$options$independent_n2
        r1 <- self$options$independent_r1
        r2 <- self$options$independent_r2
        if (private$.is_empty(n1) || private$.is_empty(n2) || private$.is_empty(r1) || private$.is_empty(r2)) return()
      }
      if (n1 < 4 || n2 < 4) {
        jmvcore::reject("Warning: Sample size in each group must be at least 4.", code = "invalid_correlation_comparison")
        return()
      }
      if (abs(r1) > 1 || abs(r2) > 1) {
        jmvcore::reject("Warning: Correlation coefficients must be bounded within [-1, 1].", code = "invalid_correlation_comparison")
        return()
      }
      if (abs(r1) == 1 || abs(r2) == 1) {
        jmvcore::reject("Warning: Fisher's Z-transformation is undefined for perfect correlations (|r| = 1).", code = "invalid_correlation_comparison")
        return()
      }
      z1 <- atanh(r1)
      z2 <- atanh(r2)
      se_diff <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
      
      z_stat  <- (z1 - z2) / se_diff
      p_value <- 2 * stats::pnorm(abs(z_stat), lower.tail = FALSE)
      
      diff_r <- r1 - r2
      crit <- stats::qnorm(0.975)
      se1  <- 1 / sqrt(n1 - 3)
      se2  <- 1 / sqrt(n2 - 3)
      l1   <- tanh(z1 - crit * se1)
      u1   <- tanh(z1 + crit * se1)
      l2   <- tanh(z2 - crit * se2)
      u2   <- tanh(z2 + crit * se2)
      
      ci_lower <- diff_r - sqrt((r1 - l1)^2 + (u2 - r2)^2)
      ci_upper <- diff_r + sqrt((u1 - r1)^2 + (r2 - l2)^2)
      
      table$setNote("diff_ci", sprintf("Difference (r1 - r2) = %.3f, 95%% CI: %.3f to %.3f.", diff_r, ci_lower, ci_upper))
      if ('statistic' %in% names(table$columns)) table$getColumn('statistic')$setTitle("Z")
      
      table$setRow(rowNo = 1, values = list(
        comparisonType = "Independent (2 Groups)",
        n_group1  = as.integer(n1),
        n_group2  = as.integer(n2),
        r_group1  = as.numeric(r1),
        r_group2  = as.numeric(r2),
        n_dep     = NA,
        r12_dep   = NA,
        r13_dep   = NA,
        r23_dep   = NA,
        r34_dep   = NA,
        statistic = as.numeric(z_stat),
        p_value   = as.numeric(p_value)
      ))
      
      if (isTRUE(self$options$show_ci_plot)) {
        plot_df <- data.frame(
          label    = c("Group 1", "Group 2"),
          r        = c(r1, r2),
          ci_lower = c(l1, l2),
          ci_upper = c(u1, u2),
          stringsAsFactors = FALSE
        )
        self$results$ciPlot$setState(plot_df)
      }
    },
    
    .runDependent = function() {
      table <- self$results$comparisonTable
      n   <- self$options$dependent_n
      r12 <- self$options$dependent_r12
      r13 <- self$options$dependent_r13
      r23 <- self$options$dependent_r23
      
      table$setNote(
        "citation",
        "Williams' t-test (Hotelling-Williams modification) compares two dependent correlations sharing a common variable. Confidence interval for the difference based on Zou's method."
      )
      
      if (private$.is_empty(n) || private$.is_empty(r12) || private$.is_empty(r13) || private$.is_empty(r23)) return()
      
      if (n < 4) {
        jmvcore::reject("Warning: Sample size (N) must be at least 4 for Williams' t-test.", code = "invalid_correlation_comparison")
        return()
      }
      
      if (abs(r12) > 1 || abs(r13) > 1 || abs(r23) > 1) {
        jmvcore::reject("Warning: Correlation coefficients must be bounded within [-1, 1].", code = "invalid_correlation_comparison")
        return()
      }
      
      if (abs(r12) == 1 || abs(r13) == 1) {
        jmvcore::reject("Warning: Williams' test is undefined for perfect target correlations (|r12| = 1 or |r13| = 1).", code = "invalid_correlation_comparison")
        return()
      }
      
      det_R <- 1 - r12^2 - r13^2 - r23^2 + 2 * r12 * r13 * r23
      if (det_R <= 1e-10) {
        jmvcore::reject("Warning: The correlation matrix implied by r12, r13, and r23 is not positive-definite (det(R) <= 0). Input values are geometrically inconsistent.", code = "invalid_correlation_comparison")
        return()
      }
      
      r_bar <- (r12 + r13) / 2
      denom <- 2 * ((n - 1) / (n - 3)) * det_R + (r_bar^2) * ((1 - r23)^3)
      if (denom <= 1e-10) {
        jmvcore::reject("Warning: Denominator in Williams' t-statistic is near zero.", code = "invalid_correlation_comparison")
        return()
      }
      
      t_stat  <- (r12 - r13) * sqrt(((n - 1) * (1 + r23)) / denom)
      df      <- n - 3
      p_value <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
      
      diff_r <- r12 - r13
      crit   <- stats::qnorm(0.975)
      se_dep <- 1 / sqrt(n - 3)
      
      z12 <- atanh(r12)
      z13 <- atanh(r13)
      
      l1 <- tanh(z12 - crit * se_dep)
      u1 <- tanh(z12 + crit * se_dep)
      l2 <- tanh(z13 - crit * se_dep)
      u2 <- tanh(z13 + crit * se_dep)
      
      num_cov <- (r23 * (1 - r12^2 - r13^2) - 0.5 * r12 * r13 * (1 - r12^2 - r13^2 - r23^2))
      den_cov <- (1 - r12^2) * (1 - r13^2)
      c_cov   <- if (abs(den_cov) > 1e-10) num_cov / den_cov else 0
      
      rad_l <- max(0, (r12 - l1)^2 + (u2 - r13)^2 - 2 * c_cov * (r12 - l1) * (u2 - r13))
      rad_u <- max(0, (u1 - r12)^2 + (r13 - l2)^2 - 2 * c_cov * (u1 - r12) * (r13 - l2))
      
      ci_lower <- diff_r - sqrt(rad_l)
      ci_upper <- diff_r + sqrt(rad_u)
      
      table$setNote("diff_ci", sprintf("Difference (r12 - r13) = %.3f, 95%% CI: %.3f to %.3f.", diff_r, ci_lower, ci_upper))
      if ('statistic' %in% names(table$columns)) {
        table$getColumn('statistic')$setTitle(paste0("t(", df, ")"))
      }
      
      table$setRow(rowNo = 1, values = list(
        comparisonType = "Dependent (Overlapping)",
        n_group1  = NA,
        n_group2  = NA,
        r_group1  = NA,
        r_group2  = NA,
        n_dep     = as.integer(n),
        r12_dep   = as.numeric(r12),
        r13_dep   = as.numeric(r13),
        r23_dep   = as.numeric(r23),
        r34_dep   = NA,
        statistic = as.numeric(t_stat),
        p_value   = as.numeric(p_value)
      ))
      
      if (isTRUE(self$options$show_ci_plot)) {
        plot_df <- data.frame(
          label    = c("r\u2081\u2082 (Var 1 vs Var 2)", "r\u2081\u2083 (Var 1 vs Var 3)"),
          r        = c(r12, r13),
          ci_lower = c(l1, l2),
          ci_upper = c(u1, u2),
          stringsAsFactors = FALSE
        )
        self$results$ciPlot$setState(plot_df)
      }
    },
    
    .runDepNonoverlapping = function() {
      table <- self$results$comparisonTable
      n   <- self$options$dep_non_n
      r12 <- self$options$dep_non_r12
      r34 <- self$options$dep_non_r34
      r13 <- self$options$dep_non_r13
      r14 <- self$options$dep_non_r14
      r23 <- self$options$dep_non_r23
      r24 <- self$options$dep_non_r24
      
      table$setNote(
        "citation",
        "Dunn and Clark's Z-test, algebraically equivalent to the statistic of Raghunathan, Rosenthal, and Rubin, compares non-overlapping dependent correlations (r12 vs r34 measured in the same sample). Confidence interval for the difference based on Zou's method."
      )
      
      if (private$.is_empty(n) || private$.is_empty(r12) || private$.is_empty(r34) ||
          private$.is_empty(r13) || private$.is_empty(r14) || private$.is_empty(r23) || private$.is_empty(r24)) return()
      
      if (n < 5 || abs(r12) >= 1 || abs(r34) >= 1) {
        jmvcore::reject("Warning: Sample size must be >= 5 and correlations bounded within (-1, 1).", code = "invalid_correlation_comparison")
        return()
      }
      
      R4 <- matrix(c(
        1,   r12, r13, r14,
        r12, 1,   r23, r24,
        r13, r23, 1,   r34,
        r14, r24, r34, 1
      ), nrow = 4, ncol = 4)
      
      ev <- eigen(R4, symmetric = TRUE, only.values = TRUE)$values
      if (any(ev <= 1e-8)) {
        jmvcore::reject("Warning: The implied 4x4 correlation matrix is not positive-definite. Input correlations are geometrically inconsistent.", code = "invalid_correlation_comparison")
        return()
      }
      
      z12 <- atanh(r12)
      z34 <- atanh(r34)
      
      s12_34 <- 0.5 * (
        (r13 - r12 * r23) * (r24 - r23 * r34) +
        (r14 - r13 * r34) * (r23 - r12 * r13) +
        (r13 - r14 * r34) * (r24 - r12 * r14) +
        (r14 - r12 * r24) * (r23 - r24 * r34)
      )
      
      cov_z <- s12_34 / ((1 - r12^2) * (1 - r34^2))
      var_diff <- (2 - 2 * cov_z) / (n - 3)
      if (var_diff <= 1e-10) var_diff <- 1e-10
      
      z_stat  <- (z12 - z34) / sqrt(var_diff)
      p_value <- 2 * stats::pnorm(abs(z_stat), lower.tail = FALSE)
      
      diff_r <- r12 - r34
      crit   <- stats::qnorm(0.975)
      se_dep <- 1 / sqrt(n - 3)
      l1     <- tanh(z12 - crit * se_dep)
      u1     <- tanh(z12 + crit * se_dep)
      l2     <- tanh(z34 - crit * se_dep)
      u2     <- tanh(z34 + crit * se_dep)
      
      rad_l <- max(0, (r12 - l1)^2 + (u2 - r34)^2 - 2 * cov_z * (r12 - l1) * (u2 - r34))
      rad_u <- max(0, (u1 - r12)^2 + (r34 - l2)^2 - 2 * cov_z * (u1 - r12) * (r34 - l2))
      ci_lower <- diff_r - sqrt(rad_l)
      ci_upper <- diff_r + sqrt(rad_u)
      
      table$setNote("diff_ci", sprintf("Difference (r12 - r34) = %.3f, 95%% CI: %.3f to %.3f.", diff_r, ci_lower, ci_upper))
      if ('statistic' %in% names(table$columns)) table$getColumn('statistic')$setTitle("Z")
      
      table$setRow(rowNo = 1, values = list(
        comparisonType = "Dependent (Non-Overlapping)",
        n_group1  = NA,
        n_group2  = NA,
        r_group1  = NA,
        r_group2  = NA,
        n_dep     = as.integer(n),
        r12_dep   = as.numeric(r12),
        r13_dep   = NA,
        r23_dep   = NA,
        r34_dep   = as.numeric(r34),
        statistic = as.numeric(z_stat),
        p_value   = as.numeric(p_value)
      ))
      
      if (isTRUE(self$options$show_ci_plot)) {
        plot_df <- data.frame(
          label    = c("r\u2081\u2082 (Var 1 vs Var 2)", "r\u2083\u2084 (Var 3 vs Var 4)"),
          r        = c(r12, r34),
          ci_lower = c(l1, l2),
          ci_upper = c(u1, u2),
          stringsAsFactors = FALSE
        )
        self$results$ciPlot$setState(plot_df)
      }
    },
    
    .runMultiGroup = function() {
      mg_table <- self$results$multiGroupTable
      ov_table <- self$results$multiGroupOverallTable
      ov_table$setNote("excluded_groups", NULL)
      ov_table$setNote("warning", NULL)
      if (mg_table$rowCount > 0) mg_table$deleteRows()
      
      data <- self$data
      if (is.null(data)) return()
      
      vx <- self$options$multi_var_x
      vy <- self$options$multi_var_y
      vg <- self$options$multi_group
      
      if (private$.is_empty(vx) || private$.is_empty(vy) || private$.is_empty(vg)) return()
      if (!(vx %in% names(data)) || !(vy %in% names(data)) || !(vg %in% names(data))) return()
      
      df_clean <- stats::na.omit(data[, c(vx, vy, vg)])
      df_clean[[vx]] <- as.numeric(df_clean[[vx]])
      df_clean[[vy]] <- as.numeric(df_clean[[vy]])
      df_clean[[vg]] <- factor(df_clean[[vg]])
      
      grps <- levels(droplevels(factor(data[[vg]])))
      if (length(grps) < 2) {
        jmvcore::reject("Grouping variable must have at least 2 distinct levels with valid data.", code = "invalid_correlation_comparison")
        return()
      }
      
      z_vec <- numeric(0)
      w_vec <- numeric(0)
      r_vec <- numeric(0)
      n_vec <- integer(0)
      valid_grps <- character(0)
      excluded_groups <- character(0)
      
      for (g in grps) {
        sub_df <- df_clean[df_clean[[vg]] == g, ]
        n_g <- nrow(sub_df)
        reason <- NULL
        if (n_g < 4) {
          reason <- paste0("N = ", n_g, " complete cases; at least 4 required")
        } else if (!is.finite(stats::var(sub_df[[vx]])) || !is.finite(stats::var(sub_df[[vy]])) || stats::var(sub_df[[vx]]) == 0 || stats::var(sub_df[[vy]]) == 0) {
          reason <- "zero or non-finite variance; correlation undefined"
        }
        r_g <- if (is.null(reason)) stats::cor(sub_df[[vx]], sub_df[[vy]]) else NA_real_
        if (is.null(reason) && (!is.finite(r_g) || abs(r_g) >= 1)) reason <- "non-finite or perfect correlation; Fisher transformation unavailable"
        if (!is.null(reason)) {
          excluded_groups <- c(excluded_groups, paste0(g, " (", reason, ")"))
          next
        }
        
        z_g <- atanh(r_g)
        w_g <- n_g - 3
        se_z <- 1 / sqrt(w_g)
        ci_lo <- tanh(z_g - 1.96 * se_z)
        ci_hi <- tanh(z_g + 1.96 * se_z)
        
        z_vec <- c(z_vec, z_g)
        w_vec <- c(w_vec, w_g)
        r_vec <- c(r_vec, r_g)
        n_vec <- c(n_vec, n_g)
        valid_grps <- c(valid_grps, g)
        
        mg_table$addRow(rowKey = g, values = list(
          group    = as.character(g),
          n        = as.integer(n_g),
          r        = as.numeric(r_g),
          z        = as.numeric(z_g),
          ci_lower = as.numeric(ci_lo),
          ci_upper = as.numeric(ci_hi)
        ))
      }
      
      if (length(excluded_groups) > 0) ov_table$setNote("excluded_groups", paste0("Excluded groups: ", paste(excluded_groups, collapse = "; "), "."))
      k <- length(valid_grps)
      if (k < 2) {
        jmvcore::reject("At least 2 groups with N >= 4 are required to compute homogeneity.", code = "invalid_correlation_comparison")
        return()
      }
      
      z_bar <- sum(w_vec * z_vec) / sum(w_vec)
      q_stat <- sum(w_vec * (z_vec - z_bar)^2)
      df_q <- k - 1
      p_q <- stats::pchisq(q_stat, df = df_q, lower.tail = FALSE)
      pooled_r <- tanh(z_bar)
      
      ov_table$setRow(rowNo = 1, values = list(
        k_groups = as.integer(k),
        pooled_r = as.numeric(pooled_r),
        q_stat   = as.numeric(q_stat),
        df       = as.integer(df_q),
        p_value  = as.numeric(p_q)
      ))
      
      ov_table$setNote(
        "citation",
        "Cochran's Q (\u03c7\u00b2) test for correlation homogeneity across k independent groups based on Fisher's z-transformation."
      )
      
      if (isTRUE(self$options$show_ellipse_plot)) {
        self$results$ellipsePlot$setState(list(
          rows = match(rownames(df_clean[df_clean[[vg]] %in% valid_grps, , drop = FALSE]), rownames(self$data)),
          x_col = vx,
          y_col = vy,
          grp_col = vg
        ))
      }
    },
    
    .runPartialComparison = function() {
      p_table <- self$results$partialTable
      p_table$deleteRows()
      p_table$addRow(rowKey = 1, values = list())
      p_table$setNote("warning", NULL)
      p_table$setNote("citation", NULL)
      self$results$ellipsePlot$setState(NULL)
      data <- self$data
      if (is.null(data)) return()
      
      vx <- self$options$partial_var_x
      vy <- self$options$partial_var_y
      covs <- self$options$partial_covs
      vg <- self$options$partial_group
      
      if (private$.is_empty(vx) || private$.is_empty(vy) || private$.is_empty(vg)) return()
      cols_needed <- c(vx, vy, vg)
      if (private$.is_defined(covs)) cols_needed <- c(cols_needed, covs)
      if (!all(cols_needed %in% names(data))) return()
      
      df_clean <- stats::na.omit(data[, cols_needed])
      df_clean[[vg]] <- factor(df_clean[[vg]])
      lvls <- levels(df_clean[[vg]])
      if (length(lvls) != 2) {
        jmvcore::reject("Grouping variable must have exactly 2 distinct levels with complete data.", code = "invalid_correlation_comparison")
        return()
      }
      
      g1 <- lvls[1]; g2 <- lvls[2]
      sub1 <- df_clean[df_clean[[vg]] == g1, ]
      sub2 <- df_clean[df_clean[[vg]] == g2, ]
      
      k_cov <- if (private$.is_defined(covs)) length(covs) else 0
      n1 <- nrow(sub1); n2 <- nrow(sub2)
      
      if (n1 <= (k_cov + 3) || n2 <= (k_cov + 3)) {
        jmvcore::reject(paste0("Sample sizes (N1=", n1, ", N2=", n2, ") must exceed k_cov + 3 = ", k_cov + 3, "."), code = "invalid_correlation_comparison")
        return()
      }

      numeric_columns <- c(vx, vy, if (k_cov > 0) covs)
      if (!all(vapply(df_clean[, unique(numeric_columns), drop = FALSE], function(column) is.numeric(column) && !is.complex(column) && is.null(dim(column)) && all(is.finite(column)), logical(1)))) {
        jmvcore::reject("Correlation variables and controls must contain finite real numeric values in the complete cases.", code = "invalid_correlation_comparison")
      }

      for (group_data in list(sub1, sub2)) {
        control_design <- matrix(1, nrow = nrow(group_data), ncol = 1)
        if (k_cov > 0) control_design <- cbind(control_design, as.matrix(group_data[, covs, drop = FALSE]))
        if (qr(control_design)$rank != k_cov + 1) {
          jmvcore::reject("Control variables must be linearly independent within each group; remove constant or redundant controls.", code = "invalid_correlation_comparison")
        }
        correlation_design <- scale(cbind(group_data[[vx]], group_data[[vy]]))
        if (any(!is.finite(correlation_design)) || qr(cbind(control_design, correlation_design), tol = .Machine$double.eps * nrow(group_data))$rank != k_cov + 3) {
          jmvcore::reject("Variables must have nonzero residual variation and a non-perfect partial correlation within each group.", code = "invalid_correlation_comparison")
        }
      }
      
      if (k_cov == 0) {
        r_p1 <- stats::cor(sub1[[vx]], sub1[[vy]])
        r_p2 <- stats::cor(sub2[[vx]], sub2[[vy]])
      } else {
        quote_names <- function(names) vapply(names, function(name) paste(deparse(as.name(name), backtick = TRUE), collapse = ""), character(1))
        f_x <- stats::as.formula(paste(quote_names(vx), "~", paste(quote_names(covs), collapse = " + ")))
        f_y <- stats::as.formula(paste(quote_names(vy), "~", paste(quote_names(covs), collapse = " + ")))
        rx1 <- stats::residuals(stats::lm(f_x, data = sub1))
        ry1 <- stats::residuals(stats::lm(f_y, data = sub1))
        r_p1 <- stats::cor(rx1, ry1)
        rx2 <- stats::residuals(stats::lm(f_x, data = sub2))
        ry2 <- stats::residuals(stats::lm(f_y, data = sub2))
        r_p2 <- stats::cor(rx2, ry2)
      }

      if (!is.finite(r_p1) || !is.finite(r_p2) || abs(r_p1) >= 1 || abs(r_p2) >= 1) {
        jmvcore::reject("Fisher's Z-transformation requires finite partial correlations strictly between -1 and 1 in both groups.", code = "invalid_correlation_comparison")
      }
      
      z1 <- atanh(r_p1); z2 <- atanh(r_p2)
      var1 <- 1 / (n1 - k_cov - 3)
      var2 <- 1 / (n2 - k_cov - 3)
      se_diff <- sqrt(var1 + var2)
      z_stat <- (z1 - z2) / se_diff
      p_val <- 2 * stats::pnorm(-abs(z_stat))
      
      p_table$setRow(rowNo = 1, values = list(
        group1        = as.character(g1),
        group2        = as.character(g2),
        n1            = as.integer(n1),
        n2            = as.integer(n2),
        r_partial_g1  = as.numeric(r_p1),
        r_partial_g2  = as.numeric(r_p2),
        z_stat        = as.numeric(z_stat),
        p_value       = as.numeric(p_val)
      ))
      
      p_table$setNote(
        "citation",
        paste0("Comparison of partial correlations controlling for ", k_cov, " covariate(s) (Z) using Fisher's Z-test with Var(Z) = 1/(N - k - 3).")
      )
      
      self$results$ellipsePlot$setState(list(
        rows = match(rownames(df_clean[df_clean[[vg]] %in% c(g1, g2), , drop = FALSE]), rownames(self$data)),
        x_col = vx,
        y_col = vy,
        grp_col = vg
      ))
    },
    
    .runJennrich = function() {
      table <- self$results$jennrichTable
      table$setNote("warning", NULL)
      data <- self$data
      if (is.null(data)) return()
      
      vars <- self$options$vars_jennrich
      groupVar <- self$options$group_jennrich
      
      if (private$.is_empty(vars) || length(vars) < 2 || private$.is_empty(groupVar)) return()
      if (!all(vars %in% names(data)) || !(groupVar %in% names(data))) return()
      
      clean_data <- stats::na.omit(data[, c(vars, groupVar)])
      clean_data[[groupVar]] <- factor(clean_data[[groupVar]])
      lvls <- levels(clean_data[[groupVar]])
      
      if (length(lvls) != 2) {
        jmvcore::reject("Grouping variable must have exactly 2 distinct levels with complete data.", code = "invalid_correlation_comparison")
        return()
      }
      
      g1_clean <- clean_data[clean_data[[groupVar]] == lvls[1], vars]
      g2_clean <- clean_data[clean_data[[groupVar]] == lvls[2], vars]
      
      n1 <- nrow(g1_clean); n2 <- nrow(g2_clean); p <- length(vars)
      if (n1 <= p || n2 <= p) {
        jmvcore::reject(paste0("Sample sizes in both groups (N1 = ", n1, ", N2 = ", n2, ") must exceed the number of variables (p = ", p, ")."), code = "invalid_correlation_comparison")
        return()
      }
      
      R1 <- stats::cor(g1_clean); R2 <- stats::cor(g2_clean)
      c_factor <- (n1 * n2) / (n1 + n2)
      R_bar    <- (n1 * R1 + n2 * R2) / (n1 + n2)
      R_bar_inv <- tryCatch(solve(R_bar), error = function(e) MASS::ginv(R_bar))
      
      S <- R_bar_inv %*% (R1 - R2)
      W <- R_bar * R_bar_inv
      diag_S <- diag(S)
      V <- diag(1, p) + W
      V_inv <- tryCatch(solve(V), error = function(e) MASS::ginv(V))
      
      chisq_val <- 0.5 * c_factor * sum(diag(S %*% S)) - c_factor * as.numeric(t(diag_S) %*% V_inv %*% diag_S)
      df_val    <- p * (p - 1) / 2
      p_val     <- 1 - stats::pchisq(max(0, chisq_val), df = df_val)
      
      table$setRow(rowNo = 1, values = list(
        group1  = lvls[1],
        group2  = lvls[2],
        n1      = as.integer(n1),
        n2      = as.integer(n2),
        numVars = as.integer(p),
        chisq   = as.numeric(chisq_val),
        df      = as.integer(df_val),
        p_value = as.numeric(p_val)
      ))
    },
    
    .renderCIPlot = function(image, ggtheme, theme, ...) {
      plot_df <- image$state
      if (is.null(plot_df) || nrow(plot_df) == 0) return(FALSE)
      
      plot_df$label <- factor(plot_df$label, levels = rev(plot_df$label))
      
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(y = label, x = r)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.7) +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), orientation = "y", width = 0.25, color = "#2c3e50", linewidth = 0.9) +
        ggplot2::geom_point(color = "#e67e22", size = 3.5) +
        ggplot2::scale_x_continuous(limits = c(-1, 1)) +
        ggplot2::labs(
          x = "Correlation Coefficient (r) & 95% Confidence Interval",
          y = "",
          title = "Comparison of Correlation Coefficients"
        ) +
        ggtheme +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 10, face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
        )
      
      print(p)
      TRUE
    },
    
    .renderEllipsePlot = function(image, ggtheme, ...) {
      state <- image$state
      if (is.null(state) || length(state$rows) == 0 || is.null(self$data)) return(FALSE)
      
      df <- self$data[state$rows, c(state$x_col, state$y_col, state$grp_col), drop = FALSE]
      df[[state$x_col]] <- as.numeric(df[[state$x_col]])
      df[[state$y_col]] <- as.numeric(df[[state$y_col]])
      df[[state$grp_col]] <- factor(df[[state$grp_col]])
      x_col <- state$x_col
      y_col <- state$y_col
      grp_col <- state$grp_col
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[[grp_col]], fill = .data[[grp_col]])) +
        ggplot2::geom_point(alpha = 0.65, size = 2) +
        ggplot2::stat_ellipse(type = "norm", level = 0.95, linewidth = 1.1, geom = "polygon", alpha = 0.12) +
        ggplot2::stat_ellipse(type = "norm", level = 0.95, linewidth = 1.1) +
        ggplot2::labs(
          title = paste("Bivariate Scatterplot & 95% Confidence Ellipses:", x_col, "vs", y_col),
          subtitle = paste("Grouped by", grp_col),
          x = x_col,
          y = y_col,
          color = grp_col,
          fill = grp_col
        ) +
        ggtheme +
        ggplot2::theme(
          legend.position = "right",
          plot.title = ggplot2::element_text(face = "bold", size = 12)
        )
      
      print(p)
      TRUE
    }
  )
)
