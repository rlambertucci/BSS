
correlationComparisonClass <- R6::R6Class(
  "correlationComparisonClass",
  inherit = correlationComparisonBase,
  private = list(
    
    .init = function() {
      info_html <- paste0(
        '<div style=\"font-size: 0.9em; line-height: 1.5;\">',
        '<ul>',
        '<li><b>Independent Comparison</b> &rarr; Tests whether two correlation coefficients ',
        'obtained from <b>two independent groups</b> differ. <i>Fisher (1921); Zou (2007).</i></li>',
        '<li><b>Dependent Overlapping</b> &rarr; Tests correlations sharing a variable (r<sub>12</sub> vs. r<sub>13</sub>). <i>Steiger (1980).</i></li>',
        '<li><b>Dependent Non-Overlapping</b> &rarr; Tests correlations without shared variables (r<sub>12</sub> vs. r<sub>34</sub>). <i>Steiger (1980).</i></li>',
        '<li><b>Multi-Group Homogeneity Test</b> &rarr; Tests whether correlations across <b>k &ge; 2 independent groups</b> are homogeneous via Cochran\'s Q / &chi;&sup2; test. <i>Cochran (1954); Fisher (1925).</i></li>',
        '<li><b>Partial Correlation Comparison</b> &rarr; Compares partial correlation r<sub>XY.Z</sub> between two independent groups controlling for covariates Z. <i>Steiger (1980).</i></li>',
        '<li><b>Jennrich Matrix Test</b> &rarr; Asymptotic &chi;&sup2; test for correlation matrix equality. <i>Jennrich (1970).</i></li>',
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
      mode <- self$options$mode
      if (private$.is_empty(mode)) return()
      
      table <- self$results$comparisonTable
      try(table$setNote("warning", NULL), silent = TRUE)
      try(table$setNote("diff_ci", NULL), silent = TRUE)
      
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
      n1 <- self$options$independent_n1
      n2 <- self$options$independent_n2
      r1 <- self$options$independent_r1
      r2 <- self$options$independent_r2
      
      table$setNote(
        "citation",
        "Note. Fisher's Z-transformation is used to compare two independent correlation coefficients. Confidence interval for difference based on Zou (2007). Citations: Fisher, R. A. (1921). Metron, 1, 3-32; Zou, G. Y. (2007). Psychol Methods, 12(4), 399-413."
      )
      
      if (private$.is_empty(n1) || private$.is_empty(n2) || private$.is_empty(r1) || private$.is_empty(r2)) return()
      
      if (n1 < 4 || n2 < 4) {
        table$setNote("warning", "Warning: Sample size in each group must be at least 4.")
        return()
      }
      if (abs(r1) > 1 || abs(r2) > 1) {
        table$setNote("warning", "Warning: Correlation coefficients must be bounded within [-1, 1].")
        return()
      }
      if (abs(r1) == 1 || abs(r2) == 1) {
        table$setNote("warning", "Warning: Fisher's Z-transformation is undefined for perfect correlations (|r| = 1).")
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
      
      table$setNote("diff_ci", sprintf("Difference (r1 - r2) = %.3f, 95%% CI: %.3f to %.3f (Zou, 2007).", diff_r, ci_lower, ci_upper))
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
        "Note. Williams' t-test (Hotelling-Williams modification) compares two dependent correlations sharing a common variable. Confidence interval based on Zou (2007). Citations: Williams, E. J. (1959). JRSS B, 21(2), 396-399; Steiger, J. H. (1980). Psychol Bull, 87(2), 244-251; Zou, G. Y. (2007). Psychol Methods, 12(4), 399-413."
      )
      
      if (private$.is_empty(n) || private$.is_empty(r12) || private$.is_empty(r13) || private$.is_empty(r23)) return()
      
      if (n < 4) {
        table$setNote("warning", "Warning: Sample size (N) must be at least 4 for Williams' t-test.")
        return()
      }
      
      if (abs(r12) > 1 || abs(r13) > 1 || abs(r23) > 1) {
        table$setNote("warning", "Warning: Correlation coefficients must be bounded within [-1, 1].")
        return()
      }
      
      if (abs(r12) == 1 || abs(r13) == 1) {
        table$setNote("warning", "Warning: Williams' test is undefined for perfect target correlations (|r12| = 1 or |r13| = 1).")
        return()
      }
      
      det_R <- 1 - r12^2 - r13^2 - r23^2 + 2 * r12 * r13 * r23
      if (det_R <= 1e-10) {
        table$setNote("warning", "Warning: The correlation matrix implied by r12, r13, and r23 is not positive-definite (det(R) <= 0). Input values are geometrically inconsistent.")
        return()
      }
      
      r_bar <- (r12 + r13) / 2
      denom <- 2 * ((n - 1) / (n - 3)) * det_R + (r_bar^2) * ((1 - r23)^3)
      if (denom <= 1e-10) {
        table$setNote("warning", "Warning: Denominator in Williams' t-statistic is near zero.")
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
      
      table$setNote("diff_ci", sprintf("Difference (r12 - r13) = %.3f, 95%% CI: %.3f to %.3f (Zou, 2007).", diff_r, ci_lower, ci_upper))
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
          label    = c("râ‚â‚‚ (Var 1 vs Var 2)", "râ‚â‚ƒ (Var 1 vs Var 3)"),
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
        "Note. Steiger's Z-test for non-overlapping dependent correlations (râ‚â‚‚ vs râ‚ƒâ‚„ measured in the same sample). Citations: Steiger, J. H. (1980). Psychol Bull, 87(2), 244-251; Raghunathan, T. E. et al. (1996). Psychol Methods, 1(2), 178-183."
      )
      
      if (private$.is_empty(n) || private$.is_empty(r12) || private$.is_empty(r34) ||
          private$.is_empty(r13) || private$.is_empty(r14) || private$.is_empty(r23) || private$.is_empty(r24)) return()
      
      if (n < 5 || abs(r12) >= 1 || abs(r34) >= 1) {
        table$setNote("warning", "Warning: Sample size must be >= 5 and correlations bounded within (-1, 1).")
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
        table$setNote("warning", "Warning: The implied 4x4 correlation matrix is not positive-definite. Input correlations are geometrically inconsistent.")
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
      
      table$setNote("diff_ci", sprintf("Difference (r12 - r34) = %.3f, 95%% CI: %.3f to %.3f (Zou, 2007).", diff_r, ci_lower, ci_upper))
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
          label    = c("râ‚â‚‚ (Var 1 vs Var 2)", "râ‚ƒâ‚„ (Var 3 vs Var 4)"),
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
      
      grps <- levels(df_clean[[vg]])
      if (length(grps) < 2) {
        ov_table$setNote("warning", "Grouping variable must have at least 2 distinct levels with valid data.")
        return()
      }
      
      z_vec <- numeric(0)
      w_vec <- numeric(0)
      r_vec <- numeric(0)
      n_vec <- integer(0)
      valid_grps <- character(0)
      
      for (g in grps) {
        sub_df <- df_clean[df_clean[[vg]] == g, ]
        n_g <- nrow(sub_df)
        if (n_g < 4) next
        r_g <- stats::cor(sub_df[[vx]], sub_df[[vy]])
        if (is.na(r_g) || abs(r_g) >= 1) next
        
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
      
      k <- length(valid_grps)
      if (k < 2) {
        ov_table$setNote("warning", "At least 2 groups with N >= 4 are required to compute homogeneity.")
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
        "Note. Cochran's Q (Ï‡Â²) test for correlation homogeneity across k independent groups based on Fisher's z-transformation. Citation: Cochran, W. G. (1954). Biometrics, 10(1), 101-129."
      )
      
      if (isTRUE(self$options$show_ellipse_plot)) {
        self$results$ellipsePlot$setState(list(
          df = df_clean[df_clean[[vg]] %in% valid_grps, ],
          x_col = vx,
          y_col = vy,
          grp_col = vg
        ))
      }
    },
    
    .runPartialComparison = function() {
      p_table <- self$results$partialTable
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
      if (length(lvls) < 2) {
        p_table$setNote("warning", "Grouping variable must have at least 2 distinct levels.")
        return()
      }
      
      g1 <- lvls[1]; g2 <- lvls[2]
      sub1 <- df_clean[df_clean[[vg]] == g1, ]
      sub2 <- df_clean[df_clean[[vg]] == g2, ]
      
      k_cov <- if (private$.is_defined(covs)) length(covs) else 0
      n1 <- nrow(sub1); n2 <- nrow(sub2)
      
      if (n1 <= (k_cov + 3) || n2 <= (k_cov + 3)) {
        p_table$setNote("warning", paste0("Sample sizes (N1=", n1, ", N2=", n2, ") must exceed k_cov + 3 = ", k_cov + 3, "."))
        return()
      }
      
      if (k_cov == 0) {
        r_p1 <- stats::cor(sub1[[vx]], sub1[[vy]])
        r_p2 <- stats::cor(sub2[[vx]], sub2[[vy]])
      } else {
        f_x <- stats::as.formula(paste(vx, "~", paste(covs, collapse = " + ")))
        f_y <- stats::as.formula(paste(vy, "~", paste(covs, collapse = " + ")))
        rx1 <- stats::residuals(stats::lm(f_x, data = sub1))
        ry1 <- stats::residuals(stats::lm(f_y, data = sub1))
        r_p1 <- stats::cor(rx1, ry1)
        rx2 <- stats::residuals(stats::lm(f_x, data = sub2))
        ry2 <- stats::residuals(stats::lm(f_y, data = sub2))
        r_p2 <- stats::cor(rx2, ry2)
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
        paste0("Note. Comparison of partial correlations controlling for ", k_cov, " covariate(s) (Z) using Fisher's Z-test with Var(Z) = 1/(N - k - 3). Citation: Steiger (1980).")
      )
      
      self$results$ellipsePlot$setState(list(
        df = df_clean[df_clean[[vg]] %in% c(g1, g2), ],
        x_col = vx,
        y_col = vy,
        grp_col = vg
      ))
    },
    
    .runJennrich = function() {
      table <- self$results$jennrichTable
      data <- self$data
      if (is.null(data)) return()
      
      vars <- self$options$vars_jennrich
      groupVar <- self$options$group_jennrich
      
      if (private$.is_empty(vars) || length(vars) < 2 || private$.is_empty(groupVar)) return()
      if (!all(vars %in% names(data)) || !(groupVar %in% names(data))) return()
      
      clean_data <- stats::na.omit(data[, c(vars, groupVar)])
      clean_data[[groupVar]] <- factor(clean_data[[groupVar]])
      lvls <- levels(clean_data[[groupVar]])
      
      if (length(lvls) < 2) {
        table$setNote("warning", "Grouping variable must have at least 2 distinct levels.")
        return()
      }
      
      g1_clean <- clean_data[clean_data[[groupVar]] == lvls[1], vars]
      g2_clean <- clean_data[clean_data[[groupVar]] == lvls[2], vars]
      
      n1 <- nrow(g1_clean); n2 <- nrow(g2_clean); p <- length(vars)
      if (n1 <= p || n2 <= p) {
        table$setNote("warning", paste0("Sample sizes in both groups (N1 = ", n1, ", N2 = ", n2, ") must exceed the number of variables (p = ", p, ")."))
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
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.25, color = "#2c3e50", linewidth = 0.9) +
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
      if (is.null(state) || is.null(state$df) || nrow(state$df) == 0) return(FALSE)
      
      df <- state$df
      x_col <- state$x_col
      y_col <- state$y_col
      grp_col <- state$grp_col
      
      p <- ggplot2::ggplot(df, ggplot2::aes_string(x = x_col, y = y_col, color = grp_col, fill = grp_col)) +
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
