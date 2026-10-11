# Correlation Comparison Implementation

correlationComparisonClass <- R6::R6Class(
  "correlationComparisonClass",
  inherit = correlationComparisonBase,
  private = list(
    
    .init = function() {
      info_html <- paste0(
        '<div style=\"font-size: 0.9em; line-height: 1.5;\">',
        '<ul>',
        '<li><b>Independent Comparison</b> &rarr; Tests whether two correlation coefficients ',
        'obtained from <b>two independent groups</b> are significantly different from each other. ',
        'Uses Fisher\'s Z-transformation to compare r<sub>1</sub> in Group 1 vs. r<sub>2</sub> in Group 2. ',
        '<i>Fisher (1921); Zou (2007).</i></li>',
        '<li><b>Dependent Overlapping</b> &rarr; Tests whether two correlations ',
        'sharing a common variable (r<sub>12</sub> vs. r<sub>13</sub>) in the same group differ, ',
        'accounting for the intercorrelation r<sub>23</sub>. <i>Williams (1959); Steiger (1980); Zou (2007).</i></li>',
        '<li><b>Dependent Non-Overlapping</b> &rarr; Tests whether two correlations ',
        'without shared variables (r<sub>12</sub> vs. r<sub>34</sub>) in the same group differ. <i>Steiger (1980); Raghunathan et al. (1996).</i></li>',
        '<li><b>Jennrich Matrix Test</b> &rarr; Asymptotic &chi;² test evaluating whether two entire correlation matrices ',
        'R<sub>1</sub> and R<sub>2</sub> are statistically equal between two independent groups. <i>Jennrich (1970).</i></li>',
        '</ul>',
        '</div>'
      )
      self$results$comparisonInfo$setContent(info_html)
    },
    
    .is_empty = function(x) {
      is.null(x) || length(x) == 0 || is.na(x)
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
        table$setNote("warning", "Warning: Sample size (N) must be at least 4 in both groups to compute Fisher's Z-transformation.")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Independent", n_group1 = n1, n_group2 = n2, r_group1 = r1, r_group2 = r2,
          n_dep = NA, r12_dep = NA, r13_dep = NA, r23_dep = NA, r34_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      if (abs(r1) > 1 || abs(r2) > 1) {
        table$setNote("warning", "Warning: Correlation coefficients must be bounded within [-1, 1].")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Independent", n_group1 = n1, n_group2 = n2, r_group1 = r1, r_group2 = r2,
          n_dep = NA, r12_dep = NA, r13_dep = NA, r23_dep = NA, r34_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      if (abs(r1) == 1 || abs(r2) == 1) {
        table$setNote("warning", "Warning: Fisher's Z-transformation is undefined for perfect correlations (|r| = 1).")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Independent", n_group1 = n1, n_group2 = n2, r_group1 = r1, r_group2 = r2,
          n_dep = NA, r12_dep = NA, r13_dep = NA, r23_dep = NA, r34_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      z1 <- atanh(r1)
      z2 <- atanh(r2)
      se <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
      z_stat  <- (z1 - z2) / se
      p_value <- 2 * stats::pnorm(abs(z_stat), lower.tail = FALSE)
      
      diff_r <- r1 - r2
      se1 <- 1 / sqrt(n1 - 3)
      se2 <- 1 / sqrt(n2 - 3)
      crit <- stats::qnorm(0.975)
      
      l1 <- tanh(z1 - crit * se1)
      u1 <- tanh(z1 + crit * se1)
      l2 <- tanh(z2 - crit * se2)
      u2 <- tanh(z2 + crit * se2)
      
      ci_lower <- diff_r - sqrt((r1 - l1)^2 + (u2 - r2)^2)
      ci_upper <- diff_r + sqrt((u1 - r1)^2 + (r2 - l2)^2)
      
      ci_note <- sprintf("Difference (r1 - r2) = %.3f, 95%% CI: %.3f to %.3f (Zou, 2007).", diff_r, ci_lower, ci_upper)
      table$setNote("diff_ci", ci_note)
      
      if ('statistic' %in% names(table$columns)) {
        table$getColumn('statistic')$setTitle("Z")
      }
      
      table$setRow(rowNo = 1, values = list(
        comparisonType = "Independent",
        n_group1  = n1,
        n_group2  = n2,
        r_group1  = r1,
        r_group2  = r2,
        n_dep     = NA,
        r12_dep   = NA,
        r13_dep   = NA,
        r23_dep   = NA,
        r34_dep   = NA,
        statistic = z_stat,
        p_value   = p_value
      ))
      
      if (isTRUE(self$options$show_ci_plot)) {
        plot_df <- data.frame(
          label    = c("Group 1 (r₁)", "Group 2 (r₂)"),
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
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, r34_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      if (abs(r12) > 1 || abs(r13) > 1 || abs(r23) > 1) {
        table$setNote("warning", "Warning: Correlation coefficients must be bounded within [-1, 1].")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, r34_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      if (abs(r12) == 1 || abs(r13) == 1) {
        table$setNote("warning", "Warning: Williams' test is undefined for perfect target correlations (|r12| = 1 or |r13| = 1).")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, r34_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      det_R <- 1 - r12^2 - r13^2 - r23^2 + 2 * r12 * r13 * r23
      if (det_R <= 1e-10) {
        table$setNote("warning", "Warning: The correlation matrix implied by r12, r13, and r23 is not positive-definite (det(R) <= 0). Input values are geometrically inconsistent.")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, r34_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      r_bar <- (r12 + r13) / 2
      denom <- 2 * ((n - 1) / (n - 3)) * det_R + (r_bar^2) * ((1 - r23)^3)
      if (denom <= 1e-10) return()
      
      t_stat  <- (r12 - r13) * sqrt(((n - 1) * (1 + r23)) / denom)
      df      <- n - 3
      p_value <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
      
      diff_r <- r12 - r13
      crit   <- stats::qnorm(0.975)
      se_dep <- 1 / sqrt(n - 3)
      
      z12 <- atanh(r12)
      z13 <- atanh(r13)
      l1  <- tanh(z12 - crit * se_dep)
      u1  <- tanh(z12 + crit * se_dep)
      l2  <- tanh(z13 - crit * se_dep)
      u2  <- tanh(z13 + crit * se_dep)
      
      num_cov <- (r23 * (1 - r12^2 - r13^2) - 0.5 * r12 * r13 * (1 - r12^2 - r13^2 - r23^2))
      den_cov <- (1 - r12^2) * (1 - r13^2)
      c_cov   <- if (abs(den_cov) > 1e-10) num_cov / den_cov else 0
      
      rad_l <- max(0, (r12 - l1)^2 + (u2 - r13)^2 - 2 * c_cov * (r12 - l1) * (u2 - r13))
      rad_u <- max(0, (u1 - r12)^2 + (r13 - l2)^2 - 2 * c_cov * (u1 - r12) * (r13 - l2))
      
      ci_lower <- diff_r - sqrt(rad_l)
      ci_upper <- diff_r + sqrt(rad_u)
      
      table$setNote("diff_ci", sprintf("Difference (r12 - r13) = %.3f, 95%% CI: %.3f to %.3f (Zou, 2007).", diff_r, ci_lower, ci_upper))
      if ('statistic' %in% names(table$columns)) table$getColumn('statistic')$setTitle(paste0("t(", df, ")"))
      
      table$setRow(rowNo = 1, values = list(
        comparisonType = "Dependent",
        n_group1  = NA,
        n_group2  = NA,
        r_group1  = NA,
        r_group2  = NA,
        n_dep     = n,
        r12_dep   = r12,
        r13_dep   = r13,
        r23_dep   = r23,
        r34_dep   = NA,
        statistic = t_stat,
        p_value   = p_value
      ))
      
      if (isTRUE(self$options$show_ci_plot)) {
        plot_df <- data.frame(
          label    = c("r₁₂ (Var 1 vs Var 2)", "r₁₃ (Var 1 vs Var 3)"),
          r        = c(r12, r13),
          ci_lower = c(l1, l2),
          ci_upper = c(u1, u2),
          stringsAsFactors = FALSE
        )
        self$results$ciPlot$setState(plot_df)
      }
    },
    
    # Dependent Non-Overlapping Correlations (BSS-FEAT-023)
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
        "Note. Steiger's Z-test for non-overlapping dependent correlations (r₁₂ vs r₃₄ measured in the same sample). Citations: Steiger, J. H. (1980). Psychol Bull, 87(2), 244-251; Raghunathan, T. E. et al. (1996). Psychol Methods, 1(2), 178-183."
      )
      
      if (private$.is_empty(n) || private$.is_empty(r12) || private$.is_empty(r34) ||
          private$.is_empty(r13) || private$.is_empty(r14) || private$.is_empty(r23) || private$.is_empty(r24)) return()
      
      if (n < 5 || abs(r12) >= 1 || abs(r34) >= 1) {
        table$setNote("warning", "Warning: Sample size must be >= 5 and correlations bounded within (-1, 1).")
        return()
      }
      
      # 4x4 correlation matrix consistency check
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
      
      # Asymptotic covariance between r12 and r34 (Steiger 1980 Eq. 11)
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
        n_dep     = n,
        r12_dep   = r12,
        r13_dep   = NA,
        r23_dep   = NA,
        r34_dep   = r34,
        statistic = z_stat,
        p_value   = p_value
      ))
      
      if (isTRUE(self$options$show_ci_plot)) {
        plot_df <- data.frame(
          label    = c("r₁₂ (Var 1 vs Var 2)", "r₃₄ (Var 3 vs Var 4)"),
          r        = c(r12, r34),
          ci_lower = c(l1, l2),
          ci_upper = c(u1, u2),
          stringsAsFactors = FALSE
        )
        self$results$ciPlot$setState(plot_df)
      }
    },
    
    # Jennrich Matrix Equality Test (BSS-FEAT-022)
    .runJennrich = function() {
      vars    <- self$options$vars_jennrich
      grp_var <- self$options$group_jennrich
      table   <- self$results$jennrichTable
      table$setRow(rowNo = 1, values = list())
      
      if (is.null(vars) || length(vars) < 2 || is.null(grp_var)) return()
      
      df_work <- self$data
      if (!all(c(vars, grp_var) %in% names(df_work))) return()
      
      grp_col <- factor(df_work[[grp_var]])
      lvls    <- levels(droplevels(grp_col[!is.na(grp_col)]))
      if (length(lvls) != 2) {
        table$setNote("warning", "Jennrich test requires a grouping variable with exactly 2 valid levels.")
        return()
      }
      
      g1_data <- df_work[grp_col == lvls[1], vars, drop = FALSE]
      g2_data <- df_work[grp_col == lvls[2], vars, drop = FALSE]
      
      g1_clean <- na.omit(g1_data)
      g2_clean <- na.omit(g2_data)
      
      n1 <- nrow(g1_clean)
      n2 <- nrow(g2_clean)
      p  <- length(vars)
      
      if (n1 <= p || n2 <= p) {
        table$setNote("warning", paste0("Sample sizes in both groups (N1 = ", n1, ", N2 = ", n2, ") must exceed the number of variables (p = ", p, ")."))
        return()
      }
      
      R1 <- stats::cor(g1_clean)
      R2 <- stats::cor(g2_clean)
      
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
    }
  )
)
