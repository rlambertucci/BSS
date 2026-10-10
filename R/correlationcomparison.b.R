
# This file is a generated template, your changes will not be overwritten

correlationComparisonClass <- R6::R6Class(
  "correlationComparisonClass",
  inherit = correlationComparisonBase,
  private = list(
    
    .init = function() {
      info_html <- paste0(
        '<div style="font-size: 0.9em; line-height: 1.5;">',
        '<ul>',
        '<li><b>Independent Comparison</b> &rarr; Tests whether two correlation coefficients ',
        'obtained from <b>two independent groups</b> are significantly different from each other. ',
        'Uses Fisher\'s Z-transformation to compare r<sub>1</sub> in Group 1 vs. r<sub>2</sub> in Group 2. ',
        '<i>Fisher (1921); Zou (2007).</i></li>',
        '<li><b>Dependent Comparison</b> &rarr; Tests whether two correlation coefficients ',
        'obtained from <b>the same group</b> are significantly different, where both correlations ',
        'share a common variable. Compares r<sub>12</sub> vs. r<sub>13</sub> while accounting for ',
        'the intercorrelation r<sub>23</sub> between the non-shared variables. ',
        '<i>Williams (1959); Steiger (1980); Zou (2007).</i></li>',
        '</ul>',
        '<p style="font-style: italic; padding-top: 10px;">',
        'Note: Sample size must be at least 4 and correlation coefficients ',
        'must be strictly between -1 and 1.',
        '</p>',
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
      # Reset previous dynamic notes
      try(table$setNote("warning", NULL), silent = TRUE)
      try(table$setNote("diff_ci", NULL), silent = TRUE)
      
      if ('statistic' %in% names(table$columns)) {
        table$getColumn('statistic')$setTitle("Statistic")
      }
      
      if (mode == "independent") {
        private$.runIndependent()
      } else if (mode == "dependent") {
        private$.runDependent()
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
      
      # Verificacao defensiva de campos vazios ou NA
      if (private$.is_empty(n1) || private$.is_empty(n2) || private$.is_empty(r1) || private$.is_empty(r2)) {
        return()
      }
      
      # Validacoes de sanidade sem crash fatal
      if (n1 < 4 || n2 < 4) {
        table$setNote("warning", "Warning: Sample size (N) must be at least 4 in both groups to compute Fisher's Z-transformation.")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Independent", n_group1 = n1, n_group2 = n2, r_group1 = r1, r_group2 = r2,
          n_dep = NA, r12_dep = NA, r13_dep = NA, r23_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      if (abs(r1) > 1 || abs(r2) > 1) {
        table$setNote("warning", "Warning: Correlation coefficients must be bounded within [-1, 1].")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Independent", n_group1 = n1, n_group2 = n2, r_group1 = r1, r_group2 = r2,
          n_dep = NA, r12_dep = NA, r13_dep = NA, r23_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      if (abs(r1) == 1 || abs(r2) == 1) {
        table$setNote("warning", "Warning: Fisher's Z-transformation is undefined for perfect correlations (|r| = 1).")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Independent", n_group1 = n1, n_group2 = n2, r_group1 = r1, r_group2 = r2,
          n_dep = NA, r12_dep = NA, r13_dep = NA, r23_dep = NA, statistic = NA, p_value = NA
        ))
        return()
      }
      
      # Calculo de Fisher's Z
      z1 <- atanh(r1)
      z2 <- atanh(r2)
      se <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
      z_stat  <- (z1 - z2) / se
      p_value <- 2 * stats::pnorm(abs(z_stat), lower.tail = FALSE)
      
      # Diferenca e IC 95% via Zou (2007)
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
        statistic = z_stat,
        p_value   = p_value
      ))
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
      
      if (private$.is_empty(n) || private$.is_empty(r12) || private$.is_empty(r13) || private$.is_empty(r23)) {
        return()
      }
      
      if (n < 4) {
        table$setNote("warning", "Warning: Sample size (N) must be at least 4 for Williams' t-test.")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, statistic = NA, p_value = NA
        ))
        return()
      }
      
      if (abs(r12) > 1 || abs(r13) > 1 || abs(r23) > 1) {
        table$setNote("warning", "Warning: Correlation coefficients must be bounded within [-1, 1].")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, statistic = NA, p_value = NA
        ))
        return()
      }
      
      if (abs(r12) == 1 || abs(r13) == 1) {
        table$setNote("warning", "Warning: Williams' test is undefined for perfect target correlations (|r12| = 1 or |r13| = 1).")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, statistic = NA, p_value = NA
        ))
        return()
      }
      
      # Determinante da matriz de correlacao 3x3
      det_R <- 1 - r12^2 - r13^2 - r23^2 + 2 * r12 * r13 * r23
      
      if (det_R <= 1e-10) {
        table$setNote("warning", "Warning: The correlation matrix implied by r12, r13, and r23 is not positive-definite (det(R) <= 0). Input values are geometrically inconsistent.")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, statistic = NA, p_value = NA
        ))
        return()
      }
      
      # Estatistica t de Williams (1959)
      r_bar <- (r12 + r13) / 2
      denom <- 2 * ((n - 1) / (n - 3)) * det_R + (r_bar^2) * ((1 - r23)^3)
      
      if (denom <= 1e-10) {
        table$setNote("warning", "Warning: Denominator in Williams' t-statistic is near zero.")
        table$setRow(rowNo = 1, values = list(
          comparisonType = "Dependent", n_group1 = NA, n_group2 = NA, r_group1 = NA, r_group2 = NA,
          n_dep = n, r12_dep = r12, r13_dep = r13, r23_dep = r23, statistic = NA, p_value = NA
        ))
        return()
      }
      
      t_stat  <- (r12 - r13) * sqrt(((n - 1) * (1 + r23)) / denom)
      df      <- n - 3
      p_value <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
      
      # Diferenca e IC 95% via Zou (2007) para correlacoes dependentes sobrepostas
      diff_r <- r12 - r13
      crit   <- stats::qnorm(0.975)
      se_dep <- 1 / sqrt(n - 3)
      
      z12 <- atanh(r12)
      z13 <- atanh(r13)
      
      l1 <- tanh(z12 - crit * se_dep)
      u1 <- tanh(z12 + crit * se_dep)
      l2 <- tanh(z13 - crit * se_dep)
      u2 <- tanh(z13 + crit * se_dep)
      
      # Covariancia assintotica entre r12 e r13 (Pearson 1898; Zou 2007 Eq. 14)
      num_cov <- (r23 * (1 - r12^2 - r13^2) - 0.5 * r12 * r13 * (1 - r12^2 - r13^2 - r23^2))
      den_cov <- (1 - r12^2) * (1 - r13^2)
      c_cov   <- if (abs(den_cov) > 1e-10) num_cov / den_cov else 0
      
      rad_l <- max(0, (r12 - l1)^2 + (u2 - r13)^2 - 2 * c_cov * (r12 - l1) * (u2 - r13))
      rad_u <- max(0, (u1 - r12)^2 + (r13 - l2)^2 - 2 * c_cov * (u1 - r12) * (r13 - l2))
      
      ci_lower <- diff_r - sqrt(rad_l)
      ci_upper <- diff_r + sqrt(rad_u)
      
      ci_note <- sprintf("Difference (r12 - r13) = %.3f, 95%% CI: %.3f to %.3f (Zou, 2007).", diff_r, ci_lower, ci_upper)
      table$setNote("diff_ci", ci_note)
      
      if ('statistic' %in% names(table$columns)) {
        table$getColumn('statistic')$setTitle(paste0("t(", df, ")"))
      }
      
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
        statistic = t_stat,
        p_value   = p_value
      ))
    }
  )
)
