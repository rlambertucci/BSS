# Test validation for correlationcomparison extensions (BSS-FEAT-022..024)
library(jmvcore)
source("R/correlationcomparison.h.R")
source("R/correlationcomparison.b.R")

cat("1. Testing Dependent Non-Overlapping Correlations (Steiger 1980)...\n")
res_dep_non <- correlationComparisonClass$new(
  data = data.frame(),
  options = correlationComparisonOptions$new(
    mode = "dep_nonoverlapping",
    dep_non_n = 100,
    dep_non_r12 = 0.50,
    dep_non_r34 = 0.20,
    dep_non_r13 = 0.30,
    dep_non_r14 = 0.10,
    dep_non_r23 = 0.20,
    dep_non_r24 = 0.30,
    show_ci_plot = TRUE
  )
)
res_dep_non$run()

df_dep_non <- res_dep_non$results$comparisonTable$asDF
print(df_dep_non)
stopifnot(!is.na(df_dep_non$statistic))
stopifnot(!is.na(df_dep_non$p_value))
stopifnot(df_dep_non$r12_dep == 0.50)
stopifnot(df_dep_non$r34_dep == 0.20)

plot_state <- res_dep_non$results$ciPlot$state
stopifnot(!is.null(plot_state))
stopifnot(nrow(plot_state) == 2)
cat("Dependent non-overlapping test and CI plot validated successfully!\n\n")

cat("2. Testing Jennrich Matrix Equality Test (Jennrich 1970)...\n")
set.seed(123)
n_grp <- 60
g1 <- matrix(rnorm(n_grp * 3), ncol = 3)
# Group 2 with high correlation between col 1 and col 2
g2 <- matrix(rnorm(n_grp * 3), ncol = 3)
g2[, 2] <- g2[, 1] * 0.8 + rnorm(n_grp, sd = 0.6)

df_jenn <- data.frame(
  v1 = c(g1[, 1], g2[, 1]),
  v2 = c(g1[, 2], g2[, 2]),
  v3 = c(g1[, 3], g2[, 3]),
  grp = factor(rep(c("A", "B"), each = n_grp))
)

res_jenn <- correlationComparisonClass$new(
  data = df_jenn,
  options = correlationComparisonOptions$new(
    mode = "jennrich",
    vars_jennrich = c("v1", "v2", "v3"),
    group_jennrich = "grp"
  )
)
res_jenn$run()

df_jenn_table <- res_jenn$results$jennrichTable$asDF
print(df_jenn_table)
stopifnot(df_jenn_table$numVars == 3)
stopifnot(df_jenn_table$df == 3) # p*(p-1)/2 = 3*2/2 = 3
stopifnot(df_jenn_table$chisq > 0)
stopifnot(!is.na(df_jenn_table$p_value))
cat("Jennrich matrix equality test validated successfully!\n\n")

cat("3. Testing CI Plot in Independent Mode...\n")
res_ind <- correlationComparisonClass$new(
  data = data.frame(),
  options = correlationComparisonOptions$new(
    mode = "independent",
    independent_n1 = 50,
    independent_n2 = 50,
    independent_r1 = 0.60,
    independent_r2 = 0.20,
    show_ci_plot = TRUE
  )
)
res_ind$run()
plot_ind <- res_ind$results$ciPlot$state
stopifnot(!is.null(plot_ind))
stopifnot(nrow(plot_ind) == 2)
cat("CI plot in independent mode validated successfully!\n\n")

cat("ALL CORRELATIONCOMPARISON EXTENSION TESTS PASSED!\n")
