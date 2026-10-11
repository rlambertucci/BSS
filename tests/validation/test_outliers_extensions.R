# Test validation for outliers extensions (BSS-FEAT-019..021)
library(jmvcore)
source("R/outliers.h.R")
source("R/outliers.b.R")

cat("1. Testing Classical Mahalanobis Outlier Detection...\n")
set.seed(42)
n <- 50
df_mah <- data.frame(
  x = rnorm(n, mean = 10, sd = 2),
  y = rnorm(n, mean = 20, sd = 3),
  z = rnorm(n, mean = 5, sd = 1)
)
# Add an extreme multivariate outlier
df_mah$x[1] <- 30
df_mah$y[1] <- 5
df_mah$z[1] <- 20

res_mah_classic <- outliersClass$new(
  data = df_mah,
  options = outliersOptions$new(
    mode = "mahalanobis",
    vars_mah = c("x", "y", "z"),
    method_mah = "classic",
    alpha_mah = "p01",
    dispPlot_mah = TRUE
  )
)
res_mah_classic$run()

summary_classic <- res_mah_classic$results$mahTable$asDF
print(summary_classic)
stopifnot(summary_classic$numOutliers >= 1)
stopifnot(summary_classic$numVars == 3)
stopifnot(summary_classic$n_analyzed == n)

outliers_classic <- res_mah_classic$results$mahOutlierList$asDF
print(outliers_classic)
stopifnot(nrow(outliers_classic) >= 1)
stopifnot(1 %in% outliers_classic$rowNumber)

qq_state <- res_mah_classic$results$mahPlot$state
stopifnot(!is.null(qq_state))
stopifnot(nrow(qq_state$df) == n)
cat("Classical Mahalanobis validated successfully!\n\n")

cat("2. Testing Robust MCD Mahalanobis Outlier Detection...\n")
res_mah_mcd <- outliersClass$new(
  data = df_mah,
  options = outliersOptions$new(
    mode = "mahalanobis",
    vars_mah = c("x", "y", "z"),
    method_mah = "robust_mcd",
    alpha_mah = "p01",
    dispPlot_mah = TRUE
  )
)
res_mah_mcd$run()

summary_mcd <- res_mah_mcd$results$mahTable$asDF
print(summary_mcd)
stopifnot(summary_mcd$numOutliers >= 1)

outliers_mcd <- res_mah_mcd$results$mahOutlierList$asDF
print(outliers_mcd)
stopifnot(nrow(outliers_mcd) >= 1)
stopifnot(1 %in% outliers_mcd$rowNumber)
cat("Robust MCD Mahalanobis validated successfully!\n\n")

cat("3. Testing Adjusted Boxplot for Skewed Distributions (Hubert & Vandervieren)...\n")
# Skewed chi-square distribution with right tail
set.seed(123)
skewed_data <- data.frame(
  val = rchisq(60, df = 2)
)

res_iqr_classic <- outliersClass$new(
  data = skewed_data,
  options = outliersOptions$new(
    mode = "iqr",
    vars_iqr = "val",
    adj_box_iqr = FALSE
  )
)
res_iqr_classic$run()
classic_fences <- res_iqr_classic$results$iqrTable$asDF
print(classic_fences)

res_iqr_adj <- outliersClass$new(
  data = skewed_data,
  options = outliersOptions$new(
    mode = "iqr",
    vars_iqr = "val",
    adj_box_iqr = TRUE
  )
)
res_iqr_adj$run()
adj_fences <- res_iqr_adj$results$iqrTable$asDF
print(adj_fences)

# Under right skew (MC > 0), the upper fence should expand to account for natural skewness
stopifnot(adj_fences$upperFence >= classic_fences$upperFence)
cat("Adjusted boxplot for skewed distributions validated successfully!\n\n")

cat("ALL OUTLIERS EXTENSION TESTS PASSED!\n")
