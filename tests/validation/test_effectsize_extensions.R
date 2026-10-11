# Test validation for effectsize extensions (BSS-FEAT-013..015)
library(jmvcore)
source("R/effectsize.h.R")
source("R/effectsize.b.R")

test_df <- data.frame(
  score = c(10, 12, 11, 14, 13, 20, 22, 21, 24, 23),
  group = factor(c(rep("A", 5), rep("B", 5)))
)

cat("1. Testing Cohen vs Sawilowsky benchmark...\n")
res_cohen <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "score",
    indep_vars_cs = "group",
    benchmark_standard = "cohen",
    run_analysis = TRUE,
    show_conversions = TRUE,
    show_es_plot = TRUE
  ),
  data = test_df
)
res_cohen$run()

row1_cohen <- res_cohen$results$effectSizeTable$asDF
print(row1_cohen)
stopifnot(nrow(row1_cohen) >= 1)
cat("Cohen interp:", row1_cohen$interpretation[1], "\n")

res_saw <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "score",
    indep_vars_cs = "group",
    benchmark_standard = "sawilowsky",
    run_analysis = TRUE,
    show_conversions = TRUE,
    show_es_plot = TRUE
  ),
  data = test_df
)
res_saw$run()

row1_saw <- res_saw$results$effectSizeTable$asDF
print(row1_saw)
cat("Sawilowsky interp:", row1_saw$interpretation[1], "\n")
stopifnot(grepl("Huge|Very large|Large", row1_saw$interpretation[1]))

cat("\n2. Testing Multi-Metric Conversions Table...\n")
conv_df <- res_saw$results$conversionTable$asDF
print(conv_df)
stopifnot(nrow(conv_df) >= 1)
stopifnot(!is.na(conv_df$cohen_d[1]))
stopifnot(!is.na(conv_df$correlation_r[1]))
stopifnot(!is.na(conv_df$eta_squared[1]))
stopifnot(!is.na(conv_df$odds_ratio[1]))
stopifnot(!is.na(conv_df$auc[1]))
cat("Conversions verified successfully!\n")

cat("\n3. Testing Forest Plot state...\n")
plot_state <- res_saw$results$esPlot$state
print(plot_state)
stopifnot(!is.null(plot_state))
stopifnot(nrow(plot_state) >= 1)
stopifnot(all(c("label", "estimate", "ci_lower", "ci_upper") %in% names(plot_state)))
cat("Forest plot state validated successfully!\n")

cat("\nALL EFFECTSIZE EXTENSION TESTS PASSED!\n")
