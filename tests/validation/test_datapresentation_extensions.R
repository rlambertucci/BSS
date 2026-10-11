# Test Suite: datapresentation Extensions (APA 7th HTML Export, Correlation Matrix with Stars, Correlation Heatmap)
suppressPackageStartupMessages({
  library(jmvcore)
  library(R6)
  library(dplyr)
  library(tidyr)
  library(emmeans)
})

source("R/datapresentation.h.R")
source("R/datapresentation.b.R")

cat("=============================================================\n")
cat("TEST SUITE: BSS-FEAT-028..030 (datapresentation extensions)\n")
cat("=============================================================\n\n")

set.seed(42)
df_test <- data.frame(
  group = factor(rep(c("Control", "Treatment"), each = 20)),
  var1  = rnorm(40, mean = 50, sd = 10),
  var2  = rnorm(40, mean = 100, sd = 15),
  var3  = rnorm(40, mean = 25, sd = 5)
)
# Induzir correlacao moderada
df_test$var2 <- df_test$var2 + 0.6 * df_test$var1

# Test 1: APA 7th Formatted HTML Table
cat("--- Test 1: APA 7th Formatted HTML Table ---\n")
res_html <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "cross_sectional",
    deps = c("var1", "var2"),
    group = "group",
    showTable = TRUE,
    showFormattedTable = TRUE
  ),
  data = df_test
)
res_html$run()
html_content <- res_html$results$formattedTable$content
stopifnot(!is.null(html_content) && nzchar(html_content))
stopifnot(grepl("Table 1", html_content))
stopifnot(grepl("border-top: 2px solid", html_content))
cat("HTML table snippet:\n")
cat(substr(html_content, 1, 300), "...\n")
cat(">>> PASS: APA 7th HTML table generated successfully!\n\n")

# Test 2: Correlation Matrix with Significance Stars (Pearson)
cat("--- Test 2: APA Correlation Matrix with Stars (Pearson) ---\n")
res_corr <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "cross_sectional",
    deps = c("var1", "var2", "var3"),
    group = "group",
    showCorrMatrix = TRUE,
    corrMethod = "pearson"
  ),
  data = df_test
)
res_corr$run()
corr_df <- res_corr$results$corrMatrixTable$asDF
print(corr_df)
stopifnot(nrow(corr_df) == 3)
stopifnot(!is.na(corr_df$mean[1]))
stopifnot(corr_df$col_1[1] == "-")
cat("r(var1, var2): ", corr_df$col_1[2], "\n")
stopifnot(grepl("\\*|\\.", corr_df$col_1[2]))
cat(">>> PASS: Correlation matrix with stars tabulated successfully!\n\n")

# Test 3: Correlation Matrix with Spearman
cat("--- Test 3: Correlation Matrix (Spearman) ---\n")
res_spearman <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "cross_sectional",
    deps = c("var1", "var2", "var3"),
    group = "group",
    showCorrMatrix = TRUE,
    corrMethod = "spearman"
  ),
  data = df_test
)
res_spearman$run()
corr_spear_df <- res_spearman$results$corrMatrixTable$asDF
print(corr_spear_df)
stopifnot(nrow(corr_spear_df) == 3)
cat(">>> PASS: Spearman correlation matrix tabulated successfully!\n\n")

# Test 4: Correlation Heatmap Plot state & hierarchical clustering
cat("--- Test 4: Correlation Heatmap State (Clustered) ---\n")
res_heat <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "cross_sectional",
    deps = c("var1", "var2", "var3"),
    group = "group",
    showCorrHeatmap = TRUE,
    corrCluster = TRUE
  ),
  data = df_test
)
res_heat$run()
heat_state <- res_heat$results$corrHeatmapPlot$state
stopifnot(!is.null(heat_state))
stopifnot(length(heat_state$vars) == 3)
stopifnot(isTRUE(heat_state$cluster))
cat("Heatmap state vars: ", paste(heat_state$vars, collapse = ", "), "\n")
cat(">>> PASS: Correlation heatmap state properly configured!\n\n")

cat("=============================================================\n")
cat("ALL EXTENSION TESTS FOR datapresentation PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")
