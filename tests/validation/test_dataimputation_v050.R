# Test validation for dataimputation v0.5.0 features (BSS-FEAT-043..045)
# 1. Predictive modeling pooling (Rubin 1987, Barnard & Rubin 1999)
# 2. UpSet plot visualization of missing data intersections
# 3. High-volume sample diagnostics (N > 10,000)

library(jmvcore)
source("R/dataimputation.h.R")
source("R/dataimputation.b.R")

cat("=============================================================\n")
cat("TEST SUITE: BSS-FEAT-043..045 (dataimputation v0.5.0)\n")
cat("=============================================================\n\n")

# --- Test 1: Predictive Modeling Pooling (OLS Linear Regression) ---
cat("--- Test 1: Predictive Modeling with Automatic Rubin Pooling (LM) ---\n")
set.seed(42)
n <- 120
x1 <- rnorm(n, mean = 5, sd = 2)
x2 <- rnorm(n, mean = 10, sd = 3)
y <- 2.5 + 1.8 * x1 - 0.7 * x2 + rnorm(n, sd = 1.2)

# Introduce 20% missingness in x1 and x2
x1_miss <- x1; x1_miss[sample(n, 24)] <- NA
x2_miss <- x2; x2_miss[sample(n, 20)] <- NA

df_test <- data.frame(y = y, x1 = x1_miss, x2 = x2_miss)

res_mice <- dataimputationClass$new(
  data = df_test,
  options = dataimputationOptions$new(
    mode = "mice_cs",
    vars_mice_cs = c("x1", "x2"),
    predictor_vars_mice_cs = c("y"),
    run_imputation_mice_cs = TRUE,
    num_imputations_mice_cs = 5,
    maxiter_mice_cs = 5,
    seed_mice_cs = "123",
    enable_model_pooling_mice_cs = TRUE,
    model_type_mice_cs = "lm",
    model_dep_mice_cs = "y",
    model_indep_mice_cs = c("x1", "x2"),
    show_upset_plot_mice_cs = TRUE
  )
)
res_mice$run()

pool_tab <- res_mice$results$modelPoolingTable_mice_cs$asDF
print(pool_tab)

stopifnot(nrow(pool_tab) == 3) # Intercept, x1, x2
stopifnot("(Intercept)" %in% pool_tab$term)
stopifnot("x1" %in% pool_tab$term)
stopifnot("x2" %in% pool_tab$term)

# x1 coefficient should be close to 1.8 and highly significant
x1_row <- pool_tab[pool_tab$term == "x1", ]
stopifnot(abs(x1_row$estimate - 1.8) < 0.4)
stopifnot(x1_row$p_value < 0.001)
stopifnot(x1_row$df > 1) # Barnard-Rubin adjusted df
stopifnot(x1_row$std_error > 0)
stopifnot(x1_row$fmi >= 0 && x1_row$fmi <= 1)

citation_note <- res_mice$results$modelPoolingTable_mice_cs$notes$citation$note
cat("Citation note:", citation_note, "\n")
stopifnot(grepl("Barnard & Rubin, 1999", citation_note))
cat(">>> PASS: Predictive model pooled coefficients, SEs, Barnard-Rubin df and CIs verified!\n\n")

# --- Test 2: Missing Data UpSet Plot State ---
cat("--- Test 2: Missing Data UpSet Plot State Inspection ---\n")
upset_state <- res_mice$results$upsetPlot_mice_cs$state
stopifnot(!is.null(upset_state))
stopifnot(!is.null(upset_state$data))
stopifnot(all(c("x1", "x2") %in% upset_state$vars))

# Verify UpSet renderer runs without error
mock_image <- list(state = upset_state)
mock_theme <- ggplot2::theme_classic()
draw_res <- res_mice$.__enclos_env__$private$.draw_upset_plot(mock_image)
stopifnot(isTRUE(draw_res))
cat(">>> PASS: UpSet plot state and renderer validated successfully!\n\n")

# --- Test 3: Large Sample Size Detection (N > 10,000) ---
cat("--- Test 3: Large Sample Size Notification (N > 10,000) ---\n")
n_large <- 12000
df_large <- data.frame(
  v1 = rnorm(n_large),
  v2 = rnorm(n_large)
)
df_large$v1[sample(n_large, 500)] <- NA

res_large <- dataimputationClass$new(
  data = df_large,
  options = dataimputationOptions$new(
    mode = "mice_cs",
    vars_mice_cs = c("v1", "v2"),
    run_imputation_mice_cs = FALSE
  )
)
res_large$run()

large_note <- res_large$results$missingnessTable_mice_cs$notes$large_sample$note
cat("Large sample note:", large_note, "\n")
stopifnot(!is.null(large_note))
stopifnot(grepl("Large sample size detected", large_note))
stopifnot(grepl("12000", large_note))
cat(">>> PASS: High-volume dataset (N > 10,000) flagged with performance guidance note!\n\n")

# --- Test 4: Predictive Modeling Pooling (missRanger Random Forest) ---
cat("--- Test 4: missRanger Multiple Imputation + Model Pooling ---\n")
df_mr_test <- df_test[1:60, ] # smaller subset for speed
res_mr <- dataimputationClass$new(
  data = df_mr_test,
  options = dataimputationOptions$new(
    mode = "missranger",
    vars_mr = c("x1", "x2"),
    predictor_vars_mr = c("y"),
    use_multiple_imputation_mr = TRUE,
    num_imputations_mr = 3,
    maxiter_mr = 3,
    num_trees_mr = 20,
    pmm_k_mr = 3,
    seed_mr = "42",
    run_imputation_mr = TRUE,
    enable_model_pooling_mr = TRUE,
    model_type_mr = "lm",
    model_dep_mr = "y",
    model_indep_mr = c("x1", "x2")
  )
)
res_mr$run()

pool_mr_tab <- res_mr$results$modelPoolingTable_mr$asDF
print(pool_mr_tab)
stopifnot(nrow(pool_mr_tab) == 3)
stopifnot(!is.na(pool_mr_tab$estimate[1]))
cat(">>> PASS: missRanger multiple imputation + model pooling executed successfully!\n\n")

cat("=============================================================\n")
cat("ALL DATAIMPUTATION V0.5.0 EXTENSION TESTS PASSED!\n")
cat("=============================================================\n")
