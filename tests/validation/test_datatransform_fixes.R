# =============================================================
# Script de Validacao Independente - Ciclo 10: datatransform
# Tickets: BSS-STAT-035, BSS-STAT-036, BSS-STAT-037, BSS-STAT-038, BSS-CODE-016
# =============================================================

suppressPackageStartupMessages({
  library(jmvcore)
  library(R6)
  library(bestNormalize)
  library(SuppDists)
})

# Carregar classe base e implementacao
source("R/datatransform.h.R")
source("R/datatransform.b.R")

cat("=============================================================\n")
cat("TEST SUITE: BSS-STAT-035 a 038, BSS-CODE-016 (datatransform)\n")
cat("=============================================================\n\n")

# -------------------------------------------------------------
# Test 1: BSS-STAT-035 (.to_numeric_vec and Yeo-Johnson transformation)
# -------------------------------------------------------------
cat("--- Test 1: BSS-STAT-035: Coercion and Yeo-Johnson Transformation ---\n")
to_num <- dataTransformClass$private_methods$.to_numeric_vec
raw_fac <- factor(c("1.2", "2.5", "3.1", "4.8", "5.2", "6.7", "7.1", "8.9"))
coerced <- to_num(raw_fac)
stopifnot(identical(coerced, c(1.2, 2.5, 3.1, 4.8, 5.2, 6.7, 7.1, 8.9)))

df_yj <- data.frame(
  score = c(-2.0, -1.0, 0.0, 1.5, 2.3, 3.8, 5.0, 8.2, 10.5)
)
res_yj <- dataTransformClass$new(
  options = dataTransformOptions$new(
    vars = "score",
    method = "yeojohnson",
    show_lambda_table = TRUE,
    show_normality_table = TRUE
  ),
  data = df_yj
)
res_yj$run()
tbl_yj <- res_yj$results$lambdaTable$asDF
print(tbl_yj)
stopifnot(tbl_yj$method[1] == "Yeo-Johnson")
stopifnot(!is.na(tbl_yj$value[1]))
cat(">>> PASS: Yeo-Johnson computed successfully with negative/zero/positive values!\n\n")

# -------------------------------------------------------------
# Test 2: BSS-STAT-036 (Box-Cox with Non-Positive Values & auto_shift = FALSE)
# -------------------------------------------------------------
cat("--- Test 2: BSS-STAT-036: Box-Cox Non-Positive Data without Crash ---\n")
df_nonpos <- data.frame(
  vals = c(-1.0, 0.0, 2.0, 3.5, 5.0, 6.2, 8.0)
)
res_boxcox_noshift <- dataTransformClass$new(
  options = dataTransformOptions$new(
    vars = "vals",
    method = "boxcox",
    auto_shift = FALSE,
    show_lambda_table = TRUE
  ),
  data = df_nonpos
)
# Should run cleanly without jmvcore::reject crash!
res_boxcox_noshift$run()
tbl_bc_noshift <- res_boxcox_noshift$results$lambdaTable$asDF
print(tbl_bc_noshift)
stopifnot(is.na(tbl_bc_noshift$value[1]))
warn_note <- res_boxcox_noshift$results$lambdaTable$notes$boxcox_warning$note
cat("Box-Cox warning note:", warn_note, "\n")
stopifnot(grepl("Box-Cox transformation requires strictly positive", warn_note))
cat(">>> PASS: Box-Cox non-positive values handled gracefully with warning note!\n\n")

# -------------------------------------------------------------
# Test 3: BSS-STAT-036 (Box-Cox with auto_shift = TRUE)
# -------------------------------------------------------------
cat("--- Test 3: BSS-STAT-036: Box-Cox with Automatic Shift ---\n")
res_boxcox_shift <- dataTransformClass$new(
  options = dataTransformOptions$new(
    vars = "vals",
    method = "boxcox",
    auto_shift = TRUE,
    show_lambda_table = TRUE
  ),
  data = df_nonpos
)
res_boxcox_shift$run()
tbl_bc_shift <- res_boxcox_shift$results$lambdaTable$asDF
print(tbl_bc_shift)
stopifnot(!is.na(tbl_bc_shift$value[1]))
stopifnot(tbl_bc_shift$shift_applied[1] == "Yes")
stopifnot(tbl_bc_shift$shift_value[1] > 1.0)
cat(">>> PASS: Box-Cox auto-shift applied correctly and lambda estimated!\n\n")

# -------------------------------------------------------------
# Test 4: BSS-STAT-037 (Johnson Curve Estimation)
# -------------------------------------------------------------
cat("--- Test 4: BSS-STAT-037: Johnson Curve Transformation ---\n")
set.seed(42)
df_johnson <- data.frame(
  skewed = rchisq(50, df = 3)
)
res_johnson <- dataTransformClass$new(
  options = dataTransformOptions$new(
    vars = "skewed",
    method = "johnson",
    show_lambda_table = TRUE,
    show_normality_table = TRUE
  ),
  data = df_johnson
)
res_johnson$run()
tbl_johnson <- res_johnson$results$lambdaTable$asDF
print(tbl_johnson)
stopifnot(nrow(tbl_johnson) >= 5)
cat(">>> PASS: Johnson curve transformation executed and parameters tabulated!\n\n")

# -------------------------------------------------------------
# Test 5: BSS-STAT-038 (Shapiro-Wilk Subsampling Note for N > 5000)
# -------------------------------------------------------------
cat("--- Test 5: BSS-STAT-038: Shapiro-Wilk Subsampling for N > 5000 ---\n")
set.seed(123)
df_large <- data.frame(
  big_x = rnorm(5500)
)
res_large <- dataTransformClass$new(
  options = dataTransformOptions$new(
    vars = "big_x",
    method = "yeojohnson",
    show_normality_table = TRUE
  ),
  data = df_large
)
res_large$run()
tbl_norm <- res_large$results$normalityTable$asDF
print(tbl_norm)
stopifnot(nrow(tbl_norm) == 2)
sw_note <- res_large$results$normalityTable$notes$sample_size_note$note
cat("Shapiro-Wilk sample note:", sw_note, "\n")
stopifnot(grepl("N > 5000", sw_note))
cat(">>> PASS: Shapiro-Wilk subsampling for N > 5000 documented with note!\n\n")

cat("=============================================================\n")
cat("ALL VALIDATION TESTS FOR datatransform PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")
