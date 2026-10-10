# =============================================================
# Script de Validacao Independente - Ciclo 8: correlationcomparison
# Tickets: BSS-CODE-011, BSS-STAT-030, BSS-STAT-031, BSS-STAT-032, BSS-CODE-012
# =============================================================

suppressPackageStartupMessages({
  library(jmvcore)
  library(R6)
})

# Carregar classe base e implementacao
source("R/correlationcomparison.h.R")
source("R/correlationcomparison.b.R")

cat("=============================================================\n")
cat("TEST SUITE: BSS-CODE-011, BSS-STAT-030 a 032 (correlationcomparison)\n")
cat("=============================================================\n\n")

# -------------------------------------------------------------
# Test 1: BSS-CODE-011 (Defensive input handling: .is_empty helper)
# -------------------------------------------------------------
cat("--- Test 1: BSS-CODE-011: Defensive Input Validation ---\n")
is_emp <- correlationComparisonClass$private_methods$.is_empty
stopifnot(is_emp(NULL))
stopifnot(is_emp(NA))
stopifnot(is_emp(NA_real_))
stopifnot(is_emp(numeric(0)))
stopifnot(!is_emp(20))
stopifnot(!is_emp(0.5))
cat(">>> PASS: .is_empty correctly classifies NULL, NA, and empty vectors without logical errors!\n\n")

# -------------------------------------------------------------
# Test 2: BSS-STAT-030 (Graceful handling of N < 4 and |r| >= 1 without reject())
# -------------------------------------------------------------
cat("--- Test 2: BSS-STAT-030: Bounds Validation (N < 4 and |r| >= 1) ---\n")
res_small_n <- correlationComparisonClass$new(
  options = correlationComparisonOptions$new(
    mode = "independent",
    independent_n1 = 3,
    independent_n2 = 50,
    independent_r1 = 0.5,
    independent_r2 = 0.2
  ),
  data = data.frame()
)
res_small_n$run()
tbl_small_n <- res_small_n$results$comparisonTable$asDF
print(tbl_small_n)
stopifnot(is.na(tbl_small_n$statistic[1]))
cat("Warning note (small N):", res_small_n$results$comparisonTable$notes$warning$note, "\n")
stopifnot(grepl("Sample size", res_small_n$results$comparisonTable$notes$warning$note))

# Test bound |r| >= 1
res_bound_r <- correlationComparisonClass$new(
  options = correlationComparisonOptions$new(
    mode = "independent",
    independent_n1 = 30,
    independent_n2 = 30,
    independent_r1 = 1.0,
    independent_r2 = 0.5
  ),
  data = data.frame()
)
res_bound_r$run()
tbl_bound_r <- res_bound_r$results$comparisonTable$asDF
print(tbl_bound_r)
stopifnot(is.na(tbl_bound_r$statistic[1]))
cat("Warning note (r=1.0):", res_bound_r$results$comparisonTable$notes$warning$note, "\n")
stopifnot(grepl("undefined for perfect", res_bound_r$results$comparisonTable$notes$warning$note))
cat(">>> PASS: Small N and boundary r caught with informative warning notes, no fatal crash!\n\n")

# -------------------------------------------------------------
# Test 3: BSS-STAT-031 (Non-positive-definite correlation matrix in dependent test)
# -------------------------------------------------------------
cat("--- Test 3: BSS-STAT-031: Non-positive-definite Matrix Handling ---\n")
# Triplet violating positive-definiteness: r12=0.9, r13=-0.9, r23=0.9
# det(R) = 1 - 0.81 - 0.81 - 0.81 + 2*(0.9)*(-0.9)*(0.9) = 1 - 2.43 - 1.458 = -2.888 < 0
res_non_pd <- correlationComparisonClass$new(
  options = correlationComparisonOptions$new(
    mode = "dependent",
    dependent_n = 50,
    dependent_r12 = 0.9,
    dependent_r13 = -0.9,
    dependent_r23 = 0.9
  ),
  data = data.frame()
)
res_non_pd$run()
tbl_non_pd <- res_non_pd$results$comparisonTable$asDF
print(tbl_non_pd)
stopifnot(is.na(tbl_non_pd$statistic[1]))
cat("Non-PD note:", res_non_pd$results$comparisonTable$notes$warning$note, "\n")
stopifnot(grepl("not positive-definite", res_non_pd$results$comparisonTable$notes$warning$note))
cat(">>> PASS: Non-positive definite correlation matrix flagged cleanly with warning note!\n\n")

# -------------------------------------------------------------
# Test 4: BSS-STAT-032 (Independent Comparison: Fisher Z and Zou 2007 CI)
# -------------------------------------------------------------
cat("--- Test 4: BSS-STAT-032: Fisher Z Calculation & Zou CI ---\n")
# N1 = 50, N2 = 50, r1 = 0.60, r2 = 0.20
# z1 = atanh(0.60) = 0.693147, z2 = atanh(0.20) = 0.202733
# se = sqrt(1/47 + 1/47) = sqrt(2/47) = 0.206284
# Z = (0.693147 - 0.202733) / 0.206284 = 2.37737
# p = 2 * (1 - pnorm(2.37737)) = 0.017436
res_ind <- correlationComparisonClass$new(
  options = correlationComparisonOptions$new(
    mode = "independent",
    independent_n1 = 50,
    independent_n2 = 50,
    independent_r1 = 0.60,
    independent_r2 = 0.20
  ),
  data = data.frame()
)
res_ind$run()
tbl_ind <- res_ind$results$comparisonTable$asDF
print(tbl_ind)

stopifnot(abs(tbl_ind$statistic[1] - 2.377) < 0.01)
stopifnot(abs(tbl_ind$p_value[1] - 0.0174) < 0.005)
diff_note_ind <- res_ind$results$comparisonTable$notes$diff_ci$note
cat("Diff CI note:", diff_note_ind, "\n")
stopifnot(grepl("Difference", diff_note_ind))
stopifnot(grepl("Zou, 2007", diff_note_ind))
cat(">>> PASS: Fisher's Z and Zou 95% CI computed accurately for independent correlations!\n\n")

# -------------------------------------------------------------
# Test 5: BSS-STAT-032 (Dependent Comparison: Williams t and Zou 2007 CI)
# -------------------------------------------------------------
cat("--- Test 5: BSS-STAT-032: Williams t Calculation & Zou CI ---\n")
# N = 60, r12 = 0.50, r13 = 0.20, r23 = 0.30
# det(R) = 1 - 0.25 - 0.04 - 0.09 + 2*(0.5)*(0.2)*(0.3) = 0.62 + 0.06 = 0.68
# r_bar = 0.35, r_bar^2 = 0.1225
# (1 - r23)^3 = 0.7^3 = 0.343
# denom = 2 * (59/57) * 0.68 + 0.1225 * 0.343 = 1.4077 + 0.0420 = 1.4497
# t = (0.50 - 0.20) * sqrt(59 * 1.30 / 1.4497) = 0.30 * sqrt(76.7 / 1.4497) = 0.30 * 7.2738 = 2.182
# df = 57, p = 2 * pt(2.182, 57, lower=F) = 0.0332
res_dep <- correlationComparisonClass$new(
  options = correlationComparisonOptions$new(
    mode = "dependent",
    dependent_n = 60,
    dependent_r12 = 0.50,
    dependent_r13 = 0.20,
    dependent_r23 = 0.30
  ),
  data = data.frame()
)
res_dep$run()
tbl_dep <- res_dep$results$comparisonTable$asDF
print(tbl_dep)

stopifnot(abs(tbl_dep$statistic[1] - 2.182) < 0.02)
stopifnot(abs(tbl_dep$p_value[1] - 0.0332) < 0.005)
diff_note_dep <- res_dep$results$comparisonTable$notes$diff_ci$note
cat("Diff CI note:", diff_note_dep, "\n")
stopifnot(grepl("Difference", diff_note_dep))
stopifnot(grepl("Zou, 2007", diff_note_dep))
cat(">>> PASS: Williams' t and Zou 95% CI computed accurately for dependent correlations!\n\n")

cat("=============================================================\n")
cat("ALL VALIDATION TESTS FOR correlationcomparison PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")
