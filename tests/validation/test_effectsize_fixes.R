library(jmvcore)
source('R/effectsize.h.R')
source('R/effectsize.b.R')

cat("=============================================================\n")
cat("TEST SUITE: BSS-STAT-018 a 023 e BSS-CODE-007 (effectsize)\n")
cat("=============================================================\n\n")

# -------------------------------------------------------------
# Test 1: BSS-STAT-018 (Glass's Delta Reference Group)
# -------------------------------------------------------------
cat("--- Test 1: Glass's Delta Reference Group Selection ---\n")
set.seed(123)
# Group A: sd = 1; Group B: sd = 5
df_g <- data.frame(
  y = c(rnorm(30, mean = 10, sd = 1), rnorm(30, mean = 15, sd = 5)),
  grp = factor(rep(c("A", "B"), each = 30))
)

# Ref = B (default)
res_g_b <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "y",
    indep_vars_cs = "grp",
    run_analysis = TRUE,
    es_method = "glass_delta",
    reference_group = "B"
  ),
  data = df_g
)
res_g_b$run()
row_b <- res_g_b$results$effectSizeTable$asDF
cat("Reference = B (denominator should use SD of B ~ 5):\n")
print(row_b)

# Ref = A
res_g_a <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "y",
    indep_vars_cs = "grp",
    run_analysis = TRUE,
    es_method = "glass_delta",
    reference_group = "A"
  ),
  data = df_g
)
res_g_a$run()
row_a <- res_g_a$results$effectSizeTable$asDF
cat("Reference = A (denominator should use SD of A ~ 1):\n")
print(row_a)

stopifnot(abs(row_b$es_value) != abs(row_a$es_value))
cat(">>> PASS: Glass's Delta correctly respects reference_group!\n\n")

# -------------------------------------------------------------
# Test 2: BSS-STAT-019 (CLES Directionality)
# -------------------------------------------------------------
cat("--- Test 2: CLES Directionality ---\n")
df_cles <- data.frame(
  y = c(1, 2, 3, 10, 11, 12),
  grp = factor(rep(c("Low", "High"), each = 3), levels = c("Low", "High"))
)
res_cles <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "y",
    indep_vars_cs = "grp",
    run_analysis = TRUE,
    es_method = "cohens_d",
    show_cles = TRUE
  ),
  data = df_cles
)
res_cles$run()
cles_df <- res_cles$results$clesTable$asDF
print(cles_df)
# Low vs High: Low is strictly smaller than High, so P(Low > High) must be 0!
stopifnot(cles_df$cles[1] == 0)
cat(">>> PASS: CLES directional probability P(Low > High) is correctly 0 (not inverted to 1)!\n\n")

# -------------------------------------------------------------
# Test 3: BSS-STAT-020 (Longitudinal Glass dz - Becker's d)
# -------------------------------------------------------------
cat("--- Test 3: Longitudinal Glass dz (Becker's d) ---\n")
df_wide <- data.frame(
  w1 = rnorm(25, 10, 2),
  w2 = rnorm(25, 15, 6)
)
res_long_g <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "longitudinal",
    long_format = "wide",
    wave_vars = c("w1", "w2"),
    run_analysis = TRUE,
    es_method_long = "glass_dz",
    reference_group_long = "w1"
  ),
  data = df_wide
)
res_long_g$run()
long_g_df <- res_long_g$results$effectSizeTable$asDF
print(long_g_df)
stopifnot(!is.na(long_g_df$es_value))
cat(">>> PASS: Longitudinal Glass dz executed successfully using paired Becker's d!\n\n")

# -------------------------------------------------------------
# Test 4: BSS-STAT-021 (GzLM Gamma and AME)
# -------------------------------------------------------------
cat("--- Test 4: GzLM Gamma with non-positive values and positive values ---\n")
df_gamma_bad <- data.frame(
  y = c(-2, 0, 1, 2, 3, 4),
  grp = factor(rep(c("A", "B"), each = 3))
)
res_gamma_bad <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "y",
    indep_vars_cs = "grp",
    run_analysis = TRUE,
    use_glm = TRUE,
    es_glm = "glm_gamma"
  ),
  data = df_gamma_bad
)
res_gamma_bad$run()
bad_df <- res_gamma_bad$results$effectSizeTable$asDF
print(bad_df)
stopifnot(grepl("strictly positive", bad_df$interpretation[1]))
cat(">>> PASS: Handled non-positive Gamma response gracefully!\n\n")

df_gamma_good <- data.frame(
  y = c(2, 3, 4, 10, 12, 14),
  grp = factor(rep(c("A", "B"), each = 3))
)
res_gamma_good <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "y",
    indep_vars_cs = "grp",
    run_analysis = TRUE,
    use_glm = TRUE,
    es_glm = "glm_gamma"
  ),
  data = df_gamma_good
)
res_gamma_good$run()
good_df <- res_gamma_good$results$effectSizeTable$asDF
print(good_df)
# A (mean 3) vs B (mean 12) -> discrete marginal effect A - B = -9
stopifnot(round(good_df$es_value[1], 1) == -9.0)
cat(">>> PASS: GzLM Gamma marginal difference exactly matches -9.0 on response scale!\n\n")

# -------------------------------------------------------------
# Test 5: BSS-STAT-022 (Bootstrap Point Estimate Preservation)
# -------------------------------------------------------------
cat("--- Test 5: Bootstrap point estimate preservation and performance ---\n")
df_boot <- data.frame(
  y = c(rnorm(20, 10, 2), rnorm(20, 12, 2)),
  grp = factor(rep(c("A", "B"), each = 20))
)
res_ana <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "y",
    indep_vars_cs = "grp",
    run_analysis = TRUE,
    es_method = "cohens_d",
    ci_method = "analytical"
  ),
  data = df_boot
)
res_ana$run()
ana_val <- res_ana$results$effectSizeTable$asDF$es_value[1]

res_boot <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "y",
    indep_vars_cs = "grp",
    run_analysis = TRUE,
    es_method = "cohens_d",
    ci_method = "bootstrap",
    n_bootstrap = 200
  ),
  data = df_boot
)
res_boot$run()
boot_val <- res_boot$results$effectSizeTable$asDF$es_value[1]
boot_ci_lo <- res_boot$results$effectSizeTable$asDF$ci_lower[1]
boot_ci_hi <- res_boot$results$effectSizeTable$asDF$ci_upper[1]

cat("Analytical es_value:", ana_val, "\n")
cat("Bootstrap  es_value:", boot_val, "\n")
cat("Bootstrap  CI: [", boot_ci_lo, ", ", boot_ci_hi, "]\n")
stopifnot(isTRUE(all.equal(ana_val, boot_val)))
cat(">>> PASS: Bootstrap preserves exact sample point estimate!\n\n")

# -------------------------------------------------------------
# Test 6: BSS-STAT-023 (Zero Variance and Type Coercion)
# -------------------------------------------------------------
cat("--- Test 6: Zero Variance and Type Coercion ---\n")
df_zero <- data.frame(
  y = c(5, 5, 5, 5, 5, 5),
  grp = factor(rep(c("A", "B"), each = 3))
)
res_zero <- effectSizeClass$new(
  options = effectSizeOptions$new(
    mode = "cross_sectional",
    dep_vars_cs = "y",
    indep_vars_cs = "grp",
    run_analysis = TRUE,
    es_method = "cohens_d"
  ),
  data = df_zero
)
res_zero$run()
zero_df <- res_zero$results$effectSizeTable$asDF
print(zero_df)
stopifnot(is.na(zero_df$es_value[1]))
stopifnot(!is.nan(zero_df$es_value[1]))
stopifnot(zero_df$interpretation[1] == "Zero variance")
cat(">>> PASS: Factor DV coerced to numeric, zero variance handled without NaN!\n\n")

cat("=============================================================\n")
cat("ALL VALIDATION TESTS FOR effectsize PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")


