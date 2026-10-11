# Test validation for samplesize extensions (BSS-FEAT-016..018)
library(jmvcore)
source("R/samplesize.h.R")
source("R/samplesize.b.R")

cat("1. Testing Power & Sample Size for t-test (power_t)...\n")
res_pwr <- samplesizeClass$new(
  data = data.frame(),
  options = samplesizeOptions$new(
    mode = "power_t",
    t_test_type = "two_sample",
    t_effect_size = 0.5,
    t_alpha = 0.05,
    t_power = 0.80,
    show_power_curve = TRUE
  )
)
res_pwr$run()

df_pwr <- res_pwr$results$sampleSizeTable_power_t$asDF
print(df_pwr)
stopifnot(nrow(df_pwr) == 1)
stopifnot(df_pwr$n_per_group == 64)
stopifnot(df_pwr$total_n == 128)

state_pwr <- res_pwr$results$powerCurvePlot$state
stopifnot(!is.null(state_pwr))
stopifnot(nrow(state_pwr$curves) > 0)
cat("Power t-test and sensitivity curve validated successfully!\n\n")

cat("2. Testing Equivalence Testing (TOST)...\n")
res_tost <- samplesizeClass$new(
  data = data.frame(),
  options = samplesizeOptions$new(
    mode = "tost",
    tost_type = "two_sample",
    tost_eq_bound = 0.5,
    tost_true_diff = 0.0,
    tost_alpha = 0.05,
    tost_power = 0.80
  )
)
res_tost$run()

df_tost <- res_tost$results$sampleSizeTable_tost$asDF
print(df_tost)
stopifnot(nrow(df_tost) == 1)
stopifnot(df_tost$n_per_group >= 60)
cat("TOST validated successfully!\n\n")

cat("3. Testing Proportions (two_prop, one_prop, mcnemar)...\n")
res_prop2 <- samplesizeClass$new(
  data = data.frame(),
  options = samplesizeOptions$new(
    mode = "proportions",
    prop_test_type = "two_prop",
    prop_p1 = 0.60,
    prop_p2 = 0.40,
    prop_alpha = 0.05,
    prop_power = 0.80
  )
)
res_prop2$run()
df_prop2 <- res_prop2$results$sampleSizeTable_proportions$asDF
print(df_prop2)
stopifnot(nrow(df_prop2) == 1)
stopifnot(df_prop2$n_per_group >= 90)

res_mcnemar <- samplesizeClass$new(
  data = data.frame(),
  options = samplesizeOptions$new(
    mode = "proportions",
    prop_test_type = "mcnemar",
    prop_p12 = 0.25,
    prop_p21 = 0.10,
    prop_alpha = 0.05,
    prop_power = 0.80
  )
)
res_mcnemar$run()
df_mcnemar <- res_mcnemar$results$sampleSizeTable_proportions$asDF
print(df_mcnemar)
stopifnot(nrow(df_mcnemar) == 1)
stopifnot(df_mcnemar$n_per_group > 0)
cat("Proportions validated successfully!\n\n")

cat("4. Testing Backward Compatibility (finite & animals)...\n")
res_fin <- samplesizeClass$new(
  data = data.frame(),
  options = samplesizeOptions$new(
    mode = "finite",
    method_finite = "Cochran",
    population_size_finite = 1000,
    confidence_level_finite = 95,
    margin_error_finite = 5,
    population_proportion_finite = 0.5
  )
)
res_fin$run()
df_fin <- res_fin$results$sampleSizeTable_finite$asDF
print(df_fin)
stopifnot(df_fin$sample_size == 278)

res_anim <- samplesizeClass$new(
  data = data.frame(),
  options = samplesizeOptions$new(
    mode = "animals",
    design_animals = "Design 1 (One-way ANOVA)",
    nGroups_animals = 4
  )
)
res_anim$run()
df_anim <- res_anim$results$sampleSizeTable_animals$asDF
print(df_anim)
stopifnot(nrow(df_anim) >= 4)
cat("Backward compatibility validated successfully!\n\n")

cat("ALL SAMPLESIZE EXTENSION TESTS PASSED!\n")
