library(jmvcore)
source('R/samplesize.h.R')
source('R/samplesize.b.R')

cat("=============================================================\n")
cat("TEST SUITE: BSS-STAT-024 a 026 e BSS-CODE-008 (samplesize)\n")
cat("=============================================================\n\n")

# -------------------------------------------------------------
# Test 1: BSS-STAT-025 (Finite Population: Cochran and Slovin)
# -------------------------------------------------------------
cat("--- Test 1: Cochran Finite Population Calculation ---\n")
res_cochran <- samplesizeClass$new(
  data = data.frame(), options = samplesizeOptions$new(
    mode = "finite",
    method_finite = "Cochran",
    population_size_finite = 500,
    confidence_level_finite = 95,
    margin_error_finite = 5,
    population_proportion_finite = 0.5
  )
)
res_cochran$run()
cochran_df <- res_cochran$results$sampleSizeTable_finite$asDF
print(cochran_df)
# Z = 1.96, n0 = 384.15. n = 384.15 / (1 + 383.15/500) = 384.15 / 1.7663 = 217.48 -> 218
stopifnot(cochran_df$sample_size[1] == 218)
cat(">>> PASS: Cochran calculation matches theoretical FPC (n = 218)!\n\n")

cat("--- Test 1b: Slovin Formula and Confidence Level Transparency ---\n")
res_slovin <- samplesizeClass$new(
  data = data.frame(), options = samplesizeOptions$new(
    mode = "finite",
    method_finite = "Slovin",
    population_size_finite = 1000,
    confidence_level_finite = 99,
    margin_error_finite = 5
  )
)
res_slovin$run()
slovin_df <- res_slovin$results$sampleSizeTable_finite$asDF
print(slovin_df)
# n = 1000 / (1 + 1000 * 0.05^2) = 1000 / 3.5 = 285.71 -> 286
stopifnot(slovin_df$sample_size[1] == 286)
stopifnot(is.na(slovin_df$confidence_level[1])) # Must be NA for Slovin
cat(">>> PASS: Slovin calculation correct (n = 286) and confidence_level reported as NA!\n\n")

# -------------------------------------------------------------
# Test 2: BSS-STAT-024 (Animal Design 1: One-way ANOVA)
# -------------------------------------------------------------
cat("--- Test 2: Animal Design 1 (One-way ANOVA, k = 3) ---\n")
res_d1 <- samplesizeClass$new(
  data = data.frame(), options = samplesizeOptions$new(
    mode = "animals",
    design_animals = "Design 1 (One-way ANOVA)",
    nGroups_animals = 3
  )
)
res_d1$run()
d1_df <- res_d1$results$sampleSizeTable_animals$asDF
print(d1_df)
# k = 3: 10/3 + 1 = 4.33 -> min_n = 5; 20/3 + 1 = 7.67 -> max_n = 7
# min_total = 15; max_total = 21
min_n_val <- as.integer(d1_df$value[d1_df$parameter == "Minimum Sample Size per Group"])
max_n_val <- as.integer(d1_df$value[d1_df$parameter == "Maximum Sample Size per Group"])
min_tot_val <- as.integer(d1_df$value[d1_df$parameter == "Minimum Total Sample Size"])
max_tot_val <- as.integer(d1_df$value[d1_df$parameter == "Maximum Total Sample Size"])
stopifnot(min_n_val == 5, max_n_val == 7, min_tot_val == 15, max_tot_val == 21)
cat(">>> PASS: Design 1 One-way ANOVA mathematically correct (5-7 per group, 15-21 total)!\n\n")

# -------------------------------------------------------------
# Test 3: BSS-STAT-024 (Animal Design 2: Repeated Measures)
# -------------------------------------------------------------
cat("--- Test 3a: Animal Design 2 (Repeated Measures, r = 4, No Sacrifice) ---\n")
res_d2_nosac <- samplesizeClass$new(
  data = data.frame(), options = samplesizeOptions$new(
    mode = "animals",
    design_animals = "Design 2 (Repeated Measures)",
    nMeasures_animals = 4,
    sacrifice_animals = FALSE
  )
)
res_d2_nosac$run()
d2_nosac_df <- res_d2_nosac$results$sampleSizeTable_animals$asDF
print(d2_nosac_df)
# r = 4: E = (n-1)(r-1) -> 10/(4-1) + 1 = 4.33 -> min_n = 5; 20/(4-1) + 1 = 7.67 -> max_n = 7
# Single cohort: min_tot = 5; max_tot = 7
min_n_d2 <- as.integer(d2_nosac_df$value[d2_nosac_df$parameter == "Minimum Sample Size per Cohort"])
max_n_d2 <- as.integer(d2_nosac_df$value[d2_nosac_df$parameter == "Maximum Sample Size per Cohort"])
min_tot_d2 <- as.integer(d2_nosac_df$value[d2_nosac_df$parameter == "Minimum Total Sample Size"])
max_tot_d2 <- as.integer(d2_nosac_df$value[d2_nosac_df$parameter == "Maximum Total Sample Size"])
stopifnot(min_n_d2 == 5, max_n_d2 == 7, min_tot_d2 == 5, max_tot_d2 == 7)
cat(">>> PASS: Design 2 without sacrifice mathematically correct (single cohort of 5-7 animals)!\n\n")

cat("--- Test 3b: Animal Design 2 (Repeated Measures, r = 4, With Sacrifice) ---\n")
res_d2_sac <- samplesizeClass$new(
  data = data.frame(), options = samplesizeOptions$new(
    mode = "animals",
    design_animals = "Design 2 (Repeated Measures)",
    nMeasures_animals = 4,
    sacrifice_animals = TRUE
  )
)
res_d2_sac$run()
d2_sac_df <- res_d2_sac$results$sampleSizeTable_animals$asDF
print(d2_sac_df)
# r = 4 independent cohorts: E = r(n-1) -> 10/4 + 1 = 3.5 -> min_n_cohort = 4; max_n_cohort = 6
# Total = 4 * 4 = 16 to 4 * 6 = 24
min_n_sac <- as.integer(d2_sac_df$value[d2_sac_df$parameter == "Minimum Sample Size per Time Point"])
max_n_sac <- as.integer(d2_sac_df$value[d2_sac_df$parameter == "Maximum Sample Size per Time Point"])
min_tot_sac <- as.integer(d2_sac_df$value[d2_sac_df$parameter == "Minimum Total Sample Size"])
max_tot_sac <- as.integer(d2_sac_df$value[d2_sac_df$parameter == "Maximum Total Sample Size"])
stopifnot(min_n_sac == 4, max_n_sac == 6, min_tot_sac == 16, max_tot_sac == 24)
cat(">>> PASS: Design 2 with sacrifice algebraically correct (4-6 per time point, 16-24 total)!\n\n")

# -------------------------------------------------------------
# Test 4: BSS-STAT-024 (Animal Design 3: Mixed ANOVA)
# -------------------------------------------------------------
cat("--- Test 4a: Animal Design 3 (Mixed ANOVA, k = 2, r = 3, No Sacrifice) ---\n")
res_d3_nosac <- samplesizeClass$new(
  data = data.frame(), options = samplesizeOptions$new(
    mode = "animals",
    design_animals = "Design 3 (Mixed ANOVA)",
    nGroups_animals = 2,
    nMeasures_animals = 3,
    sacrifice_animals = FALSE
  )
)
res_d3_nosac$run()
d3_nosac_df <- res_d3_nosac$results$sampleSizeTable_animals$asDF
print(d3_nosac_df)
# k = 2, r = 3: E_within = k(n-1)(r-1) = 2*(r-1) = 4*(n-1)
# 10/4 + 1 = 3.5 -> min_n = 4; 20/4 + 1 = 6 -> max_n = 6
# Total = 2 * 4 = 8 to 2 * 6 = 12
min_n_d3 <- as.integer(d3_nosac_df$value[d3_nosac_df$parameter == "Minimum Sample Size per Group"])
max_n_d3 <- as.integer(d3_nosac_df$value[d3_nosac_df$parameter == "Maximum Sample Size per Group"])
min_tot_d3 <- as.integer(d3_nosac_df$value[d3_nosac_df$parameter == "Minimum Total Sample Size"])
max_tot_d3 <- as.integer(d3_nosac_df$value[d3_nosac_df$parameter == "Maximum Total Sample Size"])
stopifnot(min_n_d3 == 4, max_n_d3 == 6, min_tot_d3 == 8, max_tot_d3 == 12)
cat(">>> PASS: Design 3 without sacrifice mathematically correct (4-6 per group, 8-12 total)!\n\n")

cat("--- Test 4b: Animal Design 3 (Mixed ANOVA, k = 2, r = 3, With Sacrifice) ---\n")
res_d3_sac <- samplesizeClass$new(
  data = data.frame(), options = samplesizeOptions$new(
    mode = "animals",
    design_animals = "Design 3 (Mixed ANOVA)",
    nGroups_animals = 2,
    nMeasures_animals = 3,
    sacrifice_animals = TRUE
  )
)
res_d3_sac$run()
d3_sac_df <- res_d3_sac$results$sampleSizeTable_animals$asDF
print(d3_sac_df)
# Factorial k*r = 6 cells: 10/6 + 1 = 2.67 -> min_cell = 3; 20/6 + 1 = 4.33 -> max_cell = 4
# Per group (3 times): 3 * 3 = 9 to 4 * 3 = 12
# Total: 2 * 9 = 18 to 2 * 12 = 24
min_tot_d3_sac <- as.integer(d3_sac_df$value[d3_sac_df$parameter == "Minimum Total Sample Size"])
max_tot_d3_sac <- as.integer(d3_sac_df$value[d3_sac_df$parameter == "Maximum Total Sample Size"])
stopifnot(min_tot_d3_sac == 18, max_tot_d3_sac == 24)
cat(">>> PASS: Design 3 with sacrifice algebraically correct (18-24 total animals)!\n\n")

# -------------------------------------------------------------
# Test 5: BSS-STAT-026 (Clean message on invalid input)
# -------------------------------------------------------------
cat("--- Test 5: Clean error handling without jmvcore::reject exception ---\n")
res_err <- samplesizeClass$new(
  data = data.frame(), options = samplesizeOptions$new(
    mode = "finite",
    method_finite = "Cochran",
    population_size_finite = -10
  )
)
res_err$run()
cat("Error message produced cleanly:", res_err$results$text_finite$content, "\n")
stopifnot(grepl("positive integer", res_err$results$text_finite$content))
cat(">>> PASS: Graceful UI text notification handled without exception!\n\n")

cat("=============================================================\n")
cat("ALL VALIDATION TESTS FOR samplesize PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")

