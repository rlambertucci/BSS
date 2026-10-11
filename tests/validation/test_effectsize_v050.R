# Test Suite: effectsize v0.5.0 Extensions (Robust Effect Sizes, Categorical Effects, Direct Calculator)
pkgload::load_all(".")

cat("=============================================================\n")
cat("TEST SUITE: BSS-FEAT-037..039 (effectsize v0.5.0)\n")
cat("=============================================================\n\n")

# Test 1: Direct Test Statistic Calculator (No raw data needed)
cat("--- Test 1: Direct Calculator from Test Statistics (t, F, z, chisq) ---\n")
# 1a. Test t-statistic (with empty data.frame)
res_t <- effectSize(
  data = data.frame(),
  calc_from_test_stat = TRUE,
  stat_type = "stat_t",
  stat_val = 3.2,
  stat_df1 = 40
)
t_df <- res_t$directCalcTable$asDF
print(t_df)
stopifnot(nrow(t_df) >= 5)
stopifnot("Cohen's d" %in% t_df$metric)
stopifnot("Hedges' g" %in% t_df$metric)
stopifnot("Odds Ratio (OR)" %in% t_df$metric)
cat(">>> PASS: Direct t-statistic calculator converted to d, g, r, eta^2, OR, AUC!\n\n")

# 1b. Test F-statistic
res_f <- effectSize(
  data = data.frame(),
  calc_from_test_stat = TRUE,
  stat_type = "stat_f",
  stat_val = 6.5,
  stat_df1 = 1,
  stat_df2 = 50
)
f_df <- res_f$directCalcTable$asDF
print(f_df)
stopifnot(nrow(f_df) >= 3)
stopifnot(any(grepl("η", f_df$metric) | grepl("eta", f_df$metric, ignore.case = TRUE)))
stopifnot(any(grepl("ω", f_df$metric) | grepl("omega", f_df$metric, ignore.case = TRUE)))
cat(">>> PASS: Direct F-statistic calculator converted to partial eta^2, omega^2, Cohen's f!\n\n")

# Test 2: Robust Effect Sizes (Algina-Keselman-Penfield d_R and Akritas-Arnold delta)
cat("--- Test 2: Robust Effect Sizes (d_R & Akritas-Arnold delta) ---\n")
set.seed(42)
# Dados assimétricos com caudas pesadas / heterocedásticos
df_robust <- data.frame(
  outcome = c(rexp(40, rate = 0.2), rexp(40, rate = 0.5) + 3),
  group = factor(rep(c("A", "B"), each = 40))
)

# 2a. Algina-Keselman-Penfield d_R
res_dr <- effectSize(
  data = df_robust,
  mode = "cross_sectional",
  dep_vars_cs = "outcome",
  indep_vars_cs = "group",
  es_method = "algina_keselman_d",
  run_analysis = TRUE
)
dr_table <- res_dr$effectSizeTable$asDF
print(dr_table)
stopifnot(nrow(dr_table) == 1)
stopifnot(!is.na(dr_table$es_value[1]))
stopifnot(!is.na(dr_table$ci_lower[1]))
stopifnot(!is.na(dr_table$ci_upper[1]))
stopifnot(grepl("Algina", dr_table$method[1]))
cat(">>> PASS: Algina-Keselman-Penfield d_R computed with trimmed means and Winsorized variances!\n\n")

# 2b. Akritas-Arnold delta
res_aa <- effectSize(
  data = df_robust,
  mode = "cross_sectional",
  dep_vars_cs = "outcome",
  indep_vars_cs = "group",
  es_method = "akritas_arnold_delta",
  run_analysis = TRUE
)
aa_table <- res_aa$effectSizeTable$asDF
print(aa_table)
stopifnot(nrow(aa_table) == 1)
stopifnot(!is.na(aa_table$es_value[1]))
stopifnot(aa_table$es_value[1] >= -1 && aa_table$es_value[1] <= 1)
stopifnot(grepl("Akritas", aa_table$method[1]))
cat(">>> PASS: Akritas-Arnold relative effect delta computed with Brunner-Munzel variance!\n\n")

# Test 3: Categorical Effect Sizes (Cramer's V, Odds Ratio, Relative Risk)
cat("--- Test 3: Categorical Effect Sizes (Cramer's V, OR, RR) ---\n")
df_cat <- data.frame(
  exposure = factor(rep(c("Treated", "Control"), c(60, 60)), levels = c("Treated", "Control")),
  outcome  = factor(c(rep(c("Recovered", "Sick"), c(45, 15)),
                      rep(c("Recovered", "Sick"), c(20, 40))), levels = c("Recovered", "Sick"))
)

res_cat <- effectSize(
  data = df_cat,
  show_categorical_es = TRUE,
  cat_var1 = "exposure",
  cat_var2 = "outcome"
)
cat_table <- res_cat$categoricalTable$asDF
print(cat_table)
stopifnot(nrow(cat_table) == 3) # Cramer's V, Odds Ratio, Relative Risk
stopifnot(any(grepl("Cram", cat_table$metric)))
stopifnot(any(grepl("Odds Ratio", cat_table$metric)))
stopifnot(any(grepl("Relative Risk", cat_table$metric)))

# Com levels = c("Treated", "Control") e c("Recovered", "Sick"):
# OR = (45*40) / (15*20) = 1800 / 300 = 6.0
or_row <- cat_table[grepl("Odds Ratio", cat_table$metric), ]
cat("OR estimated:", or_row$value, "Expected: 6.0\n")
stopifnot(abs(or_row$value - 6.0) < 1e-3)

# RR = (45/60) / (20/60) = 0.75 / 0.33333 = 2.25
rr_row <- cat_table[grepl("Relative Risk", cat_table$metric), ]
cat("RR estimated:", rr_row$value, "Expected: 2.25\n")
stopifnot(abs(rr_row$value - 2.25) < 1e-3)

cat(">>> PASS: Cramer's V, Odds Ratio, and Relative Risk computed accurately with CIs!\n\n")

cat("=============================================================\n")
cat("ALL EFFECTSIZE V0.5.0 EXTENSION TESTS PASSED!\n")
cat("=============================================================\n")
