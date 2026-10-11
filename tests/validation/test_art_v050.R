# Test Suite: art v0.5.0 Extensions (Nonparametric Contrast Effect Sizes & 3-Way Within Designs)
pkgload::load_all(".")

cat("=============================================================\n")
cat("TEST SUITE: BSS-FEAT-035..036 (art v0.5.0)\n")
cat("=============================================================\n\n")

# Test 1: Nonparametric effect sizes in pairwise contrasts (Cross-sectional)
cat("--- Test 1: Nonparametric Contrast Effect Sizes (Vargha-Delaney A & Rank-Biserial r_rb) ---\n")
set.seed(42)
n_per_group <- 30
df_cs <- data.frame(
  score = c(rnorm(n_per_group, 10, 2), rnorm(n_per_group, 14, 2), rnorm(n_per_group, 12, 2)),
  group = factor(rep(c("G1", "G2", "G3"), each = n_per_group))
)

res_art_cs <- art(
  data = df_cs,
  mode = "cross_sectional",
  dep_var_cs = "score",
  fixed_factors_cs = "group",
  run_analysis = TRUE,
  run_posthoc = TRUE,
  calc_nonparametric_es = TRUE,
  calc_es = TRUE,
  es_method = "all"
)

ph_df <- res_art_cs$posthocTable$asDF
print(ph_df)
stopifnot(nrow(ph_df) >= 3)
stopifnot("vd_a" %in% names(ph_df))
stopifnot("r_rb" %in% names(ph_df))
# Conferir se A e r_rb são números válidos e respeitam r_rb = 2A - 1
stopifnot(all(!is.na(ph_df$vd_a)))
stopifnot(all(!is.na(ph_df$r_rb)))
stopifnot(all(abs(ph_df$r_rb - (2 * ph_df$vd_a - 1)) < 1e-4))
cat(">>> PASS: Vargha-Delaney A and Rank-Biserial r_rb computed and verified in posthoc table!\n\n")

# Conferir effectSizeTable com 'all'
es_df <- res_art_cs$effectSizeTable$asDF
print(head(es_df, 8))
stopifnot(nrow(es_df) >= 12)
stopifnot(any(grepl("Vargha", es_df$method)))
stopifnot(any(grepl("Rank-Biserial", es_df$method)))
cat(">>> PASS: Pairwise effect size table contains all effect sizes!\n\n")

# Test 2: 3-Way Pure Within-Subject Design (A x B x C)
cat("--- Test 2: 3-Way Pure Within-Subjects Design (A x B x C) ---\n")
set.seed(123)
n_subj <- 15
df_3w <- expand.grid(
  Subject = factor(paste0("S", 1:n_subj)),
  CondA = factor(c("A1", "A2")),
  CondB = factor(c("B1", "B2")),
  CondC = factor(c("C1", "C2"))
)
# Injetar efeito em CondA e interação CondB:CondC
df_3w$DV <- rnorm(nrow(df_3w)) + (as.numeric(df_3w$CondA) * 1.5)

res_art_3w <- art(
  data = df_3w,
  mode = "longitudinal",
  long_format = "long",
  dep_var_long = "DV",
  within_factors_long = c("CondA", "CondB", "CondC"),
  subject_id_var = "Subject",
  run_analysis = TRUE,
  run_posthoc = TRUE,
  calc_nonparametric_es = TRUE
)

anova_3w <- res_art_3w$anovaTable$asDF
print(anova_3w)
stopifnot(nrow(anova_3w) == 7) # 3 principais + 3 interações 2-vias + 1 interação 3-vias
stopifnot(all(c("CondA", "CondB", "CondC", "CondA:CondB", "CondA:CondC", "CondB:CondC", "CondA:CondB:CondC") %in% anova_3w$factor))

# Verificar posthocs com medidas repetidas pareadas
ph_3w <- res_art_3w$posthocTable$asDF
print(head(ph_3w, 10))
stopifnot(nrow(ph_3w) > 0)
stopifnot(!is.na(ph_3w$vd_a[1]))
stopifnot(!is.na(ph_3w$r_rb[1]))
cat(">>> PASS: 3-Way Within Design executed smoothly with full 7 ANOVA terms and pairwise contrasts!\n\n")

cat("=============================================================\n")
cat("ALL ART V0.5.0 EXTENSION TESTS PASSED!\n")
cat("=============================================================\n")
