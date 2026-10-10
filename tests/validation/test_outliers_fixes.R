# =============================================================
# Script de Validacao Independente - Ciclo 7: outliers
# Tickets: BSS-CODE-009, BSS-STAT-027, BSS-STAT-028, BSS-STAT-029, BSS-CODE-010
# =============================================================

suppressPackageStartupMessages({
  library(jmvcore)
  library(R6)
})

# Carregar classe base e implementacao
source("R/outliers.h.R")
source("R/outliers.b.R")

cat("=============================================================\n")
cat("TEST SUITE: BSS-CODE-009, BSS-STAT-027 a 029 (outliers)\n")
cat("=============================================================\n\n")

# -------------------------------------------------------------
# Test 1: BSS-CODE-009 (Modified Z-Score crash when MAD = 0)
# -------------------------------------------------------------
cat("--- Test 1: BSS-CODE-009: Modified Z-Score with MAD = 0 ---\n")
# Vetor com maioria identica (MAD = 0) e um outlier extremo
df_mad0 <- data.frame(
  v = c(2, 2, 2, 2, 2, 2, 20)
)
res_mad0 <- outliersClass$new(
  options = outliersOptions$new(
    mode = "modz",
    vars_modz = "v",
    threshold_modz = "t35"
  ),
  data = df_mad0
)
res_mad0$run()
modz_df <- res_mad0$results$modzTable$asDF
print(modz_df)
out_df <- res_mad0$results$modzOutlierList$asDF
print(out_df)
stopifnot(modz_df$numOutliers[1] == 1)
stopifnot(out_df$rowNumber[1] == 7)
cat(">>> PASS: BSS-CODE-009 fixed! MAD = 0 handled gracefully without stats::mean crash.\n\n")

# -------------------------------------------------------------
# Test 2: BSS-STAT-027 (Factor DV Coercion vs Factor Indices)
# -------------------------------------------------------------
cat("--- Test 2: BSS-STAT-027: Factor DV Coercion to Real Values ---\n")
# Factor with numbers: c("10", "12", "11", "13", "12", "11", "100")
# If coerced with as.numeric(factor), values become 1, 3, 2, 4, 3, 2, 5 (no extreme outlier!)
# With .to_numeric_vec, value 100 is recognized as an extreme outlier!
raw_fac <- factor(c("10", "12", "11", "13", "12", "11", "100"))
# Extrair o helper privado da classe para testar diretamente
to_num <- outliersClass$private_methods$.to_numeric_vec
coerced_vals <- to_num(raw_fac)
cat("Valores originais como factor:\n")
print(raw_fac)
cat("Valores apos .to_numeric_vec:\n")
print(coerced_vals)
stopifnot(identical(coerced_vals, c(10, 12, 11, 13, 12, 11, 100)))

# Testar agora execucao com numeric
df_num <- data.frame(v = coerced_vals)
res_fac <- outliersClass$new(
  options = outliersOptions$new(
    mode = "iqr",
    vars_iqr = "v",
    iqrMultiplier_iqr = "iqr15"
  ),
  data = df_num
)
res_fac$run()
iqr_df <- res_fac$results$iqrTable$asDF
print(iqr_df)
stopifnot(iqr_df$numOutliers[1] == 1)
stopifnot(iqr_df$upperFence[1] < 100)
cat(">>> PASS: Factor values cleanly coerced to numeric! Real outlier 100 detected.\n\n")

# -------------------------------------------------------------
# Test 3: BSS-STAT-028 (Character Rownames and Row Indexing)
# -------------------------------------------------------------
cat("--- Test 3: BSS-STAT-028: Character Rownames and Row Indexing ---\n")
df_char_rows <- data.frame(
  val = c(10, 11, 12, 11, 30),
  grp = factor(rep("G1", 5))
)
rownames(df_char_rows) <- paste0("Patient_ID_", 101:105)

res_dixon <- outliersClass$new(
  options = outliersOptions$new(
    mode = "dixon",
    var_dixon = "val",
    group_dixon = "grp",
    alpha_dixon = "alpha05",
    treatment_dixon = "trim_asym"
  ),
  data = df_char_rows
)
res_dixon$run()
dixon_out_df <- res_dixon$results$dixonOutlierList$asDF
print(dixon_out_df)
# Row 5 is the outlier (Patient_ID_105)
stopifnot(!is.na(dixon_out_df$rowNumber[1]))
stopifnot(dixon_out_df$rowNumber[1] == 5)
cat(">>> PASS: Row numbers correctly tracked as integer 5 despite character rownames!\n\n")

# -------------------------------------------------------------
# Test 4: BSS-STAT-029 (Zero Variance and Identical Values)
# -------------------------------------------------------------
cat("--- Test 4: BSS-STAT-029: Identical Values Handling in Dixon's Q ---\n")
df_ident <- data.frame(
  val = c(5, 5, 5, 5, 5)
)
res_ident <- outliersClass$new(
  options = outliersOptions$new(
    mode = "dixon",
    var_dixon = "val"
  ),
  data = df_ident
)
res_ident$run()
dixon_ident_df <- res_ident$results$dixonTable$asDF
print(dixon_ident_df)
stopifnot(dixon_ident_df$significant[1] == "All values identical")
cat(">>> PASS: Zero variance / identical values reported clearly without crash!\n\n")

cat("=============================================================\n")
cat("ALL VALIDATION TESTS FOR outliers PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")
