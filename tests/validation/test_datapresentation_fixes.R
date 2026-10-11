# =============================================================
# Script de Validacao Independente - Ciclo 9: datapresentation
# Tickets: BSS-STAT-033, BSS-STAT-034, BSS-CODE-013, BSS-CODE-014, BSS-CODE-015
# =============================================================

suppressPackageStartupMessages({
  library(jmvcore)
  library(R6)
  library(dplyr)
  library(tidyr)
  library(emmeans)
})

# Carregar classe base e implementacao
source("R/datapresentation.h.R")
source("R/datapresentation.b.R")

cat("=============================================================\n")
cat("TEST SUITE: BSS-STAT-033 a 034, BSS-CODE-013 a 015 (datapresentation)\n")
cat("=============================================================\n\n")

# -------------------------------------------------------------
# Test 1: BSS-STAT-033 (Continuous variable coercion from factor/character)
# -------------------------------------------------------------
cat("--- Test 1: BSS-STAT-033: Coercion of DV from Factor/Character ---\n")
to_num <- DataPresentationClass$private_methods$.to_numeric_vec
raw_fac <- factor(c("10.5", "12.3", "11.1", "13.4", "12.0", "11.8", "14.2", "15.0"))
coerced_vals <- to_num(raw_fac)
cat("Valores convertidos via .to_numeric_vec:\n")
print(coerced_vals)
stopifnot(identical(coerced_vals, c(10.5, 12.3, 11.1, 13.4, 12.0, 11.8, 14.2, 15.0)))

df_dv <- data.frame(
  dv1 = coerced_vals,
  group = factor(rep(c("Control", "Treatment"), each = 4))
)

res_cs <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "cross_sectional",
    deps = "dv1",
    group = "group",
    showTable = TRUE,
    tableFormat = "apa"
  ),
  data = df_dv
)
res_cs$run()
tbl_cs <- res_cs$results$descriptiveTable$asDF
print(tbl_cs)

stopifnot(nrow(tbl_cs) == 2)
stopifnot(!is.na(tbl_cs$mean[1]))
stopifnot(abs(tbl_cs$mean[1] - 11.825) < 0.01) # Média de Control: (10.5+12.3+11.1+13.4)/4 = 11.825
cat(">>> PASS: Factor DV successfully coerced to numeric, exact means computed!\n\n")

# -------------------------------------------------------------
# Test 2: BSS-STAT-034 (Longitudinal design with Covariate)
# -------------------------------------------------------------
cat("--- Test 2: BSS-STAT-034: Longitudinal Design with Covariate ---\n")
df_long <- data.frame(
  pre = c(10, 12, 11, 14, 15, 16),
  post = c(14, 15, 16, 19, 20, 22),
  cov = c(1, 2, 1, 3, 2, 4)
)

res_long <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "longitudinal",
    rm_vars = c("pre", "post"),
    cov_long = "cov",
    showTable = TRUE,
    tableFormat = "apa"
  ),
  data = df_long
)
res_long$run()
tbl_long <- res_long$results$descriptiveTable$asDF
print(tbl_long)

stopifnot(nrow(tbl_long) == 2)
stopifnot(tbl_long$variable[1] == "pre" && tbl_long$variable[2] == "post")
stopifnot(!is.na(tbl_long$mean[1]))
cat(">>> PASS: Longitudinal with covariate runs cleanly with deterministic time order!\n\n")

# -------------------------------------------------------------
# Test 3: BSS-CODE-013 (Safe Dynamic Table Columns Synchronization)
# -------------------------------------------------------------
cat("--- Test 3: BSS-CODE-013: Table Column Synchronization ---\n")
df_multi_grp <- data.frame(
  score = c(10, 20, 30, 40, 50, 60),
  factorA = factor(c("A1", "A1", "A2", "A2", "A1", "A2")),
  factorB = factor(c("B1", "B2", "B1", "B2", "B1", "B2"))
)

res_multi <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "cross_sectional",
    deps = "score",
    group = c("factorA", "factorB"),
    showTable = TRUE
  ),
  data = df_multi_grp
)
res_multi$run()
tbl_multi <- res_multi$results$descriptiveTable
col_names <- sapply(tbl_multi$columns, function(c) c$name)
cat("Synchronized columns:", paste(col_names, collapse = ", "), "\n")
stopifnot("factorA" %in% col_names)
stopifnot("factorB" %in% col_names)
cat(">>> PASS: Dynamic grouping columns synchronized safely!\n\n")

# -------------------------------------------------------------
# Test 4: BSS-CODE-014 (Manual Color Palette Fallback)
# -------------------------------------------------------------
cat("--- Test 4: BSS-CODE-014: Color Palette Manual Fallback ---\n")
resolve_col <- DataPresentationClass$private_methods$.resolveColors
empty_manual <- list("none", "none", "", "none")
cols_fallback <- resolve_col("manual", 3, empty_manual)
cat("Fallback colors (manual empty):", paste(cols_fallback, collapse = ", "), "\n")
stopifnot(length(cols_fallback) == 3)
stopifnot(cols_fallback[1] == "#E41A1C") # Fallback to Set1 Red
cat(">>> PASS: Manual empty palette falls back cleanly to set1!\n\n")

# -------------------------------------------------------------
# Test 5: BSS-CODE-015 (Normative Table Formats: ABNT, APA, Vancouver, Chicago)
# -------------------------------------------------------------
cat("--- Test 5: BSS-CODE-015: Normative Table Formatting (ABNT) ---\n")
res_abnt <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "cross_sectional",
    deps = "dv1",
    group = "group",
    showTable = TRUE,
    tableFormat = "abnt"
  ),
  data = df_dv
)
res_abnt$run()
tbl_abnt <- res_abnt$results$descriptiveTable
mean_title <- tbl_abnt$getColumn("mean")$title
sd_title   <- tbl_abnt$getColumn("sd")$title
cat("ABNT mean title:", mean_title, "| ABNT SD title:", sd_title, "\n")
stopifnot(mean_title == "Média")
stopifnot(sd_title == "DP")
cat(">>> PASS: Normative formatting titles (ABNT) applied with precision!\n\n")

cat("=============================================================\n")
cat("ALL VALIDATION TESTS FOR datapresentation PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")
