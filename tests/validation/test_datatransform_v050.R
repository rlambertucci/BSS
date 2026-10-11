# Test Suite: datatransform v0.5.0 Extensions (Rank-based INT & Transformation Recommender)
pkgload::load_all(".")

cat("=============================================================\n")
cat("TEST SUITE: BSS-FEAT-033..034 (datatransform v0.5.0)\n")
cat("=============================================================\n\n")

set.seed(123)
# Dados assimétricos positivos (exponencial)
df_skew <- data.frame(
  skewed_pos = rexp(150, rate = 0.5),
  skewed_bimodal = c(rnorm(80, 2, 0.5), rnorm(70, 7, 0.8))
)

# Test 1: Rank-Based INT (Blom)
cat("--- Test 1: Rank-Based INT (Blom c = 3/8) ---\n")
res_blom <- dataTransform(
  data = df_skew,
  vars = "skewed_pos",
  method = "int",
  int_formula = "blom",
  show_lambda_table = TRUE,
  show_normality_table = TRUE
)
lt_blom <- res_blom$lambdaTable$asDF
print(lt_blom)
stopifnot(lt_blom$method[1] == "Rank-Based INT")
stopifnot(abs(lt_blom$value[1] - 0.375) < 1e-4)

nt_blom <- res_blom$normalityTable$asDF
print(nt_blom)
# O momento "After" do INT deve ter p-valor alto de normalidade (distribuição normalizada)
p_after <- nt_blom$p[nt_blom$moment == "After"]
cat("Blom after p-value:", p_after, "\n")
stopifnot(!is.na(p_after))
stopifnot(p_after > 0.05)
cat(">>> PASS: Blom INT normalizes skewed data successfully!\n\n")

# Test 2: Rank-Based INT (Van der Waerden, Tukey, Rankit)
cat("--- Test 2: Other INT formulas (VdW, Tukey, Rankit) ---\n")
for (formula in c("vdw", "tukey", "rankit")) {
  res_int <- dataTransform(
    data = df_skew,
    vars = "skewed_pos",
    method = "int",
    int_formula = formula,
    show_lambda_table = TRUE
  )
  lt_curr <- res_int$lambdaTable$asDF
  stopifnot(lt_curr$method[1] == "Rank-Based INT")
  cat("Formula", formula, "parameter value:", lt_curr$value[1], "\n")
}
cat(">>> PASS: All INT formulas execute consistently!\n\n")

# Test 3: Transformation Recommender & Ranking
cat("--- Test 3: Transformation Recommender & Ranking ---\n")
res_rec <- dataTransform(
  data = df_skew,
  vars = c("skewed_pos", "skewed_bimodal"),
  method = "yeojohnson",
  show_recommendation_table = TRUE
)
rt <- res_rec$recommendationTable$asDF
print(head(rt, 12))
stopifnot(nrow(rt) >= 10)
stopifnot("Rank" %in% names(rt) || "rank" %in% names(rt))
# Verificar se rank 1 possui recomendação
var1_rows <- rt[rt$variable == "skewed_pos", ]
stopifnot(nrow(var1_rows) > 3)
stopifnot(var1_rows$rank[1] == 1)
stopifnot(grepl("Recommended", var1_rows$recommended[1]))
# Verificar ordenação monotônica crescente de score
stopifnot(all(diff(var1_rows$score) >= -1e-6))
cat(">>> PASS: Transformation Recommender successfully evaluated and ranked candidate methods!\n\n")

cat("=============================================================\n")
cat("ALL DATATRANSFORM V0.5.0 EXTENSION TESTS PASSED!\n")
cat("=============================================================\n")
