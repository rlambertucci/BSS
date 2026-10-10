# Validation test for dataimputation (MICE, multilevel MICE, missRanger)
# BSS Audit Cycle 2

suppressPackageStartupMessages({
  library(mice)
  library(missRanger)
})

cat("=== TEST 1: Cross-Sectional MICE with PMM & Categorical ===\n")
set.seed(42)
n <- 60
df_cs <- data.frame(
  num1 = c(rnorm(n - 10), rep(NA, 10)),
  num2 = rnorm(n),
  cat1 = factor(c(sample(c("A", "B"), n - 5, replace = TRUE), rep(NA, 5)))
)

imp_cs <- mice::mice(df_cs, m = 3, maxit = 5, printFlag = FALSE, seed = 42)
comp1 <- mice::complete(imp_cs, 1)
cat("Number of missing in comp1 num1:", sum(is.na(comp1$num1)), "\n")
cat("Number of missing in comp1 cat1:", sum(is.na(comp1$cat1)), "\n")

cat("\n=== TEST 2: Bug in .fill_imputed_table with Categorical Variables (%||%) ===\n")
# Simulating the exact line from dataimputation.b.R:566
imputed_col <- comp1$cat1
na_idx <- which(is.na(df_cs$cat1))
cat("True imputed categories for missing indices:\n")
print(imputed_col[na_idx])

# The flawed logic currently in BSS:
current_bss_display <- sapply(na_idx, function(i) {
  num_attempt <- suppressWarnings(as.numeric(imputed_col[i]))
  # In R, if num_attempt is NA (because cat1 is 'A'/'B'), round(NA, 3) is NA.
  # If %||% is defined as function(x, y) if(is.null(x)) y else x:
  coalesce_val <- if (is.null(round(num_attempt, 3))) as.character(imputed_col[i]) else round(num_attempt, 3)
  as.character(coalesce_val)
})
cat("Current BSS displayed value in table (BUGGY):\n")
print(current_bss_display)

# The correct logic:
correct_bss_display <- sapply(na_idx, function(i) {
  val <- imputed_col[i]
  if (is.numeric(val)) as.character(round(val, 3)) else as.character(val)
})
cat("Correct display value:\n")
print(correct_bss_display)

cat("\n=== TEST 3: Bug in Subject ID Coercion in Multilevel MICE ===\n")
# If subject IDs are alphanumeric characters (e.g. 'P01', 'P02')
subj_char <- c("P01", "P01", "P02", "P02", "P03", "P03")
cat("Direct as.integer(character IDs) (CURRENT BSS):\n")
print(suppressWarnings(as.integer(subj_char)))

cat("Safe as.integer(as.factor(character IDs)) (CORRECT):\n")
print(as.integer(as.factor(subj_char)))

cat("\n=== TEST 4: missRanger Random Forest Imputation ===\n")
df_mr <- df_cs
res_mr <- missRanger::missRanger(df_mr, pmm.k = 3, maxiter = 5, verbose = 0, seed = 42)
cat("Missing count after missRanger:\n")
print(colSums(is.na(res_mr)))

cat("\nValidation test completed successfully.\n")
