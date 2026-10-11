# Test Suite: datatransform Extensions (Arcsine, Logit, Anderson-Darling, Shapiro-Francia, Density Plot)
pkgload::load_all(".")

cat("=============================================================\n")
cat("TEST SUITE: BSS-FEAT-025..027 (datatransform extensions)\n")
cat("=============================================================\n\n")

# Test 1: Arcsine transformation (proportions & percentages)
cat("--- Test 1: Arcsine square root transformation ---\n")
set.seed(42)
df_prop <- data.frame(rate = rbeta(100, shape1 = 2, shape2 = 5))
dt_arcsine <- dataTransform(data = df_prop, vars = "rate", method = "arcsine")
l_tbl <- dt_arcsine$lambdaTable$asDF
print(l_tbl)
stopifnot(l_tbl$method[1] == "Arcsine")
stopifnot(!is.na(l_tbl$value[1]))
cat(">>> PASS: Arcsine transformation executed and tabulated successfully!\n\n")

# Test 2: Empirical Logit transformation
cat("--- Test 2: Empirical Logit transformation ---\n")
dt_logit <- dataTransform(data = df_prop, vars = "rate", method = "logit")
l_tbl_log <- dt_logit$lambdaTable$asDF
print(l_tbl_log)
stopifnot(l_tbl_log$method[1] == "Empirical Logit")
cat(">>> PASS: Empirical Logit transformation executed successfully!\n\n")

# Test 3: Anderson-Darling normality test
cat("--- Test 3: Anderson-Darling Normality Test ---\n")
dt_ad <- dataTransform(data = df_prop, vars = "rate", method = "yeojohnson",
                       show_normality_table = TRUE, normality_test = "anderson_darling")
n_tbl_ad <- dt_ad$normalityTable$asDF
print(n_tbl_ad)
stopifnot(!is.na(n_tbl_ad$W[1]))
stopifnot(!is.na(n_tbl_ad$p[1]))
cat(">>> PASS: Anderson-Darling test computed correctly!\n\n")

# Test 4: Shapiro-Francia normality test
cat("--- Test 4: Shapiro-Francia Normality Test ---\n")
dt_sf <- dataTransform(data = df_prop, vars = "rate", method = "yeojohnson",
                       show_normality_table = TRUE, normality_test = "shapiro_francia")
n_tbl_sf <- dt_sf$normalityTable$asDF
print(n_tbl_sf)
stopifnot(!is.na(n_tbl_sf$W[1]))
stopifnot(!is.na(n_tbl_sf$p[1]))
cat(">>> PASS: Shapiro-Francia test computed correctly!\n\n")

# Test 5: Density Plot state inspection
cat("--- Test 5: Density Plot state ---\n")
dt_plot <- dataTransform(data = df_prop, vars = "rate", method = "yeojohnson",
                         show_density_plot = TRUE)
d_state <- dt_plot$densityPlots$get(key = "rate")$state
stopifnot(!is.null(d_state) && nrow(d_state) == 100)
cat("Density state preview (first 3 rows):\n")
print(head(d_state, 3))
cat(">>> PASS: Density plot state properly populated!\n\n")

cat("=============================================================\n")
cat("ALL EXTENSION TESTS FOR datatransform PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")
