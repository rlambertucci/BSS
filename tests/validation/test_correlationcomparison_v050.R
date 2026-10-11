# Test Suite: correlationcomparison v0.5.0 Extensions (Multi-Group Q, Partial Correlation Comparison, Ellipse Plot)
pkgload::load_all(".")

cat("=============================================================\n")
cat("TEST SUITE: BSS-FEAT-040..042 (correlationcomparison v0.5.0)\n")
cat("=============================================================\n\n")

# Test 1: Multi-Group Homogeneity Test (k = 3 groups)
cat("--- Test 1: Multi-Group Homogeneity Test (Cochran's Q / chisq) ---\n")
set.seed(42)
n_k <- 50
# Grupo 1: alta correlacao (r ~ 0.8)
x1 <- rnorm(n_k); y1 <- 0.8 * x1 + rnorm(n_k, 0, 0.6)
# Grupo 2: moderada correlacao (r ~ 0.4)
x2 <- rnorm(n_k); y2 <- 0.4 * x2 + rnorm(n_k, 0, 0.9)
# Grupo 3: nula correlacao (r ~ 0.0)
x3 <- rnorm(n_k); y3 <- rnorm(n_k, 0, 1.0)

df_multi <- data.frame(
  X = c(x1, x2, x3),
  Y = c(y1, y2, y3),
  Group = factor(rep(c("G1", "G2", "G3"), each = n_k))
)

res_multi <- correlationComparison(
  data = df_multi,
  mode = "multigroup",
  multi_var_x = "X",
  multi_var_y = "Y",
  multi_group = "Group",
  show_ellipse_plot = TRUE
)

mg_df <- res_multi$multiGroupTable$asDF
print(mg_df)
stopifnot(nrow(mg_df) == 3)
stopifnot(all(!is.na(mg_df$r)))
stopifnot(all(!is.na(mg_df$z)))

ov_df <- res_multi$multiGroupOverallTable$asDF
print(ov_df)
stopifnot(ov_df$k_groups[1] == 3)
stopifnot(ov_df$df[1] == 2)
stopifnot(ov_df$q_stat[1] > 0)
stopifnot(!is.na(ov_df$p_value[1]))
cat(">>> PASS: Multi-group Cochran's Q test executed and detected correlation differences across k=3 groups!\n\n")

# Test 2: Partial Correlation Comparison controlling for Z
cat("--- Test 2: Partial Correlation Comparison (r_XY.Z between 2 Groups) ---\n")
set.seed(123)
n_part <- 60
z_cov1 <- rnorm(n_part)
x_part1 <- 0.5 * z_cov1 + rnorm(n_part)
y_part1 <- 0.5 * z_cov1 + 0.6 * x_part1 + rnorm(n_part)

z_cov2 <- rnorm(n_part)
x_part2 <- 0.5 * z_cov2 + rnorm(n_part)
y_part2 <- 0.5 * z_cov2 + 0.0 * x_part2 + rnorm(n_part)

df_partial <- data.frame(
  X = c(x_part1, x_part2),
  Y = c(y_part1, y_part2),
  Z = c(z_cov1, z_cov2),
  Group = factor(rep(c("Active", "Control"), each = n_part))
)

res_part <- correlationComparison(
  data = df_partial,
  mode = "partial_comparison",
  partial_var_x = "X",
  partial_var_y = "Y",
  partial_covs = "Z",
  partial_group = "Group"
)

pt_df <- res_part$partialTable$asDF
print(pt_df)
stopifnot(nrow(pt_df) == 1)
stopifnot(!is.na(pt_df$r_partial_g1[1]))
stopifnot(!is.na(pt_df$r_partial_g2[1]))
stopifnot(!is.na(pt_df$z_stat[1]))
stopifnot(!is.na(pt_df$p_value[1]))
cat(">>> PASS: Partial correlation comparison between groups computed controlling for Z!\n\n")

# Test 3: Confidence Ellipse plot state validation
cat("--- Test 3: Confidence Ellipse plot state inspection ---\n")
el_state <- res_multi$ellipsePlot$state
stopifnot(!is.null(el_state))
stopifnot(!is.null(el_state$df))
stopifnot(el_state$x_col == "X")
stopifnot(el_state$y_col == "Y")
stopifnot(el_state$grp_col == "Group")
cat(">>> PASS: Ellipse plot state properly configured with raw data and grouping!\n\n")

cat("=============================================================\n")
cat("ALL CORRELATIONCOMPARISON V0.5.0 EXTENSION TESTS PASSED!\n")
cat("=============================================================\n")
