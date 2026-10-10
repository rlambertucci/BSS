# Validation test for networkanalysis (GGM and MGM)
# BSS Audit Cycle 1

suppressPackageStartupMessages({
  library(bootnet)
  library(qgraph)
  library(mgm)
})

cat("=== TEST 1: GGM Continuous Network Estimation & Centrality ===\n")
set.seed(42)
n <- 150
# Synthetic Gaussian data with known partial correlation
Sigma <- matrix(c(
  1.0, 0.4, 0.2,
  0.4, 1.0, 0.0,
  0.2, 0.0, 1.0
), 3, 3)
data_cont <- as.data.frame(MASS::mvrnorm(n, mu = c(0,0,0), Sigma = Sigma))
colnames(data_cont) <- c("V1", "V2", "V3")

net_res <- bootnet::estimateNetwork(data_cont, default = "EBICglasso", corMethod = "cor_auto", missing = "pairwise")
g <- net_res$graph
cat("GGM Estimated Graph Matrix:\n")
print(round(g, 3))

cent <- qgraph::centrality(g)
cat("\nqgraph::centrality Output:\n")
cat("OutDegree (Strength):", round(cent$OutDegree, 3), "\n")
cat("Betweenness (raw):", round(cent$Betweenness, 3), "\n")
cat("Closeness (raw):", round(cent$Closeness, 3), "\n")
cat("OutExpectedInfluence:", round(cent$OutExpectedInfluence, 3), "\n")

cat("\n=== TEST 2: MGM Mixed Network with Factors (>2 levels) ===\n")
set.seed(42)
data_mixed <- data.frame(
  num1 = rnorm(n),
  num2 = rnorm(n),
  cat_fac = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE))
)

# Convert factor to 0-based integer as required by mgm for categorical variables
cat_int <- as.integer(data_mixed$cat_fac) - 1L
mat_mixed <- cbind(num1 = data_mixed$num1, num2 = data_mixed$num2, cat_fac = cat_int)

fit_mgm <- mgm::mgm(
  data = mat_mixed,
  type = c("g", "g", "c"),
  level = c(1, 1, 3),
  lambdaSel = "EBIC",
  lambdaGam = 0.5,
  ruleReg = "AND",
  pbar = FALSE
)

cat("MGM wadj matrix:\n")
print(round(fit_mgm$pairwise$wadj, 3))
cat("MGM signs matrix:\n")
print(fit_mgm$pairwise$signs)

# Demonstrating bug in current BSS code:
bad_adj <- fit_mgm$pairwise$wadj * fit_mgm$pairwise$signs
bad_adj[!is.finite(bad_adj)] <- 0
cat("\nCURRENT BSS result (wadj * signs, wiping non-finite to 0):\n")
print(round(bad_adj, 3))

# Correct approach: wadj is the true edge weights matrix; signs are used for edge colors
correct_adj <- fit_mgm$pairwise$wadj
cat("\nCORRECT result (wadj preserves all edges):\n")
print(round(correct_adj, 3))

cat("\nValidation script executed successfully.\n")
