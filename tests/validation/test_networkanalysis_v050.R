# Test validation for networkanalysis v0.5.0 features (BSS-FEAT-046..048)
# 1. Network stability via bootstrapping (Edge CIs and CS-coefficient)
# 2. Network Comparison Test (NCT: structure M, global strength S, edge invariance)
# 3. Directed Acyclic Graph (DAG: causal discovery, paths and plotting)

library(jmvcore)
source("R/networkanalysis.h.R")
source("R/networkanalysis.b.R")

cat("=============================================================\n")
cat("TEST SUITE: BSS-FEAT-046..048 (networkanalysis v0.5.0)\n")
cat("=============================================================\n\n")

# --- Setup Data ---
set.seed(123)
n_cases <- 120
x1 <- rnorm(n_cases)
x2 <- 0.6 * x1 + rnorm(n_cases, sd = 0.8)
x3 <- 0.5 * x2 + rnorm(n_cases, sd = 0.8)
x4 <- -0.4 * x1 + 0.5 * x3 + rnorm(n_cases, sd = 0.8)

grp <- factor(rep(c("Control", "Clinical"), each = n_cases / 2))
x2[grp == "Clinical"] <- x2[grp == "Clinical"] + 0.8 * x1[grp == "Clinical"]

df_net <- data.frame(X1 = x1, X2 = x2, X3 = x3, X4 = x4, Group = grp)

# --- Test 1: Network Stability via Bootstrapping (Edge CIs & CS-coefficient) ---
cat("--- Test 1: Bootnet Network Stability (Edge CIs & CS-coefficient) ---\n")
res_boot <- networkanalysisClass$new(
  data = df_net,
  options = networkanalysisOptions$new(
    mode = "continuous",
    vars_net = c("X1", "X2", "X3", "X4"),
    estimator_net = "correlation",
    bootStability_net = TRUE,
    bootN_net = 40,
    bootEdgeCI_net = TRUE,
    bootCS_net = TRUE
  )
)
res_boot$run()

# 1. Edge stability table
edge_tab <- res_boot$results$edgeStability_net$asDF
print(edge_tab)
stopifnot(nrow(edge_tab) == 6)
stopifnot(!is.na(edge_tab$weight[1]))
stopifnot(!is.na(edge_tab$boot_mean[1]))
stopifnot(edge_tab$ci_lower[1] <= edge_tab$ci_upper[1])

edge_note <- res_boot$results$edgeStability_net$notes$citation$note
cat("Edge CI note:", edge_note, "\n")
stopifnot(grepl("Epskamp", edge_note, fixed = TRUE))

# 2. CS-coefficient table
cs_tab <- res_boot$results$csStability_net$asDF
print(cs_tab)
stopifnot(nrow(cs_tab) == 4)
stopifnot(all(cs_tab$cs_coefficient >= 0 & cs_tab$cs_coefficient <= 0.75))
stopifnot(all(nchar(cs_tab$interpretation) > 0))

cs_note <- res_boot$results$csStability_net$notes$citation$note
cat("CS note:", cs_note, "\n")
stopifnot(grepl("Epskamp", cs_note, fixed = TRUE))
cat(">>> PASS: Bootnet Edge CIs and Centrality Stability (CS-coefficient) successfully verified!\n\n")

# --- Test 2: Network Comparison Test (NCT) ---
cat("--- Test 2: Network Comparison Test (NCT: Structure M & Global Strength S) ---\n")
res_nct <- networkanalysisClass$new(
  data = df_net,
  options = networkanalysisOptions$new(
    mode = "continuous",
    vars_net = c("X1", "X2", "X3", "X4"),
    groupVar_net = "Group",
    estimator_net = "correlation",
    nctTest_net = TRUE,
    nctPermutations_net = 35
  )
)
res_nct$run()

# Global invariance
nct_glob <- res_nct$results$nctGlobal_net$asDF
print(nct_glob)
stopifnot(nrow(nct_glob) == 2)
stopifnot(!is.na(nct_glob$statistic[1]))
stopifnot(nct_glob$p_value[1] >= 0 && nct_glob$p_value[1] <= 1)
stopifnot(!is.na(nct_glob$statistic[2]))
stopifnot(nct_glob$p_value[2] >= 0 && nct_glob$p_value[2] <= 1)

nct_note <- res_nct$results$nctGlobal_net$notes$citation$note
cat("NCT note:", nct_note, "\n")
stopifnot(grepl("van Borkulo", nct_note, fixed = TRUE))

# Edge invariance
nct_edges <- res_nct$results$nctEdges_net$asDF
print(nct_edges)
stopifnot(nrow(nct_edges) == 6)
stopifnot(all(nct_edges$diff >= 0))
stopifnot(all(nct_edges$p_value >= 0 & nct_edges$p_value <= 1))
cat(">>> PASS: Network Comparison Test (NCT) permutation testing validated successfully!\n\n")

# --- Test 3: Directed Acyclic Graph (DAG) Estimation ---
cat("--- Test 3: Directed Acyclic Graph (DAG) Estimation & Causal Paths ---\n")
res_dag <- networkanalysisClass$new(
  data = df_net,
  options = networkanalysisOptions$new(
    mode = "continuous",
    vars_net = c("X1", "X2", "X3", "X4"),
    dagAnalysis_net = TRUE,
    dagAlgorithm_net = "pc",
    plotDAG_net = TRUE
  )
)
res_dag$run()

dag_tab <- res_dag$results$dagTable_net$asDF
print(dag_tab)
stopifnot(nrow(dag_tab) >= 1)
stopifnot(all(dag_tab$direction == "-->"))
stopifnot(!is.na(dag_tab$estimate[1]))
stopifnot(!is.na(dag_tab$p_value[1]))

dag_state <- res_dag$results$dagPlot_net$state
stopifnot(!is.null(dag_state))
stopifnot(!is.null(dag_state$adj))
g_dag <- igraph::graph_from_adjacency_matrix(dag_state$adj, mode = "directed")
stopifnot(igraph::is_dag(g_dag))
cat("Acyclicity check: is_dag =", igraph::is_dag(g_dag), "\n")

dag_note <- res_dag$results$dagTable_net$notes$citation$note
cat("DAG note:", dag_note, "\n")
stopifnot(grepl("Spirtes", dag_note, fixed = TRUE))
cat(">>> PASS: Directed Acyclic Graph (DAG) causal structure estimated and confirmed strictly acyclic!\n\n")

cat("=============================================================\n")
cat("ALL NETWORKANALYSIS V0.5.0 EXTENSION TESTS PASSED!\n")
cat("=============================================================\n")
