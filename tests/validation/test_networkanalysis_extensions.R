# Teste Automatizado de Validacao: BSS-FEAT-001, BSS-FEAT-002, BSS-FEAT-003
library(jmvcore)
library(bootnet)
library(qgraph)
library(igraph)
library(networktools)

source("R/networkanalysis.h.R")
source("R/networkanalysis.b.R")

cat("=== TESTE 1: Validacao do Helper .computeCommunitiesAndBridge ===\n")
adj <- matrix(c(
  0,   0.5, 0.2, 0,
  0.5, 0,   0,   0.3,
  0.2, 0,   0,   0.4,
  0,   0.3, 0.4, 0
), 4, 4)
node_labels <- c("X1", "X2", "X3", "X4")
colnames(adj) <- rownames(adj) <- node_labels

# Test 1.1: Walktrap
res_wt <- networkanalysisClass$private_methods$.computeCommunitiesAndBridge(adj, node_labels, NULL, "walktrap")
stopifnot(!is.null(res_wt$communities))
stopifnot(!is.null(res_wt$bridge))
cat("OK: Deteccao de comunidades Walktrap e Bridge Centrality calculados com sucesso!\n")
print(res_wt$communities)
print(res_wt$bridge)

# Test 1.2: Louvain
res_lv <- networkanalysisClass$private_methods$.computeCommunitiesAndBridge(adj, node_labels, NULL, "louvain")
stopifnot(!is.null(res_lv$communities))
cat("OK: Deteccao de comunidades Louvain calculada com sucesso!\n")

# Test 1.3: Grupos definidos externamente
ext_groups <- list(ClusterA = c(1, 2), ClusterB = c(3, 4))
res_ext <- networkanalysisClass$private_methods$.computeCommunitiesAndBridge(adj, node_labels, ext_groups, "none")
stopifnot(!is.null(res_ext$bridge))
stopifnot(all(res_ext$bridge$community == c("ClusterA", "ClusterA", "ClusterB", "ClusterB")))
cat("OK: Bridge Centrality com grupos de nos pre-definidos validado com sucesso!\n")

cat("\n=== TESTE 2: Execucao Completa no Dataset Big5 (Continuous GGM) ===\n")
data(bfi, package = "psych")
df_test <- na.omit(bfi[1:100, c("A1", "A2", "A3", "C1", "C2", "C3")])

analysis <- networkanalysisClass$new(
  options = networkanalysisOptions$new(
    vars_net = c("A1", "A2", "A3", "C1", "C2", "C3"),
    estimator_net = "correlation",
    tableCentrality_net = TRUE,
    tableBridge_net = TRUE,
    tableCommunities_net = TRUE,
    communityAlgorithm_net = "walktrap",
    layoutAlgorithm_net = "spring",
    layoutSeed_net = 123
  ),
  data = df_test
)

analysis$run()

# Checar tabelas de saida
bridge_rows <- analysis$results$bridge_net$asDF
comm_rows <- analysis$results$communities_net$asDF
cent_rows <- analysis$results$centrality_net$asDF

cat("Linhas na tabela de Bridge Centrality:", nrow(bridge_rows), "\n")
print(head(bridge_rows))
stopifnot(nrow(bridge_rows) == 6)

cat("Linhas na tabela de Comunidades:", nrow(comm_rows), "\n")
print(comm_rows)
stopifnot(nrow(comm_rows) == 6)

stopifnot(nrow(cent_rows) == 6)

cat("\nTODOS OS TESTES DE EXTENSAO DO MODULO 1 PASSARAM COM 100% DE SUCESSO!\n")
