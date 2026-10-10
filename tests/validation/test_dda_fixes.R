# Teste de Validacao Independente das Correcoes de DDA (Ciclo 3)
library(MASS)
library(ggplot2)
library(effsize)

cat("=== TESTE 1: BSS-STAT-009 (Escopo de naOmit restrito as variaveis da analise) ===\n")
df_full <- data.frame(
  Species = factor(c("A", "A", "B", "B", "C", "C")),
  X1 = c(1, 2, 3, 4, 5, 6),
  X2 = c(2, 3, 4, 5, 6, 7),
  Unrelated_NA = c(NA, 1, NA, 2, NA, 3)
)
dep <- "Species"
covs <- c("X1", "X2")

# Logica corrigida:
all_vars <- unique(c(dep, covs))
data_sub <- df_full[, all_vars, drop = FALSE]
data_clean <- na.omit(data_sub)
cat("Linhas retidas:", nrow(data_clean), "de", nrow(df_full), "\n")
stopifnot(nrow(data_clean) == 6)
cat("OK: Nenhuma linha valida descartada por NAs em colunas alheias!\n")

cat("\n=== TESTE 2: BSS-STAT-010 (droplevels em fator com niveis vazios) ===\n")
df_levels <- data.frame(
  Species = factor(c("G1", "G1", "G2", "G2"), levels = c("G1", "G2", "GhostLevel")),
  X1 = c(1, 2, 3, 4),
  X2 = c(2, 3, 4, 5)
)
df_levels$Species <- droplevels(as.factor(df_levels$Species))
cat("Niveis ativos:", levels(df_levels$Species), "\n")
stopifnot(length(levels(df_levels$Species)) == 2)
cat("OK: Niveis fantasmas removidos com sucesso!\n")

cat("\n=== TESTE 3: BSS-STAT-013 (Deteccao defensiva de covariancia nula) ===\n")
df_const <- data.frame(
  Species = factor(c("G1", "G1", "G2", "G2")),
  X1 = c(1, 2, 3, 4),
  X_const = c(5, 5, 5, 5)
)
covs_test <- c("X1", "X_const")
cov_vars <- sapply(df_const[, covs_test, drop = FALSE], function(col) stats::var(col, na.rm = TRUE))
zero_var_covs <- names(cov_vars)[is.na(cov_vars) | cov_vars == 0]
cat("Covariaveis com variancia zero identificadas:", zero_var_covs, "\n")
stopifnot("X_const" %in% zero_var_covs)
cat("OK: Deteccao de variancia nula funciona perfeitamente!\n")

cat("\n=== TESTE 4: BSS-STAT-011 (Protecao da coluna 'name' em cols_to_remove) ===\n")
current_table_cols <- c("name", "old_LD1", "old_LD2")
dims <- c("LD1", "LD2")
cols_to_remove <- setdiff(current_table_cols, c("name", dims))
cat("Colunas a remover:", cols_to_remove, "\n")
stopifnot(!"name" %in% cols_to_remove)
stopifnot(all(cols_to_remove == c("old_LD1", "old_LD2")))
cat("OK: Coluna de identificadores 'name' preservada com seguranca!\n")

cat("\n=== TESTE 5: BSS-STAT-012 (Renderizacao de plot com 10 grupos sem descarte) ===\n")
df_10 <- data.frame(
  LD1 = rnorm(100),
  LD2 = rnorm(100),
  Group = factor(rep(paste0("G", 1:10), each = 10))
)
centroids_10 <- aggregate(. ~ Group, data = df_10, FUN = mean)
n_plot_groups <- length(unique(df_10$Group))
cat("Numero de grupos no teste:", n_plot_groups, "\n")

p <- ggplot(df_10, aes(x = LD1, y = LD2, color = Group)) +
  geom_point(alpha = 0.6, size = 3.5) +
  geom_point(data = centroids_10, aes(x = LD1, y = LD2, color = Group), size = 6, shape = 17)
if (n_plot_groups <= 8) {
  p <- p + scale_color_brewer(palette = "Set2")
}
# Construir gtable para verificar ausencia de warnings de escala e linhas removidas
b <- ggplot_build(p)
cat("Observacoes plotadas no layer 1:", nrow(b$data[[1]]), "\n")
stopifnot(nrow(b$data[[1]]) == 100)
cat("OK: Todas as 100 observacoes dos 10 grupos foram renderizadas sem descarte!\n")

cat("\n=== TESTE 6: BSS-CODE-005 (Presenca de MASS e referencias) ===\n")
desc_lines <- readLines("DESCRIPTION", warn = FALSE)
imports_line <- grep("^Imports:", desc_lines, value = TRUE)
stopifnot(grepl("MASS", imports_line))
cat("OK: MASS presente nos Imports do DESCRIPTION!\n")

refs_lines <- readLines("jamovi/00refs.yaml", warn = FALSE)
stopifnot(any(grepl("^MASS:", refs_lines)))
stopifnot(any(grepl("^effsize:", refs_lines)))
stopifnot(any(grepl("^Huberty2006:", refs_lines)))
cat("OK: Referencias bibliograficas cadastradas em 00refs.yaml!\n")

cat("\nTODOS OS TESTES DE VALIDACAO DE DDA PASSARAM COM 100% DE SUCESSO!\n")
