# Teste de naOmit no dataset inteiro vs subset
library(jmvcore)

df <- data.frame(
  Species = c("A", "A", "B", "B", "C", "C"),
  X1 = c(1, 2, 3, 4, 5, 6),
  X2 = c(2, 3, 4, 5, 6, 7),
  Unrelated_Var_With_NA = c(NA, 1, NA, 2, NA, 3)
)

cat("Tamanho original:", nrow(df), "\n")
df_buggy <- jmvcore::naOmit(df)
cat("Tamanho apos naOmit no df inteiro (BUGGY):", nrow(df_buggy), "\n")

df_correct <- df[, c("Species", "X1", "X2")]
df_correct <- jmvcore::naOmit(df_correct)
cat("Tamanho apos subset e naOmit (CORRETO):", nrow(df_correct), "\n")
