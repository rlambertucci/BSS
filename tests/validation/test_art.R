# Script de Teste Diagnostico para Ciclo 4 (art)
library(ARTool)
library(emmeans)

cat("=== TESTE 1: Modelo Cross-Sectional e Longitudinal Básico ===\n")
data(Higgins1990Table5, package = "ARTool")
m_long <- art(DryMatter ~ Moisture * Fertilizer + (1|Tray), data = Higgins1990Table5)
a_long <- anova(m_long)
print(a_long)

cat("\n=== TESTE 2: BSS-STAT-014 (make.names para variaveis com espacos) ===\n")
df_spaces <- Higgins1990Table5
names(df_spaces)[names(df_spaces) == "DryMatter"]  <- "Dry Matter (g)"
names(df_spaces)[names(df_spaces) == "Moisture"]   <- "Moisture Level"
names(df_spaces)[names(df_spaces) == "Fertilizer"] <- "Fertilizer Type"
names(df_spaces)[names(df_spaces) == "Tray"]       <- "Tray ID"

# Higienizacao de nomes
name_map <- setNames(make.names(names(df_spaces)), names(df_spaces))
names(df_spaces) <- name_map

dep_clean <- name_map[["Dry Matter (g)"]]
preds_clean <- unname(name_map[c("Moisture Level", "Fertilizer Type")])
id_clean <- name_map[["Tray ID"]]

f_clean <- paste0(
  dep_clean, " ~ ",
  paste(preds_clean, collapse = " * "),
  " + (1|", id_clean, ")"
)
m_spaces_ok <- art(as.formula(f_clean), data = df_spaces)
stopifnot(!is.null(m_spaces_ok))
cat("OK: make.names permitiu ajuste perfeito com nomes complexos!\n")

cat("\n=== TESTE 3: BSS-STAT-015 (Coercao numerica de DV categórica/Likert) ===\n")
df_likert <- Higgins1990Table5
df_likert$DryMatter <- factor(round(df_likert$DryMatter))
df_likert$DryMatter <- as.numeric(as.character(df_likert$DryMatter))
m_likert <- art(DryMatter ~ Moisture * Fertilizer + (1|Tray), data = df_likert)
stopifnot(!is.null(m_likert))
cat("OK: Coercao de DV categórica permitiu ajuste sem erro no ARTool!\n")

cat("\n=== TESTE 4: BSS-STAT-017 (Rotulo e correcao de Hedges' g) ===\n")
t_ratio <- 3.5
df_val <- 10
d_val <- (2 * t_ratio) / sqrt(df_val)
j_corr <- if (df_val > 1) (1 - 3 / (4 * df_val - 1)) else 1
g_val <- d_val * j_corr
cat(sprintf("Cohen's d: %.4f | Hedges' g: %.4f\n", d_val, g_val))
stopifnot(g_val < d_val)
cat("OK: Hedges' g calculado e ajustado com precisao!\n")

cat("\nTODOS OS TESTES DIAGNOSTICOS DE ART CONCLUIDOS COM SUCESSO!\n")
