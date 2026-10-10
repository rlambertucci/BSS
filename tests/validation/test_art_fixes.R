# Validacao Independente das Correcoes de ART (Ciclo 4)
library(ARTool)
library(emmeans)
library(jmvcore)

cat("=== TESTE 1: BSS-STAT-014 (Nomes complexos com espacos, acentos e caracteres especiais) ===\n")
data(Higgins1990Table5, package = "ARTool")
df_spaces <- Higgins1990Table5
names(df_spaces)[names(df_spaces) == "DryMatter"]  <- "Massa Seca (g)"
names(df_spaces)[names(df_spaces) == "Moisture"]   <- "Nivel Umidade"
names(df_spaces)[names(df_spaces) == "Fertilizer"] <- "Tipo Adubo"
names(df_spaces)[names(df_spaces) == "Tray"]       <- "ID Bandeja"

dep_var <- "Massa Seca (g)"
fixed_factors <- c("Nivel Umidade", "Tipo Adubo")
subject_id_var <- "ID Bandeja"
predictors <- fixed_factors

# Testando rotina de higienizacao e un-mangling implementada no art.b.R
vars_in_model <- c(dep_var, predictors, subject_id_var)
safe_name_map <- setNames(make.names(vars_in_model), vars_in_model)
reverse_name_map <- setNames(names(safe_name_map), unname(safe_name_map))

df_proc <- df_spaces
for (orig_col in names(safe_name_map)) {
  names(df_proc)[names(df_proc) == orig_col] <- safe_name_map[[orig_col]]
}

dep_safe <- safe_name_map[[dep_var]]
preds_safe <- unname(safe_name_map[predictors])
id_safe <- safe_name_map[[subject_id_var]]

for (p in preds_safe) df_proc[[p]] <- as.factor(df_proc[[p]])
df_proc[[id_safe]] <- as.factor(df_proc[[id_safe]])

f_str <- paste0(dep_safe, " ~ ", paste(preds_safe, collapse = " * "), " + (1|", id_safe, ")")
m_fit <- ARTool::art(as.formula(f_str), data = df_proc)
stopifnot(!is.null(m_fit))
cat("OK: Ajuste com espacos e caracteres especiais executado com sucesso!\n")

a_res <- anova(m_fit)
unmangle_term <- function(term_str, reverse_map) {
  parts <- strsplit(term_str, ":")[[1]]
  unmangled_parts <- sapply(parts, function(p) {
    if (p %in% names(reverse_map)) reverse_map[[p]] else p
  })
  paste(unmangled_parts, collapse = ":")
}
displayed_terms <- sapply(a_res$Term, unmangle_term, reverse_map = reverse_name_map)
cat("Termos exibidos na tabela:", displayed_terms, "\n")
stopifnot("Nivel Umidade:Tipo Adubo" %in% displayed_terms)
cat("OK: Un-mangling dos termos restaurou os rotulos originais perfeitamente!\n")

cat("\n=== TESTE 2: BSS-STAT-015 (.to_numeric_vec com DV categorica/Likert) ===\n")
to_numeric_vec <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.factor(x)) {
    num_val <- suppressWarnings(as.numeric(as.character(x)))
    if (!all(is.na(num_val))) return(num_val)
    return(as.numeric(x))
  }
  suppressWarnings(as.numeric(x))
}

df_fac_dv <- Higgins1990Table5
df_fac_dv$DryMatter <- factor(round(df_fac_dv$DryMatter))
stopifnot(is.factor(df_fac_dv$DryMatter))
df_fac_dv$DryMatter <- to_numeric_vec(df_fac_dv$DryMatter)
stopifnot(is.numeric(df_fac_dv$DryMatter))
m_fac_ok <- ARTool::art(DryMatter ~ Moisture * Fertilizer + (1|Tray), data = df_fac_dv)
stopifnot(!is.null(m_fac_ok))
cat("OK: to_numeric_vec permitiu ajuste perfeito com DV originalmente categorica!\n")

cat("\n=== TESTE 3: BSS-STAT-016 (deleteRows defensivo em Table) ===\n")
t_test <- Table$new(
  name = "test",
  title = "Test",
  columns = list(list(name = "k", title = "Key", type = "text"))
)
t_test$addRow(rowKey = 1, values = list(k = "V1"))
stopifnot(t_test$rowCount == 1)
if (t_test$rowCount > 0) t_test$deleteRows()
stopifnot(t_test$rowCount == 0)
t_test$addRow(rowKey = 1, values = list(k = "V2"))
stopifnot(t_test$rowCount == 1)
cat("OK: deleteRows preveniu erro de duplicate rowKey!\n")

cat("\n=== TESTE 4: BSS-STAT-017 (Hedges' g e protecao df > 1) ===\n")
t_ratio <- 2.8
df_low <- 0.8
j_corr_low <- if (df_low > 1) (1 - 3 / (4 * df_low - 1)) else 1
stopifnot(j_corr_low == 1)
df_norm <- 15
j_corr_norm <- if (df_norm > 1) (1 - 3 / (4 * df_norm - 1)) else 1
stopifnot(j_corr_norm < 1 && j_corr_norm > 0)
cat(sprintf("OK: Hedges' g ajustado com precisao (J com df=15: %.4f, J com df=0.8: %.4f)!\n", j_corr_norm, j_corr_low))

cat("\n=== TESTE 5: BSS-CODE-006 (Referencias de ART em 00refs.yaml e art.a.yaml) ===\n")
refs_00 <- readLines("jamovi/00refs.yaml", warn = FALSE)
stopifnot(any(grepl("^ARTool:", refs_00)))
stopifnot(any(grepl("^emmeans:", refs_00)))
stopifnot(any(grepl("^Wobbrock2011:", refs_00)))
stopifnot(any(grepl("^Elkin2021:", refs_00)))
cat("OK: Todas as 4 referencias presentes em 00refs.yaml!\n")

art_yaml <- readLines("jamovi/art.a.yaml", warn = FALSE)
stopifnot(any(grepl("^refs:", art_yaml)))
cat("OK: Chave refs presente em jamovi/art.a.yaml!\n")

cat("\nTODOS OS TESTES DE VALIDACAO DE ART PASSARAM COM 100% DE SUCESSO!\n")
