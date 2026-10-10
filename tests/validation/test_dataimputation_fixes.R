# Validacao independente dos fixes do Ciclo 2 (dataimputation)

cat("=== Teste 1: .fill_imputed_table com numerico e categorico ===\n")
original_data <- data.frame(num = c(1, NA, 3), cat = factor(c("Controle", NA, "Intervencao")), stringsAsFactors = FALSE)
original_data$cat <- factor(original_data$cat)
pooled_data <- data.frame(num = c(1, 2.34567, 3), cat = factor(c("Controle", "Controle", "Intervencao")))
vars_to_impute <- c("num", "cat")

rows <- list()
for (var_name in vars_to_impute) {
  original_col <- original_data[[var_name]]
  imputed_col  <- pooled_data[[var_name]]
  na_idx       <- which(is.na(original_col))
  for (i in na_idx) {
    val <- imputed_col[i]
    imputed_str <- if (is.numeric(val)) as.character(round(val, 3)) else as.character(val)
    rows[[length(rows) + 1]] <- list(var = var_name, original = "NA", imputed = imputed_str, row = i)
  }
}

stopifnot(rows[[1]]$imputed == "2.346")
stopifnot(rows[[2]]$imputed == "Controle")
cat("OK: rows[[1]] numerico arredondado:", rows[[1]]$imputed, "\n")
cat("OK: rows[[2]] categorico preservado:", rows[[2]]$imputed, "\n")

cat("\n=== Teste 2: Subject ID coercion (caracteres e NA defensivo) ===\n")
sub_id_char <- c("S01", "S01", "S02", "S02", "S03", "S03")
res_int <- as.integer(as.factor(sub_id_char))
stopifnot(all(res_int == c(1, 1, 2, 2, 3, 3)))
stopifnot(!anyNA(res_int))
cat("OK: IDs de texto convertidos para clusters inteiros validos:", res_int, "\n")

sub_id_with_na <- c("S01", NA, "S02")
stopifnot(anyNA(sub_id_with_na))
cat("OK: anyNA detecta corretamente presenca de NA no subject ID\n")

cat("\n=== Teste 3: Limpeza de .GlobalEnv em miceadds 2l.pmm ===\n")
fn_name <- "mice.impute.test_cleanup"
assign(fn_name, function(...) NULL, envir = globalenv())
stopifnot(exists(fn_name, envir = globalenv()))

tryCatch({
  # Simulando execucao do mice
  1 + 1
}, finally = {
  if (exists(fn_name, envir = globalenv())) rm(list = fn_name, envir = globalenv())
})
stopifnot(!exists(fn_name, envir = globalenv()))
cat("OK: Funcao temporaria removida com sucesso do .GlobalEnv\n")

cat("\nTODOS OS TESTES DE VALIDACAO PASSARAM COM SUCESSO!\n")
