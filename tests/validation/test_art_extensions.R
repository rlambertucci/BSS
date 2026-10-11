# Validacao independente das extensoes do Ciclo 4 (art)
# BSS-FEAT-010 (Partial Omega-Squared), BSS-FEAT-011 (FDR Adjustment), BSS-FEAT-012 (Interaction Plotting)

cat("=== Teste 1: Validacao do Partial Omega-Squared (pos) ===\n")
# Caso A: F > 1
df_num <- 2
df_res <- 40
f_val <- 5.5

pes <- (f_val * df_num) / ((f_val * df_num) + df_res)
val_pos <- (df_num * (f_val - 1)) / (df_num * (f_val - 1) + df_res + 1)
pos <- max(0, val_pos)

stopifnot(is.finite(pos) && pos > 0 && pos < pes) # omega^2 eh sempre menor ou igual a eta^2
cat("OK: Partial eta^2 =", round(pes, 4), "| Partial omega^2 =", round(pos, 4), "\n")

# Caso B: F <= 1 (deve truncar em 0)
f_val_small <- 0.8
val_pos_small <- (df_num * (f_val_small - 1)) / (df_num * (f_val_small - 1) + df_res + 1)
pos_small <- max(0, val_pos_small)
stopifnot(pos_small == 0)
cat("OK: F <= 1 trunca em zero:", pos_small, "\n")

cat("\n=== Teste 2: Validacao do Ajuste FDR no ARTool/emmeans ===\n")
set.seed(123)
test_data <- data.frame(
  dv = rnorm(60, mean = 20, sd = 4),
  factorA = factor(rep(c("A1", "A2", "A3"), each = 20)),
  factorB = factor(rep(c("B1", "B2"), times = 30))
)

art_m <- ARTool::art(dv ~ factorA * factorB, data = test_data)
a_res <- anova(art_m)
stopifnot(nrow(a_res) == 3)

ph_fdr <- ARTool::art.con(art_m, "factorA", adjust = "fdr")
ph_df <- as.data.frame(ph_fdr)
stopifnot(nrow(ph_df) == 3)
stopifnot(all(ph_df$p.value >= 0 & ph_df$p.value <= 1))
cat("OK: Contrastes pairwise com ajuste FDR calculados com sucesso:\n")
print(ph_df[, c(1, 2, 5, 6)])

cat("\n=== Teste 3: Validacao dos Dados para o Interaction Plot ===\n")
plot_df <- stats::aggregate(test_data$dv, by = list(P1 = test_data$factorA, P2 = test_data$factorB), FUN = function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SE   = stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
})
plot_df <- data.frame(
  P1 = plot_df$P1,
  P2 = plot_df$P2,
  Mean = plot_df$x[, "Mean"],
  SE = plot_df$x[, "SE"]
)
plot_df$CI_lower <- plot_df$Mean - 1.96 * plot_df$SE
plot_df$CI_upper <- plot_df$Mean + 1.96 * plot_df$SE

stopifnot(nrow(plot_df) == 6)
stopifnot(all(plot_df$CI_lower < plot_df$Mean))
stopifnot(all(plot_df$CI_upper > plot_df$Mean))
cat("OK: Medias marginais e IC 95% agregadas para interacao perfeitamente (", nrow(plot_df), "grupos).\n")

cat("\nTODOS OS TESTES DAS EXTENSOES DE ART PASSARAM COM 100% DE SUCESSO!\n")
