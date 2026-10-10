# Teste de stat_ellipse com grupo pequeno
library(ggplot2)

df <- data.frame(
  x = c(1, 2, 10, 11, 12),
  y = c(1, 2, 10, 11, 12),
  Group = factor(c("G1", "G1", "G2", "G2", "G2"))
)

p <- ggplot(df, aes(x = x, y = y, color = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95)

tryCatch({
  print(p)
  cat("PLOT_SUCCESS\n")
}, error = function(e) {
  cat("PLOT_ERROR:", e$message, "\n")
})
