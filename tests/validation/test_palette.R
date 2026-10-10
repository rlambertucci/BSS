# Teste de palette com mais de 8 grupos
library(ggplot2)

df <- data.frame(
  x = 1:10,
  y = 1:10,
  Group = factor(paste0("G", 1:10))
)

p <- ggplot(df, aes(x = x, y = y, color = Group)) +
  geom_point() +
  scale_color_brewer(palette = "Set2")

tryCatch({
  print(p)
  cat("PLOT_SUCCESS_10_GROUPS\n")
}, error = function(e) {
  cat("PLOT_ERROR_10_GROUPS:", e$message, "\n")
})
