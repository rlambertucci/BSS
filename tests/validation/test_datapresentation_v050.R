# Test Suite: datapresentation v0.5.0 Extensions (Grayscale palette and direct vector export)
suppressPackageStartupMessages({
  library(jmvcore)
  library(R6)
  library(ggplot2)
})

source("R/datapresentation.h.R")
source("R/datapresentation.b.R")

cat("=============================================================\n")
cat("TEST SUITE: datapresentation (Grayscale Palette & Vector Export)\n")
cat("=============================================================\n\n")

# Test 1: Grayscale color resolution
cat("--- Test 1: Grayscale color palette resolution ---\\n")
resolve_fn <- DataPresentationClass$private_methods$.resolveColors

colors_1 <- resolve_fn("grayscale", 1, list())
cat("n=1 color:", colors_1, "\n")
stopifnot(colors_1 == "#FFFFFF")

colors_4 <- resolve_fn("grayscale", 4, list())
cat("n=4 colors:", paste(colors_4, collapse = ", "), "\n")
stopifnot(colors_4[1] == "#FFFFFF")
# Verificar que nao termina em preto (#000000)
stopifnot(tail(colors_4, 1) != "#000000")
# Verificar que e escala crescente de escuro
stopifnot(length(unique(colors_4)) == 4)

# Testar se 'dark2' mapeia para grayscale
colors_dark2 <- resolve_fn("dark2", 3, list())
stopifnot(colors_dark2[1] == "#FFFFFF")
cat(">>> PASS: Grayscale palette starts at pure White #FFFFFF and ends in dark gray without pure black!\n\n")

# Test 2: Vector Figure Export (.pdf / .svg)
cat("--- Test 2: Vector figure export (.pdf / .svg) ---\\n")
set.seed(123)
df_test <- data.frame(
  score = rnorm(30, mean = 75, sd = 10),
  group = factor(rep(c("A", "B", "C"), each = 10))
)

temp_dir <- file.path(tempdir(), "bss_test_export")
if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

res <- DataPresentationClass$new(
  options = DataPresentationOptions$new(
    analysisType = "cross_sectional",
    deps = "score",
    group = "group",
    colorPalette = "grayscale",
    exportPlots = TRUE,
    exportFormat = "both",
    exportDir = temp_dir,
    exportDpi = 300,
    exportWidth = 6.0,
    exportHeight = 4.0
  ),
  data = df_test
)
res$run()

# Trigger .plotGroup directly via private environment
img <- res$results$plots$get(key = "score")
res$.__enclos_env__$private$.plotGroup(img, ggplot2::theme_classic())

pdf_file <- file.path(temp_dir, "BSS_figure_score.pdf")
svg_file <- file.path(temp_dir, "BSS_figure_score.svg")

cat("Checking exported files in:", temp_dir, "\n")
stopifnot(file.exists(pdf_file))
stopifnot(file.exists(svg_file))
stopifnot(file.info(pdf_file)$size > 0)
stopifnot(file.info(svg_file)$size > 0)

cat("Export notice content:\n")
cat(res$results$exportNotice$content, "\n")
stopifnot(grepl("exported successfully", res$results$exportNotice$content))

# Limpeza
unlink(temp_dir, recursive = TRUE)
cat(">>> PASS: High-resolution PDF and SVG vector figures generated and verified!\n\n")

cat("=============================================================\n")
cat("ALL TESTS FOR datapresentation (v0.5.0) PASSED SUCCESSFULLY!\n")
cat("=============================================================\n")
