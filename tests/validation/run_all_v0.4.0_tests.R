# Master Test Runner - BSS v0.4.0 Full Verification Suite
cat("======================================================================\n")
cat("          BSS v0.4.0 FULL VERIFICATION & REGRESSION SUITE             \n")
cat("======================================================================\n\n")

test_files <- c(
  # Testes de regressao dos 10 modulos
  "tests/validation/test_networkanalysis.R",
  "tests/validation/test_dataimputation_fixes.R",
  "tests/validation/test_dda_fixes.R",
  "tests/validation/test_art_fixes.R",
  "tests/validation/test_effectsize_fixes.R",
  "tests/validation/test_samplesize_fixes.R",
  "tests/validation/test_outliers_fixes.R",
  "tests/validation/test_correlationcomparison_fixes.R",
  "tests/validation/test_datatransform_fixes.R",
  "tests/validation/test_datapresentation_fixes.R",
  # Testes de extensao
  "tests/validation/test_networkanalysis_extensions.R",
  "tests/validation/test_dataimputation_extensions.R",
  "tests/validation/test_dda_extensions.R",
  "tests/validation/test_art_extensions.R",
  "tests/validation/test_effectsize_extensions.R",
  "tests/validation/test_samplesize_extensions.R",
  "tests/validation/test_outliers_extensions.R",
  "tests/validation/test_correlationcomparison_extensions.R",
  "tests/validation/test_datatransform_extensions.R",
  "tests/validation/test_datapresentation_extensions.R"
)

results <- list()
for (tf in test_files) {
  cat(sprintf("[RUNNING] %s ...\n", basename(tf)))
  res <- tryCatch({
    system2("C:/Program Files/R/R-4.5.2/bin/Rscript.exe", args = tf, stdout = TRUE, stderr = TRUE)
  }, error = function(e) paste("ERROR:", e$message))
  
  status <- attr(res, "status")
  is_pass <- is.null(status) || status == 0
  results[[basename(tf)]] <- list(pass = is_pass, output = res)
  if (is_pass) {
    cat(sprintf("  -> [PASS] %s\n\n", basename(tf)))
  } else {
    cat(sprintf("  -> [FAIL] %s\n", basename(tf)))
    cat("  Output tail:\n")
    cat(tail(res, 10), sep = "\n")
    cat("\n")
  }
}

cat("======================================================================\n")
cat("                       SUMMARY OF TEST RESULTS                        \n")
cat("======================================================================\n")
all_passed <- TRUE
for (name in names(results)) {
  status_str <- if (results[[name]]$pass) "PASS" else "FAIL"
  if (!results[[name]]$pass) all_passed <- FALSE
  cat(sprintf("%-45s : [%s]\n", name, status_str))
}
cat("======================================================================\n")
if (all_passed) {
  cat("SUCCESS: All 20 validation suites passed with 100% compliance!\n")
} else {
  stop("FAILURE: One or more test suites failed.")
}
cat("======================================================================\n")
