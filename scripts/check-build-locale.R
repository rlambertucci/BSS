locale <- l10n_info()
cat("Build locale:", Sys.getlocale(), "\n")
if (!isTRUE(locale[["UTF-8"]])) {
    stop("Bundled R must use a valid UTF-8 locale before building BSS.")
}
cat("BSS_R_VERSION=", as.character(getRversion()), "\n", sep = "")
architecture <- if (R.version$arch == "x86_64") "x64" else R.version$arch
cat("BSS_R_ARCH=", architecture, "\n", sep = "")
cat("PASS UTF-8 build locale.\n")
