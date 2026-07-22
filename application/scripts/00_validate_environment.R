#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(repo_root, "main.tex"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

cat("RQR-GIBBS environment preflight\n")
cat("repo_root:", repo_root, "\n")
cat("R:", R.version.string, "\n")
cat("platform:", R.version$platform, "\n")

required_dirs <- c(
  "application/R",
  "application/scripts",
  "application/tests",
  "application/config",
  "application/manifests",
  "docs",
  "literature",
  "tables"
)
missing_dirs <- required_dirs[!dir.exists(file.path(repo_root, required_dirs))]
if (length(missing_dirs)) {
  stop("Missing required directories: ", paste(missing_dirs, collapse = ", "), call. = FALSE)
}

tools <- c("git", "pdflatex", "bibtex", "latexmk")
tool_paths <- vapply(tools, Sys.which, character(1))
print(data.frame(tool = tools, path = unname(tool_paths), row.names = NULL))

packages <- c("testthat", "pkgload", "jsonlite", "yaml", "digest")
pkg_status <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
print(data.frame(package = packages, installed = unname(pkg_status), row.names = NULL))

if (!nzchar(tool_paths[["pdflatex"]])) {
  stop("pdflatex is required to compile the manuscript scaffold.", call. = FALSE)
}

cat("Preflight completed. Optional package gaps are acceptable until the relevant target is used.\n")

