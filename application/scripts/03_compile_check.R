#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(repo_root, "main.tex"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

pdflatex <- Sys.which("pdflatex")
bibtex <- Sys.which("bibtex")
if (!nzchar(pdflatex)) stop("pdflatex is not available.", call. = FALSE)

compile_one <- function(stem) {
  tex <- paste0(stem, ".tex")
  cat("Compiling", tex, "\n")
  system2(pdflatex, c("-interaction=nonstopmode", tex), stdout = TRUE, stderr = TRUE)
  if (nzchar(bibtex)) {
    system2(bibtex, stem, stdout = TRUE, stderr = TRUE)
  }
  system2(pdflatex, c("-interaction=nonstopmode", tex), stdout = TRUE, stderr = TRUE)
  system2(pdflatex, c("-interaction=nonstopmode", tex), stdout = TRUE, stderr = TRUE)
  pdf <- paste0(stem, ".pdf")
  if (!file.exists(pdf)) stop("Expected PDF not created: ", pdf, call. = FALSE)
}

compile_one("main")
compile_one("rqr-gibbs-supplement")
cat("Compile check completed.\n")

