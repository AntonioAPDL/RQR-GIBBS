#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), mustWork = TRUE)
if (!dir.exists(file.path(repo_root, "literature"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

pdf_dir <- file.path(repo_root, "literature", "pdfs")
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
pdfs <- sort(list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE))

sha256_one <- function(path) {
  out <- tryCatch(
    system2("sha256sum", path, stdout = TRUE, stderr = TRUE),
    error = function(e) NA_character_
  )
  if (length(out) && !is.na(out[1])) {
    return(strsplit(out[1], "\\s+")[[1]][1])
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(file = path, algo = "sha256"))
  }
  NA_character_
}

manifest <- data.frame(
  file = basename(pdfs),
  bytes = file.info(pdfs)$size,
  sha256 = vapply(pdfs, sha256_one, character(1)),
  stringsAsFactors = FALSE
)

utils::write.csv(manifest, file.path(repo_root, "literature", "pdf_manifest.csv"), row.names = FALSE, na = "")
writeLines(
  sprintf("%s  %s", manifest$sha256, file.path("pdfs", manifest$file)),
  file.path(repo_root, "literature", "SHA256SUMS")
)

cat("Wrote literature/pdf_manifest.csv and literature/SHA256SUMS for", nrow(manifest), "PDFs.\n")

