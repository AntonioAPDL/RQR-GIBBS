#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(repo_root, "main.tex"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

candidate_paths <- unique(c(
  Sys.getenv("EXDQLM_RQR_REPO", unset = NA_character_),
  file.path(dirname(repo_root), "exdqlm__wt__qdesn_0p4p0_integration"),
  file.path(dirname(dirname(repo_root)), "exdqlm__wt__qdesn_0p4p0_integration")
))
candidate_paths <- candidate_paths[!is.na(candidate_paths) & nzchar(candidate_paths)]
is_git_repo <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  out <- tryCatch(
    system2("git", c("-C", path, "rev-parse", "--is-inside-work-tree"), stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  length(out) && identical(trimws(out[1]), "true")
}
exdqlm_repo <- candidate_paths[vapply(candidate_paths, is_git_repo, logical(1))][1]
if (is.na(exdqlm_repo)) {
  stop(
    "Could not find the exdqlm RQR branch. Set EXDQLM_RQR_REPO or clone it beside RQR-GIBBS.",
    call. = FALSE
  )
}
exdqlm_repo <- normalizePath(exdqlm_repo, mustWork = TRUE)

expected_commit <- "dffb71ee70b597d6a716ee74be1cbc99731cd453"
actual_commit <- system2("git", c("-C", exdqlm_repo, "rev-parse", "HEAD"), stdout = TRUE)
branch <- system2("git", c("-C", exdqlm_repo, "rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)
cat("exdqlm_repo:", exdqlm_repo, "\n")
cat("branch:", branch, "\n")
cat("commit:", actual_commit, "\n")
if (!identical(actual_commit, expected_commit)) {
  warning("exdqlm commit differs from expected RQR seed commit: ", expected_commit, call. = FALSE)
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required for this smoke check.", call. = FALSE)
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required for this smoke check.", call. = FALSE)
}

pkgload::load_all(exdqlm_repo, quiet = TRUE)
focused <- c(
  "test-rqr-algebra.R",
  "test-rqr-mcmc-fixed-design.R",
  "test-rqr-learned-scale-mcmc.R",
  "test-rqr-desn-design-parity.R",
  "test-rqr-forecast-contract.R"
)
test_dir <- file.path(exdqlm_repo, "tests", "testthat")
for (ff in focused) {
  path <- file.path(test_dir, ff)
  if (!file.exists(path)) stop("Missing focused test: ", path, call. = FALSE)
  cat("Running", ff, "\n")
  testthat::test_file(path, reporter = "summary")
}
cat("Focused exdqlm RQR smoke tests completed.\n")
