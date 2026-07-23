#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "main.tex"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "pinned_exdqlm_runtime.R"
  ),
  envir = environment()
)

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
exdqlm_repo <- normalizePath(
  exdqlm_repo, winslash = "/", mustWork = TRUE
)

expected_commit <- "dffb71ee70b597d6a716ee74be1cbc99731cd453"
expected_branch <- "feature/rqr-desn-readout-20260716"
layout <- rqr_exdqlm_runtime_layout(
  repo_root = repo_root,
  exdqlm_repo = exdqlm_repo,
  pinned_commit = expected_commit
)
source_before <- rqr_capture_external_checkout(exdqlm_repo)
guard_pending <- TRUE
on.exit({
  if (isTRUE(guard_pending)) {
    rqr_assert_external_checkout_unchanged(source_before)
  }
}, add = TRUE)

cat("exdqlm_repo:", exdqlm_repo, "\n")
cat("branch:", source_before$branch, "\n")
cat("commit:", source_before$commit, "\n")
if (!identical(source_before$commit, expected_commit)) {
  stop("exdqlm commit differs from pinned RQR commit: ", expected_commit, call. = FALSE)
}
if (!identical(source_before$branch, expected_branch)) {
  stop("exdqlm branch differs from pinned RQR branch: ", expected_branch, call. = FALSE)
}
if (nzchar(source_before$status)) {
  stop(
    "The pinned exdqlm worktree must be clean. Dirty entries: ",
    source_before$status,
    call. = FALSE
  )
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required for this smoke check.", call. = FALSE)
}
if (!dir.exists(layout$library_root) || !file.exists(layout$attestation_path)) {
  stop(
    "The isolated exdqlm runtime is missing. Run ",
    "'make prepare-exdqlm-runtime' first.",
    call. = FALSE
  )
}
if ("exdqlm" %in% loadedNamespaces()) {
  stop("exdqlm was loaded before the isolated library was selected.", call. = FALSE)
}
.libPaths(c(
  normalizePath(layout$library_root, winslash = "/", mustWork = TRUE),
  .libPaths()
))
if (!requireNamespace("rqrgibbs", quietly = TRUE)) {
  stop("Install rqrgibbs before running the reference smoke tests.", call. = FALSE)
}
runtime_state <- rqrgibbs:::.rqr_repository_provenance(list(
  repo_root = exdqlm_repo,
  expected_git_commit = expected_commit,
  runtime_package = "exdqlm",
  runtime_attestation = layout$attestation_path,
  require_isolated_runtime = TRUE
))
if (isTRUE(runtime_state$runtime_direct_source_path_match) ||
    !isTRUE(runtime_state$runtime_isolated_from_source) ||
    !isTRUE(runtime_state$source_archive_verified) ||
    !isTRUE(runtime_state$source_archive_isolated_from_source) ||
    !isTRUE(runtime_state$source_package_archive_match) ||
    !isTRUE(runtime_state$build_evidence_verified) ||
    !isTRUE(runtime_state$install_evidence_verified) ||
    !isTRUE(runtime_state$runtime_lineage_marker_match) ||
    !isTRUE(runtime_state$runtime_attestation_match) ||
    !isTRUE(runtime_state$runtime_source_match) ||
    !isTRUE(runtime_state$reproducibility_eligible)) {
  stop(
    paste(
      "The executing exdqlm namespace is not an isolated, archive-attested",
      "runtime of the clean pinned checkout."
    ),
    call. = FALSE
  )
}
cat("runtime package path:", runtime_state$runtime_package_path, "\n")
cat("runtime package version:", runtime_state$runtime_package_version, "\n")
cat("runtime source tree:", runtime_state$source_tree_digest, "\n")
cat("runtime source access:", runtime_state$source_access_mode, "\n")
focused <- c(
  "test-rqr-algebra.R",
  "test-rqr-mcmc-fixed-design.R",
  "test-rqr-learned-scale-mcmc.R",
  "test-rqr-desn-design-parity.R",
  "test-rqr-forecast-contract.R"
)
attestation <- readRDS(layout$attestation_path)
test_staging <- tempfile("exdqlm-tests-", tmpdir = layout$cache_root)
dir.create(test_staging)
on.exit(unlink(test_staging, recursive = TRUE, force = TRUE), add = TRUE)
utils::untar(attestation$source_archive_path, exdir = test_staging)
test_dir <- file.path(test_staging, "exdqlm", "tests", "testthat")
for (ff in focused) {
  path <- file.path(test_dir, ff)
  if (!file.exists(path)) stop("Missing focused test: ", path, call. = FALSE)
  cat("Running", ff, "\n")
  testthat::test_file(path, reporter = "summary")
}
source_after <- rqr_assert_external_checkout_unchanged(source_before)
guard_pending <- FALSE
if (!identical(source_before$guard_digest, source_after$guard_digest)) {
  stop("Protected source checkout guard failed.", call. = FALSE)
}
cat("protected source checkout unchanged:", source_after$guard_digest, "\n")
cat("Focused exdqlm RQR smoke tests completed.\n")
