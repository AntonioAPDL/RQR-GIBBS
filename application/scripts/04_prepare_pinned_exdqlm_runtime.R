#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}

pinned_commit <- "dffb71ee70b597d6a716ee74be1cbc99731cd453"
pinned_branch <- "feature/rqr-desn-readout-20260716"
exdqlm_repo <- Sys.getenv(
  "EXDQLM_RQR_REPO",
  unset = file.path(dirname(repo_root), "exdqlm__wt__qdesn_0p4p0_integration")
)
exdqlm_repo <- normalizePath(exdqlm_repo, winslash = "/", mustWork = TRUE)
cache_root <- normalizePath(
  Sys.getenv(
    "RQR_EXDQLM_RUNTIME_ROOT",
    unset = file.path(repo_root, "application", "cache", "exdqlm_runtime")
  ),
  winslash = "/", mustWork = FALSE
)
library_root <- file.path(cache_root, "library")
git_archive <- file.path(
  cache_root, paste0("exdqlm_git_", substr(pinned_commit, 1L, 12L), ".tar.gz")
)
attestation_path <- file.path(
  cache_root, "attestations",
  paste0("exdqlm_", substr(pinned_commit, 1L, 12L), ".rds")
)
dir.create(library_root, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(attestation_path), recursive = TRUE, showWarnings = FALSE)

git_value <- function(args) {
  out <- system2(
    "git", c("-C", shQuote(exdqlm_repo), args),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(out, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop("Git command failed: ", paste(args, collapse = " "), call. = FALSE)
  }
  trimws(paste(out, collapse = "\n"))
}
`%||%` <- function(x, y) if (is.null(x)) y else x

actual_commit <- tolower(git_value(c("rev-parse", "HEAD")))
actual_branch <- git_value(c("rev-parse", "--abbrev-ref", "HEAD"))
status <- git_value(c("status", "--porcelain", "--untracked-files=all"))
if (!identical(actual_commit, pinned_commit)) {
  stop("exdqlm is not at the pinned commit ", pinned_commit, ".", call. = FALSE)
}
if (!identical(actual_branch, pinned_branch)) {
  stop("exdqlm is not on the pinned branch ", pinned_branch, ".", call. = FALSE)
}
if (nzchar(status)) {
  stop("The pinned exdqlm checkout is dirty: ", status, call. = FALSE)
}
source_tree <- tolower(git_value(c("rev-parse", "HEAD^{tree}")))
source_version <- read.dcf(
  file.path(exdqlm_repo, "DESCRIPTION"), fields = "Version"
)[1L, 1L]
package_archive <- file.path(cache_root, paste0("exdqlm_", source_version, ".tar.gz"))

if (file.exists(git_archive)) unlink(git_archive)
archive_status <- system2(
  "git",
  c(
    "-C", shQuote(exdqlm_repo), "archive", "--format=tar.gz",
    "--prefix=exdqlm/", "-o", shQuote(git_archive), pinned_commit
  )
)
if (!identical(as.integer(archive_status), 0L) || !file.exists(git_archive)) {
  stop("Could not archive the pinned exdqlm commit.", call. = FALSE)
}

r_bin <- file.path(R.home("bin"), "R")
staging <- tempfile("exdqlm-build-", tmpdir = cache_root)
dir.create(staging)
utils::untar(git_archive, exdir = staging)
old_workdir <- setwd(staging)
on.exit(setwd(old_workdir), add = TRUE)
build_stdout <- file.path(cache_root, "build.stdout.log")
build_stderr <- file.path(cache_root, "build.stderr.log")
build_status <- system2(
  r_bin,
  c("CMD", "build", "--no-manual", "--no-build-vignettes", "exdqlm"),
  stdout = build_stdout, stderr = build_stderr
)
if (!identical(as.integer(build_status), 0L)) {
  cat(tail(readLines(build_stderr, warn = FALSE), 40L), sep = "\n")
  stop("R CMD build failed for the pinned exdqlm archive.", call. = FALSE)
}
built_archive <- file.path(staging, basename(package_archive))
if (!file.exists(built_archive)) {
  stop("R CMD build did not create the expected exdqlm source package.", call. = FALSE)
}
if (file.exists(package_archive)) unlink(package_archive)
if (!file.copy(built_archive, package_archive, overwrite = TRUE)) {
  stop("Could not preserve the built exdqlm source package.", call. = FALSE)
}
setwd(old_workdir)
install_stdout <- file.path(cache_root, "install.stdout.log")
install_stderr <- file.path(cache_root, "install.stderr.log")
install_status <- system2(
  r_bin,
  c(
    "CMD", "INSTALL", "--preclean", "--clean",
    paste0("--library=", shQuote(library_root)),
    shQuote(package_archive)
  ),
  stdout = install_stdout, stderr = install_stderr
)
if (!identical(as.integer(install_status), 0L)) {
  cat(tail(readLines(install_stderr, warn = FALSE), 40L), sep = "\n")
  stop("R CMD INSTALL failed for the pinned exdqlm archive.", call. = FALSE)
}
unlink(staging, recursive = TRUE)

runtime_path <- normalizePath(
  file.path(library_root, "exdqlm"), winslash = "/", mustWork = TRUE
)
runtime_version <- read.dcf(
  file.path(runtime_path, "DESCRIPTION"), fields = "Version"
)[1L, 1L]
if (!identical(runtime_version, source_version)) {
  stop("Installed exdqlm version does not match the pinned source.", call. = FALSE)
}
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required to create the runtime attestation.", call. = FALSE)
}
directory_digest <- function(path) {
  files <- list.files(
    path, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  files <- sort(normalizePath(files, winslash = "/", mustWork = TRUE))
  relative <- substring(files, nchar(path) + 2L)
  hashes <- vapply(
    files,
    function(file) digest::digest(file = file, algo = "sha256", serialize = FALSE),
    character(1L)
  )
  digest::digest(
    paste(relative, hashes, sep = "\t", collapse = "\n"),
    algo = "sha256", serialize = FALSE
  )
}
attestation <- list(
  schema_version = "rqrgibbs_runtime_attestation/1.0.0",
  package = "exdqlm",
  package_version = source_version,
  source_commit = pinned_commit,
  source_tree_digest = source_tree,
  source_archive_sha256 = digest::digest(
    file = git_archive, algo = "sha256", serialize = FALSE
  ),
  source_repo_root = exdqlm_repo,
  runtime_package_path = runtime_path,
  runtime_package_tree_digest = directory_digest(runtime_path),
  R_version = R.version.string,
  platform = R.version$platform,
  created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
saveRDS(attestation, attestation_path, version = 3)

cat("Pinned exdqlm runtime prepared.\n")
cat("  source commit:", pinned_commit, "\n")
cat("  source tree:", source_tree, "\n")
cat("  package version:", source_version, "\n")
cat("  library:", library_root, "\n")
cat("  runtime path:", runtime_path, "\n")
cat("  attestation:", attestation_path, "\n")
cat("Set R_LIBS_USER to the library path before starting a promotion run.\n")
