#!/usr/bin/env Rscript

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "pinned_exdqlm_runtime.R"
  ),
  envir = environment()
)
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "isolated_runtime_lineage.R"
  ),
  envir = environment()
)

pinned_commit <- "dffb71ee70b597d6a716ee74be1cbc99731cd453"
pinned_branch <- "feature/rqr-desn-readout-20260716"
exdqlm_repo <- Sys.getenv(
  "EXDQLM_RQR_REPO",
  unset = file.path(dirname(repo_root), "exdqlm__wt__qdesn_0p4p0_integration")
)
exdqlm_repo <- normalizePath(exdqlm_repo, winslash = "/", mustWork = TRUE)
layout <- rqr_exdqlm_runtime_layout(
  repo_root = repo_root,
  exdqlm_repo = exdqlm_repo,
  pinned_commit = pinned_commit
)
cache_root <- layout$cache_root
library_root <- layout$library_root
git_archive <- layout$git_archive
attestation_path <- layout$attestation_path
dir.create(library_root, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(attestation_path), recursive = TRUE, showWarnings = FALSE)

source_before <- rqr_capture_external_checkout(exdqlm_repo)
guard_pending <- TRUE
on.exit({
  if (isTRUE(guard_pending)) {
    rqr_assert_external_checkout_unchanged(source_before)
  }
}, add = TRUE)

if (!identical(source_before$commit, pinned_commit)) {
  stop("exdqlm is not at the pinned commit ", pinned_commit, ".", call. = FALSE)
}
if (!identical(source_before$branch, pinned_branch)) {
  stop("exdqlm is not on the pinned branch ", pinned_branch, ".", call. = FALSE)
}
if (nzchar(source_before$status)) {
  stop(
    "The pinned exdqlm checkout is dirty: ",
    source_before$status,
    call. = FALSE
  )
}
source_tree <- source_before$tree

if (file.exists(git_archive)) unlink(git_archive)
archive_status <- system2(
  Sys.which("git"),
  c(
    "-C", shQuote(exdqlm_repo), "archive", "--format=tar.gz",
    "--prefix=exdqlm/", "-o", shQuote(git_archive), pinned_commit
  ),
  env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
)
if (!identical(as.integer(archive_status), 0L) || !file.exists(git_archive)) {
  stop("Could not archive the pinned exdqlm commit.", call. = FALSE)
}
archive_lineage <- rqr_verify_archive_matches_git(
  archive_path = git_archive,
  archive_prefix = "exdqlm",
  repo_root = exdqlm_repo,
  commit = pinned_commit,
  source_subdir = "."
)
if (!isTRUE(archive_lineage$match)) {
  stop(
    "The exdqlm source archive does not match the pinned Git tree.",
    call. = FALSE
  )
}

r_bin <- file.path(R.home("bin"), "R")
staging <- tempfile("exdqlm-build-", tmpdir = cache_root)
dir.create(staging)
on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
utils::untar(git_archive, exdir = staging)
source_version <- read.dcf(
  file.path(staging, "exdqlm", "DESCRIPTION"), fields = "Version"
)[1L, 1L]
package_archive <- file.path(
  cache_root, paste0("exdqlm_", source_version, ".tar.gz")
)
source_archive_sha256 <- rqr_file_sha256(git_archive)
old_workdir <- setwd(staging)
on.exit(setwd(old_workdir), add = TRUE)
build_stdout <- file.path(cache_root, "build.stdout.log")
build_stderr <- file.path(cache_root, "build.stderr.log")
build_arguments <- c(
  "CMD", "build", "--no-manual", "--no-build-vignettes", "exdqlm"
)
build_command <- rqr_write_command_receipt(
  path = file.path(cache_root, "build.command.rds"),
  executable = r_bin,
  arguments = build_arguments,
  working_directory = staging,
  input_path = file.path(staging, "exdqlm"),
  input_sha256 = source_archive_sha256
)
build_status <- system2(
  r_bin,
  build_arguments,
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
source_package_sha256 <- rqr_file_sha256(package_archive)
source_package_lineage <- rqr_source_package_lineage(
  source_archive_path = git_archive,
  source_archive_prefix = "exdqlm",
  source_package_path = package_archive
)
if (!isTRUE(source_package_lineage$match)) {
  stop(
    "The built exdqlm source package is not traceable to the Git archive.",
    call. = FALSE
  )
}
setwd(old_workdir)
install_stdout <- file.path(cache_root, "install.stdout.log")
install_stderr <- file.path(cache_root, "install.stderr.log")
install_arguments <- c(
  "CMD", "INSTALL", "--preclean", "--clean",
  paste0("--library=", shQuote(library_root)),
  shQuote(package_archive)
)
install_command <- rqr_write_command_receipt(
  path = file.path(cache_root, "install.command.rds"),
  executable = r_bin,
  arguments = install_arguments,
  working_directory = cache_root,
  input_path = package_archive,
  input_sha256 = source_package_sha256
)
install_status <- system2(
  r_bin,
  install_arguments,
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
runtime_lineage_marker_path <- file.path(
  runtime_path, "RQR-RUNTIME-LINEAGE.rds"
)
saveRDS(
  rqr_runtime_lineage_marker(
    package = "exdqlm",
    package_version = source_version,
    source_package_sha256 = source_package_sha256,
    built_source_manifest_digest =
      source_package_lineage$built_source_manifest_digest,
    install_command_receipt_sha256 = install_command$sha256
  ),
  runtime_lineage_marker_path,
  version = 3
)
source_after <- rqr_assert_external_checkout_unchanged(source_before)
guard_pending <- FALSE
if (rqr_path_within(runtime_path, exdqlm_repo)) {
  stop("The installed runtime must not reside in the exdqlm checkout.", call. = FALSE)
}
runtime_tree_digest <- rqr_directory_digest(runtime_path)
build_stdout_sha256 <- rqr_file_sha256(build_stdout)
build_stderr_sha256 <- rqr_file_sha256(build_stderr)
install_stdout_sha256 <- rqr_file_sha256(install_stdout)
install_stderr_sha256 <- rqr_file_sha256(install_stderr)
runtime_lineage_marker_sha256 <- rqr_file_sha256(
  runtime_lineage_marker_path
)
install_receipt_digest <- rqr_runtime_install_receipt(
  source_archive_sha256 = source_archive_sha256,
  source_package_sha256 = source_package_sha256,
  built_source_manifest_digest =
    source_package_lineage$built_source_manifest_digest,
  runtime_package_tree_digest = runtime_tree_digest,
  build_stdout_sha256 = build_stdout_sha256,
  build_stderr_sha256 = build_stderr_sha256,
  install_stdout_sha256 = install_stdout_sha256,
  install_stderr_sha256 = install_stderr_sha256,
  build_command_receipt_sha256 = build_command$sha256,
  install_command_receipt_sha256 = install_command$sha256,
  runtime_lineage_marker_sha256 = runtime_lineage_marker_sha256,
  R_version = R.version.string,
  platform = R.version$platform
)
attestation <- list(
  schema_version = "rqrgibbs_runtime_attestation/4.0.0",
  package = "exdqlm",
  package_version = source_version,
  source_commit = pinned_commit,
  source_tree_digest = source_tree,
  source_repo_root = exdqlm_repo,
  source_subdir = ".",
  source_access_mode = "git_archive_read_only",
  source_archive_prefix = "exdqlm",
  source_checkout_snapshot_before = source_before$guard_digest,
  source_checkout_snapshot_after = source_after$guard_digest,
  source_checkout_unchanged =
    identical(source_before$guard_digest, source_after$guard_digest),
  source_archive_path = normalizePath(
    git_archive, winslash = "/", mustWork = TRUE
  ),
  source_archive_sha256 = source_archive_sha256,
  source_git_manifest_digest =
    archive_lineage$source_git_manifest_digest,
  source_archive_manifest_digest =
    archive_lineage$source_archive_manifest_digest,
  source_archive_tree_match = archive_lineage$match,
  source_manifest_entries = archive_lineage$entries,
  source_archive_isolated_from_source =
    !rqr_path_within(git_archive, exdqlm_repo) &&
    !rqr_path_within(exdqlm_repo, git_archive),
  source_package_path = normalizePath(
    package_archive, winslash = "/", mustWork = TRUE
  ),
  source_package_sha256 = source_package_sha256,
  source_package_archive_match = source_package_lineage$match,
  built_source_manifest_digest =
    source_package_lineage$built_source_manifest_digest,
  built_source_manifest_entries =
    source_package_lineage$built_source_manifest_entries,
  build_stdout_path = normalizePath(
    build_stdout, winslash = "/", mustWork = TRUE
  ),
  build_stdout_sha256 = build_stdout_sha256,
  build_stderr_path = normalizePath(
    build_stderr, winslash = "/", mustWork = TRUE
  ),
  build_stderr_sha256 = build_stderr_sha256,
  install_stdout_path = normalizePath(
    install_stdout, winslash = "/", mustWork = TRUE
  ),
  install_stdout_sha256 = install_stdout_sha256,
  install_stderr_path = normalizePath(
    install_stderr, winslash = "/", mustWork = TRUE
  ),
  install_stderr_sha256 = install_stderr_sha256,
  build_command_receipt_path = build_command$path,
  build_command_receipt_sha256 = build_command$sha256,
  build_executable = normalizePath(
    r_bin, winslash = "/", mustWork = TRUE
  ),
  build_arguments = build_arguments,
  build_working_directory = normalizePath(
    staging, winslash = "/", mustWork = FALSE
  ),
  build_input_path = normalizePath(
    file.path(staging, "exdqlm"), winslash = "/", mustWork = FALSE
  ),
  install_command_receipt_path = install_command$path,
  install_command_receipt_sha256 = install_command$sha256,
  install_executable = normalizePath(
    r_bin, winslash = "/", mustWork = TRUE
  ),
  install_arguments = install_arguments,
  install_working_directory = normalizePath(
    cache_root, winslash = "/", mustWork = TRUE
  ),
  install_input_path = normalizePath(
    package_archive, winslash = "/", mustWork = TRUE
  ),
  runtime_package_path = runtime_path,
  runtime_lineage_marker_path = normalizePath(
    runtime_lineage_marker_path, winslash = "/", mustWork = TRUE
  ),
  runtime_lineage_marker_sha256 = runtime_lineage_marker_sha256,
  runtime_package_tree_digest = runtime_tree_digest,
  runtime_isolated_from_source = !rqr_path_within(runtime_path, exdqlm_repo),
  runtime_install_receipt_digest = install_receipt_digest,
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
cat("  source access: immutable Git archive; checkout unchanged\n")
cat("Set R_LIBS_USER to the library path before starting a promotion run.\n")
