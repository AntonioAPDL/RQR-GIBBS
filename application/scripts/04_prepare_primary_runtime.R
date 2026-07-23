#!/usr/bin/env Rscript

# Build rqrgibbs from the exact committed application/ subtree into an ignored,
# isolated library.  Promotion-grade runs load only this runtime and verify its
# content-level source lineage before fitting.

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
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required.", call. = FALSE)
}

expected_commit <- tolower(Sys.getenv(
  "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
))
if (!grepl("^[0-9a-f]{40}$", expected_commit)) {
  stop(
    paste(
      "Set RQR_EXPECTED_PRIMARY_COMMIT to the reviewed, complete",
      "RQR-GIBBS commit."
    ),
    call. = FALSE
  )
}

git_snapshot <- function() {
  fields <- c(
    branch = rqr_readonly_git(
      repo_root, c("rev-parse", "--abbrev-ref", "HEAD")
    ),
    commit = tolower(rqr_readonly_git(
      repo_root, c("rev-parse", "HEAD")
    )),
    status = rqr_readonly_git(
      repo_root, c("status", "--porcelain=v2", "--untracked-files=all")
    ),
    refs = rqr_readonly_git(
      repo_root, c("show-ref", "--head", "--dereference")
    ),
    local_config = rqr_readonly_git(
      repo_root, c("config", "--local", "--list", "--show-origin")
    )
  )
  list(
    fields = fields,
    digest = digest::digest(
      paste(names(fields), fields, sep = "=", collapse = "\n"),
      algo = "sha256", serialize = FALSE
    )
  )
}

source_before <- git_snapshot()
if (!identical(source_before$fields[["branch"]], "main") ||
    !identical(source_before$fields[["commit"]], expected_commit) ||
    nzchar(source_before$fields[["status"]])) {
  stop(
    "The primary source must be clean, on main, and at the expected commit.",
    call. = FALSE
  )
}

cache_root <- normalizePath(
  Sys.getenv(
    "RQR_PRIMARY_RUNTIME_ROOT",
    unset = file.path(dirname(repo_root), ".rqr_gibbs_primary_runtime")
  ),
  winslash = "/", mustWork = FALSE
)
if (rqr_path_within(cache_root, repo_root) ||
    rqr_path_within(repo_root, cache_root)) {
  stop(
    "RQR_PRIMARY_RUNTIME_ROOT and the primary checkout must be disjoint.",
    call. = FALSE
  )
}
commit_root <- file.path(cache_root, expected_commit)
library_root <- file.path(commit_root, "library")
attestation_path <- file.path(
  commit_root, "attestations",
  paste0("rqrgibbs_", expected_commit, ".rds")
)
git_archive <- file.path(
  commit_root,
  paste0("rqrgibbs_git_", expected_commit, ".tar.gz")
)
dir.create(library_root, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(attestation_path), recursive = TRUE, showWarnings = FALSE)

if (file.exists(git_archive)) unlink(git_archive)
archive_status <- system2(
  Sys.which("git"),
  c(
    "-C", shQuote(repo_root), "archive", "--format=tar.gz",
    "--prefix=rqrgibbs/", "-o", shQuote(git_archive),
    paste0(expected_commit, ":application")
  ),
  env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
)
if (!identical(as.integer(archive_status), 0L) ||
    !file.exists(git_archive)) {
  stop("Could not archive the primary application subtree.", call. = FALSE)
}
archive_lineage <- rqr_verify_archive_matches_git(
  archive_path = git_archive,
  archive_prefix = "rqrgibbs",
  repo_root = repo_root,
  commit = expected_commit,
  source_subdir = "application"
)
if (!isTRUE(archive_lineage$match)) {
  stop(
    "The primary source archive does not match HEAD:application.",
    call. = FALSE
  )
}

staging <- tempfile("rqrgibbs-build-", tmpdir = commit_root)
dir.create(staging)
on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
utils::untar(git_archive, exdir = staging)
source_version <- read.dcf(
  file.path(staging, "rqrgibbs", "DESCRIPTION"), fields = "Version"
)[1L, 1L]
r_bin <- file.path(R.home("bin"), "R")
build_stdout <- file.path(commit_root, "build.stdout.log")
build_stderr <- file.path(commit_root, "build.stderr.log")
source_archive_sha256 <- rqr_file_sha256(git_archive)
build_arguments <- c(
  "CMD", "build", "--no-manual", "--no-build-vignettes",
  "rqrgibbs"
)
build_command <- rqr_write_command_receipt(
  path = file.path(commit_root, "build.command.rds"),
  executable = r_bin,
  arguments = build_arguments,
  working_directory = staging,
  input_path = file.path(staging, "rqrgibbs"),
  input_sha256 = source_archive_sha256
)
old_workdir <- setwd(staging)
on.exit(setwd(old_workdir), add = TRUE)
build_status <- system2(
  r_bin,
  build_arguments,
  stdout = build_stdout, stderr = build_stderr
)
if (!identical(as.integer(build_status), 0L)) {
  cat(tail(readLines(build_stderr, warn = FALSE), 40L), sep = "\n")
  stop("R CMD build failed for the primary Git archive.", call. = FALSE)
}
built_archive <- file.path(
  staging, paste0("rqrgibbs_", source_version, ".tar.gz")
)
if (!file.exists(built_archive)) {
  stop("R CMD build did not create the expected source package.", call. = FALSE)
}
package_archive <- file.path(commit_root, basename(built_archive))
if (file.exists(package_archive)) unlink(package_archive)
if (!file.copy(built_archive, package_archive, overwrite = TRUE)) {
  stop("Could not preserve the primary source package.", call. = FALSE)
}
source_package_sha256 <- rqr_file_sha256(package_archive)
source_package_lineage <- rqr_source_package_lineage(
  source_archive_path = git_archive,
  source_archive_prefix = "rqrgibbs",
  source_package_path = package_archive
)
if (!isTRUE(source_package_lineage$match)) {
  stop(
    "The built primary source package is not traceable to the Git archive.",
    call. = FALSE
  )
}
setwd(old_workdir)

install_stdout <- file.path(commit_root, "install.stdout.log")
install_stderr <- file.path(commit_root, "install.stderr.log")
install_arguments <- c(
  "CMD", "INSTALL", "--preclean", "--clean",
  paste0("--library=", shQuote(library_root)),
  shQuote(package_archive)
)
install_command <- rqr_write_command_receipt(
  path = file.path(commit_root, "install.command.rds"),
  executable = r_bin,
  arguments = install_arguments,
  working_directory = commit_root,
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
  stop("R CMD INSTALL failed for the primary source package.", call. = FALSE)
}
runtime_path <- normalizePath(
  file.path(library_root, "rqrgibbs"), winslash = "/", mustWork = TRUE
)
runtime_version <- read.dcf(
  file.path(runtime_path, "DESCRIPTION"), fields = "Version"
)[1L, 1L]
if (!identical(runtime_version, source_version)) {
  stop("Installed primary version does not match its source.", call. = FALSE)
}
runtime_lineage_marker_path <- file.path(
  runtime_path, "RQR-RUNTIME-LINEAGE.rds"
)
saveRDS(
  rqr_runtime_lineage_marker(
    package = "rqrgibbs",
    package_version = source_version,
    source_package_sha256 = source_package_sha256,
    built_source_manifest_digest =
      source_package_lineage$built_source_manifest_digest,
    install_command_receipt_sha256 = install_command$sha256
  ),
  runtime_lineage_marker_path,
  version = 3
)
source_after <- git_snapshot()
if (!identical(source_before$digest, source_after$digest)) {
  stop(
    "The primary Git state changed while preparing its isolated runtime.",
    call. = FALSE
  )
}

source_tree <- tolower(rqr_readonly_git(
  repo_root, c("rev-parse", paste0(expected_commit, ":application"))
))
runtime_tree_digest <- rqr_directory_digest(runtime_path)
build_stdout_sha256 <- rqr_file_sha256(build_stdout)
build_stderr_sha256 <- rqr_file_sha256(build_stderr)
install_stdout_sha256 <- rqr_file_sha256(install_stdout)
install_stderr_sha256 <- rqr_file_sha256(install_stderr)
runtime_lineage_marker_sha256 <- rqr_file_sha256(
  runtime_lineage_marker_path
)
attestation <- list(
  schema_version = "rqrgibbs_runtime_attestation/4.0.0",
  package = "rqrgibbs",
  package_version = source_version,
  source_commit = expected_commit,
  source_tree_digest = source_tree,
  source_repo_root = repo_root,
  source_subdir = "application",
  source_access_mode = "git_archive_read_only",
  source_archive_prefix = "rqrgibbs",
  source_checkout_snapshot_before = source_before$digest,
  source_checkout_snapshot_after = source_after$digest,
  source_checkout_unchanged =
    identical(source_before$digest, source_after$digest),
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
  source_archive_isolated_from_source = TRUE,
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
    staging, winslash = "/", mustWork = TRUE
  ),
  build_input_path = normalizePath(
    file.path(staging, "rqrgibbs"), winslash = "/", mustWork = TRUE
  ),
  install_command_receipt_path = install_command$path,
  install_command_receipt_sha256 = install_command$sha256,
  install_executable = normalizePath(
    r_bin, winslash = "/", mustWork = TRUE
  ),
  install_arguments = install_arguments,
  install_working_directory = normalizePath(
    commit_root, winslash = "/", mustWork = TRUE
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
  runtime_isolated_from_source = TRUE,
  runtime_install_receipt_digest = rqr_runtime_install_receipt(
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
  ),
  R_version = R.version.string,
  platform = R.version$platform,
  created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
saveRDS(attestation, attestation_path, version = 3)

cat("Isolated primary rqrgibbs runtime prepared.\n")
cat("  source commit:", expected_commit, "\n")
cat("  application tree:", source_tree, "\n")
cat("  package version:", source_version, "\n")
cat("  library:", library_root, "\n")
cat("  runtime path:", runtime_path, "\n")
cat("  attestation:", attestation_path, "\n")
cat("Set R_LIBS_USER to the library path before a promotion run.\n")
