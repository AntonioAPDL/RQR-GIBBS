#!/usr/bin/env Rscript

# Materialize the exact CRAN exdqlm 1.1.0 source package under the ignored
# RQR-GIBBS cache. No exdqlm source checkout is read, loaded, compiled, or
# installed by this script.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "isolated_runtime_lineage.R"
  ),
  envir = environment()
)

source_url <-
  "https://cran.r-project.org/src/contrib/exdqlm_1.1.0.tar.gz"
expected_sha256 <-
  "51bc968f617721c9ab1dcfc6ec14857d30827fcd36659f3de45337cc3c82bd14"
cache_root <- normalizePath(
  Sys.getenv(
    "RQR_EXDQLM_CRAN_RUNTIME_ROOT",
    unset = file.path(
      repo_root, "application", "cache", "exdqlm_cran_1.1.0"
    )
  ),
  winslash = "/", mustWork = FALSE
)
if (!startsWith(
      paste0(cache_root, "/"),
      paste0(normalizePath(
        file.path(repo_root, "application", "cache"),
        winslash = "/", mustWork = FALSE
      ), "/")
    )) {
  stop("The CRAN comparator runtime must remain under application/cache/.",
       call. = FALSE)
}
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
source_package <- file.path(cache_root, "exdqlm_1.1.0.tar.gz")
partial <- paste0(source_package, ".part")
unlink(partial, force = TRUE)
if (!file.exists(source_package) ||
    !identical(rqr_file_sha256(source_package), expected_sha256)) {
  utils::download.file(source_url, partial, mode = "wb", quiet = FALSE)
  if (!identical(rqr_file_sha256(partial), expected_sha256)) {
    stop("Downloaded exdqlm source package has the wrong SHA-256.",
         call. = FALSE)
  }
  if (file.exists(source_package)) unlink(source_package, force = TRUE)
  if (!file.rename(partial, source_package)) {
    stop("Could not atomically publish the exdqlm source package.",
         call. = FALSE)
  }
}

# read.dcf cannot read through a tar archive; inspect the archived DESCRIPTION
# in a private staging directory instead.
staging <- tempfile("exdqlm-cran-inspect-", tmpdir = cache_root)
dir.create(staging)
on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
utils::untar(source_package, files = "exdqlm/DESCRIPTION", exdir = staging)
description <- read.dcf(file.path(staging, "exdqlm", "DESCRIPTION"))
if (!identical(unname(description[1L, "Package"]), "exdqlm") ||
    !identical(unname(description[1L, "Version"]), "1.1.0")) {
  stop("The pinned comparator source is not exdqlm 1.1.0.", call. = FALSE)
}

library_root <- file.path(cache_root, "library")
runtime_path <- file.path(library_root, "exdqlm")
dir.create(library_root, recursive = TRUE, showWarnings = FALSE)
if (dir.exists(runtime_path)) {
  unlink(runtime_path, recursive = TRUE, force = TRUE)
}
if (dir.exists(runtime_path)) {
  stop("Could not clear the pre-existing isolated exdqlm runtime.",
       call. = FALSE)
}
r_bin <- file.path(R.home("bin"), "R")
stdout_path <- file.path(cache_root, "install.stdout.log")
stderr_path <- file.path(cache_root, "install.stderr.log")
arguments <- c(
  "CMD", "INSTALL", "--preclean", "--clean",
  paste0("--library=", shQuote(library_root)),
  shQuote(source_package)
)
started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
status <- system2(
  r_bin, arguments, stdout = stdout_path, stderr = stderr_path
)
ended_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
if (!identical(as.integer(status), 0L) || !dir.exists(runtime_path)) {
  if (file.exists(stderr_path)) {
    cat(tail(readLines(stderr_path, warn = FALSE), 80L), sep = "\n")
  }
  stop("The isolated exdqlm 1.1.0 installation failed.", call. = FALSE)
}
installed <- read.dcf(file.path(runtime_path, "DESCRIPTION"))
if (!identical(unname(installed[1L, "Package"]), "exdqlm") ||
    !identical(unname(installed[1L, "Version"]), "1.1.0")) {
  stop("The isolated runtime has the wrong package identity.", call. = FALSE)
}

attestation <- list(
  schema_version = "rqrgibbs_external_cran_runtime/1.0.0",
  package = "exdqlm",
  version = "1.1.0",
  source_url = source_url,
  source_package_path = normalizePath(source_package, winslash = "/"),
  source_package_sha256 = rqr_file_sha256(source_package),
  install_command = paste(c(r_bin, arguments), collapse = " "),
  install_input_count = 1L,
  install_exit_status = as.integer(status),
  install_started_at = started_at,
  install_ended_at = ended_at,
  install_stdout_sha256 = rqr_file_sha256(stdout_path),
  install_stderr_sha256 = rqr_file_sha256(stderr_path),
  runtime_path = normalizePath(runtime_path, winslash = "/"),
  runtime_tree_digest = rqr_directory_digest(runtime_path),
  R_version = R.version.string,
  platform = R.version$platform,
  protected_exdqlm_checkout_used = FALSE,
  generalized_bayes_target = FALSE,
  comparator_role = "reduced_AL_DQLM_MCMC_quantile_endpoint_comparator"
)
attestation_path <- file.path(
  cache_root, "exdqlm_1.1.0_runtime_attestation.json"
)
temporary <- paste0(attestation_path, ".part")
jsonlite::write_json(
  attestation, temporary, auto_unbox = TRUE, pretty = TRUE,
  null = "null"
)
if (file.exists(attestation_path)) unlink(attestation_path, force = TRUE)
if (!file.rename(temporary, attestation_path)) {
  stop("Could not atomically publish the comparator attestation.",
       call. = FALSE)
}
cat("Isolated CRAN exdqlm comparator prepared.\n")
cat("  source SHA-256:", attestation$source_package_sha256, "\n")
cat("  runtime:", attestation$runtime_path, "\n")
cat("  runtime digest:", attestation$runtime_tree_digest, "\n")
cat("  attestation:", normalizePath(attestation_path), "\n")
