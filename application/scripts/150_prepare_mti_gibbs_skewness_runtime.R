#!/usr/bin/env Rscript
root <- dirname(normalizePath(system2("git", c("--no-optional-locks", "rev-parse", "--git-common-dir"), stdout = TRUE)))
pointer <- jsonlite::read_json(file.path(root, ".codex_work/mti_skewness_extension_latest.json"))
source <- file.path(pointer$checkout, "application/cache/skewness_library")
destination <- Sys.getenv("MTI_GIBBS_SKEWNESS_LIBRARY", "application/cache/gibbs_skewness_library")
receipt <- file.path(dirname(destination), "gibbs_skewness_runtime.json")
hash <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
if (file.exists(receipt)) {
  old <- jsonlite::read_json(receipt)
  paths <- file.path(destination, names(old$installed_hashes))
  stopifnot(all(file.exists(paths)), identical(unname(vapply(paths, hash, "")), unname(unlist(old$installed_hashes))))
  cat("Verified existing isolated Gibbs library:", normalizePath(destination), "\n")
  quit(save = "no")
}
stopifnot(dir.exists(source), !dir.exists(destination))
dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
stopifnot(system2("cp", shQuote(c("-a", source, destination))) == 0L)
.libPaths(c(normalizePath(destination), .libPaths()))
requested <- c("rqrgibbs", "posterior", "data.table", "jsonlite", "digest", "actuar", "statmod", "expint", "parallelly")
installed <- installed.packages()
installed <- installed[!duplicated(rownames(installed)), , drop = FALSE]
packages <- unique(c(requested, unlist(tools::package_dependencies(requested, db = installed,
  which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE))))
packages <- packages[packages %in% rownames(installed) & is.na(installed[packages, "Priority"])]
for (package in packages) if (!dir.exists(file.path(destination, package)))
  stopifnot(file.copy(find.package(package), destination, recursive = TRUE))
stopifnot(all(vapply(requested, requireNamespace, FALSE, quietly = TRUE)),
  packageVersion("rqrgibbs") == "0.1.0.9038")
files <- list.files(destination, recursive = TRUE, full.names = TRUE)
relative <- substring(files, nchar(destination) + 2L)
record <- list(parent_source_commit = pointer$source_commit,
  parent_inventory_sha256 = hash(file.path(pointer$checkout, "application/cache/skewness_runtime.json")),
  source_library = normalizePath(source), source_commit = system2("git", c("--no-optional-locks", "rev-parse", "HEAD"), stdout = TRUE),
  R = R.version.string, packages = as.list(setNames(vapply(packages,
    function(p) as.character(packageVersion(p)), ""), packages)),
  installed_hashes = as.list(setNames(vapply(files, hash, ""), relative)))
jsonlite::write_json(record, receipt, pretty = TRUE, auto_unbox = TRUE)
cat("Prepared isolated Gibbs library:", normalizePath(destination), "\n")
