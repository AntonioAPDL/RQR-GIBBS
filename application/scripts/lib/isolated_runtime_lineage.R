# Content-level lineage helpers for isolated R package runtimes.
#
# These functions intentionally use only read-only Git commands.  They compare
# the mode, Git blob identifier, and relative path of every archived source
# entry with the declared commit (or commit subtree) before R CMD build runs.

rqr_manifest_payload <- function(manifest) {
  manifest <- manifest[order(manifest$path), , drop = FALSE]
  paste(
    manifest$mode, manifest$type, manifest$object, manifest$path,
    sep = "\t", collapse = "\n"
  )
}

rqr_manifest_digest <- function(manifest) {
  digest::digest(
    rqr_manifest_payload(manifest),
    algo = "sha256", serialize = FALSE
  )
}

rqr_git_tree_manifest <- function(
    repo_root, commit, source_subdir = ".") {
  treeish <- if (identical(source_subdir, ".")) {
    commit
  } else {
    paste0(commit, ":", source_subdir)
  }
  raw <- rqr_readonly_git(
    repo_root, c("ls-tree", "-r", "--full-tree", treeish)
  )
  lines <- if (nzchar(raw)) strsplit(raw, "\n", fixed = TRUE)[[1L]] else {
    character(0)
  }
  pattern <- "^([0-9]{6}) ([^ ]+) ([0-9a-f]{40,64})\\t(.*)$"
  matches <- regexec(pattern, lines)
  fields <- regmatches(lines, matches)
  if (!length(fields) || any(lengths(fields) != 5L)) {
    stop("Could not parse the declared Git tree manifest.", call. = FALSE)
  }
  manifest <- data.frame(
    mode = vapply(fields, `[[`, character(1L), 2L),
    type = vapply(fields, `[[`, character(1L), 3L),
    object = tolower(vapply(fields, `[[`, character(1L), 4L)),
    path = vapply(fields, `[[`, character(1L), 5L),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(manifest$path) || any(manifest$type != "blob")) {
    stop(
      "The package source tree must contain unique blob paths only.",
      call. = FALSE
    )
  }
  manifest[order(manifest$path), , drop = FALSE]
}

rqr_git_blob_id <- function(path, link_target = NULL) {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Git is required.", call. = FALSE)
  hash_path <- path
  temporary <- NULL
  if (!is.null(link_target)) {
    temporary <- tempfile("rqr-link-target-")
    con <- file(temporary, open = "wb")
    on.exit(close(con), add = TRUE)
    writeBin(charToRaw(link_target), con)
    close(con)
    on.exit(NULL, add = FALSE)
    hash_path <- temporary
    on.exit(unlink(temporary), add = TRUE)
  }
  out <- suppressWarnings(system2(
    git, c("hash-object", "--no-filters", shQuote(hash_path)),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  value <- trimws(paste(out, collapse = "\n"))
  if (!identical(as.integer(status), 0L) ||
      !grepl("^[0-9a-f]{40,64}$", value)) {
    stop("Could not compute an archived Git blob identifier.", call. = FALSE)
  }
  tolower(value)
}

rqr_archive_tree_manifest <- function(archive_path, archive_prefix) {
  archive_path <- normalizePath(
    archive_path, winslash = "/", mustWork = TRUE
  )
  archive_prefix <- sub("/+$", "", as.character(archive_prefix)[1L])
  if (is.na(archive_prefix) || !nzchar(archive_prefix) ||
      grepl("(^|/)\\.\\.(/|$)", archive_prefix) ||
      startsWith(archive_prefix, "/")) {
    stop("archive_prefix must be one safe relative path.", call. = FALSE)
  }
  entries <- utils::untar(archive_path, list = TRUE)
  entries <- sub("^\\./", "", entries)
  if (!length(entries) || anyNA(entries) || any(!nzchar(entries)) ||
      any(startsWith(entries, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", entries)) ||
      anyDuplicated(entries)) {
    stop("The source archive contains unsafe or duplicate paths.", call. = FALSE)
  }
  prefix_with_slash <- paste0(archive_prefix, "/")
  if (any(
        entries != archive_prefix &
          !startsWith(entries, prefix_with_slash)
      )) {
    stop(
      "The source archive contains entries outside its declared prefix.",
      call. = FALSE
    )
  }
  extraction_root <- tempfile("rqr-lineage-archive-")
  dir.create(extraction_root)
  on.exit(unlink(extraction_root, recursive = TRUE, force = TRUE), add = TRUE)
  utils::untar(archive_path, exdir = extraction_root)
  source_root <- file.path(extraction_root, archive_prefix)
  if (!dir.exists(source_root)) {
    stop("The source archive did not produce its declared root.", call. = FALSE)
  }
  paths <- list.files(
    source_root, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  paths <- sort(paths)
  relative <- substring(paths, nchar(source_root) + 2L)
  links <- Sys.readlink(paths)
  info <- file.info(paths)
  if (anyNA(info$isdir) || any(info$isdir) || anyDuplicated(relative)) {
    stop("The extracted source manifest is not a unique file set.", call. = FALSE)
  }
  modes <- vapply(seq_along(paths), function(index) {
    if (nzchar(links[index])) return("120000")
    executable <- bitwAnd(as.integer(info$mode[index]), 73L) != 0L
    if (executable) "100755" else "100644"
  }, character(1L))
  objects <- vapply(seq_along(paths), function(index) {
    if (nzchar(links[index])) {
      rqr_git_blob_id(paths[index], link_target = links[index])
    } else {
      rqr_git_blob_id(paths[index])
    }
  }, character(1L))
  manifest <- data.frame(
    mode = modes,
    type = "blob",
    object = objects,
    path = relative,
    stringsAsFactors = FALSE
  )
  manifest[order(manifest$path), , drop = FALSE]
}

rqr_verify_archive_matches_git <- function(
    archive_path, archive_prefix, repo_root, commit,
    source_subdir = ".") {
  git_manifest <- rqr_git_tree_manifest(
    repo_root, commit, source_subdir = source_subdir
  )
  archive_manifest <- rqr_archive_tree_manifest(
    archive_path, archive_prefix
  )
  git_payload <- rqr_manifest_payload(git_manifest)
  archive_payload <- rqr_manifest_payload(archive_manifest)
  list(
    match = identical(git_payload, archive_payload),
    source_git_manifest_digest = digest::digest(
      git_payload, algo = "sha256", serialize = FALSE
    ),
    source_archive_manifest_digest = digest::digest(
      archive_payload, algo = "sha256", serialize = FALSE
    ),
    entries = nrow(git_manifest)
  )
}

rqr_file_sha256 <- function(path) {
  digest::digest(
    file = normalizePath(path, winslash = "/", mustWork = TRUE),
    algo = "sha256", serialize = FALSE
  )
}

rqr_directory_digest <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- list.files(
    path, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  files <- sort(normalizePath(files, winslash = "/", mustWork = TRUE))
  relative <- substring(files, nchar(path) + 2L)
  hashes <- vapply(files, rqr_file_sha256, character(1L))
  digest::digest(
    paste(relative, hashes, sep = "\t", collapse = "\n"),
    algo = "sha256", serialize = FALSE
  )
}

rqr_runtime_install_receipt <- function(
    source_archive_sha256, source_package_sha256,
    runtime_package_tree_digest, build_log_sha256,
    install_log_sha256, R_version, platform) {
  fields <- c(
    source_archive_sha256 = source_archive_sha256,
    source_package_sha256 = source_package_sha256,
    runtime_package_tree_digest = runtime_package_tree_digest,
    build_log_sha256 = build_log_sha256,
    install_log_sha256 = install_log_sha256,
    R_version = R_version,
    platform = platform
  )
  digest::digest(
    paste(names(fields), fields, sep = "=", collapse = "\n"),
    algo = "sha256", serialize = FALSE
  )
}
