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

rqr_directory_manifest <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- list.files(
    path, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  files <- sort(files)
  relative <- substring(files, nchar(path) + 2L)
  info <- file.info(files)
  links <- Sys.readlink(files)
  if (anyNA(info$mode) || anyDuplicated(relative)) {
    stop("The runtime directory is not a unique readable file set.", call. = FALSE)
  }
  kind <- ifelse(nzchar(links), "symlink", "file")
  mode <- sprintf("%04o", bitwAnd(as.integer(info$mode), 511L))
  content <- vapply(seq_along(files), function(index) {
    if (nzchar(links[index])) {
      digest::digest(links[index], algo = "sha256", serialize = FALSE)
    } else {
      rqr_file_sha256(files[index])
    }
  }, character(1L))
  data.frame(
    kind = kind, mode = mode, content = content, path = relative,
    stringsAsFactors = FALSE
  )
}

rqr_directory_manifest_payload <- function(path) {
  manifest <- rqr_directory_manifest(path)
  paste(
    manifest$kind, manifest$mode, manifest$content, manifest$path,
    sep = "\t", collapse = "\n"
  )
}

rqr_directory_digest <- function(path) {
  digest::digest(
    rqr_directory_manifest_payload(path),
    algo = "sha256", serialize = FALSE
  )
}

rqr_normalized_file_sha256 <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = file.info(path)$size)
  if (any(bytes == as.raw(0L))) {
    return(rqr_file_sha256(path))
  }
  value <- rawToChar(bytes)
  value <- gsub("\r\n?", "\n", value, perl = TRUE)
  value <- sub("\n*$", "\n", value, perl = TRUE)
  digest::digest(value, algo = "sha256", serialize = FALSE)
}

rqr_safe_archive_extract <- function(archive_path, prefix = NULL) {
  archive_path <- normalizePath(archive_path, winslash = "/", mustWork = TRUE)
  entries <- sub("^\\./", "", utils::untar(archive_path, list = TRUE))
  if (!length(entries) || anyNA(entries) || any(!nzchar(entries)) ||
      any(startsWith(entries, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", entries))) {
    stop("Package archive paths are unsafe.", call. = FALSE)
  }
  roots <- unique(sub("/.*$", "", entries))
  if (is.null(prefix)) {
    if (length(roots) != 1L) {
      stop("Package archive must contain exactly one top-level root.", call. = FALSE)
    }
    prefix <- roots
  }
  prefix <- sub("/+$", "", as.character(prefix)[1L])
  if (!identical(roots, prefix)) {
    stop("Package archive root does not match its declaration.", call. = FALSE)
  }
  extraction_root <- tempfile("rqr-package-lineage-")
  dir.create(extraction_root)
  utils::untar(archive_path, exdir = extraction_root)
  list(
    extraction_root = extraction_root,
    package_root = file.path(extraction_root, prefix),
    prefix = prefix
  )
}

rqr_source_package_lineage <- function(
    source_archive_path, source_archive_prefix, source_package_path) {
  source <- rqr_safe_archive_extract(
    source_archive_path, source_archive_prefix
  )
  built <- rqr_safe_archive_extract(source_package_path)
  on.exit(
    unlink(source$extraction_root, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  on.exit(
    unlink(built$extraction_root, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  built_files <- list.files(
    built$package_root, recursive = TRUE, all.files = TRUE,
    full.names = TRUE, include.dirs = FALSE, no.. = TRUE
  )
  built_relative <- substring(
    built_files, nchar(built$package_root) + 2L
  )
  source_files <- file.path(source$package_root, built_relative)
  comparable <- built_relative != "DESCRIPTION"
  missing_source <- built_relative[comparable & !file.exists(source_files)]
  comparable_index <- which(comparable & file.exists(source_files))
  changed_source <- built_relative[comparable_index[vapply(
    comparable_index, function(index) {
      !identical(
        rqr_normalized_file_sha256(built_files[index]),
        rqr_normalized_file_sha256(source_files[index])
      )
    }, logical(1L)
  )]]
  changed_mode <- built_relative[comparable_index[vapply(
    comparable_index, function(index) {
      built_link <- Sys.readlink(built_files[index])
      source_link <- Sys.readlink(source_files[index])
      if (nzchar(built_link) || nzchar(source_link)) {
        return(!identical(built_link, source_link))
      }
      built_executable <- bitwAnd(
        as.integer(file.info(built_files[index])$mode), 73L
      ) != 0L
      source_executable <- bitwAnd(
        as.integer(file.info(source_files[index])$mode), 73L
      ) != 0L
      !identical(built_executable, source_executable)
    }, logical(1L)
  )]]
  source_description <- tryCatch(
    read.dcf(file.path(source$package_root, "DESCRIPTION")),
    error = function(e) NULL
  )
  built_description <- tryCatch(
    read.dcf(file.path(built$package_root, "DESCRIPTION")),
    error = function(e) NULL
  )
  allowed_description_transformations <- c(
    "Packaged", "Built", "NeedsCompilation", "Depends",
    "Author", "Maintainer"
  )
  source_fields <- if (is.null(source_description)) {
    character(0)
  } else {
    colnames(source_description)
  }
  built_fields <- if (is.null(built_description)) {
    character(0)
  } else {
    colnames(built_description)
  }
  compared_fields <- setdiff(
    source_fields, allowed_description_transformations
  )
  normalize_dcf <- function(value) {
    gsub("[[:space:]]+", " ", trimws(as.character(value)))
  }
  description_match <- length(compared_fields) > 0L &&
    all(compared_fields %in% built_fields) &&
    !length(setdiff(
      built_fields, c(source_fields, allowed_description_transformations)
    )) &&
    all(vapply(compared_fields, function(field) {
      identical(
        normalize_dcf(source_description[1L, field]),
        normalize_dcf(built_description[1L, field])
      )
    }, logical(1L)))
  source_relative <- list.files(
    source$package_root, recursive = TRUE, all.files = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  critical <- grepl("^(R/|src/|NAMESPACE$)", source_relative)
  critical <- critical & !grepl(
    "\\.(o|so|dll|dylib|a|sl|gch)$|(^|/)symbols\\.rds$",
    source_relative, ignore.case = TRUE
  )
  critical <- critical & !grepl("(^|/)\\.", source_relative)
  buildignore_path <- file.path(source$package_root, ".Rbuildignore")
  if (file.exists(buildignore_path)) {
    patterns <- trimws(readLines(buildignore_path, warn = FALSE))
    patterns <- patterns[nzchar(patterns) & !startsWith(patterns, "#")]
    if (length(patterns)) {
      ignored <- vapply(source_relative, function(path) {
        any(vapply(patterns, function(pattern) {
          tryCatch(
            grepl(pattern, path, perl = TRUE, ignore.case = TRUE),
            error = function(e) {
              stop("Invalid .Rbuildignore expression.", call. = FALSE)
            }
          )
        }, logical(1L)))
      }, logical(1L))
      critical <- critical & !ignored
    }
  }
  missing_critical <- setdiff(
    source_relative[critical], built_relative
  )
  built_manifest_digest <- rqr_directory_digest(built$package_root)
  list(
    match = description_match &&
      !length(missing_source) &&
      !length(changed_source) &&
      !length(changed_mode) &&
      !length(missing_critical),
    description_match = description_match,
    missing_source_entries = sort(missing_source),
    changed_source_entries = sort(changed_source),
    changed_mode_entries = sort(changed_mode),
    missing_critical_entries = sort(missing_critical),
    built_source_manifest_digest = built_manifest_digest,
    built_source_manifest_entries = length(built_files),
    built_package = if (is.null(built_description)) {
      NA_character_
    } else {
      as.character(built_description[1L, "Package"])
    },
    built_version = if (is.null(built_description)) {
      NA_character_
    } else {
      as.character(built_description[1L, "Version"])
    }
  )
}

rqr_write_command_receipt <- function(
    path, executable, arguments, working_directory, input_path,
    input_sha256) {
  receipt <- list(
    executable = normalizePath(
      executable, winslash = "/", mustWork = TRUE
    ),
    arguments = as.character(arguments),
    working_directory = normalizePath(
      working_directory, winslash = "/", mustWork = TRUE
    ),
    input_path = normalizePath(
      input_path, winslash = "/", mustWork = TRUE
    ),
    input_sha256 = tolower(as.character(input_sha256)[1L])
  )
  saveRDS(receipt, path, version = 3)
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    sha256 = rqr_file_sha256(path),
    receipt = receipt
  )
}

rqr_runtime_lineage_marker <- function(
    package, package_version, source_package_sha256,
    built_source_manifest_digest, install_command_receipt_sha256) {
  list(
    schema_version = "rqrgibbs_runtime_lineage_marker/1.0.0",
    package = package,
    package_version = package_version,
    source_package_sha256 = source_package_sha256,
    built_source_manifest_digest = built_source_manifest_digest,
    install_command_receipt_sha256 = install_command_receipt_sha256
  )
}

rqr_runtime_install_receipt <- function(
    source_archive_sha256, source_package_sha256,
    built_source_manifest_digest, runtime_package_tree_digest,
    build_stdout_sha256, build_stderr_sha256,
    install_stdout_sha256, install_stderr_sha256,
    build_command_receipt_sha256, install_command_receipt_sha256,
    runtime_lineage_marker_sha256, R_version, platform) {
  fields <- c(
    source_archive_sha256 = source_archive_sha256,
    source_package_sha256 = source_package_sha256,
    built_source_manifest_digest = built_source_manifest_digest,
    runtime_package_tree_digest = runtime_package_tree_digest,
    build_stdout_sha256 = build_stdout_sha256,
    build_stderr_sha256 = build_stderr_sha256,
    install_stdout_sha256 = install_stdout_sha256,
    install_stderr_sha256 = install_stderr_sha256,
    build_command_receipt_sha256 = build_command_receipt_sha256,
    install_command_receipt_sha256 = install_command_receipt_sha256,
    runtime_lineage_marker_sha256 = runtime_lineage_marker_sha256,
    R_version = R_version,
    platform = platform
  )
  digest::digest(
    paste(names(fields), fields, sep = "=", collapse = "\n"),
    algo = "sha256", serialize = FALSE
  )
}
