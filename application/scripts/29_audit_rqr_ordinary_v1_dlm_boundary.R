#!/usr/bin/env Rscript

# Read-only protected-DLM boundary audit for ordinary RQR version 1.
#
# The audit compares Git objects without checking out either revision.  A
# development-only WORKTREE candidate is supported, but its evidence is
# explicitly nonpromotion.  Promotion evidence requires two full commit SHAs,
# a clean repository, the candidate at HEAD, and the baseline as an ancestor.

`%||%` <- function(x, y) if (is.null(x)) y else x

rqr_dlm_boundary_schema <- function() {
  "rqrgibbs_ordinary_v1_dlm_boundary/1.0.0"
}

rqr_dlm_boundary_public_functions <- function() {
  c(
    "rqr_as_dlm_model", "rqr_polytrend", "rqr_seasonal",
    "rqr_regression", "+.rqr_dlm_model", "rqr_discount_matrix",
    "rqr_freeze_discount_template", "rqr_evolution_fixed",
    "rqr_evolution_adaptive_working", "rqr_evolution_component_scale",
    "rqr_ffbs_smooth", "rqr_ffbs_sample", "rqr_dlm_fit",
    "rqr_dlm_continue", "rqr_posterior_draws.rqr_dlm_mcmc",
    "predict_interval.rqr_dlm_mcmc", "rqr_forecast_roots",
    "print.rqr_dlm_mcmc"
  )
}

rqr_dlm_boundary_find_repo <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "application", "DESCRIPTION")) &&
        dir.exists(file.path(path, ".git"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Cannot locate the RQR-GIBBS Git repository.", call. = FALSE)
    }
    path <- parent
  }
}

rqr_dlm_boundary_git <- function(
    repo_root, arguments, stdout = TRUE, allow_failure = FALSE) {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("git is required.", call. = FALSE)
  redirected <- is.character(stdout) && length(stdout) == 1L &&
    !identical(stdout, TRUE)
  stderr_target <- if (redirected) tempfile("rqr-dlm-boundary-git-stderr-") else TRUE
  if (redirected) {
    on.exit(unlink(stderr_target, force = TRUE), add = TRUE)
  }
  output <- suppressWarnings(system2(
    git,
    c(
      "-C", shQuote(repo_root),
      vapply(as.character(arguments), shQuote, character(1L))
    ),
    stdout = stdout,
    stderr = stderr_target,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  status <- as.integer(status)
  if (!identical(status, 0L) && !isTRUE(allow_failure)) {
    detail <- if (redirected && file.exists(stderr_target)) {
      paste(readLines(stderr_target, warn = FALSE), collapse = "\n")
    } else if (is.character(output)) {
      paste(output, collapse = "\n")
    } else {
      ""
    }
    stop(
      paste("Read-only Git command failed.", detail, sep = "\n"),
      call. = FALSE
    )
  }
  list(status = status, output = output)
}

rqr_dlm_boundary_git_text <- function(repo_root, arguments) {
  result <- rqr_dlm_boundary_git(repo_root, arguments)
  trimws(paste(result$output, collapse = "\n"))
}

rqr_dlm_boundary_resolve_commit <- function(repo_root, revision) {
  revision <- as.character(revision)
  if (length(revision) != 1L || is.na(revision) || !nzchar(revision)) {
    stop("A nonempty Git revision is required.", call. = FALSE)
  }
  value <- tolower(rqr_dlm_boundary_git_text(
    repo_root, c("rev-parse", "--verify", paste0(revision, "^{commit}"))
  ))
  if (!grepl("^[0-9a-f]{40}$", value)) {
    stop("Git did not resolve a complete commit SHA.", call. = FALSE)
  }
  value
}

rqr_dlm_boundary_sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest package is required.", call. = FALSE)
  }
  if (!file.exists(path) || dir.exists(path)) {
    stop("Cannot hash an absent or nonregular file.", call. = FALSE)
  }
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

rqr_dlm_boundary_sha256_object <- function(object) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest package is required.", call. = FALSE)
  }
  digest::digest(object, algo = "sha256", serialize = TRUE)
}

rqr_dlm_boundary_is_symlink <- function(path) {
  link <- Sys.readlink(path)
  is.character(link) && length(link) == 1L && !is.na(link) && nzchar(link)
}

rqr_dlm_boundary_regular_file <- function(path) {
  info <- file.info(path)
  file.exists(path) && !dir.exists(path) && !is.na(info$isdir[[1L]]) &&
    !info$isdir[[1L]] && !rqr_dlm_boundary_is_symlink(path)
}

rqr_dlm_boundary_atomic_file <- function(path, writer, validator) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path) || rqr_dlm_boundary_is_symlink(path)) {
    if (!rqr_dlm_boundary_regular_file(path)) {
      stop("Refusing to replace a symlink or nonregular artifact.",
           call. = FALSE)
    }
  }
  temporary <- tempfile(paste0(".", basename(path), "-"), tmpdir = directory)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writer(temporary)
  if (!rqr_dlm_boundary_regular_file(temporary) ||
      !isTRUE(validator(temporary))) {
    stop("Atomic boundary-audit artifact validation failed.", call. = FALSE)
  }
  if ((file.exists(path) || rqr_dlm_boundary_is_symlink(path)) &&
      !rqr_dlm_boundary_regular_file(path)) {
    stop("The destination changed to a nonregular object before publication.",
         call. = FALSE)
  }
  if (!file.rename(temporary, path)) {
    stop("Cannot publish the boundary-audit artifact atomically.",
         call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

rqr_dlm_boundary_atomic_csv <- function(data, path) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!"schema_version" %in% names(data)) {
    data <- cbind(
      schema_version = rep(rqr_dlm_boundary_schema(), nrow(data)),
      data,
      stringsAsFactors = FALSE
    )
  }
  rqr_dlm_boundary_atomic_file(
    path,
    function(temporary) {
      utils::write.csv(data, temporary, row.names = FALSE, na = "")
    },
    function(temporary) {
      restored <- utils::read.csv(
        temporary, stringsAsFactors = FALSE, check.names = FALSE,
        na.strings = "NA"
      )
      nrow(restored) == nrow(data) && identical(names(restored), names(data))
    }
  )
}

rqr_dlm_boundary_source_runner <- function(repo_root) {
  path <- file.path(
    repo_root, "application", "scripts",
    "25_validate_rqr_ordinary_v1.R"
  )
  old <- Sys.getenv("RQR_ORDINARY_V1_SOURCE_ONLY", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("RQR_ORDINARY_V1_SOURCE_ONLY")
    } else {
      Sys.setenv(RQR_ORDINARY_V1_SOURCE_ONLY = old)
    }
  }, add = TRUE)
  Sys.setenv(RQR_ORDINARY_V1_SOURCE_ONLY = "YES")
  environment <- new.env(parent = globalenv())
  sys.source(path, envir = environment)
  paths <- environment$rqr_ordinary_v1_protected_dlm_paths()
  if (!is.character(paths) || length(paths) != 23L ||
      anyNA(paths) || any(!nzchar(paths)) || anyDuplicated(paths)) {
    stop("The canonical protected-DLM inventory is not the reviewed 23 paths.",
         call. = FALSE)
  }
  paths
}

rqr_dlm_boundary_blob_exists <- function(repo_root, revision, relative_path) {
  if (identical(revision, "WORKTREE")) {
    return(rqr_dlm_boundary_regular_file(file.path(repo_root, relative_path)))
  }
  result <- rqr_dlm_boundary_git(
    repo_root,
    c("cat-file", "-e", paste0(revision, ":", relative_path)),
    allow_failure = TRUE
  )
  identical(result$status, 0L)
}

rqr_dlm_boundary_blob_file <- function(
    repo_root, revision, relative_path, temporary_directory) {
  if (identical(revision, "WORKTREE")) {
    path <- file.path(repo_root, relative_path)
    if (!rqr_dlm_boundary_regular_file(path)) return(NA_character_)
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }
  if (!rqr_dlm_boundary_blob_exists(repo_root, revision, relative_path)) {
    return(NA_character_)
  }
  target <- tempfile("rqr-dlm-boundary-blob-", tmpdir = temporary_directory)
  result <- rqr_dlm_boundary_git(
    repo_root,
    c("cat-file", "blob", paste0(revision, ":", relative_path)),
    stdout = target
  )
  if (!identical(result$status, 0L) ||
      !rqr_dlm_boundary_regular_file(target)) {
    stop("Failed to materialize a Git blob.", call. = FALSE)
  }
  target
}

rqr_dlm_boundary_read_lines <- function(
    repo_root, revision, relative_path, temporary_directory) {
  path <- rqr_dlm_boundary_blob_file(
    repo_root, revision, relative_path, temporary_directory
  )
  if (is.na(path)) return(NULL)
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

rqr_dlm_boundary_file_table <- function(
    repo_root, baseline, candidate, protected_paths, temporary_directory) {
  rows <- lapply(protected_paths, function(relative_path) {
    baseline_path <- rqr_dlm_boundary_blob_file(
      repo_root, baseline, relative_path, temporary_directory
    )
    candidate_path <- rqr_dlm_boundary_blob_file(
      repo_root, candidate, relative_path, temporary_directory
    )
    baseline_exists <- !is.na(baseline_path)
    candidate_exists <- !is.na(candidate_path)
    baseline_hash <- if (baseline_exists) {
      rqr_dlm_boundary_sha256_file(baseline_path)
    } else {
      NA_character_
    }
    candidate_hash <- if (candidate_exists) {
      rqr_dlm_boundary_sha256_file(candidate_path)
    } else {
      NA_character_
    }
    status <- if (!baseline_exists && !candidate_exists) {
      "missing_both"
    } else if (!baseline_exists) {
      "added"
    } else if (!candidate_exists) {
      "removed"
    } else if (identical(baseline_hash, candidate_hash)) {
      "unchanged"
    } else {
      "changed"
    }
    data.frame(
      relative_path = relative_path,
      baseline_exists = baseline_exists,
      candidate_exists = candidate_exists,
      baseline_bytes = if (baseline_exists) {
        as.numeric(file.info(baseline_path)$size)
      } else {
        NA_real_
      },
      candidate_bytes = if (candidate_exists) {
        as.numeric(file.info(candidate_path)$size)
      } else {
        NA_real_
      },
      baseline_sha256 = baseline_hash,
      candidate_sha256 = candidate_hash,
      status = status,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

rqr_dlm_boundary_r_paths <- function(repo_root, revision) {
  if (identical(revision, "WORKTREE")) {
    paths <- list.files(
      file.path(repo_root, "application", "R"),
      pattern = "[.]R$", full.names = FALSE
    )
    return(sort(file.path("application", "R", paths)))
  }
  text <- rqr_dlm_boundary_git_text(
    repo_root,
    c("ls-tree", "-r", "--name-only", revision, "--", "application/R")
  )
  if (!nzchar(text)) return(character(0))
  paths <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  sort(paths[grepl("[.]R$", paths)])
}

rqr_dlm_boundary_function_ast <- function(
    repo_root, revision, temporary_directory) {
  found <- list()
  source_path <- character(0)
  for (relative_path in rqr_dlm_boundary_r_paths(repo_root, revision)) {
    lines <- rqr_dlm_boundary_read_lines(
      repo_root, revision, relative_path, temporary_directory
    )
    if (is.null(lines)) next
    expressions <- parse(
      text = paste(lines, collapse = "\n"), keep.source = FALSE
    )
    for (expression in expressions) {
      if (!is.call(expression) || length(expression) != 3L ||
          !as.character(expression[[1L]]) %in% c("<-", "=") ||
          !is.symbol(expression[[2L]]) ||
          !is.call(expression[[3L]]) ||
          !identical(expression[[3L]][[1L]], as.name("function"))) {
        next
      }
      name <- as.character(expression[[2L]])
      if (!is.null(found[[name]])) {
        stop(sprintf("Duplicate top-level function definition: %s", name),
             call. = FALSE)
      }
      found[[name]] <- expression[[3L]]
      source_path[[name]] <- relative_path
    }
  }
  list(functions = found, source_path = source_path)
}

rqr_dlm_boundary_public_roles <- function(lines) {
  roles <- character(0)
  if (is.null(lines)) return(roles)
  lines <- gsub("[[:space:]]+", "", trimws(sub("#.*$", "", lines)))
  exports <- lines[grepl("^export[(].+[)]$", lines)]
  for (line in exports) {
    name <- sub("^export[(](.+)[)]$", "\\1", line)
    roles[[name]] <- "export"
  }
  methods <- lines[grepl("^S3method[(].+,.+[)]$", lines)]
  for (line in methods) {
    fields <- strsplit(
      sub("^S3method[(](.+)[)]$", "\\1", line), ",", fixed = TRUE
    )[[1L]]
    if (length(fields) != 2L) next
    fields <- gsub('^["\\047]|["\\047]$', "", fields)
    name <- paste0(fields[[1L]], ".", fields[[2L]])
    roles[[name]] <- paste0(
      "S3method(", fields[[1L]], ",", fields[[2L]], ")"
    )
  }
  roles
}

rqr_dlm_boundary_function_table <- function(
    repo_root, baseline, candidate, temporary_directory,
    public_functions = rqr_dlm_boundary_public_functions()) {
  baseline_ast <- rqr_dlm_boundary_function_ast(
    repo_root, baseline, temporary_directory
  )
  candidate_ast <- rqr_dlm_boundary_function_ast(
    repo_root, candidate, temporary_directory
  )
  baseline_roles <- rqr_dlm_boundary_public_roles(
    rqr_dlm_boundary_read_lines(
      repo_root, baseline, "application/NAMESPACE", temporary_directory
    )
  )
  candidate_roles <- rqr_dlm_boundary_public_roles(
    rqr_dlm_boundary_read_lines(
      repo_root, candidate, "application/NAMESPACE", temporary_directory
    )
  )
  rows <- lapply(public_functions, function(name) {
    left <- baseline_ast$functions[[name]]
    right <- candidate_ast$functions[[name]]
    left_exists <- !is.null(left)
    right_exists <- !is.null(right)
    digest_part <- function(value, part) {
      if (is.null(value)) return(NA_character_)
      rqr_dlm_boundary_sha256_object(value[[part]])
    }
    left_formals <- digest_part(left, 2L)
    right_formals <- digest_part(right, 2L)
    left_body <- digest_part(left, 3L)
    right_body <- digest_part(right, 3L)
    status <- if (!left_exists && !right_exists) {
      "missing_both"
    } else if (!left_exists) {
      "added"
    } else if (!right_exists) {
      "removed"
    } else if (identical(left_formals, right_formals) &&
               identical(left_body, right_body)) {
      "unchanged"
    } else if (!identical(left_formals, right_formals) &&
               !identical(left_body, right_body)) {
      "formals_and_body_changed"
    } else if (!identical(left_formals, right_formals)) {
      "formals_changed"
    } else {
      "body_changed"
    }
    data.frame(
      function_name = name,
      baseline_source_path =
        unname(baseline_ast$source_path[[name]] %||% NA_character_),
      candidate_source_path =
        unname(candidate_ast$source_path[[name]] %||% NA_character_),
      baseline_namespace_role =
        unname(baseline_roles[[name]] %||% NA_character_),
      candidate_namespace_role =
        unname(candidate_roles[[name]] %||% NA_character_),
      baseline_exists = left_exists,
      candidate_exists = right_exists,
      baseline_formals_sha256 = left_formals,
      candidate_formals_sha256 = right_formals,
      baseline_body_sha256 = left_body,
      candidate_body_sha256 = right_body,
      status = status,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

rqr_dlm_boundary_schema_occurrences <- function(
    repo_root, revision, protected_paths, temporary_directory) {
  pattern <- "rqrgibbs_[[:alnum:]_.-]+/[0-9]+(?:[.][0-9]+)*"
  rows <- list()
  for (relative_path in protected_paths) {
    lines <- rqr_dlm_boundary_read_lines(
      repo_root, revision, relative_path, temporary_directory
    )
    if (is.null(lines)) next
    matches <- regmatches(
      paste(lines, collapse = "\n"),
      gregexpr(pattern, paste(lines, collapse = "\n"), perl = TRUE)
    )[[1L]]
    if (!length(matches) || identical(matches, character(0))) next
    counts <- table(matches)
    rows[[length(rows) + 1L]] <- data.frame(
      relative_path = relative_path,
      schema_string = names(counts),
      occurrences = as.integer(counts),
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(data.frame(
      relative_path = character(0), schema_string = character(0),
      occurrences = integer(0), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

rqr_dlm_boundary_pair_counts <- function(
    baseline, candidate, key_columns, value_column = "occurrences") {
  key <- function(data) {
    if (!nrow(data)) return(character(0))
    do.call(paste, c(data[key_columns], sep = "\034"))
  }
  left_key <- key(baseline)
  right_key <- key(candidate)
  keys <- sort(unique(c(left_key, right_key)))
  if (!length(keys)) {
    out <- as.data.frame(
      setNames(
        replicate(length(key_columns), character(0), simplify = FALSE),
        key_columns
      ),
      stringsAsFactors = FALSE
    )
    out$baseline_occurrences <- integer(0)
    out$candidate_occurrences <- integer(0)
    out$status <- character(0)
    return(out)
  }
  parts <- strsplit(keys, "\034", fixed = TRUE)
  out <- as.data.frame(
    do.call(rbind, parts), stringsAsFactors = FALSE
  )
  names(out) <- key_columns
  left <- match(keys, left_key)
  right <- match(keys, right_key)
  out$baseline_occurrences <- ifelse(
    is.na(left), 0L, baseline[[value_column]][left]
  )
  out$candidate_occurrences <- ifelse(
    is.na(right), 0L, candidate[[value_column]][right]
  )
  out$status <- ifelse(
    out$baseline_occurrences == out$candidate_occurrences,
    "unchanged", "changed"
  )
  out
}

rqr_dlm_boundary_namespace_entries <- function(lines) {
  if (is.null(lines)) {
    return(data.frame(
      directive_type = character(0), directive = character(0),
      occurrences = integer(0), stringsAsFactors = FALSE
    ))
  }
  lines <- trimws(sub("#.*$", "", lines))
  lines <- lines[nzchar(lines)]
  lines <- gsub("[[:space:]]+", "", lines)
  keep <- grepl("^[[:alpha:]][[:alnum:]_]*(?:[.]?[[:alnum:]_]*)*[(].*[)]$", lines)
  lines <- lines[keep]
  if (!length(lines)) {
    return(data.frame(
      directive_type = character(0), directive = character(0),
      occurrences = integer(0), stringsAsFactors = FALSE
    ))
  }
  counts <- table(lines)
  data.frame(
    directive_type = sub("[(].*$", "", names(counts)),
    directive = names(counts),
    occurrences = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

rqr_dlm_boundary_namespace_table <- function(
    repo_root, baseline, candidate, temporary_directory) {
  read <- function(revision) {
    rqr_dlm_boundary_namespace_entries(rqr_dlm_boundary_read_lines(
      repo_root, revision, "application/NAMESPACE", temporary_directory
    ))
  }
  rqr_dlm_boundary_pair_counts(
    read(baseline), read(candidate),
    c("directive_type", "directive")
  )
}

rqr_dlm_boundary_r_call_entries <- function(lines) {
  if (is.null(lines)) return(list())
  expressions <- parse(
    text = paste(lines, collapse = "\n"), keep.source = FALSE
  )
  found <- list()
  visit <- function(node) {
    if (!is.call(node)) return(invisible(NULL))
    if (identical(node[[1L]], as.name(".Call")) && length(node) >= 2L) {
      symbol <- as.character(node[[2L]])
      found[[length(found) + 1L]] <<- data.frame(
        registration_kind = "R_wrapper_Call",
        symbol = symbol,
        arity = as.character(length(node) - 2L),
        occurrences = 1L,
        stringsAsFactors = FALSE
      )
    }
    for (index in seq_along(node)[-1L]) visit(node[[index]])
    invisible(NULL)
  }
  for (expression in expressions) visit(expression)
  found
}

rqr_dlm_boundary_registration_entries <- function(cpp_lines, r_lines) {
  empty <- data.frame(
    registration_kind = character(0), symbol = character(0),
    arity = character(0), occurrences = integer(0),
    stringsAsFactors = FALSE
  )
  rows <- rqr_dlm_boundary_r_call_entries(r_lines)
  if (is.null(cpp_lines)) {
    if (!length(rows)) return(empty)
    return(do.call(rbind, rows))
  }
  text <- paste(cpp_lines, collapse = "\n")
  call_pattern <- paste0(
    '[{]"([^"]+)",[[:space:]]*[(]DL_FUNC[)][[:space:]]*&[^,]+,',
    "[[:space:]]*([0-9]+)[}]"
  )
  locations <- gregexpr(call_pattern, text, perl = TRUE)[[1L]]
  values <- regmatches(text, list(locations))[[1L]]
  if (length(values) && !identical(values, character(0))) {
    rows[[length(rows) + 1L]] <- data.frame(
      registration_kind = "CallEntries",
      symbol = sub(call_pattern, "\\1", values, perl = TRUE),
      arity = sub(call_pattern, "\\2", values, perl = TRUE),
      occurrences = 1L,
      stringsAsFactors = FALSE
    )
  }
  special <- c(
    R_init_rqrgibbs = grepl(
      "R_init_rqrgibbs[[:space:]]*[(]", text, perl = TRUE
    ),
    dynamic_symbols_disabled = grepl(
      "R_useDynamicSymbols[[:space:]]*[(][^,]+,[[:space:]]*FALSE[[:space:]]*[)]",
      text, perl = TRUE
    )
  )
  rows[[length(rows) + 1L]] <- data.frame(
    registration_kind = "registration_policy",
    symbol = names(special)[special],
    arity = NA_character_,
    occurrences = 1L,
    stringsAsFactors = FALSE
  )
  do.call(rbind, rows)
}

rqr_dlm_boundary_registration_table <- function(
    repo_root, baseline, candidate, temporary_directory) {
  read <- function(revision) {
    rqr_dlm_boundary_registration_entries(
      rqr_dlm_boundary_read_lines(
        repo_root, revision, "application/src/RcppExports.cpp",
        temporary_directory
      ),
      rqr_dlm_boundary_read_lines(
        repo_root, revision, "application/R/RcppExports.R",
        temporary_directory
      )
    )
  }
  rqr_dlm_boundary_pair_counts(
    read(baseline), read(candidate),
    c("registration_kind", "symbol", "arity")
  )
}

rqr_dlm_boundary_description <- function(
    repo_root, revision, temporary_directory) {
  path <- rqr_dlm_boundary_blob_file(
    repo_root, revision, "application/DESCRIPTION", temporary_directory
  )
  if (is.na(path)) {
    return(data.frame(
      field = character(0), value = character(0),
      stringsAsFactors = FALSE
    ))
  }
  value <- read.dcf(path, all = TRUE)
  data.frame(
    field = colnames(value),
    value = gsub("[[:space:]]+", " ", as.character(value[1L, ])),
    stringsAsFactors = FALSE
  )
}

rqr_dlm_boundary_description_table <- function(
    repo_root, baseline, candidate, temporary_directory) {
  left <- rqr_dlm_boundary_description(
    repo_root, baseline, temporary_directory
  )
  right <- rqr_dlm_boundary_description(
    repo_root, candidate, temporary_directory
  )
  fields <- sort(unique(c(left$field, right$field)))
  left_index <- match(fields, left$field)
  right_index <- match(fields, right$field)
  left_value <- ifelse(is.na(left_index), NA_character_, left$value[left_index])
  right_value <- ifelse(
    is.na(right_index), NA_character_, right$value[right_index]
  )
  data.frame(
    field = fields,
    baseline_value = left_value,
    candidate_value = right_value,
    status = ifelse(
      is.na(left_value) & !is.na(right_value), "added",
      ifelse(
        !is.na(left_value) & is.na(right_value), "removed",
        ifelse(left_value == right_value, "unchanged", "changed")
      )
    ),
    stringsAsFactors = FALSE
  )
}

rqr_dlm_boundary_auth_state <- function(
    repo_root, revision, temporary_directory) {
  relative_path <- paste0(
    "application/config/rqr_dlm/",
    "rqr_dlm_main_simulation_20260724.R"
  )
  lines <- rqr_dlm_boundary_read_lines(
    repo_root, revision, relative_path, temporary_directory
  )
  if (is.null(lines)) {
    return(data.frame(
      field = character(0), value = character(0),
      stringsAsFactors = FALSE
    ))
  }
  expressions <- parse(
    text = paste(lines, collapse = "\n"), keep.source = FALSE
  )
  assignments <- Filter(function(expression) {
    is.call(expression) && length(expression) == 3L &&
      as.character(expression[[1L]]) %in% c("<-", "=") &&
      is.symbol(expression[[2L]]) &&
      identical(
        as.character(expression[[2L]]), "rqr_dlm_main_simulation"
      )
  }, as.list(expressions))
  if (length(assignments) != 1L ||
      !is.call(assignments[[1L]][[3L]]) ||
      !identical(assignments[[1L]][[3L]][[1L]], as.name("list"))) {
    stop("The confirmatory DLM config is not one literal top-level list.",
         call. = FALSE)
  }
  config <- assignments[[1L]][[3L]]
  named <- function(call, name) {
    if (!is.call(call) || !identical(call[[1L]], as.name("list"))) {
      stop("A required confirmatory-config section is not a literal list.",
           call. = FALSE)
    }
    arguments <- as.list(call)[-1L]
    tags <- names(arguments)
    index <- which(!is.na(tags) & tags == name)
    if (length(index) != 1L) {
      stop(sprintf("Missing or duplicate confirmatory field: %s", name),
           call. = FALSE)
    }
    arguments[[index]]
  }
  scalar <- function(value, name) {
    if ((is.character(value) || is.logical(value) || is.numeric(value)) &&
        length(value) == 1L && !is.na(value)) {
      return(as.character(value))
    }
    stop(sprintf("Confirmatory field %s is not a literal scalar.", name),
         call. = FALSE)
  }
  correction <- named(config, "implementation_correction")
  review <- named(config, "review_contract")
  authorization <- named(config, "authorization_contract")
  fields <- list(
    schema_version = scalar(
      named(config, "schema_version"), "schema_version"
    ),
    config_id = scalar(named(config, "config_id"), "config_id"),
    status = scalar(named(config, "status"), "status"),
    diagnostic_pilot_execution_authorized =
      scalar(
        named(config, "diagnostic_pilot_execution_authorized"),
        "diagnostic_pilot_execution_authorized"
      ),
    confirmatory_execution_authorized =
      scalar(
        named(config, "confirmatory_execution_authorized"),
        "confirmatory_execution_authorized"
      ),
    implementation_target_prior_seed_or_threshold_changed =
      scalar(
        named(
          correction,
          "target_prior_seed_or_diagnostic_threshold_changed"
        ),
        "target_prior_seed_or_diagnostic_threshold_changed"
      ),
    implementation_transition_or_schedule_changed =
      scalar(
        named(
          correction, "mcmc_transition_and_standard_schedule_changed"
        ),
        "mcmc_transition_and_standard_schedule_changed"
      ),
    review_branch_tip = scalar(
      named(review, "review_branch_tip"), "review_branch_tip"
    ),
    authorization_schema = scalar(
      named(authorization, "schema_version"),
      "authorization_contract.schema_version"
    ),
    reviewed_implementation_commit_required =
      scalar(
        named(
          authorization, "reviewed_implementation_commit_required"
        ),
        "reviewed_implementation_commit_required"
      ),
    authorization_commit_must_equal_runtime_commit =
      scalar(
        named(
          authorization,
          "authorization_commit_must_equal_runtime_commit"
        ),
        "authorization_commit_must_equal_runtime_commit"
      ),
    authorization_diff_must_only_flip_confirmatory_flag =
      scalar(
        named(
          authorization,
          "authorization_diff_must_only_flip_confirmatory_flag"
        ),
        "authorization_diff_must_only_flip_confirmatory_flag"
      ),
    explicit_user_confirmation_required =
      scalar(
        named(authorization, "explicit_user_confirmation_required"),
        "explicit_user_confirmation_required"
      ),
    primary_clean_worktree_required =
      scalar(
        named(authorization, "primary_clean_worktree_required"),
        "primary_clean_worktree_required"
      ),
    protected_checkout_used =
      scalar(
        named(authorization, "protected_checkout_used"),
        "protected_checkout_used"
      )
  )
  data.frame(
    field = names(fields),
    value = vapply(fields, function(value) {
      paste(as.character(value %||% NA_character_), collapse = "|")
    }, character(1L)),
    stringsAsFactors = FALSE
  )
}

rqr_dlm_boundary_auth_table <- function(
    repo_root, baseline, candidate, temporary_directory) {
  left <- rqr_dlm_boundary_auth_state(
    repo_root, baseline, temporary_directory
  )
  right <- rqr_dlm_boundary_auth_state(
    repo_root, candidate, temporary_directory
  )
  fields <- sort(unique(c(left$field, right$field)))
  left_value <- left$value[match(fields, left$field)]
  right_value <- right$value[match(fields, right$field)]
  data.frame(
    field = fields,
    baseline_value = left_value,
    candidate_value = right_value,
    status = ifelse(left_value == right_value, "unchanged", "changed"),
    stringsAsFactors = FALSE
  )
}

rqr_dlm_boundary_output_is_local <- function(repo_root, output_dir) {
  absolute <- normalizePath(
    output_dir, winslash = "/", mustWork = FALSE
  )
  root <- paste0(
    normalizePath(repo_root, winslash = "/", mustWork = TRUE), "/"
  )
  if (!startsWith(paste0(absolute, "/"), root)) return(TRUE)
  relative <- substring(absolute, nchar(root) + 1L)
  result <- rqr_dlm_boundary_git(
    repo_root, c("check-ignore", "-q", "--", relative),
    allow_failure = TRUE
  )
  identical(result$status, 0L)
}

rqr_dlm_boundary_validate_promotion <- function(
    repo_root, baseline_input, baseline, candidate_input, candidate,
    promotion) {
  status <- rqr_dlm_boundary_git_text(
    repo_root, c("status", "--porcelain=v1", "--untracked-files=all")
  )
  clean <- !nzchar(status)
  head <- rqr_dlm_boundary_resolve_commit(repo_root, "HEAD")
  merge_base <- rqr_dlm_boundary_git_text(
    repo_root, c("merge-base", baseline, candidate)
  )
  ancestor <- identical(tolower(merge_base), baseline)
  strict_ancestor <- ancestor && !identical(baseline, candidate)
  full_inputs <- grepl("^[0-9a-f]{40}$", baseline_input) &&
    grepl("^[0-9a-f]{40}$", candidate_input)
  if (isTRUE(promotion) &&
      (!full_inputs || identical(candidate_input, "WORKTREE") ||
       !clean || !identical(head, candidate) || !strict_ancestor)) {
    stop(
      paste(
        "Promotion mode requires full baseline/candidate SHAs, a clean",
        "repository at candidate HEAD, and a strict ancestral baseline."
      ),
      call. = FALSE
    )
  }
  list(
    source_clean = clean,
    source_head = head,
    baseline_is_ancestor = ancestor,
    baseline_is_strict_ancestor = strict_ancestor,
    full_sha_inputs = full_inputs
  )
}

rqr_dlm_boundary_manifest <- function(output_dir) {
  paths <- list.files(
    output_dir, full.names = TRUE, recursive = FALSE, all.files = FALSE
  )
  paths <- paths[
    vapply(paths, rqr_dlm_boundary_regular_file, logical(1L)) &
      basename(paths) != "artifact_hashes.csv"
  ]
  paths <- paths[order(basename(paths))]
  data.frame(
    relative_path = basename(paths),
    byte_count = as.numeric(file.info(paths)$size),
    sha256 = unname(vapply(
      paths, rqr_dlm_boundary_sha256_file, character(1L)
    )),
    stringsAsFactors = FALSE
  )
}

rqr_dlm_boundary_audit <- function(
    repo_root, baseline, candidate, output_dir, promotion = FALSE) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  if (!rqr_dlm_boundary_output_is_local(repo_root, output_dir)) {
    stop("Output must be outside Git or under an ignored repository path.",
         call. = FALSE)
  }
  baseline_input <- tolower(as.character(baseline))
  candidate_input <- as.character(candidate)
  baseline_sha <- rqr_dlm_boundary_resolve_commit(repo_root, baseline_input)
  candidate_kind <- if (identical(candidate_input, "WORKTREE")) {
    "working_tree_nonpromotion"
  } else {
    "committed_revision"
  }
  if (identical(candidate_input, "WORKTREE")) {
    if (isTRUE(promotion)) {
      stop("WORKTREE evidence is never promotion evidence.", call. = FALSE)
    }
    candidate_revision <- "WORKTREE"
    candidate_sha <- rqr_dlm_boundary_resolve_commit(repo_root, "HEAD")
    promotion_state <- list(
      source_clean = !nzchar(rqr_dlm_boundary_git_text(
        repo_root, c("status", "--porcelain=v1", "--untracked-files=all")
      )),
      source_head = candidate_sha,
      baseline_is_ancestor = identical(
        tolower(rqr_dlm_boundary_git_text(
          repo_root, c("merge-base", baseline_sha, candidate_sha)
        )),
        baseline_sha
      ),
      baseline_is_strict_ancestor = !identical(
        baseline_sha, candidate_sha
      ) && identical(
        tolower(rqr_dlm_boundary_git_text(
          repo_root, c("merge-base", baseline_sha, candidate_sha)
        )),
        baseline_sha
      ),
      full_sha_inputs = grepl("^[0-9a-f]{40}$", baseline_input)
    )
  } else {
    candidate_input <- tolower(candidate_input)
    candidate_sha <- rqr_dlm_boundary_resolve_commit(
      repo_root, candidate_input
    )
    candidate_revision <- candidate_sha
    promotion_state <- rqr_dlm_boundary_validate_promotion(
      repo_root, baseline_input, baseline_sha, candidate_input,
      candidate_sha, promotion
    )
  }
  protected_paths <- rqr_dlm_boundary_source_runner(repo_root)
  temporary_directory <- tempfile("rqr-dlm-boundary-audit-")
  dir.create(temporary_directory, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temporary_directory, recursive = TRUE, force = TRUE),
          add = TRUE)

  files <- rqr_dlm_boundary_file_table(
    repo_root, baseline_sha, candidate_revision, protected_paths,
    temporary_directory
  )
  functions <- rqr_dlm_boundary_function_table(
    repo_root, baseline_sha, candidate_revision, temporary_directory
  )
  schemas <- rqr_dlm_boundary_pair_counts(
    rqr_dlm_boundary_schema_occurrences(
      repo_root, baseline_sha, protected_paths, temporary_directory
    ),
    rqr_dlm_boundary_schema_occurrences(
      repo_root, candidate_revision, protected_paths, temporary_directory
    ),
    c("relative_path", "schema_string")
  )
  namespace <- rqr_dlm_boundary_namespace_table(
    repo_root, baseline_sha, candidate_revision, temporary_directory
  )
  registrations <- rqr_dlm_boundary_registration_table(
    repo_root, baseline_sha, candidate_revision, temporary_directory
  )
  package <- rqr_dlm_boundary_description_table(
    repo_root, baseline_sha, candidate_revision, temporary_directory
  )
  authorization <- rqr_dlm_boundary_auth_table(
    repo_root, baseline_sha, candidate_revision, temporary_directory
  )
  candidate_complete <- all(files$candidate_exists)
  public_complete <- all(functions$candidate_exists)
  evidence_role <- if (isTRUE(promotion)) {
    "promotion_boundary_evidence"
  } else {
    "development_nonpromotion_evidence"
  }
  metadata <- data.frame(
    field = c(
      "evidence_role", "promotion_mode", "baseline_input",
      "baseline_commit", "candidate_input", "candidate_commit",
      "candidate_kind", "source_head", "source_clean",
      "baseline_is_ancestor", "baseline_is_strict_ancestor",
      "full_sha_inputs",
      "protected_path_count", "candidate_protected_inventory_complete",
      "public_function_count", "candidate_public_function_inventory_complete",
      "statistical_validation_performed", "external_repository_mutation",
      "audit_status"
    ),
    value = c(
      evidence_role, as.character(isTRUE(promotion)), baseline_input,
      baseline_sha, candidate_input, candidate_sha, candidate_kind,
      promotion_state$source_head,
      as.character(promotion_state$source_clean),
      as.character(promotion_state$baseline_is_ancestor),
      as.character(promotion_state$baseline_is_strict_ancestor),
      as.character(promotion_state$full_sha_inputs),
      as.character(length(protected_paths)), as.character(candidate_complete),
      as.character(length(rqr_dlm_boundary_public_functions())),
      as.character(public_complete), "FALSE", "FALSE",
      if (candidate_complete && public_complete) {
        "compact_boundary_evidence_generated"
      } else {
        "incomplete_candidate_boundary"
      }
    ),
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    comparison = c(
      "protected_files", "public_functions", "schema_occurrences",
      "namespace_directives", "compiled_registrations",
      "package_metadata", "confirmatory_authorization"
    ),
    total = c(
      nrow(files), nrow(functions), nrow(schemas), nrow(namespace),
      nrow(registrations), nrow(package), nrow(authorization)
    ),
    changed = c(
      sum(files$status != "unchanged"),
      sum(functions$status != "unchanged"),
      sum(schemas$status != "unchanged"),
      sum(namespace$status != "unchanged"),
      sum(registrations$status != "unchanged"),
      sum(package$status != "unchanged"),
      sum(authorization$status != "unchanged")
    ),
    stringsAsFactors = FALSE
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- list(
    audit_metadata.csv = metadata,
    boundary_summary.csv = summary,
    protected_file_comparison.csv = files,
    public_function_comparison.csv = functions,
    schema_string_comparison.csv = schemas,
    namespace_registration_comparison.csv = namespace,
    compiled_registration_comparison.csv = registrations,
    package_metadata_comparison.csv = package,
    confirmatory_authorization_comparison.csv = authorization
  )
  for (name in names(tables)) {
    rqr_dlm_boundary_atomic_csv(
      tables[[name]], file.path(output_dir, name)
    )
  }
  manifest <- rqr_dlm_boundary_manifest(output_dir)
  rqr_dlm_boundary_atomic_csv(
    manifest, file.path(output_dir, "artifact_hashes.csv")
  )
  invisible(list(
    metadata = metadata, summary = summary, tables = tables,
    manifest = manifest
  ))
}

rqr_dlm_boundary_parse_arguments <- function(arguments) {
  values <- list(
    repo_root = getwd(), baseline = NULL, candidate = NULL,
    output_dir = NULL, promotion = FALSE
  )
  index <- 1L
  while (index <= length(arguments)) {
    key <- arguments[[index]]
    if (identical(key, "--promotion")) {
      if (index == length(arguments)) {
        stop("--promotion requires YES or NO.", call. = FALSE)
      }
      choice <- toupper(arguments[[index + 1L]])
      if (!choice %in% c("YES", "NO")) {
        stop("--promotion requires YES or NO.", call. = FALSE)
      }
      values$promotion <- identical(choice, "YES")
      index <- index + 2L
      next
    }
    mapping <- c(
      "--repo-root" = "repo_root", "--baseline" = "baseline",
      "--candidate" = "candidate", "--output-dir" = "output_dir"
    )
    if (!key %in% names(mapping) || index == length(arguments)) {
      stop(sprintf("Unknown or incomplete argument: %s", key),
           call. = FALSE)
    }
    values[[unname(mapping[[key]])]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  if (is.null(values$baseline) || is.null(values$candidate) ||
      is.null(values$output_dir)) {
    stop(
      "--baseline, --candidate, and --output-dir are required.",
      call. = FALSE
    )
  }
  values
}

rqr_dlm_boundary_main <- function(
    arguments = commandArgs(trailingOnly = TRUE)) {
  values <- rqr_dlm_boundary_parse_arguments(arguments)
  repo_root <- rqr_dlm_boundary_find_repo(values$repo_root)
  result <- rqr_dlm_boundary_audit(
    repo_root = repo_root,
    baseline = values$baseline,
    candidate = values$candidate,
    output_dir = values$output_dir,
    promotion = values$promotion
  )
  message(
    "Protected-DLM boundary audit generated: ",
    normalizePath(values$output_dir, winslash = "/", mustWork = TRUE),
    " [", result$metadata$value[
      result$metadata$field == "evidence_role"
    ], "]"
  )
  invisible(result)
}

if (!identical(
    Sys.getenv("RQR_DLM_BOUNDARY_AUDIT_SOURCE_ONLY", unset = ""), "YES"
  )) {
  rqr_dlm_boundary_main()
}
