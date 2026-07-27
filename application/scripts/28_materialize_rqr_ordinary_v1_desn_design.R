#!/usr/bin/env Rscript

# Materialize the frozen ordinary-RQR v1 DESN design through the exact pinned
# exdqlm runtime.
#
# This script performs feature-design materialization only.  It never fits an
# RQR readout, runs MCMC/VB, simulates a response, or launches a simulation
# study.  Both package namespaces must come from archive-built, independently
# attested runtime libraries.  The protected exdqlm checkout is read-only and
# guarded, including ignored files, before and after materialization.

`%||%` <- function(x, y) if (is.null(x)) y else x

rqr_ordinary_v1_materializer_schema <- function() {
  "rqrgibbs_ordinary_v1_desn_materialization/1.0.0"
}

rqr_ordinary_v1_materializer_find_repo <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "application", "DESCRIPTION"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Cannot locate the RQR-GIBBS repository root.", call. = FALSE)
    }
    path <- parent
  }
}

rqr_ordinary_v1_materializer_read_git <- function(repo_root, args) {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Git is required.", call. = FALSE)
  output <- suppressWarnings(system2(
    git,
    c("-C", shQuote(repo_root), args),
    stdout = TRUE,
    stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    stop(
      "A read-only Git command failed: ",
      paste(args, collapse = " "), "\n",
      paste(output, collapse = "\n"),
      call. = FALSE
    )
  }
  trimws(paste(output, collapse = "\n"))
}

rqr_ordinary_v1_materializer_primary_snapshot <- function(repo_root) {
  fields <- c(
    branch = rqr_ordinary_v1_materializer_read_git(
      repo_root, c("rev-parse", "--abbrev-ref", "HEAD")
    ),
    commit = tolower(rqr_ordinary_v1_materializer_read_git(
      repo_root, c("rev-parse", "HEAD")
    )),
    tree = tolower(rqr_ordinary_v1_materializer_read_git(
      repo_root, c("rev-parse", "HEAD^{tree}")
    )),
    status = rqr_ordinary_v1_materializer_read_git(
      repo_root, c("status", "--porcelain=v2", "--untracked-files=all")
    ),
    refs = rqr_ordinary_v1_materializer_read_git(
      repo_root, c("show-ref", "--head", "--dereference")
    ),
    local_config = rqr_ordinary_v1_materializer_read_git(
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

rqr_ordinary_v1_materializer_load_config <- function(repo_root) {
  path <- file.path(
    repo_root, "application", "config", "rqr_ordinary_v1",
    "rqr_ordinary_v1_bounded_validation_20260726.R"
  )
  if (!file.exists(path)) {
    stop("The frozen ordinary-v1 configuration is absent.", call. = FALSE)
  }
  environment <- new.env(parent = baseenv())
  sys.source(path, envir = environment)
  config <- environment$rqr_ordinary_v1_bounded_validation
  if (!is.list(config) ||
      !identical(
        config$schema_version,
        "rqrgibbs_ordinary_v1_validation/1.0.0"
      )) {
    stop("The ordinary-v1 configuration schema is invalid.", call. = FALSE)
  }
  attr(config, "source_path") <- normalizePath(
    path, winslash = "/", mustWork = TRUE
  )
  config
}

rqr_ordinary_v1_materializer_integer <- function(x, name, minimum = 0L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x != floor(x) || x < minimum ||
      x > .Machine$integer.max) {
    stop(sprintf("%s must be a finite integer >= %d.", name, minimum),
         call. = FALSE)
  }
  as.integer(x)
}

rqr_ordinary_v1_materializer_validate_d02 <- function(config) {
  expected_schemas <- c(
    design = "rqrgibbs_desn_design/1.0.0",
    materialization_receipt =
      "rqrgibbs_desn_materialization_receipt/2.0.0",
    materialization_receipt_status =
      "rqrgibbs_desn_materialization_receipt_status/1.0.0",
    materialization_verification =
      "rqrgibbs_desn_materialization_verification/1.0.0",
    fit = "rqrgibbs_desn_fit/1.1.0",
    future_design = "rqrgibbs_desn_future_design/1.1.0",
    future_verification =
      "rqrgibbs_desn_future_verification/1.0.0"
  )
  if (!identical(config$desn_schema_contract, expected_schemas)) {
    stop("The frozen DESN schema contract changed.", call. = FALSE)
  }
  d02 <- config$fixtures$D02 %||% NULL
  if (!is.list(d02)) stop("The frozen D02 fixture is absent.", call. = FALSE)
  response <- d02$response_history
  if (!is.numeric(response) || length(response) < 8L ||
      anyNA(response) || any(!is.finite(response))) {
    stop(
      "D02$response_history must be a complete finite numeric history.",
      call. = FALSE
    )
  }
  seed <- rqr_ordinary_v1_materializer_integer(
    d02$materializer_seed, "D02$materializer_seed"
  )
  arguments <- d02$effective_arguments
  if (!is.list(arguments) || !length(arguments) ||
      is.null(names(arguments)) || anyNA(names(arguments)) ||
      any(!nzchar(names(arguments))) || anyDuplicated(names(arguments))) {
    stop("D02$effective_arguments must be a fully named unique list.",
         call. = FALSE)
  }
  forbidden <- intersect(
    names(arguments),
    c(
      "y", "p0", "fit_readout", "vb_args", "coverage_level",
      "design", "design_engine", "design_metadata", "inference",
      "provenance_control", "mcmc_args", "lambda_prior",
      "learning_rate_mode", "numerical_policy"
    )
  )
  if (length(forbidden)) {
    stop(
      "D02$effective_arguments contains protected fields: ",
      paste(forbidden, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!"seed" %in% names(arguments)) {
    stop(
      "D02$effective_arguments must include its materializer seed.",
      call. = FALSE
    )
  }
  argument_seed <- rqr_ordinary_v1_materializer_integer(
    arguments$seed, "D02$effective_arguments$seed"
  )
  if (!identical(argument_seed, seed)) {
    stop(
      "D02 effective-argument seed and materializer_seed disagree.",
      call. = FALSE
    )
  }
  ledger <- config$seed_ledger
  ledger_rows <- which(
    ledger$purpose == "desn_materialization" &
      ledger$fixture_id == "D02"
  )
  if (length(ledger_rows) != 1L) {
    stop(
      "D02 materializer seed does not match its unique seed-ledger row.",
      call. = FALSE
    )
  }
  ledger_seed <- rqr_ordinary_v1_materializer_integer(
    ledger$seed[[ledger_rows]], "D02 seed-ledger seed"
  )
  if (!identical(ledger_seed, seed)) {
    stop(
      "D02 materializer seed does not match its unique seed-ledger row.",
      call. = FALSE
    )
  }
  if (!identical(
        config$pinned_exdqlm$commit,
        "dffb71ee70b597d6a716ee74be1cbc99731cd453"
      ) ||
      !identical(
        config$pinned_exdqlm$branch,
        "feature/rqr-desn-readout-20260716"
      ) ||
      !isTRUE(
        d02$promotion_requires_isolated_attested_exdqlm_runtime
      )) {
    stop("D02 does not bind the reviewed pinned exdqlm source.",
         call. = FALSE)
  }
  list(
    response_history = as.numeric(response),
    materializer_seed = seed,
    effective_arguments = arguments,
    response_digest = digest::digest(
      as.numeric(response), algo = "sha256", serialize = TRUE
    ),
    arguments_digest = digest::digest(
      arguments, algo = "sha256", serialize = TRUE
    )
  )
}

rqr_ordinary_v1_materializer_path_within <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

rqr_ordinary_v1_materializer_output_dir <- function(
    repo_root, expected_commit, config_digest) {
  supplied <- Sys.getenv(
    "RQR_ORDINARY_V1_DESN_MATERIALIZATION_DIR", unset = ""
  )
  path <- if (nzchar(supplied)) {
    supplied
  } else {
    file.path(
      repo_root, "application", "cache",
      "rqr_ordinary_v1_desn_materialization",
      expected_commit, substr(config_digest, 1L, 16L)
    )
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  allowed <- c(
    file.path(repo_root, "application", "cache"),
    file.path(repo_root, "application", "outputs")
  )
  if (!any(vapply(
        allowed,
        function(root) {
          rqr_ordinary_v1_materializer_path_within(path, root) &&
            !identical(
              normalizePath(path, winslash = "/", mustWork = FALSE),
              normalizePath(root, winslash = "/", mustWork = FALSE)
            )
        },
        logical(1L)
      ))) {
    stop(
      paste(
        "Materialization output must be a child of ignored",
        "application/cache or application/outputs."
      ),
      call. = FALSE
    )
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

rqr_ordinary_v1_materializer_sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

rqr_ordinary_v1_materializer_is_regular_file <- function(path) {
  posix_test <- Sys.which("test")
  if (!nzchar(posix_test)) {
    stop(
      "The POSIX test utility is required for atomic materialization.",
      call. = FALSE
    )
  }
  status <- suppressWarnings(system2(
    posix_test,
    c("-f", shQuote(path)),
    stdout = FALSE,
    stderr = FALSE
  ))
  if (is.null(status)) status <- 0L
  identical(as.integer(status), 0L)
}

rqr_ordinary_v1_materializer_assert_regular_file <- function(
    path, role, allow_absent = FALSE) {
  if (!is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(path)) {
    stop(sprintf("%s path must be one nonempty string.", role),
         call. = FALSE)
  }
  link_target <- Sys.readlink(path)
  if (length(link_target) == 1L &&
      !is.na(link_target) && nzchar(link_target)) {
    stop(sprintf("%s cannot be a symbolic link.", role), call. = FALSE)
  }
  exists <- file.exists(path) || dir.exists(path)
  if (!exists && isTRUE(allow_absent)) {
    return(invisible(TRUE))
  }
  if (!exists ||
      !rqr_ordinary_v1_materializer_is_regular_file(path)) {
    stop(sprintf("%s must be a regular file.", role), call. = FALSE)
  }
  invisible(TRUE)
}

rqr_ordinary_v1_materializer_atomic <- function(path, writer, validator) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  rqr_ordinary_v1_materializer_assert_regular_file(
    path, "Materialization destination", allow_absent = TRUE
  )
  temporary <- tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  writer(temporary)
  rqr_ordinary_v1_materializer_assert_regular_file(
    temporary, "Temporary materialization artifact"
  )
  if (!isTRUE(validator(temporary))) {
    stop(
      "Atomic materialization artifact validation failed: ",
      basename(path), ".",
      call. = FALSE
    )
  }
  # The temporary file is created in the destination directory. On the Jerez
  # POSIX runtime, this same-filesystem rename atomically replaces an existing
  # regular file. Recheck the destination after validation and never unlink
  # the prior artifact before publication.
  rqr_ordinary_v1_materializer_assert_regular_file(
    path, "Materialization destination", allow_absent = TRUE
  )
  if (!file.rename(temporary, path)) {
    stop(
      "Cannot publish materialization artifact atomically: ",
      basename(path), ".",
      call. = FALSE
    )
  }
  rqr_ordinary_v1_materializer_assert_regular_file(
    path, "Published materialization artifact"
  )
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

rqr_ordinary_v1_materializer_atomic_csv <- function(data, path) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!"schema_version" %in% names(data)) {
    data <- cbind(
      schema_version = rqr_ordinary_v1_materializer_schema(),
      data,
      stringsAsFactors = FALSE
    )
  }
  rqr_ordinary_v1_materializer_atomic(
    path,
    function(temporary) {
      utils::write.csv(data, temporary, row.names = FALSE, na = "")
    },
    function(temporary) {
      readback <- utils::read.csv(
        temporary, stringsAsFactors = FALSE, check.names = FALSE,
        na.strings = c("", "NA")
      )
      nrow(readback) == nrow(data) &&
        identical(names(readback), names(data))
    }
  )
}

rqr_ordinary_v1_materializer_atomic_lines <- function(lines, path) {
  lines <- enc2utf8(as.character(lines))
  rqr_ordinary_v1_materializer_atomic(
    path,
    function(temporary) writeLines(lines, temporary, useBytes = TRUE),
    function(temporary) {
      identical(readLines(temporary, warn = FALSE), lines)
    }
  )
}

rqr_ordinary_v1_materializer_atomic_rds <- function(object, path, validator) {
  digest_before <- digest::digest(object, algo = "sha256", serialize = TRUE)
  rqr_ordinary_v1_materializer_atomic(
    path,
    function(temporary) saveRDS(object, temporary, version = 3),
    function(temporary) {
      readback <- readRDS(temporary)
      identical(
        digest::digest(readback, algo = "sha256", serialize = TRUE),
        digest_before
      ) && isTRUE(validator(readback))
    }
  )
}

rqr_ordinary_v1_materializer_manifest <- function(directory) {
  root <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  files <- list.files(
    root, recursive = TRUE, full.names = TRUE, all.files = FALSE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  files <- files[basename(files) != "artifact_hashes.csv"]
  relative <- substring(files, nchar(root) + 2L)
  ordering <- order(relative)
  files <- files[ordering]
  relative <- relative[ordering]
  result <- data.frame(
    relative_path = relative,
    byte_count = as.numeric(file.info(files)$size),
    sha256 = unname(vapply(
      files,
      rqr_ordinary_v1_materializer_sha256_file,
      character(1L)
    )),
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL
  result
}

rqr_ordinary_v1_materializer_validate_manifest <- function(
    directory, manifest) {
  required <- c("relative_path", "byte_count", "sha256")
  if (!is.data.frame(manifest) || any(!required %in% names(manifest))) {
    return(FALSE)
  }
  actual <- rqr_ordinary_v1_materializer_manifest(directory)
  identical(
    as.character(actual$relative_path),
    as.character(manifest$relative_path)
  ) &&
    identical(
      as.numeric(actual$byte_count),
      as.numeric(manifest$byte_count)
    ) &&
    identical(as.character(actual$sha256), as.character(manifest$sha256))
}

rqr_ordinary_v1_materializer_runtime_gates <- function(
    state, package, commit, runtime_path) {
  required_true <- c(
    "runtime_attestation_match", "source_archive_verified",
    "source_archive_tree_match", "source_package_verified",
    "source_package_archive_match", "build_evidence_verified",
    "install_evidence_verified", "runtime_lineage_marker_match",
    "runtime_install_receipt_match", "source_checkout_unchanged",
    "source_archive_isolated_from_source",
    "runtime_isolated_from_source", "runtime_source_match",
    "reproducibility_eligible"
  )
  true_values <- vapply(
    required_true, function(field) isTRUE(state[[field]]), logical(1L)
  )
  path_matches <- is.character(state$runtime_package_path) &&
    length(state$runtime_package_path) == 1L &&
    !is.na(state$runtime_package_path) &&
    identical(
      normalizePath(
        state$runtime_package_path, winslash = "/", mustWork = TRUE
      ),
      normalizePath(runtime_path, winslash = "/", mustWork = TRUE)
    )
  package_matches <- identical(state$runtime_package, package)
  commit_matches <- identical(state$git_commit, commit) &&
    identical(state$expected_git_commit, commit) &&
    isTRUE(state$expected_git_commit_match)
  isolated_required <- isTRUE(state$require_isolated_runtime)
  list(
    pass = all(true_values) && path_matches && package_matches &&
      commit_matches && isolated_required,
    table = data.frame(
      gate = c(
        required_true, "runtime_path", "runtime_package",
        "source_commit", "isolated_runtime_required"
      ),
      pass = c(
        unname(true_values), path_matches, package_matches,
        commit_matches, isolated_required
      ),
      stringsAsFactors = FALSE
    )
  )
}

rqr_ordinary_v1_materializer_runtime_state <- function(
    repo_root, expected_commit, primary_attestation,
    exdqlm_root, exdqlm_commit, exdqlm_attestation) {
  primary_spec <- list(
    repo_root = repo_root,
    expected_git_commit = expected_commit,
    runtime_package = "rqrgibbs",
    runtime_attestation = primary_attestation,
    require_isolated_runtime = TRUE,
    source_subdir = "application"
  )
  exdqlm_spec <- list(
    repo_root = exdqlm_root,
    expected_git_commit = exdqlm_commit,
    runtime_package = "exdqlm",
    runtime_attestation = exdqlm_attestation,
    require_isolated_runtime = TRUE,
    source_subdir = "."
  )
  provenance <- getFromNamespace(
    ".rqr_repository_provenance", "rqrgibbs"
  )
  primary <- provenance(primary_spec)
  external <- provenance(exdqlm_spec)
  primary_path <- normalizePath(
    getNamespaceInfo(asNamespace("rqrgibbs"), "path"),
    winslash = "/", mustWork = TRUE
  )
  external_path <- normalizePath(
    getNamespaceInfo(asNamespace("exdqlm"), "path"),
    winslash = "/", mustWork = TRUE
  )
  primary_gates <- rqr_ordinary_v1_materializer_runtime_gates(
    primary, "rqrgibbs", expected_commit, primary_path
  )
  external_gates <- rqr_ordinary_v1_materializer_runtime_gates(
    external, "exdqlm", exdqlm_commit, external_path
  )
  if (!isTRUE(primary_gates$pass) || !isTRUE(external_gates$pass)) {
    failed <- rbind(
      transform(primary_gates$table, package = "rqrgibbs"),
      transform(external_gates$table, package = "exdqlm")
    )
    failed <- failed[!failed$pass, , drop = FALSE]
    stop(
      "Isolated runtime lineage failed: ",
      paste(paste(failed$package, failed$gate, sep = ":"), collapse = ", "),
      call. = FALSE
    )
  }
  list(
    primary = primary,
    external = external,
    primary_spec = primary_spec,
    external_spec = exdqlm_spec,
    gates = rbind(
      transform(primary_gates$table, package = "rqrgibbs"),
      transform(external_gates$table, package = "exdqlm")
    )
  )
}

rqr_ordinary_v1_materializer_validate_design <- function(
    design, config, d02, runtime, exdqlm_attestation,
    design_validator = NULL) {
  if (is.null(design_validator)) {
    design_validator <- getExportedValue(
      "rqrgibbs", "rqr_validate_desn_design"
    )
  }
  design_validator(design)
  if (!inherits(design, "rqr_desn_design") ||
      any(c(
        "fit", "samp.beta_root1", "samp.beta_root2",
        "samp.lambda", "posterior_draws"
      ) %in% names(design))) {
    stop(
      "Materialization must return only an rqr_desn_design, never a fit.",
      call. = FALSE
    )
  }
  receipt <- design$builder$materialization_receipt %||% NULL
  attestation_sha256 <-
    rqr_ordinary_v1_materializer_sha256_file(exdqlm_attestation)
  status_function <- getFromNamespace(
    ".rqr_desn_materialization_receipt_status", "rqrgibbs"
  )
  verification_function <- getFromNamespace(
    ".rqr_desn_materialization_verification", "rqrgibbs"
  )
  receipt_status <- status_function(design)
  verification <- verification_function(
    design,
    external_state = runtime$external,
    runtime_attestation = exdqlm_attestation
  )
  expected <- list(
    builder_id = "exdqlm_qdesn_fit_vb_design_adapter",
    builder_version =
      as.character(runtime$external$runtime_package_version),
    builder_source_commit = config$pinned_exdqlm$commit,
    builder_arguments_digest = d02$arguments_digest,
    builder_adapter = "rqrgibbs_frozen_design_materializer/2.0.0",
    receipt_schema = "rqrgibbs_desn_materialization_receipt/2.0.0",
    receipt_package = "exdqlm",
    receipt_package_version =
      as.character(runtime$external$runtime_package_version),
    receipt_source_commit = config$pinned_exdqlm$commit,
    receipt_source_tree_digest = runtime$external$source_tree_digest,
    receipt_runtime_tree_digest =
      runtime$external$runtime_package_tree_digest,
    receipt_attestation_schema =
      runtime$external$runtime_attestation_schema,
    receipt_attestation_sha256 = attestation_sha256,
    receipt_materializer_arguments_digest = d02$arguments_digest,
    receipt_materialized_design_payload_digest =
      receipt_status$materialized_design_payload_digest,
    receipt_status_schema =
      "rqrgibbs_desn_materialization_receipt_status/1.0.0",
    materialization_verification_schema =
      "rqrgibbs_desn_materialization_verification/1.0.0",
    reservoir_source_package = "exdqlm",
    reservoir_source_commit = config$pinned_exdqlm$commit
  )
  actual <- list(
    builder_id = design$builder$id,
    builder_version = design$builder$version,
    builder_source_commit = design$builder$source_commit,
    builder_arguments_digest = design$builder$arguments_digest,
    builder_adapter = design$builder$adapter,
    receipt_schema = receipt$schema_version %||% NA_character_,
    receipt_package = receipt$package %||% NA_character_,
    receipt_package_version =
      receipt$package_version %||% NA_character_,
    receipt_source_commit = receipt$source_commit %||% NA_character_,
    receipt_source_tree_digest =
      receipt$source_tree_digest %||% NA_character_,
    receipt_runtime_tree_digest =
      receipt$runtime_tree_digest %||% NA_character_,
    receipt_attestation_schema =
      receipt$runtime_attestation_schema %||% NA_character_,
    receipt_attestation_sha256 =
      receipt$runtime_attestation_sha256 %||% NA_character_,
    receipt_materializer_arguments_digest =
      receipt$materializer_arguments_digest %||% NA_character_,
    receipt_materialized_design_payload_digest =
      receipt$materialized_design_payload_digest %||% NA_character_,
    receipt_status_schema =
      receipt_status$schema_version %||% NA_character_,
    materialization_verification_schema =
      verification$schema_version %||% NA_character_,
    reservoir_source_package =
      design$reservoir$source_package %||% NA_character_,
    reservoir_source_commit =
      design$reservoir$source_commit %||% NA_character_
  )
  comparisons <- data.frame(
    field = names(expected),
    expected = vapply(expected, as.character, character(1L)),
    actual = vapply(actual, as.character, character(1L)),
    stringsAsFactors = FALSE
  )
  comparisons$pass <- comparisons$actual == comparisons$expected
  semantic_flags <- c(
    receipt_valid = isTRUE(receipt_status$receipt_valid),
    current_external_state_match =
      isTRUE(verification$external_state_match),
    current_attestation_hash_verified =
      isTRUE(verification$runtime_attestation_sha256_verified),
    materialization_reproducibility_eligible =
      isTRUE(verification$materialization_reproducibility_eligible),
    response_simulation_disabled =
      !isTRUE(design$driver$response_simulation),
    no_current_response = !isTRUE(design$causal$uses_current_response),
    no_future_response = !isTRUE(design$causal$uses_future_response),
    finite_design = is.matrix(design$X) && all(is.finite(design$X)),
    finite_aligned_response =
      is.numeric(design$y) && all(is.finite(design$y)) &&
        nrow(design$X) == length(design$y),
    materializer_seed_bound =
      identical(
        rqr_ordinary_v1_materializer_integer(
          d02$effective_arguments$seed,
          "D02$effective_arguments$seed"
        ),
        d02$materializer_seed
      )
  )
  if (!all(comparisons$pass) || !all(semantic_flags)) {
    stop(
      "The materialized design failed its live source/runtime receipt contract.",
      call. = FALSE
    )
  }
  list(
    receipt = receipt,
    receipt_status = receipt_status,
    verification = verification,
    comparisons = comparisons,
    semantic_flags = semantic_flags,
    design_digest = digest::digest(
      design, algo = "sha256", serialize = TRUE
    )
  )
}

rqr_ordinary_v1_materializer_main <- function(
    arguments = commandArgs(trailingOnly = TRUE)) {
  if (length(arguments)) {
    stop(
      "This materializer accepts no positional arguments; use reviewed environment variables.",
      call. = FALSE
    )
  }
  repo_root <- rqr_ordinary_v1_materializer_find_repo()
  helper_path <- file.path(
    repo_root, "application", "scripts", "lib",
    "pinned_exdqlm_runtime.R"
  )
  if (!file.exists(helper_path)) {
    stop("The protected-checkout helper is absent.", call. = FALSE)
  }
  sys.source(helper_path, envir = environment())

  expected_commit <- tolower(Sys.getenv(
    "RQR_EXPECTED_PRIMARY_COMMIT", unset = ""
  ))
  if (!grepl("^[0-9a-f]{40}$", expected_commit)) {
    stop(
      "RQR_EXPECTED_PRIMARY_COMMIT must be the reviewed full primary SHA.",
      call. = FALSE
    )
  }
  config <- rqr_ordinary_v1_materializer_load_config(repo_root)
  branch <- rqr_ordinary_v1_materializer_read_git(
    repo_root, c("rev-parse", "--abbrev-ref", "HEAD")
  )
  actual_commit <- tolower(rqr_ordinary_v1_materializer_read_git(
    repo_root, c("rev-parse", "HEAD")
  ))
  source_status <- rqr_ordinary_v1_materializer_read_git(
    repo_root, c("status", "--porcelain=v2", "--untracked-files=all")
  )
  if (!identical(branch, "main") ||
      !identical(actual_commit, expected_commit) ||
      nzchar(source_status)) {
    stop(
      "Materialization requires clean main at the reviewed full primary SHA.",
      call. = FALSE
    )
  }

  exdqlm_root <- Sys.getenv(
    "RQR_EXDQLM_REFERENCE_ROOT",
    unset = file.path(
      dirname(repo_root), "exdqlm__wt__qdesn_0p4p0_integration"
    )
  )
  exdqlm_root <- normalizePath(
    exdqlm_root, winslash = "/", mustWork = TRUE
  )
  primary_runtime_root <- normalizePath(
    Sys.getenv(
      "RQR_PRIMARY_RUNTIME_ROOT",
      unset = file.path(dirname(repo_root), ".rqr_gibbs_primary_runtime")
    ),
    winslash = "/", mustWork = TRUE
  )
  primary_commit_root <- file.path(primary_runtime_root, expected_commit)
  primary_library <- normalizePath(
    file.path(primary_commit_root, "library"),
    winslash = "/", mustWork = TRUE
  )
  primary_attestation <- normalizePath(
    file.path(
      primary_commit_root, "attestations",
      paste0("rqrgibbs_", expected_commit, ".rds")
    ),
    winslash = "/", mustWork = TRUE
  )
  exdqlm_layout <- rqr_exdqlm_runtime_layout(
    repo_root = repo_root,
    exdqlm_repo = exdqlm_root,
    pinned_commit = config$pinned_exdqlm$commit
  )
  exdqlm_library <- normalizePath(
    exdqlm_layout$library_root,
    winslash = "/", mustWork = TRUE
  )
  exdqlm_attestation <- Sys.getenv(
    "RQR_EXDQLM_RUNTIME_ATTESTATION",
    unset = exdqlm_layout$attestation_path
  )
  exdqlm_attestation <- normalizePath(
    exdqlm_attestation, winslash = "/", mustWork = TRUE
  )

  if ("rqrgibbs" %in% loadedNamespaces() ||
      "exdqlm" %in% loadedNamespaces()) {
    stop(
      "rqrgibbs and exdqlm must not be loaded before isolated-library selection.",
      call. = FALSE
    )
  }
  .libPaths(c(primary_library, exdqlm_library, .libPaths()))
  required <- c("digest", "rqrgibbs", "exdqlm")
  missing <- required[!vapply(
    required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L)
  )]
  if (length(missing)) {
    stop(
      "Missing isolated materializer packages: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  executing_primary <- normalizePath(
    getNamespaceInfo(asNamespace("rqrgibbs"), "path"),
    winslash = "/", mustWork = TRUE
  )
  executing_external <- normalizePath(
    getNamespaceInfo(asNamespace("exdqlm"), "path"),
    winslash = "/", mustWork = TRUE
  )
  if (!identical(
        executing_primary,
        normalizePath(
          file.path(primary_library, "rqrgibbs"),
          winslash = "/", mustWork = TRUE
        )
      ) ||
      !identical(
        executing_external,
        normalizePath(
          file.path(exdqlm_library, "exdqlm"),
          winslash = "/", mustWork = TRUE
        )
      ) ||
      rqr_ordinary_v1_materializer_path_within(
        executing_primary, repo_root
      ) ||
      rqr_ordinary_v1_materializer_path_within(
        executing_external, exdqlm_root
      )) {
    stop(
      "A package namespace is not executing from its exact isolated library.",
      call. = FALSE
    )
  }

  d02 <- rqr_ordinary_v1_materializer_validate_d02(config)
  primary_before <-
    rqr_ordinary_v1_materializer_primary_snapshot(repo_root)
  external_before <- rqr_capture_external_checkout(exdqlm_root)
  external_guard_pending <- TRUE
  on.exit({
    if (isTRUE(external_guard_pending)) {
      rqr_assert_external_checkout_unchanged(external_before)
    }
  }, add = TRUE)
  if (!identical(
        external_before$branch, config$pinned_exdqlm$branch
      ) ||
      !identical(
        external_before$commit, config$pinned_exdqlm$commit
      ) ||
      nzchar(external_before$status)) {
    stop(
      "The protected exdqlm checkout is not clean at its pinned branch/commit.",
      call. = FALSE
    )
  }

  runtime <- rqr_ordinary_v1_materializer_runtime_state(
    repo_root = repo_root,
    expected_commit = expected_commit,
    primary_attestation = primary_attestation,
    exdqlm_root = exdqlm_root,
    exdqlm_commit = config$pinned_exdqlm$commit,
    exdqlm_attestation = exdqlm_attestation
  )
  provenance_control <- list(
    repo_root = repo_root,
    expected_git_commit = expected_commit,
    primary_runtime_attestation = primary_attestation,
    external_repositories = list(exdqlm = runtime$external_spec),
    required_external_repositories = "exdqlm"
  )

  materializer_arguments <- c(
    list(
      y = d02$response_history,
      coverage_level = config$coverage_level,
      design_engine = "exdqlm_reference",
      inference = "mcmc",
      fit_readout = FALSE,
      provenance_control = provenance_control
    ),
    d02$effective_arguments
  )
  design <- do.call(
    getExportedValue("rqrgibbs", "rqr_desn_fit"),
    materializer_arguments
  )
  validation <- rqr_ordinary_v1_materializer_validate_design(
    design = design,
    config = config,
    d02 = d02,
    runtime = runtime,
    exdqlm_attestation = exdqlm_attestation
  )

  external_after <- rqr_assert_external_checkout_unchanged(
    external_before
  )
  external_guard_pending <- FALSE
  primary_after <-
    rqr_ordinary_v1_materializer_primary_snapshot(repo_root)
  if (!identical(primary_before$digest, primary_after$digest)) {
    stop(
      "The primary tracked Git state changed during materialization.",
      call. = FALSE
    )
  }

  config_for_digest <- config
  attr(config_for_digest, "source_path") <- NULL
  config_digest <- digest::digest(
    config_for_digest, algo = "sha256", serialize = TRUE
  )
  output_dir <- rqr_ordinary_v1_materializer_output_dir(
    repo_root, expected_commit, config_digest
  )
  design_path <- file.path(
    output_dir,
    paste0(
      "rqr_ordinary_v1_attested_desn_design_",
      expected_commit, ".rds"
    )
  )
  design_path <- rqr_ordinary_v1_materializer_atomic_rds(
    design, design_path,
    validator = function(readback) {
      isTRUE(tryCatch({
        rqr_ordinary_v1_materializer_validate_design(
          readback, config, d02, runtime, exdqlm_attestation
        )
        TRUE
      }, error = function(error) FALSE))
    }
  )

  runtime_rows <- lapply(list(
    rqrgibbs = runtime$primary,
    exdqlm = runtime$external
  ), function(state) {
    attestation <- if (identical(
        state$runtime_package, "rqrgibbs"
      )) primary_attestation else exdqlm_attestation
    data.frame(
      package = state$runtime_package,
      package_version = state$runtime_package_version,
      source_commit = state$git_commit,
      source_tree_digest = state$source_tree_digest,
      runtime_path = state$runtime_package_path,
      runtime_tree_digest = state$runtime_package_tree_digest,
      runtime_attestation_schema = state$runtime_attestation_schema,
      runtime_attestation_sha256 =
        rqr_ordinary_v1_materializer_sha256_file(attestation),
      runtime_source_match = state$runtime_source_match,
      reproducibility_eligible = state$reproducibility_eligible,
      stringsAsFactors = FALSE
    )
  })
  runtime_table <- do.call(rbind, runtime_rows)
  rqr_ordinary_v1_materializer_atomic_csv(
    runtime_table,
    file.path(output_dir, "runtime_attestations.csv")
  )
  rqr_ordinary_v1_materializer_atomic_csv(
    cbind(
      package = runtime$gates$package,
      runtime$gates[, c("gate", "pass"), drop = FALSE],
      stringsAsFactors = FALSE
    ),
    file.path(output_dir, "runtime_gates.csv")
  )
  rqr_ordinary_v1_materializer_atomic_csv(
    validation$comparisons,
    file.path(output_dir, "receipt_comparisons.csv")
  )
  rqr_ordinary_v1_materializer_atomic_csv(
    data.frame(
      check = names(validation$semantic_flags),
      pass = unname(validation$semantic_flags),
      stringsAsFactors = FALSE
    ),
    file.path(output_dir, "semantic_checks.csv")
  )
  rqr_ordinary_v1_materializer_atomic_csv(
    data.frame(
      config_id = config$config_id,
      config_digest = config_digest,
      config_path = attr(config, "source_path"),
      source_commit = expected_commit,
      materializer_seed = d02$materializer_seed,
      effective_arguments_digest = d02$arguments_digest,
      response_history_digest = d02$response_digest,
      response_history_length = length(d02$response_history),
      design_semantic_digest = design$semantic_digest,
      design_object_digest = validation$design_digest,
      design_file = basename(design_path),
      design_file_sha256 =
        rqr_ordinary_v1_materializer_sha256_file(design_path),
      design_rows = nrow(design$X),
      design_columns = ncol(design$X),
      fit_readout = FALSE,
      mcmc_executed = FALSE,
      vb_executed = FALSE,
      response_simulation = FALSE,
      stringsAsFactors = FALSE
    ),
    file.path(output_dir, "materialization_contract.csv")
  )
  rqr_ordinary_v1_materializer_atomic_csv(
    data.frame(
      repository = c("RQR-GIBBS", "exdqlm"),
      branch = c(
        primary_before$fields[["branch"]],
        external_before$branch
      ),
      commit = c(
        primary_before$fields[["commit"]],
        external_before$commit
      ),
      clean = c(
        !nzchar(primary_before$fields[["status"]]),
        !nzchar(external_before$status)
      ),
      before_guard_digest = c(
        primary_before$digest, external_before$guard_digest
      ),
      after_guard_digest = c(
        primary_after$digest, external_after$guard_digest
      ),
      unchanged = c(
        identical(primary_before$digest, primary_after$digest),
        identical(
          external_before$guard_digest,
          external_after$guard_digest
        )
      ),
      stringsAsFactors = FALSE
    ),
    file.path(output_dir, "source_state.csv")
  )
  rqr_ordinary_v1_materializer_atomic_csv(
    data.frame(
      status = "pass",
      operation = "design_materialization_only",
      fits_executed = 0L,
      response_draws_generated = 0L,
      stringsAsFactors = FALSE
    ),
    file.path(output_dir, "run_status.csv")
  )
  rqr_ordinary_v1_materializer_atomic_lines(
    capture.output(sessionInfo()),
    file.path(output_dir, "session_info.txt")
  )
  manifest <- rqr_ordinary_v1_materializer_manifest(output_dir)
  manifest_path <- rqr_ordinary_v1_materializer_atomic_csv(
    manifest, file.path(output_dir, "artifact_hashes.csv")
  )
  manifest_readback <- utils::read.csv(
    manifest_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!rqr_ordinary_v1_materializer_validate_manifest(
      output_dir, manifest_readback
    )) {
    stop("The final materialization artifact manifest failed.",
         call. = FALSE)
  }
  message("ordinary-v1 DESN materialization PASS")
  message("design: ", design_path)
  message("evidence: ", output_dir)
  invisible(list(
    design_path = design_path,
    output_dir = output_dir,
    design_digest = validation$design_digest
  ))
}

if (!identical(
    Sys.getenv(
      "RQR_ORDINARY_V1_MATERIALIZER_SOURCE_ONLY", unset = ""
    ),
    "YES"
  )) {
  rqr_ordinary_v1_materializer_main()
}
