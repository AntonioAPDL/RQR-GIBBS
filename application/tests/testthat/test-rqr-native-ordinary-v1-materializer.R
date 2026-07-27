ordinary_v1_materializer_environment <- function() {
  path <- testthat::test_path(
    "..", "..", "scripts",
    "28_materialize_rqr_ordinary_v1_desn_design.R"
  )
  old <- Sys.getenv(
    "RQR_ORDINARY_V1_MATERIALIZER_SOURCE_ONLY",
    unset = NA_character_
  )
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("RQR_ORDINARY_V1_MATERIALIZER_SOURCE_ONLY")
    } else {
      Sys.setenv(RQR_ORDINARY_V1_MATERIALIZER_SOURCE_ONLY = old)
    }
  }, add = TRUE)
  Sys.setenv(RQR_ORDINARY_V1_MATERIALIZER_SOURCE_ONLY = "YES")
  environment <- new.env(parent = globalenv())
  sys.source(path, envir = environment)
  environment
}

ordinary_v1_materializer_repo <- function() {
  normalizePath(
    testthat::test_path("..", "..", ".."),
    winslash = "/", mustWork = TRUE
  )
}

ordinary_v1_materializer_config <- function(environment) {
  environment$rqr_ordinary_v1_materializer_load_config(
    ordinary_v1_materializer_repo()
  )
}

test_that("ordinary-v1 materializer freezes the exact D02 seed contract", {
  materializer <- ordinary_v1_materializer_environment()
  config <- ordinary_v1_materializer_config(materializer)
  d02 <- materializer$rqr_ordinary_v1_materializer_validate_d02(config)

  expect_identical(d02$materializer_seed, 82701L)
  expect_identical(d02$effective_arguments$seed, 82701L)
  expect_identical(d02$effective_arguments$add_bias, TRUE)
  expect_identical(
    d02$effective_arguments$input_mode, "raw_y_lags"
  )
  expect_identical(
    d02$effective_arguments$standardize_inputs, FALSE
  )
  expect_identical(length(d02$response_history), 48L)
  expect_false(anyNA(d02$response_history))
  expect_true(all(is.finite(d02$response_history)))
  expect_match(d02$response_digest, "^[0-9a-f]{64}$")
  expect_match(d02$arguments_digest, "^[0-9a-f]{64}$")
  expect_identical(
    d02$arguments_digest,
    digest::digest(
      config$fixtures$D02$effective_arguments,
      algo = "sha256", serialize = TRUE
    )
  )

  bad <- config
  bad$fixtures$D02$effective_arguments$seed <- 82702L
  expect_error(
    materializer$rqr_ordinary_v1_materializer_validate_d02(bad),
    "seed and materializer_seed disagree"
  )

  bad <- config
  bad$fixtures$D02$response_history[[1L]] <- NA_real_
  expect_error(
    materializer$rqr_ordinary_v1_materializer_validate_d02(bad),
    "complete finite numeric history"
  )

  bad <- config
  bad$fixtures$D02$effective_arguments$fit_readout <- FALSE
  expect_error(
    materializer$rqr_ordinary_v1_materializer_validate_d02(bad),
    "protected fields"
  )

  bad <- config
  bad$fixtures$D02$effective_arguments <- c(
    bad$fixtures$D02$effective_arguments,
    list(seed = 82701L)
  )
  expect_error(
    materializer$rqr_ordinary_v1_materializer_validate_d02(bad),
    "fully named unique list"
  )

  bad <- config
  row <- which(
    bad$seed_ledger$purpose == "desn_materialization" &
      bad$seed_ledger$fixture_id == "D02"
  )
  bad$seed_ledger$seed[[row]] <- 82702L
  expect_error(
    materializer$rqr_ordinary_v1_materializer_validate_d02(bad),
    "seed-ledger row"
  )

  bad <- config
  row <- which(
    bad$seed_ledger$purpose == "desn_materialization" &
      bad$seed_ledger$fixture_id == "D02"
  )
  bad$seed_ledger$seed[[row]] <- 82701.5
  expect_error(
    materializer$rqr_ordinary_v1_materializer_validate_d02(bad),
    "finite integer"
  )
})

test_that("materializer output is confined to ignored local roots", {
  materializer <- ordinary_v1_materializer_environment()
  repo_root <- ordinary_v1_materializer_repo()
  target <- tempfile(
    "ordinary-v1-materializer-test-",
    tmpdir = file.path(repo_root, "application", "cache")
  )
  on.exit(unlink(target, recursive = TRUE, force = TRUE), add = TRUE)
  withr::local_envvar(
    RQR_ORDINARY_V1_DESN_MATERIALIZATION_DIR = target
  )

  observed <- materializer$rqr_ordinary_v1_materializer_output_dir(
    repo_root, paste(rep("a", 40L), collapse = ""),
    paste(rep("b", 64L), collapse = "")
  )
  expect_identical(
    observed,
    normalizePath(target, winslash = "/", mustWork = TRUE)
  )

  outside <- tempfile("ordinary-v1-materializer-outside-")
  withr::local_envvar(
    RQR_ORDINARY_V1_DESN_MATERIALIZATION_DIR = outside
  )
  expect_error(
    materializer$rqr_ordinary_v1_materializer_output_dir(
      repo_root, paste(rep("a", 40L), collapse = ""),
      paste(rep("b", 64L), collapse = "")
    ),
    "must be a child of ignored"
  )
})

test_that("materializer artifacts publish atomically and rehash exactly", {
  materializer <- ordinary_v1_materializer_environment()
  directory <- withr::local_tempdir()
  object <- list(
    schema_version = "unit-test/1.0.0",
    X = matrix(seq_len(6L), nrow = 3L)
  )
  rds_path <- materializer$rqr_ordinary_v1_materializer_atomic_rds(
    object,
    file.path(directory, "design.rds"),
    validator = function(x) identical(x, object)
  )
  csv_path <- materializer$rqr_ordinary_v1_materializer_atomic_csv(
    data.frame(key = "fits_executed", value = 0L),
    file.path(directory, "status.csv")
  )
  text_path <- materializer$rqr_ordinary_v1_materializer_atomic_lines(
    c("readout_fit=FALSE", "response_simulation=FALSE"),
    file.path(directory, "contract.txt")
  )

  expect_true(all(file.exists(rds_path, csv_path, text_path)))
  manifest <- materializer$rqr_ordinary_v1_materializer_manifest(directory)
  expect_identical(
    manifest$relative_path,
    c("contract.txt", "design.rds", "status.csv")
  )
  expect_true(materializer$rqr_ordinary_v1_materializer_validate_manifest(
    directory, manifest
  ))
  bad <- manifest
  bad$sha256[[1L]] <- paste(rep("0", 64L), collapse = "")
  expect_false(materializer$rqr_ordinary_v1_materializer_validate_manifest(
    directory, bad
  ))
})

test_that("materializer atomically replaces only regular destinations", {
  materializer <- ordinary_v1_materializer_environment()
  directory <- withr::local_tempdir()
  path <- file.path(directory, "replace.txt")
  writeLines("old", path, useBytes = TRUE)

  real_rename <- base::file.rename
  destination_present_at_rename <- FALSE
  assign(
    "file.rename",
    function(from, to) {
      destination_present_at_rename <<- file.exists(to)
      real_rename(from, to)
    },
    envir = materializer
  )
  assign(
    "file.remove",
    function(...) stop("atomic publication must not call file.remove"),
    envir = materializer
  )
  on.exit({
    rm("file.rename", envir = materializer)
    rm("file.remove", envir = materializer)
  }, add = TRUE)

  observed <- materializer$rqr_ordinary_v1_materializer_atomic_lines(
    "new", path
  )
  expect_true(destination_present_at_rename)
  expect_identical(
    observed,
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
  expect_identical(readLines(path, warn = FALSE), "new")
})

test_that("materializer atomic failures preserve the previous artifact", {
  materializer <- ordinary_v1_materializer_environment()
  directory <- withr::local_tempdir()
  path <- file.path(directory, "preserved.txt")
  writeLines("old", path, useBytes = TRUE)

  expect_error(
    materializer$rqr_ordinary_v1_materializer_atomic(
      path,
      function(temporary) stop("injected writer failure"),
      function(temporary) TRUE
    ),
    "injected writer failure"
  )
  expect_identical(readLines(path, warn = FALSE), "old")

  expect_error(
    materializer$rqr_ordinary_v1_materializer_atomic(
      path,
      function(temporary) {
        writeLines("candidate", temporary, useBytes = TRUE)
      },
      function(temporary) FALSE
    ),
    "validation failed"
  )
  expect_identical(readLines(path, warn = FALSE), "old")

  assign("file.rename", function(from, to) FALSE, envir = materializer)
  on.exit(rm("file.rename", envir = materializer), add = TRUE)
  expect_error(
    materializer$rqr_ordinary_v1_materializer_atomic_lines(
      "candidate", path
    ),
    "Cannot publish materialization artifact atomically"
  )
  expect_identical(readLines(path, warn = FALSE), "old")
  expect_length(
    list.files(
      directory,
      pattern = paste0("^[.]", basename(path), "-"),
      all.files = TRUE
    ),
    0L
  )
})

test_that("materializer rejects symlink and nonregular publication targets", {
  materializer <- ordinary_v1_materializer_environment()
  directory <- withr::local_tempdir()

  directory_target <- file.path(directory, "directory-target")
  dir.create(directory_target)
  expect_error(
    materializer$rqr_ordinary_v1_materializer_atomic_lines(
      "candidate", directory_target
    ),
    "Materialization destination must be a regular file"
  )
  expect_true(dir.exists(directory_target))

  prior <- file.path(directory, "prior.txt")
  link <- file.path(directory, "linked-target.txt")
  writeLines("old", prior, useBytes = TRUE)
  linked <- file.symlink(prior, link)
  expect_true(linked)
  if (isTRUE(linked)) {
    expect_error(
      materializer$rqr_ordinary_v1_materializer_atomic_lines(
        "candidate", link
      ),
      "Materialization destination cannot be a symbolic link"
    )
  }
  expect_identical(readLines(prior, warn = FALSE), "old")
  if (isTRUE(linked)) {
    expect_true(nzchar(Sys.readlink(link)))
  }

  dangling_link <- file.path(directory, "dangling-target.txt")
  dangling_created <- file.symlink(
    file.path(directory, "absent-target.txt"),
    dangling_link
  )
  expect_true(dangling_created)
  if (isTRUE(dangling_created)) {
    expect_error(
      materializer$rqr_ordinary_v1_materializer_atomic_lines(
        "candidate", dangling_link
      ),
      "Materialization destination cannot be a symbolic link"
    )
  }

  mkfifo <- Sys.which("mkfifo")
  if (nzchar(mkfifo)) {
    fifo <- file.path(directory, "nonregular-target.fifo")
    expect_identical(
      as.integer(system2(mkfifo, fifo, stdout = FALSE, stderr = FALSE)),
      0L
    )
    expect_error(
      materializer$rqr_ordinary_v1_materializer_atomic_lines(
        "candidate", fifo
      ),
      "Materialization destination must be a regular file"
    )
  }

  regular_target <- file.path(directory, "regular-target.txt")
  writeLines("old", regular_target, useBytes = TRUE)
  expect_error(
    materializer$rqr_ordinary_v1_materializer_atomic(
      regular_target,
      function(temporary) file.symlink(prior, temporary),
      function(temporary) TRUE
    ),
    "Temporary materialization artifact cannot be a symbolic link"
  )
  expect_identical(readLines(regular_target, warn = FALSE), "old")

  expect_error(
    materializer$rqr_ordinary_v1_materializer_atomic(
      regular_target,
      function(temporary) dir.create(temporary),
      function(temporary) TRUE
    ),
    "Temporary materialization artifact must be a regular file"
  )
  expect_identical(readLines(regular_target, warn = FALSE), "old")
})

test_that("runtime promotion gate fails closed on every required fact", {
  materializer <- ordinary_v1_materializer_environment()
  runtime_path <- withr::local_tempdir()
  commit <- paste(rep("c", 40L), collapse = "")
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
  state <- as.list(setNames(rep(TRUE, length(required_true)), required_true))
  state$runtime_package_path <- runtime_path
  state$runtime_package <- "exdqlm"
  state$git_commit <- commit
  state$expected_git_commit <- commit
  state$expected_git_commit_match <- TRUE
  state$require_isolated_runtime <- TRUE

  observed <- materializer$rqr_ordinary_v1_materializer_runtime_gates(
    state, "exdqlm", commit, runtime_path
  )
  expect_true(observed$pass)
  expect_true(all(observed$table$pass))

  for (field in required_true) {
    bad <- state
    bad[[field]] <- FALSE
    expect_false(
      materializer$rqr_ordinary_v1_materializer_runtime_gates(
        bad, "exdqlm", commit, runtime_path
      )$pass,
      info = field
    )
  }
  bad <- state
  bad$git_commit <- paste(rep("d", 40L), collapse = "")
  expect_false(
    materializer$rqr_ordinary_v1_materializer_runtime_gates(
      bad, "exdqlm", commit, runtime_path
    )$pass
  )
})

test_that("source-only materializer contains no fitting execution path", {
  materializer <- ordinary_v1_materializer_environment()
  expect_true(is.function(
    materializer$rqr_ordinary_v1_materializer_main
  ))
  source <- paste(
    readLines(
      testthat::test_path(
        "..", "..", "scripts",
        "28_materialize_rqr_ordinary_v1_desn_design.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(source, 'design_engine = "exdqlm_reference"', fixed = TRUE)
  expect_match(source, "fit_readout = FALSE", fixed = TRUE)
  expect_match(
    source,
    'getExportedValue("rqrgibbs", "rqr_desn_fit")',
    fixed = TRUE
  )
  expect_false(grepl("rqr_mcmc_fit\\s*\\(", source))
  expect_false(grepl("qdesn_fit_vb\\s*\\(", source))
  expect_match(source, "fits_executed")
  expect_match(source, "response_simulation = FALSE", fixed = TRUE)
})
