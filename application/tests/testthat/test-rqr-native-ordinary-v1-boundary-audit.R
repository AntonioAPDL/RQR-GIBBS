ordinary_v1_boundary_audit_environment <- function() {
  path <- testthat::test_path(
    "..", "..", "scripts",
    "29_audit_rqr_ordinary_v1_dlm_boundary.R"
  )
  old <- Sys.getenv(
    "RQR_DLM_BOUNDARY_AUDIT_SOURCE_ONLY", unset = NA_character_
  )
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("RQR_DLM_BOUNDARY_AUDIT_SOURCE_ONLY")
    } else {
      Sys.setenv(RQR_DLM_BOUNDARY_AUDIT_SOURCE_ONLY = old)
    }
  }, add = TRUE)
  Sys.setenv(RQR_DLM_BOUNDARY_AUDIT_SOURCE_ONLY = "YES")
  environment <- new.env(parent = globalenv())
  sys.source(path, envir = environment)
  environment
}

ordinary_v1_boundary_audit_repo <- function() {
  normalizePath(
    testthat::test_path("..", "..", ".."),
    winslash = "/", mustWork = TRUE
  )
}

test_that("protected-DLM inventory and public boundary are explicit", {
  audit <- ordinary_v1_boundary_audit_environment()
  repo_root <- ordinary_v1_boundary_audit_repo()

  expect_identical(
    length(audit$rqr_dlm_boundary_source_runner(repo_root)), 29L
  )
  expect_identical(
    length(audit$rqr_dlm_boundary_public_functions()), 18L
  )
  expect_true(all(c(
    "application/DESCRIPTION", "application/NAMESPACE",
    "application/R/RcppExports.R", "application/src/RcppExports.cpp",
    "application/src/rqr_interweave.cpp",
    "application/src/Makevars", "application/src/Makevars.win",
    "application/scripts/22_validate_rqr_dlm_wave1_corrections.R",
    "application/scripts/23_validate_rqr_dlm_wave1_comparator_projection.R",
    "application/scripts/24_validate_rqr_dlm_horizon_and_fixed_design.R",
    "application/scripts/25_validate_rqr_dlm_resource_envelope.R",
    "application/scripts/lib/rqr_dlm_confirmatory_simulation.R",
    "docs/audits/rqr_dlm_main_correction_budget_20260727.csv"
  ) %in% audit$rqr_dlm_boundary_source_runner(repo_root)))
  expect_true(all(c(
    "rqr_dlm_fit", "rqr_dlm_continue", "rqr_forecast_roots",
    "rqr_ffbs_sample", "rqr_evolution_component_scale"
  ) %in% audit$rqr_dlm_boundary_public_functions()))
})

test_that("development audit emits complete compact nonpromotion evidence", {
  testthat::skip_if_not(nzchar(Sys.which("git")))
  audit <- ordinary_v1_boundary_audit_environment()
  repo_root <- ordinary_v1_boundary_audit_repo()
  testthat::skip_if_not(dir.exists(file.path(repo_root, ".git")))
  head <- audit$rqr_dlm_boundary_resolve_commit(repo_root, "HEAD")
  output_dir <- withr::local_tempdir(
    pattern = "ordinary-v1-dlm-boundary-audit-"
  )

  result <- audit$rqr_dlm_boundary_audit(
    repo_root = repo_root,
    baseline = head,
    candidate = "WORKTREE",
    output_dir = output_dir,
    promotion = FALSE
  )

  expect_identical(nrow(result$tables$protected_file_comparison.csv), 29L)
  expect_true(all(
    result$tables$protected_file_comparison.csv$candidate_exists
  ))
  expect_identical(nrow(result$tables$public_function_comparison.csv), 18L)
  expect_true(all(
    result$tables$public_function_comparison.csv$candidate_exists
  ))
  expect_identical(
    result$metadata$value[
      result$metadata$field == "evidence_role"
    ],
    "development_nonpromotion_evidence"
  )
  expect_identical(
    result$metadata$value[
      result$metadata$field == "statistical_validation_performed"
    ],
    "FALSE"
  )
  expected_files <- c(
    "artifact_hashes.csv", "audit_metadata.csv", "boundary_summary.csv",
    "compiled_registration_comparison.csv",
    "confirmatory_authorization_comparison.csv",
    "namespace_registration_comparison.csv",
    "package_metadata_comparison.csv",
    "protected_file_comparison.csv", "public_function_comparison.csv",
    "schema_string_comparison.csv"
  )
  expect_setequal(list.files(output_dir), expected_files)
  manifest <- utils::read.csv(
    file.path(output_dir, "artifact_hashes.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_setequal(
    manifest$relative_path,
    setdiff(expected_files, "artifact_hashes.csv")
  )
  for (index in seq_len(nrow(manifest))) {
    path <- file.path(output_dir, manifest$relative_path[[index]])
    expect_identical(
      audit$rqr_dlm_boundary_sha256_file(path),
      manifest$sha256[[index]]
    )
    expect_identical(
      as.numeric(file.info(path)$size),
      as.numeric(manifest$byte_count[[index]])
    )
  }
  expect_true(any(
    result$tables$compiled_registration_comparison.csv$symbol ==
      "_rqrgibbs_rqr_ffbs_cpp"
  ))
  expect_setequal(
    result$tables$compiled_registration_comparison.csv$registration_kind[
      result$tables$compiled_registration_comparison.csv$symbol ==
        "_rqrgibbs_rqr_ffbs_cpp"
    ],
    c("CallEntries", "R_wrapper_Call")
  )
  expect_true(any(
    result$tables$namespace_registration_comparison.csv$directive ==
      "export(rqr_dlm_fit)"
  ))
})

test_that("WORKTREE candidate is rejected for promotion evidence", {
  testthat::skip_if_not(nzchar(Sys.which("git")))
  audit <- ordinary_v1_boundary_audit_environment()
  repo_root <- ordinary_v1_boundary_audit_repo()
  testthat::skip_if_not(dir.exists(file.path(repo_root, ".git")))
  head <- audit$rqr_dlm_boundary_resolve_commit(repo_root, "HEAD")

  expect_error(
    audit$rqr_dlm_boundary_audit(
      repo_root = repo_root,
      baseline = head,
      candidate = "WORKTREE",
      output_dir = withr::local_tempdir(
        pattern = "ordinary-v1-dlm-boundary-promotion-reject-"
      ),
      promotion = TRUE
    ),
    "never promotion evidence"
  )
})

test_that("promotion guard requires a clean strict committed lineage", {
  testthat::skip_if_not(nzchar(Sys.which("git")))
  audit <- ordinary_v1_boundary_audit_environment()
  repository <- withr::local_tempdir(
    pattern = "ordinary-v1-dlm-boundary-lineage-"
  )
  expect_identical(
    audit$rqr_dlm_boundary_git(
      repository, c("init", "--quiet", "-b", "main")
    )$status,
    0L
  )
  audit$rqr_dlm_boundary_git(
    repository, c("config", "user.name", "Boundary Test")
  )
  audit$rqr_dlm_boundary_git(
    repository,
    c("config", "user.email", "boundary-test@example.invalid")
  )
  audit$rqr_dlm_boundary_git(
    repository, c("config", "commit.gpgsign", "false")
  )
  writeLines("baseline", file.path(repository, "fixture.txt"))
  audit$rqr_dlm_boundary_git(repository, c("add", "fixture.txt"))
  audit$rqr_dlm_boundary_git(
    repository, c("commit", "--quiet", "-m", "baseline")
  )
  baseline <- audit$rqr_dlm_boundary_resolve_commit(repository, "HEAD")
  writeLines("candidate", file.path(repository, "fixture.txt"))
  audit$rqr_dlm_boundary_git(repository, c("add", "fixture.txt"))
  audit$rqr_dlm_boundary_git(
    repository, c("commit", "--quiet", "-m", "candidate")
  )
  candidate <- audit$rqr_dlm_boundary_resolve_commit(repository, "HEAD")

  state <- audit$rqr_dlm_boundary_validate_promotion(
    repository, baseline, baseline, candidate, candidate, TRUE
  )
  expect_true(state$source_clean)
  expect_true(state$baseline_is_ancestor)
  expect_true(state$baseline_is_strict_ancestor)
  expect_true(state$full_sha_inputs)

  writeLines("dirty", file.path(repository, "fixture.txt"))
  expect_error(
    audit$rqr_dlm_boundary_validate_promotion(
      repository, baseline, baseline, candidate, candidate, TRUE
    ),
    "clean"
  )
  expect_error(
    audit$rqr_dlm_boundary_validate_promotion(
      repository, candidate, candidate, candidate, candidate, TRUE
    ),
    "strict ancestral"
  )
})

test_that("boundary artifacts reject symlink destinations", {
  audit <- ordinary_v1_boundary_audit_environment()
  directory <- withr::local_tempdir(
    pattern = "ordinary-v1-dlm-boundary-symlink-"
  )
  target <- file.path(directory, "target.csv")
  link <- file.path(directory, "artifact.csv")
  writeLines("sentinel", target)
  linked <- file.symlink(target, link)
  testthat::skip_if_not(isTRUE(linked))

  expect_error(
    audit$rqr_dlm_boundary_atomic_csv(
      data.frame(value = 1L),
      link
    ),
    "symlink or nonregular"
  )
  expect_identical(readLines(target, warn = FALSE), "sentinel")
})
