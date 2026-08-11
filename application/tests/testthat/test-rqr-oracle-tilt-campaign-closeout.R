testthat::local_edition(3)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", "..", ".."))
source(file.path(
  repo_root, "application", "scripts", "49_oracle_tilt_campaign_gate.R"
))

evidence_root <- file.path(
  repo_root, "docs", "audits",
  "oracle_tilt_c095_v3_nonpromotion_evidence_20260805"
)

testthat::test_that("campaign registry retains historical and current campaigns", {
  registry <- otcg_read_registry(repo_root)
  testthat::expect_invisible(otcg_validate_registry(registry))
  testthat::expect_identical(
    registry$active_manuscript_campaign, "publication_v5"
  )
  testthat::expect_identical(
    registry$active_manuscript_evidence_directory,
    "figures/data/oracle_tilt_c095_v5_exact_delta"
  )
  testthat::expect_true(
    registry$campaigns$publication_v2$manuscript_illustration_evidence_eligible
  )
  testthat::expect_true(
    registry$campaigns$publication_v3$manuscript_illustration_evidence_eligible
  )
  testthat::expect_true(
    registry$campaigns$publication_v5$manuscript_illustration_evidence_eligible
  )
  testthat::expect_false(
    registry$campaigns$publication_v3$original_strict_contract_eligible
  )
  testthat::expect_true(
    registry$campaigns$publication_v3$post_hoc_revision_disclosed
  )
  testthat::expect_false(
    registry$campaigns$publication_v3_dlm_sh_adjudication$
      automatic_promotion_eligible
  )
  bad <- registry
  bad$campaigns$publication_v3$config_sha256 <- paste0(
    "0", substring(bad$campaigns$publication_v3$config_sha256, 2L)
  )
  testthat::expect_error(
    otcg_validate_registry(bad), "registry invariant"
  )
  bad <- registry
  bad$campaigns$publication_v3_dlm_sh_adjudication$
    width_contrast_relative_error <- 0.19
  testthat::expect_error(
    otcg_validate_registry(bad), "adjudication closeout"
  )
})

testthat::test_that("closed campaigns allow lightweight actions only", {
  for (action in c("audit", "render", "test")) {
    testthat::expect_invisible(
      otcg_assert_action(repo_root, "publication_v3", action)
    )
  }
  for (action in c(
    "benchmark", "resource-rehearsal", "acceptance", "execute",
    "adjudication"
  )) {
    testthat::expect_error(
      otcg_assert_action(repo_root, "publication_v3", action),
      "campaign is closed"
    )
  }
  testthat::expect_invisible(otcg_assert_action(
    repo_root, "publication_v3_dlm_sh_adjudication", "audit"
  ))
  testthat::expect_error(
    otcg_assert_action(
      repo_root, "publication_v3_dlm_sh_adjudication", "execute"
    ),
    "campaign is closed"
  )
})

testthat::test_that("version-3 remains reproducible historical evidence", {
  receipt <- jsonlite::read_json(file.path(
    repo_root, "figures", "data", "oracle_tilt_c095_v3",
    "evidence_receipt.json"
  ), simplifyVector = TRUE)
  testthat::expect_identical(
    receipt$baseline_source_commit,
    "99a088fbdd7c3f3ed18f99197294038f62dbfe41"
  )
  testthat::expect_false(receipt$all_cells_original_strict_pass)
  testthat::expect_true(receipt$all_cells_accepted_for_illustration)
  testthat::expect_true(receipt$post_hoc_revision_disclosed)
  testthat::expect_true(receipt$manuscript_illustration_evidence_eligible)
  testthat::expect_equal(receipt$completed_chains, 27L)
  testthat::expect_equal(receipt$original_strict_pass_cells, 5L)
  testthat::expect_equal(receipt$revised_tolerance_accepted_cells, 1L)
  testthat::expect_equal(
    receipt$observed_width_contrast_relative_error, 0.202622544829516
  )
  testthat::expect_gt(
    receipt$observed_width_contrast_relative_error,
    receipt$original_width_contrast_relative_error_max
  )
  testthat::expect_lte(
    receipt$observed_width_contrast_relative_error,
    receipt$revised_width_contrast_relative_error_max
  )

  testthat::expect_false(receipt$response_predictive_analysis)
  testthat::expect_false(receipt$simulation_study)
})

testthat::test_that("promoted compact evidence is hashed and contains no raw objects", {
  root <- file.path(repo_root, "figures", "data", "oracle_tilt_c095_v3")
  manifest <- utils::read.csv(
    file.path(root, "evidence_manifest.csv"), stringsAsFactors = FALSE
  )
  testthat::expect_gt(nrow(manifest), 30L)
  testthat::expect_equal(anyDuplicated(manifest$path), 0L)
  paths <- file.path(root, manifest$path)
  testthat::expect_true(all(file.exists(paths)))
  testthat::expect_equal(as.numeric(file.info(paths)$size), manifest$bytes)
  observed <- vapply(
    paths, digest::digest, character(1L), algo = "sha256", file = TRUE,
    serialize = FALSE
  )
  testthat::expect_identical(unname(observed), manifest$sha256)
  testthat::expect_false(any(grepl(
    "\\.(rds|rda|RData|so|o|tar\\.gz|log)$",
    list.files(root, recursive = TRUE), ignore.case = TRUE
  )))

  fit <- utils::read.csv(
    file.path(root, "fit_summary.csv"), stringsAsFactors = FALSE
  )
  testthat::expect_equal(nrow(fit), 6L)
  testthat::expect_equal(sum(fit$promotion_disposition == "strict_pass"), 5L)
  accepted <- fit[
    fit$promotion_disposition == "accepted_revised_tolerance", , drop = FALSE
  ]
  testthat::expect_identical(accepted$family, "dlm")
  testthat::expect_identical(accepted$target, "SH")
  testthat::expect_identical(accepted$original_disposition, "fail")
  testthat::expect_false(
    accepted$original_manuscript_illustration_evidence_eligible
  )
  testthat::expect_gt(accepted$width_contrast_relative_error, 0.20)
  testthat::expect_lte(accepted$width_contrast_relative_error, 0.21)
})

testthat::test_that("compact non-promotion evidence is complete and immutable", {
  testthat::expect_true(dir.exists(evidence_root))
  manifest_path <- file.path(evidence_root, "artifact_manifest.csv")
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  testthat::expect_gt(nrow(manifest), 20L)
  testthat::expect_equal(anyDuplicated(manifest$path), 0L)
  testthat::expect_false(any(manifest$path == "artifact_manifest.csv"))
  testthat::expect_true(all(file.exists(file.path(evidence_root, manifest$path))))
  observed_bytes <- as.numeric(
    file.info(file.path(evidence_root, manifest$path))$size
  )
  observed_hashes <- vapply(
    file.path(evidence_root, manifest$path),
    digest::digest, character(1L), algo = "sha256", file = TRUE,
    serialize = FALSE
  )
  testthat::expect_equal(observed_bytes, manifest$bytes)
  testthat::expect_identical(unname(observed_hashes), manifest$sha256)

  files <- list.files(evidence_root, recursive = TRUE, all.files = FALSE)
  testthat::expect_false(any(grepl(
    "\\.(rds|rda|RData|so|o|tar\\.gz|log)$", files, ignore.case = TRUE
  )))
  testthat::expect_false(any(grepl(
    "worker_results|chain_[0-9]+\\.rds", files, ignore.case = TRUE
  )))

  receipt <- jsonlite::read_json(
    file.path(evidence_root, "evidence_receipt.json"),
    simplifyVector = TRUE
  )
  testthat::expect_identical(
    receipt$schema_version,
    "rqrgibbs_oracle_tilt_v3_nonpromotion_evidence/1.0.0"
  )
  testthat::expect_equal(receipt$baseline_completed_chains, 27L)
  testthat::expect_equal(receipt$baseline_strict_pass_cells, 5L)
  testthat::expect_equal(receipt$baseline_target_cells, 6L)
  testthat::expect_identical(receipt$failed_cell, "dlm/SH")
  testthat::expect_equal(receipt$adjudication_completed_chains, 5L)
  testthat::expect_equal(receipt$adjudication_retained_draws, 60000L)
  testthat::expect_equal(receipt$adjudication_prefix_checks_passed, 15L)
  testthat::expect_equal(receipt$adjudication_numerical_repairs, 0L)
  testthat::expect_true(receipt$adjudication_strict_diagnostics_pass)
  testthat::expect_false(receipt$adjudication_heterogeneity_pass)
  testthat::expect_gt(
    receipt$adjudication_width_contrast_relative_error,
    receipt$adjudication_maximum_width_contrast_relative_error
  )
  testthat::expect_false(receipt$automatic_promotion_eligible)
  testthat::expect_false(receipt$manuscript_illustration_evidence_eligible)
  testthat::expect_false(receipt$raw_chain_objects_included)
  testthat::expect_false(receipt$response_predictive_analysis)
  testthat::expect_false(receipt$simulation_study)
})

testthat::test_that("compact tables reproduce the final decisions", {
  status <- utils::read.csv(file.path(
    evidence_root, "v3_baseline_run_status.csv"
  ), stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(status), 6L)
  testthat::expect_equal(sum(status$chains_completed), 27L)
  testthat::expect_equal(sum(status$disposition == "strict_pass"), 5L)
  failed <- status[status$disposition == "fail", , drop = FALSE]
  testthat::expect_identical(failed$family, "dlm")
  testthat::expect_identical(failed$target, "SH")

  decision <- utils::read.csv(file.path(
    evidence_root, "adjudication_decision.csv"
  ), stringsAsFactors = FALSE)
  testthat::expect_true(decision$prefix_parity_pass)
  testthat::expect_true(decision$hard_integrity_pass)
  testthat::expect_true(decision$strict_diagnostics_pass)
  testthat::expect_false(decision$heterogeneity_pass)
  testthat::expect_false(decision$strict_pass)
  testthat::expect_false(decision$automatic_promotion_eligible)
  testthat::expect_true(decision$descriptive_review_required)

  diagnostics <- utils::read.csv(file.path(
    evidence_root, "adjudication_mcmc_diagnostics.csv"
  ), stringsAsFactors = FALSE)
  prefix <- utils::read.csv(file.path(
    evidence_root, "adjudication_prefix_parity.csv"
  ), stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(diagnostics), 137L)
  testthat::expect_true(all(diagnostics$pass))
  testthat::expect_equal(nrow(prefix), 15L)
  testthat::expect_true(all(prefix$bitwise_identical))
  testthat::expect_true(all(prefix$maximum_absolute_difference == 0))
})
