testthat::local_edition(3)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", "..", ".."))
source(file.path(
  repo_root, "application", "scripts", "49_oracle_tilt_campaign_gate.R"
))
evidence_root <- file.path(
  repo_root, "figures", "data", "oracle_tilt_c095_v5_exact_delta"
)

testthat::test_that("version-5 is the active closed illustration campaign", {
  registry <- otcg_read_registry(repo_root)
  testthat::expect_identical(
    registry$active_manuscript_campaign, "publication_v5"
  )
  testthat::expect_identical(
    registry$active_manuscript_evidence_directory,
    "figures/data/oracle_tilt_c095_v5_exact_delta"
  )
  testthat::expect_invisible(
    otcg_assert_action(repo_root, "publication_v5", "render")
  )
  testthat::expect_error(
    otcg_assert_action(repo_root, "publication_v5", "execute"),
    "campaign is closed"
  )

  bad <- registry
  bad$campaigns$publication_v5$strict_diagnostic_thresholds_relabelled <- TRUE
  testthat::expect_error(
    otcg_validate_registry(bad), "version-5 diagnostic-aware closeout"
  )
  bad <- registry
  bad$campaigns$publication_v5$diagnostic_warning_cell <- "fixed_design/SH"
  testthat::expect_error(
    otcg_validate_registry(bad), "version-5 diagnostic-aware closeout"
  )
})

testthat::test_that("tracked version-5 evidence is complete and immutable", {
  testthat::expect_true(dir.exists(evidence_root))
  manifest <- utils::read.csv(
    file.path(evidence_root, "evidence_manifest.csv"),
    stringsAsFactors = FALSE
  )
  testthat::expect_gt(nrow(manifest), 40L)
  testthat::expect_equal(anyDuplicated(manifest$path), 0L)
  paths <- file.path(evidence_root, manifest$path)
  testthat::expect_true(all(file.exists(paths)))
  testthat::expect_equal(as.numeric(file.info(paths)$size), manifest$bytes)
  observed <- vapply(
    paths, digest::digest, character(1L), algo = "sha256", file = TRUE,
    serialize = FALSE
  )
  testthat::expect_identical(unname(observed), manifest$sha256)
  testthat::expect_false(any(grepl(
    "\\.(rds|rda|RData|so|o|tar\\.gz|log)$",
    list.files(evidence_root, recursive = TRUE), ignore.case = TRUE
  )))

  receipt <- jsonlite::read_json(
    file.path(evidence_root, "evidence_receipt.json"), simplifyVector = TRUE
  )
  testthat::expect_identical(
    receipt$schema_version, "rqrgibbs_oracle_tilt_evidence/5.1.0"
  )
  testthat::expect_identical(
    receipt$source_commit,
    "24065941c44a836d2f385b9fe4cf28fcd18d08bd"
  )
  testthat::expect_identical(
    receipt$config_sha256,
    "e0a603d05e01aecc8f6402d3303d90f62de20b4cdee1fa69f7118419b438f893"
  )
  testthat::expect_identical(
    receipt$runtime_tree_digest,
    "20ca720b6d0874b11cdab342fcdfddd9be3c271fe81c5724ea6c3ca43a9c3614"
  )
  testthat::expect_equal(receipt$target_cells, 6L)
  testthat::expect_equal(receipt$completed_chains, 27L)
  testthat::expect_equal(receipt$strict_pass_cells, 5L)
  testthat::expect_equal(receipt$diagnostic_warning_cells, 1L)
  testthat::expect_true(receipt$all_cells_hard_computational_pass)
  testthat::expect_false(receipt$all_cells_strict_pass)
  testthat::expect_false(receipt$strict_diagnostic_thresholds_relabelled)
  testthat::expect_false(receipt$reseeded_or_selectively_extended)
  testthat::expect_true(receipt$exact_population_oracle_tilts)
  testthat::expect_false(receipt$cornish_fisher_used)
  testthat::expect_false(receipt$response_predictive_analysis)
  testthat::expect_false(receipt$simulation_study)
})

testthat::test_that("version-5 cells retain the prospective diagnostics", {
  fit <- utils::read.csv(
    file.path(evidence_root, "fit_summary.csv"), stringsAsFactors = FALSE
  )
  diagnostic <- utils::read.csv(
    file.path(evidence_root, "mcmc_diagnostics.csv"),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(nrow(fit), 6L)
  testthat::expect_true(all(fit$hard_computational_pass))
  testthat::expect_true(all(fit$broad_recovery_pass))
  testthat::expect_true(all(fit$broad_heterogeneity_pass))
  testthat::expect_true(all(fit$manuscript_illustration_evidence_eligible))
  testthat::expect_equal(sum(fit$disposition == "strict_pass"), 5L)
  warning <- fit[fit$disposition == "diagnostic_aware_pass", , drop = FALSE]
  testthat::expect_equal(nrow(warning), 1L)
  testthat::expect_identical(warning$family, "dlm")
  testthat::expect_identical(warning$target, "SH")
  testthat::expect_equal(warning$diagnostic_warning_count, 5L)
  testthat::expect_equal(warning$n_chains, 5L)
  testthat::expect_equal(warning$retained_draws, 30000L)
  testthat::expect_equal(warning$numerical_repair_count, 0L)

  failed <- diagnostic[
    diagnostic$family == "dlm" & diagnostic$target == "SH" &
      !diagnostic$pass, , drop = FALSE
  ]
  testthat::expect_equal(nrow(failed), 5L)
  testthat::expect_true(all(failed$rhat <= 1.01))
  testthat::expect_true(all(failed$ess_tail >= 1000))
  testthat::expect_true(all(failed$mcse_over_sd <= 0.05))
  testthat::expect_true(all(failed$ess_bulk < 1000))
  testthat::expect_true(all(diagnostic$pass[
    diagnostic$family != "dlm" | diagnostic$target != "SH"
  ]))
})

testthat::test_that("population tilts use the corrected retained-mean map", {
  oracle <- utils::read.csv(
    file.path(evidence_root, "oracle_targets.csv"), stringsAsFactors = FALSE
  )
  oracle <- oracle[match(c("RQR", "ET", "SH"), oracle$target), ]
  testthat::expect_equal(
    oracle$delta_innovation,
    c(0, 0.0560608464325982, 0.11472186306441), tolerance = 1e-14
  )
  testthat::expect_true(all(
    oracle$tilt_definition == "conditional_retained_mean_minus_population_mean"
  ))
  testthat::expect_true(all(oracle$unique_minimizer))
  testthat::expect_false(any(oracle$uses_cornish_fisher))
  testthat::expect_equal(oracle$content_residual, rep(0, 3), tolerance = 1e-13)
  testthat::expect_equal(
    oracle$retained_mean_residual, rep(0, 3), tolerance = 1e-13
  )
})
