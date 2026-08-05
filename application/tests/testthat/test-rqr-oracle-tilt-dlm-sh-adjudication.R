testthat::local_edition(3)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", "..", ".."))
script_dir <- file.path(repo_root, "application", "scripts")
for (file in c(
  "32_oracle_tilt_illustration_utils.R",
  "33_oracle_tilt_forensic_utils.R",
  "34_oracle_tilt_publication_utils.R",
  "42_oracle_tilt_publication_v3_utils.R",
  "46_oracle_tilt_dlm_sh_adjudication_utils.R"
)) source(file.path(script_dir, file))

testthat::test_that("DLM/SH adjudication configuration is frozen", {
  config <- oti_read_json(file.path(
    repo_root, "application", "config",
    "oracle_tilt_c095_dlm_sh_adjudication_recovery_20260805.json"
  ))
  testthat::expect_silent(otad_validate_config(config, repo_root))
  testthat::expect_identical(config$execution_attempt, 2L)
  testthat::expect_identical(config$statistical_attempt, 1L)
  testthat::expect_false(
    config$software_recovery_contract$failed_execution_is_statistical_attempt
  )
  testthat::expect_false(
    config$software_recovery_contract$scientific_contract_changed
  )
  bad <- config
  bad$mcmc_override$n_mcmc <- 12001
  testthat::expect_error(
    otad_validate_config(bad, repo_root), "MCMC extension contract"
  )
  bad <- config
  bad$decision_contract$no_gate_relaxation <- FALSE
  testthat::expect_error(
    otad_validate_config(bad, repo_root), "decision contract"
  )
  bad <- config
  bad$software_recovery_contract$invalidated_worker_artifact_count <- 1L
  testthat::expect_error(
    otad_validate_config(bad, repo_root), "software-recovery evidence"
  )
  bad <- config
  bad$staging_contract$remaining_chains <- c(2L, 3L, 5L, 4L)
  testthat::expect_error(
    otad_validate_config(bad, repo_root), "staged software-recovery"
  )
  historical <- oti_read_json(file.path(
    repo_root, "application", "config",
    "oracle_tilt_c095_dlm_sh_adjudication_20260805.json"
  ))
  testthat::expect_error(
    otad_validate_config(historical, repo_root), "Unsupported"
  )
})

testthat::test_that("production-shaped adjudication workers validate", {
  contract <- list(
    source_commit = strrep("a", 40L),
    runtime_tree_digest = strrep("b", 64L),
    family = "dlm", target = "SH", chain = 1L,
    mcmc_override = list(n_mcmc = 12000L)
  )
  gates <- otad_worker_contract_self_test(contract)
  testthat::expect_identical(
    gates$gate,
    c(
      "production_shaped_worker_acceptance",
      "forbidden_prediction_payload_rejection"
    )
  )
  testthat::expect_true(all(gates$pass))
})

testthat::test_that("singleton and parallel worker errors share one contract", {
  successful <- otad_run_batches(
    1:5, 2L, function(chain) list(chain = chain), "test"
  )
  testthat::expect_identical(
    vapply(successful, `[[`, integer(1L), "chain"), 1:5
  )
  singleton_failure <- otad_run_batches(
    1:5, 2L, function(chain) {
      if (chain == 5L) stop("singleton failure")
      list(chain = chain)
    }, "test"
  )
  testthat::expect_s3_class(singleton_failure[[5L]], "otad_worker_error")
  testthat::expect_identical(singleton_failure[[5L]]$chain, 5L)
  testthat::expect_identical(singleton_failure[[5L]]$stage, "test")
  testthat::expect_match(
    singleton_failure[[5L]]$message, "singleton failure"
  )
  parallel_failure <- otad_run_batches(
    1:2, 2L, function(chain) {
      if (chain == 2L) stop("parallel failure")
      list(chain = chain)
    }, "test"
  )
  testthat::expect_s3_class(parallel_failure[[2L]], "otad_worker_error")
  testthat::expect_identical(parallel_failure[[2L]]$chain, 2L)
})

testthat::test_that("prefix parity is bitwise and mutation sensitive", {
  baseline_matrix <- matrix(seq_len(18), 3, 6)
  scalar <- cbind(a = seq_len(6), b = seq_len(6) / 10)
  baseline <- list(result = list(
    pred = list(lower_draws = baseline_matrix,
                upper_draws = baseline_matrix + 1),
    scalar_draws = scalar
  ))
  extended <- list(result = list(
    pred = list(
      lower_draws = cbind(baseline_matrix, baseline_matrix),
      upper_draws = cbind(baseline_matrix + 1, baseline_matrix + 1)
    ),
    scalar_draws = rbind(scalar, scalar)
  ))
  parity <- otad_prefix_parity(baseline, extended, 1L, 6L)
  testthat::expect_equal(nrow(parity), 3L)
  testthat::expect_true(all(parity$pass))
  extended$result$scalar_draws[2L, 1L] <- -1
  parity <- otad_prefix_parity(baseline, extended, 1L, 6L)
  testthat::expect_false(parity$pass[parity$object == "scalar_draws"])
})

testthat::test_that("adjudication decisions never promote non-strict cells", {
  prefix <- data.frame(pass = rep(TRUE, 15L))
  summary <- data.frame(
    manuscript_illustration_evidence_eligible = TRUE,
    disposition = "strict_pass", provenance_pass = TRUE,
    provenance_snapshots_pass = TRUE, conditional_parity_pass = TRUE,
    pathology_pass = TRUE, recovery_pass = TRUE,
    numerical_repair_count = 0L, strict_diagnostics_pass = TRUE,
    heterogeneity_pass = TRUE
  )
  decision <- otad_decision(list(fit_summary = summary), prefix)
  testthat::expect_true(decision$strict_pass)
  summary$heterogeneity_pass <- FALSE
  summary$manuscript_illustration_evidence_eligible <- FALSE
  summary$disposition <- "fail"
  decision <- otad_decision(list(fit_summary = summary), prefix)
  testthat::expect_false(decision$automatic_promotion_eligible)
  testthat::expect_true(decision$descriptive_review_required)
  testthat::expect_identical(
    decision$disposition, "descriptive_review_required"
  )
})
