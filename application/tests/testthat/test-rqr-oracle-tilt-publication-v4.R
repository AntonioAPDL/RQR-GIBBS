testthat::local_edition(3)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", "..", ".."))
for (file in c(
  "32_oracle_tilt_illustration_utils.R",
  "33_oracle_tilt_forensic_utils.R",
  "34_oracle_tilt_publication_utils.R",
  "42_oracle_tilt_publication_v3_utils.R",
  "52_oracle_tilt_publication_v4_utils.R"
)) source(file.path(repo_root, "application", "scripts", file))

config_path <- file.path(
  repo_root, "application", "config",
  "oracle_tilt_c095_publication_v4_seed_screen_20260805.json"
)
read_config <- function() oti_read_json(config_path)

synthetic_fit_summary <- function(config = read_config()) {
  plan <- otv4_cell_plan(otv4_plan(config))
  quality <- c(candidate_01 = 0.80, candidate_02 = 0.40,
               candidate_03 = 0.60)
  factor <- unname(quality[plan$candidate_id])
  data.frame(
    candidate_id = plan$candidate_id, family = plan$family,
    target = plan$target, computational_pass = TRUE,
    pathology_pass = TRUE, recovery_pass = TRUE,
    heterogeneity_pass = TRUE,
    endpoint_rmse_over_oracle_width = 0.12 * factor,
    mean_width_ratio = 1 + 0.05 * factor,
    lower_bias_over_oracle_width = 0.04 * factor,
    upper_bias_over_oracle_width = -0.03 * factor,
    static_edge_center_rmse_ratio = 1.2 * factor,
    low_scale_endpoint_rmse_over_local_width = 0.12 * factor,
    high_scale_endpoint_rmse_over_local_width = 0.14 * factor,
    width_contrast_relative_error = 0.10 * factor,
    seasonal_width_amplitude_ratio = 1 + 0.10 * factor,
    seasonal_width_phase_error = 0.10 * factor,
    stringsAsFactors = FALSE
  )
}

testthat::test_that("V4 freezes one family-shared candidate screen", {
  config <- read_config()
  testthat::expect_invisible(otv4_validate_config(config))
  testthat::expect_false(config$execution_authorized)
  testthat::expect_identical(otv4_candidate_ids(), sprintf("candidate_%02d", 1:3))
  candidates <- otv4_candidates(config)
  testthat::expect_equal(nrow(candidates), 3L)
  plan <- otv4_plan(config)
  testthat::expect_equal(nrow(plan), 81L)
  testthat::expect_equal(length(unique(plan$seed)), 81L)
  testthat::expect_equal(
    unname(as.integer(table(plan$family))), c(45L, 36L)
  )
  testthat::expect_equal(
    unname(as.integer(table(plan$candidate_id))), rep(27L, 3L)
  )
  testthat::expect_equal(nrow(otv4_cell_plan(plan)), 18L)
  testthat::expect_true(all(plan$learning_rate_mode == "fixed_rate"))
  testthat::expect_true(all(plan$tilt_source == "exact_population_oracle"))
  testthat::expect_false(any(plan$cornish_fisher_used))
  testthat::expect_identical(
    vapply(c("RQR", "ET", "SH"), function(target) {
      as.integer(config$dlm$target_retained_draws[[target]])
    }, integer(1L)), c(RQR = 6000L, ET = 6000L, SH = 12000L)
  )
})

testthat::test_that("V4 config rejects scientific, selection, and resource drift", {
  config <- read_config()
  bad <- config; bad$candidate_contract$candidate_count <- 4L
  testthat::expect_error(otv4_validate_config(bad), "candidate")
  bad <- config; bad$candidate_contract$candidates[[2L]]$master_seed <-
    bad$candidate_contract$candidates[[1L]]$master_seed
  testthat::expect_error(otv4_validate_config(bad), "unique")
  bad <- config; bad$fixed_design$workers <- 2L
  testthat::expect_error(otv4_validate_config(bad), "exactly one")
  bad <- config; bad$dlm$target_retained_draws$SH <- 6000L
  testthat::expect_error(otv4_validate_config(bad), "retained-draw")
  bad <- config; bad$selection$realized_content_in_score <- TRUE
  testthat::expect_error(otv4_validate_config(bad), "selection contract")
  bad <- config; bad$resources$maximum_fit_workers <- 19L
  testthat::expect_error(otv4_validate_config(bad), "resource contract")
  bad <- config; bad$interpretation$simulation_study <- TRUE
  testthat::expect_error(otv4_validate_config(bad), "interpretation")
})

testthat::test_that("named L'Ecuyer streams are unique and preserve caller RNG", {
  config <- read_config()
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(9182)
  before_kind <- RNGkind()
  before_seed <- .Random.seed
  manifest <- otv4_seed_manifest(config)
  testthat::expect_identical(RNGkind(), before_kind)
  testthat::expect_identical(.Random.seed, before_seed)
  testthat::expect_equal(nrow(manifest), 9L)
  testthat::expect_equal(length(unique(manifest$state_digest)), 9L)
  states <- strsplit(manifest$state, ";", fixed = TRUE)
  testthat::expect_true(all(lengths(states) == 7L))
  testthat::expect_identical(manifest, otv4_seed_manifest(config))
})

testthat::test_that("candidate DGPs differ only through declared streams", {
  preflight <- otv4_design_preflight(read_config())
  testthat::expect_true(preflight$pass)
  testthat::expect_true(all(preflight$candidate_gates$pass))
  testthat::expect_true(all(preflight$cross_candidate_gates$pass))
  manifest <- preflight$dgp_manifest
  testthat::expect_equal(length(unique(manifest$fixed_response_digest)), 3L)
  testthat::expect_equal(length(unique(manifest$dlm_response_digest)), 3L)
  testthat::expect_equal(length(unique(manifest$dlm_state_digest)), 3L)
  testthat::expect_equal(length(unique(manifest$fixed_design_digest)), 1L)
  testthat::expect_equal(length(unique(manifest$dlm_design_digest)), 1L)
  for (candidate in preflight$candidates) {
    testthat::expect_equal(length(candidate$fixed_dgp$y), 2400L)
    testthat::expect_equal(length(candidate$dlm_dgp$y), 1200L)
    testthat::expect_equal(sum(!candidate$dlm_dgp$observed), 22L)
  }
})

testthat::test_that("DGP envelopes bind source, config, data, and targets", {
  config <- read_config()
  preflight <- otv4_candidate_preflight(config, "candidate_01")
  commit <- strrep("a", 40L); config_sha <- strrep("b", 64L)
  envelope <- otv4_dgp_envelope(preflight, "fixed_design", commit, config_sha)
  testthat::expect_invisible(otv4_validate_dgp_envelope(
    envelope, "candidate_01", "fixed_design", commit, config_sha
  ))
  bad <- envelope; bad$dgp$y[1L] <- bad$dgp$y[1L] + 1
  testthat::expect_error(
    otv4_validate_dgp_envelope(
      bad, "candidate_01", "fixed_design", commit, config_sha
    ), "invalid"
  )
  bad <- envelope; bad$targets$oracle_lower[1L] <-
    bad$targets$oracle_lower[1L] + 1
  testthat::expect_error(
    otv4_validate_dgp_envelope(
      bad, "candidate_01", "fixed_design", commit, config_sha
    ), "invalid"
  )
})

testthat::test_that("selection is deterministic, family-shared, and order invariant", {
  config <- read_config()
  summary <- synthetic_fit_summary(config)
  selection <- otv4_select_candidates(summary, config)
  testthat::expect_true(selection$complete)
  testthat::expect_equal(nrow(selection$selected), 2L)
  testthat::expect_identical(
    selection$selected$selected_candidate_id, rep("candidate_02", 2L)
  )
  testthat::expect_false(selection$realized_content_used)
  testthat::expect_false(selection$aesthetic_judgment_used)
  shuffled <- summary[sample.int(nrow(summary)), , drop = FALSE]
  repeated <- otv4_select_candidates(shuffled, config)
  testthat::expect_equal(selection$selected, repeated$selected)
  testthat::expect_equal(selection$family_ranking, repeated$family_ranking)
})

testthat::test_that("ineligible cells disqualify the whole family candidate", {
  config <- read_config()
  summary <- synthetic_fit_summary(config)
  selected <- summary$candidate_id == "candidate_02" &
    summary$family == "dlm" & summary$target == "SH"
  summary$computational_pass[selected] <- FALSE
  selection <- otv4_select_candidates(summary, config)
  chosen <- setNames(
    selection$selected$selected_candidate_id, selection$selected$family
  )
  testthat::expect_identical(unname(chosen[["fixed_design"]]), "candidate_02")
  testthat::expect_identical(unname(chosen[["dlm"]]), "candidate_03")
  audit <- selection$cell_audit
  testthat::expect_false(audit$selection_eligible[
    audit$candidate_id == "candidate_02" & audit$family == "dlm" &
      audit$target == "SH"
  ])
})

testthat::test_that("production execution and launch stay fail-closed", {
  config <- read_config()
  testthat::expect_false(config$execution_authorized)
  runner <- readLines(file.path(
    repo_root, "application", "scripts",
    "52_run_oracle_tilt_publication_v4.R"
  ), warn = FALSE)
  launcher <- readLines(file.path(
    repo_root, "application", "scripts",
    "55_launch_oracle_tilt_v4_overnight.sh"
  ), warn = FALSE)
  testthat::expect_true(any(grepl(
    "production execution is disabled", runner, fixed = TRUE
  )))
  testthat::expect_true(any(grepl(
    "execution_authorized", launcher, fixed = TRUE
  )))
  testthat::expect_true(any(grepl(
    "automatic_manuscript_promotion = FALSE", runner, fixed = TRUE
  )))
})
