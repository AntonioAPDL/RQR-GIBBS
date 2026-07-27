# Frozen, prospective validation contract for ordinary (zero-tilt) RQR.
#
# This configuration describes target/computation validation only.  It does
# not define a response likelihood or response-simulation distribution, does
# not authorize a matched simulation, and deliberately leaves the 48-fit
# bounded diagnostic grid disabled.

.rqr_ordinary_v1_chain_rows <- function(
    cell_id, family, fixture_id, prior_id, learning_rate_mode, seeds) {
  data.frame(
    cell_id = rep(cell_id, length(seeds)),
    family = rep(family, length(seeds)),
    fixture_id = rep(fixture_id, length(seeds)),
    prior_id = rep(prior_id, length(seeds)),
    learning_rate_mode = rep(learning_rate_mode, length(seeds)),
    chain = seq_along(seeds),
    seed = as.integer(seeds),
    stringsAsFactors = FALSE
  )
}

.rqr_ordinary_v1_fit_plan <- do.call(rbind, list(
  .rqr_ordinary_v1_chain_rows(
    "S01", "fixed_design", "F03", "ridge", "fixed_rate", 82611:82614
  ),
  .rqr_ordinary_v1_chain_rows(
    "S02", "fixed_design", "F03", "ridge",
    "learned_pseudoresidual_normalized", 82621:82624
  ),
  .rqr_ordinary_v1_chain_rows(
    "S03", "fixed_design", "F02", "gaussian", "fixed_rate", 82631:82634
  ),
  .rqr_ordinary_v1_chain_rows(
    "S04", "fixed_design", "F02", "gaussian",
    "learned_pseudoresidual_normalized", 82641:82644
  ),
  .rqr_ordinary_v1_chain_rows(
    "S05", "fixed_design", "F04", "rhs_ns_sampled", "fixed_rate",
    82651:82654
  ),
  .rqr_ordinary_v1_chain_rows(
    "S06", "fixed_design", "F04", "rhs_ns_sampled",
    "learned_pseudoresidual_normalized", 82661:82664
  ),
  .rqr_ordinary_v1_chain_rows(
    "S07", "fixed_design", "F05", "rhs_ns_fixed", "fixed_rate",
    82671:82674
  ),
  .rqr_ordinary_v1_chain_rows(
    "S08", "fixed_design", "F05", "rhs_ns_fixed",
    "learned_pseudoresidual_normalized", 82681:82684
  ),
  .rqr_ordinary_v1_chain_rows(
    "DESN01", "desn", "D02", "ridge", "fixed_rate", 82711:82714
  ),
  .rqr_ordinary_v1_chain_rows(
    "DESN02", "desn", "D02", "ridge",
    "learned_pseudoresidual_normalized", 82731:82734
  ),
  .rqr_ordinary_v1_chain_rows(
    "DESN03", "desn", "D02", "rhs_ns_fixed", "fixed_rate",
    82721:82724
  ),
  .rqr_ordinary_v1_chain_rows(
    "DESN04", "desn", "D02", "rhs_ns_fixed",
    "learned_pseudoresidual_normalized", 82741:82744
  )
))
rownames(.rqr_ordinary_v1_fit_plan) <- NULL

.rqr_ordinary_v1_benchmark_plan <- .rqr_ordinary_v1_chain_rows(
  "BENCH01", "desn", "D02", "rhs_ns_fixed",
  "learned_pseudoresidual_normalized", 82961:82964
)

.rqr_ordinary_v1_seed_ledger <- rbind(
  transform(
    .rqr_ordinary_v1_fit_plan,
    seed_id = sprintf("bounded_%s_chain_%02d", cell_id, chain),
    stage = "execute-bounded",
    purpose = "bounded_chain"
  )[, c(
    "seed_id", "stage", "purpose", "fixture_id", "prior_id",
    "learning_rate_mode", "chain", "seed"
  )],
  transform(
    .rqr_ordinary_v1_benchmark_plan,
    seed_id = sprintf("benchmark_%s_chain_%02d", cell_id, chain),
    stage = "benchmark-one-cell",
    purpose = "benchmark_chain"
  )[, c(
    "seed_id", "stage", "purpose", "fixture_id", "prior_id",
    "learning_rate_mode", "chain", "seed"
  )],
  data.frame(
    seed_id = c(
      "static_fixture", "desn_materialization",
      sprintf("static_continuation_%02d", 1:8),
      sprintf("desn_continuation_%02d", 1:4),
      "future_design_selection", "rhs_reference_parity"
    ),
    stage = c(
      "reference-only", "reference-only",
      rep("reference-only", 8), rep("reference-only", 4),
      "reference-only", "reference-only"
    ),
    purpose = c(
      "static_fixture", "desn_materialization",
      rep("static_continuation", 8), rep("desn_continuation", 4),
      "future_design_selection", "rhs_reference_parity"
    ),
    fixture_id = c(
      "F02", "D02", rep(c("F02", "F03", "F04", "F05"), each = 2),
      rep("D01", 4), "D03", "F04"
    ),
    prior_id = c(
      NA_character_, NA_character_,
      rep(c("gaussian", "ridge", "rhs_ns_sampled", "rhs_ns_fixed"), each = 2),
      rep(c("ridge", "rhs_ns_fixed"), each = 2),
      NA_character_, "rhs_ns_sampled"
    ),
    learning_rate_mode = c(
      NA_character_, NA_character_,
      rep(c("fixed_rate", "learned_pseudoresidual_normalized"), 4),
      rep(c("fixed_rate", "learned_pseudoresidual_normalized"), 2),
      NA_character_, NA_character_
    ),
    chain = c(
      NA_integer_, NA_integer_, rep(NA_integer_, 8), rep(NA_integer_, 4),
      NA_integer_, NA_integer_
    ),
    seed = as.integer(c(
      82601, 82701, 82801:82808, 82821:82824, 82901, 82921
    )),
    stringsAsFactors = FALSE
  ),
  data.frame(
    seed_id = sprintf("attested_desn_reference_%02d", 1:4),
    stage = rep("reference-only", 4),
    purpose = rep("attested_desn_end_to_end", 4),
    fixture_id = rep("D02", 4),
    prior_id = rep(c("ridge", "rhs_ns_fixed"), each = 2),
    learning_rate_mode = rep(
      c("fixed_rate", "learned_pseudoresidual_normalized"), 2
    ),
    chain = rep(NA_integer_, 4),
    seed = as.integer(82831:82834),
    stringsAsFactors = FALSE
  ),
  data.frame(
    seed_id = sprintf("f01_sampler_quadrature_chain_%02d", 1:4),
    stage = rep("reference-only", 4),
    purpose = rep("f01_sampler_quadrature", 4),
    fixture_id = rep("F01", 4),
    prior_id = rep("ridge", 4),
    learning_rate_mode = rep(
      "learned_pseudoresidual_normalized", 4
    ),
    chain = 1:4,
    seed = as.integer(82931:82934),
    stringsAsFactors = FALSE
  ),
  data.frame(
    seed_id = c(
      "loss_sign_partition", "pseudo_al_kernel", "gig_parameters",
      "lambda_power", "gaussian_conditional", "protected_dlm_hashes"
    ),
    stage = rep("reference-only", 6),
    purpose = c(
      "deterministic_oracle", "deterministic_oracle",
      "deterministic_oracle", "deterministic_oracle",
      "deterministic_oracle", "deterministic_oracle"
    ),
    fixture_id = c("F01", "F01", "F01", "F01", "F02", NA_character_),
    prior_id = c(NA_character_, NA_character_, NA_character_, NA_character_,
                 "gaussian", NA_character_),
    learning_rate_mode = rep(NA_character_, 6),
    chain = rep(NA_integer_, 6),
    seed = rep(NA_integer_, 6),
    stringsAsFactors = FALSE
  )
)
rownames(.rqr_ordinary_v1_seed_ledger) <- NULL

x_f02 <- seq(-1, 1, length.out = 9)
z_f02 <- c(-1, 1, 0, -0.5, 0.5, 1.5, -1.5, 0.25, -0.25)
X_f02 <- cbind(intercept = 1, x = x_f02, z = z_f02)
y_f02_complete <- 0.4 - 0.7 * x_f02 + 0.25 * z_f02 +
  c(-0.15, 0.05, 0.10, -0.05, 0, 0.08, -0.12, 0.03, 0.06)
y_f02 <- y_f02_complete
y_f02[c(3L, 7L)] <- NA_real_

x1_f04 <- seq(-1, 1, length.out = 10)
x2_f04 <- cos(seq_len(10) / 3)
x3_f04 <- sin(seq_len(10) / 4)
X_f04 <- cbind(intercept = 1, x1 = x1_f04, x2 = x2_f04, x3 = x3_f04)
y_f04 <- 0.2 + 0.6 * x1_f04 - 0.25 * x2_f04 + 0.1 * x3_f04
y_f04[c(4L, 8L)] <- NA_real_

n_d01 <- 12L
X_d01 <- cbind(
  intercept = 1,
  h_last_001 = seq(-0.9, 0.9, length.out = n_d01),
  reduced_h_001 = sin(seq_len(n_d01) / 3)
)
y_d01 <- 0.3 + 0.4 * X_d01[, "h_last_001"] -
  0.2 * X_d01[, "reduced_h_001"] +
  0.05 * cos(seq_len(n_d01))
y_d01[c(3L, 9L)] <- NA_real_
X_d01_future <- cbind(
  intercept = 1,
  h_last_001 = c(1.0, 1.1),
  reduced_h_001 = c(-0.3, -0.2)
)
y_d02 <- 0.35 * sin(seq_len(48L) / 4) +
  0.20 * cos(seq_len(48L) / 9) +
  0.01 * seq_len(48L)

rqr_ordinary_v1_bounded_validation <- list(
  schema_version = "rqrgibbs_ordinary_v1_validation/1.0.0",
  config_id = "rqr_ordinary_v1_bounded_validation_20260726",
  scope = "ordinary_zero_tilt_target_and_computation_validation",
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  matched_simulation_authorized = FALSE,
  ordinary_v1_execute_enabled = FALSE,
  reviewed_implementation_commit = NA_character_,
  runner_modes = c(
    "preflight", "reference-only", "benchmark-one-cell",
    "execute-bounded"
  ),
  accepted_learning_rate_modes = c(
    "fixed_rate", "learned_pseudoresidual_normalized"
  ),
  rejected_promotion_modes = c(
    "learned_pure", "nonzero_tilt", "vb", "adaptive_conditional_discount",
    "response_simulation"
  ),
  coverage_level = 0.80,
  fixed_learning_rate = 1,
  loss_reference_scale = 1,
  lambda_prior = list(shape = 4, rate = 4),
  pinned_exdqlm = list(
    branch = "feature/rqr-desn-readout-20260716",
    commit = "dffb71ee70b597d6a716ee74be1cbc99731cd453",
    checkout_role = "read_only_reference_never_loaded_or_built_in_place"
  ),
  desn_schema_contract = c(
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
  ),
  f01_reference_contract = list(
    schema_version =
      "rqrgibbs_ordinary_v1_f01_quadrature/1.0.0",
    generator_path =
      "application/scripts/30_generate_ordinary_v1_f01_references.R",
    generator_sha256 =
      "e7ca4f6e4976181278d5d517208079b298a167439f9c011617ad1ae6091c510d",
    artifact_path = paste0(
      "application/inst/extdata/",
      "ordinary_v1_f01_quadrature_references.csv"
    ),
    artifact_sha256 =
      "d791576df8836a2fde2e59bc9be19bafac390abead8d24b5319b23de73a12459",
    mean_source_path =
      paste0(
        "application/inst/extdata/",
        "ordinary_v1_f01_independent_mean_references.csv"
      ),
    mean_source_sha256 =
      "6eb163ceb7838c215774fe0a47e7f83c6a79bf1cc2091762ccdb211eeb097613",
    mean_provenance_sha256 =
      "413148d9da9cd15ba9714edbe350762e915874eee27cfeb9c9b280e3ac585b9a",
    cdf_source_path =
      "application/inst/extdata/output7_corrected_cdf_references.csv",
    cdf_source_sha256 =
      "60e76a362952d3cc3c3aecb1f57722c033f016baf70f426212e60c752d9d62fe",
    cdf_provenance_sha256 =
      "f27a07c0747abe43451ec59d286f6b5c4c4e4a91207a1dced6d4140af8de0fba",
    tracked_mean_values = c(
      lambda = 1.1347690848653513,
      lower_root = -1.4295614449618761,
      upper_root = 2.4442393354324303,
      width = 3.8738007803943084,
      midpoint = 0.50733894523527689,
      total_loss = 10.163729538711271
    ),
    tracked_cdf_values = c(
      lambda = 0.34724758430380498,
      lower_root = 0.40819300327404501,
      upper_root = 0.56214056814096802,
      width = 0.57300384946857796,
      midpoint = 0.48905951933756098
    ),
    comparison_rows = 11L,
    quadrature_order = 80L,
    previous_order = 64L,
    mean_comparison_tolerance = 1e-9,
    cdf_comparison_tolerance = 5e-11,
    order_convergence_tolerance = 5e-11,
    sampler = list(
      schema_version =
        "rqrgibbs_ordinary_v1_f01_sampler_oracle/1.0.0",
      learning_rate_mode =
        "learned_pseudoresidual_normalized",
      chains = 4L,
      burn_in = 5000L,
      retained_per_chain = 20000L,
      thin = 1L,
      seeds = as.integer(82931:82934),
      mcse_multiplier = 4
    )
  ),
  mcmc = list(
    chains = 4L,
    burn_in = 1000L,
    retained_per_chain = 3000L,
    thin = 1L,
    numerical_policy = "fail",
    root_swap_probability = 0.5,
    store_latent_draws = FALSE,
    prior_state_draw_storage = "rhs_ns_only",
    sequential_execution = TRUE,
    initialization_profiles = list(
      low_wide = list(midpoint_shift = -1.5, half_width = 2.5,
                      rhs_scale_multiplier = 0.5),
      high_wide = list(midpoint_shift = 1.5, half_width = 2.5,
                       rhs_scale_multiplier = 2),
      low_narrow = list(midpoint_shift = -0.75, half_width = 0.5,
                        rhs_scale_multiplier = 4),
      high_narrow = list(midpoint_shift = 0.75, half_width = 0.5,
                         rhs_scale_multiplier = 1)
    )
  ),
  resources = list(
    hard_timeout_minutes = 240L,
    maximum_processes = 3L,
    maximum_threads = 4L,
    maximum_artifact_bytes = 1024^3,
    monitor_interval_seconds = 0.20,
    require_external_process_group_monitor = TRUE,
    sampled_resource_maxima_are_telemetry = TRUE,
    automatic_retries = FALSE
  ),
  gates = list(
    maximum_rank_normalized_rhat = 1.01,
    minimum_bulk_ess = 1000,
    minimum_tail_ess = 1000,
    maximum_numerical_repairs = 0L,
    require_exact_joint_target = TRUE,
    require_isolated_primary_runtime = TRUE,
    require_intact_checkpoint = TRUE,
    require_intact_continuation_history = TRUE,
    mcse_provider = "posterior_mcse_mean",
    fixed_parameters_gate = "exact_identity",
    root_swap_activity_role = "sidecar_only",
    stop_after_first_failing_four_chain_cell = TRUE
  ),
  seed_ledger_note = paste(
    "The source protocol assigns only one four-seed range to each DESN prior",
    "although it requires two rate modes and forbids duplicate seed uses.",
    "Prospective, previously unused ranges 82731:82734 and 82741:82744",
    "complete the learned-rate DESN cells. Their use requires independent",
    "review before execution. The separate 82931:82934 range is frozen for",
    "the reference-only learned-rate F01 sampler-to-quadrature oracle;",
    "bounded execution remains disabled."
  ),
  seed_ledger = .rqr_ordinary_v1_seed_ledger,
  benchmark_plan = .rqr_ordinary_v1_benchmark_plan,
  fit_plan = .rqr_ordinary_v1_fit_plan,
  fixtures = list(
    F01 = list(
      y = c(-2.0, -1.3, -0.8, -0.4, -0.1, 0.1,
            0.35, 0.7, 1.1, 1.6, 2.2, 3.0),
      X = matrix(
        1, 12, 1, dimnames = list(NULL, "intercept")
      ),
      prior = list(type = "ridge", tau2 = 25)
    ),
    F02 = list(
      y = y_f02, X = X_f02,
      prior = list(
        type = "gaussian",
        mean = c(intercept = 0.2, x = -0.1, z = 0.3),
        precision = matrix(
          c(2.0, 0.3, -0.1, 0.3, 1.5, 0.2, -0.1, 0.2, 1.2),
          3, 3, byrow = TRUE,
          dimnames = list(colnames(X_f02), colnames(X_f02))
        )
      )
    ),
    F03 = list(
      y = y_f02_complete, X = X_f02,
      ridge = list(type = "ridge", tau2 = 4),
      gaussian = list(
        type = "gaussian",
        mean = stats::setNames(rep(0, 3), colnames(X_f02)),
        precision = structure(
          diag(0.25, 3, 3),
          dimnames = list(colnames(X_f02), colnames(X_f02))
        )
      )
    ),
    F04 = list(
      y = y_f04, X = X_f04,
      prior = list(
        type = "rhs_ns", intercept_name = "intercept",
        intercept_mean = 0, intercept_precision = 0.04,
        tau0 = 0.7, a_zeta = 2.5, b_zeta = 1.3,
        zeta2_fixed = NULL
      )
    ),
    F05 = list(
      y = y_f04, X = X_f04,
      prior = list(
        type = "rhs_ns", intercept_name = "intercept",
        intercept_mean = 0, intercept_precision = 0.04,
        tau0 = 0.7, a_zeta = 2.5, b_zeta = 1.3,
        zeta2_fixed = 3
      )
    ),
    D01 = list(
      y = y_d01, X = X_d01, time_index = 101 + seq_len(n_d01),
      intercept = "intercept",
      builder = list(id = "ordinary_v1_deterministic_fixture", version = "1.0.0"),
      reservoir_seed = 82701L,
      reservoir_depth = 2L,
      causal = list(
        uses_current_response = FALSE,
        uses_future_response = FALSE,
        minimum_response_lag = 1L
      ),
      future_X = X_d01_future,
      future_time_index = 101 + n_d01 + 1:2
    ),
    D02 = list(
      materializer_seed = 82701L,
      response_history = y_d02,
      effective_arguments = list(
        D = 1L, n = 4L, n_tilde = integer(0), m = 2L,
        washout = 3L, seed = 82701L
      ),
      promotion_requires_isolated_attested_exdqlm_runtime = TRUE,
      bounded_grid_role =
        "attested_frozen_design_for_all_16_desn_fits",
      future_extension = list(
        horizon = 2L,
        semantics = "precomputed_design",
        role = "mechanics_only_deterministic_origin_fixed_extension",
        response_simulation = FALSE
      )
    ),
    D03 = list(
      semantics = c(
        "precomputed_design", "teacher_forced_one_step",
        "external_driver_path"
      ),
      horizon = 2L
    )
  ),
  protected_dlm_companion = list(
    schema_version =
      "rqrgibbs_ordinary_v1_protected_dlm_companion/1.0.0",
    collector_path = paste0(
      "application/scripts/",
      "30_bundle_rqr_ordinary_v1_protected_dlm_evidence.R"
    ),
    collector_sha256 =
      "eba18d34f7b50a9166d4991fafcaa1ae669e0dd0eba3ca0d4c9d3416d0be0f88",
    compact_files = c(
      "artifact_hashes.csv", "bundle_manifest.json",
      "input_artifact_hashes.csv", "input_bundle_summary.csv",
      "semantic_gates.csv"
    ),
    semantic_gate_count = 16L,
    input_role_count = 4L,
    input_artifact_count = 39L,
    execution_environment =
      "RQR_ORDINARY_V1_DLM_COMPANION_DIR",
    source_state_role =
      "reviewed_disabled_candidate_runtime_bound_to_benchmark"
  ),
  protected_dlm_sha256 = c(
    "application/R/rqr_dlm_fit.R" =
      "3bfc0daa937dbd17d0cfa867829c5c03aa243b709b48a3a34679ce188b3b4d73",
    "application/R/rqr_dlm_model.R" =
      "753a2b5c4abaec188735cf28d4490d85043a09983d5f6ef19c9501b86c902c44",
    "application/R/rqr_evolution.R" =
      "d73aeaf24ec830ac18c771aafff383953e472fe813d65c3e4331991a19355ee1",
    "application/R/rqr_ffbs.R" =
      "18396d7e1b59a216676aefea5690a1a59f6afa42371108f9bc88e41e40735438",
    "application/R/rqr_utils.R" =
      "657d08b36464f96158f4d1fa481dff298b84b19efdc9f5b4647006ce8df281b8",
    "application/R/rqr_numerics.R" =
      "0a1308ef56e987d802044448d7a61433eda5ae9677aa9a2b2b30a183a730268a",
    "application/src/rqr_ffbs.cpp" =
      "e14848e8fd3dabaa7be5b55d568cb85ac634df98a9b5cd20673a567a9bbfa0de",
    "application/config/rqr_dlm/rqr_dlm_bounded_dynamic_fixtures_20260723.R" =
      "f74e2becb1148c92ef37a7fa61e3d467d3b74d13f5199c3a5a7d4c9405ad46b6",
    "application/config/rqr_dlm/rqr_dlm_main_simulation_20260724.R" =
      "f44ec8d5bce3a264faaf2381b92ad2e678ab8eb0f18cc5c1c5d25c5a9bee750a",
    "application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_20260724.R" =
      "8aa1da4174134a5d1945bd3e7f5bfbb96b1d93d1b5b16896d4939f5362bd0dd4",
    "application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_methods_20260724.csv" =
      "2e0f3e371e0a75afda434b1a5971059b67965f3ef07bd4aa799135b5e6f3bda3",
    "application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv" =
      "3355474d8c68bd7d466b061c5c760045b5dfacb055998cfa00a4aa0147680d18",
    "application/config/rqr_dlm/rqr_dlm_output13_bounded_expected_bundle_20260724.json" =
      "0c66e12b580b382d2eb5543b0fa758a1839f43a3529ed0740c15d2bf925ac1dd",
    "application/scripts/15_run_rqr_dlm_confirmatory_simulation.R" =
      "83645e83706a2f9b5933d605455016a6e22e379ac8c70bbc1c41621fef28ea9b",
    "application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh" =
      "bb9de87021432f5cb38b7e8dfe89f3266b88570153d8512f4ae032e415119bef",
    "application/scripts/17_launch_rqr_dlm_confirmatory_wave.R" =
      "643c3908feeb52d4ffb4af406ab6fdfaa07a93b286af6722ec0d12d465bf5f16",
    "application/DESCRIPTION" =
      "1d9d9218915eaed659cd93ae373f433fe96bb20822b93426a481cfaff644a494",
    "application/NAMESPACE" =
      "27d4e8f2dd18692b2a0af3be61c421ee1246c20bf52216ad2d0faa58701ea950",
    "application/R/RcppExports.R" =
      "99c796cbf443f71e62dd2997e31d841af08908db38287481fe2cafc7895d4e61",
    "application/src/RcppExports.cpp" =
      "727fcc6758bc6a3d617eef1e6444690c874cf93495ba00f2f630b7d2941c7e7a",
    "application/src/rqr_interweave.cpp" =
      "6a71e8cb2de83d912946924b1f90cc8c4290517e465d13faec9b5e178c49b077",
    "application/src/Makevars" =
      "3b8ad3ea8672999f60077c4aef88c24d39c2854efd8858532d7f86351948ff37",
    "application/src/Makevars.win" =
      "3b8ad3ea8672999f60077c4aef88c24d39c2854efd8858532d7f86351948ff37"
  ),
  evidence_schema_version = "rqrgibbs_ordinary_v1_evidence/1.0.0",
  compact_evidence_files = c(
    "source_state.csv", "runtime_attestations.csv",
    "validation_config_digest.csv", "fixture_manifest.csv",
    "seed_ledger.csv", "reference_gates.csv", "oracle_comparisons.csv",
    "f01_quadrature_checks.csv", "f01_sampler_checks.csv",
    "missingness_checks.csv", "rhs_ns_conditional_checks.csv",
    "continuation_checks.csv", "history_mutation_checks.csv",
    "desn_design_checks.csv", "desn_future_checks.csv",
    "protected_dlm_hashes.csv",
    "protected_dlm_companion_checks.csv", "package_checks.csv",
    "protected_dlm_companion/artifact_hashes.csv",
    "protected_dlm_companion/bundle_manifest.json",
    "protected_dlm_companion/input_artifact_hashes.csv",
    "protected_dlm_companion/input_bundle_summary.csv",
    "protected_dlm_companion/semantic_gates.csv",
    "fit_plan.csv", "initialization_manifest.csv",
    "fit_plan_status.csv", "bounded_diagnostics.csv",
    "compact_posterior_summaries.csv",
    "checkpoint_manifest.csv", "provenance_checks.csv",
    "root_swap_sidecar.csv", "rhs_root_trace_sidecar.csv",
    "fixed_parameter_checks.csv", "local_chain_hashes.csv",
    "resource_summary.csv", "failure_log.csv", "run_status.csv",
    "session_info.txt", "artifact_hashes.csv", "closeout.md"
  )
)

rm(
  .rqr_ordinary_v1_chain_rows, .rqr_ordinary_v1_fit_plan,
  .rqr_ordinary_v1_benchmark_plan, .rqr_ordinary_v1_seed_ledger,
  x_f02, z_f02, X_f02, y_f02_complete,
  y_f02, x1_f04, x2_f04, x3_f04, X_f04, y_f04,
  n_d01, X_d01, y_d01, X_d01_future, y_d02
)
