# Frozen configuration for the first bounded multicomponent RQR-DLM fixtures.
# This file declares validation work only. It is not a matched simulation
# protocol and does not authorize a production launch.

rqr_dlm_bounded_dynamic_fixtures <- list(
  schema_version = "rqrgibbs_dlm_bounded_fixtures/3.0.0",
  config_id = "rqr_dlm_bounded_dynamic_fixtures_20260723",
  scope = "bounded_dynamic_target_and_mixing_validation",
  generalized_bayes = TRUE,
  response_likelihood = FALSE,
  response_prediction_contract = FALSE,
  production_simulation_authorized = FALSE,
  bounded_dynamic_execution_authorized = FALSE,
  runner_modes = c("preflight", "reference-only", "execute-bounded"),
  coverage_level = 0.80,
  learning_rate_modes = c(
    "fixed_rate",
    "learned_pseudoresidual_normalized"
  ),
  fixed_learning_rate = 1,
  loss_reference_scale = 1,
  lambda_prior = list(shape = 4, rate = 4),
  mcmc = list(
    chains = 4L,
    burn_in = 2000L,
    retained_per_chain = 4000L,
    thin = 1L,
    seeds = c(84201L, 84202L, 84203L, 84204L),
    backend = "cpp",
    numerical_policy = "fail",
    store_state_draws = TRUE,
    store_latent_draws = FALSE,
    initialization_profiles = list(
      low_wide = list(
        lower_root_shift = -3, upper_root_shift = 3,
        lambda_initial = 0.50, component_scale_multiplier = 0.50
      ),
      high_wide = list(
        lower_root_shift = -1, upper_root_shift = 5,
        lambda_initial = 2.00, component_scale_multiplier = 2.00
      ),
      low_narrow = list(
        lower_root_shift = -2, upper_root_shift = 0.5,
        lambda_initial = 4.00, component_scale_multiplier = 4.00
      ),
      high_narrow = list(
        lower_root_shift = 0.5, upper_root_shift = 3,
        lambda_initial = 1.00, component_scale_multiplier = 1.00
      )
    )
  ),
  seeds = list(
    conditional_reference = 84301L,
    ffbs_parity = 84302L,
    missing_measurement = 84303L,
    future_state = 84304L,
    component_scale = 84305L,
    continuation = 84306L,
    initialization = 84307L,
    forecast_by_fixture = c(
      fixed_W_local_level = 84311L,
      frozen_trend_seasonal_discount = 84312L,
      shared_component_scale_trend_regression = 84313L
    )
  ),
  continuation = list(
    history_segments = 3L,
    generation_indices = 0:2,
    retained_by_segment = c(2L, 2L, 2L),
    uninterrupted_retained = 6L,
    require_checkpoint_digest = TRUE,
    require_history_digest = TRUE
  ),
  resources = list(
    sequential_execution = TRUE,
    hard_timeout_minutes = 45L,
    maximum_process_tree_rss_gib = 4,
    maximum_process_tree_threads = 4L,
    maximum_process_tree_processes = 3L,
    monitor_interval_seconds = 0.20,
    require_active_process_tree_monitor = TRUE
  ),
  gates = list(
    maximum_rank_normalized_rhat = 1.01,
    minimum_bulk_ess = 1000,
    minimum_tail_ess = 1000,
    maximum_numerical_repairs = 0L,
    require_primary_runtime_source_match = TRUE,
    require_exact_joint_target = TRUE,
    root_swap_activity_role = "sidecar_only",
    primary_diagnostics = "posterior_rank_normalized_rhat_bulk_tail_ess",
    custom_diagnostics_role = "reproduction_crosscheck",
    coda_diagnostics_role = "classical_sidecar",
    mcse_provider = "posterior_mcse_mean",
    fixed_rate_lambda_gate = "exact_identity",
    require_three_segment_continuation = TRUE
  ),
  fixtures = list(
    fixed_W_local_level = list(
      evolution_mode = "fixed_W",
      state_components = list(
        list(type = "polytrend", order = 1L, name = "level", C0 = 4)
      ),
      n_time = 24L,
      y = as.numeric(0.04 * seq_len(24L) + sin(seq_len(24L) / 3)),
      missing_indices = c(6L, 17L),
      W = 0.04,
      future_horizon = 4L,
      future_W = array(0.04, dim = c(1L, 1L, 4L)),
      reference_gate = "dense_Gaussian_conditional_FFBS_moments"
    ),
    frozen_trend_seasonal_discount = list(
      evolution_mode = "discount_template",
      state_components = list(
        list(
          type = "polytrend", order = 2L, name = "trend",
          C0 = diag(c(4, 1))
        ),
        list(
          type = "seasonal", period = 4L, harmonics = c(1L, 2L),
          name = "seasonal", C0 = diag(c(2, 2, 2))
        )
      ),
      n_time = 36L,
      y = as.numeric(
        0.025 * seq_len(36L) +
          0.8 * sin(2 * pi * seq_len(36L) / 4)
      ),
      missing_indices = integer(0),
      df = c(0.96, 0.92),
      dim_df = c(2L, 3L),
      reference_variance = 1,
      future_horizon = 4L,
      future_template_rule = "extend_reference_recursion_T_plus_H",
      reference_gate = "frozen_template_reconstruction_and_FFBS_parity"
    ),
    shared_component_scale_trend_regression = list(
      evolution_mode = "component_scale",
      state_components = list(
        list(
          type = "polytrend", order = 2L, name = "trend",
          C0 = diag(c(4, 1))
        ),
        list(
          type = "regression", name = "regression",
          C0 = matrix(2, 1L, 1L),
          X = matrix(
            seq(-1, 1, length.out = 30L), ncol = 1L,
            dimnames = list(NULL, "x")
          )
        )
      ),
      n_time = 30L,
      y = as.numeric(
        0.03 * seq_len(30L) +
          0.6 * seq(-1, 1, length.out = 30L)
      ),
      missing_indices = c(11L),
      component_templates = list(diag(2), matrix(1, 1, 1)),
      component_scale_prior = list(
        shape = c(3, 3), rate = c(0.1, 0.1)
      ),
      component_scale_initial = c(0.05, 0.05),
      future_horizon = 3L,
      future_X = matrix(
        seq(1 + 2 / 29, 1 + 6 / 29, length.out = 3L),
        ncol = 1L, dimnames = list(NULL, "x")
      ),
      component_templates_future = list(
        array(rep(diag(2), 3L), dim = c(2L, 2L, 3L)),
        array(1, dim = c(1L, 1L, 3L))
      ),
      reference_gate = "analytic_shared_inverse_Gamma_component_conditionals"
    )
  )
)
