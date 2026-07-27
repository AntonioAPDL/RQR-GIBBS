.rqr_static_continuation_token <- new.env(parent = emptyenv())

.rqr_static_prior_init_payload <- function(prior, state) {
  if (is.null(state) || !identical(prior$type, "rhs_ns") ||
      !identical(
        state$schema_version %||% NA_character_,
        .rqr_rhs_ns_state_schema()
      )) {
    return(state)
  }
  .rqr_prior_state_validate(prior, state)
  list(
    lambda2 = state$lambda2,
    nu = state$nu,
    tau2 = state$tau2,
    xi = state$xi,
    zeta2 = state$zeta2,
    update_count = state$update_count,
    numerical_repair_count = state$numerical_repair_count
  )
}

#' Fit ordinary fixed-design RQR by exact Gibbs sampling
#'
#' This function updates two exchangeable interval-root regressions under the
#' ordinary (zero-tilt) RQR loss.  The result is a generalized-Bayes interval
#' update.  It is not a likelihood for the response and does not provide
#' posterior-predictive response draws.
#'
#' @param y Numeric response vector. `NA` entries are allowed and omitted from
#'   every loss and augmentation update while their design rows are retained.
#' @param X Finite numeric design matrix. Column names may be absent; when
#'   supplied, they must be complete, nonempty, and unique.
#' @param coverage_level Interval coverage level in `(0, 1)`.
#' @param learning_rate Positive fixed generalized-Bayes rate `omega_R`.
#' @param lambda_initial Positive initial inverse loss scale for learned mode.
#' @param loss_reference_scale Positive fixed reference scale `s_L`.
#' @param learning_rate_mode One of `"fixed_rate"`,
#'   `"learned_pseudoresidual_normalized"`, or `"learned_pure"`. The
#'   backward-compatible `"learned_pure"` target remains executable only for
#'   diagnostic comparison; it is outside ordinary-v1, nonpromotable, and
#'   noncontinuable.
#' @param lambda_prior Gamma prior for learned `lambda`, with positive `shape`
#'   and `rate`.
#' @param beta_prior_obj A closure-free prior from [rqr_beta_prior()] or a
#'   supported legacy prior. Legacy hyperparameters are extracted into a
#'   native closure-free contract; callbacks are neither retained nor run.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param root_swap_probability Probability of the complete root/prior-state
#'   label swap at the end of each Gibbs transition.
#' @param provenance_control Repository and isolated-runtime provenance
#'   controls.
#' @param mcmc_control Named list with `n_burn`, `n_mcmc`, `thin`, `seed`,
#'   `precision_beta`, `store_latent_draws`, and
#'   `store_prior_state_draws`.
#' @param init Optional complete or partial initial Markov state.
#' @param embedding_contract Optional closure-free semantic contract for a
#'   design generator such as a frozen DESN design.
#' @param ... Reserved.
#' @return An `rqr_mcmc` object.
#' @export
rqr_mcmc_fit <- function(
    y, X, coverage_level, learning_rate = 1,
    lambda_initial = 1, loss_reference_scale = 1,
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized", "learned_pure"
    ),
    lambda_prior = list(shape = 4, rate = 4),
    beta_prior_obj = NULL,
    numerical_policy = c("fail", "record_repair"),
    root_swap_probability = 0.5,
    provenance_control = list(), mcmc_control = list(),
    init = list(), embedding_contract = NULL, ...) {
  continuation_only_fields <- c(
    "continued_from_checkpoint", "completed_iterations",
    "parent_cumulative_numerical_repair_count",
    "parent_chain_history_numerically_exact",
    "continuation_control"
  )
  if (is.list(init) &&
      any(continuation_only_fields %in% names(init))) {
    stop(
      paste(
        "Continuation-only init fields are private to",
        "rqr_mcmc_continue()."
      ),
      call. = FALSE
    )
  }
  .rqr_mcmc_fit_impl(
    y = y, X = X, coverage_level = coverage_level,
    learning_rate = learning_rate,
    lambda_initial = lambda_initial,
    loss_reference_scale = loss_reference_scale,
    learning_rate_mode = learning_rate_mode,
    lambda_prior = lambda_prior,
    beta_prior_obj = beta_prior_obj,
    numerical_policy = numerical_policy,
    root_swap_probability = root_swap_probability,
    provenance_control = provenance_control,
    mcmc_control = mcmc_control,
    init = init,
    embedding_contract = embedding_contract,
    ...,
    .continuation_token = NULL
  )
}

.rqr_mcmc_fit_impl <- function(
    y, X, coverage_level, learning_rate = 1,
    lambda_initial = 1, loss_reference_scale = 1,
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized", "learned_pure"
    ),
    lambda_prior = list(shape = 4, rate = 4),
    beta_prior_obj = NULL,
    numerical_policy = c("fail", "record_repair"),
    root_swap_probability = 0.5,
    provenance_control = list(), mcmc_control = list(),
    init = list(), embedding_contract = NULL, ...,
    .continuation_token = NULL) {
  dots <- list(...)
  if (length(dots)) {
    stop(
      sprintf(
        "Unused arguments in ...: %s.",
        paste(names(dots) %||% rep("<unnamed>", length(dots)),
              collapse = ", ")
      ),
      call. = FALSE
    )
  }
  data <- .rqr_fixed_design_data(y, X)
  y <- data$y
  X <- data$X
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  lambda_prior <- .rqr_lambda_prior(lambda_prior, mode)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  root_swap_probability <- .rqr_validate_root_swap_probability(
    root_swap_probability
  )
  provenance_control <- .rqr_provenance_control(provenance_control)
  if (!is.list(init)) stop("init must be a list.", call. = FALSE)
  .rqr_validate_named_list_fields(
    init, "init",
    c(
      "beta1", "beta2", "beta_root1", "beta_root2",
      "lambda", "latent_v", "V", "beta_prior_state1",
      "beta_prior_state2", "rhs_ns_state1", "rhs_ns_state2",
      "intercept_name", "rng_state", "continued_from_checkpoint",
      "completed_iterations",
      "parent_cumulative_numerical_repair_count",
      "parent_chain_history_numerically_exact",
      "continuation_control"
    )
  )
  if (all(c("beta1", "beta_root1") %in% names(init)) ||
      all(c("beta2", "beta_root2") %in% names(init)) ||
      all(c("latent_v", "V") %in% names(init)) ||
      all(c(
        "beta_prior_state1", "rhs_ns_state1"
      ) %in% names(init)) ||
      all(c(
        "beta_prior_state2", "rhs_ns_state2"
      ) %in% names(init))) {
    stop(
      "init cannot supply both canonical fields and their legacy aliases.",
      call. = FALSE
    )
  }
  continued <- identical(
    .continuation_token, .rqr_static_continuation_token
  )
  continuation_only_fields <- c(
    "continued_from_checkpoint", "completed_iterations",
    "parent_cumulative_numerical_repair_count",
    "parent_chain_history_numerically_exact",
    "continuation_control"
  )
  declared_continuation <- init$continued_from_checkpoint %||% FALSE
  if (!is.logical(declared_continuation) ||
      length(declared_continuation) != 1L ||
      is.na(declared_continuation) ||
      !identical(declared_continuation, continued) ||
      (!continued &&
       any(continuation_only_fields %in% names(init)))) {
    stop(
      paste(
        "Continuation-only init fields require the private validated",
        "rqr_mcmc_continue() worker boundary."
      ),
      call. = FALSE
    )
  }
  if (!is.list(mcmc_control)) {
    stop("mcmc_control must be a list.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    mcmc_control, "mcmc_control",
    c(
      "n_burn", "n_mcmc", "thin", "seed", "rng_seed",
      "verbose", "progress_every", "store_latent_draws",
      "store_prior_state_draws", "precision_beta", "precision",
      "intercept_name"
    )
  )
  loss_reference_scale <- .rqr_scalar_numeric(
    loss_reference_scale, "loss_reference_scale",
    lower = 0, lower_open = TRUE
  )
  learning_rate <- .rqr_scalar_numeric(
    learning_rate, "learning_rate", lower = 0, lower_open = TRUE
  )
  learn_lambda <- !identical(mode, "fixed_rate")
  lambda_initial <- .rqr_scalar_numeric(
    init$lambda %||% lambda_initial, "lambda_initial",
    lower = 0, lower_open = TRUE
  )
  lambda_current <- if (learn_lambda) {
    lambda_initial
  } else {
    learning_rate * loss_reference_scale
  }
  constants <- rqr_constants(
    coverage_level, lambda_current / loss_reference_scale
  )

  if (is.null(beta_prior_obj)) {
    beta_prior_obj <- .rqr_beta_prior_spec(
      "ridge", ridge = list(tau2 = 1e4)
    )
  }
  intercept_name <- mcmc_control$intercept_name %||%
    init$intercept_name %||% NULL
  prior <- .rqr_beta_prior_coerce(
    beta_prior_obj, X = X, intercept_name = intercept_name
  )

  n_burn <- .rqr_scalar_integer(
    mcmc_control$n_burn %||% 1000L,
    "mcmc_control$n_burn", 0L
  )
  if (continued && n_burn != 0L) {
    stop(
      "An exact continuation segment cannot add a new burn-in.",
      call. = FALSE
    )
  }
  n_keep <- .rqr_scalar_integer(
    mcmc_control$n_mcmc %||% 1000L,
    "mcmc_control$n_mcmc", 1L
  )
  thin <- .rqr_scalar_integer(
    mcmc_control$thin %||% 1L,
    "mcmc_control$thin", 1L
  )
  seed <- mcmc_control$seed %||%
    mcmc_control$rng_seed %||% NULL
  if (all(c("seed", "rng_seed") %in% names(mcmc_control))) {
    stop(
      "mcmc_control cannot supply both seed and rng_seed.",
      call. = FALSE
    )
  }
  if (!is.null(seed) && !is.null(init$rng_state)) {
    stop(
      "Supply mcmc_control$seed or init$rng_state, not both.",
      call. = FALSE
    )
  }
  if (!is.null(seed)) {
    seed <- .rqr_scalar_integer(
      seed, "mcmc_control$seed", 0L
    )
  }
  verbose <- .rqr_scalar_logical(
    mcmc_control$verbose %||% FALSE,
    "mcmc_control$verbose"
  )
  progress_every <- .rqr_scalar_integer(
    mcmc_control$progress_every %||% 100L,
    "mcmc_control$progress_every", 1L
  )
  store_latent_draws <- .rqr_scalar_logical(
    mcmc_control$store_latent_draws %||% FALSE,
    "mcmc_control$store_latent_draws"
  )
  store_prior_state_draws <- .rqr_scalar_logical(
    mcmc_control$store_prior_state_draws %||% FALSE,
    "mcmc_control$store_prior_state_draws"
  )
  if (all(c("precision_beta", "precision") %in% names(mcmc_control))) {
    stop(
      paste(
        "mcmc_control cannot supply both precision_beta and",
        "its legacy precision alias."
      ),
      call. = FALSE
    )
  }
  precision_beta_cfg <- .exal_normalize_mcmc_precision_beta_cfg(
    mcmc_control$precision_beta %||%
      mcmc_control$precision %||% list()
  )
  precision_beta_cfg$jitter_ladder <- .rqr_jitter_ladder(
    numerical_policy, precision_beta_cfg$jitter_ladder
  )
  target <- .rqr_static_target_contract(
    coverage_level = coverage_level,
    learning_rate_mode = mode,
    fixed_learning_rate =
      if (learn_lambda) NA_real_ else learning_rate,
    loss_reference_scale = loss_reference_scale,
    lambda_prior = lambda_prior,
    beta_prior = prior,
    numerical_policy = numerical_policy,
    precision_beta = precision_beta_cfg,
    root_swap_probability = root_swap_probability,
    embedding_contract = embedding_contract
  )
  completed_offset <- .rqr_history_count(
    init$completed_iterations %||% 0L,
    "completed_iterations"
  )

  initial_roots <- .rqr_init_roots(
    y[data$observed], X[data$observed, , drop = FALSE],
    coverage_level, init = init
  )
  if (length(initial_roots$beta1) != data$p ||
      length(initial_roots$beta2) != data$p ||
      any(!is.finite(initial_roots$beta1)) ||
      any(!is.finite(initial_roots$beta2))) {
    stop("Initial beta vectors must be finite with ncol(X) entries.",
         call. = FALSE)
  }
  state <- list(
    beta_root1 = as.numeric(initial_roots$beta1),
    beta_root2 = as.numeric(initial_roots$beta2),
    lambda = lambda_current,
    latent_v = .rqr_static_full_latent(
      init$latent_v %||% init$V, data,
      placeholder = constants$sigma
    ),
    beta_prior_state1 = .rqr_prior_initialize(
      prior, data$p,
      init = .rqr_static_prior_init_payload(
        prior,
        init$beta_prior_state1 %||% init$rhs_ns_state1
      )
    ),
    beta_prior_state2 = .rqr_prior_initialize(
      prior, data$p,
      init = .rqr_static_prior_init_payload(
        prior,
        init$beta_prior_state2 %||% init$rhs_ns_state2
      )
    )
  )
  if (identical(prior$type, "rhs_ns")) {
    for (root_name in c(
        "beta_prior_state1", "beta_prior_state2"
      )) {
      if (!identical(
          as.integer(state[[root_name]]$update_count),
          completed_offset
        )) {
        stop(
          paste(
            "RHS-NS prior-state update_count must equal",
            "the chain's completed_iterations at initialization."
          ),
          call. = FALSE
        )
      }
    }
  }
  initialization_contract <- .rqr_make_initialization_contract(
    family = "rqr_fixed_design",
    initial_state = state,
    seed = seed,
    rng_state = init$rng_state %||% NULL,
    continued = continued,
    parent_checkpoint_digest = if (continued) {
      init$continuation_control$parent_checkpoint_digest
    } else {
      NULL
    }
  )
  initialization_digest <- .rqr_digest(
    initialization_contract
  )

  total_iter <- n_burn + n_keep * thin
  beta1_draws <- matrix(
    NA_real_, n_keep, data$p,
    dimnames = list(NULL, colnames(X))
  )
  beta2_draws <- matrix(
    NA_real_, n_keep, data$p,
    dimnames = list(NULL, colnames(X))
  )
  lambda_draws <- numeric(n_keep)
  latent_v_draws <- if (store_latent_draws) {
    matrix(
      NA_real_, n_keep, data$n_total,
      dimnames = list(NULL, data$row_ids)
    )
  } else {
    NULL
  }
  prior_state1_draws <- if (store_prior_state_draws) {
    vector("list", n_keep)
  } else {
    NULL
  }
  prior_state2_draws <- if (store_prior_state_draws) {
    vector("list", n_keep)
  } else {
    NULL
  }
  prior_stats1 <- vector("list", n_keep)
  prior_stats2 <- vector("list", n_keep)
  loss_trace <- numeric(total_iter)
  lambda_trace <- numeric(total_iter)
  effective_rate_trace <- numeric(total_iter)
  lambda_shape_trace <- rep(NA_real_, total_iter)
  lambda_rate_trace <- rep(NA_real_, total_iter)
  precision_strategy1 <- character(total_iter)
  precision_strategy2 <- character(total_iter)
  root_swap_trace <- logical(total_iter)
  repair_records <- NULL
  prior_repair_count <- 0L
  save_index <- 0L

  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    .rqr_restore_rng(init$rng_state %||% NULL)
  }
  for (iteration in seq_len(total_iter)) {
    transition <- .rqr_fixed_design_transition(
      state = state, data = data, target = target, prior = prior,
      precision_beta_cfg = precision_beta_cfg,
      iteration = iteration
    )
    state <- transition$state
    constants <- transition$constants
    loss_trace[iteration] <- transition$loss
    lambda_trace[iteration] <- state$lambda
    effective_rate_trace[iteration] <- constants$omega
    lambda_shape_trace[iteration] <-
      transition$lambda_posterior$shape
    lambda_rate_trace[iteration] <-
      transition$lambda_posterior$rate
    precision_strategy1[iteration] <- as.character(
      transition$precision_info_root1$strategy %||% "cholesky"
    )
    precision_strategy2[iteration] <- as.character(
      transition$precision_info_root2$strategy %||% "cholesky"
    )
    root_swap_trace[iteration] <- transition$root_swapped
    if (nrow(transition$precision_repair_records)) {
      repair_records <- if (is.null(repair_records)) {
        transition$precision_repair_records
      } else {
        rbind(
          repair_records, transition$precision_repair_records
        )
      }
    }
    prior_repair_count <- .rqr_history_count(
      as.double(prior_repair_count) +
        as.double(transition$prior_numerical_repair_count),
      "cumulative prior numerical-repair count"
    )

    if (iteration > n_burn &&
        (iteration - n_burn) %% thin == 0L) {
      save_index <- save_index + 1L
      beta1_draws[save_index, ] <- state$beta_root1
      beta2_draws[save_index, ] <- state$beta_root2
      lambda_draws[save_index] <- state$lambda
      if (store_latent_draws) {
        latent_v_draws[save_index, ] <- state$latent_v
      }
      if (store_prior_state_draws) {
        prior_state1_draws[[save_index]] <-
          state$beta_prior_state1
        prior_state2_draws[[save_index]] <-
          state$beta_prior_state2
      }
      prior_stats1[[save_index]] <-
        transition$prior_stats_root1
      prior_stats2[[save_index]] <-
        transition$prior_stats_root2
    }
    if (verbose &&
        (iteration %% progress_every == 0L ||
          iteration == total_iter)) {
      message(sprintf(
        "[rqr_mcmc_fit] iter %d/%d loss=%.6g",
        iteration, total_iter, loss_trace[iteration]
      ))
    }
  }

  segment_precision_repairs <- if (is.null(repair_records)) {
    0L
  } else {
    nrow(repair_records)
  }
  segment_repairs <- .rqr_history_count(
    as.double(segment_precision_repairs) +
      as.double(prior_repair_count),
    "segment numerical-repair count"
  )
  parent_repairs <- if (continued) {
    .rqr_history_count(
      init$parent_cumulative_numerical_repair_count,
      "parent cumulative numerical-repair count"
    )
  } else {
    0L
  }
  cumulative_repairs <- .rqr_history_count(
    as.double(parent_repairs) + as.double(segment_repairs),
    "cumulative numerical-repair count"
  )
  parent_chain_exact <- if (continued) {
    isTRUE(init$parent_chain_history_numerically_exact)
  } else {
    TRUE
  }
  segment_exact <- segment_repairs == 0L
  chain_exact <- parent_chain_exact && segment_exact
  completed_iterations <- .rqr_history_count(
    as.double(completed_offset) + as.double(total_iter),
    "cumulative completed_iterations"
  )

  provenance <- .rqr_provenance(
    data = .rqr_static_provenance_data(data),
    matrices = list(X = X),
    numerical_policy = numerical_policy,
    initial_seed = seed,
    repo_root = provenance_control$repo_root,
    expected_git_commit = provenance_control$expected_git_commit,
    backend = "R_precision_cholesky",
    backend_requested = "R_precision_cholesky",
    backend_resolved = "R_precision_cholesky",
    objects = .rqr_static_provenance_objects(target, prior),
    external_repositories =
      provenance_control$external_repositories,
    required_external_repositories =
      provenance_control$required_external_repositories,
    primary_runtime_attestation =
      provenance_control$primary_runtime_attestation,
    initialization_contract = initialization_contract
  )
  checkpoint <- .rqr_static_checkpoint(
    state = state, completed_iterations = completed_iterations,
    data = data,
    target_digest = provenance$object_digests$target,
    prior_digest = provenance$object_digests$beta_prior
  )
  checkpoint_digest <- .rqr_digest(checkpoint)
  continuation <- init$continuation_control %||% list()
  if (!is.list(continuation)) {
    stop("The private continuation control must be a list.",
         call. = FALSE)
  }
  segment_schedule_contract <- .rqr_make_static_schedule_contract(
    start_completed_iterations = completed_offset,
    n_burn = n_burn,
    n_retained_draws = n_keep,
    thin = thin,
    initialization_contract = initialization_contract,
    initialization_digest = initialization_digest,
    checkpoint_digest = checkpoint_digest,
    checkpoint_state = checkpoint,
    parent = continuation$parent_schedule_contract %||% NULL,
    parent_digest = continuation$parent_schedule_digest %||% NULL
  )
  segment_schedule_digest <- .rqr_digest(
    segment_schedule_contract
  )
  lambda_summary <- .rqr_lambda_summary(lambda_draws)
  effective_rate_summary <- .rqr_lambda_summary(
    lambda_draws / loss_reference_scale
  )
  summary <- .rqr_static_fit_summary(
    data, beta1_draws, beta2_draws
  )
  ordinary_v1_scope <- !identical(mode, "learned_pure")
  target_numerical_eligible <- chain_exact
  provisional_promotion <- target_numerical_eligible &&
    provenance$reproducibility_eligible &&
    ordinary_v1_scope

  out <- list(
    schema_version = .rqr_static_fit_schema(),
    method = "mcmc",
    family = "rqr_fixed_design",
    data_contract = data,
    embedding_contract = embedding_contract,
    y = y,
    X = X,
    model_spec = list(
      schema_version = .rqr_static_fit_schema(),
      family = "rqr_fixed_design",
      parameterization = "two_exchangeable_linear_roots",
      tilt = 0,
      loss_name = "rqr_residual_product_check_loss",
      coverage_level = target$coverage_level,
      learning_rate_mode = mode,
      fixed_learning_rate =
        if (learn_lambda) NA_real_ else learning_rate,
      learning_rate = if (learn_lambda) {
        effective_rate_summary$mean
      } else {
        learning_rate
      },
      lambda_initial = lambda_current,
      loss_reference_scale = loss_reference_scale,
      effective_learning_rate = effective_rate_summary$mean,
      effective_learning_rate_summary = effective_rate_summary,
      learned_inverse_loss_scale = learn_lambda,
      lambda_prior = lambda_prior,
      lambda_power = if (learn_lambda) {
        lambda_prior$power * data$n_observed
      } else {
        0
      },
      lambda_power_per_observation =
        if (learn_lambda) lambda_prior$power else 0,
      lambda_summary = lambda_summary,
      inferential_target = .rqr_target_formula(mode),
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      response_prediction_contract = FALSE,
      target_contract = "fixed_joint_exact",
      exact_joint_target = TRUE,
      ordinary_v1_scope_eligible = ordinary_v1_scope,
      continuation_supported = ordinary_v1_scope,
      numerical_policy = numerical_policy,
      numerical_repair_count = segment_repairs,
      cumulative_numerical_repair_count = cumulative_repairs,
      numerically_exact_transition = segment_exact,
      chain_history_numerically_exact = chain_exact,
      target_numerical_eligible = target_numerical_eligible,
      reproducibility_eligible =
        provenance$reproducibility_eligible,
      promotion_eligible = provisional_promotion,
      beta_prior_type = prior$type,
      root_priors_exchangeable = TRUE,
      root_swap_move = root_swap_probability > 0,
      root_swap_probability = root_swap_probability,
      n_total = data$n_total,
      n_observed = data$n_observed,
      missing_response_count =
        data$n_total - data$n_observed
    ),
    samp.beta_root1 = beta1_draws,
    samp.beta_root2 = beta2_draws,
    samp.lambda = lambda_draws,
    samp.latent_v = latent_v_draws,
    samp.beta_prior_state_root1 = prior_state1_draws,
    samp.beta_prior_state_root2 = prior_state2_draws,
    summary = summary,
    diagnostics = list(
      loss_trace = loss_trace,
      scaled_loss_trace = loss_trace / loss_reference_scale,
      weighted_loss_trace =
        lambda_trace * loss_trace / loss_reference_scale,
      lambda_trace = lambda_trace,
      effective_learning_rate_trace = effective_rate_trace,
      lambda_post_shape_trace = lambda_shape_trace,
      lambda_post_rate_trace = lambda_rate_trace,
      precision_strategy_root1 = precision_strategy1,
      precision_strategy_root2 = precision_strategy2,
      precision_beta = precision_beta_cfg,
      numerical_repairs =
        repair_records %||% .rqr_empty_repair_records(),
      prior_numerical_repair_count = prior_repair_count,
      root_swap_trace = root_swap_trace,
      prior_stats_root1 = prior_stats1,
      prior_stats_root2 = prior_stats2,
      partial_collapse_order = c(
        if (learn_lambda) "lambda_collapsed" else NULL,
        "latent_v_observed_refresh",
        "root1_beta",
        "root1_prior_state",
        "root2_beta",
        "root2_prior_state",
        "global_complete_root_prior_state_swap"
      )
    ),
    beta_prior = prior,
    initialization_contract = initialization_contract,
    initialization_digest = initialization_digest,
    provenance = provenance,
    checkpoint_state = checkpoint,
    checkpoint_digest = checkpoint_digest,
    last = checkpoint,
    segment_schedule_contract = segment_schedule_contract,
    segment_schedule_digest = segment_schedule_digest,
    misc = list(
      n_burn = n_burn, n_mcmc = n_keep, thin = thin,
      seed = seed, constants = constants,
      precision_beta = precision_beta_cfg,
      store_latent_draws = store_latent_draws,
      store_prior_state_draws = store_prior_state_draws,
      column_names = colnames(X),
      row_ids = data$row_ids,
      observed = data$observed,
      transition_version = .rqr_static_transition_version(),
      note = paste(
        "RQR is a generalized-Bayes interval-root update,",
        "not a response likelihood or response simulator."
      )
    )
  )
  out$retained_draws_contract <- .rqr_make_retained_draws_contract(
    family = "rqr_fixed_design",
    draws = .rqr_static_retained_draws(out)
  )
  out$retained_draws_digest <- .rqr_digest(
    out$retained_draws_contract
  )
  out$retained_evidence_contract <-
    .rqr_make_retained_evidence_contract(
      family = "rqr_fixed_design",
      evidence = .rqr_static_retained_evidence(out)
    )
  out$retained_evidence_digest <- .rqr_digest(
    out$retained_evidence_contract
  )

  if (ordinary_v1_scope) {
    out$continuation_history_contract <-
      .rqr_make_continuation_history(
        checkpoint_digest = checkpoint_digest,
        segment_numerical_repair_count = segment_repairs,
        segment_exact_joint_target = TRUE,
        segment_environment_base_eligible =
          provenance$reproducibility_eligible,
        segment_target_contract_digest =
          provenance$object_digests$target,
        backend_requested = provenance$backend_requested,
        backend_resolved = provenance$backend_resolved,
        parent = continuation$parent_history %||% NULL,
        parent_checkpoint_digest =
          continuation$parent_checkpoint_digest %||% NULL,
        environment_mismatches =
          continuation$environment_mismatches %||% character(0),
        environment_override_used =
          isTRUE(continuation$environment_override_used)
      )
    out$continuation_history_digest <- .rqr_digest(
      out$continuation_history_contract
    )
    out$model_spec$cumulative_numerical_repair_count <-
      out$continuation_history_contract$
        cumulative_numerical_repair_count
    out$model_spec$chain_history_numerically_exact <-
      out$continuation_history_contract$
        chain_history_numerically_exact
    out$model_spec$target_numerical_eligible <-
      out$continuation_history_contract$
        target_numerical_eligible
    out$model_spec$reproducibility_eligible <-
      out$continuation_history_contract$
        reproducibility_eligible
    out$model_spec$promotion_eligible <-
      out$continuation_history_contract$promotion_eligible
    out$provenance$reproducibility_eligible <-
      out$continuation_history_contract$
        reproducibility_eligible
  } else {
    out["continuation_history_contract"] <- list(NULL)
    out$continuation_history_digest <- NA_character_
    out$model_spec$promotion_eligible <- FALSE
  }
  class(out) <- c("rqr_mcmc", "rqr_fit")
  if (!continued) {
    .rqr_validate_static_fit_envelope(out)
  }
  out
}

.rqr_static_retained_draws <- function(object) {
  list(
    samp.beta_root1 = object$samp.beta_root1,
    samp.beta_root2 = object$samp.beta_root2,
    samp.lambda = object$samp.lambda,
    samp.latent_v = object$samp.latent_v,
    samp.beta_prior_state_root1 =
      object$samp.beta_prior_state_root1,
    samp.beta_prior_state_root2 =
      object$samp.beta_prior_state_root2
  )
}

.rqr_static_retained_evidence <- function(object) {
  list(
    summary = object$summary,
    diagnostics = object$diagnostics,
    lambda_summary = object$model_spec$lambda_summary,
    effective_learning_rate_summary =
      object$model_spec$effective_learning_rate_summary
  )
}

#' Continue an ordinary fixed-design RQR chain
#'
#' The exact checkpoint restores both root coefficients, both prior states,
#' the current loss scale and latent vector, and the full R RNG state.  Only
#' newly retained draws are returned.
#'
#' @param object An `rqr_mcmc` fit.
#' @param n_mcmc Positive number of additional retained draws.
#' @param thin Positive thinning interval.
#' @param store_latent_draws,store_prior_state_draws Storage choices for the
#'   returned segment.
#' @param allow_environment_mismatch Permit a recorded non-bitwise portability
#'   continuation. Model, data, target, prior, and checkpoint mismatches always
#'   stop. An accepted override is persisted in continuation history and
#'   removes reproducibility and promotion eligibility from the returned
#'   segment and its descendants.
#' @return A new `rqr_mcmc` segment.
#' @export
rqr_mcmc_continue <- function(
    object, n_mcmc, thin = object$misc$thin,
    store_latent_draws = object$misc$store_latent_draws,
    store_prior_state_draws =
      object$misc$store_prior_state_draws,
    allow_environment_mismatch = FALSE) {
  store_latent_draws <- .rqr_scalar_logical(
    store_latent_draws, "store_latent_draws"
  )
  store_prior_state_draws <- .rqr_scalar_logical(
    store_prior_state_draws, "store_prior_state_draws"
  )
  allow_environment_mismatch <- .rqr_scalar_logical(
    allow_environment_mismatch, "allow_environment_mismatch"
  )
  if (identical(
      object$model_spec$learning_rate_mode, "learned_pure"
    )) {
    stop(
      "learned_pure is a diagnostic legacy target and is not continuable.",
      call. = FALSE
    )
  }
  validation <- .rqr_validate_static_checkpoint(
    object, allow_environment_mismatch
  )
  n_mcmc <- .rqr_scalar_integer(n_mcmc, "n_mcmc", 1L)
  thin <- .rqr_scalar_integer(thin, "thin", 1L)
  checkpoint <- validation$checkpoint
  mismatches <- validation$environment_mismatches
  environment_override_used <- length(mismatches) > 0L
  stored_repo_root <- .rqr_stored_optional_provenance_text(
    object$provenance$repo_root, "repo_root"
  )
  stored_expected <- .rqr_stored_optional_provenance_text(
    object$provenance$expected_git_commit, "expected_git_commit"
  )
  primary_attestation <- .rqr_stored_optional_provenance_text(
    object$provenance$primary_runtime_attestation,
    "primary_runtime_attestation"
  )
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) {
    fixed_rate <- 1
  }
  segment <- .rqr_mcmc_fit_impl(
    y = object$y, X = object$X,
    coverage_level = object$model_spec$coverage_level,
    learning_rate = fixed_rate,
    lambda_initial = checkpoint$lambda,
    loss_reference_scale =
      object$model_spec$loss_reference_scale,
    learning_rate_mode =
      object$model_spec$learning_rate_mode,
    lambda_prior = object$model_spec$lambda_prior,
    beta_prior_obj = validation$prior,
    numerical_policy = object$model_spec$numerical_policy,
    root_swap_probability =
      object$model_spec$root_swap_probability,
    provenance_control = list(
      repo_root = stored_repo_root,
      expected_git_commit = stored_expected,
      primary_runtime_attestation = primary_attestation,
      external_repositories = .rqr_static_external_specs(
        object$provenance$external_repositories
      ),
      required_external_repositories =
        object$provenance$required_external_repositories
    ),
    mcmc_control = list(
      n_burn = 0L, n_mcmc = n_mcmc, thin = thin,
      seed = NULL,
      precision_beta = object$misc$precision_beta,
      store_latent_draws = store_latent_draws,
      store_prior_state_draws = store_prior_state_draws
    ),
    init = list(
      beta_root1 = checkpoint$beta_root1,
      beta_root2 = checkpoint$beta_root2,
      lambda = checkpoint$lambda,
      latent_v = checkpoint$latent_v,
      beta_prior_state1 = checkpoint$beta_prior_state1,
      beta_prior_state2 = checkpoint$beta_prior_state2,
      rng_state = checkpoint$rng_state,
      completed_iterations = checkpoint$completed_iterations,
      continued_from_checkpoint = TRUE,
      parent_cumulative_numerical_repair_count =
        validation$history$cumulative_numerical_repair_count,
      parent_chain_history_numerically_exact =
        validation$history$chain_history_numerically_exact,
      continuation_control = list(
        parent_history = validation$history,
        parent_checkpoint_digest =
          validation$checkpoint_digest,
        parent_schedule_contract = validation$schedule,
        parent_schedule_digest =
          object$segment_schedule_digest,
        environment_mismatches = mismatches,
        environment_override_used =
          environment_override_used
      )
    ),
    embedding_contract = object$embedding_contract,
    .continuation_token = .rqr_static_continuation_token
  )
  segment$continuation_contract <- list(
    continued_from_checkpoint = TRUE,
    parent_checkpoint_digest =
      validation$checkpoint_digest,
    parent_completed_iterations =
      checkpoint$completed_iterations,
    parent_schedule_digest =
      object$segment_schedule_digest,
    environment_mismatches = mismatches,
    environment_override_used =
      environment_override_used,
    bitwise_continuation_claim =
      !environment_override_used &&
      validation$history$reproducibility_eligible &&
      validation$current_provenance$reproducibility_eligible,
    parent_history_digest =
      object$continuation_history_digest,
    transition_version = .rqr_static_transition_version()
  )
  segment$provenance$continued_from_checkpoint <- TRUE
  segment$provenance$parent_checkpoint_digest <-
    validation$checkpoint_digest
  segment$provenance$parent_reproducibility_eligible <-
    validation$history$reproducibility_eligible
  segment$provenance$environment_override_used <-
    environment_override_used
  .rqr_validate_static_fit_envelope(segment)
  segment
}

.rqr_static_fit_summary <- function(
    data, beta1_draws, beta2_draws) {
  eta1 <- data$X %*% t(beta1_draws)
  eta2 <- data$X %*% t(beta2_draws)
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  lower_mean <- rowMeans(lower)
  upper_mean <- rowMeans(upper)
  observed <- data$observed
  covered_by_draw <- sweep(
    lower[observed, , drop = FALSE], 1L,
    data$y[observed], `<=`
  ) & sweep(
    upper[observed, , drop = FALSE], 1L,
    data$y[observed], `>=`
  )
  coverage_by_draw <- colMeans(covered_by_draw)
  list(
    beta_root1_mean = colMeans(beta1_draws),
    beta_root2_mean = colMeans(beta2_draws),
    lower_mean = lower_mean,
    upper_mean = upper_mean,
    midpoint_mean = rowMeans(0.5 * (lower + upper)),
    width_mean = rowMeans(upper - lower),
    coverage_posterior_mean_endpoints = mean(
      data$y[observed] >= lower_mean[observed] &
        data$y[observed] <= upper_mean[observed]
    ),
    coverage_draw_mean = mean(coverage_by_draw),
    coverage_draw_quantiles = stats::quantile(
      coverage_by_draw, c(0.05, 0.5, 0.95),
      names = TRUE, type = 8
    ),
    width_mean_scalar = mean(rowMeans(upper - lower)),
    n_total = data$n_total,
    n_observed = data$n_observed
  )
}

.rqr_static_assert_exact_list_object <- function(
    object, expected_class, name) {
  if (!is.list(object) ||
      !identical(class(object), expected_class) ||
      !identical(
        names(attributes(object)), c("names", "class")
      )) {
    stop(
      sprintf(
        "%s must have the exact canonical class and attributes.",
        name
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_static_draw_source <- function(
    object, source_type = c(
      "native_retained_draws", "explicit_unbound"
    )) {
  source_type <- match.arg(source_type)
  native <- identical(source_type, "native_retained_draws")
  list(
    schema_version = .rqr_static_draw_source_schema(),
    source_type = source_type,
    source_fit_schema_version = if (native) {
      object$schema_version
    } else {
      NA_character_
    },
    fit_checkpoint_digest = if (native) {
      object$checkpoint_digest
    } else {
      NA_character_
    },
    retained_draws_digest = if (native) {
      object$retained_draws_digest
    } else {
      NA_character_
    },
    target_digest = if (native) {
      object$checkpoint_state$target_digest
    } else {
      NA_character_
    },
    data_digest = if (native) {
      object$data_contract$data_digest
    } else {
      NA_character_
    },
    design_digest = if (native) {
      object$data_contract$design_digest
    } else {
      NA_character_
    },
    compatibility_fit_schema_version = object$schema_version,
    compatibility_design_digest =
      object$data_contract$design_digest
  )
}

.rqr_static_draw_selection <- function(
    method, n_available, nd_requested, replacement,
    seed, rng_state_before, rng_state_after,
    reproducibility_bound) {
  list(
    schema_version = .rqr_static_draw_selection_schema(),
    method = method,
    n_available = n_available,
    nd_requested = nd_requested,
    replacement = replacement,
    seed_supplied = !is.na(seed),
    seed = seed,
    rng_state_before = rng_state_before,
    rng_state_after = rng_state_after,
    reproducibility_bound = reproducibility_bound
  )
}

.rqr_static_with_rng_state <- function(state, expression) {
  old_kind <- RNGkind()
  old_exists <- exists(
    ".Random.seed", envir = .GlobalEnv, inherits = FALSE
  )
  old_state <- if (old_exists) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    do.call(RNGkind, as.list(unname(old_kind)))
    if (old_exists) {
      assign(".Random.seed", old_state, envir = .GlobalEnv)
    } else if (exists(
      ".Random.seed", envir = .GlobalEnv, inherits = FALSE
    )) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  assign(
    ".Random.seed", .rqr_canonical_rng_state(state),
    envir = .GlobalEnv
  )
  force(expression)
}

.rqr_static_seed_rng_state <- function(seed) {
  old_kind <- RNGkind()
  old_exists <- exists(
    ".Random.seed", envir = .GlobalEnv, inherits = FALSE
  )
  old_state <- if (old_exists) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    do.call(RNGkind, as.list(unname(old_kind)))
    if (old_exists) {
      assign(".Random.seed", old_state, envir = .GlobalEnv)
    } else if (exists(
      ".Random.seed", envir = .GlobalEnv, inherits = FALSE
    )) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  .rqr_rng_state()
}

.rqr_validate_static_draw_selection <- function(
    selection, source_type, draw_index) {
  n_draw <- length(draw_index)
  fields <- c(
    "schema_version", "method", "n_available",
    "nd_requested", "replacement", "seed_supplied", "seed",
    "rng_state_before", "rng_state_after",
    "reproducibility_bound"
  )
  if (!is.list(selection) || is.object(selection) ||
      !identical(names(attributes(selection)), "names") ||
      !identical(names(selection), fields) ||
      !identical(
        selection$schema_version,
        .rqr_static_draw_selection_schema()
      ) ||
      !is.character(selection$method) ||
      length(selection$method) != 1L ||
      is.na(selection$method) ||
      !selection$method %in% c(
        "all_retained", "random_subsample",
        "explicit_unbound"
      ) ||
      !is.logical(selection$seed_supplied) ||
      length(selection$seed_supplied) != 1L ||
      is.na(selection$seed_supplied) ||
      !is.logical(selection$reproducibility_bound) ||
      length(selection$reproducibility_bound) != 1L ||
      is.na(selection$reproducibility_bound)) {
    stop(
      "The static draw-selection contract is noncanonical.",
      call. = FALSE
    )
  }
  seed <- selection$seed
  seed_valid <- is.integer(seed) && length(seed) == 1L &&
    (is.na(seed) ||
      is.finite(seed) && seed == floor(seed) &&
        seed >= 0 && seed <= .Machine$integer.max)
  if (!seed_valid ||
      !identical(selection$seed_supplied, !is.na(seed))) {
    stop(
      "The static draw-selection seed semantics are invalid.",
      call. = FALSE
    )
  }
  before <- .rqr_canonical_rng_state(selection$rng_state_before)
  after <- .rqr_canonical_rng_state(selection$rng_state_after)
  if (!identical(selection$rng_state_before, before) ||
      !identical(selection$rng_state_after, after)) {
    stop(
      "The static draw-selection RNG states are noncanonical.",
      call. = FALSE
    )
  }
  if (identical(selection$method, "explicit_unbound")) {
    valid <- identical(source_type, "explicit_unbound") &&
      identical(selection$n_available, NA_integer_) &&
      identical(selection$nd_requested, as.integer(n_draw)) &&
      identical(selection$replacement, NA) &&
      identical(selection$seed_supplied, FALSE) &&
      identical(selection$seed, NA_integer_) &&
      is.null(before) && is.null(after) &&
      identical(selection$reproducibility_bound, FALSE)
  } else if (identical(selection$method, "all_retained")) {
    valid <- identical(source_type, "native_retained_draws") &&
      is.integer(selection$n_available) &&
      length(selection$n_available) == 1L &&
      identical(selection$n_available, as.integer(n_draw)) &&
      identical(selection$nd_requested, NA_integer_) &&
      identical(selection$replacement, FALSE) &&
      identical(selection$seed_supplied, FALSE) &&
      identical(selection$seed, NA_integer_) &&
      is.null(before) && is.null(after) &&
      identical(selection$reproducibility_bound, TRUE)
  } else {
    valid <- identical(source_type, "native_retained_draws") &&
      is.integer(selection$n_available) &&
      length(selection$n_available) == 1L &&
      !is.na(selection$n_available) &&
      selection$n_available >= 1L &&
      is.integer(selection$nd_requested) &&
      length(selection$nd_requested) == 1L &&
      identical(selection$nd_requested, as.integer(n_draw)) &&
      is.logical(selection$replacement) &&
      length(selection$replacement) == 1L &&
      !is.na(selection$replacement) &&
      identical(
        selection$replacement,
        n_draw > selection$n_available
      ) &&
      !is.null(after) &&
      identical(
        selection$reproducibility_bound,
        !is.null(before)
      )
    if (isTRUE(valid) && !is.null(before)) {
      replay <- .rqr_static_with_rng_state(
        before,
        {
          replay_index <- sample.int(
            selection$n_available,
            selection$nd_requested,
            replace = selection$replacement
          )
          list(
            draw_index = as.integer(replay_index),
            rng_state_after = .rqr_rng_state()
          )
        }
      )
      valid <- identical(replay$draw_index, draw_index) &&
        identical(replay$rng_state_after, after)
    }
    if (isTRUE(valid) && isTRUE(selection$seed_supplied)) {
      valid <- identical(
        .rqr_static_seed_rng_state(selection$seed),
        before
      )
    }
  }
  if (!isTRUE(valid)) {
    stop(
      "The static draw-selection contract violates its source or RNG semantics.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.rqr_validate_static_draw_payload <- function(object, draws) {
  allowed_fields <- c(
    "beta_root1", "beta_root2", "lambda", "draw_index", "nd"
  )
  if (!is.list(draws) || is.object(draws)) {
    stop("draws must be a plain named list.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    draws, "draws", allowed = allowed_fields
  )
  if (!all(c("beta_root1", "beta_root2") %in% names(draws))) {
    stop(
      "draws must contain both root-coefficient matrices.",
      call. = FALSE
    )
  }

  beta1 <- draws$beta_root1
  beta2 <- draws$beta_root2
  if (!is.matrix(beta1) || !is.numeric(beta1) || is.object(beta1) ||
      !is.matrix(beta2) || !is.numeric(beta2) || is.object(beta2)) {
    stop(
      "The supplied root-coefficient draws must be plain numeric matrices.",
      call. = FALSE
    )
  }
  if (!identical(dim(beta1), dim(beta2)) ||
      nrow(beta1) < 1L ||
      ncol(beta1) != ncol(object$X) ||
      any(!is.finite(beta1)) || any(!is.finite(beta2))) {
    stop(
      "The supplied root-coefficient draws are invalid.",
      call. = FALSE
    )
  }
  fitted_names <- object$data_contract$column_names
  if (!is.null(fitted_names) &&
      (!identical(colnames(beta1), fitted_names) ||
        !identical(colnames(beta2), fitted_names))) {
    stop(
      "Named root-coefficient draws must preserve the fitted column order.",
      call. = FALSE
    )
  } else if (is.null(fitted_names) &&
      (!is.null(colnames(beta1)) || !is.null(colnames(beta2)))) {
    stop(
      "Unnamed fitted designs require unnamed root-coefficient columns.",
      call. = FALSE
    )
  }

  storage.mode(beta1) <- "double"
  storage.mode(beta2) <- "double"
  if (is.null(fitted_names)) {
    attributes(beta1) <- list(dim = dim(beta1))
    attributes(beta2) <- list(dim = dim(beta2))
  } else {
    attributes(beta1) <- list(
      dim = dim(beta1),
      dimnames = list(NULL, fitted_names)
    )
    attributes(beta2) <- list(
      dim = dim(beta2),
      dimnames = list(NULL, fitted_names)
    )
  }
  n_draw <- nrow(beta1)
  n_save <- nrow(object$samp.beta_root1)

  lambda <- NULL
  if ("lambda" %in% names(draws) && !is.null(draws$lambda)) {
    lambda <- draws$lambda
    if (!is.numeric(lambda) || is.object(lambda) ||
        !is.null(dim(lambda)) ||
        length(lambda) != n_draw ||
        any(!is.finite(lambda)) || any(lambda <= 0)) {
      stop(
        paste(
          "The supplied lambda draws must be a finite positive numeric",
          "vector with one value per draw."
        ),
        call. = FALSE
      )
    }
    storage.mode(lambda) <- "double"
    names(lambda) <- NULL
  }

  nd <- if ("nd" %in% names(draws)) {
    .rqr_scalar_integer(draws$nd, "draws$nd", 1L)
  } else {
    n_draw
  }
  if (!identical(nd, n_draw)) {
    stop(
      "draws$nd must equal the number of coefficient draws.",
      call. = FALSE
    )
  }

  draw_index <- if ("draw_index" %in% names(draws)) {
    draws$draw_index
  } else {
    rep(NA_integer_, n_draw)
  }
  if (!is.integer(draw_index) || is.object(draw_index) ||
      !is.null(dim(draw_index)) || length(draw_index) != n_draw) {
    stop(
      "draws$draw_index must be an integer vector with one entry per draw.",
      call. = FALSE
    )
  }
  unknown_index <- all(is.na(draw_index))
  if (!unknown_index) {
    if (anyNA(draw_index) ||
        any(draw_index < 1L) || any(draw_index > n_save)) {
      stop(
        paste(
          "Known draws$draw_index values must lie within the fitted",
          "retained-draw range."
        ),
        call. = FALSE
      )
    }
    if (n_draw <= n_save && anyDuplicated(draw_index)) {
      stop(
        paste(
          "draws$draw_index must be unique when sampling no more than",
          "the fitted retained draws."
        ),
        call. = FALSE
      )
    }
    expected_beta1 <- object$samp.beta_root1[
      draw_index, , drop = FALSE
    ]
    expected_beta2 <- object$samp.beta_root2[
      draw_index, , drop = FALSE
    ]
    if (!identical(as.numeric(beta1), as.numeric(expected_beta1)) ||
        !identical(as.numeric(beta2), as.numeric(expected_beta2))) {
      stop(
        paste(
          "Known draws$draw_index values do not identify the supplied",
          "coefficient draws in this fit."
        ),
        call. = FALSE
      )
    }
    if (!is.null(lambda) &&
        !identical(
          lambda,
          as.numeric(object$samp.lambda[draw_index])
        )) {
      stop(
        paste(
          "Known draws$draw_index values do not identify the supplied",
          "lambda draws in this fit."
        ),
        call. = FALSE
      )
    }
  }
  names(draw_index) <- NULL

  list(
    beta_root1 = beta1,
    beta_root2 = beta2,
    lambda = lambda,
    draw_index = draw_index,
    nd = nd
  )
}

.rqr_static_build_draws <- function(
    object, payload, source_type, selection) {
  payload <- .rqr_validate_static_draw_payload(object, payload)
  source <- .rqr_static_draw_source(object, source_type)
  source_bound <- identical(
    source_type, "native_retained_draws"
  )
  selection_bound <- isTRUE(selection$reproducibility_bound)
  out <- list(
    schema_version = .rqr_static_draws_schema(),
    beta_root1 = payload$beta_root1,
    beta_root2 = payload$beta_root2,
    lambda = payload$lambda,
    draw_index = payload$draw_index,
    nd = payload$nd,
    source = source,
    selection = selection,
    source_bound = source_bound,
    reproducibility_eligible =
      source_bound && selection_bound &&
        isTRUE(object$model_spec$reproducibility_eligible),
    promotion_eligible =
      source_bound && selection_bound &&
        isTRUE(object$model_spec$promotion_eligible),
    response_predictive_draws = FALSE,
    payload_digest = .rqr_digest(payload)
  )
  out$semantic_digest <- .rqr_digest(out)
  class(out) <- c("rqr_static_draws", "list")
  out
}

#' Extract source-bound root-coefficient draws from fixed-design RQR MCMC
#'
#' Native extraction returns a versioned, tamper-evident object bound to the
#' exact fit checkpoint, retained-draw contract, generalized-Bayes target,
#' data, fitted design, selected indices, and draw-selection RNG semantics.
#' The extracted roots are interval-function parameters, not
#' posterior-predictive response draws.
#'
#' @param object An `rqr_mcmc` fit.
#' @param nd Number of retained draws to return. `NULL` keeps all draws.
#' @param seed Optional seed used only when draws are subsampled. When omitted,
#'   the complete available ambient RNG state is recorded; a previously
#'   uninitialized ambient RNG makes the extracted selection non-promotable.
#' @param ... Reserved; supplying an argument is an error.
#' @return An exact `rqr_static_draws` envelope containing draw-by-coefficient
#'   matrices, loss-rate draws, selected indices, source hashes, selection
#'   semantics, and explicit no-response-prediction metadata.
#' @export
rqr_posterior_draws.rqr_mcmc <- function(
    object, nd = NULL, seed = NULL, ...) {
  .rqr_reject_dots(list(...), "rqr_posterior_draws.rqr_mcmc")
  .rqr_validate_static_fit_envelope(object)
  beta1 <- object$samp.beta_root1
  beta2 <- object$samp.beta_root2
  n_save <- nrow(beta1)
  lambda <- object$samp.lambda
  if (is.null(nd)) {
    if (!is.null(seed)) {
      stop(
        "seed must be NULL when nd is NULL because no subsampling occurs.",
        call. = FALSE
      )
    }
    index <- seq_len(n_save)
    selection <- .rqr_static_draw_selection(
      method = "all_retained",
      n_available = as.integer(n_save),
      nd_requested = NA_integer_,
      replacement = FALSE,
      seed = NA_integer_,
      rng_state_before = NULL,
      rng_state_after = NULL,
      reproducibility_bound = TRUE
    )
  } else {
    nd <- .rqr_scalar_integer(nd, "nd", 1L)
    seed <- if (is.null(seed)) {
      NA_integer_
    } else {
      .rqr_scalar_integer(seed, "seed", 0L)
    }
    if (!is.na(seed)) set.seed(seed)
    rng_before <- .rqr_rng_state()
    index <- sample.int(
      n_save, nd, replace = nd > n_save
    )
    rng_after <- .rqr_rng_state()
    selection <- .rqr_static_draw_selection(
      method = "random_subsample",
      n_available = as.integer(n_save),
      nd_requested = as.integer(nd),
      replacement = nd > n_save,
      seed = seed,
      rng_state_before = rng_before,
      rng_state_after = rng_after,
      reproducibility_bound = !is.null(rng_before)
    )
  }
  out <- .rqr_static_build_draws(
    object = object,
    payload = list(
      beta_root1 = beta1[index, , drop = FALSE],
      beta_root2 = beta2[index, , drop = FALSE],
      lambda = as.numeric(lambda[index]),
      draw_index = as.integer(index),
      nd = as.integer(length(index))
    ),
    source_type = "native_retained_draws",
    selection = selection
  )
  .rqr_validate_static_draws(object, out)
}

#' Evaluate fixed-design RQR interval roots
#'
#' The two raw linear predictors are ordered at every supplied row. The result
#' contains interval-root functions conditional on `X_new`; it does not
#' contain posterior-predictive response draws.
#'
#' @param object An `rqr_mcmc` fit.
#' @param X_new A finite numeric matrix with the fitted columns in their exact
#'   order. Named fitted designs require identical column names; unnamed
#'   fitted designs require an unnamed matrix.
#' @param nd Number of retained draws to use when `draws` is `NULL`.
#' @param draws Optional exact output from [rqr_posterior_draws()] for this fit.
#'   A plain list containing `beta_root1` and `beta_root2` is retained as an
#'   explicit-evaluation compatibility route, but is normalized to a typed
#'   `explicit_unbound` draw envelope and can never become reproducibility- or
#'   promotion-eligible.
#' @param seed Optional seed used only when draws are subsampled.
#' @param ... Reserved; supplying an argument is an error.
#' @return An exact `rqr_static_prediction` envelope containing ordered
#'   endpoint, midpoint, and width draws and summaries. It binds the complete
#'   evaluation design and numerical content; source and promotion status
#'   distinguish native retained draws from explicit unbound coefficients.
#' @export
predict_interval.rqr_mcmc <- function(
    object, X_new, nd = NULL, draws = NULL, seed = NULL, ...) {
  .rqr_reject_dots(list(...), "predict_interval.rqr_mcmc")
  .rqr_validate_static_fit_envelope(object)
  X_new <- .rqr_validate_prediction_design(object, X_new)
  if (is.null(draws)) {
    draws <- rqr_posterior_draws(
      object, nd = nd, seed = seed
    )
  } else {
    if (!is.null(nd) || !is.null(seed)) {
      stop(
        "nd and seed must be NULL when explicit draws are supplied.",
        call. = FALSE
      )
    }
    draws <- .rqr_validate_static_draws(object, draws)
  }
  out <- .rqr_static_build_prediction(object, X_new, draws)
  .rqr_validate_static_prediction(object, out)
  out
}

.rqr_validate_prediction_design <- function(object, X_new) {
  if (!is.matrix(X_new) || !is.numeric(X_new)) {
    stop(
      paste(
        "X_new must be a nonempty finite numeric matrix with",
        "the fitted number of columns."
      ),
      call. = FALSE
    )
  }
  supplied_names <- colnames(X_new)
  supplied_rows <- rownames(X_new)
  if (!nrow(X_new) ||
      ncol(X_new) != ncol(object$X) ||
      any(!is.finite(X_new))) {
    stop(
      paste(
        "X_new must be a nonempty finite numeric matrix with",
        "the fitted number of columns."
      ),
      call. = FALSE
    )
  }
  storage.mode(X_new) <- "double"
  fitted_names <- object$data_contract$column_names
  if (!is.null(fitted_names)) {
    if (is.null(supplied_names) ||
        !identical(as.character(supplied_names), fitted_names)) {
      stop(
        "Named fitted designs require X_new columns in the exact fitted order.",
        call. = FALSE
      )
    }
  } else if (!is.null(supplied_names)) {
    stop(
      "Unnamed fitted designs require unnamed X_new columns.",
      call. = FALSE
    )
  }
  attributes(X_new) <- if (is.null(supplied_rows) &&
      is.null(supplied_names)) {
    list(dim = dim(X_new))
  } else {
    list(
      dim = dim(X_new),
      dimnames = list(supplied_rows, supplied_names)
    )
  }
  X_new
}

.rqr_validate_static_draws <- function(object, draws) {
  .rqr_validate_static_fit_envelope(object)
  if (inherits(draws, "rqr_static_draws")) {
    .rqr_static_assert_exact_list_object(
      draws, c("rqr_static_draws", "list"),
      "Fixed-design RQR posterior draws"
    )
    fields <- c(
      "schema_version", "beta_root1", "beta_root2", "lambda",
      "draw_index", "nd", "source", "selection", "source_bound",
      "reproducibility_eligible", "promotion_eligible",
      "response_predictive_draws", "payload_digest",
      "semantic_digest"
    )
    if (!identical(names(draws), fields) ||
        !identical(
          draws$schema_version, .rqr_static_draws_schema()
        )) {
      stop(
        "The fixed-design RQR draw envelope is noncanonical.",
        call. = FALSE
      )
    }
    payload <- .rqr_validate_static_draw_payload(
      object,
      draws[c(
        "beta_root1", "beta_root2", "lambda",
        "draw_index", "nd"
      )]
    )
    source <- draws$source
    source_fields <- names(.rqr_static_draw_source(
      object, "native_retained_draws"
    ))
    if (!is.list(source) || is.object(source) ||
        !identical(names(attributes(source)), "names") ||
        !identical(names(source), source_fields) ||
        !identical(
          source$schema_version,
          .rqr_static_draw_source_schema()
        ) ||
        !is.character(source$source_type) ||
        length(source$source_type) != 1L ||
        is.na(source$source_type) ||
        !source$source_type %in% c(
          "native_retained_draws", "explicit_unbound"
        ) ||
        !identical(
          source,
          .rqr_static_draw_source(object, source$source_type)
        )) {
      stop(
        paste(
          "The fixed-design RQR draws are not bound to their",
          "declared source and compatibility design."
        ),
        call. = FALSE
      )
    }
    .rqr_validate_static_draw_selection(
      draws$selection, source$source_type, payload$draw_index
    )
    source_bound <- identical(
      source$source_type, "native_retained_draws"
    )
    if (source_bound &&
        (!identical(
          draws$selection$n_available,
          as.integer(nrow(object$samp.beta_root1))
        ) ||
        identical(draws$selection$method, "all_retained") &&
          !identical(
            payload$draw_index,
            seq_len(nrow(object$samp.beta_root1))
          ))) {
      stop(
        paste(
          "The native draw selection is inconsistent with the",
          "fit retained-draw population."
        ),
        call. = FALSE
      )
    }
    expected_reproducible <- source_bound &&
      isTRUE(draws$selection$reproducibility_bound) &&
      isTRUE(object$model_spec$reproducibility_eligible)
    expected_promotable <- source_bound &&
      isTRUE(draws$selection$reproducibility_bound) &&
      isTRUE(object$model_spec$promotion_eligible)
    semantic_payload <- draws[
      setdiff(fields, "semantic_digest")
    ]
    if (!identical(draws$source_bound, source_bound) ||
        !identical(
          draws$reproducibility_eligible,
          expected_reproducible
        ) ||
        !identical(
          draws$promotion_eligible, expected_promotable
        ) ||
        !identical(draws$response_predictive_draws, FALSE) ||
        !identical(draws$payload_digest, .rqr_digest(payload)) ||
        !identical(
          draws$semantic_digest, .rqr_digest(semantic_payload)
        )) {
      stop(
        paste(
          "The fixed-design RQR draw payload, digest,",
          "eligibility, or no-response semantics are inconsistent."
        ),
        call. = FALSE
      )
    }
    return(draws)
  }
  if (is.object(draws)) {
    stop(
      paste(
        "Explicit draws must be a source-bound rqr_static_draws",
        "object or a plain unbound coefficient list."
      ),
      call. = FALSE
    )
  }
  payload <- .rqr_validate_static_draw_payload(object, draws)
  selection <- .rqr_static_draw_selection(
    method = "explicit_unbound",
    n_available = NA_integer_,
    nd_requested = as.integer(payload$nd),
    replacement = NA,
    seed = NA_integer_,
    rng_state_before = NULL,
    rng_state_after = NULL,
    reproducibility_bound = FALSE
  )
  out <- .rqr_static_build_draws(
    object = object,
    payload = payload,
    source_type = "explicit_unbound",
    selection = selection
  )
  .rqr_validate_static_draws(object, out)
}

.rqr_static_prediction_source <- function(
    object, draws, new_design_digest) {
  list(
    schema_version = .rqr_static_prediction_source_schema(),
    evaluation_fit_checkpoint_digest =
      object$checkpoint_digest,
    evaluation_retained_draws_digest =
      object$retained_draws_digest,
    evaluation_target_digest =
      object$checkpoint_state$target_digest,
    fitted_data_digest = object$data_contract$data_digest,
    fitted_design_digest = object$data_contract$design_digest,
    new_design_digest = new_design_digest,
    draw_semantic_digest = draws$semantic_digest,
    draw_source_type = draws$source$source_type
  )
}

.rqr_static_build_prediction <- function(
    object, X_new, draws) {
  X_new <- .rqr_validate_prediction_design(object, X_new)
  draws <- .rqr_validate_static_draws(object, draws)
  eta1 <- X_new %*% t(draws$beta_root1)
  eta2 <- X_new %*% t(draws$beta_root2)
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  midpoint <- 0.5 * (lower + upper)
  width <- upper - lower
  new_design_digest <- .rqr_digest(list(
    X_new = X_new,
    column_names = colnames(X_new),
    row_names = rownames(X_new),
    fit_design_digest = object$data_contract$design_digest
  ))
  source <- .rqr_static_prediction_source(
    object, draws, new_design_digest
  )
  source_bound <- isTRUE(draws$source_bound)
  out <- list(
    schema_version = .rqr_static_prediction_schema(),
    X_new = X_new,
    lower_draws = lower,
    upper_draws = upper,
    midpoint_draws = midpoint,
    width_draws = width,
    lower_mean = rowMeans(lower),
    upper_mean = rowMeans(upper),
    midpoint_mean = rowMeans(midpoint),
    width_mean = rowMeans(width),
    draws = draws,
    model_spec = object$model_spec,
    source = source,
    fit_checkpoint_digest = object$checkpoint_digest,
    new_design_digest = new_design_digest,
    draw_index = draws$draw_index,
    source_bound = source_bound,
    reproducibility_eligible =
      source_bound &&
        isTRUE(draws$reproducibility_eligible),
    promotion_eligible =
      source_bound && isTRUE(draws$promotion_eligible),
    promotion_status = if (source_bound) {
      "native_source_bound_interval_roots"
    } else {
      "explicit_coefficients_unbound_nonpromotable"
    },
    response_predictive_draws = FALSE,
    interpretation = if (source_bound) {
      paste(
        "Source-bound interval-root functionals conditional on X_new;",
        "no response draw is defined."
      )
    } else {
      paste(
        "Interval-root functionals from explicit unbound coefficients;",
        "the result is non-promotable and no response draw is defined."
      )
    },
    content_digest = .rqr_digest(list(
      X_new = X_new,
      lower_draws = lower,
      upper_draws = upper,
      midpoint_draws = midpoint,
      width_draws = width,
      lower_mean = rowMeans(lower),
      upper_mean = rowMeans(upper),
      midpoint_mean = rowMeans(midpoint),
      width_mean = rowMeans(width)
    ))
  )
  out$semantic_digest <- .rqr_digest(out)
  class(out) <- c("rqr_static_prediction", "list")
  out
}

.rqr_validate_static_prediction <- function(
    object, prediction) {
  .rqr_validate_static_fit_envelope(object)
  .rqr_static_assert_exact_list_object(
    prediction, c("rqr_static_prediction", "list"),
    "Fixed-design RQR prediction"
  )
  fields <- c(
    "schema_version", "X_new", "lower_draws", "upper_draws",
    "midpoint_draws", "width_draws", "lower_mean",
    "upper_mean", "midpoint_mean", "width_mean", "draws",
    "model_spec", "source", "fit_checkpoint_digest",
    "new_design_digest", "draw_index", "source_bound",
    "reproducibility_eligible", "promotion_eligible",
    "promotion_status", "response_predictive_draws",
    "interpretation", "content_digest", "semantic_digest"
  )
  if (!identical(names(prediction), fields) ||
      !identical(
        prediction$schema_version,
        .rqr_static_prediction_schema()
      )) {
    stop(
      "The fixed-design RQR prediction envelope is noncanonical.",
      call. = FALSE
    )
  }
  X_new <- .rqr_validate_prediction_design(
    object, prediction$X_new
  )
  draws <- .rqr_validate_static_draws(
    object, prediction$draws
  )
  expected <- .rqr_static_build_prediction(
    object, X_new, draws
  )
  if (!identical(prediction, expected)) {
    stop(
      paste(
        "The fixed-design RQR prediction roots, input design,",
        "source binding, content digest, or no-response semantics",
        "are inconsistent."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Print a fixed-design RQR MCMC summary
#'
#' @param x An `rqr_mcmc` fit.
#' @param ... Reserved; supplying an argument is an error.
#' @return `x`, invisibly.
#' @export
print.rqr_mcmc <- function(x, ...) {
  .rqr_reject_dots(list(...), "print.rqr_mcmc")
  .rqr_validate_static_fit_envelope(x)
  cat("Ordinary RQR fixed-design MCMC fit\n")
  cat(sprintf(
    "  coverage_level: %.4f\n",
    x$model_spec$coverage_level
  ))
  cat(sprintf(
    "  rate_mode:      %s\n",
    x$model_spec$learning_rate_mode
  ))
  cat(sprintf(
    "  observed rows:  %d/%d\n",
    x$model_spec$n_observed, x$model_spec$n_total
  ))
  if (isTRUE(x$model_spec$learned_inverse_loss_scale)) {
    cat(sprintf(
      "  lambda_mean:    %.4f\n",
      x$model_spec$lambda_summary$mean
    ))
  }
  cat(sprintf(
    "  prior:          %s\n",
    x$model_spec$beta_prior_type
  ))
  cat(sprintf(
    "  numerical repairs: %d\n",
    x$model_spec$numerical_repair_count
  ))
  cat(sprintf(
    "  promotion eligible: %s\n",
    if (isTRUE(x$model_spec$promotion_eligible)) "yes" else "no"
  ))
  cat(sprintf(
    "  draws:          %d\n",
    nrow(x$samp.beta_root1)
  ))
  cat(
    "  interpretation: generalized-Bayes interval roots, not response draws\n"
  )
  invisible(x)
}
