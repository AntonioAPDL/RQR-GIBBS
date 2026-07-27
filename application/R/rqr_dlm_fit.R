.rqr_state_ordinates <- function(FF, path) colSums(FF * path)

.rqr_init_state_paths <- function(y, FF, m0, coverage_level, init = list()) {
  p <- nrow(FF)
  T <- ncol(FF)
  supplied1 <- init$state_root1 %||% init$theta1 %||% NULL
  supplied2 <- init$state_root2 %||% init$theta2 %||% NULL
  if (!is.null(supplied1) || !is.null(supplied2)) {
    if (is.null(supplied1) || is.null(supplied2)) stop("init must provide both state paths.", call. = FALSE)
    if (!is.matrix(supplied1) || !is.numeric(supplied1) ||
        is.object(supplied1) ||
        !is.matrix(supplied2) || !is.numeric(supplied2) ||
        is.object(supplied2) ||
        !identical(dim(supplied1), c(p, T)) ||
        !identical(dim(supplied2), c(p, T)) ||
        any(!is.finite(supplied1)) || any(!is.finite(supplied2))) {
      stop(
        "Initial state paths must be finite plain numeric p x T matrices.",
        call. = FALSE
      )
    }
    storage.mode(supplied1) <- "double"
    storage.mode(supplied2) <- "double"
    return(list(theta1 = supplied1, theta2 = supplied2))
  }
  observed <- !is.na(y)
  alpha <- rqr_constants(coverage_level)$alpha
  probs <- c((1 - alpha) / 2, 1 - (1 - alpha) / 2)
  qs <- as.numeric(stats::quantile(y[observed], probs = probs, names = FALSE, type = 8))
  Fobs <- FF[, observed, drop = FALSE]
  base <- drop(crossprod(Fobs, m0))
  precision <- tcrossprod(Fobs) + diag(1e-6, p)
  correction <- function(target) {
    rhs <- Fobs %*% rep(target, ncol(Fobs)) - Fobs %*% base
    m0 + solve(precision, rhs)
  }
  b1 <- correction(qs[1L])
  b2 <- correction(qs[2L])
  list(
    theta1 = matrix(rep(b1, T), p, T),
    theta2 = matrix(rep(b2, T), p, T)
  )
}

.rqr_dlm_evolution <- function(
    mode, model, expanded, y, W, df, dim.df, reference_variance,
    reference_design, component_templates, evolution_scale_prior,
    evolution_scale_initial, evolution_spec, numerical_policy, jitter_ladder) {
  if (!is.null(evolution_spec)) {
    if (!inherits(evolution_spec, "rqr_evolution") || is.null(evolution_spec$mode)) {
      stop("evolution_spec must be an rqr_evolution object.", call. = FALSE)
    }
    mode <- as.character(evolution_spec$mode)[1L]
    if (!mode %in% c("fixed_W", "discount_template", "component_scale", "adaptive_discount")) {
      stop("evolution_spec has an unsupported mode.", call. = FALSE)
    }
    if (identical(mode, "component_scale") &&
        !identical(as.integer(evolution_spec$component_dims), as.integer(expanded$component_dims))) {
      stop("component_scale dimensions must match model$component_dims.", call. = FALSE)
    }
    return(evolution_spec)
  }
  mode <- match.arg(
    mode, c("fixed_W", "discount_template", "component_scale", "adaptive_discount")
  )
  p <- expanded$p
  T <- expanded$n_time
  if (mode == "fixed_W") {
    if (is.null(W)) stop("evolution_mode='fixed_W' requires W.", call. = FALSE)
    return(rqr_evolution_fixed(.rqr_expand_cube(W, T, p, "W")))
  }
  if (mode == "component_scale") {
    if (is.null(component_templates)) {
      stop("evolution_mode='component_scale' requires component_templates.", call. = FALSE)
    }
    return(rqr_evolution_component_scale(
      templates = component_templates,
      component_dims = expanded$component_dims,
      component_names = expanded$component_names,
      prior = evolution_scale_prior,
      initial = evolution_scale_initial
    ))
  }
  if (is.null(dim.df)) dim.df <- model$component_dims %||% p
  if (is.null(df)) stop("Discount evolution requires df.", call. = FALSE)
  if (mode == "adaptive_discount") {
    return(rqr_evolution_adaptive_working(df, dim.df))
  }
  reference_source <- if (is.null(reference_variance)) {
    "training_response_variance"
  } else {
    "user_supplied"
  }
  if (is.null(reference_variance)) {
    yy <- y[!is.na(y)]
    empirical_variance <- stats::var(yy)
    if (!is.finite(empirical_variance) || empirical_variance <= 0) empirical_variance <- 1
    reference_variance <- max(empirical_variance, sqrt(.Machine$double.eps))
  }
  template <- rqr_freeze_discount_template(
    model = model, n_time = T, df = df, dim.df = dim.df,
    reference_variance = reference_variance,
    reference_design = reference_design,
    numerical_policy = numerical_policy,
    jitter_ladder = jitter_ladder
  )
  template$reference_source <- reference_source
  template$empirical_bayes <- identical(reference_source, "training_response_variance")
  template
}

.rqr_bind_ffbs_repairs <- function(records, diagnostics, iteration, root) {
  current <- diagnostics$repair_records
  if (is.null(current) || !nrow(current)) return(records)
  current$iteration <- as.integer(iteration)
  current$root <- as.character(root)
  current <- current[, c(
    "iteration", "root", "stage", "time", "strategy", "jitter",
    "relative_jitter", "min_eigenvalue", "matrix_scale", "jitter_scale",
    "absolute_jitter_fallback", "clamped_eigenvalues"
  )]
  if (is.null(records)) current else rbind(records, current)
}

.rqr_dlm_provenance_matrices <- function(expanded, evolution) {
  list(
    FF = expanded$FF,
    GG = expanded$GG,
    C0 = expanded$C0,
    evolution_W = evolution$W %||% NULL,
    evolution_templates = evolution$templates %||% NULL,
    evolution_discount = evolution$D %||% NULL
  )
}

.rqr_dlm_model_contract <- function(expanded) {
  list(
    FF = expanded$FF,
    GG = expanded$GG,
    m0 = expanded$m0,
    C0 = expanded$C0,
    component_dims = expanded$component_dims,
    component_names = expanded$component_names,
    state_dimension = expanded$p,
    n_time = expanded$n_time
  )
}

.rqr_dlm_target_contract <- function(
    coverage_level, learning_rate_mode, fixed_learning_rate,
    loss_reference_scale, lambda_prior, numerical_policy, jitter_ladder) {
  list(
    loss_name = "rqr_residual_product_check_loss",
    coverage_level = coverage_level,
    learning_rate_mode = learning_rate_mode,
    fixed_learning_rate = fixed_learning_rate,
    loss_reference_scale = loss_reference_scale,
    lambda_prior = lambda_prior,
    numerical_policy = numerical_policy,
    jitter_ladder = as.numeric(jitter_ladder),
    root_priors_exchangeable = TRUE,
    root_swap_move = TRUE
  )
}

.rqr_dlm_evolution_contract <- function(evolution) {
  unclass(evolution)
}

.rqr_dlm_provenance_objects <- function(expanded, evolution, target_contract) {
  list(
    model = .rqr_dlm_model_contract(expanded),
    target = target_contract,
    evolution = .rqr_dlm_evolution_contract(evolution)
  )
}

.rqr_dlm_coverage_summary <- function(y, observed, lower, upper) {
  lower_mean <- rowMeans(lower)
  upper_mean <- rowMeans(upper)
  lower_observed <- lower[observed, , drop = FALSE]
  upper_observed <- upper[observed, , drop = FALSE]
  y_observed <- y[observed]
  covered <- sweep(lower_observed, 1L, y_observed, `<=`) &
    sweep(upper_observed, 1L, y_observed, `>=`)
  coverage_by_draw <- colMeans(covered)
  list(
    lower_mean = lower_mean,
    upper_mean = upper_mean,
    midpoint_mean = rowMeans(0.5 * (lower + upper)),
    width_mean = rowMeans(upper - lower),
    coverage_posterior_mean_endpoints = mean(
      y_observed >= lower_mean[observed] & y_observed <= upper_mean[observed]
    ),
    coverage_draw_mean = mean(coverage_by_draw),
    coverage_draw_quantiles = stats::quantile(
      coverage_by_draw, c(0.05, 0.5, 0.95), names = TRUE, type = 8
    ),
    width_mean_scalar = mean(rowMeans(upper - lower))
  )
}

.rqr_dlm_continuation_token <- new.env(parent = emptyenv())

.rqr_dlm_continuation_init_fields <- function() {
  c(
    "completed_iterations", "continued_from_checkpoint",
    "parent_cumulative_numerical_repair_count",
    "parent_chain_history_numerically_exact",
    "parent_promotion_eligible", "continuation_control"
  )
}

.rqr_dlm_schedule_schema <- function() {
  "rqrgibbs_dlm_segment_schedule/1.0.0"
}

.rqr_dlm_schedule_prefix <- function(segments) {
  list(
    schema_version = .rqr_dlm_schedule_schema(),
    fit_schema_version = .rqr_schema_version(),
    generation = as.integer(length(segments) - 1L),
    segments = segments
  )
}

.rqr_validate_dlm_schedule_value <- function(
    contract, stored_digest = NULL) {
  contract_fields <- c(
    "schema_version", "fit_schema_version", "generation", "segments"
  )
  segment_fields <- c(
    "generation", "start_completed_iterations", "n_burn",
    "n_retained_draws", "thin", "raw_iterations",
    "end_completed_iterations", "ends_on_retained_draw",
    "parent_checkpoint_digest", "parent_schedule_digest",
    "checkpoint_digest"
  )
  if (!is.list(contract) ||
      !identical(names(contract), contract_fields) ||
      !identical(contract$schema_version, .rqr_dlm_schedule_schema()) ||
      !identical(contract$fit_schema_version, .rqr_schema_version()) ||
      !is.list(contract$segments) || !length(contract$segments)) {
    stop("The DLM segment-schedule contract is unsupported.",
         call. = FALSE)
  }
  if (!is.null(stored_digest) &&
      (!is.character(stored_digest) ||
       length(stored_digest) != 1L || is.na(stored_digest) ||
       !grepl("^[0-9a-f]{64}$", stored_digest) ||
       !identical(.rqr_digest(contract), stored_digest))) {
    stop("The DLM segment-schedule digest is invalid.", call. = FALSE)
  }
  generation <- tryCatch(
    .rqr_history_count(
      contract$generation, "segment_schedule_contract$generation"
    ),
    error = function(error) NA_integer_
  )
  if (is.na(generation) ||
      !identical(generation, as.integer(length(contract$segments) - 1L))) {
    stop("The DLM segment-schedule generation is invalid.", call. = FALSE)
  }

  previous_end <- 0L
  previous_checkpoint <- NA_character_
  for (index in seq_along(contract$segments)) {
    segment <- contract$segments[[index]]
    if (!is.list(segment) ||
        !identical(names(segment), segment_fields)) {
      stop(
        sprintf(
          "DLM segment schedule %d is incomplete.",
          as.integer(index - 1L)
        ),
        call. = FALSE
      )
    }
    count <- function(field, minimum = 0L) {
      tryCatch(
        .rqr_scalar_integer(
          segment[[field]], sprintf("segment_schedule$%s", field),
          minimum = minimum
        ),
        error = function(error) NA_integer_
      )
    }
    segment_generation <- count("generation")
    start <- count("start_completed_iterations")
    burn <- count("n_burn")
    retained <- count("n_retained_draws", 1L)
    thin <- count("thin", 1L)
    raw <- count("raw_iterations", 1L)
    end <- count("end_completed_iterations", 1L)
    expected_raw <- as.double(burn) +
      as.double(retained) * as.double(thin)
    expected_end <- as.double(start) + expected_raw
    arithmetic_valid <- is.finite(expected_raw) &&
      is.finite(expected_end) &&
      expected_raw <= .Machine$integer.max &&
      expected_end <= .Machine$integer.max &&
      identical(raw, as.integer(expected_raw)) &&
      identical(end, as.integer(expected_end))
    checkpoint_valid <-
      is.character(segment$checkpoint_digest) &&
      length(segment$checkpoint_digest) == 1L &&
      !is.na(segment$checkpoint_digest) &&
      grepl("^[0-9a-f]{64}$", segment$checkpoint_digest)
    links_valid <- if (index == 1L) {
      is.character(segment$parent_checkpoint_digest) &&
        length(segment$parent_checkpoint_digest) == 1L &&
        is.na(segment$parent_checkpoint_digest) &&
        is.character(segment$parent_schedule_digest) &&
        length(segment$parent_schedule_digest) == 1L &&
        is.na(segment$parent_schedule_digest)
    } else {
      identical(
        segment$parent_checkpoint_digest, previous_checkpoint
      ) &&
        identical(
          segment$parent_schedule_digest,
          .rqr_digest(.rqr_dlm_schedule_prefix(
            contract$segments[seq_len(index - 1L)]
          ))
        )
    }
    endpoint_valid <-
      is.logical(segment$ends_on_retained_draw) &&
      length(segment$ends_on_retained_draw) == 1L &&
      !is.na(segment$ends_on_retained_draw) &&
      isTRUE(segment$ends_on_retained_draw)
    if (anyNA(c(
        segment_generation, start, burn, retained, thin, raw, end
      )) ||
        !identical(segment_generation, as.integer(index - 1L)) ||
        !identical(start, previous_end) ||
        (index > 1L && burn != 0L) ||
        !arithmetic_valid || !checkpoint_valid ||
        !links_valid || !endpoint_valid) {
      stop(
        sprintf(
          "DLM segment schedule %d is structurally invalid.",
          as.integer(index - 1L)
        ),
        call. = FALSE
      )
    }
    previous_end <- end
    previous_checkpoint <- segment$checkpoint_digest
  }
  invisible(contract)
}

.rqr_make_dlm_schedule_contract <- function(
    start_completed_iterations, n_burn, n_retained_draws, thin,
    checkpoint_digest, parent = NULL, parent_digest = NULL) {
  start_completed_iterations <- .rqr_history_count(
    start_completed_iterations, "start_completed_iterations"
  )
  n_burn <- .rqr_history_count(n_burn, "n_burn")
  n_retained_draws <- .rqr_scalar_integer(
    n_retained_draws, "n_retained_draws", 1L
  )
  thin <- .rqr_scalar_integer(thin, "thin", 1L)
  if (!is.character(checkpoint_digest) ||
      length(checkpoint_digest) != 1L ||
      is.na(checkpoint_digest) ||
      !grepl("^[0-9a-f]{64}$", checkpoint_digest)) {
    stop("checkpoint_digest must be a complete SHA-256 digest.",
         call. = FALSE)
  }
  raw_iterations <- .rqr_history_count(
    as.double(n_burn) +
      as.double(n_retained_draws) * as.double(thin),
    "raw_iterations"
  )
  end_completed_iterations <- .rqr_history_count(
    as.double(start_completed_iterations) +
      as.double(raw_iterations),
    "end_completed_iterations"
  )
  if (is.null(parent)) {
    if (!is.null(parent_digest) || start_completed_iterations != 0L) {
      stop(
        "An initial DLM schedule must start at zero without a parent.",
        call. = FALSE
      )
    }
    generation <- 0L
    segments <- list()
    parent_checkpoint_digest <- NA_character_
    stored_parent_digest <- NA_character_
  } else {
    .rqr_validate_dlm_schedule_value(parent, parent_digest)
    if (n_burn != 0L) {
      stop("A continued DLM schedule cannot add burn-in.",
           call. = FALSE)
    }
    generation <- .rqr_history_count(
      as.double(parent$generation) + 1,
      "continued DLM schedule generation"
    )
    segments <- parent$segments
    prior <- utils::tail(segments, 1L)[[1L]]
    if (!identical(
        start_completed_iterations, prior$end_completed_iterations
      )) {
      stop(
        "A continued DLM schedule must start at the parent end count.",
        call. = FALSE
      )
    }
    parent_checkpoint_digest <- prior$checkpoint_digest
    stored_parent_digest <- parent_digest
  }
  segment <- list(
    generation = generation,
    start_completed_iterations = start_completed_iterations,
    n_burn = n_burn,
    n_retained_draws = n_retained_draws,
    thin = thin,
    raw_iterations = raw_iterations,
    end_completed_iterations = end_completed_iterations,
    ends_on_retained_draw = TRUE,
    parent_checkpoint_digest = parent_checkpoint_digest,
    parent_schedule_digest = stored_parent_digest,
    checkpoint_digest = checkpoint_digest
  )
  contract <- .rqr_dlm_schedule_prefix(c(segments, list(segment)))
  .rqr_validate_dlm_schedule_value(contract)
  contract
}

#' Fit a dynamic RQR interval-root model with alternating FFBS
#'
#' This sampler targets root trajectories under an exponentiated RQR loss and
#' Gaussian state priors. It does not define a response likelihood. Fixed `W`,
#' frozen discount templates, and component-scale evolution priors define exact
#' generalized-posterior samplers when no numerical repair is used. Adaptive
#' discount recursion is an explicitly experimental working update.
#'
#' @param y Response vector. `NA` values are treated as missing observations.
#' @param model An [rqr_as_dlm_model()] or exdqlm-compatible model.
#' @param coverage_level Interval coverage target in `(0,1)`.
#' @param evolution_mode One of `"fixed_W"`, `"discount_template"`,
#'   `"component_scale"`, or `"adaptive_discount"`.
#' @param evolution_spec Optional previously constructed evolution object. Used
#'   for exact continuation and overrides the other evolution arguments.
#' @param W Fixed evolution covariance matrix or cube.
#' @param df,dim.df Component discounts and their state dimensions.
#' @param reference_variance,reference_design Inputs used to freeze a discount
#'   template before MCMC.
#' @param component_templates Fixed SPD templates for exact component scales.
#' @param evolution_scale_prior Inverse-Gamma shape and rate for component
#'   evolution multipliers.
#' @param evolution_scale_initial Initial positive component multipliers.
#' @param learning_rate Fixed generalized-Bayes learning rate `omega_R`.
#' @param lambda_initial Initial inverse loss scale for learned modes.
#' @param loss_reference_scale Positive reference scale `s_L`. It does not
#'   alter fixed `learning_rate`; learned modes use `lambda/s_L`.
#' @param learning_rate_mode One of `"fixed_rate"`,
#'   `"learned_pseudoresidual_normalized"`, or `"learned_pure"`. Legacy mode
#'   spellings are accepted and normalized to these names. `"learned_pure"`
#'   remains executable only for diagnostic compatibility; it is outside the
#'   ordinary-v1 scope, nonpromotable, and noncontinuable.
#' @param lambda_prior Gamma shape--rate prior without a custom power field.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param provenance_control Optional primary-repository provenance plus named
#'   `external_repositories`. Repository specifications contain `repo_root`, a
#'   complete 40-character `expected_git_commit`, and optional runtime package
#'   and attestation fields.
#' @param mcmc_control A fully named list containing supported iteration, seed,
#'   storage, backend, progress, jitter, and optional exact partially
#'   collapsed/interwoven component-scale controls. Unknown fields and
#'   coercive scalar values are rejected.
#' @param init A fully named list of optional initial state paths, latent
#'   scales, lambda, evolution scales, time-zero states, and RNG state.
#'   `theta1` and `theta2` remain supported as legacy aliases for
#'   `state_root1` and `state_root2`; supplying both forms is an error.
#'   Continuation-history fields are private to [rqr_dlm_continue()].
#' @return An `rqr_dlm_mcmc` object.
#' @export
rqr_dlm_fit <- function(
    y, model, coverage_level,
    evolution_mode = c(
      "fixed_W", "discount_template", "component_scale", "adaptive_discount"
    ),
    evolution_spec = NULL,
    W = NULL, df = NULL, dim.df = NULL,
    reference_variance = NULL, reference_design = NULL,
    component_templates = NULL,
    evolution_scale_prior = list(shape = 2, rate = 1),
    evolution_scale_initial = 1,
    learning_rate = 1, lambda_initial = 1, loss_reference_scale = 1,
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized", "learned_pure"
    ),
    lambda_prior = list(shape = 4, rate = 4),
    numerical_policy = c("fail", "record_repair"),
    provenance_control = list(),
    mcmc_control = list(), init = list()) {
  if (is.list(init) &&
      any(.rqr_dlm_continuation_init_fields() %in% names(init))) {
    stop(
      paste(
        "Continuation-only init fields are private to",
        "rqr_dlm_continue()."
      ),
      call. = FALSE
    )
  }
  .rqr_dlm_fit_impl(
    y = y, model = model, coverage_level = coverage_level,
    evolution_mode = evolution_mode,
    evolution_spec = evolution_spec,
    W = W, df = df, dim.df = dim.df,
    reference_variance = reference_variance,
    reference_design = reference_design,
    component_templates = component_templates,
    evolution_scale_prior = evolution_scale_prior,
    evolution_scale_initial = evolution_scale_initial,
    learning_rate = learning_rate,
    lambda_initial = lambda_initial,
    loss_reference_scale = loss_reference_scale,
    learning_rate_mode = learning_rate_mode,
    lambda_prior = lambda_prior,
    numerical_policy = numerical_policy,
    provenance_control = provenance_control,
    mcmc_control = mcmc_control,
    init = init,
    .continuation_token = NULL
  )
}

.rqr_dlm_fit_impl <- function(
    y, model, coverage_level,
    evolution_mode = c(
      "fixed_W", "discount_template", "component_scale", "adaptive_discount"
    ),
    evolution_spec = NULL,
    W = NULL, df = NULL, dim.df = NULL,
    reference_variance = NULL, reference_design = NULL,
    component_templates = NULL,
    evolution_scale_prior = list(shape = 2, rate = 1),
    evolution_scale_initial = 1,
    learning_rate = 1, lambda_initial = 1, loss_reference_scale = 1,
    learning_rate_mode = c(
      "fixed_rate", "learned_pseudoresidual_normalized", "learned_pure"
    ),
    lambda_prior = list(shape = 4, rate = 4),
    numerical_policy = c("fail", "record_repair"),
    provenance_control = list(),
    mcmc_control = list(), init = list(),
    .continuation_token = NULL) {
  if (!is.numeric(y) || is.object(y) || !is.null(dim(y)) ||
      !length(y) || !any(!is.na(y)) || any(is.nan(y)) ||
      any(is.infinite(y))) {
    stop(
      paste(
        "y must be a plain numeric vector containing at least one finite",
        "observation; missing values must be NA."
      ),
      call. = FALSE
    )
  }
  y <- as.numeric(y)
  names(y) <- NULL
  if (!is.list(init)) stop("init must be a list.", call. = FALSE)
  .rqr_validate_named_list_fields(
    init, "init",
    c(
      "state_root1", "state_root2", "theta1", "theta2",
      "latent_v", "lambda", "evolution_scale",
      "theta0_root1", "theta0_root2", "rng_state",
      "completed_iterations", "continued_from_checkpoint",
      "parent_cumulative_numerical_repair_count",
      "parent_chain_history_numerically_exact",
      "parent_promotion_eligible", "continuation_control"
    )
  )
  if (all(c("state_root1", "theta1") %in% names(init)) ||
      all(c("state_root2", "theta2") %in% names(init))) {
    stop(
      "init cannot supply both canonical state paths and their legacy aliases.",
      call. = FALSE
    )
  }
  continued_from_checkpoint <- identical(
    .continuation_token, .rqr_dlm_continuation_token
  )
  declared_continuation <- .rqr_scalar_logical(
    init$continued_from_checkpoint %||% FALSE,
    "init$continued_from_checkpoint"
  )
  if (!identical(
      declared_continuation, continued_from_checkpoint
    ) ||
      (!continued_from_checkpoint &&
       any(.rqr_dlm_continuation_init_fields() %in% names(init)))) {
    stop(
      paste(
        "Continuation-only init fields require the private validated",
        "rqr_dlm_continue() worker boundary."
      ),
      call. = FALSE
    )
  }
  continuation_control <- init$continuation_control %||% list()
  if (!is.list(continuation_control)) {
    stop("The private DLM continuation control must be a list.",
         call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    continuation_control, "init$continuation_control",
    c(
      "parent_schedule_contract", "parent_schedule_digest"
    )
  )
  parent_chain_numerically_exact <- .rqr_scalar_logical(
    init$parent_chain_history_numerically_exact %||% TRUE,
    "init$parent_chain_history_numerically_exact"
  )
  parent_promotion_eligible_input <- .rqr_scalar_logical(
    init$parent_promotion_eligible %||%
      if (continued_from_checkpoint) FALSE else TRUE,
    "init$parent_promotion_eligible"
  )
  completed_offset_input <- .rqr_history_count(
    init$completed_iterations %||% 0L,
    "init$completed_iterations"
  )
  parent_cumulative_repairs_input <- .rqr_history_count(
    init$parent_cumulative_numerical_repair_count %||% 0L,
    "init$parent_cumulative_numerical_repair_count"
  )
  if (!continued_from_checkpoint &&
      any(c(
        "completed_iterations",
        "parent_cumulative_numerical_repair_count",
        "parent_chain_history_numerically_exact",
        "parent_promotion_eligible"
      ) %in% names(init))) {
    stop(
      paste(
        "Continuation-history init fields require",
        "init$continued_from_checkpoint=TRUE."
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
      "n_burn", "n_mcmc", "thin", "seed", "backend",
      "store_state_draws", "store_latent_draws", "verbose",
      "progress_every", "jitter_ladder",
      "component_scale_collapsed_update",
      "component_scale_interweave",
      "component_scale_slice_width",
      "component_scale_slice_max_steps",
      "component_scale_slice_max_shrink",
      "component_scale_slice_sweeps",
      "component_scale_interweave_cycles"
    )
  )
  model <- rqr_as_dlm_model(model)
  expanded <- .rqr_expand_model(model, length(y))
  T <- length(y)
  p <- expanded$p
  observed <- !is.na(y)
  n_obs <- sum(observed)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  provenance_control <- .rqr_provenance_control(provenance_control)
  jitter_ladder <- mcmc_control$jitter_ladder %||%
    c(0, 1e-12, 1e-10, 1e-8, 1e-6)
  if (!is.numeric(jitter_ladder) || is.object(jitter_ladder) ||
      !is.null(dim(jitter_ladder)) || !length(jitter_ladder) ||
      any(!is.finite(jitter_ladder)) || any(jitter_ladder < 0)) {
    stop(
      paste(
        "mcmc_control$jitter_ladder must be a nonempty plain numeric",
        "vector of finite nonnegative values."
      ),
      call. = FALSE
    )
  }
  jitter_ladder <- as.numeric(jitter_ladder)
  .rqr_jitter_ladder(numerical_policy, jitter_ladder)

  learning_rate_mode <- .rqr_learning_rate_mode(learning_rate_mode)
  lambda_prior <- .rqr_lambda_prior(lambda_prior, learning_rate_mode)
  loss_reference_scale <- .rqr_scalar_numeric(
    loss_reference_scale, "loss_reference_scale",
    lower = 0, lower_open = TRUE
  )
  learning_rate <- .rqr_scalar_numeric(
    learning_rate, "learning_rate",
    lower = 0, lower_open = TRUE
  )
  lambda_initial <- .rqr_scalar_numeric(
    init$lambda %||% lambda_initial, "lambda_initial",
    lower = 0, lower_open = TRUE
  )
  learn_lambda <- learning_rate_mode != "fixed_rate"
  lambda <- if (learn_lambda) lambda_initial else learning_rate * loss_reference_scale
  constants <- rqr_constants(coverage_level, lambda / loss_reference_scale)

  requested_mode <- if (is.null(evolution_spec)) match.arg(evolution_mode) else evolution_spec$mode
  evolution <- .rqr_dlm_evolution(
    requested_mode, model, expanded, y, W, df, dim.df,
    reference_variance, reference_design, component_templates,
    evolution_scale_prior, evolution_scale_initial, evolution_spec,
    numerical_policy, jitter_ladder
  )
  evolution_mode <- evolution$mode
  if (!isTRUE(evolution$exact_joint_target)) {
    warning(
      paste(
        "adaptive_discount is an experimental working/sequential recursion,",
        "not an exact Gibbs sampler for a declared fixed joint target."
      ),
      call. = FALSE
    )
  }

  n_burn <- .rqr_scalar_integer(mcmc_control$n_burn %||% 500L, "mcmc_control$n_burn", 0L)
  n_keep <- .rqr_scalar_integer(mcmc_control$n_mcmc %||% 1000L, "mcmc_control$n_mcmc", 1L)
  thin <- .rqr_scalar_integer(mcmc_control$thin %||% 1L, "mcmc_control$thin", 1L)
  seed <- mcmc_control$seed %||% NULL
  if (!is.null(seed)) {
    seed <- .rqr_scalar_integer(seed, "mcmc_control$seed", 0L)
  }
  if (!is.null(seed) && !is.null(init$rng_state)) {
    stop(
      "Supply mcmc_control$seed or init$rng_state, not both.",
      call. = FALSE
    )
  }
  backend_requested <- mcmc_control$backend %||% "cpp"
  if (!is.character(backend_requested) ||
      length(backend_requested) != 1L ||
      is.na(backend_requested) ||
      !backend_requested %in% c("cpp", "R", "auto")) {
    stop(
      "mcmc_control$backend must be exactly one of 'cpp', 'R', or 'auto'.",
      call. = FALSE
    )
  }
  backend_resolved <- .rqr_resolve_ffbs_backend(backend_requested)
  store_state_draws <- .rqr_scalar_logical(
    mcmc_control$store_state_draws %||% FALSE,
    "mcmc_control$store_state_draws"
  )
  store_latent_draws <- .rqr_scalar_logical(
    mcmc_control$store_latent_draws %||% FALSE,
    "mcmc_control$store_latent_draws"
  )
  verbose <- .rqr_scalar_logical(
    mcmc_control$verbose %||% FALSE,
    "mcmc_control$verbose"
  )
  progress_every <- .rqr_scalar_integer(
    mcmc_control$progress_every %||% 100L, "mcmc_control$progress_every", 1L
  )

  paths <- .rqr_init_state_paths(y, expanded$FF, expanded$m0, coverage_level, init)
  theta1 <- paths$theta1
  theta2 <- paths$theta2
  v_initial <- init$latent_v %||% loss_reference_scale / lambda
  if (!is.numeric(v_initial) || is.object(v_initial) ||
      !is.null(dim(v_initial)) ||
      !length(v_initial) %in% c(1L, T) ||
      any(!is.finite(v_initial)) || any(v_initial <= 0)) {
    stop(
      paste(
        "init$latent_v must be a plain numeric, finite, positive vector",
        "that is scalar or length(y)."
      ),
      call. = FALSE
    )
  }
  v_initial <- as.numeric(v_initial)
  v <- rep_len(v_initial, T)
  component_mode <- identical(evolution_mode, "component_scale")
  component_scale_interweave <- .rqr_scalar_logical(
    mcmc_control$component_scale_interweave %||% FALSE,
    "mcmc_control$component_scale_interweave"
  )
  if (isTRUE(component_scale_interweave) && !component_mode) {
    stop(
      "Component-scale interweaving requires evolution_mode='component_scale'.",
      call. = FALSE
    )
  }
  component_scale_collapsed_update <- .rqr_scalar_logical(
    mcmc_control$component_scale_collapsed_update %||% FALSE,
    "mcmc_control$component_scale_collapsed_update"
  )
  if (isTRUE(component_scale_collapsed_update) && !component_mode) {
    stop(
      paste(
        "The partially collapsed scale update requires",
        "evolution_mode='component_scale'."
      ),
      call. = FALSE
    )
  }
  component_scale_slice_width <- .rqr_scalar_numeric(
    mcmc_control$component_scale_slice_width %||% 1,
    "mcmc_control$component_scale_slice_width",
    lower = 0, lower_open = TRUE
  )
  component_scale_slice_max_steps <- .rqr_scalar_integer(
    mcmc_control$component_scale_slice_max_steps %||% 100L,
    "mcmc_control$component_scale_slice_max_steps", 1L
  )
  component_scale_slice_max_shrink <- .rqr_scalar_integer(
    mcmc_control$component_scale_slice_max_shrink %||% 1000L,
    "mcmc_control$component_scale_slice_max_shrink", 1L
  )
  component_scale_slice_sweeps <- .rqr_scalar_integer(
    mcmc_control$component_scale_slice_sweeps %||% 1L,
    "mcmc_control$component_scale_slice_sweeps", 1L
  )
  component_scale_interweave_cycles <- .rqr_scalar_integer(
    mcmc_control$component_scale_interweave_cycles %||% 1L,
    "mcmc_control$component_scale_interweave_cycles", 1L
  )
  component_scale_kernel_contract <- list(
    one_root_partially_collapsed =
      component_mode && isTRUE(component_scale_collapsed_update),
    collapsed_integrated_root = if (
        component_mode && isTRUE(component_scale_collapsed_update)
      ) {
      "root1"
    } else {
      "none"
    },
    centered_inverse_gamma = component_mode,
    noncentered_slice_interweave =
      component_mode && isTRUE(component_scale_interweave),
    interweave_cycles = component_scale_interweave_cycles,
    slice_width = component_scale_slice_width,
    slice_sweeps_per_cycle = component_scale_slice_sweeps,
    slice_max_steps = component_scale_slice_max_steps,
    slice_max_shrink = component_scale_slice_max_shrink,
    target_change = FALSE
  )
  time0_completion_mode <- component_mode ||
    (
      store_state_draws &&
        evolution_mode %in% c("fixed_W", "discount_template")
    )
  q_evolution <- if (component_mode) {
    init$evolution_scale %||% evolution$initial
  } else {
    numeric(0)
  }
  if (component_mode &&
      (!is.numeric(q_evolution) || is.object(q_evolution) ||
       !is.null(dim(q_evolution)) ||
       length(q_evolution) != length(evolution$component_dims) ||
       any(!is.finite(q_evolution)) || any(q_evolution <= 0))) {
    stop(
      paste(
        "Initial component evolution scales must be a plain numeric,",
        "finite, positive vector with one value per component."
      ),
      call. = FALSE
    )
  }
  q_evolution <- as.numeric(q_evolution)
  theta01 <- init$theta0_root1 %||% expanded$m0
  theta02 <- init$theta0_root2 %||% expanded$m0
  if (!is.numeric(theta01) || is.object(theta01) ||
      !is.null(dim(theta01)) ||
      !is.numeric(theta02) || is.object(theta02) ||
      !is.null(dim(theta02)) ||
      length(theta01) != p || length(theta02) != p ||
      any(!is.finite(theta01)) || any(!is.finite(theta02))) {
    stop(
      paste(
        "Initial time-zero states must be plain numeric finite vectors",
        "of state dimension p."
      ),
      call. = FALSE
    )
  }
  theta01 <- as.numeric(theta01)
  theta02 <- as.numeric(theta02)
  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    .rqr_restore_rng(init$rng_state %||% NULL)
  }

  eta1_draws <- matrix(NA_real_, T, n_keep)
  eta2_draws <- matrix(NA_real_, T, n_keep)
  theta1_draws <- if (store_state_draws) array(NA_real_, c(p, T, n_keep)) else NULL
  theta2_draws <- if (store_state_draws) array(NA_real_, c(p, T, n_keep)) else NULL
  terminal1_draws <- terminal2_draws <- matrix(NA_real_, p, n_keep)
  v_draws <- if (store_latent_draws) matrix(NA_real_, T, n_keep) else NULL
  lambda_draws <- numeric(n_keep)
  q_draws <- if (component_mode) {
    matrix(NA_real_, n_keep, length(q_evolution), dimnames = list(NULL, evolution$component_names))
  } else NULL
  q_shape_draws <- if (component_mode) {
    matrix(
      NA_real_, n_keep, length(q_evolution),
      dimnames = list(NULL, evolution$component_names)
    )
  } else NULL
  q_rate_draws <- if (component_mode) {
    matrix(
      NA_real_, n_keep, length(q_evolution),
      dimnames = list(NULL, evolution$component_names)
    )
  } else NULL
  theta01_draws <- theta02_draws <- if (
      time0_completion_mode
    ) {
    matrix(NA_real_, p, n_keep)
  } else {
    NULL
  }

  total_iter <- n_burn + n_keep * thin
  loss_trace <- lambda_trace <- effective_rate_trace <- numeric(total_iter)
  lambda_shape_trace <- lambda_rate_trace <- rep(NA_real_, total_iter)
  root_swap_trace <- logical(total_iter)
  ffbs_iteration <- vector("list", 2L * total_iter)
  component_scale_interweave_iteration <- if (
      component_mode && isTRUE(component_scale_interweave)
    ) {
    vector("list", total_iter)
  } else {
    NULL
  }
  component_scale_collapsed_iteration <- if (
      component_mode && isTRUE(component_scale_collapsed_update)
    ) {
    vector("list", total_iter)
  } else {
    NULL
  }
  repair_records <- NULL
  save_idx <- 0L

  for (iter in seq_len(total_iter)) {
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)
    eta2 <- .rqr_state_ordinates(expanded$FF, theta2)
    e <- rqr_residual_product(y[observed], eta1[observed], eta2[observed])
    loss_current <- sum(rqr_check_loss(e, constants$alpha))
    if (learn_lambda) {
      lp <- .rqr_lambda_posterior_params(
        loss_sum = loss_current / loss_reference_scale,
        n = n_obs, lambda_prior = lambda_prior,
        learning_rate_mode = learning_rate_mode
      )
      lambda_shape_trace[iter] <- lp$shape
      lambda_rate_trace[iter] <- lp$rate
      lambda <- stats::rgamma(1L, shape = lp$shape, rate = lp$rate)
      constants <- rqr_constants(coverage_level, lambda / loss_reference_scale)
    }

    # Full latent-scale refresh immediately after the collapsed lambda draw.
    gp <- rqr_gig_params(e, coverage_level, constants$omega)
    v[observed] <- rqr_sample_gig_half(gp$b, gp$a)
    v[!observed] <- loss_reference_scale / lambda
    obs_variance <- constants$phi * loss_reference_scale * v / lambda
    H1 <- sweep(expanded$FF, 2L, y - eta2, `*`)
    H1[, !observed] <- 0
    z1 <- y * (y - eta2) - constants$xi * v
    z1[!observed] <- NA_real_
    if (component_mode && isTRUE(component_scale_collapsed_update)) {
      collapsed_update <- .rqr_collapsed_component_scale_update(
        conditioned_theta = theta2,
        conditioned_theta0 = theta02,
        z = z1, H = H1, obs_variance = obs_variance,
        GG = expanded$GG, m0 = expanded$m0, C0 = expanded$C0,
        evolution = evolution, q = q_evolution,
        backend = backend_resolved,
        width = component_scale_slice_width,
        sweeps = component_scale_slice_sweeps,
        max_steps = component_scale_slice_max_steps,
        max_shrink = component_scale_slice_max_shrink
      )
      q_evolution <- collapsed_update$q
      component_scale_collapsed_iteration[[iter]] <- data.frame(
        iteration = iter,
        component = evolution$component_names,
        evaluations = collapsed_update$diagnostics$evaluations,
        shrink_steps = collapsed_update$diagnostics$shrink_steps,
        sweeps = collapsed_update$diagnostics$sweeps,
        integrated_root = "root1",
        conditioned_root = "root2",
        exact_partially_collapsed = TRUE,
        stringsAsFactors = FALSE
      )
    }
    evolution_iter <- if (component_mode) {
      .rqr_materialize_component_evolution(evolution, q_evolution, T, p)
    } else evolution
    draw1 <- rqr_ffbs_sample(
      z1, H1, obs_variance, expanded$GG, expanded$m0, expanded$C0,
      evolution_iter, backend = backend_resolved, jitter_ladder = jitter_ladder,
      numerical_policy = numerical_policy
    )
    theta1 <- draw1$path
    if (component_mode) {
      theta01 <- .rqr_draw_initial_state(
        theta1[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L]
      )
    }
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)

    H2 <- sweep(expanded$FF, 2L, y - eta1, `*`)
    H2[, !observed] <- 0
    z2 <- y * (y - eta1) - constants$xi * v
    z2[!observed] <- NA_real_
    draw2 <- rqr_ffbs_sample(
      z2, H2, obs_variance, expanded$GG, expanded$m0, expanded$C0,
      evolution_iter, backend = backend_resolved, jitter_ladder = jitter_ladder,
      numerical_policy = numerical_policy
    )
    theta2 <- draw2$path
    if (component_mode) {
      theta02 <- .rqr_draw_initial_state(
        theta2[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L]
      )
      if (isTRUE(component_scale_interweave)) {
        iteration_interweave <- vector(
          "list", component_scale_interweave_cycles
        )
        for (cycle in seq_len(component_scale_interweave_cycles)) {
          q_update <- .rqr_sample_component_scales(
            theta1, theta2, theta01, theta02, expanded$GG, evolution
          )
          q_evolution <- q_update$draw
          interweave <- .rqr_interweave_component_scales(
            theta1 = theta1, theta2 = theta2,
            theta01 = theta01, theta02 = theta02,
            GG = expanded$GG, FF = expanded$FF,
            y = y, observed = observed, v = v,
            xi = constants$xi, obs_variance = obs_variance,
            evolution = evolution, q = q_evolution,
            width = component_scale_slice_width,
            sweeps = component_scale_slice_sweeps,
            max_steps = component_scale_slice_max_steps,
            max_shrink = component_scale_slice_max_shrink
          )
          q_evolution <- interweave$q
          theta1 <- interweave$theta1
          theta2 <- interweave$theta2
          iteration_interweave[[cycle]] <- data.frame(
            iteration = iter,
            cycle = cycle,
            component = evolution$component_names,
            evaluations = interweave$diagnostics$evaluations,
            shrink_steps = interweave$diagnostics$shrink_steps,
            sweeps_per_cycle = interweave$diagnostics$sweeps,
            exact_noncentered_slice = TRUE,
            stringsAsFactors = FALSE
          )
        }
        q_update <- list(
          draw = q_evolution,
          posterior = .rqr_component_scale_posterior(
            theta1, theta2, theta01, theta02, expanded$GG, evolution
          )
        )
        component_scale_interweave_iteration[[iter]] <-
          do.call(rbind, iteration_interweave)
      } else {
        q_update <- .rqr_sample_component_scales(
          theta1, theta2, theta01, theta02, expanded$GG, evolution
        )
        q_evolution <- q_update$draw
      }
    } else if (time0_completion_mode) {
      # Fixed-W and frozen-template FFBS integrate theta_0 out through
      # (m0, C0). Complete each root path with an exact draw from
      # p(theta_0 | theta_1) so stored full-state summaries have the same
      # time-zero contract as component-scale fits.
      theta01 <- .rqr_draw_initial_state(
        theta1[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L]
      )
      theta02 <- .rqr_draw_initial_state(
        theta2[, 1L], expanded$GG[, , 1L], expanded$m0, expanded$C0,
        evolution_iter$W[, , 1L]
      )
    }

    if (stats::runif(1L) < 0.5) {
      tmp <- theta1; theta1 <- theta2; theta2 <- tmp
      tmp <- theta01; theta01 <- theta02; theta02 <- tmp
      root_swap_trace[iter] <- TRUE
    }
    eta1 <- .rqr_state_ordinates(expanded$FF, theta1)
    eta2 <- .rqr_state_ordinates(expanded$FF, theta2)

    loss_trace[iter] <- sum(rqr_check_loss(
      rqr_residual_product(y[observed], eta1[observed], eta2[observed]), constants$alpha
    ))
    lambda_trace[iter] <- lambda
    effective_rate_trace[iter] <- lambda / loss_reference_scale
    repair_records <- .rqr_bind_ffbs_repairs(repair_records, draw1$diagnostics, iter, "root1")
    repair_records <- .rqr_bind_ffbs_repairs(repair_records, draw2$diagnostics, iter, "root2")
    ffbs_iteration[[2L * iter - 1L]] <- data.frame(
      iteration = iter, root = "root1",
      jitter_count = draw1$diagnostics$jitter_count,
      repair_count = draw1$diagnostics$repair_count,
      psd_draw_count = draw1$diagnostics$psd_draw_count,
      min_forecast_variance = draw1$diagnostics$min_forecast_variance
    )
    ffbs_iteration[[2L * iter]] <- data.frame(
      iteration = iter, root = "root2",
      jitter_count = draw2$diagnostics$jitter_count,
      repair_count = draw2$diagnostics$repair_count,
      psd_draw_count = draw2$diagnostics$psd_draw_count,
      min_forecast_variance = draw2$diagnostics$min_forecast_variance
    )

    if (iter > n_burn && (iter - n_burn) %% thin == 0L) {
      save_idx <- save_idx + 1L
      eta1_draws[, save_idx] <- eta1
      eta2_draws[, save_idx] <- eta2
      terminal1_draws[, save_idx] <- theta1[, T]
      terminal2_draws[, save_idx] <- theta2[, T]
      lambda_draws[save_idx] <- lambda
      if (store_state_draws) {
        theta1_draws[, , save_idx] <- theta1
        theta2_draws[, , save_idx] <- theta2
      }
      if (!is.null(theta01_draws)) {
        theta01_draws[, save_idx] <- theta01
        theta02_draws[, save_idx] <- theta02
      }
      if (store_latent_draws) v_draws[, save_idx] <- v
      if (component_mode) {
        q_draws[save_idx, ] <- q_evolution
        q_shape_draws[save_idx, ] <- q_update$posterior$shape
        q_rate_draws[save_idx, ] <- q_update$posterior$rate
      }
    }
    if (verbose && (iter %% progress_every == 0L || iter == total_iter)) {
      message(sprintf("[rqr_dlm_fit] iter %d/%d loss=%.6g", iter, total_iter, loss_trace[iter]))
    }
  }

  lower <- pmin(eta1_draws, eta2_draws)
  upper <- pmax(eta1_draws, eta2_draws)
  lambda_summary <- .rqr_lambda_summary(lambda_draws)
  template_repairs <- if (continued_from_checkpoint) {
    0L
  } else {
    .rqr_history_count(
      evolution$construction_audit$repair_count %||% 0L,
      "evolution construction repair count"
    )
  }
  mcmc_repairs <- .rqr_history_count(
    if (is.null(repair_records)) 0L else nrow(repair_records),
    "MCMC repair count"
  )
  segment_repairs <- .rqr_history_count(
    as.double(template_repairs) + as.double(mcmc_repairs),
    "segment repair count"
  )
  numerical_exact <- segment_repairs == 0L
  parent_cumulative_repairs <- .rqr_history_count(
    parent_cumulative_repairs_input,
    "parent cumulative repair count"
  )
  cumulative_repairs <- .rqr_history_count(
    as.double(parent_cumulative_repairs) + as.double(segment_repairs),
    "cumulative repair count"
  )
  chain_history_numerically_exact <-
    parent_chain_numerically_exact && numerical_exact
  parent_promotion_eligible <- if (continued_from_checkpoint) {
    parent_promotion_eligible_input
  } else {
    TRUE
  }
  mathematical_exact <- isTRUE(evolution$exact_joint_target)
  rng_state <- .rqr_rng_state()
  completed_offset <- .rqr_history_count(
    completed_offset_input,
    "completed_iterations"
  )
  completed_iterations <- .rqr_history_count(
    as.double(completed_offset) + as.double(total_iter),
    "cumulative completed_iterations"
  )
  target_contract <- .rqr_dlm_target_contract(
    coverage_level = constants$alpha,
    learning_rate_mode = learning_rate_mode,
    fixed_learning_rate = if (learn_lambda) NA_real_ else learning_rate,
    loss_reference_scale = loss_reference_scale,
    lambda_prior = lambda_prior,
    numerical_policy = numerical_policy,
    jitter_ladder = jitter_ladder
  )
  provenance <- .rqr_provenance(
    data = list(y = y),
    matrices = .rqr_dlm_provenance_matrices(expanded, evolution),
    numerical_policy = numerical_policy,
    initial_seed = seed,
    repo_root = provenance_control$repo_root,
    expected_git_commit = provenance_control$expected_git_commit,
    backend = backend_resolved,
    backend_requested = backend_requested,
    backend_resolved = backend_resolved,
    objects = .rqr_dlm_provenance_objects(
      expanded, evolution, target_contract
    ),
    external_repositories = provenance_control$external_repositories,
    required_external_repositories =
      provenance_control$required_external_repositories,
    primary_runtime_attestation =
      provenance_control$primary_runtime_attestation
  )
  segment_target_numerical_eligible <- mathematical_exact && numerical_exact
  target_numerical_eligible <- mathematical_exact &&
    chain_history_numerically_exact
  ordinary_v1_scope_eligible <- !identical(
    learning_rate_mode, "learned_pure"
  )
  checkpoint <- list(
    schema_version = provenance$schema_version,
    completed_iterations = completed_iterations,
    theta_root1 = theta1,
    theta_root2 = theta2,
    theta0_root1 = theta01,
    theta0_root2 = theta02,
    latent_v = v,
    lambda = lambda,
    evolution_scale = q_evolution,
    transition_kernel = component_scale_kernel_contract,
    rng_state = rng_state
  )
  checkpoint_digest <- .rqr_digest(checkpoint)
  segment_schedule_contract <- .rqr_make_dlm_schedule_contract(
    start_completed_iterations = completed_offset,
    n_burn = n_burn,
    n_retained_draws = n_keep,
    thin = thin,
    checkpoint_digest = checkpoint_digest,
    parent =
      continuation_control$parent_schedule_contract %||% NULL,
    parent_digest =
      continuation_control$parent_schedule_digest %||% NULL
  )
  segment_schedule_digest <- .rqr_digest(
    segment_schedule_contract
  )
  out <- list(
    method = "mcmc_ffbs",
    family = "rqr_dlm",
    model = model,
    expanded_model = expanded,
    evolution = evolution,
    y = y,
    model_spec = list(
      family = "rqr_dlm",
      parameterization = "exchangeable_dynamic_roots",
      loss_name = "rqr_residual_product_check_loss",
      state_model = "linear_gaussian_interval_root_evolution",
      coverage_level = constants$alpha,
      learning_rate_mode = learning_rate_mode,
      fixed_learning_rate = if (learn_lambda) NA_real_ else learning_rate,
      learning_rate = if (learn_lambda) mean(lambda_draws / loss_reference_scale) else learning_rate,
      lambda_initial = lambda_initial,
      loss_reference_scale = loss_reference_scale,
      effective_learning_rate = mean(lambda_draws / loss_reference_scale),
      lambda_prior = lambda_prior,
      lambda_summary = lambda_summary,
      inferential_target = .rqr_target_formula(learning_rate_mode),
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      evolution_mode = evolution_mode,
      component_scale_transition_kernel =
        component_scale_kernel_contract,
      target_contract = if (mathematical_exact) "fixed_joint_exact" else "working_sequential",
      exact_joint_target = mathematical_exact,
      ordinary_v1_scope_eligible = ordinary_v1_scope_eligible,
      continuation_supported = ordinary_v1_scope_eligible,
      numerical_policy = numerical_policy,
      numerical_repair_count = segment_repairs,
      cumulative_numerical_repair_count = cumulative_repairs,
      numerically_exact_transition = numerical_exact,
      chain_history_numerically_exact = chain_history_numerically_exact,
      parent_chain_history_numerically_exact =
        parent_chain_numerically_exact,
      segment_target_numerical_eligible =
        segment_target_numerical_eligible,
      target_numerical_eligible = target_numerical_eligible,
      reproducibility_eligible = provenance$reproducibility_eligible,
      parent_promotion_eligible = parent_promotion_eligible,
      promotion_eligible = ordinary_v1_scope_eligible &&
        target_numerical_eligible &&
        parent_promotion_eligible && provenance$reproducibility_eligible,
      root_priors_exchangeable = TRUE,
      root_swap_move = TRUE
    ),
    samp.eta_root1 = eta1_draws,
    samp.eta_root2 = eta2_draws,
    samp.theta_root1 = theta1_draws,
    samp.theta_root2 = theta2_draws,
    samp.theta_terminal_root1 = terminal1_draws,
    samp.theta_terminal_root2 = terminal2_draws,
    samp.theta0_root1 = theta01_draws,
    samp.theta0_root2 = theta02_draws,
    samp.lambda = lambda_draws,
    samp.latent_v = v_draws,
    samp.evolution_scale = q_draws,
    samp.evolution_scale_shape = q_shape_draws,
    samp.evolution_scale_rate = q_rate_draws,
    summary = .rqr_dlm_coverage_summary(y, observed, lower, upper),
    diagnostics = list(
      loss_trace = loss_trace,
      scaled_loss_trace = loss_trace / loss_reference_scale,
      lambda_trace = lambda_trace,
      effective_learning_rate_trace = effective_rate_trace,
      lambda_post_shape_trace = lambda_shape_trace,
      lambda_post_rate_trace = lambda_rate_trace,
      root_swap_trace = root_swap_trace,
      ffbs_iteration = do.call(rbind, ffbs_iteration),
      numerical_repairs = repair_records %||% data.frame(),
      component_scale_interweave = if (
          is.null(component_scale_interweave_iteration)) {
        data.frame()
      } else {
        do.call(rbind, component_scale_interweave_iteration)
      },
      component_scale_collapsed = if (
          is.null(component_scale_collapsed_iteration)) {
        data.frame()
      } else {
        do.call(rbind, component_scale_collapsed_iteration)
      },
      template_construction_audit = evolution$construction_audit %||% NULL,
      partial_collapse_order = c(
        "lambda_collapsed", "latent_v_refresh",
        if (
            component_mode &&
              isTRUE(component_scale_collapsed_update)
          ) {
          "component_scale_root1_collapsed"
        } else NULL,
        "root1_ffbs",
        if (component_mode) "root1_time0" else NULL,
        "root2_ffbs",
        if (component_mode) {
          "root2_time0"
        } else if (time0_completion_mode) {
          "fixed_evolution_time0_completion"
        } else NULL,
        if (component_mode && isTRUE(component_scale_interweave)) {
          sprintf(
            "component_scale_centered_noncentered_cycles_%d",
            component_scale_interweave_cycles
          )
        } else if (component_mode) {
          "component_scale_centered_update"
        } else NULL,
        "global_root_swap"
      )
    ),
    provenance = provenance,
    checkpoint_state = checkpoint,
    checkpoint_digest = checkpoint_digest,
    last = checkpoint,
    segment_schedule_contract = segment_schedule_contract,
    segment_schedule_digest = segment_schedule_digest,
    misc = list(
      n_burn = n_burn, n_mcmc = n_keep, thin = thin, seed = seed,
      backend = backend_resolved,
      backend_requested = backend_requested,
      backend_resolved = backend_resolved,
      observed = observed,
      store_state_draws = store_state_draws,
      store_latent_draws = store_latent_draws,
      jitter_ladder = jitter_ladder,
      component_scale_collapsed_update =
        component_scale_collapsed_update,
      component_scale_interweave = component_scale_interweave,
      component_scale_interweave_cycles =
        component_scale_interweave_cycles,
      component_scale_slice_width = component_scale_slice_width,
      component_scale_slice_sweeps = component_scale_slice_sweeps,
      component_scale_slice_max_steps =
        component_scale_slice_max_steps,
      component_scale_slice_max_shrink =
        component_scale_slice_max_shrink,
      note = paste(
        "Root trajectory draws arise from a generalized-Bayes loss update;",
        "they are not response draws."
      )
    )
  )
  if (ordinary_v1_scope_eligible) {
    out$continuation_history_contract <- .rqr_make_continuation_history(
      checkpoint_digest = checkpoint_digest,
      segment_numerical_repair_count =
        out$model_spec$numerical_repair_count,
      segment_exact_joint_target =
        out$model_spec$exact_joint_target,
      segment_environment_base_eligible =
        out$provenance$reproducibility_eligible,
      segment_target_contract_digest =
        out$provenance$object_digests$target,
      backend_requested = out$provenance$backend_requested,
      backend_resolved = out$provenance$backend_resolved
    )
    out$continuation_history_digest <- .rqr_digest(
      out$continuation_history_contract
    )
  } else {
    out$continuation_history_contract <- NULL
    out$continuation_history_digest <- NA_character_
    out$model_spec$promotion_eligible <- FALSE
  }
  class(out) <- c("rqr_dlm_mcmc", "rqr_fit")
  if (!continued_from_checkpoint) {
    .rqr_validate_dlm_fit_envelope(out)
  }
  out
}

.rqr_validate_dlm_fit_envelope <- function(object) {
  if (!inherits(object, "rqr_dlm_mcmc")) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  if (!identical(object$method, "mcmc_ffbs") ||
      !identical(object$family, "rqr_dlm") ||
      !is.list(object$provenance) ||
      !identical(
        object$provenance$schema_version, .rqr_schema_version()
      ) ||
      !is.list(object$checkpoint_state) ||
      !identical(
        object$checkpoint_state$schema_version,
        .rqr_schema_version()
      )) {
    stop(
      sprintf(
        "The DLM fitted object requires schema %s.",
        .rqr_schema_version()
      ),
      call. = FALSE
    )
  }
  checkpoint <- object$checkpoint_state
  completed_iterations <- .rqr_history_count(
    checkpoint$completed_iterations,
    "checkpoint_state$completed_iterations"
  )
  checkpoint_digest <- object$checkpoint_digest %||% NA_character_
  if (!.rqr_nonmissing_text(checkpoint_digest) ||
      !grepl("^[0-9a-f]{64}$", checkpoint_digest) ||
      !identical(.rqr_digest(checkpoint), checkpoint_digest)) {
    stop("The DLM checkpoint digest is invalid.", call. = FALSE)
  }
  if (!is.list(object$last) ||
      !identical(object$last, checkpoint)) {
    stop(
      paste(
        "The DLM compatibility last-state alias does not match the",
        "authoritative checkpoint."
      ),
      call. = FALSE
    )
  }

  continuation_supported <- isTRUE(
    object$model_spec$continuation_supported
  )
  history <- if (continuation_supported) {
    .rqr_validate_continuation_history(object)
  } else {
    history_digest <- object$continuation_history_digest %||%
      NA_character_
    if (!is.null(object$continuation_history_contract) ||
        !is.character(history_digest) ||
        length(history_digest) != 1L ||
        !is.na(history_digest)) {
      stop(
        "A noncontinuable DLM fit cannot carry continuation history.",
        call. = FALSE
      )
    }
    NULL
  }
  schedule <- object$segment_schedule_contract
  schedule_digest <- object$segment_schedule_digest %||% NA_character_
  .rqr_validate_dlm_schedule_value(schedule, schedule_digest)
  final_schedule <- utils::tail(schedule$segments, 1L)[[1L]]
  if (!identical(
      final_schedule$checkpoint_digest, checkpoint_digest
    ) ||
      !identical(
        final_schedule$end_completed_iterations,
        completed_iterations
      )) {
    stop(
      paste(
        "The final DLM segment schedule does not bind the checkpoint",
        "and completed-iteration count."
      ),
      call. = FALSE
    )
  }
  if (continuation_supported) {
    if (!identical(schedule$generation, history$generation) ||
        length(schedule$segments) != length(history$segments) ||
        !all(vapply(
          seq_along(schedule$segments),
          function(index) identical(
            schedule$segments[[index]]$checkpoint_digest,
            history$segments[[index]]$checkpoint_digest
          ),
          logical(1L)
        ))) {
      stop(
        paste(
          "The DLM segment schedule does not match the",
          "continuation-history checkpoint chain."
        ),
        call. = FALSE
      )
    }
  } else if (length(schedule$segments) != 1L ||
      schedule$generation != 0L) {
    stop(
      "A noncontinuable DLM fit must have one initial schedule segment.",
      call. = FALSE
    )
  }

  n_burn <- .rqr_history_count(object$misc$n_burn, "misc$n_burn")
  n_mcmc <- .rqr_scalar_integer(
    object$misc$n_mcmc, "misc$n_mcmc", 1L
  )
  thin <- .rqr_scalar_integer(object$misc$thin, "misc$thin", 1L)
  store_state_draws <- .rqr_scalar_logical(
    object$misc$store_state_draws, "misc$store_state_draws"
  )
  store_latent_draws <- .rqr_scalar_logical(
    object$misc$store_latent_draws, "misc$store_latent_draws"
  )
  if (!identical(final_schedule$n_burn, n_burn) ||
      !identical(final_schedule$n_retained_draws, n_mcmc) ||
      !identical(final_schedule$thin, thin)) {
    stop(
      "The final DLM segment schedule conflicts with MCMC controls.",
      call. = FALSE
    )
  }

  if (!is.numeric(object$y) || is.object(object$y) ||
      !is.null(dim(object$y)) || !length(object$y) ||
      !any(!is.na(object$y)) || any(is.nan(object$y)) ||
      any(is.infinite(object$y))) {
    stop("The fitted DLM response is invalid.", call. = FALSE)
  }
  expanded <- .rqr_expand_model(
    rqr_as_dlm_model(object$model), length(object$y)
  )
  p <- expanded$p
  T <- length(object$y)
  if (!identical(
      .rqr_digest(list(y = as.numeric(object$y))),
      object$provenance$data_digest
    ) ||
      !identical(
        lapply(
          .rqr_dlm_provenance_matrices(expanded, object$evolution),
          .rqr_digest
        ),
        object$provenance$matrix_digests
      )) {
    stop("The DLM data digest or model/evolution matrix digest changed.",
         call. = FALSE)
  }
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) {
    fixed_rate <- NA_real_
  }
  target <- .rqr_dlm_target_contract(
    coverage_level = object$model_spec$coverage_level,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    fixed_learning_rate = fixed_rate,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    lambda_prior = object$model_spec$lambda_prior,
    numerical_policy = object$model_spec$numerical_policy,
    jitter_ladder = object$misc$jitter_ladder
  )
  object_digests <- lapply(
    .rqr_dlm_provenance_objects(expanded, object$evolution, target),
    .rqr_digest
  )
  if (!identical(
      object_digests, object$provenance$object_digests
    )) {
    stop("The DLM model, target, or evolution digest changed.",
         call. = FALSE)
  }
  transition_kernel <- list(
    one_root_partially_collapsed =
      identical(object$model_spec$evolution_mode, "component_scale") &&
        isTRUE(object$misc$component_scale_collapsed_update),
    collapsed_integrated_root = if (
        identical(object$model_spec$evolution_mode, "component_scale") &&
          isTRUE(object$misc$component_scale_collapsed_update)
      ) {
      "root1"
    } else {
      "none"
    },
    centered_inverse_gamma =
      identical(object$model_spec$evolution_mode, "component_scale"),
    noncentered_slice_interweave =
      identical(object$model_spec$evolution_mode, "component_scale") &&
        isTRUE(object$misc$component_scale_interweave),
    interweave_cycles =
      object$misc$component_scale_interweave_cycles %||% 1L,
    slice_width = object$misc$component_scale_slice_width %||% 1,
    slice_sweeps_per_cycle =
      object$misc$component_scale_slice_sweeps %||% 1L,
    slice_max_steps =
      object$misc$component_scale_slice_max_steps %||% 100L,
    slice_max_shrink =
      object$misc$component_scale_slice_max_shrink %||% 1000L,
    target_change = FALSE
  )
  if (!identical(checkpoint$transition_kernel, transition_kernel) ||
      !identical(
        object$model_spec$component_scale_transition_kernel,
        transition_kernel
      )) {
    stop("The DLM transition-kernel contract changed.", call. = FALSE)
  }
  valid_path <- function(value) {
    is.matrix(value) && is.numeric(value) && !is.object(value) &&
      identical(dim(value), c(p, T)) && all(is.finite(value))
  }
  valid_state <- function(value) {
    is.numeric(value) && !is.object(value) && is.null(dim(value)) &&
      length(value) == p && all(is.finite(value))
  }
  if (!valid_path(checkpoint$theta_root1) ||
      !valid_path(checkpoint$theta_root2) ||
      !valid_state(checkpoint$theta0_root1) ||
      !valid_state(checkpoint$theta0_root2) ||
      !is.numeric(checkpoint$latent_v) ||
      is.object(checkpoint$latent_v) ||
      !is.null(dim(checkpoint$latent_v)) ||
      length(checkpoint$latent_v) != T ||
      any(!is.finite(checkpoint$latent_v)) ||
      any(checkpoint$latent_v <= 0) ||
      !is.numeric(checkpoint$lambda) ||
      length(checkpoint$lambda) != 1L ||
      !is.finite(checkpoint$lambda) ||
      checkpoint$lambda <= 0 ||
      is.null(checkpoint$rng_state)) {
    stop("The DLM checkpoint state is incomplete or invalid.",
         call. = FALSE)
  }

  stored <- .rqr_validate_dlm_stored_draws(object)
  if (!identical(dim(stored$eta_root1), c(T, n_mcmc))) {
    stop("The retained DLM root draws conflict with MCMC controls.",
         call. = FALSE)
  }
  forecast_state <- .rqr_validate_dlm_forecast_state(object)
  if (!identical(forecast_state$n_save, n_mcmc) ||
      !identical(
        as.numeric(forecast_state$terminal_root1[, n_mcmc]),
        as.numeric(checkpoint$theta_root1[, T])
      ) ||
      !identical(
        as.numeric(forecast_state$terminal_root2[, n_mcmc]),
        as.numeric(checkpoint$theta_root2[, T])
      ) ||
      !identical(
        as.numeric(stored$lambda[n_mcmc]),
        as.numeric(checkpoint$lambda)
      )) {
    stop(
      paste(
        "The final retained DLM state or lambda does not match the",
        "authoritative checkpoint."
      ),
      call. = FALSE
    )
  }
  expected_eta1 <- .rqr_state_ordinates(
    expanded$FF, checkpoint$theta_root1
  )
  expected_eta2 <- .rqr_state_ordinates(
    expanded$FF, checkpoint$theta_root2
  )
  if (!identical(
      as.numeric(stored$eta_root1[, n_mcmc]),
      as.numeric(expected_eta1)
    ) ||
      !identical(
        as.numeric(stored$eta_root2[, n_mcmc]),
        as.numeric(expected_eta2)
      )) {
    stop(
      "The final retained DLM root ordinates do not match the checkpoint.",
      call. = FALSE
    )
  }
  if (store_state_draws) {
    if (!identical(
        as.numeric(object$samp.theta_root1[, , n_mcmc]),
        as.numeric(checkpoint$theta_root1)
      ) ||
        !identical(
          as.numeric(object$samp.theta_root2[, , n_mcmc]),
          as.numeric(checkpoint$theta_root2)
        )) {
      stop(
        "The final retained DLM state paths do not match the checkpoint.",
        call. = FALSE
      )
    }
  } else if (!is.null(object$samp.theta_root1) ||
      !is.null(object$samp.theta_root2)) {
    stop(
      "Full state paths are present although their storage is disabled.",
      call. = FALSE
    )
  }
  time0_root1_present <- !is.null(object$samp.theta0_root1)
  time0_root2_present <- !is.null(object$samp.theta0_root2)
  if (!identical(time0_root1_present, time0_root2_present)) {
    stop(
      "Stored DLM time-zero draws must be present for both roots or neither.",
      call. = FALSE
    )
  }
  time0_present <- time0_root1_present
  if (time0_present) {
    if (!is.matrix(object$samp.theta0_root1) ||
        !is.matrix(object$samp.theta0_root2) ||
        !identical(dim(object$samp.theta0_root1), c(p, n_mcmc)) ||
        !identical(dim(object$samp.theta0_root2), c(p, n_mcmc)) ||
        !identical(
          as.numeric(object$samp.theta0_root1[, n_mcmc]),
          as.numeric(checkpoint$theta0_root1)
        ) ||
        !identical(
          as.numeric(object$samp.theta0_root2[, n_mcmc]),
          as.numeric(checkpoint$theta0_root2)
        )) {
      stop(
        "The final retained DLM time-zero states do not match the checkpoint.",
        call. = FALSE
      )
    }
  }
  if (store_latent_draws) {
    if (!is.matrix(object$samp.latent_v) ||
        !is.numeric(object$samp.latent_v) ||
        !identical(dim(object$samp.latent_v), c(T, n_mcmc)) ||
        !identical(
          as.numeric(object$samp.latent_v[, n_mcmc]),
          as.numeric(checkpoint$latent_v)
        )) {
      stop(
        "The final retained DLM latent state does not match the checkpoint.",
        call. = FALSE
      )
    }
  } else if (!is.null(object$samp.latent_v)) {
    stop(
      "Latent states are present although their storage is disabled.",
      call. = FALSE
    )
  }
  component_mode <- identical(
    object$model_spec$evolution_mode, "component_scale"
  )
  if (component_mode) {
    q_draws <- forecast_state$evolution_scale
    q_count <- ncol(q_draws)
    component_names <- object$evolution$component_names
    q_shape <- object$samp.evolution_scale_shape
    q_rate <- object$samp.evolution_scale_rate
    if (!is.numeric(checkpoint$evolution_scale) ||
        is.object(checkpoint$evolution_scale) ||
        !is.null(dim(checkpoint$evolution_scale)) ||
        length(checkpoint$evolution_scale) != q_count ||
        !identical(
          as.numeric(q_draws[n_mcmc, ]),
          as.numeric(checkpoint$evolution_scale)
        ) ||
        !is.matrix(q_shape) || !is.numeric(q_shape) ||
        is.object(q_shape) ||
        !identical(dim(q_shape), c(n_mcmc, q_count)) ||
        any(!is.finite(q_shape)) || any(q_shape <= 0) ||
        !identical(colnames(q_shape), component_names) ||
        !is.matrix(q_rate) || !is.numeric(q_rate) ||
        is.object(q_rate) ||
        !identical(dim(q_rate), c(n_mcmc, q_count)) ||
        any(!is.finite(q_rate)) || any(q_rate <= 0) ||
        !identical(colnames(q_rate), component_names)) {
      stop(
        paste(
          "The final retained DLM evolution state or conditional",
          "parameters do not match the checkpoint contract."
        ),
        call. = FALSE
      )
    }
  } else if (length(checkpoint$evolution_scale) != 0L ||
      !is.null(object$samp.evolution_scale) ||
      !is.null(object$samp.evolution_scale_shape) ||
      !is.null(object$samp.evolution_scale_rate)) {
    stop(
      "Non-component DLM fits must not contain evolution-scale draws.",
      call. = FALSE
    )
  }
  if (identical(
      object$model_spec$learning_rate_mode, "fixed_rate"
    )) {
    expected_lambda <- object$model_spec$fixed_learning_rate *
      object$model_spec$loss_reference_scale
    if (!identical(
        as.numeric(checkpoint$lambda), as.numeric(expected_lambda)
      ) ||
        !identical(
          as.numeric(stored$lambda),
          rep(as.numeric(expected_lambda), n_mcmc)
        )) {
      stop("Fixed-rate DLM lambda values are not exact constants.",
           call. = FALSE)
    }
  }
  invisible(list(
    checkpoint = checkpoint,
    checkpoint_digest = checkpoint_digest,
    history = history,
    schedule = schedule,
    expanded = expanded,
    target = target,
    object_digests = object_digests
  ))
}

.rqr_validate_dlm_fit_if_present <- function(object) {
  full_markers <- c(
    "provenance", "checkpoint_state", "checkpoint_digest", "last",
    "segment_schedule_contract", "segment_schedule_digest"
  )
  claimed_full_fit <-
    identical(object$method %||% NA_character_, "mcmc_ffbs") ||
    identical(object$family %||% NA_character_, "rqr_dlm") ||
    any(full_markers %in% names(object))
  if (claimed_full_fit) {
    .rqr_validate_dlm_fit_envelope(object)
  }
  invisible(claimed_full_fit)
}

.rqr_validate_dlm_continuation <- function(object, allow_environment_mismatch = FALSE) {
  if (length(allow_environment_mismatch) != 1L ||
      !is.logical(allow_environment_mismatch) || is.na(allow_environment_mismatch)) {
    stop("allow_environment_mismatch must be TRUE or FALSE.", call. = FALSE)
  }
  envelope <- .rqr_validate_dlm_fit_envelope(object)
  continuation_history <- envelope$history
  stored_checkpoint_digest <- envelope$checkpoint_digest

  expanded <- .rqr_expand_model(rqr_as_dlm_model(object$model), length(object$y))
  current_data_digest <- .rqr_digest(list(y = as.numeric(object$y)))
  current_matrix_digests <- lapply(
    .rqr_dlm_provenance_matrices(expanded, object$evolution), .rqr_digest
  )
  if (!identical(current_data_digest, object$provenance$data_digest)) {
    stop("Continuation data digest does not match the fitted object.", call. = FALSE)
  }
  if (!identical(current_matrix_digests, object$provenance$matrix_digests)) {
    stop("Continuation model/evolution matrix digests do not match the fitted object.", call. = FALSE)
  }
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) fixed_rate <- NA_real_
  current_target_contract <- .rqr_dlm_target_contract(
    coverage_level = object$model_spec$coverage_level,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    fixed_learning_rate = fixed_rate,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    lambda_prior = object$model_spec$lambda_prior,
    numerical_policy = object$model_spec$numerical_policy,
    jitter_ladder = object$misc$jitter_ladder
  )
  current_transition_kernel <- list(
    one_root_partially_collapsed =
      identical(object$model_spec$evolution_mode, "component_scale") &&
        isTRUE(object$misc$component_scale_collapsed_update),
    collapsed_integrated_root = if (
        identical(object$model_spec$evolution_mode, "component_scale") &&
          isTRUE(object$misc$component_scale_collapsed_update)
      ) {
      "root1"
    } else {
      "none"
    },
    centered_inverse_gamma =
      identical(object$model_spec$evolution_mode, "component_scale"),
    noncentered_slice_interweave =
      identical(object$model_spec$evolution_mode, "component_scale") &&
        isTRUE(object$misc$component_scale_interweave),
    interweave_cycles =
      object$misc$component_scale_interweave_cycles %||% 1L,
    slice_width = object$misc$component_scale_slice_width %||% 1,
    slice_sweeps_per_cycle =
      object$misc$component_scale_slice_sweeps %||% 1L,
    slice_max_steps =
      object$misc$component_scale_slice_max_steps %||% 100L,
    slice_max_shrink =
      object$misc$component_scale_slice_max_shrink %||% 1000L,
    target_change = FALSE
  )
  if (!identical(
      object$checkpoint_state$transition_kernel,
      current_transition_kernel
    ) ||
      !identical(
        object$model_spec$component_scale_transition_kernel,
        current_transition_kernel
      )) {
    stop(
      "Continuation transition-kernel contract does not match the fitted object.",
      call. = FALSE
    )
  }
  current_object_digests <- lapply(
    .rqr_dlm_provenance_objects(
      expanded, object$evolution, current_target_contract
    ),
    .rqr_digest
  )
  if (!identical(current_object_digests, object$provenance$object_digests)) {
    stop(
      "Continuation model, target, or evolution digest does not match the fitted object.",
      call. = FALSE
    )
  }

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
  backend_requested <- object$misc$backend_requested %||% object$misc$backend
  backend_resolved <- .rqr_resolve_ffbs_backend(backend_requested)
  current <- .rqr_provenance(
    data = list(y = as.numeric(object$y)),
    matrices = .rqr_dlm_provenance_matrices(expanded, object$evolution),
    numerical_policy = object$model_spec$numerical_policy,
    repo_root = stored_repo_root,
    expected_git_commit = stored_expected,
    backend = backend_resolved,
    backend_requested = backend_requested,
    backend_resolved = backend_resolved,
    objects = .rqr_dlm_provenance_objects(
      expanded, object$evolution, current_target_contract
    ),
    external_repositories = object$provenance$external_repositories,
    required_external_repositories =
      object$provenance$required_external_repositories,
    primary_runtime_attestation = primary_attestation
  )
  compare_fields <- c(
    "package_version", "R_version", "platform", "compiler", "BLAS", "LAPACK",
    "git_commit", "git_commit_available", "git_status_available", "git_dirty",
    "expected_git_commit", "expected_git_commit_match",
    "basic_provenance_complete", "provenance_complete",
    "primary_runtime_source_match", "primary_runtime_package_path",
    "primary_source_commit", "primary_source_tree_digest",
    "primary_runtime_tree_digest",
    "backend_requested", "backend_resolved", "RNGkind"
  )
  mismatches <- compare_fields[!vapply(compare_fields, function(field) {
    identical(object$provenance[[field]], current[[field]])
  }, logical(1L))]
  if (!identical(object$provenance$dependency_versions, current$dependency_versions)) {
    mismatches <- c(mismatches, "dependency_versions")
  }
  if (!identical(
        object$provenance$external_repositories,
        current$external_repositories
      )) {
    mismatches <- c(mismatches, "external_repositories")
  }
  if (length(mismatches)) {
    message <- sprintf(
      "Continuation environment differs in: %s.", paste(unique(mismatches), collapse = ", ")
    )
    if (!allow_environment_mismatch) {
      stop(
        paste(message, "Set allow_environment_mismatch=TRUE only for a non-bitwise portability run."),
        call. = FALSE
      )
    }
    warning(
      paste(message, "Exact bitwise continuation is not claimed for this segment."),
      call. = FALSE
    )
  }
  invisible(list(
    current_provenance = current,
    environment_mismatches = unique(mismatches),
    checkpoint_digest = stored_checkpoint_digest,
    object_digests = current_object_digests,
    continuation_history = continuation_history,
    schedule = envelope$schedule
  ))
}

#' Continue an RQR-DLM chain from its exact checkpoint
#'
#' Continuation restores the full RNG state and every Markov state required by
#' the native sampler. Numerical repair and promotion eligibility are inherited
#' cumulatively, and the requested and resolved FFBS backends are checked
#' separately. The function returns only the newly requested draws. Diagnostic
#' `"learned_pure"` fits are deliberately not continuable.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param n_mcmc Positive number of additional retained draws.
#' @param thin Positive thinning interval; defaults to the original fit.
#' @param store_state_draws,store_latent_draws Storage choices for new draws.
#' @param allow_environment_mismatch If `TRUE`, continue after an explicit
#'   warning when package, R, platform, BLAS/LAPACK, dependency, or source-commit
#'   metadata differ. Schema, checkpoint, data, model, target, and evolution
#'   digest mismatches always stop. The override is persisted and removes
#'   reproducibility and promotion eligibility from the returned segment.
#' @return A new `rqr_dlm_mcmc` segment beginning at the checkpoint.
#' @export
rqr_dlm_continue <- function(object, n_mcmc, thin = object$misc$thin,
                             store_state_draws = object$misc$store_state_draws,
                             store_latent_draws = object$misc$store_latent_draws,
                             allow_environment_mismatch = FALSE) {
  if (!inherits(object, "rqr_dlm_mcmc")) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  if (identical(
      object$model_spec$learning_rate_mode, "learned_pure"
    ) ||
      identical(
        object$model_spec$continuation_supported, FALSE
      )) {
    stop(
      "learned_pure is a diagnostic legacy target and is not continuable.",
      call. = FALSE
    )
  }
  n_mcmc <- .rqr_scalar_integer(n_mcmc, "n_mcmc", 1L)
  thin <- .rqr_scalar_integer(thin, "thin", 1L)
  store_state_draws <- .rqr_scalar_logical(
    store_state_draws, "store_state_draws"
  )
  store_latent_draws <- .rqr_scalar_logical(
    store_latent_draws, "store_latent_draws"
  )
  allow_environment_mismatch <- .rqr_scalar_logical(
    allow_environment_mismatch, "allow_environment_mismatch"
  )
  validation <- .rqr_validate_dlm_continuation(
    object, allow_environment_mismatch
  )
  checkpoint <- object$checkpoint_state
  if (is.null(checkpoint$rng_state)) stop("The fit does not contain a complete RNG checkpoint.", call. = FALSE)
  fixed_rate <- object$model_spec$fixed_learning_rate
  if (is.null(fixed_rate) || !is.finite(fixed_rate)) fixed_rate <- 1
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
  segment <- .rqr_dlm_fit_impl(
    y = object$y,
    model = object$model,
    coverage_level = object$model_spec$coverage_level,
    evolution_spec = object$evolution,
    learning_rate = fixed_rate,
    lambda_initial = checkpoint$lambda,
    loss_reference_scale = object$model_spec$loss_reference_scale,
    learning_rate_mode = object$model_spec$learning_rate_mode,
    lambda_prior = object$model_spec$lambda_prior,
    numerical_policy = object$model_spec$numerical_policy,
    provenance_control = list(
      repo_root = stored_repo_root,
      expected_git_commit = stored_expected,
      primary_runtime_attestation = primary_attestation,
      external_repositories = lapply(
        object$provenance$external_repositories %||% list(),
        function(x) list(
          repo_root = .rqr_stored_optional_provenance_text(
            x$repo_root, "external_repositories$repo_root"
          ),
          expected_git_commit = .rqr_stored_optional_provenance_text(
            x$expected_git_commit,
            "external_repositories$expected_git_commit"
          ),
          runtime_package = .rqr_stored_optional_provenance_text(
            x$runtime_package, "external_repositories$runtime_package"
          ),
          runtime_attestation = .rqr_stored_optional_provenance_text(
            x$runtime_attestation,
            "external_repositories$runtime_attestation"
          ),
          require_isolated_runtime = isTRUE(x$require_isolated_runtime)
          ,
          source_subdir = x$source_subdir %||% "."
        )
      ),
      required_external_repositories =
        object$provenance$required_external_repositories %||% character(0)
    ),
    mcmc_control = list(
      n_burn = 0L, n_mcmc = n_mcmc, thin = thin, seed = NULL,
      backend = object$misc$backend_requested %||% object$misc$backend,
      store_state_draws = store_state_draws,
      store_latent_draws = store_latent_draws,
      jitter_ladder = object$misc$jitter_ladder,
      component_scale_collapsed_update =
        isTRUE(object$misc$component_scale_collapsed_update),
      component_scale_interweave =
        isTRUE(object$misc$component_scale_interweave),
      component_scale_interweave_cycles =
        object$misc$component_scale_interweave_cycles %||% 1L,
      component_scale_slice_width =
        object$misc$component_scale_slice_width %||% 1,
      component_scale_slice_sweeps =
        object$misc$component_scale_slice_sweeps %||% 1L,
      component_scale_slice_max_steps =
        object$misc$component_scale_slice_max_steps %||% 100L,
      component_scale_slice_max_shrink =
        object$misc$component_scale_slice_max_shrink %||% 1000L
    ),
    init = list(
      state_root1 = checkpoint$theta_root1,
      state_root2 = checkpoint$theta_root2,
      theta0_root1 = checkpoint$theta0_root1,
      theta0_root2 = checkpoint$theta0_root2,
      latent_v = checkpoint$latent_v,
      lambda = checkpoint$lambda,
      evolution_scale = checkpoint$evolution_scale,
      rng_state = checkpoint$rng_state,
      completed_iterations = checkpoint$completed_iterations,
      continued_from_checkpoint = TRUE,
      parent_cumulative_numerical_repair_count =
        validation$continuation_history$
          cumulative_numerical_repair_count,
      parent_chain_history_numerically_exact =
        validation$continuation_history$
          chain_history_numerically_exact,
      parent_promotion_eligible =
        validation$continuation_history$promotion_eligible,
      continuation_control = list(
        parent_schedule_contract = validation$schedule,
        parent_schedule_digest = object$segment_schedule_digest
      )
    ),
    .continuation_token = .rqr_dlm_continuation_token
  )
  environment_override_used <- length(validation$environment_mismatches) > 0L
  parent_reproducibility_eligible <- isTRUE(
    validation$continuation_history$reproducibility_eligible
  )
  current_environment_eligible <- isTRUE(
    segment$provenance$reproducibility_eligible
  )
  inherited_reproducibility_eligible <- current_environment_eligible &&
    parent_reproducibility_eligible && !environment_override_used
  parent_chain_history_numerically_exact <- isTRUE(
    validation$continuation_history$chain_history_numerically_exact
  )
  parent_promotion_eligible <- isTRUE(
    validation$continuation_history$promotion_eligible
  )
  parent_cumulative_repairs <- .rqr_history_count(
    validation$continuation_history$cumulative_numerical_repair_count,
    "parent cumulative repair count"
  )
  same_resolved_backend <- identical(
    object$provenance$backend_resolved %||% object$provenance$backend,
    validation$current_provenance$backend_resolved %||%
      validation$current_provenance$backend
  )
  bitwise_continuation_claim <- !environment_override_used &&
    !length(validation$environment_mismatches) &&
    parent_reproducibility_eligible &&
    current_environment_eligible &&
    same_resolved_backend
  segment$continuation_contract <- list(
    continued_from_checkpoint = TRUE,
    parent_checkpoint_digest = validation$checkpoint_digest,
    parent_completed_iterations = checkpoint$completed_iterations,
    model_target_evolution_digests = validation$object_digests,
    environment_mismatches = validation$environment_mismatches,
    environment_override_used = environment_override_used,
    bitwise_continuation_claim = bitwise_continuation_claim,
    parent_reproducibility_eligible = parent_reproducibility_eligible,
    parent_target_numerical_eligible =
      isTRUE(object$model_spec$target_numerical_eligible),
    parent_promotion_eligible = parent_promotion_eligible,
    parent_chain_history_numerically_exact =
      parent_chain_history_numerically_exact,
    parent_cumulative_numerical_repair_count =
      parent_cumulative_repairs,
    chain_history_numerically_exact =
      isTRUE(segment$model_spec$chain_history_numerically_exact),
    cumulative_numerical_repair_count =
      segment$model_spec$cumulative_numerical_repair_count,
    backend_requested =
      segment$provenance$backend_requested,
    parent_backend_resolved =
      object$provenance$backend_resolved %||% object$provenance$backend,
    current_backend_resolved =
      segment$provenance$backend_resolved %||% segment$provenance$backend,
    current_environment_reproducibility_eligible =
      current_environment_eligible
  )
  segment$provenance$continued_from_checkpoint <- TRUE
  segment$provenance$parent_checkpoint_digest <- validation$checkpoint_digest
  segment$provenance$environment_override_used <- environment_override_used
  segment$provenance$reproducibility_eligible <-
    inherited_reproducibility_eligible
  segment$model_spec$reproducibility_eligible <-
    inherited_reproducibility_eligible
  segment$model_spec$promotion_eligible <-
    isTRUE(segment$model_spec$ordinary_v1_scope_eligible) &&
    isTRUE(segment$model_spec$target_numerical_eligible) &&
    parent_promotion_eligible &&
    inherited_reproducibility_eligible
  segment$continuation_history_contract <- .rqr_make_continuation_history(
    checkpoint_digest = segment$checkpoint_digest,
    segment_numerical_repair_count =
      segment$model_spec$numerical_repair_count,
    segment_exact_joint_target =
      segment$model_spec$exact_joint_target,
    segment_environment_base_eligible =
      current_environment_eligible,
    segment_target_contract_digest =
      segment$provenance$object_digests$target,
    backend_requested = segment$provenance$backend_requested,
    backend_resolved = segment$provenance$backend_resolved,
    parent = validation$continuation_history,
    parent_checkpoint_digest = validation$checkpoint_digest,
    environment_mismatches = validation$environment_mismatches,
    environment_override_used = environment_override_used
  )
  segment$continuation_history_digest <- .rqr_digest(
    segment$continuation_history_contract
  )
  .rqr_validate_dlm_fit_envelope(segment)
  segment
}

#' Extract fitted root-ordinate draws from RQR-DLM MCMC
#'
#' The returned matrices contain root ordinates over the fitted time range,
#' rather than full state paths. These are generalized-posterior root
#' functionals, not response draws.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param nd Number of retained draws to return. `NULL` keeps all draws.
#' @param seed Optional seed used only when draws are subsampled.
#' @param ... Reserved; supplying an argument is an error.
#' @return A list containing time-by-draw matrices `eta_root1` and
#'   `eta_root2`, the corresponding loss-rate draws `lambda`, selected indices
#'   `index`, and `nd`.
#' @export
rqr_posterior_draws.rqr_dlm_mcmc <- function(
    object, nd = NULL, seed = NULL, ...) {
  .rqr_reject_dots(
    list(...), "rqr_posterior_draws.rqr_dlm_mcmc"
  )
  .rqr_validate_dlm_fit_if_present(object)
  stored <- .rqr_validate_dlm_stored_draws(object)
  n_save <- ncol(stored$eta_root1)
  idx <- if (is.null(nd)) {
    if (!is.null(seed)) {
      stop(
        "seed must be NULL when nd is NULL because no subsampling occurs.",
        call. = FALSE
      )
    }
    seq_len(n_save)
  } else {
    nd <- .rqr_scalar_integer(nd, "nd", 1L)
    if (!is.null(seed)) {
      set.seed(.rqr_scalar_integer(seed, "seed", 0L))
    }
    sample.int(n_save, nd, replace = nd > n_save)
  }
  list(
    eta_root1 = stored$eta_root1[, idx, drop = FALSE],
    eta_root2 = stored$eta_root2[, idx, drop = FALSE],
    lambda = stored$lambda[idx],
    index = idx,
    nd = length(idx)
  )
}

.rqr_validate_dlm_stored_draws <- function(object) {
  if (!inherits(object, "rqr_dlm_mcmc")) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  eta1 <- object$samp.eta_root1
  eta2 <- object$samp.eta_root2
  if (!is.matrix(eta1) || !is.numeric(eta1) || is.object(eta1) ||
      !is.matrix(eta2) || !is.numeric(eta2) || is.object(eta2) ||
      !identical(dim(eta1), dim(eta2)) ||
      nrow(eta1) < 1L || ncol(eta1) < 1L ||
      any(!is.finite(eta1)) || any(!is.finite(eta2))) {
    stop(
      paste(
        "Stored DLM root-ordinate draws must be matching nonempty finite",
        "plain numeric matrices."
      ),
      call. = FALSE
    )
  }
  if (!is.numeric(object$y) || is.object(object$y) ||
      !is.null(dim(object$y)) || length(object$y) != nrow(eta1) ||
      !any(!is.na(object$y)) || any(is.nan(object$y)) ||
      any(is.infinite(object$y))) {
    stop(
      "Stored DLM root-ordinate rows must match the fitted response length.",
      call. = FALSE
    )
  }
  lambda <- object$samp.lambda
  if (!is.numeric(lambda) || is.object(lambda) || !is.null(dim(lambda)) ||
      length(lambda) != ncol(eta1) ||
      any(!is.finite(lambda)) || any(lambda <= 0)) {
    stop(
      "Stored DLM lambda draws must be finite, positive, and aligned with the root draws.",
      call. = FALSE
    )
  }
  storage.mode(eta1) <- "double"
  storage.mode(eta2) <- "double"
  lambda <- as.numeric(lambda)
  names(lambda) <- NULL
  list(
    eta_root1 = eta1,
    eta_root2 = eta2,
    lambda = lambda
  )
}

.rqr_validate_dlm_forecast_state <- function(object) {
  if (!inherits(object, "rqr_dlm_mcmc")) {
    stop("Expected an rqr_dlm_mcmc object.", call. = FALSE)
  }
  validate_terminal <- function(value, name) {
    if (!is.matrix(value) || !is.numeric(value) || is.object(value) ||
        nrow(value) < 1L || ncol(value) < 1L ||
        any(!is.finite(value))) {
      stop(
        sprintf(
          "%s must be a nonempty finite plain numeric state-by-draw matrix.",
          name
        ),
        call. = FALSE
      )
    }
    storage.mode(value) <- "double"
    value
  }
  validate_paths <- function(value, name) {
    dimensions <- dim(value)
    if (!is.array(value) || !is.numeric(value) || is.object(value) ||
        length(dimensions) != 3L ||
        any(dimensions < 1L) || any(!is.finite(value))) {
      stop(
        sprintf(
          "%s must be a nonempty finite plain numeric p-by-T-by-draw array.",
          name
        ),
        call. = FALSE
      )
    }
    storage.mode(value) <- "double"
    value
  }

  terminal1_present <- !is.null(object$samp.theta_terminal_root1)
  terminal2_present <- !is.null(object$samp.theta_terminal_root2)
  if (!identical(terminal1_present, terminal2_present)) {
    stop(
      "Stored terminal state draws must be present for both roots or neither.",
      call. = FALSE
    )
  }
  paths1_present <- !is.null(object$samp.theta_root1)
  paths2_present <- !is.null(object$samp.theta_root2)
  if (!identical(paths1_present, paths2_present)) {
    stop(
      "Stored full state draws must be present for both roots or neither.",
      call. = FALSE
    )
  }

  paths1 <- paths2 <- NULL
  if (paths1_present) {
    paths1 <- validate_paths(
      object$samp.theta_root1, "samp.theta_root1"
    )
    paths2 <- validate_paths(
      object$samp.theta_root2, "samp.theta_root2"
    )
    if (!identical(dim(paths1), dim(paths2))) {
      stop(
        "Stored full state draws must have matching root dimensions.",
        call. = FALSE
      )
    }
  }
  if (terminal1_present) {
    terminal1 <- validate_terminal(
      object$samp.theta_terminal_root1,
      "samp.theta_terminal_root1"
    )
    terminal2 <- validate_terminal(
      object$samp.theta_terminal_root2,
      "samp.theta_terminal_root2"
    )
    if (!identical(dim(terminal1), dim(terminal2))) {
      stop(
        "Stored terminal state draws must have matching root dimensions.",
        call. = FALSE
      )
    }
  } else if (paths1_present) {
    path_dim <- dim(paths1)
    terminal1 <- matrix(
      paths1[, path_dim[2L], , drop = FALSE],
      nrow = path_dim[1L], ncol = path_dim[3L]
    )
    terminal2 <- matrix(
      paths2[, path_dim[2L], , drop = FALSE],
      nrow = path_dim[1L], ncol = path_dim[3L]
    )
  } else {
    stop(
      "The fit contains neither terminal nor full state draws.",
      call. = FALSE
    )
  }

  p <- nrow(terminal1)
  n_save <- ncol(terminal1)
  if (paths1_present) {
    path_dim <- dim(paths1)
    if (!identical(path_dim[c(1L, 3L)], c(p, n_save))) {
      stop(
        "Full and terminal state draws have incompatible retained-draw dimensions.",
        call. = FALSE
      )
    }
    terminal_from_paths1 <- matrix(
      paths1[, path_dim[2L], , drop = FALSE],
      nrow = p, ncol = n_save
    )
    terminal_from_paths2 <- matrix(
      paths2[, path_dim[2L], , drop = FALSE],
      nrow = p, ncol = n_save
    )
    if (terminal1_present &&
        (!identical(as.numeric(terminal1), as.numeric(terminal_from_paths1)) ||
         !identical(as.numeric(terminal2), as.numeric(terminal_from_paths2)))) {
      stop(
        "Stored terminal states do not equal the last retained full-state slices.",
        call. = FALSE
      )
    }
  }

  expanded_p <- object$expanded_model$p %||% NULL
  if (!is.null(expanded_p)) {
    expanded_p <- .rqr_scalar_integer(
      expanded_p, "expanded_model$p", 1L
    )
    if (!identical(expanded_p, p)) {
      stop(
        "Stored terminal-state dimension does not match expanded_model$p.",
        call. = FALSE
      )
    }
  }

  fitted_fields <- c(
    "samp.eta_root1", "samp.eta_root2", "samp.lambda", "y"
  )
  fitted_fields_present <- vapply(
    fitted_fields, function(field) !is.null(object[[field]]), logical(1L)
  )
  draw_binding_status <- "unbound_external_state_fixture"
  stored <- NULL
  if (any(fitted_fields_present)) {
    stored <- .rqr_validate_dlm_stored_draws(object)
    if (ncol(stored$eta_root1) != n_save) {
      stop(
        "Stored terminal states are not aligned with the retained DLM draws.",
        call. = FALSE
      )
    }
    draw_binding_status <- "fit_retained_draws"
  }

  if (!is.null(stored) &&
      (is.null(expanded_p) || is.null(object$expanded_model$FF))) {
    stop(
      paste(
        "A fitted-draw forecast requires the stored expanded-model",
        "state dimension and observation design."
      ),
      call. = FALSE
    )
  }
  if (!is.null(stored)) {
    FF <- object$expanded_model$FF
    if (!is.matrix(FF) || !is.numeric(FF) || is.object(FF) ||
        !identical(dim(FF), c(p, nrow(stored$eta_root1))) ||
        any(!is.finite(FF))) {
      stop(
        "expanded_model$FF is incompatible with the stored forecast state.",
        call. = FALSE
      )
    }
    fitted_terminal1 <- colSums(
      terminal1 * as.numeric(FF[, ncol(FF)])
    )
    fitted_terminal2 <- colSums(
      terminal2 * as.numeric(FF[, ncol(FF)])
    )
    ordinate_scale <- pmax(
      1, abs(stored$eta_root1[nrow(stored$eta_root1), ]),
      abs(stored$eta_root2[nrow(stored$eta_root2), ])
    )
    tolerance <- 100 * .Machine$double.eps * ordinate_scale
    if (any(abs(
        fitted_terminal1 -
          stored$eta_root1[nrow(stored$eta_root1), ]
      ) > tolerance) ||
        any(abs(
          fitted_terminal2 -
            stored$eta_root2[nrow(stored$eta_root2), ]
        ) > tolerance)) {
      stop(
        paste(
          "Stored terminal states are not draw-wise aligned with the",
          "terminal fitted root ordinates."
        ),
        call. = FALSE
      )
    }
  }

  evolution_mode <- object$model_spec$evolution_mode %||% NULL
  if (!is.character(evolution_mode) ||
      length(evolution_mode) != 1L || is.na(evolution_mode) ||
      !evolution_mode %in% c(
        "fixed_W", "discount_template", "component_scale",
        "adaptive_discount"
      )) {
    stop(
      "model_spec$evolution_mode is missing or unsupported.",
      call. = FALSE
    )
  }
  component_scale <- object$samp.evolution_scale
  if (identical(evolution_mode, "component_scale")) {
    evolution <- object$evolution
    if (!inherits(evolution, "rqr_evolution") ||
        !identical(evolution$mode, "component_scale")) {
      stop(
        "A component-scale forecast requires its stored rqr_evolution contract.",
        call. = FALSE
      )
    }
    component_dims <- .rqr_positive_integer_vector(
      evolution$component_dims, "evolution$component_dims"
    )
    component_names <- evolution$component_names
    if (sum(component_dims) != p ||
        !is.character(component_names) ||
        length(component_names) != length(component_dims) ||
        anyNA(component_names) || any(!nzchar(component_names)) ||
        anyDuplicated(component_names)) {
      stop(
        "Stored component names and dimensions do not match the state dimension.",
        call. = FALSE
      )
    }
    if (!is.matrix(component_scale) ||
        !is.numeric(component_scale) || is.object(component_scale) ||
        !identical(
          dim(component_scale), c(n_save, length(component_dims))
        ) ||
        any(!is.finite(component_scale)) ||
        any(component_scale <= 0)) {
      stop(
        paste(
          "samp.evolution_scale must be a finite positive",
          "retained-draw-by-component numeric matrix."
        ),
        call. = FALSE
      )
    }
    if (!identical(colnames(component_scale), component_names)) {
      stop(
        paste(
          "samp.evolution_scale columns must exactly follow the stored",
          "component-name order."
        ),
        call. = FALSE
      )
    }
    storage.mode(component_scale) <- "double"
  } else if (!is.null(component_scale)) {
    stop(
      "Non-component-scale fits must not contain component-scale draws.",
      call. = FALSE
    )
  }

  list(
    terminal_root1 = terminal1,
    terminal_root2 = terminal2,
    evolution_scale = component_scale,
    state_dimension = p,
    n_save = n_save,
    draw_binding_status = draw_binding_status
  )
}

.rqr_validate_dlm_draws <- function(object, draws) {
  stored <- .rqr_validate_dlm_stored_draws(object)
  allowed_fields <- c(
    "eta_root1", "eta_root2", "lambda", "index", "nd"
  )
  if (!is.list(draws) || is.object(draws)) {
    stop("draws must be a plain named list.", call. = FALSE)
  }
  .rqr_validate_named_list_fields(
    draws, "draws", allowed = allowed_fields
  )
  if (!all(c("eta_root1", "eta_root2") %in% names(draws))) {
    stop(
      "draws must contain both root-ordinate matrices.",
      call. = FALSE
    )
  }
  eta1 <- draws$eta_root1
  eta2 <- draws$eta_root2
  if (!is.matrix(eta1) || !is.numeric(eta1) || is.object(eta1) ||
      !is.matrix(eta2) || !is.numeric(eta2) || is.object(eta2)) {
    stop(
      "The supplied DLM root-ordinate draws must be plain numeric matrices.",
      call. = FALSE
    )
  }
  if (!identical(dim(eta1), dim(eta2)) ||
      nrow(eta1) != nrow(stored$eta_root1) ||
      ncol(eta1) < 1L ||
      any(!is.finite(eta1)) || any(!is.finite(eta2))) {
    stop(
      paste(
        "Supplied DLM root-ordinate draws must be matching finite numeric",
        "matrices with one row per fitted time."
      ),
      call. = FALSE
    )
  }
  storage.mode(eta1) <- "double"
  storage.mode(eta2) <- "double"
  n_draw <- ncol(eta1)

  lambda <- NULL
  if ("lambda" %in% names(draws) && !is.null(draws$lambda)) {
    lambda <- draws$lambda
    if (!is.numeric(lambda) || is.object(lambda) ||
        !is.null(dim(lambda)) ||
        length(lambda) != n_draw ||
        any(!is.finite(lambda)) || any(lambda <= 0)) {
      stop(
        paste(
          "Supplied DLM lambda draws must be a finite positive numeric",
          "vector with one value per root draw."
        ),
        call. = FALSE
      )
    }
    storage.mode(lambda) <- "double"
    names(lambda) <- NULL
  }

  nd <- if ("nd" %in% names(draws)) {
    if (!is.numeric(draws$nd) || is.object(draws$nd) ||
        !is.null(dim(draws$nd)) || length(draws$nd) != 1L) {
      stop(
        "draws$nd must be one plain numeric scalar.",
        call. = FALSE
      )
    }
    .rqr_scalar_integer(draws$nd, "draws$nd", 1L)
  } else {
    n_draw
  }
  if (!identical(nd, n_draw)) {
    stop(
      "draws$nd must equal the number of root-draw columns.",
      call. = FALSE
    )
  }

  index <- if ("index" %in% names(draws)) {
    draws$index
  } else {
    rep(NA_integer_, n_draw)
  }
  if (!is.integer(index) || is.object(index) ||
      !is.null(dim(index)) || length(index) != n_draw) {
    stop(
      "draws$index must be an integer vector with one entry per draw.",
      call. = FALSE
    )
  }
  n_save <- ncol(stored$eta_root1)
  unknown_index <- all(is.na(index))
  if (!unknown_index) {
    if (anyNA(index) ||
        any(index < 1L) || any(index > n_save)) {
      stop(
        "Known draws$index values must lie within the fitted retained-draw range.",
        call. = FALSE
      )
    }
    if (n_draw <= n_save && anyDuplicated(index)) {
      stop(
        paste(
          "draws$index must be unique when sampling no more than the",
          "fitted retained draws."
        ),
        call. = FALSE
      )
    }
    expected_eta1 <- stored$eta_root1[, index, drop = FALSE]
    expected_eta2 <- stored$eta_root2[, index, drop = FALSE]
    if (!identical(as.numeric(eta1), as.numeric(expected_eta1)) ||
        !identical(as.numeric(eta2), as.numeric(expected_eta2))) {
      stop(
        paste(
          "Known draws$index values do not identify the supplied",
          "DLM root-ordinate draws in this fit."
        ),
        call. = FALSE
      )
    }
    if (!is.null(lambda) &&
        !identical(lambda, stored$lambda[index])) {
      stop(
        paste(
          "Known draws$index values do not identify the supplied",
          "DLM lambda draws in this fit."
        ),
        call. = FALSE
      )
    }
  }
  names(index) <- NULL

  # A minimal two-matrix input remains supported for explicit fitted-time
  # evaluation. Omitted metadata is represented explicitly as unbound.
  list(
    eta_root1 = eta1,
    eta_root2 = eta2,
    lambda = lambda,
    index = index,
    nd = nd
  )
}

#' Evaluate fitted RQR-DLM interval-root paths
#'
#' The method orders the two fitted root ordinates at every training time. It
#' does not propagate future states and does not define response-predictive
#' draws; use [rqr_forecast_roots()] for explicit future root evolution.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param nd Number of retained draws to use when `draws` is `NULL`.
#' @param draws Optional output from [rqr_posterior_draws()] for this fit. A
#'   plain list containing only the two time-by-draw root-ordinate matrices is
#'   also accepted for fitted-time evaluation; its draw indices are then
#'   retained as explicitly unknown rather than inferred.
#' @param seed Optional seed used only when draws are subsampled.
#' @param ... Reserved; supplying an argument is an error.
#' @return A list of fitted-time lower, upper, midpoint, and width draws and
#'   their row summaries, together with canonical draw metadata.
#' @export
predict_interval.rqr_dlm_mcmc <- function(
    object, nd = NULL, draws = NULL, seed = NULL, ...) {
  .rqr_reject_dots(
    list(...), "predict_interval.rqr_dlm_mcmc"
  )
  .rqr_validate_dlm_fit_if_present(object)
  .rqr_validate_dlm_stored_draws(object)
  if (is.null(draws)) {
    draws <- rqr_posterior_draws(object, nd = nd, seed = seed)
  } else {
    if (!is.null(nd) || !is.null(seed)) {
      stop(
        "nd and seed must be NULL when explicit draws are supplied.",
        call. = FALSE
      )
    }
    draws <- .rqr_validate_dlm_draws(object, draws)
  }
  lower <- pmin(draws$eta_root1, draws$eta_root2)
  upper <- pmax(draws$eta_root1, draws$eta_root2)
  list(
    lower_draws = lower, upper_draws = upper,
    midpoint_draws = 0.5 * (lower + upper), width_draws = upper - lower,
    lower_mean = rowMeans(lower), upper_mean = rowMeans(upper),
    midpoint_mean = rowMeans(0.5 * (lower + upper)),
    width_mean = rowMeans(upper - lower),
    draws = draws,
    draw_index = draws$index,
    model_spec = object$model_spec,
    response_predictive_draws = FALSE,
    interpretation = paste(
      "Fitted-time interval-root functionals;",
      "no response draw is defined."
    )
  )
}

#' Draw future root trajectories from an RQR-DLM fit
#'
#' These are state-evolution draws for interval roots, not predictive response
#' draws. Future evolution covariances are explicit and validated.
#'
#' @param object An `rqr_dlm_mcmc` fit.
#' @param FF_future State-by-horizon observation design.
#' @param GG_future Evolution matrix or cube.
#' @param W_future Explicit evolution covariance matrix or cube. Supply either
#'   this argument or `component_templates_future`.
#' @param component_templates_future Optional fixed future component templates.
#'   For a component-scale fit, these are combined with the saved draw-specific
#'   evolution multipliers.
#' @param nd Number of saved MCMC draws to use.
#' @param seed Optional seed.
#' @param numerical_policy Either `"fail"` or `"record_repair"`.
#' @param jitter_ladder Matrix-relative jitter ladder for record-repair mode.
#'   An exactly zero matrix uses a separately recorded absolute fallback.
#' @return Future root and ordered endpoint draws with repair diagnostics and
#'   an explicit retained-draw binding status. Hand-built state-only fixtures
#'   are marked unbound rather than being represented as fitted-draw indices.
#' @export
rqr_forecast_roots <- function(
    object, FF_future, GG_future, W_future = NULL,
    component_templates_future = NULL, nd = NULL, seed = NULL,
    numerical_policy = object$model_spec$numerical_policy %||% "fail",
    jitter_ladder = object$misc$jitter_ladder %||% c(0, 1e-12, 1e-10, 1e-8, 1e-6)) {
  .rqr_validate_dlm_fit_if_present(object)
  forecast_state <- .rqr_validate_dlm_forecast_state(object)
  terminal1 <- forecast_state$terminal_root1
  terminal2 <- forecast_state$terminal_root2
  p <- forecast_state$state_dimension
  if (!is.matrix(FF_future) || !is.numeric(FF_future) ||
      is.object(FF_future)) {
    stop("FF_future must be a plain numeric p x H matrix.", call. = FALSE)
  }
  H <- ncol(FF_future)
  if (nrow(FF_future) != p || H < 1L || any(!is.finite(FF_future))) {
    stop("FF_future must be finite p x H.", call. = FALSE)
  }
  storage.mode(FF_future) <- "double"
  GG <- .rqr_expand_cube(GG_future, H, p, "GG_future")
  if (!is.null(W_future) && !is.null(component_templates_future)) {
    stop("Supply W_future or component_templates_future, not both.", call. = FALSE)
  }
  if (is.null(W_future) && is.null(component_templates_future)) {
    stop("Supply W_future or component_templates_future.", call. = FALSE)
  }
  component_future <- !is.null(component_templates_future)
  W_fixed <- NULL
  future_component_evolution <- NULL
  if (component_future) {
    if (!identical(object$model_spec$evolution_mode, "component_scale") ||
        is.null(object$samp.evolution_scale)) {
      stop(
        "component_templates_future requires a component_scale fit with saved scale draws.",
        call. = FALSE
      )
    }
    future_component_evolution <- rqr_evolution_component_scale(
      templates = component_templates_future,
      component_dims = object$evolution$component_dims,
      prior = object$evolution$prior,
      initial = 1,
      component_names = object$evolution$component_names
    )
    .rqr_expand_component_templates(future_component_evolution, H, p)
  } else {
    W_fixed <- .rqr_prepare_evolution(
      list(mode = "fixed_W", W = W_future), p, H
    )$W
  }
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  if (!is.numeric(jitter_ladder) || is.object(jitter_ladder) ||
      !is.null(dim(jitter_ladder)) || !length(jitter_ladder) ||
      any(!is.finite(jitter_ladder)) || any(jitter_ladder < 0)) {
    stop(
      paste(
        "jitter_ladder must be a nonempty plain numeric vector of finite",
        "nonnegative values."
      ),
      call. = FALSE
    )
  }
  ladder <- .rqr_jitter_ladder(numerical_policy, jitter_ladder)
  n_save <- forecast_state$n_save
  if (!is.null(seed)) {
    seed <- .rqr_scalar_integer(seed, "seed", 0L)
  }
  if (!is.null(nd)) {
    nd <- .rqr_scalar_integer(nd, "nd", 1L)
  }
  if (!is.null(seed)) set.seed(seed)
  idx <- if (is.null(nd)) seq_len(n_save) else {
    sample.int(n_save, nd, replace = nd > n_save)
  }
  root1 <- root2 <- matrix(NA_real_, H, length(idx))
  repairs <- NULL
  for (j in seq_along(idx)) {
    s1 <- terminal1[, idx[j]]
    s2 <- terminal2[, idx[j]]
    W_draw <- if (component_future) {
      .rqr_materialize_component_evolution(
        future_component_evolution,
        q = forecast_state$evolution_scale[idx[j], ],
        n_time = H,
        p = p
      )$W
    } else {
      W_fixed
    }
    for (hh in seq_len(H)) {
      mu1 <- drop(GG[, , hh] %*% s1)
      mu2 <- drop(GG[, , hh] %*% s2)
      d1 <- .rqr_sample_mvnorm_covariance(
        mu1, W_draw[, , hh], ladder, numerical_policy
      )
      d2 <- .rqr_sample_mvnorm_covariance(
        mu2, W_draw[, , hh], ladder, numerical_policy
      )
      s1 <- d1$draw
      s2 <- d2$draw
      new_repair <- .rqr_add_repair_record(
        .rqr_empty_repair_records(), "future_state_covariance_root1", hh, d1$info
      )
      if (nrow(new_repair)) {
        new_repair$draw <- j; new_repair$root <- 1L
        repairs <- if (is.null(repairs)) new_repair else rbind(repairs, new_repair)
      }
      new_repair <- .rqr_add_repair_record(
        .rqr_empty_repair_records(), "future_state_covariance_root2", hh, d2$info
      )
      if (nrow(new_repair)) {
        new_repair$draw <- j; new_repair$root <- 2L
        repairs <- if (is.null(repairs)) new_repair else rbind(repairs, new_repair)
      }
      root1[hh, j] <- drop(crossprod(FF_future[, hh], s1))
      root2[hh, j] <- drop(crossprod(FF_future[, hh], s2))
    }
  }
  lower <- pmin(root1, root2)
  upper <- pmax(root1, root2)
  list(
    eta_root1 = root1, eta_root2 = root2,
    lower_draws = lower, upper_draws = upper,
    midpoint_draws = 0.5 * (lower + upper), width_draws = upper - lower,
    lower_mean = rowMeans(lower), upper_mean = rowMeans(upper),
    draw_index = idx,
    draw_binding_status = forecast_state$draw_binding_status,
    diagnostics = list(
      numerical_policy = numerical_policy,
      repair_count = if (is.null(repairs)) 0L else nrow(repairs),
      repair_records = repairs %||% data.frame(),
      future_evolution_mode = if (component_future) "component_scale" else "fixed_W",
      draw_binding_status = forecast_state$draw_binding_status,
      component_scale_draws = if (component_future) {
        forecast_state$evolution_scale[idx, , drop = FALSE]
      } else {
        NULL
      }
    ),
    interpretation = "Future interval-root state draws; no response simulation contract is implied."
  )
}

#' Print an RQR-DLM MCMC summary
#'
#' @param x An `rqr_dlm_mcmc` fit.
#' @param ... Reserved; supplying an argument is an error.
#' @return `x`, invisibly.
#' @export
print.rqr_dlm_mcmc <- function(x, ...) {
  .rqr_reject_dots(list(...), "print.rqr_dlm_mcmc")
  .rqr_validate_dlm_fit_if_present(x)
  stored <- .rqr_validate_dlm_stored_draws(x)
  cat("RQR dynamic MCMC fit\n")
  cat(sprintf("  coverage_level: %.4f\n", x$model_spec$coverage_level))
  cat(sprintf("  evolution_mode: %s\n", x$model_spec$evolution_mode))
  cat(sprintf("  target contract: %s\n", x$model_spec$target_contract))
  cat(sprintf("  numerical repairs: %d\n", x$model_spec$numerical_repair_count))
  cat(sprintf("  promotion eligible: %s\n", if (x$model_spec$promotion_eligible) "yes" else "no"))
  cat(sprintf("  draws:          %d\n", ncol(stored$eta_root1)))
  cat("  interpretation: generalized-Bayes root paths, not response draws\n")
  invisible(x)
}
