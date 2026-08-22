.rqr_ecm_run_start_intercept_cpp <- function(
    y, X, coverage_level, learning_rate, mean_tilt_info, beta_prior_obj,
    control, start) {
  constants <- rqr_constants(coverage_level, learning_rate)
  p <- ncol(X)
  prior_prec <- .rqr_prior_precision(beta_prior_obj, list(), p = p)
  if (p != 1L || !.rqr_ecm_intercept_only(X)) {
    stop("C++ ECM backend is currently available only for intercept-only designs.",
         call. = FALSE)
  }
  beta1 <- as.numeric(start$beta1)
  beta2 <- as.numeric(start$beta2)
  response_product_scale <- .rqr_ecm_response_product_scale(y)
  base_floor <- control$residual_product_floor * response_product_scale
  floor_schedule <- base_floor * control$floor_schedule
  cpp <- rqr_ecm_intercept_run_cpp(
    y = y,
    coverage_level = constants$alpha,
    learning_rate = constants$omega,
    mean_tilt = mean_tilt_info$observed,
    prior_prec = prior_prec[[1L]],
    beta1_start = beta1[[1L]],
    beta2_start = beta2[[1L]],
    max_iter = control$max_iter,
    tol_objective = control$tol_objective,
    tol_parameters = control$tol_parameters,
    tol_stationarity = control$tol_stationarity,
    stable_iterations = control$stable_iterations,
    residual_product_floor = control$residual_product_floor,
    floor_schedule = control$floor_schedule,
    floor_type = control$floor_type,
    monotone_backtracking = control$monotone_backtracking,
    backtracking_max_steps = control$backtracking_max_steps,
    monotone_tolerance = control$monotone_tolerance,
    canonicalize_complete_roots = control$canonicalize_complete_roots,
    start_label = start$label
  )
  beta1 <- as.numeric(cpp$beta_root1)
  beta2 <- as.numeric(cpp$beta_root2)
  objective <- .rqr_ecm_objective(
    y, X, beta1, beta2, constants, mean_tilt_info$observed,
    prior_prec, prior_prec
  )
  final_stationarity <- .rqr_ecm_stationarity(
    y, X, beta1, beta2, constants, mean_tilt_info$observed,
    prior_prec, prior_prec
  )
  final_latent <- .rqr_ecm_latent_inverse_mean(
    objective$residual_products,
    coverage_level = constants$alpha,
    residual_product_floor = utils::tail(floor_schedule, 1L),
    floor_type = control$floor_type
  )
  list(
    beta_root1 = beta1,
    beta_root2 = beta2,
    objective = objective,
    objective_trace = cpp$objective_trace,
    latent_inverse_mean = final_latent$inverse_mean,
    latent_mean = .rqr_ecm_latent_mean(
      objective$residual_products, constants$alpha, constants$omega
    ),
    residual_products = objective$residual_products,
    minimum_absolute_residual_product =
      min(abs(objective$residual_products)),
    residual_floor = list(
      relative = control$residual_product_floor,
      absolute = base_floor,
      final_absolute = utils::tail(floor_schedule, 1L),
      response_product_scale = response_product_scale,
      floor_type = control$floor_type,
      schedule = floor_schedule
    ),
    safeguard_used = base_floor > 0,
    exact_ecm_eligible = !isTRUE(cpp$zero_residual_encountered),
    backtracking_count = as.integer(cpp$backtracking_count),
    precision_repairs = as.integer(cpp$precision_repairs),
    condition_numbers = list(root1 = 1, root2 = 1),
    iterations = as.integer(cpp$iterations),
    converged = isTRUE(cpp$converged),
    convergence_code = as.character(cpp$convergence_code)[1L],
    stationarity_diagnostic = final_stationarity,
    selected_start_label = as.character(cpp$selected_start_label)[1L],
    root_swap_count = as.integer(cpp$root_swap_count),
    ecm_backend = "cpp"
  )
}

.rqr_ecm_run_start <- function(y, X, coverage_level, learning_rate,
                               mean_tilt_info, beta_prior_obj, control,
                               start) {
  if (identical(control$ecm_backend, "cpp")) {
    return(.rqr_ecm_run_start_intercept_cpp(
      y = y,
      X = X,
      coverage_level = coverage_level,
      learning_rate = learning_rate,
      mean_tilt_info = mean_tilt_info,
      beta_prior_obj = beta_prior_obj,
      control = control,
      start = start
    ))
  }
  constants <- rqr_constants(coverage_level, learning_rate)
  p <- ncol(X)
  prior_prec <- .rqr_prior_precision(beta_prior_obj, list(), p = p)
  beta1 <- as.numeric(start$beta1)
  beta2 <- as.numeric(start$beta2)
  if (isTRUE(control$canonicalize_complete_roots)) {
    canon <- .rqr_ecm_canonicalize_roots(beta1, beta2, X)
    beta1 <- canon$beta1
    beta2 <- canon$beta2
  }
  response_product_scale <- .rqr_ecm_response_product_scale(y)
  base_floor <- control$residual_product_floor * response_product_scale
  floor_schedule <- base_floor * control$floor_schedule
  objective <- .rqr_ecm_objective(
    y, X, beta1, beta2, constants, mean_tilt_info$observed,
    prior_prec, prior_prec
  )
  trace_rows <- list(data.frame(
    start_label = start$label,
    stage = 0L,
    iteration = 0L,
    objective = objective$total,
    relative_objective_change = NA_real_,
    relative_parameter_change = NA_real_,
    stationarity = NA_real_,
    minimum_absolute_residual_product =
      min(abs(objective$residual_products)),
    residual_product_floor = NA_real_,
    backtracking_steps = 0L,
    step_size = 0,
    root_swap_after_cycle = FALSE,
    precision_repairs = 0L,
    condition_number_root1 = NA_real_,
    condition_number_root2 = NA_real_
  ))

  convergence_code <- "max_iter_reached"
  converged <- FALSE
  stalled <- FALSE
  stable_count <- 0L
  total_backtracking <- 0L
  precision_repairs <- 0L
  root_swap_count <- 0L
  zero_residual_encountered <- FALSE
  iterations <- 0L
  stage_iter_limit <- max(1L, ceiling(control$max_iter / length(floor_schedule)))
  last_sys1 <- last_sys2 <- NULL

  for (stage in seq_along(floor_schedule)) {
    floor_current <- floor_schedule[[stage]]
    for (stage_iter in seq_len(stage_iter_limit)) {
      if (iterations >= control$max_iter || stalled || converged) break
      iterations <- iterations + 1L
      beta1_old <- beta1
      beta2_old <- beta2
      objective_old <- objective

      latent <- .rqr_ecm_latent_inverse_mean(
        objective_old$residual_products,
        coverage_level = constants$alpha,
        residual_product_floor = floor_current,
        floor_type = control$floor_type
      )
      zero_residual_encountered <- zero_residual_encountered ||
        latent$zero_residual_count > 0L
      sys1 <- .rqr_root_gaussian_system(
        y = y,
        X = X,
        beta_other = beta2,
        constants = constants,
        prior_prec = prior_prec,
        mean_tilt_observed = mean_tilt_info$observed,
        latent_mode = "inverse_moment",
        inverse_V_mean = latent$inverse_mean
      )
      mean1 <- .rqr_ecm_precision_mean(
        sys1$precision, sys1$rhs, jitter = control$precision_jitter
      )
      beta1_star <- mean1$mean

      sys2 <- .rqr_root_gaussian_system(
        y = y,
        X = X,
        beta_other = beta1_star,
        constants = constants,
        prior_prec = prior_prec,
        mean_tilt_observed = mean_tilt_info$observed,
        latent_mode = "inverse_moment",
        inverse_V_mean = latent$inverse_mean
      )
      mean2 <- .rqr_ecm_precision_mean(
        sys2$precision, sys2$rhs, jitter = control$precision_jitter
      )
      beta2_star <- mean2$mean

      swapped <- FALSE
      if (isTRUE(control$canonicalize_complete_roots)) {
        canon <- .rqr_ecm_canonicalize_roots(beta1_star, beta2_star, X)
        beta1_star <- canon$beta1
        beta2_star <- canon$beta2
        swapped <- isTRUE(canon$swapped)
        root_swap_count <- root_swap_count + as.integer(swapped)
      }

      candidate_objective <- .rqr_ecm_objective(
        y, X, beta1_star, beta2_star, constants, mean_tilt_info$observed,
        prior_prec, prior_prec
      )
      step_size <- 1
      backtracking_steps <- 0L
      accepted <- candidate_objective$total <=
        objective_old$total + control$monotone_tolerance

      if (!accepted && isTRUE(control$monotone_backtracking)) {
        for (bt in seq_len(control$backtracking_max_steps)) {
          lambda <- 0.5^bt
          bt_beta1 <- beta1_old + lambda * (beta1_star - beta1_old)
          bt_beta2 <- beta2_old + lambda * (beta2_star - beta2_old)
          if (isTRUE(control$canonicalize_complete_roots)) {
            canon <- .rqr_ecm_canonicalize_roots(bt_beta1, bt_beta2, X)
            bt_beta1 <- canon$beta1
            bt_beta2 <- canon$beta2
          }
          bt_objective <- .rqr_ecm_objective(
            y, X, bt_beta1, bt_beta2, constants, mean_tilt_info$observed,
            prior_prec, prior_prec
          )
          backtracking_steps <- bt
          if (bt_objective$total <=
              objective_old$total + control$monotone_tolerance) {
            beta1_star <- bt_beta1
            beta2_star <- bt_beta2
            candidate_objective <- bt_objective
            step_size <- lambda
            accepted <- TRUE
            break
          }
        }
      }

      if (!accepted) {
        convergence_code <- "stalled_backtracking"
        stalled <- TRUE
        break
      }

      beta1 <- beta1_star
      beta2 <- beta2_star
      objective <- candidate_objective
      last_sys1 <- sys1
      last_sys2 <- sys2
      total_backtracking <- total_backtracking + backtracking_steps
      precision_repairs <- precision_repairs +
        mean1$repair_count + mean2$repair_count

      parameter_change <- max(abs(c(beta1 - beta1_old, beta2 - beta2_old)))
      parameter_scale <- 1 + max(abs(c(beta1_old, beta2_old)))
      relative_parameter_change <- parameter_change / parameter_scale
      relative_objective_change <- abs(objective$total - objective_old$total) /
        (1 + abs(objective_old$total))
      stationarity <- .rqr_ecm_stationarity(
        y, X, beta1, beta2, constants, mean_tilt_info$observed,
        prior_prec, prior_prec
      )
      is_final_stage <- stage == length(floor_schedule)
      stable <- is_final_stage &&
        relative_objective_change <= control$tol_objective &&
        relative_parameter_change <= control$tol_parameters &&
        stationarity$max_abs_midpoint_gradient <= control$tol_stationarity
      stable_count <- if (stable) stable_count + 1L else 0L
      if (stable_count >= control$stable_iterations) {
        converged <- TRUE
        convergence_code <- "converged"
      }
      trace_rows[[length(trace_rows) + 1L]] <- data.frame(
        start_label = start$label,
        stage = as.integer(stage),
        iteration = as.integer(iterations),
        objective = objective$total,
        relative_objective_change = relative_objective_change,
        relative_parameter_change = relative_parameter_change,
        stationarity = stationarity$max_abs_midpoint_gradient,
        minimum_absolute_residual_product =
          stationarity$minimum_absolute_residual_product,
        residual_product_floor = floor_current,
        backtracking_steps = as.integer(backtracking_steps),
        step_size = step_size,
        root_swap_after_cycle = swapped,
        precision_repairs = as.integer(mean1$repair_count + mean2$repair_count),
        condition_number_root1 = mean1$condition_number,
        condition_number_root2 = mean2$condition_number
      )
      if (isTRUE(control$verbose)) {
        message(sprintf(
          "[rqr_ecm_fit] start=%s iter=%d objective=%.8g rel_obj=%.3g rel_beta=%.3g stat=%.3g",
          start$label, iterations, objective$total,
          relative_objective_change, relative_parameter_change,
          stationarity$max_abs_midpoint_gradient
        ))
      }
    }
  }
  if (!converged && !stalled && iterations < control$max_iter) {
    convergence_code <- "floor_schedule_completed"
  }
  objective_trace <- do.call(rbind, trace_rows)
  final_stationarity <- .rqr_ecm_stationarity(
    y, X, beta1, beta2, constants, mean_tilt_info$observed,
    prior_prec, prior_prec
  )
  final_latent <- .rqr_ecm_latent_inverse_mean(
    objective$residual_products,
    coverage_level = constants$alpha,
    residual_product_floor = utils::tail(floor_schedule, 1L),
    floor_type = control$floor_type
  )
  list(
    beta_root1 = beta1,
    beta_root2 = beta2,
    objective = objective,
    objective_trace = objective_trace,
    latent_inverse_mean = final_latent$inverse_mean,
    latent_mean = .rqr_ecm_latent_mean(
      objective$residual_products, constants$alpha, constants$omega
    ),
    residual_products = objective$residual_products,
    minimum_absolute_residual_product =
      min(abs(objective$residual_products)),
    residual_floor = list(
      relative = control$residual_product_floor,
      absolute = base_floor,
      final_absolute = utils::tail(floor_schedule, 1L),
      response_product_scale = response_product_scale,
      floor_type = control$floor_type,
      schedule = floor_schedule
    ),
    safeguard_used = base_floor > 0,
    exact_ecm_eligible = !zero_residual_encountered,
    backtracking_count = as.integer(total_backtracking),
    precision_repairs = as.integer(precision_repairs),
    condition_numbers = list(
      root1 = if (is.null(last_sys1)) NA_real_ else suppressWarnings(kappa(last_sys1$precision, exact = FALSE)),
      root2 = if (is.null(last_sys2)) NA_real_ else suppressWarnings(kappa(last_sys2$precision, exact = FALSE))
    ),
    iterations = as.integer(iterations),
    converged = converged,
    convergence_code = convergence_code,
    stationarity_diagnostic = final_stationarity,
    selected_start_label = start$label,
    root_swap_count = as.integer(root_swap_count)
  )
}

#' Fit fixed-target MT-RQR by expectation/conditional-maximization
#'
#' Computes a deterministic mode/M-estimator for the fixed content, fixed
#' response-scale tilt, fixed learning-rate RQR generalized-Bayes target.  ECM
#' is an optimizer for that loss-defined target; it is not a response-likelihood
#' EM algorithm and does not return posterior draws or a tolerance guarantee.
#'
#' @param y Response vector.
#' @param X Fixed design matrix.
#' @param coverage_level Target interval content `q` in `(0, 1)`.
#' @param learning_rate Positive generalized-Bayes learning rate.
#' @param mean_tilt Fixed scalar or row-specific response-scale mean tilt.
#' @param beta_prior_obj Beta prior object from [beta_prior()]. The ECM backend
#'   currently supports ridge roots only.
#' @param ecm_control Named list controlling iterations, safeguards,
#'   backtracking, and deterministic multi-start.
#' @param init Optional initial values or `previous_fit` for warm starts.
#' @param ... Reserved.
#' @return An `rqr_ecm` object.
#' @export
rqr_ecm_fit <- function(y, X, coverage_level, learning_rate = 1,
                        mean_tilt = 0, beta_prior_obj = NULL,
                        ecm_control = list(), init = list(), ...) {
  extra <- list(...)
  if (length(extra)) {
    stop("Additional arguments are reserved for future ECM extensions.",
         call. = FALSE)
  }
  dat <- .rqr_assert_xy(y, X)
  y <- dat$y
  X <- dat$X
  if (is.null(colnames(X))) {
    colnames(X) <- sprintf("x%d", seq_len(ncol(X)))
  }
  constants <- rqr_constants(coverage_level, learning_rate)
  control <- .rqr_ecm_assert_control(ecm_control)
  if (identical(control$ecm_backend, "cpp") &&
      !.rqr_ecm_intercept_only(X)) {
    stop("ecm_backend='cpp' is currently available only for intercept-only designs.",
         call. = FALSE)
  }
  mean_tilt_info <- .rqr_normalize_mean_tilt(
    mean_tilt, n = nrow(X), observed = rep(TRUE, nrow(X))
  )
  if (is.null(beta_prior_obj)) {
    beta_prior_obj <- beta_prior(
      "ridge",
      ridge = list(tau2 = ecm_control$beta_ridge_tau2 %||%
                     ecm_control$tau2 %||% 1e4)
    )
  }
  beta_prior_type <- as.character(beta_prior_obj$type %||% "ridge")[1L]
  if (!identical(beta_prior_type, "ridge")) {
    stop("rqr_ecm_fit currently supports ridge beta priors only.",
         call. = FALSE)
  }
  prior_prec <- .rqr_prior_precision(beta_prior_obj, list(), p = ncol(X))
  starts <- .rqr_ecm_start_list(y, X, constants$alpha, init, control)
  if (!length(starts)) {
    stop("No valid ECM starts were generated.", call. = FALSE)
  }
  start_results <- vector("list", length(starts))
  summary_rows <- vector("list", length(starts))
  for (ii in seq_along(starts)) {
    result <- tryCatch(
      .rqr_ecm_run_start(
        y = y,
        X = X,
        coverage_level = constants$alpha,
        learning_rate = constants$omega,
        mean_tilt_info = mean_tilt_info,
        beta_prior_obj = beta_prior_obj,
        control = control,
        start = starts[[ii]]
      ),
      error = function(e) e
    )
    start_results[[ii]] <- result
    summary_rows[[ii]] <- if (inherits(result, "error")) {
      data.frame(
        start_index = ii,
        start_label = starts[[ii]]$label,
        objective = Inf,
        converged = FALSE,
        convergence_code = paste("error:", conditionMessage(result)),
        iterations = 0L,
        backtracking_count = NA_integer_,
        exact_ecm_eligible = FALSE
      )
    } else {
      data.frame(
        start_index = ii,
        start_label = starts[[ii]]$label,
        objective = result$objective$total,
        converged = result$converged,
        convergence_code = result$convergence_code,
        iterations = result$iterations,
        backtracking_count = result$backtracking_count,
        exact_ecm_eligible = result$exact_ecm_eligible
      )
    }
  }
  multistart_summary <- do.call(rbind, summary_rows)
  feasible <- which(is.finite(multistart_summary$objective))
  if (!length(feasible)) {
    stop("All ECM starts failed.", call. = FALSE)
  }
  selected_index <- feasible[[which.min(multistart_summary$objective[feasible])]]
  best <- start_results[[selected_index]]
  if (!best$converged && isTRUE(control$fail_on_nonconvergence)) {
    stop(
      sprintf("ECM did not converge: %s", best$convergence_code),
      call. = FALSE
    )
  }

  eta1 <- drop(X %*% best$beta_root1)
  eta2 <- drop(X %*% best$beta_root2)
  endpoints <- rqr_order_endpoints(eta1, eta2)
  ordered_endpoint_summary <- list(
    lower = endpoints$lower,
    upper = endpoints$upper,
    midpoint = endpoints$midpoint,
    width = endpoints$width,
    lower_mean = mean(endpoints$lower),
    upper_mean = mean(endpoints$upper),
    midpoint_mean = mean(endpoints$midpoint),
    width_mean = mean(endpoints$width),
    empirical_coverage = mean(y >= endpoints$lower & y <= endpoints$upper)
  )
  objective_components <- best$objective[c(
    "total", "product_loss", "mean_tilt", "prior_root1", "prior_root2"
  )]
  algorithm_variant <- if (isTRUE(best$safeguard_used)) {
    if (identical(best$ecm_backend %||% control$ecm_backend, "cpp")) {
      "safeguarded_ecm_mm_cpp_intercept"
    } else {
      "safeguarded_ecm_mm"
    }
  } else {
    if (identical(best$ecm_backend %||% control$ecm_backend, "cpp")) {
      "exact_ecm_cpp_intercept"
    } else {
      "exact_ecm"
    }
  }
  out <- list(
    schema_version = .rqr_ecm_schema(),
    method = "mt_rqr_fixed_target_ecm",
    algorithm_variant = algorithm_variant,
    target_formula =
      "omega*sum[rho_q((y-eta1)(y-eta2))-q*delta*(eta1+eta2-2y)] + Gaussian root penalties",
    coverage_level = constants$alpha,
    learning_rate = constants$omega,
    mean_tilt = mean_tilt_info$observed,
    beta_prior_type = beta_prior_type,
    ecm_backend = control$ecm_backend,
    beta_root1 = best$beta_root1,
    beta_root2 = best$beta_root2,
    ordered_endpoint_summary = ordered_endpoint_summary,
    objective = best$objective$total,
    objective_components = objective_components,
    objective_trace = best$objective_trace,
    latent_inverse_mean = best$latent_inverse_mean,
    latent_mean_optional = best$latent_mean,
    residual_products = best$residual_products,
    minimum_absolute_residual_product =
      best$minimum_absolute_residual_product,
    residual_floor = best$residual_floor,
    safeguard_used = best$safeguard_used,
    exact_ecm_eligible = best$exact_ecm_eligible,
    backtracking_count = best$backtracking_count,
    precision_repairs = best$precision_repairs,
    condition_numbers = best$condition_numbers,
    iterations = best$iterations,
    converged = best$converged,
    convergence_code = best$convergence_code,
    stationarity_diagnostic = best$stationarity_diagnostic,
    multistart_summary = multistart_summary,
    selected_start = list(
      index = as.integer(selected_index),
      label = best$selected_start_label
    ),
    root_label_contract =
      "complete roots are exchangeable; any canonicalization swaps whole root coefficient blocks only",
    response_likelihood = FALSE,
    formal_tolerance_action = FALSE,
    conditional_on_fixed_target = TRUE,
    model_spec = list(
      family = "rqr_fixed_design",
      parameterization = "two_root_readouts",
      coverage_level = constants$alpha,
      learning_rate = constants$omega,
      fixed_learning_rate = constants$omega,
      mean_tilt = mean_tilt_info$observed,
      mean_tilt_mode = mean_tilt_info$mode,
      mean_tilt_digest = mean_tilt_info$digest,
      inference = "ecm",
      ecm_backend = control$ecm_backend,
      algorithm_variant = algorithm_variant,
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      calibrated_uncertainty = FALSE,
      beta_prior_type = beta_prior_type
    ),
    beta_prior = list(type = beta_prior_type, hypers = beta_prior_obj$hypers),
    y = y,
    X = X,
    provenance = .rqr_ecm_provenance(
      y, X, constants$alpha, constants$omega, mean_tilt_info,
      beta_prior_obj, control, best$objective_trace
    )
  )
  class(out) <- c("mti_ecm", "rqr_ecm", "mti_fit", "rqr_fit")
  out
}

#' Fit a fixed-target MT-RQR-ECM path
#'
#' This traces fixed content and fixed tilt targets.  It does not recalibrate
#' TCSP retained counts or tolerance confidence.
#'
#' @param y Response vector.
#' @param X Fixed design matrix.
#' @param target_contents Numeric vector of fixed contents in `(0, 1)`.
#' @param mean_tilts Scalar, row vector, per-target vector, or list of fixed
#'   tilts.
#' @param learning_rate Positive generalized-Bayes learning rate.
#' @param beta_prior_obj Optional ridge beta prior.
#' @param ecm_control ECM control list.
#' @param warm_start Use the previous path fit as an initialization.
#' @param global_multistart_check_every Frequency for full multi-start checks.
#' @return An `rqr_ecm_path` object.
#' @export
rqr_ecm_path <- function(y, X, target_contents, mean_tilts = 0,
                         learning_rate = 1, beta_prior_obj = NULL,
                         ecm_control = list(), warm_start = TRUE,
                         global_multistart_check_every = 1L) {
  dat <- .rqr_assert_xy(y, X)
  y <- dat$y
  X <- dat$X
  contents <- as.numeric(target_contents)
  if (!length(contents) || any(!is.finite(contents)) ||
      any(contents <= 0 | contents >= 1)) {
    stop("target_contents must contain finite values in (0, 1).",
         call. = FALSE)
  }
  global_multistart_check_every <- .rqr_scalar_integer(
    global_multistart_check_every,
    "global_multistart_check_every",
    1L
  )
  tilt_for <- function(ii) {
    if (is.list(mean_tilts)) {
      if (length(mean_tilts) == 1L) return(mean_tilts[[1L]])
      if (length(mean_tilts) == length(contents)) return(mean_tilts[[ii]])
      stop("mean_tilts list must have length 1 or length(target_contents).",
           call. = FALSE)
    }
    mt <- as.numeric(mean_tilts)
    if (length(mt) == 1L || length(mt) == nrow(X)) return(mt)
    if (length(mt) == length(contents)) return(mt[[ii]])
    stop(
      "mean_tilts must be scalar, length nrow(X), length target_contents, or a list.",
      call. = FALSE
    )
  }
  fits <- vector("list", length(contents))
  rows <- vector("list", length(contents))
  previous <- NULL
  for (ii in seq_along(contents)) {
    control_i <- ecm_control
    if (isTRUE(warm_start) && ii > 1L &&
        (ii %% global_multistart_check_every) != 0L) {
      control_i$multistart <- FALSE
    }
    init_i <- if (isTRUE(warm_start) && !is.null(previous)) {
      list(previous_fit = previous)
    } else {
      list()
    }
    fit <- rqr_ecm_fit(
      y = y,
      X = X,
      coverage_level = contents[[ii]],
      learning_rate = learning_rate,
      mean_tilt = tilt_for(ii),
      beta_prior_obj = beta_prior_obj,
      ecm_control = control_i,
      init = init_i
    )
    fits[[ii]] <- fit
    rows[[ii]] <- data.frame(
      target_content = fit$coverage_level,
      learning_rate = fit$learning_rate,
      objective = fit$objective,
      iterations = fit$iterations,
      converged = fit$converged,
      convergence_code = fit$convergence_code,
      selected_start = fit$selected_start$label,
      width_mean = fit$ordered_endpoint_summary$width_mean,
      minimum_absolute_residual_product =
        fit$minimum_absolute_residual_product,
      safeguard_used = fit$safeguard_used
    )
    previous <- fit
  }
  out <- list(
    schema_version = .rqr_ecm_path_schema(),
    method = "fixed_target_mti_ecm_path",
    legacy_method = "fixed_target_mt_rqr_ecm_path",
    target_contents = contents,
    learning_rate = learning_rate,
    warm_start = isTRUE(warm_start),
    global_multistart_check_every = global_multistart_check_every,
    path = do.call(rbind, rows),
    fits = fits,
    target_contract =
      "fixed q/delta path only; no TCSP scan calibration is performed"
  )
  class(out) <- c("mti_ecm_path", "rqr_ecm_path", "list")
  out
}

#' @export
print.rqr_ecm <- function(x, ...) {
  cat("MT-RQR fixed-target ECM fit\n")
  cat(sprintf("  coverage_level: %.4f\n", x$coverage_level))
  cat(sprintf("  learning_rate:  %.4f\n", x$learning_rate))
  cat(sprintf("  objective:      %.8g\n", x$objective))
  cat(sprintf("  variant:        %s\n", x$algorithm_variant))
  cat(sprintf("  convergence:    %s\n", x$convergence_code))
  invisible(x)
}

#' @export
summary.rqr_ecm <- function(object, ...) {
  out <- list(
    method = object$method,
    coverage_level = object$coverage_level,
    learning_rate = object$learning_rate,
    algorithm_variant = object$algorithm_variant,
    objective = object$objective,
    objective_components = object$objective_components,
    beta_root1 = object$beta_root1,
    beta_root2 = object$beta_root2,
    ordered_endpoint_summary = object$ordered_endpoint_summary[c(
      "lower_mean", "upper_mean", "midpoint_mean", "width_mean",
      "empirical_coverage"
    )],
    converged = object$converged,
    convergence_code = object$convergence_code,
    stationarity_diagnostic = object$stationarity_diagnostic,
    selected_start = object$selected_start,
    multistart_summary = object$multistart_summary
  )
  class(out) <- c("summary.rqr_ecm", "list")
  out
}

#' @export
coef.rqr_ecm <- function(object, ordered = FALSE, ...) {
  if (!isTRUE(ordered)) {
    out <- cbind(root1 = object$beta_root1, root2 = object$beta_root2)
    rownames(out) <- colnames(object$X)
    return(out)
  }
  endpoints <- predict_interval(object, X_new = object$X)
  data.frame(
    lower = endpoints$lower,
    upper = endpoints$upper,
    midpoint = endpoints$midpoint,
    width = endpoints$width
  )
}

#' @export
predict_interval.rqr_ecm <- function(object, X_new = NULL, ...) {
  if (!inherits(object, "rqr_ecm")) {
    stop("Expected an rqr_ecm object.", call. = FALSE)
  }
  if (is.null(X_new)) X_new <- object$X
  X_new <- as.matrix(X_new)
  if (ncol(X_new) != ncol(object$X)) {
    stop("X_new must have the same number of columns as the fitted design.",
         call. = FALSE)
  }
  eta1 <- drop(X_new %*% object$beta_root1)
  eta2 <- drop(X_new %*% object$beta_root2)
  endpoints <- rqr_order_endpoints(eta1, eta2)
  list(
    lower = endpoints$lower,
    upper = endpoints$upper,
    midpoint = endpoints$midpoint,
    width = endpoints$width,
    beta_root1 = object$beta_root1,
    beta_root2 = object$beta_root2,
    deterministic = TRUE,
    model_spec = object$model_spec
  )
}
