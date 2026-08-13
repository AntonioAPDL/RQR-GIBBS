#' Fit fixed-design RQR with exact Gibbs MCMC
#'
#' This fits the generalized-Bayes RQR interval readout for a fixed design
#' matrix. It is not a likelihood for the original response and does not define
#' posterior predictive response draws.
#'
#' @param y Response vector.
#' @param X Design matrix.
#' @param coverage_level Interval coverage level in `(0, 1)`.
#' @param learning_rate Positive fixed generalized-Bayes rate `omega_R`.
#' @param mean_tilt Fixed response-scale mean tilt. A scalar is recycled across
#'   rows; a vector must have length `nrow(X)`. Nonzero tilt is currently
#'   implemented for fixed-rate ridge MCMC only.
#' @param lambda_initial Positive initial inverse loss scale for learned modes.
#' @param loss_reference_scale Positive reference scale `s_L`. In fixed-rate
#'   mode, `learning_rate` is the effective rate `omega_R` and this argument
#'   does not alter the target. Learned modes use `omega_R=lambda/s_L`.
#' @param learning_rate_mode Learning-rate treatment. `"fixed_rate"` uses
#'   `learning_rate` directly as `omega_R`.
#'   `"learned_pseudoresidual_normalized"` samples the inverse-loss scale from
#'   the generalized posterior proportional
#'   to `lambda^T exp(-lambda L_c/s_L)`. `"learned_pure"` is a diagnostic target
#'   proportional to `exp(-lambda L_c/s_L)`.
#' @param lambda_prior Gamma prior for learned `lambda`, as a list with
#'   positive `shape` and `rate`. Target powers are fixed by the selected mode.
#' @param beta_prior_obj Beta prior object from [beta_prior()]. Version 1
#'   supports `"ridge"` and `"rhs_ns"`.
#' @param numerical_policy Either `"fail"` or `"record_repair"` for Gaussian
#'   precision factorizations.
#' @param provenance_control Optional primary-repository provenance plus named
#'   `external_repositories`. Each repository specification contains
#'   `repo_root`, a complete 40-character `expected_git_commit`, and optional
#'   runtime-package attestation metadata. RHS fits require the executing
#'   exdqlm namespace to match the pinned source for promotion eligibility.
#' @param mcmc_control Named list with `n_burn`, `n_mcmc`, `thin`, `seed`,
#'   `verbose`, `progress_every`, `precision_beta`, `store_latent_draws`, and
#'   optional `root_label_control`, `kernel_repetitions`, and
#'   `replica_exchange`. The latter is an opt-in likelihood-tempered,
#'   alternating-adjacent replica-exchange kernel for fixed-rate ridge RQR;
#'   only draws from its inverse-temperature-one replica are retained. The
#'   cold target is unchanged. `kernel_repetitions` is a
#'   positive integer giving the number of complete, exact Gibbs transitions
#'   composed between recorded iterations; its default is one. Root-label
#'   control governs the complete-root swap probability and post-processing
#'   canonicalization of exchangeable raw root labels.
#' @param init Optional initial values.
#' @param ... Reserved.
#' @return An `rqr_mcmc` object.
#' @export
rqr_mcmc_fit <- function(y, X, coverage_level, learning_rate = 1,
                         mean_tilt = 0,
                         lambda_initial = 1,
                         loss_reference_scale = 1,
                         learning_rate_mode = c(
                           "fixed_rate", "learned_pseudoresidual_normalized", "learned_pure"
                         ),
                         lambda_prior = list(shape = 4, rate = 4),
                         beta_prior_obj = NULL,
                         numerical_policy = c("fail", "record_repair"),
                         provenance_control = list(),
                         mcmc_control = list(),
                         init = list(),
                         ...) {
  dat <- .rqr_assert_xy(y, X)
  y <- dat$y
  X <- dat$X
  n <- nrow(X)
  p <- ncol(X)
  mean_tilt_info <- .rqr_normalize_mean_tilt(
    mean_tilt,
    n = n, observed = rep(TRUE, n)
  )
  learning_rate_mode <- .rqr_learning_rate_mode(learning_rate_mode)
  lambda_prior <- .rqr_lambda_prior(lambda_prior, learning_rate_mode)
  numerical_policy <- .rqr_numerical_policy(numerical_policy)
  provenance_control <- .rqr_provenance_control(provenance_control)
  loss_reference_scale <- as.numeric(loss_reference_scale %||% 1)[1L]
  if (!is.finite(loss_reference_scale) || loss_reference_scale <= 0) {
    stop("loss_reference_scale must be finite and positive.", call. = FALSE)
  }
  learning_rate <- as.numeric(learning_rate)[1L]
  if (!is.finite(learning_rate) || learning_rate <= 0) {
    stop("learning_rate must be finite and positive.", call. = FALSE)
  }
  learn_lambda <- !identical(learning_rate_mode, "fixed_rate")
  if (mean_tilt_info$nonzero && learn_lambda) {
    stop(
      "Nonzero mean_tilt is currently implemented only for learning_rate_mode='fixed_rate'.",
      call. = FALSE
    )
  }
  lambda_initial <- as.numeric(init$lambda %||% lambda_initial)[1L]
  if (!is.finite(lambda_initial) || lambda_initial <= 0) {
    stop("lambda_initial must be finite and positive.", call. = FALSE)
  }
  lambda_current <- if (learn_lambda) lambda_initial else learning_rate * loss_reference_scale
  constants <- rqr_constants(coverage_level, lambda_current / loss_reference_scale)

  if (is.null(beta_prior_obj)) {
    beta_prior_obj <- beta_prior("ridge", ridge = list(tau2 = 1e4))
  }
  if (!is.list(beta_prior_obj) || is.null(beta_prior_obj$type)) {
    stop("beta_prior_obj must be a beta prior object.", call. = FALSE)
  }
  beta_prior_type <- as.character(beta_prior_obj$type)[1L]
  if (!beta_prior_type %in% c("ridge", "rhs_ns")) {
    stop("rqr_mcmc_fit supports beta_prior_obj$type in {'ridge','rhs_ns'}.", call. = FALSE)
  }
  if (mean_tilt_info$nonzero && !identical(beta_prior_type, "ridge")) {
    stop(
      "Nonzero mean_tilt is currently implemented for ridge beta priors only; RHS-NS requires a separate propriety audit.",
      call. = FALSE
    )
  }
  if (identical(beta_prior_type, "rhs_ns")) {
    .qdesn_assert_rhs_prior_obj_intercept_policy(beta_prior_obj, context = "rqr_mcmc_fit")
    provenance_control <- .rqr_require_external_repository(
      provenance_control, "exdqlm", .rqr_pinned_exdqlm_commit(),
      runtime_package = "exdqlm"
    )
  }

  if (!is.list(mcmc_control)) stop("mcmc_control must be a list.", call. = FALSE)
  n_burn <- .rqr_scalar_integer(mcmc_control$n_burn %||% 1000L, "mcmc_control$n_burn", 0L)
  n_keep <- .rqr_scalar_integer(mcmc_control$n_mcmc %||% 1000L, "mcmc_control$n_mcmc", 1L)
  thin <- .rqr_scalar_integer(mcmc_control$thin %||% 1L, "mcmc_control$thin", 1L)
  seed <- mcmc_control$seed %||% mcmc_control$rng_seed %||% NULL
  if (!is.null(seed)) {
    seed <- .rqr_scalar_integer(seed, "mcmc_control$seed", 0L)
    set.seed(seed)
  } else {
    .rqr_restore_rng(init$rng_state %||% NULL)
  }
  verbose <- isTRUE(mcmc_control$verbose %||% FALSE)
  progress_every <- .rqr_scalar_integer(
    mcmc_control$progress_every %||% 100L, "mcmc_control$progress_every", 1L
  )
  kernel_repetitions <- .rqr_scalar_integer(
    mcmc_control$kernel_repetitions %||% 1L,
    "mcmc_control$kernel_repetitions", 1L
  )
  replica_exchange <- .rqr_normalize_replica_exchange_control(
    mcmc_control$replica_exchange %||% list()
  )
  store_latent_draws <- isTRUE(mcmc_control$store_latent_draws %||% FALSE)
  if (isTRUE(replica_exchange$enabled) &&
      (!identical(learning_rate_mode, "fixed_rate") ||
        !identical(beta_prior_type, "ridge") ||
        mean_tilt_info$nonzero || store_latent_draws)) {
    stop(
      paste(
        "Replica exchange currently requires fixed-rate, zero-tilt ridge",
        "RQR with store_latent_draws = FALSE."
      ),
      call. = FALSE
    )
  }
  root_label_control <- .rqr_normalize_root_label_control(
    mcmc_control$root_label_control %||% list(), X
  )
  precision_beta_cfg <- .exal_normalize_mcmc_precision_beta_cfg(
    mcmc_control$precision_beta %||% mcmc_control$precision %||% list()
  )
  precision_beta_cfg$jitter_ladder <- .rqr_jitter_ladder(
    numerical_policy, precision_beta_cfg$jitter_ladder
  )

  init_roots <- .rqr_init_roots(y, X, coverage_level, init = init)
  beta1 <- init_roots$beta1
  beta2 <- init_roots$beta2
  replica_beta1_initial <- .rqr_replica_initial_matrix(
    init$replica_beta_root1 %||% NULL, beta1,
    replica_exchange$replicas, p, "init$replica_beta_root1"
  )
  replica_beta2_initial <- .rqr_replica_initial_matrix(
    init$replica_beta_root2 %||% NULL, beta2,
    replica_exchange$replicas, p, "init$replica_beta_root2"
  )
  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  e <- rqr_residual_product(y, eta1, eta2)
  V <- as.numeric(init$latent_v %||% init$V %||% rep(constants$sigma, n))
  if (length(V) != n || any(!is.finite(V)) || any(V <= 0)) {
    V <- rep(constants$sigma, n)
  }

  state1 <- .rqr_prior_state_init(beta_prior_obj, p, init_state = init$beta_prior_state1 %||% init$rhs_ns_state1)
  state2 <- .rqr_prior_state_init(beta_prior_obj, p, init_state = init$beta_prior_state2 %||% init$rhs_ns_state2)

  beta1_draws <- matrix(NA_real_, n_keep, p)
  beta2_draws <- matrix(NA_real_, n_keep, p)
  colnames(beta1_draws) <- colnames(X)
  colnames(beta2_draws) <- colnames(X)
  latent_v_draws <- if (store_latent_draws) matrix(NA_real_, n_keep, n) else NULL
  lambda_draws <- numeric(n_keep)
  loss_trace <- numeric(n_burn + n_keep * thin)
  mean_tilted_target_loss_trace <- numeric(n_burn + n_keep * thin)
  tilt_linear_trace <- numeric(n_burn + n_keep * thin)
  lambda_trace <- numeric(n_burn + n_keep * thin)
  effective_learning_rate_trace <- numeric(n_burn + n_keep * thin)
  lambda_post_shape_trace <- rep(NA_real_, n_burn + n_keep * thin)
  lambda_post_rate_trace <- rep(NA_real_, n_burn + n_keep * thin)
  precision_strategy_root1 <- character(n_burn + n_keep * thin)
  precision_strategy_root2 <- character(n_burn + n_keep * thin)
  rhs_stats1 <- vector("list", n_keep)
  rhs_stats2 <- vector("list", n_keep)
  root_swap_trace <- logical(n_burn + n_keep * thin)
  root_swap_count_trace <- integer(n_burn + n_keep * thin)
  replica_energy_trace <- NULL
  replica_cold_label_trace <- NULL
  replica_swap_attempts <- integer(0L)
  replica_swap_accepts <- integer(0L)
  replica_round_trips <- integer(0L)
  repair_records <- NULL

  total_iter <- n_burn + n_keep * thin
  save_idx <- 0L
  if (!isTRUE(replica_exchange$enabled)) {
  for (iter in seq_len(total_iter)) {
    for (kernel_step in seq_len(kernel_repetitions)) {
      eta1 <- drop(X %*% beta1)
      eta2 <- drop(X %*% beta2)
      e <- rqr_residual_product(y, eta1, eta2)
      if (learn_lambda) {
        loss_for_lambda <- sum(rqr_check_loss(e, constants$alpha)) / loss_reference_scale
        lambda_post <- .rqr_lambda_posterior_params(
          loss_sum = loss_for_lambda,
          n = n,
          lambda_prior = lambda_prior,
          learning_rate_mode = learning_rate_mode
        )
        lambda_post_shape_trace[iter] <- lambda_post$shape
        lambda_post_rate_trace[iter] <- lambda_post$rate
        lambda_current <- stats::rgamma(1L, shape = lambda_post$shape, rate = lambda_post$rate)
        constants <- rqr_constants(coverage_level, lambda_current / loss_reference_scale)
      }
      gp <- rqr_gig_params(e, coverage_level = constants$alpha, learning_rate = constants$omega)
      V <- as.numeric(.sample_gig_devroye_required(
        1L,
        p = gp$p,
        a = gp$a,
        b_vec = gp$b,
        context = "rqr_mcmc_fit::latent_v"
      )[1L, ])

      prior_prec1 <- .rqr_prior_precision(beta_prior_obj, state1, p = p)
      upd1 <- .rqr_beta_update(
        y = y,
        X = X,
        beta_other = beta2,
        V = V,
        constants = constants,
        prior_prec = prior_prec1,
        precision_beta_cfg = precision_beta_cfg,
        context = list(
          iter = iter,
          n_burn = n_burn,
          likelihood_family = "rqr_generalized_bayes",
          beta_prior_type = beta_prior_type,
          root = "root1"
        ),
        mean_tilt_observed = if (mean_tilt_info$nonzero) {
          mean_tilt_info$observed
        } else {
          NULL
        }
      )
      beta1 <- upd1$draw
      pstats1 <- upd1$info %||% list()
      current_repair <- .rqr_add_repair_record(
        .rqr_empty_repair_records(), "beta_precision", NA_integer_, pstats1
      )
      if (nrow(current_repair)) {
        current_repair$iteration <- iter
        current_repair$root <- "root1"
        repair_records <- if (is.null(repair_records)) {
          current_repair
        } else {
          rbind(repair_records, current_repair)
        }
      }
      pr_upd1 <- .rqr_prior_state_update(beta_prior_obj, state1, beta1)
      state1 <- pr_upd1$state

      prior_prec2 <- .rqr_prior_precision(beta_prior_obj, state2, p = p)
      upd2 <- .rqr_beta_update(
        y = y,
        X = X,
        beta_other = beta1,
        V = V,
        constants = constants,
        prior_prec = prior_prec2,
        precision_beta_cfg = precision_beta_cfg,
        context = list(
          iter = iter,
          n_burn = n_burn,
          likelihood_family = "rqr_generalized_bayes",
          beta_prior_type = beta_prior_type,
          root = "root2"
        ),
        mean_tilt_observed = if (mean_tilt_info$nonzero) {
          mean_tilt_info$observed
        } else {
          NULL
        }
      )
      beta2 <- upd2$draw
      pstats2 <- upd2$info %||% list()
      current_repair <- .rqr_add_repair_record(
        .rqr_empty_repair_records(), "beta_precision", NA_integer_, pstats2
      )
      if (nrow(current_repair)) {
        current_repair$iteration <- iter
        current_repair$root <- "root2"
        repair_records <- if (is.null(repair_records)) {
          current_repair
        } else {
          rbind(repair_records, current_repair)
        }
      }
      pr_upd2 <- .rqr_prior_state_update(beta_prior_obj, state2, beta2)
      state2 <- pr_upd2$state

      if (stats::runif(1L) < root_label_control$swap_probability) {
        tmp <- beta1
        beta1 <- beta2
        beta2 <- tmp
        tmp <- state1
        state1 <- state2
        state2 <- tmp
        tmp <- pr_upd1
        pr_upd1 <- pr_upd2
        pr_upd2 <- tmp
        tmp <- pstats1
        pstats1 <- pstats2
        pstats2 <- tmp
        root_swap_count_trace[iter] <-
          root_swap_count_trace[iter] + 1L
      }
    }
    root_swap_trace[iter] <- root_swap_count_trace[iter] > 0L

    eta1 <- drop(X %*% beta1)
    eta2 <- drop(X %*% beta2)
    target_loss <- rqr_mean_tilt_loss(
      y, eta1, eta2, constants$alpha,
      mean_tilt = mean_tilt_info$full,
      details = TRUE
    )
    loss_trace[iter] <- sum(target_loss$product_loss)
    tilt_linear_trace[iter] <- sum(target_loss$linear_tilt)
    mean_tilted_target_loss_trace[iter] <- sum(target_loss$total)
    lambda_trace[iter] <- lambda_current
    effective_learning_rate_trace[iter] <- constants$omega
    precision_strategy_root1[iter] <- as.character(pstats1$strategy %||% "direct")
    precision_strategy_root2[iter] <- as.character(pstats2$strategy %||% "direct")

    if (iter > n_burn && ((iter - n_burn) %% thin == 0L)) {
      save_idx <- save_idx + 1L
      beta1_draws[save_idx, ] <- beta1
      beta2_draws[save_idx, ] <- beta2
      lambda_draws[save_idx] <- lambda_current
      if (store_latent_draws) latent_v_draws[save_idx, ] <- V
      rhs_stats1[[save_idx]] <- pr_upd1$stats %||% list()
      rhs_stats2[[save_idx]] <- pr_upd2$stats %||% list()
    }

    if (verbose && (iter %% progress_every == 0L || iter == total_iter)) {
      message(sprintf("[rqr_mcmc_fit] iter %d/%d loss=%.6g", iter, total_iter, loss_trace[iter]))
    }
  }
  } else {
    temperatures <- replica_exchange$inverse_temperatures
    n_replicas <- replica_exchange$replicas
    replica_beta1 <- lapply(
      seq_len(n_replicas),
      function(index) as.numeric(replica_beta1_initial[index, ])
    )
    replica_beta2 <- lapply(
      seq_len(n_replicas),
      function(index) as.numeric(replica_beta2_initial[index, ])
    )
    replica_V <- lapply(
      temperatures,
      function(temperature) rep(
        rqr_constants(
          coverage_level, learning_rate * temperature
        )$sigma,
        n
      )
    )
    replica_labels <- seq_len(n_replicas)
    label_reached_hot <- logical(n_replicas)
    replica_round_trips <- integer(n_replicas)
    replica_swap_attempts <- integer(n_replicas - 1L)
    replica_swap_accepts <- integer(n_replicas - 1L)
    replica_cold_label_trace <- integer(total_iter)
    if (isTRUE(replica_exchange$store_energy_trace)) {
      replica_energy_trace <- matrix(
        NA_real_, total_iter, n_replicas,
        dimnames = list(NULL, sprintf("temperature_%02d", seq_len(n_replicas)))
      )
    }
    swap_round <- 0L
    for (iter in seq_len(total_iter)) {
      replica_root_swaps <- integer(n_replicas)
      replica_pstats1 <- vector("list", n_replicas)
      replica_pstats2 <- vector("list", n_replicas)
      for (replica in seq_len(n_replicas)) {
        replica_constants <- rqr_constants(
          coverage_level, learning_rate * temperatures[[replica]]
        )
        for (kernel_step in seq_len(kernel_repetitions)) {
          eta1_replica <- drop(X %*% replica_beta1[[replica]])
          eta2_replica <- drop(X %*% replica_beta2[[replica]])
          e_replica <- rqr_residual_product(
            y, eta1_replica, eta2_replica
          )
          gp <- rqr_gig_params(
            e_replica,
            coverage_level = replica_constants$alpha,
            learning_rate = replica_constants$omega
          )
          replica_V[[replica]] <- as.numeric(
            .sample_gig_devroye_required(
              1L, p = gp$p, a = gp$a, b_vec = gp$b,
              context = "rqr_mcmc_fit::replica_exchange_latent_v"
            )[1L, ]
          )
          prior_prec1 <- .rqr_prior_precision(
            beta_prior_obj, list(), p = p
          )
          upd1 <- .rqr_beta_update(
            y = y, X = X,
            beta_other = replica_beta2[[replica]],
            V = replica_V[[replica]],
            constants = replica_constants,
            prior_prec = prior_prec1,
            precision_beta_cfg = precision_beta_cfg,
            context = list(
              iter = iter, n_burn = n_burn,
              likelihood_family =
                "rqr_generalized_bayes_replica_exchange",
              beta_prior_type = beta_prior_type,
              root = "root1", replica = replica,
              inverse_temperature = temperatures[[replica]]
            )
          )
          replica_beta1[[replica]] <- upd1$draw
          replica_pstats1[[replica]] <- upd1$info %||% list()
          current_repair <- .rqr_add_repair_record(
            .rqr_empty_repair_records(), "beta_precision",
            NA_integer_, replica_pstats1[[replica]]
          )
          if (nrow(current_repair)) {
            current_repair$iteration <- iter
            current_repair$root <- "root1"
            current_repair$replica <- replica
            current_repair$inverse_temperature <-
              temperatures[[replica]]
            repair_records <- if (is.null(repair_records)) {
              current_repair
            } else {
              rbind(repair_records, current_repair)
            }
          }

          prior_prec2 <- .rqr_prior_precision(
            beta_prior_obj, list(), p = p
          )
          upd2 <- .rqr_beta_update(
            y = y, X = X,
            beta_other = replica_beta1[[replica]],
            V = replica_V[[replica]],
            constants = replica_constants,
            prior_prec = prior_prec2,
            precision_beta_cfg = precision_beta_cfg,
            context = list(
              iter = iter, n_burn = n_burn,
              likelihood_family =
                "rqr_generalized_bayes_replica_exchange",
              beta_prior_type = beta_prior_type,
              root = "root2", replica = replica,
              inverse_temperature = temperatures[[replica]]
            )
          )
          replica_beta2[[replica]] <- upd2$draw
          replica_pstats2[[replica]] <- upd2$info %||% list()
          current_repair <- .rqr_add_repair_record(
            .rqr_empty_repair_records(), "beta_precision",
            NA_integer_, replica_pstats2[[replica]]
          )
          if (nrow(current_repair)) {
            current_repair$iteration <- iter
            current_repair$root <- "root2"
            current_repair$replica <- replica
            current_repair$inverse_temperature <-
              temperatures[[replica]]
            repair_records <- if (is.null(repair_records)) {
              current_repair
            } else {
              rbind(repair_records, current_repair)
            }
          }
          if (stats::runif(1L) < root_label_control$swap_probability) {
            temporary <- replica_beta1[[replica]]
            replica_beta1[[replica]] <- replica_beta2[[replica]]
            replica_beta2[[replica]] <- temporary
            replica_root_swaps[[replica]] <-
              replica_root_swaps[[replica]] + 1L
          }
        }
      }

      energies <- vapply(seq_len(n_replicas), function(replica) {
        eta1_replica <- drop(X %*% replica_beta1[[replica]])
        eta2_replica <- drop(X %*% replica_beta2[[replica]])
        learning_rate * sum(rqr_check_loss(
          rqr_residual_product(y, eta1_replica, eta2_replica),
          coverage_level
        ))
      }, numeric(1L))
      if (iter %% replica_exchange$swap_every == 0L) {
        swap_round <- swap_round + 1L
        first <- if (swap_round %% 2L == 1L) 1L else 2L
        pairs <- if (first <= n_replicas - 1L) {
          seq.int(first, n_replicas - 1L, by = 2L)
        } else {
          integer(0L)
        }
        for (left in pairs) {
          right <- left + 1L
          replica_swap_attempts[[left]] <-
            replica_swap_attempts[[left]] + 1L
          log_acceptance <- .rqr_replica_exchange_log_acceptance(
            temperatures[[left]], temperatures[[right]],
            energies[[left]], energies[[right]]
          )
          if (log(stats::runif(1L)) < min(0, log_acceptance)) {
            temporary <- replica_beta1[[left]]
            replica_beta1[[left]] <- replica_beta1[[right]]
            replica_beta1[[right]] <- temporary
            temporary <- replica_beta2[[left]]
            replica_beta2[[left]] <- replica_beta2[[right]]
            replica_beta2[[right]] <- temporary
            temporary_root_swaps <- replica_root_swaps[[left]]
            replica_root_swaps[[left]] <- replica_root_swaps[[right]]
            replica_root_swaps[[right]] <- temporary_root_swaps
            energies[c(left, right)] <- energies[c(right, left)]
            temporary_label <- replica_labels[[left]]
            replica_labels[[left]] <- replica_labels[[right]]
            replica_labels[[right]] <- temporary_label
            replica_swap_accepts[[left]] <-
              replica_swap_accepts[[left]] + 1L
          }
        }
      }
      label_reached_hot[replica_labels[[n_replicas]]] <- TRUE
      cold_label <- replica_labels[[1L]]
      if (label_reached_hot[[cold_label]]) {
        replica_round_trips[[cold_label]] <-
          replica_round_trips[[cold_label]] + 1L
        label_reached_hot[[cold_label]] <- FALSE
      }
      replica_cold_label_trace[[iter]] <- cold_label
      if (!is.null(replica_energy_trace)) {
        replica_energy_trace[iter, ] <- energies
      }

      beta1 <- replica_beta1[[1L]]
      beta2 <- replica_beta2[[1L]]
      constants <- rqr_constants(coverage_level, learning_rate)
      eta1 <- drop(X %*% beta1)
      eta2 <- drop(X %*% beta2)
      target_loss <- rqr_mean_tilt_loss(
        y, eta1, eta2, constants$alpha,
        mean_tilt = mean_tilt_info$full, details = TRUE
      )
      loss_trace[[iter]] <- sum(target_loss$product_loss)
      tilt_linear_trace[[iter]] <- sum(target_loss$linear_tilt)
      mean_tilted_target_loss_trace[[iter]] <- sum(target_loss$total)
      lambda_trace[[iter]] <- lambda_current
      effective_learning_rate_trace[[iter]] <- constants$omega
      precision_strategy_root1[[iter]] <- as.character(
        replica_pstats1[[1L]]$strategy %||% "direct"
      )
      precision_strategy_root2[[iter]] <- as.character(
        replica_pstats2[[1L]]$strategy %||% "direct"
      )
      root_swap_count_trace[[iter]] <- replica_root_swaps[[1L]]
      root_swap_trace[[iter]] <- root_swap_count_trace[[iter]] > 0L

      if (iter > n_burn && ((iter - n_burn) %% thin == 0L)) {
        save_idx <- save_idx + 1L
        beta1_draws[save_idx, ] <- beta1
        beta2_draws[save_idx, ] <- beta2
        lambda_draws[[save_idx]] <- lambda_current
        rhs_stats1[[save_idx]] <- list()
        rhs_stats2[[save_idx]] <- list()
      }
      if (verbose &&
          (iter %% progress_every == 0L || iter == total_iter)) {
        message(sprintf(
          paste0(
            "[rqr_mcmc_fit replica exchange] iter %d/%d ",
            "loss=%.6g cold_label=%d"
          ),
          iter, total_iter, loss_trace[[iter]], cold_label
        ))
      }
    }
    beta1 <- replica_beta1[[1L]]
    beta2 <- replica_beta2[[1L]]
    eta1 <- drop(X %*% beta1)
    eta2 <- drop(X %*% beta2)
    final_gp <- rqr_gig_params(
      rqr_residual_product(y, eta1, eta2),
      coverage_level = coverage_level,
      learning_rate = learning_rate
    )
    V <- as.numeric(.sample_gig_devroye_required(
      1L, p = final_gp$p, a = final_gp$a, b_vec = final_gp$b,
      context = "rqr_mcmc_fit::replica_exchange_final_latent_v"
    )[1L, ])
    state1 <- list()
    state2 <- list()
    pr_upd1 <- list(state = state1, stats = list())
    pr_upd2 <- list(state = state2, stats = list())
    pstats1 <- replica_pstats1[[1L]] %||% list()
    pstats2 <- replica_pstats2[[1L]] %||% list()
  }

  summary <- .rqr_fit_summary(y, X, beta1_draws, beta2_draws)
  summary$beta_raw_root1_mean <- summary$beta_root1_mean
  summary$beta_raw_root2_mean <- summary$beta_root2_mean
  root_label_diagnostics <- NULL
  canonical_beta_lower <- NULL
  canonical_beta_upper <- NULL
  if (isTRUE(root_label_control$canonicalize_draws)) {
    root_label_diagnostics <- tryCatch(
      rqr_canonicalize_root_draws(
        beta_root1 = beta1_draws,
        beta_root2 = beta2_draws,
        X_audit = root_label_control$audit_X,
        reference_beta_lower = root_label_control$reference_beta_lower,
        reference_beta_upper = root_label_control$reference_beta_upper,
        audit_weights = root_label_control$audit_weights,
        gap_tolerance = root_label_control$gap_tolerance,
        ambiguity_tolerance = root_label_control$ambiguity_tolerance,
        reference_method = root_label_control$reference_method,
        max_iter = root_label_control$max_iter,
        fail_on_ambiguous = root_label_control$fail_on_ambiguous
      ),
      error = function(e) {
        out <- list(
          schema_version = .rqr_root_label_schema(),
          object = "static_root_coefficients",
          status = "failed_with_error",
          error = conditionMessage(e),
          root_estimand = "unordered_root_pair",
          raw_root_labels_identified = FALSE,
          canonicalization_changes_chain = FALSE,
          interpretation = paste(
            "Canonical coefficient labels were not produced; raw root labels",
            "remain exchangeable and interval endpoints are obtained by",
            "pointwise sorting."
          )
        )
        class(out) <- c("rqr_root_label_diagnostics", "list")
        out
      }
    )
    if (identical(root_label_diagnostics$status, "ok")) {
      canonical_beta_lower <- root_label_diagnostics$canonical_beta_lower
      canonical_beta_upper <- root_label_diagnostics$canonical_beta_upper
      summary$beta_canonical_lower_mean <- colMeans(canonical_beta_lower)
      summary$beta_canonical_upper_mean <- colMeans(canonical_beta_upper)
    }
  }
  lambda_summary <- .rqr_lambda_summary(lambda_draws)
  effective_learning_rate_summary <- .rqr_lambda_summary(lambda_draws / loss_reference_scale)
  learning_rate_report <- if (learn_lambda) lambda_summary$mean / loss_reference_scale else learning_rate
  rng_state <- .rqr_rng_state()
  numerical_repair_count <- if (is.null(repair_records)) 0L else nrow(repair_records)
  numerically_exact <- numerical_repair_count == 0L
  provenance <- .rqr_provenance(
    data = list(y = y, X = X),
    matrices = list(X = X),
    numerical_policy = numerical_policy,
    initial_seed = seed,
    repo_root = provenance_control$repo_root,
    expected_git_commit = provenance_control$expected_git_commit,
    backend = "R_precision_cholesky",
    objects = list(
      target = list(
        coverage_level = constants$alpha,
        learning_rate_mode = learning_rate_mode,
        fixed_learning_rate = if (learn_lambda) NA_real_ else learning_rate,
        loss_reference_scale = loss_reference_scale,
        lambda_prior = lambda_prior,
        beta_prior_type = beta_prior_type,
        beta_prior_hypers = beta_prior_obj$hypers,
        mean_tilt_target = mean_tilt_info$contract,
        mean_tilt_digest = mean_tilt_info$digest,
        root_label_contract = root_label_control$contract,
        numerical_policy = numerical_policy,
        precision_beta = precision_beta_cfg
      ),
      transition = list(
        kernel = if (isTRUE(replica_exchange$enabled)) {
          "likelihood_tempered_replica_exchange"
        } else {
          "complete_exact_gibbs_composition"
        },
        kernel_repetitions = kernel_repetitions,
        replica_exchange = replica_exchange,
        replica_initial_root_digest = if (
          isTRUE(replica_exchange$enabled)
        ) {
          .rqr_digest(list(
            beta_root1 = replica_beta1_initial,
            beta_root2 = replica_beta2_initial
          ))
        } else {
          NA_character_
        }
      )
    ),
    external_repositories = provenance_control$external_repositories,
    required_external_repositories =
      provenance_control$required_external_repositories,
    primary_runtime_attestation =
      provenance_control$primary_runtime_attestation
  )
  target_numerical_eligible <- numerically_exact
  out <- list(
    method = "mcmc",
    family = "rqr_fixed_design",
    model_spec = list(
      family = "rqr_fixed_design",
      parameterization = "two_root_readouts",
      loss_name = if (mean_tilt_info$nonzero) {
        "mean_tilted_rqr_product_check_loss"
      } else {
        "rqr_residual_product_check_loss"
      },
      ordinary_loss_name = "rqr_residual_product_check_loss",
      mean_tilt_mode = mean_tilt_info$mode,
      mean_tilt = mean_tilt_info$full,
      mean_tilt_observed = mean_tilt_info$observed,
      mean_tilt_summary = mean_tilt_info$summary,
      mean_tilt_digest = mean_tilt_info$digest,
      coverage_level = constants$alpha,
      learning_rate = learning_rate_report,
      fixed_learning_rate = if (learn_lambda) NA_real_ else learning_rate,
      lambda_initial = lambda_initial,
      loss_reference_scale = loss_reference_scale,
      effective_learning_rate = if (learn_lambda) effective_learning_rate_summary$mean else constants$omega,
      effective_learning_rate_summary = effective_learning_rate_summary,
      learning_rate_mode = learning_rate_mode,
      learned_inverse_loss_scale = learn_lambda,
      lambda_prior = lambda_prior,
      lambda_power = if (learn_lambda) lambda_prior$power * n else 0,
      lambda_power_per_observation = if (learn_lambda) lambda_prior$power else 0,
      lambda_summary = lambda_summary,
      inferential_target = .rqr_target_formula(
        learning_rate_mode, mean_tilt_info$mode
      ),
      sigma = if (learn_lambda) effective_learning_rate_summary$implied_sigma_mean else constants$sigma,
      inference = "mcmc",
      generalized_bayes = TRUE,
      response_likelihood = FALSE,
      root_estimand = "unordered_root_pair",
      raw_root_labels_identified = FALSE,
      root_priors_exchangeable = TRUE,
      root_swap_enabled = root_label_control$swap_probability > 0,
      root_swap_probability = root_label_control$swap_probability,
      kernel_repetitions = kernel_repetitions,
      replica_exchange = replica_exchange,
      replica_exchange_enabled = isTRUE(replica_exchange$enabled),
      retained_replica_inverse_temperature = 1,
      replica_exchange_target_contract = if (
        isTRUE(replica_exchange$enabled)
      ) {
        paste(
          "Each replica targets the common ridge prior times exp(-t * omega_R * L);",
          "alternating adjacent beta-state swaps use the exact marginal loss-energy ratio;",
          "only the t=1 cold replica is retained."
        )
      } else {
        "not_enabled"
      },
      coefficient_label_contract = root_label_control$contract,
      canonicalization_status =
        root_label_diagnostics$status %||% "not_requested",
      canonical_coefficient_inference_available =
        identical(root_label_diagnostics$status %||% "not_requested", "ok"),
      canonicalization_reference_source =
        root_label_diagnostics$reference$method %||% NA_character_,
      canonicalization_audit_domain = if (!is.null(root_label_diagnostics)) {
        root_label_diagnostics$audit_domain
      } else {
        list(n = nrow(root_label_control$audit_X), p = ncol(root_label_control$audit_X))
      },
      target_contract = "fixed_joint_exact",
      exact_joint_target = TRUE,
      numerical_policy = numerical_policy,
      numerical_repair_count = numerical_repair_count,
      numerically_exact_transition = numerically_exact,
      target_numerical_eligible = target_numerical_eligible,
      reproducibility_eligible = provenance$reproducibility_eligible,
      promotion_eligible = target_numerical_eligible && provenance$reproducibility_eligible
    ),
    y = y,
    X = X,
    samp.beta_root1 = beta1_draws,
    samp.beta_root2 = beta2_draws,
    samp.beta_lower_canonical = canonical_beta_lower,
    samp.beta_upper_canonical = canonical_beta_upper,
    samp.lambda = lambda_draws,
    samp.latent_v = latent_v_draws,
    summary = summary,
    diagnostics = list(
      loss_trace = loss_trace,
      ordinary_product_check_loss_trace = loss_trace,
      mean_tilted_target_loss_trace = mean_tilted_target_loss_trace,
      tilt_linear_trace = tilt_linear_trace,
      scaled_loss_trace = loss_trace / loss_reference_scale,
      scaled_target_loss_trace =
        mean_tilted_target_loss_trace / loss_reference_scale,
      weighted_loss_trace = lambda_trace * loss_trace / loss_reference_scale,
      weighted_target_loss_trace =
        lambda_trace * mean_tilted_target_loss_trace / loss_reference_scale,
      lambda_trace = lambda_trace,
      effective_learning_rate_trace = effective_learning_rate_trace,
      lambda_post_shape_trace = lambda_post_shape_trace,
      lambda_post_rate_trace = lambda_post_rate_trace,
      precision_strategy_root1 = precision_strategy_root1,
      precision_strategy_root2 = precision_strategy_root2,
      precision_beta = precision_beta_cfg,
      numerical_repairs = repair_records %||% data.frame(),
      root_swap_trace = root_swap_trace,
      root_swap_count_trace = root_swap_count_trace,
      replica_energy_trace = replica_energy_trace,
      replica_cold_label_trace = replica_cold_label_trace,
      replica_swap_attempts = replica_swap_attempts,
      replica_swap_accepts = replica_swap_accepts,
      replica_swap_acceptance = if (length(replica_swap_attempts)) {
        replica_swap_accepts / pmax(replica_swap_attempts, 1L)
      } else {
        numeric(0L)
      },
      replica_round_trips = replica_round_trips,
      root_label_diagnostics = root_label_diagnostics,
      rhs_stats_root1 = rhs_stats1,
      rhs_stats_root2 = rhs_stats2
    ),
    beta_prior = list(type = beta_prior_type, hypers = beta_prior_obj$hypers),
    last = list(
      beta_root1 = beta1,
      beta_root2 = beta2,
      lambda = lambda_current,
      effective_learning_rate = constants$omega,
      beta_prior_state1 = state1,
      beta_prior_state2 = state2,
      latent_v = V,
      mean_tilt_digest = mean_tilt_info$digest,
      rng_state = rng_state
    ),
    provenance = provenance,
    checkpoint_state = list(
      schema_version = provenance$schema_version,
      completed_iterations = total_iter,
      beta_root1 = beta1,
      beta_root2 = beta2,
      lambda = lambda_current,
      latent_v = V,
      beta_prior_state1 = state1,
      beta_prior_state2 = state2,
      mean_tilt_digest = mean_tilt_info$digest,
      rng_state = rng_state
    ),
    misc = list(
      n_burn = n_burn,
      n_mcmc = n_keep,
      thin = thin,
      kernel_repetitions = kernel_repetitions,
      replica_exchange = replica_exchange,
      replica_initial_beta_root1 = if (
        isTRUE(replica_exchange$enabled)
      ) replica_beta1_initial else NULL,
      replica_initial_beta_root2 = if (
        isTRUE(replica_exchange$enabled)
      ) replica_beta2_initial else NULL,
      continuation_supported = !isTRUE(replica_exchange$enabled),
      seed = seed,
      constants = constants,
      mean_tilt = mean_tilt_info,
      root_label_control = root_label_control$contract,
      column_names = colnames(X),
      note = "MTI is a generalized-Bayes interval readout, not a response likelihood.",
      legacy_name = "RQR"
    )
  )
  class(out) <- c("mti_mcmc", "rqr_mcmc", "mti_fit", "rqr_fit")
  out
}

#' @export
rqr_posterior_draws.rqr_mcmc <- function(
  object, nd = NULL, seed = NULL,
  root_representation = c("raw", "canonical"), ...
) {
  if (!inherits(object, "rqr_mcmc")) stop("Expected an rqr_mcmc object.", call. = FALSE)
  root_representation <- match.arg(root_representation)
  if (!is.null(seed)) set.seed(.rqr_scalar_integer(seed, "seed", 0L))
  if (identical(root_representation, "canonical")) {
    if (!isTRUE(object$model_spec$canonical_coefficient_inference_available) ||
      is.null(object$samp.beta_lower_canonical) ||
      is.null(object$samp.beta_upper_canonical)) {
      stop(
        "Canonical coefficient draws are unavailable: the global root-label audit did not pass for this fit.",
        call. = FALSE
      )
    }
    b1 <- as.matrix(object$samp.beta_lower_canonical)
    b2 <- as.matrix(object$samp.beta_upper_canonical)
  } else {
    b1 <- as.matrix(object$samp.beta_root1)
    b2 <- as.matrix(object$samp.beta_root2)
  }
  n_save <- nrow(b1)
  lambda_all <- as.numeric(object$samp.lambda %||% rep(object$model_spec$learning_rate %||% NA_real_, n_save))
  if (is.null(nd)) {
    idx <- seq_len(n_save)
  } else {
    nd <- .rqr_scalar_integer(nd, "nd", 1L)
    idx <- sample.int(n_save, size = nd, replace = nd > n_save)
  }
  list(
    beta_root1 = b1[idx, , drop = FALSE],
    beta_root2 = b2[idx, , drop = FALSE],
    beta_lower = if (identical(root_representation, "canonical")) b1[idx, , drop = FALSE] else NULL,
    beta_upper = if (identical(root_representation, "canonical")) b2[idx, , drop = FALSE] else NULL,
    lambda = lambda_all[idx],
    nd = length(idx),
    root_representation = root_representation,
    root_label_interpretation = if (identical(root_representation, "canonical")) {
      "Canonical lower/upper coefficient blocks from a passed audit-domain relabeling."
    } else {
      "Raw exchangeable root labels; use pointwise sorting for interval endpoints."
    }
  )
}

#' @export
predict_interval.rqr_mcmc <- function(object, X_new, nd = NULL, draws = NULL, seed = NULL, ...) {
  if (!inherits(object, "rqr_mcmc")) stop("Expected an rqr_mcmc object.", call. = FALSE)
  X_new <- as.matrix(X_new)
  if (ncol(X_new) != ncol(object$X)) {
    stop("X_new must have the same number of columns as the fitted design.", call. = FALSE)
  }
  if (is.null(draws)) draws <- rqr_posterior_draws(object, nd = nd, seed = seed)
  eta1 <- X_new %*% t(draws$beta_root1)
  eta2 <- X_new %*% t(draws$beta_root2)
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  list(
    lower_draws = lower,
    upper_draws = upper,
    midpoint_draws = 0.5 * (lower + upper),
    width_draws = upper - lower,
    lower_mean = rowMeans(lower),
    upper_mean = rowMeans(upper),
    midpoint_mean = rowMeans(0.5 * (lower + upper)),
    width_mean = rowMeans(upper - lower),
    draws = draws,
    model_spec = object$model_spec
  )
}

#' @export
print.rqr_mcmc <- function(x, ...) {
  cat("RQR fixed-design MCMC fit\n")
  cat(sprintf("  coverage_level: %.4f\n", x$model_spec$coverage_level))
  cat(sprintf("  learning_rate:  %.4f\n", x$model_spec$learning_rate))
  cat(sprintf("  rate_mode:      %s\n", x$model_spec$learning_rate_mode %||% "fixed"))
  cat(sprintf("  mean_tilt:      %s\n", x$model_spec$mean_tilt_mode %||% "zero"))
  if (isTRUE(x$model_spec$learned_inverse_loss_scale)) {
    cat(sprintf("  lambda_mean:    %.4f\n", x$model_spec$lambda_summary$mean))
  }
  cat(sprintf("  numerical repairs: %d\n", x$model_spec$numerical_repair_count %||% 0L))
  cat(sprintf("  promotion eligible: %s\n", if (isTRUE(x$model_spec$promotion_eligible)) "yes" else "no"))
  cat(sprintf("  draws:          %d\n", nrow(x$samp.beta_root1)))
  cat("  interpretation: generalized-Bayes interval readout, not response likelihood\n")
  invisible(x)
}
