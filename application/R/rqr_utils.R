# RQR utilities deliberately keep their own fallback for the package's common
# null-coalescing idiom so source load order cannot affect the new backend.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

#' RQR loss constants
#'
#' @param coverage_level Interval coverage level in `(0, 1)`.
#' @param learning_rate Positive generalized-Bayes learning rate.
#' @return A named list with `alpha`, `omega`, `sigma`, `xi`, and `phi`.
#' @export
rqr_constants <- function(coverage_level, learning_rate = 1) {
  alpha <- as.numeric(coverage_level)[1L]
  omega <- as.numeric(learning_rate)[1L]
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("coverage_level must be a finite scalar in (0, 1).", call. = FALSE)
  }
  if (!is.finite(omega) || omega <= 0) {
    stop("learning_rate must be a finite positive scalar.", call. = FALSE)
  }
  list(
    alpha = alpha,
    omega = omega,
    sigma = 1 / omega,
    xi = (1 - 2 * alpha) / (alpha * (1 - alpha)),
    phi = 2 / (alpha * (1 - alpha))
  )
}

#' RQR check loss
#'
#' @param u Numeric residual product.
#' @param coverage_level Interval coverage level.
#' @return Numeric vector of RQR check losses.
#' @export
rqr_check_loss <- function(u, coverage_level) {
  alpha <- rqr_constants(coverage_level)$alpha
  u <- as.numeric(u)
  u * (alpha - as.numeric(u < 0))
}

#' RQR root residual product
#'
#' @param y Response vector.
#' @param eta1,eta2 Numeric root ordinates.
#' @return Numeric vector `(y - eta1) * (y - eta2)`.
#' @export
rqr_residual_product <- function(y, eta1, eta2) {
  y <- as.numeric(y)
  eta1 <- as.numeric(eta1)
  eta2 <- as.numeric(eta2)
  if (length(y) != length(eta1) || length(y) != length(eta2)) {
    stop("y, eta1, and eta2 must have the same length.", call. = FALSE)
  }
  (y - eta1) * (y - eta2)
}

#' RQR pseudo residual from the transformed AL representation
#'
#' @param y Response vector.
#' @param eta1,eta2 Numeric root ordinates.
#' @return Numeric vector `y^2 - y * (eta1 + eta2) + eta1 * eta2`.
#' @export
rqr_pseudo_residual <- function(y, eta1, eta2) {
  y <- as.numeric(y)
  eta1 <- as.numeric(eta1)
  eta2 <- as.numeric(eta2)
  if (length(y) != length(eta1) || length(y) != length(eta2)) {
    stop("y, eta1, and eta2 must have the same length.", call. = FALSE)
  }
  y^2 - y * (eta1 + eta2) + eta1 * eta2
}

#' RQR ordered endpoints
#'
#' @param eta1,eta2 Numeric root ordinates.
#' @return A list with `lower`, `upper`, `midpoint`, and `width`.
#' @export
rqr_order_endpoints <- function(eta1, eta2) {
  eta1 <- as.numeric(eta1)
  eta2 <- as.numeric(eta2)
  if (length(eta1) != length(eta2)) {
    stop("eta1 and eta2 must have the same length.", call. = FALSE)
  }
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  list(
    lower = lower,
    upper = upper,
    midpoint = 0.5 * (lower + upper),
    width = upper - lower
  )
}

#' RQR GIG parameters for latent pseudo-AL scales
#'
#' The returned `a` and `b` match the package GIG convention proportional to
#' `x^(p - 1) exp(-(a * x + b / x) / 2)`.
#'
#' @param e Residual product.
#' @param coverage_level Interval coverage level.
#' @param learning_rate Positive generalized-Bayes learning rate.
#' @return A list with `p`, `a`, and `b`.
#' @export
rqr_gig_params <- function(e, coverage_level, learning_rate = 1) {
  cc <- rqr_constants(coverage_level, learning_rate)
  e <- as.numeric(e)
  list(
    p = 0.5,
    a = 1 / (2 * cc$sigma * cc$alpha * (1 - cc$alpha)),
    b = cc$alpha * (1 - cc$alpha) * e^2 / (2 * cc$sigma)
  )
}

.rqr_learning_rate_mode <- function(learning_rate_mode) {
  mode <- tolower(as.character(learning_rate_mode %||% "fixed_rate")[1L])
  mode <- switch(mode,
    fixed = "fixed_rate",
    learned = "learned_pseudoresidual_normalized",
    scale = "learned_pseudoresidual_normalized",
    learned_scale = "learned_pseudoresidual_normalized",
    learned_loss_scale = "learned_pseudoresidual_normalized",
    pure = "learned_pure",
    mode
  )
  choices <- c("fixed_rate", "learned_pseudoresidual_normalized", "learned_pure")
  if (!mode %in% choices) {
    stop(
      sprintf(
        "learning_rate_mode must be one of {'%s'}.",
        paste(choices, collapse = "','")
      ),
      call. = FALSE
    )
  }
  mode
}

.rqr_lambda_prior <- function(lambda_prior = list(), learning_rate_mode = "fixed_rate") {
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  if (inherits(lambda_prior, "rqr_lambda_prior")) {
    if (!identical(lambda_prior$mode, mode)) {
      stop("Internal lambda prior mode does not match learning_rate_mode.", call. = FALSE)
    }
    return(lambda_prior)
  }
  if (is.null(lambda_prior)) lambda_prior <- list()
  if (!is.list(lambda_prior)) {
    stop("lambda_prior must be a list with positive shape and rate.", call. = FALSE)
  }
  shape <- as.numeric(lambda_prior$shape %||% lambda_prior$a %||% 4)[1L]
  rate <- as.numeric(lambda_prior$rate %||% lambda_prior$b %||% 4)[1L]
  if (!is.null(lambda_prior$power) || !is.null(lambda_prior$nu)) {
    stop(
      "lambda_prior$power and lambda_prior$nu are not accepted: the normalized and pure targets have fixed powers.",
      call. = FALSE
    )
  }
  power <- if (identical(mode, "learned_pseudoresidual_normalized")) 1 else 0
  if (identical(mode, "fixed_rate")) {
    if (!is.finite(shape)) shape <- 4
    if (!is.finite(rate)) rate <- 4
  }
  if (!is.finite(shape) || shape <= 0) stop("lambda_prior$shape must be positive.", call. = FALSE)
  if (!is.finite(rate) || rate <= 0) stop("lambda_prior$rate must be positive.", call. = FALSE)
  structure(
    list(shape = shape, rate = rate, power = power, mode = mode),
    class = c("rqr_lambda_prior", "list")
  )
}

.rqr_lambda_posterior_params <- function(loss_sum, n, lambda_prior, learning_rate_mode) {
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  prior <- .rqr_lambda_prior(lambda_prior, mode)
  loss_sum <- as.numeric(loss_sum)[1L]
  n <- as.integer(n)[1L]
  if (!is.finite(loss_sum) || loss_sum < 0) stop("loss_sum must be finite and nonnegative.", call. = FALSE)
  if (!is.finite(n) || n <= 0L) stop("n must be a positive integer.", call. = FALSE)
  if (identical(mode, "fixed_rate")) {
    return(list(shape = NA_real_, rate = NA_real_, power_count = 0))
  }
  power_count <- prior$power * n
  list(
    shape = prior$shape + power_count,
    rate = prior$rate + loss_sum,
    power_count = power_count
  )
}

.rqr_target_formula <- function(learning_rate_mode) {
  mode <- .rqr_learning_rate_mode(learning_rate_mode)
  switch(mode,
    fixed_rate = "pi(theta|y) proportional to pi(theta) exp{-omega_R L(theta)}",
    learned_pseudoresidual_normalized = paste0(
      "pi(theta,lambda|y) proportional to pi(theta) pi(lambda) ",
      "lambda^n_obs exp{-lambda L(theta)/s_L}"
    ),
    learned_pure = paste0(
      "pi(theta,lambda|y) proportional to pi(theta) pi(lambda) ",
      "exp{-lambda L(theta)/s_L}"
    )
  )
}

.rqr_scalar_integer <- function(x, name, minimum = 0L, maximum = .Machine$integer.max) {
  if (length(x) != 1L || is.na(x) || !is.finite(x) || x != floor(x) ||
      x < minimum || x > maximum) {
    stop(
      sprintf("%s must be one finite integer in [%d, %d].", name, minimum, maximum),
      call. = FALSE
    )
  }
  as.integer(x)
}

.rqr_positive_integer_vector <- function(x, name) {
  if (!is.numeric(x) || !length(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x != floor(x)) || any(x < 1) || any(x > .Machine$integer.max)) {
    stop(sprintf("%s must contain positive integers.", name), call. = FALSE)
  }
  as.integer(x)
}

.rqr_rng_state <- function() {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) return(NULL)
  get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
}

.rqr_restore_rng <- function(state) {
  if (is.null(state)) return(invisible(FALSE))
  state <- as.integer(state)
  if (length(state) < 2L || anyNA(state)) {
    stop("init$rng_state must be a complete integer .Random.seed vector.", call. = FALSE)
  }
  assign(".Random.seed", state, envir = .GlobalEnv)
  invisible(TRUE)
}

.rqr_digest <- function(object) {
  digest::digest(object, algo = "sha256", serialize = TRUE)
}

.rqr_schema_version <- function() "rqrgibbs_fit/1.6.0"

.rqr_continuation_history_schema <- function() {
  "rqrgibbs_continuation_history/3.0.0"
}

.rqr_make_continuation_history <- function(
    checkpoint_digest, segment_numerical_repair_count,
    segment_exact_joint_target, segment_environment_base_eligible,
    segment_target_contract_digest,
    backend_requested, backend_resolved, parent = NULL,
    parent_checkpoint_digest = NULL,
    environment_mismatches = character(0),
    environment_override_used = FALSE) {
  checkpoint_digest <- tolower(as.character(checkpoint_digest)[1L])
  if (!grepl("^[0-9a-f]{64}$", checkpoint_digest)) {
    stop("checkpoint_digest must be a complete SHA-256 digest.", call. = FALSE)
  }
  segment_numerical_repair_count <- as.integer(
    segment_numerical_repair_count
  )
  if (length(segment_numerical_repair_count) != 1L ||
      is.na(segment_numerical_repair_count) ||
      segment_numerical_repair_count < 0L) {
    stop(
      "segment_numerical_repair_count must be one nonnegative integer.",
      call. = FALSE
    )
  }
  environment_mismatches <- sort(unique(as.character(
    environment_mismatches
  )))
  if (anyNA(environment_mismatches) ||
      any(!nzchar(environment_mismatches))) {
    stop("environment_mismatches must contain nonempty text.", call. = FALSE)
  }
  if (is.null(parent)) {
    generation <- 0L
    segments <- list()
    expected_parent_checkpoint <- NA_character_
    parent_cumulative_repairs <- 0L
    parent_chain_exact <- TRUE
    parent_target_eligible <- TRUE
    parent_reproducibility_eligible <- TRUE
    parent_override_used <- FALSE
    parent_backend_requested <- NA_character_
    parent_backend_resolved <- NA_character_
  } else {
    if (!is.list(parent) ||
        !identical(
          parent$schema_version %||% NA_character_,
          .rqr_continuation_history_schema()
        )) {
      stop("parent must be a validated continuation-history contract.", call. = FALSE)
    }
    generation <- as.integer(parent$generation) + 1L
    segments <- parent$segments
    expected_parent_checkpoint <- utils::tail(segments, 1L)[[1L]]$
      checkpoint_digest
    supplied_parent_checkpoint <- tolower(as.character(
      parent_checkpoint_digest %||% NA_character_
    )[1L])
    if (!identical(supplied_parent_checkpoint, expected_parent_checkpoint)) {
      stop(
        "parent_checkpoint_digest does not link to the parent history.",
        call. = FALSE
      )
    }
    parent_cumulative_repairs <- as.integer(
      parent$cumulative_numerical_repair_count
    )
    parent_chain_exact <- isTRUE(
      parent$chain_history_numerically_exact
    )
    parent_target_eligible <- isTRUE(parent$target_numerical_eligible)
    parent_reproducibility_eligible <- isTRUE(
      parent$reproducibility_eligible
    )
    parent_override_used <- isTRUE(
      parent$cumulative_environment_override_used
    )
    parent_segment <- utils::tail(segments, 1L)[[1L]]
    parent_backend_requested <- parent_segment$backend_requested
    parent_backend_resolved <- parent_segment$backend_resolved
  }
  valid_logical_scalar <- function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }
  if (!valid_logical_scalar(segment_exact_joint_target)) {
    stop("segment_exact_joint_target must be TRUE or FALSE.", call. = FALSE)
  }
  if (!valid_logical_scalar(segment_environment_base_eligible)) {
    stop(
      "segment_environment_base_eligible must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  if (!valid_logical_scalar(environment_override_used)) {
    stop("environment_override_used must be TRUE or FALSE.", call. = FALSE)
  }
  segment_target_contract_digest <- tolower(as.character(
    segment_target_contract_digest
  )[1L])
  if (!grepl("^[0-9a-f]{64}$", segment_target_contract_digest)) {
    stop(
      "segment_target_contract_digest must be a complete SHA-256 digest.",
      call. = FALSE
    )
  }
  if (!is.null(parent)) {
    parent_target_digest <- utils::tail(segments, 1L)[[1L]]$
      segment_target_contract_digest
    if (!identical(segment_target_contract_digest, parent_target_digest)) {
      stop(
        "A continuation cannot change its target-contract digest.",
        call. = FALSE
      )
    }
  }
  backend_requested <- as.character(backend_requested)[1L]
  backend_resolved <- as.character(backend_resolved)[1L]
  if (!.rqr_nonmissing_text(c(backend_requested, backend_resolved))) {
    stop("Backend values must contain nonempty text.", call. = FALSE)
  }
  environment_override_used <- isTRUE(environment_override_used)
  backend_changed <- !is.null(parent) && (
    !identical(backend_requested, parent_backend_requested) ||
      !identical(backend_resolved, parent_backend_resolved)
  )
  backend_mismatch_recorded <- any(c(
    "backend_requested", "backend_resolved"
  ) %in% environment_mismatches)
  if (length(environment_mismatches) > 0L &&
      !environment_override_used) {
    stop(
      "Environment mismatches require an explicit continuation override.",
      call. = FALSE
    )
  }
  if (environment_override_used && !length(environment_mismatches)) {
    stop(
      "An environment override requires at least one recorded mismatch.",
      call. = FALSE
    )
  }
  if (backend_changed &&
      (!environment_override_used || !backend_mismatch_recorded)) {
    stop(
      paste(
        "A continuation backend change requires an explicit override and",
        "a recorded backend mismatch."
      ),
      call. = FALSE
    )
  }
  segment_numerically_exact <- segment_numerical_repair_count == 0L
  segment_target_numerical_eligible <-
    isTRUE(segment_exact_joint_target) && segment_numerically_exact
  segment_environment_reproducibility_eligible <-
    isTRUE(segment_environment_base_eligible) &&
    !length(environment_mismatches) &&
    !environment_override_used
  cumulative_repairs <- parent_cumulative_repairs +
    segment_numerical_repair_count
  chain_exact <- parent_chain_exact && segment_numerically_exact
  target_eligible <- parent_target_eligible &&
    segment_target_numerical_eligible
  reproducibility_eligible <- parent_reproducibility_eligible &&
    segment_environment_reproducibility_eligible
  promotion_eligible <- target_eligible && reproducibility_eligible
  cumulative_override <- parent_override_used ||
    environment_override_used
  segment <- list(
    generation = generation,
    parent_checkpoint_digest = expected_parent_checkpoint,
    checkpoint_digest = checkpoint_digest,
    segment_numerical_repair_count = segment_numerical_repair_count,
    cumulative_numerical_repair_count = cumulative_repairs,
    segment_target_contract_digest = segment_target_contract_digest,
    segment_exact_joint_target = isTRUE(segment_exact_joint_target),
    segment_numerically_exact = segment_numerically_exact,
    chain_history_numerically_exact = chain_exact,
    segment_target_numerical_eligible =
      segment_target_numerical_eligible,
    target_numerical_eligible = target_eligible,
    segment_environment_reproducibility_eligible =
      segment_environment_reproducibility_eligible,
    segment_environment_base_eligible =
      isTRUE(segment_environment_base_eligible),
    reproducibility_eligible = reproducibility_eligible,
    promotion_eligible = promotion_eligible,
    environment_mismatches = environment_mismatches,
    environment_override_used = environment_override_used,
    cumulative_environment_override_used = cumulative_override,
    parent_backend_requested = parent_backend_requested,
    parent_backend_resolved = parent_backend_resolved,
    backend_requested = backend_requested,
    backend_resolved = backend_resolved,
    backend_changed = backend_changed
  )
  all_segments <- c(segments, list(segment))
  mismatch_ledger <- lapply(
    Filter(
      function(item) {
        length(item$environment_mismatches) > 0L ||
          isTRUE(item$environment_override_used)
      },
      all_segments
    ),
    function(item) {
      list(
        generation = item$generation,
        checkpoint_digest = item$checkpoint_digest,
        environment_mismatches = item$environment_mismatches,
        environment_override_used = item$environment_override_used
      )
    }
  )
  list(
    schema_version = .rqr_continuation_history_schema(),
    generation = generation,
    cumulative_numerical_repair_count = cumulative_repairs,
    chain_history_numerically_exact = chain_exact,
    target_numerical_eligible = target_eligible,
    promotion_eligible = promotion_eligible,
    reproducibility_eligible = reproducibility_eligible,
    cumulative_environment_override_used = cumulative_override,
    cumulative_environment_mismatch_ledger = mismatch_ledger,
    segments = all_segments
  )
}

.rqr_validate_continuation_history <- function(object) {
  contract <- object$continuation_history_contract
  stored_digest <- object$continuation_history_digest %||% NA_character_
  if (!is.list(contract) ||
      !identical(
        contract$schema_version %||% NA_character_,
        .rqr_continuation_history_schema()
      ) ||
      !.rqr_nonmissing_text(stored_digest) ||
      !identical(.rqr_digest(contract), stored_digest)) {
    stop(
      "Continuation history contract or digest does not match the fitted object.",
      call. = FALSE
    )
  }
  segments <- contract$segments
  required_segment_fields <- c(
    "generation", "parent_checkpoint_digest", "checkpoint_digest",
    "segment_numerical_repair_count",
    "cumulative_numerical_repair_count",
    "segment_target_contract_digest",
    "segment_exact_joint_target",
    "segment_numerically_exact",
    "chain_history_numerically_exact",
    "segment_target_numerical_eligible",
    "target_numerical_eligible",
    "segment_environment_reproducibility_eligible",
    "segment_environment_base_eligible",
    "reproducibility_eligible", "promotion_eligible",
    "environment_mismatches", "environment_override_used",
    "cumulative_environment_override_used",
    "parent_backend_requested", "parent_backend_resolved",
    "backend_requested", "backend_resolved", "backend_changed"
  )
  if (!is.list(segments) || !length(segments) ||
      any(!vapply(
        segments,
        function(segment) {
          is.list(segment) &&
            all(required_segment_fields %in% names(segment))
        },
        logical(1L)
      ))) {
    stop("Continuation history segments are incomplete.", call. = FALSE)
  }
  cumulative_repairs <- 0L
  chain_exact <- TRUE
  target_eligible <- TRUE
  reproducibility_eligible <- TRUE
  promotion_eligible <- TRUE
  cumulative_override <- FALSE
  prior_checkpoint <- NA_character_
  reconstructed_ledger <- list()
  for (index in seq_along(segments)) {
    segment <- segments[[index]]
    expected_generation <- as.integer(index - 1L)
    mismatches <- segment$environment_mismatches
    valid_mismatches <- is.character(mismatches) &&
      !anyNA(mismatches) && all(nzchar(mismatches)) &&
      identical(mismatches, sort(unique(mismatches)))
    valid_checkpoint <- is.character(segment$checkpoint_digest) &&
      length(segment$checkpoint_digest) == 1L &&
      grepl("^[0-9a-f]{64}$", segment$checkpoint_digest)
    valid_target_digest <-
      is.character(segment$segment_target_contract_digest) &&
      length(segment$segment_target_contract_digest) == 1L &&
      grepl(
        "^[0-9a-f]{64}$", segment$segment_target_contract_digest
      )
    valid_parent_link <- if (index == 1L) {
      is.character(segment$parent_checkpoint_digest) &&
        length(segment$parent_checkpoint_digest) == 1L &&
        is.na(segment$parent_checkpoint_digest)
    } else {
      identical(segment$parent_checkpoint_digest, prior_checkpoint)
    }
    segment_repairs <- as.integer(segment$segment_numerical_repair_count)
    valid_repairs <- length(segment_repairs) == 1L &&
      !is.na(segment_repairs) && segment_repairs >= 0L
    valid_logical_scalar <- function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value)
    }
    logical_fields <- c(
      "segment_exact_joint_target", "segment_numerically_exact",
      "chain_history_numerically_exact",
      "segment_target_numerical_eligible",
      "target_numerical_eligible",
      "segment_environment_base_eligible",
      "segment_environment_reproducibility_eligible",
      "reproducibility_eligible", "promotion_eligible",
      "environment_override_used",
      "cumulative_environment_override_used", "backend_changed"
    )
    valid_logicals <- all(vapply(
      logical_fields,
      function(field) valid_logical_scalar(segment[[field]]),
      logical(1L)
    ))
    valid_parent_backends <- if (index == 1L) {
      is.character(segment$parent_backend_requested) &&
        length(segment$parent_backend_requested) == 1L &&
        is.na(segment$parent_backend_requested) &&
        is.character(segment$parent_backend_resolved) &&
        length(segment$parent_backend_resolved) == 1L &&
        is.na(segment$parent_backend_resolved)
    } else {
      identical(
        segment$parent_backend_requested,
        segments[[index - 1L]]$backend_requested
      ) &&
        identical(
          segment$parent_backend_resolved,
          segments[[index - 1L]]$backend_resolved
        )
    }
    if (!identical(as.integer(segment$generation), expected_generation) ||
        !valid_checkpoint || !valid_target_digest ||
        !valid_parent_link || !valid_mismatches ||
        !valid_repairs || !valid_logicals || !valid_parent_backends ||
        !.rqr_nonmissing_text(c(
          segment$backend_requested, segment$backend_resolved
        ))) {
      stop(
        sprintf(
          "Continuation history segment %d is structurally invalid.",
          expected_generation
        ),
        call. = FALSE
      )
    }
    backend_changed <- index > 1L && (
      !identical(
        segment$backend_requested,
        segments[[index - 1L]]$backend_requested
      ) ||
        !identical(
          segment$backend_resolved,
          segments[[index - 1L]]$backend_resolved
        )
    )
    backend_mismatch_recorded <- any(c(
      "backend_requested", "backend_resolved"
    ) %in% mismatches)
    environment_override <- isTRUE(segment$environment_override_used)
    semantic_checks <- c(
      identical(
        isTRUE(segment$segment_numerically_exact),
        segment_repairs == 0L
      ),
      identical(
        isTRUE(segment$segment_target_numerical_eligible),
        isTRUE(segment$segment_exact_joint_target) &&
          segment_repairs == 0L
      ),
      identical(
        isTRUE(segment$segment_environment_reproducibility_eligible),
        isTRUE(segment$segment_environment_base_eligible) &&
          !length(mismatches) && !environment_override
      ),
      identical(isTRUE(segment$backend_changed), backend_changed),
      identical(
        segment$segment_target_contract_digest,
        segments[[1L]]$segment_target_contract_digest
      ),
      !length(mismatches) || environment_override,
      !environment_override || length(mismatches) > 0L,
      !backend_changed ||
        (environment_override && backend_mismatch_recorded)
    )
    if (!all(semantic_checks)) {
      stop(
        sprintf(
          "Continuation history segment %d violates derived-status semantics.",
          expected_generation
        ),
        call. = FALSE
      )
    }
    cumulative_repairs <- cumulative_repairs + segment_repairs
    chain_exact <- chain_exact && segment_repairs == 0L
    target_eligible <- target_eligible &&
      isTRUE(segment$segment_exact_joint_target) &&
      segment_repairs == 0L
    reproducibility_eligible <- reproducibility_eligible &&
      isTRUE(segment$segment_environment_base_eligible) &&
      !length(mismatches) && !environment_override
    promotion_eligible <- target_eligible && reproducibility_eligible
    cumulative_override <- cumulative_override ||
      environment_override
    recursive_checks <- c(
      identical(
        as.integer(segment$cumulative_numerical_repair_count),
        cumulative_repairs
      ),
      identical(
        isTRUE(segment$chain_history_numerically_exact), chain_exact
      ),
      identical(
        isTRUE(segment$target_numerical_eligible), target_eligible
      ),
      identical(
        isTRUE(segment$reproducibility_eligible),
        reproducibility_eligible
      ),
      identical(
        isTRUE(segment$promotion_eligible), promotion_eligible
      ),
      identical(
        isTRUE(segment$cumulative_environment_override_used),
        cumulative_override
      )
    )
    if (!all(recursive_checks)) {
      stop(
        sprintf(
          "Continuation history segment %d violates cumulative recursion.",
          expected_generation
        ),
        call. = FALSE
      )
    }
    if (length(mismatches) || isTRUE(segment$environment_override_used)) {
      reconstructed_ledger[[length(reconstructed_ledger) + 1L]] <- list(
        generation = segment$generation,
        checkpoint_digest = segment$checkpoint_digest,
        environment_mismatches = mismatches,
        environment_override_used = segment$environment_override_used
      )
    }
    prior_checkpoint <- segment$checkpoint_digest
  }
  if (!identical(
        contract$cumulative_environment_mismatch_ledger,
        reconstructed_ledger
      )) {
    stop(
      "Continuation history mismatch ledger is not reconstructible.",
      call. = FALSE
    )
  }
  last_segment <- utils::tail(segments, 1L)[[1L]]
  checks <- list(
    generation = identical(
      as.integer(contract$generation),
      as.integer(length(segments) - 1L)
    ),
    cumulative_numerical_repair_count = identical(
      as.integer(contract$cumulative_numerical_repair_count),
      cumulative_repairs
    ),
    chain_history_numerically_exact = identical(
      isTRUE(contract$chain_history_numerically_exact),
      chain_exact
    ),
    target_numerical_eligible = identical(
      isTRUE(contract$target_numerical_eligible),
      target_eligible
    ),
    promotion_eligible = identical(
      isTRUE(contract$promotion_eligible),
      promotion_eligible
    ),
    reproducibility_eligible = identical(
      isTRUE(contract$reproducibility_eligible),
      reproducibility_eligible
    ),
    cumulative_environment_override_used = identical(
      isTRUE(contract$cumulative_environment_override_used),
      cumulative_override
    ),
    redundant_cumulative_repairs = identical(
      cumulative_repairs,
      as.integer(object$model_spec$cumulative_numerical_repair_count)
    ),
    redundant_chain_exact = identical(
      chain_exact,
      isTRUE(object$model_spec$chain_history_numerically_exact)
    ),
    redundant_target_eligible = identical(
      target_eligible,
      isTRUE(object$model_spec$target_numerical_eligible)
    ),
    redundant_promotion = identical(
      promotion_eligible,
      isTRUE(object$model_spec$promotion_eligible)
    ),
    redundant_reproducibility = identical(
      reproducibility_eligible,
      isTRUE(object$provenance$reproducibility_eligible)
    ),
    checkpoint_digest = identical(
      last_segment$checkpoint_digest,
      object$checkpoint_digest
    ),
    backend_requested = identical(
      last_segment$backend_requested,
      object$provenance$backend_requested
    ),
    backend_resolved = identical(
      last_segment$backend_resolved,
      object$provenance$backend_resolved
    ),
    exact_joint_target = identical(
      isTRUE(last_segment$segment_exact_joint_target),
      isTRUE(object$model_spec$exact_joint_target)
    ),
    all_exact_joint_target = all(vapply(
      segments,
      function(segment) {
        identical(
          isTRUE(segment$segment_exact_joint_target),
          isTRUE(object$model_spec$exact_joint_target)
        )
      },
      logical(1L)
    )),
    target_contract_digest = all(vapply(
      segments,
      function(segment) {
        identical(
          segment$segment_target_contract_digest,
          object$provenance$object_digests$target
        )
      },
      logical(1L)
    ))
  )
  if (!all(unlist(checks, use.names = FALSE))) {
    stop(
      "Continuation history contract conflicts with redundant fit metadata.",
      call. = FALSE
    )
  }
  contract
}

.rqr_pinned_exdqlm_commit <- function() {
  "dffb71ee70b597d6a716ee74be1cbc99731cd453"
}

.rqr_find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, ".git")) &&
        file.exists(file.path(current, "application", "DESCRIPTION"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) return(NA_character_)
    current <- parent
  }
}

.rqr_git_result <- function(repo_root, args) {
  git <- Sys.which("git")
  if (length(repo_root) != 1L || is.na(repo_root) || !nzchar(repo_root) || !nzchar(git)) {
    return(list(available = FALSE, value = NA_character_))
  }
  out <- suppressWarnings(tryCatch(
    system2(
      git,
      c("-C", shQuote(repo_root), args),
      stdout = TRUE,
      stderr = TRUE,
      env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
    ),
    error = function(e) structure(character(0), status = 1L)
  ))
  status <- attr(out, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    return(list(available = FALSE, value = NA_character_))
  }
  list(available = TRUE, value = paste(out, collapse = "\n"))
}

.rqr_git_value <- function(repo_root, args) {
  .rqr_git_result(repo_root, args)$value
}

.rqr_normalize_repository_spec <- function(spec, name) {
  if (is.null(spec)) spec <- list()
  if (!is.list(spec)) {
    stop(
      sprintf("provenance_control$external_repositories$%s must be a list.", name),
      call. = FALSE
    )
  }
  repo_root <- spec$repo_root %||% NULL
  if (!is.null(repo_root)) {
    if (length(repo_root) != 1L || is.na(repo_root) ||
        !nzchar(as.character(repo_root))) {
      stop(
        sprintf(
          "provenance_control$external_repositories$%s$repo_root must be one nonempty path.",
          name
        ),
        call. = FALSE
      )
    }
    repo_root <- normalizePath(as.character(repo_root), winslash = "/", mustWork = FALSE)
  }
  expected <- spec$expected_git_commit %||% NULL
  if (!is.null(expected)) {
    expected <- tolower(as.character(expected))
    if (length(expected) != 1L || is.na(expected) ||
        !grepl("^[0-9a-f]{40}$", expected)) {
      stop(
        sprintf(
          paste0(
            "provenance_control$external_repositories$%s$expected_git_commit ",
            "must be one complete 40-character Git SHA."
          ),
          name
        ),
        call. = FALSE
      )
    }
  }
  runtime_package <- spec$runtime_package %||% NULL
  if (!is.null(runtime_package)) {
    runtime_package <- as.character(runtime_package)
    if (length(runtime_package) != 1L || is.na(runtime_package) ||
        !nzchar(runtime_package)) {
      stop(
        sprintf(
          "provenance_control$external_repositories$%s$runtime_package must be one nonempty package name.",
          name
        ),
        call. = FALSE
      )
    }
  }
  runtime_attestation <- spec$runtime_attestation %||% NULL
  if (!is.null(runtime_attestation)) {
    runtime_attestation <- as.character(runtime_attestation)
    if (length(runtime_attestation) != 1L || is.na(runtime_attestation) ||
        !nzchar(runtime_attestation)) {
      stop(
        sprintf(
          "provenance_control$external_repositories$%s$runtime_attestation must be one nonempty path.",
          name
        ),
        call. = FALSE
      )
    }
    runtime_attestation <- normalizePath(
      runtime_attestation, winslash = "/", mustWork = FALSE
    )
  }
  require_isolated_runtime <- spec$require_isolated_runtime %||% FALSE
  if (length(require_isolated_runtime) != 1L ||
      is.na(require_isolated_runtime) ||
      !is.logical(require_isolated_runtime)) {
    stop(
      sprintf(
        paste0(
          "provenance_control$external_repositories$%s$",
          "require_isolated_runtime must be TRUE or FALSE."
        ),
        name
      ),
      call. = FALSE
    )
  }
  source_subdir <- as.character(spec$source_subdir %||% ".")[1L]
  if (is.na(source_subdir) || !nzchar(source_subdir) ||
      grepl("(^|/)\\.\\.(/|$)", source_subdir) ||
      startsWith(source_subdir, "/")) {
    stop(
      sprintf(
        "provenance_control$external_repositories$%s$source_subdir must be a safe relative path.",
        name
      ),
      call. = FALSE
    )
  }
  list(
    repo_root = repo_root,
    expected_git_commit = expected,
    runtime_package = runtime_package,
    runtime_attestation = runtime_attestation,
    require_isolated_runtime = require_isolated_runtime,
    source_subdir = source_subdir
  )
}

.rqr_provenance_control <- function(control = list()) {
  if (is.null(control)) control <- list()
  if (!is.list(control)) stop("provenance_control must be a list.", call. = FALSE)
  repo_root <- control$repo_root %||% NULL
  if (!is.null(repo_root)) {
    if (length(repo_root) != 1L || is.na(repo_root) || !nzchar(as.character(repo_root))) {
      stop("provenance_control$repo_root must be one nonempty path.", call. = FALSE)
    }
    repo_root <- normalizePath(as.character(repo_root), winslash = "/", mustWork = FALSE)
  }
  expected <- control$expected_git_commit %||% NULL
  if (!is.null(expected)) {
    expected <- tolower(as.character(expected))
    if (length(expected) != 1L || is.na(expected) ||
        !grepl("^[0-9a-f]{40}$", expected)) {
      stop(
        "provenance_control$expected_git_commit must be one complete 40-character Git SHA.",
        call. = FALSE
      )
    }
  }
  external <- control$external_repositories %||% list()
  if (!is.list(external)) {
    stop("provenance_control$external_repositories must be a named list.", call. = FALSE)
  }
  if (length(external)) {
    external_names <- names(external)
    if (is.null(external_names) || anyNA(external_names) ||
        any(!nzchar(external_names)) || anyDuplicated(external_names)) {
      stop(
        "provenance_control$external_repositories must have unique nonempty names.",
        call. = FALSE
      )
    }
    external <- lapply(external_names, function(name) {
      .rqr_normalize_repository_spec(external[[name]], name)
    })
    names(external) <- external_names
  }
  required_external <- as.character(
    control$required_external_repositories %||% character(0)
  )
  if (anyNA(required_external) || any(!nzchar(required_external)) ||
      anyDuplicated(required_external)) {
    stop(
      "provenance_control$required_external_repositories must contain unique nonempty names.",
      call. = FALSE
    )
  }
  primary_runtime_attestation <- control$primary_runtime_attestation %||% NULL
  if (!is.null(primary_runtime_attestation)) {
    primary_runtime_attestation <- as.character(primary_runtime_attestation)[1L]
    if (is.na(primary_runtime_attestation) ||
        !nzchar(primary_runtime_attestation)) {
      stop(
        "provenance_control$primary_runtime_attestation must be one nonempty path.",
        call. = FALSE
      )
    }
    primary_runtime_attestation <- normalizePath(
      primary_runtime_attestation, winslash = "/", mustWork = FALSE
    )
  }
  list(
    repo_root = repo_root,
    expected_git_commit = expected,
    primary_runtime_attestation = primary_runtime_attestation,
    external_repositories = external,
    required_external_repositories = required_external
  )
}

.rqr_require_external_repository <- function(
    control, name, expected_git_commit, runtime_package = NULL) {
  control <- .rqr_provenance_control(control)
  name <- as.character(name)[1L]
  if (is.na(name) || !nzchar(name)) {
    stop("Required external repository name must be nonempty.", call. = FALSE)
  }
  expected_git_commit <- tolower(as.character(expected_git_commit)[1L])
  if (is.na(expected_git_commit) ||
      !grepl("^[0-9a-f]{40}$", expected_git_commit)) {
    stop("Required external repository commit must be a complete Git SHA.", call. = FALSE)
  }
  spec <- control$external_repositories[[name]] %||% list(
    repo_root = NULL, expected_git_commit = NULL,
    runtime_package = NULL, runtime_attestation = NULL,
    require_isolated_runtime = FALSE
  )
  if (!is.null(spec$expected_git_commit) &&
      !identical(spec$expected_git_commit, expected_git_commit)) {
    stop(
      sprintf(
        "External repository '%s' must use the pinned commit %s.",
        name, expected_git_commit
      ),
      call. = FALSE
    )
  }
  spec$expected_git_commit <- expected_git_commit
  if (!is.null(runtime_package)) {
    runtime_package <- as.character(runtime_package)[1L]
    if (is.na(runtime_package) || !nzchar(runtime_package)) {
      stop("Required runtime package name must be nonempty.", call. = FALSE)
    }
    if (!is.null(spec$runtime_package) &&
        !identical(spec$runtime_package, runtime_package)) {
      stop(
        sprintf(
          "External repository '%s' must attest runtime package '%s'.",
          name, runtime_package
        ),
        call. = FALSE
      )
    }
    spec$runtime_package <- runtime_package
    spec$require_isolated_runtime <- TRUE
  }
  control$external_repositories[[name]] <- spec
  control$required_external_repositories <- unique(c(
    control$required_external_repositories, name
  ))
  control
}

.rqr_directory_digest <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- list.files(
    path, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  files <- sort(files)
  relative <- substring(files, nchar(path) + 2L)
  info <- file.info(files)
  links <- Sys.readlink(files)
  if (anyNA(info$mode) || anyDuplicated(relative)) {
    stop("The runtime directory is not a unique readable file set.", call. = FALSE)
  }
  kind <- ifelse(nzchar(links), "symlink", "file")
  mode <- sprintf("%04o", bitwAnd(as.integer(info$mode), 511L))
  content <- vapply(seq_along(files), function(index) {
    if (nzchar(links[index])) {
      digest::digest(links[index], algo = "sha256", serialize = FALSE)
    } else {
      digest::digest(
        file = files[index], algo = "sha256", serialize = FALSE
      )
    }
  }, character(1L))
  payload <- paste(
    kind, mode, content, relative, sep = "\t", collapse = "\n"
  )
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}

.rqr_git_manifest_payload <- function(
    repo_root, commit, source_subdir = ".") {
  treeish <- if (identical(source_subdir, ".")) {
    commit
  } else {
    paste0(commit, ":", source_subdir)
  }
  result <- .rqr_git_result(
    repo_root, c("ls-tree", "-r", "--full-tree", treeish)
  )
  if (!isTRUE(result$available)) {
    stop("Could not read the declared Git tree manifest.", call. = FALSE)
  }
  lines <- if (nzchar(result$value)) {
    strsplit(result$value, "\n", fixed = TRUE)[[1L]]
  } else {
    character(0)
  }
  pattern <- "^([0-9]{6}) ([^ ]+) ([0-9a-f]{40,64})\\t(.*)$"
  fields <- regmatches(lines, regexec(pattern, lines))
  if (!length(fields) || any(lengths(fields) != 5L)) {
    stop("Could not parse the declared Git tree manifest.", call. = FALSE)
  }
  manifest <- data.frame(
    mode = vapply(fields, `[[`, character(1L), 2L),
    type = vapply(fields, `[[`, character(1L), 3L),
    object = tolower(vapply(fields, `[[`, character(1L), 4L)),
    path = vapply(fields, `[[`, character(1L), 5L),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(manifest$path) || any(manifest$type != "blob")) {
    stop("The declared package tree is not a unique blob set.", call. = FALSE)
  }
  manifest <- manifest[order(manifest$path), , drop = FALSE]
  paste(
    manifest$mode, manifest$type, manifest$object, manifest$path,
    sep = "\t", collapse = "\n"
  )
}

.rqr_hash_archive_blob <- function(path, link_target = NULL) {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Git is required.", call. = FALSE)
  hash_path <- path
  temporary <- NULL
  if (!is.null(link_target)) {
    temporary <- tempfile("rqr-attested-link-")
    con <- file(temporary, open = "wb")
    writeBin(charToRaw(link_target), con)
    close(con)
    hash_path <- temporary
    on.exit(unlink(temporary), add = TRUE)
  }
  out <- suppressWarnings(system2(
    git, c("hash-object", "--no-filters", shQuote(hash_path)),
    stdout = TRUE, stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  ))
  status <- attr(out, "status") %||% 0L
  value <- trimws(paste(out, collapse = "\n"))
  if (!identical(as.integer(status), 0L) ||
      !grepl("^[0-9a-f]{40,64}$", value)) {
    stop("Could not hash an archived source entry.", call. = FALSE)
  }
  tolower(value)
}

.rqr_archive_manifest_payload <- function(archive_path, archive_prefix) {
  archive_prefix <- sub("/+$", "", as.character(archive_prefix)[1L])
  if (is.na(archive_prefix) || !nzchar(archive_prefix) ||
      grepl("(^|/)\\.\\.(/|$)", archive_prefix) ||
      startsWith(archive_prefix, "/")) {
    stop("Attested archive prefix is unsafe.", call. = FALSE)
  }
  entries <- utils::untar(archive_path, list = TRUE)
  entries <- sub("^\\./", "", entries)
  prefix_with_slash <- paste0(archive_prefix, "/")
  if (!length(entries) || anyNA(entries) || any(!nzchar(entries)) ||
      anyDuplicated(entries) || any(startsWith(entries, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", entries)) ||
      any(entries != archive_prefix &
            !startsWith(entries, prefix_with_slash))) {
    stop("Attested source archive paths are invalid.", call. = FALSE)
  }
  extraction_root <- tempfile("rqr-attested-archive-")
  dir.create(extraction_root)
  on.exit(unlink(extraction_root, recursive = TRUE, force = TRUE), add = TRUE)
  utils::untar(archive_path, exdir = extraction_root)
  source_root <- file.path(extraction_root, archive_prefix)
  if (!dir.exists(source_root)) {
    stop("Attested source archive root is missing.", call. = FALSE)
  }
  paths <- sort(list.files(
    source_root, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  ))
  relative <- substring(paths, nchar(source_root) + 2L)
  info <- file.info(paths)
  links <- Sys.readlink(paths)
  if (!length(paths) || anyNA(info$isdir) || any(info$isdir) ||
      anyDuplicated(relative)) {
    stop("Attested archive is not a unique file set.", call. = FALSE)
  }
  modes <- vapply(seq_along(paths), function(index) {
    if (nzchar(links[index])) return("120000")
    if (bitwAnd(as.integer(info$mode[index]), 73L) != 0L) {
      "100755"
    } else {
      "100644"
    }
  }, character(1L))
  objects <- vapply(seq_along(paths), function(index) {
    if (nzchar(links[index])) {
      .rqr_hash_archive_blob(paths[index], links[index])
    } else {
      .rqr_hash_archive_blob(paths[index])
    }
  }, character(1L))
  manifest <- data.frame(
    mode = modes, type = "blob", object = objects, path = relative,
    stringsAsFactors = FALSE
  )
  manifest <- manifest[order(manifest$path), , drop = FALSE]
  paste(
    manifest$mode, manifest$type, manifest$object, manifest$path,
    sep = "\t", collapse = "\n"
  )
}

.rqr_normalized_file_sha256 <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = file.info(path)$size)
  if (any(bytes == as.raw(0L))) {
    return(digest::digest(
      file = path, algo = "sha256", serialize = FALSE
    ))
  }
  value <- rawToChar(bytes)
  value <- gsub("\r\n?", "\n", value, perl = TRUE)
  value <- sub("\n*$", "\n", value, perl = TRUE)
  digest::digest(value, algo = "sha256", serialize = FALSE)
}

.rqr_safe_archive_extract <- function(archive_path, prefix = NULL) {
  archive_path <- normalizePath(archive_path, winslash = "/", mustWork = TRUE)
  entries <- sub("^\\./", "", utils::untar(archive_path, list = TRUE))
  if (!length(entries) || anyNA(entries) || any(!nzchar(entries)) ||
      any(startsWith(entries, "/")) ||
      any(grepl("(^|/)\\.\\.(/|$)", entries))) {
    stop("Package archive paths are unsafe.", call. = FALSE)
  }
  roots <- unique(sub("/.*$", "", entries))
  if (is.null(prefix)) {
    if (length(roots) != 1L) {
      stop("Package archive must contain one top-level root.", call. = FALSE)
    }
    prefix <- roots
  }
  prefix <- sub("/+$", "", as.character(prefix)[1L])
  if (!identical(roots, prefix)) {
    stop("Package archive root does not match its declaration.", call. = FALSE)
  }
  extraction_root <- tempfile("rqr-package-lineage-")
  dir.create(extraction_root)
  utils::untar(archive_path, exdir = extraction_root)
  list(
    extraction_root = extraction_root,
    package_root = file.path(extraction_root, prefix)
  )
}

.rqr_source_package_lineage <- function(
    source_archive_path, source_archive_prefix, source_package_path) {
  source <- .rqr_safe_archive_extract(
    source_archive_path, source_archive_prefix
  )
  built <- .rqr_safe_archive_extract(source_package_path)
  on.exit(
    unlink(source$extraction_root, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  on.exit(
    unlink(built$extraction_root, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  built_files <- list.files(
    built$package_root, recursive = TRUE, all.files = TRUE,
    full.names = TRUE, include.dirs = FALSE, no.. = TRUE
  )
  built_relative <- substring(
    built_files, nchar(built$package_root) + 2L
  )
  source_files <- file.path(source$package_root, built_relative)
  comparable <- built_relative != "DESCRIPTION"
  missing_source <- built_relative[comparable & !file.exists(source_files)]
  comparable_index <- which(comparable & file.exists(source_files))
  changed_source <- built_relative[comparable_index[vapply(
    comparable_index, function(index) {
      !identical(
        .rqr_normalized_file_sha256(built_files[index]),
        .rqr_normalized_file_sha256(source_files[index])
      )
    }, logical(1L)
  )]]
  changed_mode <- built_relative[comparable_index[vapply(
    comparable_index, function(index) {
      built_link <- Sys.readlink(built_files[index])
      source_link <- Sys.readlink(source_files[index])
      if (nzchar(built_link) || nzchar(source_link)) {
        return(!identical(built_link, source_link))
      }
      built_executable <- bitwAnd(
        as.integer(file.info(built_files[index])$mode), 73L
      ) != 0L
      source_executable <- bitwAnd(
        as.integer(file.info(source_files[index])$mode), 73L
      ) != 0L
      !identical(built_executable, source_executable)
    }, logical(1L)
  )]]
  source_description <- tryCatch(
    read.dcf(file.path(source$package_root, "DESCRIPTION")),
    error = function(e) NULL
  )
  built_description <- tryCatch(
    read.dcf(file.path(built$package_root, "DESCRIPTION")),
    error = function(e) NULL
  )
  allowed_description_transformations <- c(
    "Packaged", "Built", "NeedsCompilation", "Depends",
    "Author", "Maintainer"
  )
  source_fields <- if (is.null(source_description)) {
    character(0)
  } else {
    colnames(source_description)
  }
  built_fields <- if (is.null(built_description)) {
    character(0)
  } else {
    colnames(built_description)
  }
  compared_fields <- setdiff(
    source_fields, allowed_description_transformations
  )
  normalize_dcf <- function(value) {
    gsub("[[:space:]]+", " ", trimws(as.character(value)))
  }
  description_match <- length(compared_fields) > 0L &&
    all(compared_fields %in% built_fields) &&
    !length(setdiff(
      built_fields, c(source_fields, allowed_description_transformations)
    )) &&
    all(vapply(compared_fields, function(field) {
      identical(
        normalize_dcf(source_description[1L, field]),
        normalize_dcf(built_description[1L, field])
      )
    }, logical(1L)))
  source_relative <- list.files(
    source$package_root, recursive = TRUE, all.files = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  critical <- grepl("^(R/|src/|NAMESPACE$)", source_relative) &
    !grepl(
      "\\.(o|so|dll|dylib|a|sl|gch)$|(^|/)symbols\\.rds$",
      source_relative, ignore.case = TRUE
    ) &
    !grepl("(^|/)\\.", source_relative)
  buildignore_path <- file.path(source$package_root, ".Rbuildignore")
  if (file.exists(buildignore_path)) {
    patterns <- trimws(readLines(buildignore_path, warn = FALSE))
    patterns <- patterns[nzchar(patterns) & !startsWith(patterns, "#")]
    if (length(patterns)) {
      ignored <- vapply(source_relative, function(path) {
        any(vapply(patterns, function(pattern) {
          tryCatch(
            grepl(pattern, path, perl = TRUE, ignore.case = TRUE),
            error = function(e) {
              stop("Invalid .Rbuildignore expression.", call. = FALSE)
            }
          )
        }, logical(1L)))
      }, logical(1L))
      critical <- critical & !ignored
    }
  }
  missing_critical <- setdiff(source_relative[critical], built_relative)
  list(
    match = description_match &&
      !length(missing_source) &&
      !length(changed_source) &&
      !length(changed_mode) &&
      !length(missing_critical),
    description_match = description_match,
    missing_source_entries = sort(missing_source),
    changed_source_entries = sort(changed_source),
    changed_mode_entries = sort(changed_mode),
    missing_critical_entries = sort(missing_critical),
    built_source_manifest_digest =
      .rqr_directory_digest(built$package_root),
    built_source_manifest_entries = length(built_files)
  )
}

.rqr_command_receipt_verified <- function(
    path, recorded_sha256, expected_input_path, expected_input_sha256) {
  if (!file.exists(path) ||
      !grepl("^[0-9a-f]{64}$", recorded_sha256) ||
      !identical(
        digest::digest(file = path, algo = "sha256", serialize = FALSE),
        recorded_sha256
      )) {
    return(FALSE)
  }
  receipt <- tryCatch(readRDS(path), error = function(e) NULL)
  is.list(receipt) &&
    .rqr_nonmissing_text(c(
      receipt$executable, receipt$working_directory,
      receipt$input_path, receipt$input_sha256
    )) &&
    is.character(receipt$arguments) &&
    identical(
      normalizePath(
        receipt$input_path, winslash = "/", mustWork = FALSE
      ),
      normalizePath(
        expected_input_path, winslash = "/", mustWork = FALSE
      )
    ) &&
    identical(
      tolower(as.character(receipt$input_sha256)),
      tolower(as.character(expected_input_sha256))
    )
}

.rqr_runtime_install_receipt_digest <- function(
    source_archive_sha256, source_package_sha256,
    built_source_manifest_digest, runtime_package_tree_digest,
    build_stdout_sha256, build_stderr_sha256,
    install_stdout_sha256, install_stderr_sha256,
    build_command_receipt_sha256, install_command_receipt_sha256,
    runtime_lineage_marker_sha256, R_version, platform) {
  fields <- c(
    source_archive_sha256 = source_archive_sha256,
    source_package_sha256 = source_package_sha256,
    built_source_manifest_digest = built_source_manifest_digest,
    runtime_package_tree_digest = runtime_package_tree_digest,
    build_stdout_sha256 = build_stdout_sha256,
    build_stderr_sha256 = build_stderr_sha256,
    install_stdout_sha256 = install_stdout_sha256,
    install_stderr_sha256 = install_stderr_sha256,
    build_command_receipt_sha256 = build_command_receipt_sha256,
    install_command_receipt_sha256 = install_command_receipt_sha256,
    runtime_lineage_marker_sha256 = runtime_lineage_marker_sha256,
    R_version = R_version,
    platform = platform
  )
  digest::digest(
    paste(names(fields), fields, sep = "=", collapse = "\n"),
    algo = "sha256", serialize = FALSE
  )
}

.rqr_description_version <- function(repo_root, source_subdir = ".") {
  description <- file.path(repo_root, source_subdir, "DESCRIPTION")
  if (!file.exists(description)) return(NA_character_)
  value <- suppressWarnings(tryCatch(
    read.dcf(description, fields = "Version")[1L, 1L],
    error = function(e) NA_character_
  ))
  as.character(value)[1L]
}

.rqr_runtime_package_provenance <- function(
    package, repository_state, runtime_attestation = NULL) {
  require_isolated_runtime <- isTRUE(
    repository_state$require_isolated_runtime
  )
  empty <- list(
    runtime_package = package %||% NA_character_,
    runtime_package_available = FALSE,
    runtime_package_path = NA_character_,
    runtime_package_version = NA_character_,
    source_description_version = NA_character_,
    source_package_path = NA_character_,
    source_tree_digest = NA_character_,
    source_worktree_digest = NA_character_,
    runtime_package_tree_digest = NA_character_,
    runtime_direct_source_path_match = FALSE,
    runtime_attestation = runtime_attestation %||% NA_character_,
    runtime_attestation_available = FALSE,
    runtime_attestation_match = FALSE,
    runtime_attestation_schema = NA_character_,
    source_access_mode = NA_character_,
    source_archive_verified = FALSE,
    source_archive_tree_match = FALSE,
    source_git_manifest_digest = NA_character_,
    source_archive_manifest_digest = NA_character_,
    source_package_verified = FALSE,
    source_package_archive_match = FALSE,
    build_evidence_verified = FALSE,
    install_evidence_verified = FALSE,
    runtime_lineage_marker_match = FALSE,
    runtime_install_receipt_match = FALSE,
    source_archive_isolated_from_source = FALSE,
    source_checkout_unchanged = FALSE,
    runtime_isolated_from_source = FALSE,
    require_isolated_runtime = require_isolated_runtime,
    runtime_source_match = FALSE,
    runtime_provenance_complete = FALSE
  )
  if (is.null(package) || length(package) != 1L ||
      is.na(package) || !nzchar(as.character(package))) return(empty)
  package <- as.character(package)[1L]
  empty$runtime_package <- package
  source_subdir <- repository_state$source_subdir %||% "."
  source_path <- normalizePath(
    file.path(repository_state$repo_root, source_subdir),
    winslash = "/", mustWork = FALSE
  )
  source_version <- .rqr_description_version(
    repository_state$repo_root, source_subdir
  )
  tree_spec <- if (identical(source_subdir, ".")) {
    "HEAD^{tree}"
  } else {
    paste0("HEAD:", source_subdir)
  }
  tree_result <- .rqr_git_result(
    repository_state$repo_root, c("rev-parse", tree_spec)
  )
  source_tree <- if (tree_result$available) {
    tolower(tree_result$value)
  } else {
    NA_character_
  }
  empty$source_description_version <- source_version
  empty$source_package_path <- source_path
  empty$source_tree_digest <- source_tree
  empty$source_worktree_digest <- if (dir.exists(source_path)) {
    tryCatch(.rqr_directory_digest(source_path), error = function(e) NA_character_)
  } else {
    NA_character_
  }
  if (!requireNamespace(package, quietly = TRUE)) return(empty)

  namespace <- asNamespace(package)
  runtime_path <- suppressWarnings(tryCatch(
    normalizePath(
      getNamespaceInfo(namespace, "path"), winslash = "/", mustWork = TRUE
    ),
    error = function(e) NA_character_
  ))
  runtime_version <- tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(e) NA_character_
  )
  direct_path_match <- !is.na(runtime_path) &&
    !is.na(source_path) &&
    identical(runtime_path, source_path)
  runtime_digest <- if (!is.na(runtime_path)) {
    tryCatch(.rqr_directory_digest(runtime_path), error = function(e) NA_character_)
  } else {
    NA_character_
  }

  attestation_available <- !is.null(runtime_attestation) &&
    file.exists(runtime_attestation)
  attestation_match <- FALSE
  attestation_schema <- NA_character_
  source_access_mode <- NA_character_
  source_archive_verified <- FALSE
  source_archive_tree_match <- FALSE
  source_git_manifest_digest <- NA_character_
  source_archive_manifest_digest <- NA_character_
  source_package_verified <- FALSE
  source_package_archive_match <- FALSE
  build_evidence_verified <- FALSE
  install_evidence_verified <- FALSE
  runtime_lineage_marker_match <- FALSE
  runtime_install_receipt_match <- FALSE
  source_archive_isolated_from_source <- FALSE
  source_checkout_unchanged <- FALSE
  source_root <- normalizePath(
    repository_state$repo_root, winslash = "/", mustWork = FALSE
  )
  path_within <- function(path, root) {
    identical(path, root) || startsWith(path, paste0(root, "/"))
  }
  runtime_isolated_from_source <- !is.na(runtime_path) &&
    !is.na(source_root) &&
    !path_within(runtime_path, source_root) &&
    !path_within(source_root, runtime_path)
  if (attestation_available) {
    attestation <- tryCatch(readRDS(runtime_attestation), error = function(e) NULL)
    required <- c(
      "schema_version", "package", "package_version", "source_commit",
      "source_tree_digest", "source_repo_root", "source_subdir",
      "source_access_mode", "source_archive_prefix",
      "source_checkout_snapshot_before", "source_checkout_snapshot_after",
      "source_checkout_unchanged", "source_archive_path",
      "source_archive_sha256", "source_git_manifest_digest",
      "source_archive_manifest_digest", "source_archive_tree_match",
      "source_archive_isolated_from_source", "source_package_path",
      "source_package_sha256", "source_package_archive_match",
      "built_source_manifest_digest", "built_source_manifest_entries",
      "build_stdout_path", "build_stdout_sha256",
      "build_stderr_path", "build_stderr_sha256",
      "install_stdout_path", "install_stdout_sha256",
      "install_stderr_path", "install_stderr_sha256",
      "build_command_receipt_path", "build_command_receipt_sha256",
      "build_executable", "build_arguments", "build_working_directory",
      "build_input_path",
      "install_command_receipt_path", "install_command_receipt_sha256",
      "install_executable", "install_arguments",
      "install_working_directory", "install_input_path",
      "runtime_package_path", "runtime_install_receipt_digest",
      "runtime_lineage_marker_path", "runtime_lineage_marker_sha256",
      "runtime_package_tree_digest", "runtime_isolated_from_source",
      "R_version", "platform"
    )
    attestation_schema <- if (is.list(attestation)) {
      as.character(attestation$schema_version %||% NA_character_)[1L]
    } else {
      NA_character_
    }
    source_access_mode <- if (is.list(attestation)) {
      as.character(attestation$source_access_mode %||% NA_character_)[1L]
    } else {
      NA_character_
    }
    archive_path <- if (is.list(attestation)) {
      as.character(attestation$source_archive_path %||% NA_character_)[1L]
    } else {
      NA_character_
    }
    recorded_archive_digest <- if (is.list(attestation)) {
      tolower(as.character(
        attestation$source_archive_sha256 %||% NA_character_
      )[1L])
    } else {
      NA_character_
    }
    source_archive_verified <- !is.na(archive_path) &&
      nzchar(archive_path) &&
      file.exists(archive_path) &&
      grepl("^[0-9a-f]{64}$", recorded_archive_digest) &&
      identical(
        digest::digest(
          file = archive_path, algo = "sha256", serialize = FALSE
        ),
        recorded_archive_digest
      )
    source_package_path <- if (is.list(attestation)) {
      as.character(attestation$source_package_path %||% NA_character_)[1L]
    } else {
      NA_character_
    }
    recorded_source_package_digest <- if (is.list(attestation)) {
      tolower(as.character(
        attestation$source_package_sha256 %||% NA_character_
      )[1L])
    } else {
      NA_character_
    }
    source_package_verified <- !is.na(source_package_path) &&
      nzchar(source_package_path) && file.exists(source_package_path) &&
      grepl("^[0-9a-f]{64}$", recorded_source_package_digest) &&
      identical(
        digest::digest(
          file = source_package_path, algo = "sha256", serialize = FALSE
        ),
        recorded_source_package_digest
      )
    source_package_lineage <- if (
        source_archive_verified && source_package_verified &&
        is.list(attestation)) {
      tryCatch(
        .rqr_source_package_lineage(
          archive_path,
          as.character(attestation$source_archive_prefix)[1L],
          source_package_path
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    source_package_archive_match <-
      is.list(source_package_lineage) &&
      isTRUE(source_package_lineage$match) &&
      identical(
        source_package_lineage$built_source_manifest_digest,
        tolower(as.character(
          attestation$built_source_manifest_digest %||% NA_character_
        )[1L])
      ) &&
      identical(
        as.integer(source_package_lineage$built_source_manifest_entries),
        as.integer(attestation$built_source_manifest_entries)
      ) &&
      isTRUE(attestation$source_package_archive_match)
    git_manifest_payload <- archive_manifest_payload <- NULL
    if (source_archive_verified && is.list(attestation)) {
      git_manifest_payload <- tryCatch(
        .rqr_git_manifest_payload(
          repository_state$repo_root,
          repository_state$git_commit,
          source_subdir = source_subdir
        ),
        error = function(e) NULL
      )
      archive_manifest_payload <- tryCatch(
        .rqr_archive_manifest_payload(
          archive_path,
          as.character(
            attestation$source_archive_prefix %||% NA_character_
          )[1L]
        ),
        error = function(e) NULL
      )
    }
    source_git_manifest_digest <- if (is.character(git_manifest_payload)) {
      digest::digest(
        git_manifest_payload, algo = "sha256", serialize = FALSE
      )
    } else {
      NA_character_
    }
    source_archive_manifest_digest <- if (is.character(
        archive_manifest_payload
      )) {
      digest::digest(
        archive_manifest_payload, algo = "sha256", serialize = FALSE
      )
    } else {
      NA_character_
    }
    source_archive_tree_match <-
      is.character(git_manifest_payload) &&
      is.character(archive_manifest_payload) &&
      identical(git_manifest_payload, archive_manifest_payload) &&
      identical(
        source_git_manifest_digest,
        tolower(as.character(
          attestation$source_git_manifest_digest %||% NA_character_
        )[1L])
      ) &&
      identical(
        source_archive_manifest_digest,
        tolower(as.character(
          attestation$source_archive_manifest_digest %||% NA_character_
        )[1L])
      ) &&
      isTRUE(attestation$source_archive_tree_match)
    verify_file <- function(path, recorded_digest) {
      .rqr_nonmissing_text(c(path, recorded_digest)) &&
        file.exists(path) &&
        grepl("^[0-9a-f]{64}$", recorded_digest) &&
        identical(
          digest::digest(
            file = path, algo = "sha256", serialize = FALSE
          ),
          tolower(recorded_digest)
        )
    }
    evidence_fields <- c(
      "build_stdout", "build_stderr", "install_stdout", "install_stderr"
    )
    evidence_verified <- stats::setNames(vapply(evidence_fields, function(field) {
      verify_file(
        as.character(attestation[[paste0(field, "_path")]] %||%
          NA_character_)[1L],
        tolower(as.character(
          attestation[[paste0(field, "_sha256")]] %||% NA_character_
        )[1L])
      )
    }, logical(1L)), evidence_fields)
    read_command_receipt <- function(field) {
      path <- as.character(
        attestation[[paste0(field, "_command_receipt_path")]] %||%
          NA_character_
      )[1L]
      digest_value <- tolower(as.character(
        attestation[[paste0(field, "_command_receipt_sha256")]] %||%
          NA_character_
      )[1L])
      input_path <- as.character(
        attestation[[paste0(field, "_input_path")]] %||% NA_character_
      )[1L]
      input_digest <- if (identical(field, "build")) {
        recorded_archive_digest
      } else {
        recorded_source_package_digest
      }
      verified <- .rqr_command_receipt_verified(
        path, digest_value, input_path, input_digest
      )
      receipt <- if (verified) {
        tryCatch(readRDS(path), error = function(e) NULL)
      } else {
        NULL
      }
      expected_executable <- normalizePath(
        file.path(R.home("bin"), "R"),
        winslash = "/", mustWork = FALSE
      )
      expected_arguments <- as.character(
        attestation[[paste0(field, "_arguments")]] %||% character(0)
      )
      expected_workdir <- normalizePath(
        as.character(
          attestation[[paste0(field, "_working_directory")]] %||%
            NA_character_
        )[1L],
        winslash = "/", mustWork = FALSE
      )
      command_arguments_unquoted <- if (is.list(receipt)) {
        gsub("[\"']", "", receipt$arguments)
      } else {
        character(0)
      }
      input_candidates <- c(
        normalizePath(input_path, winslash = "/", mustWork = FALSE),
        basename(input_path)
      )
      input_candidates <- unique(input_candidates[
        !is.na(input_candidates) & nzchar(input_candidates)
      ])
      input_appears_in_command <- is.list(receipt) &&
        length(input_candidates) > 0L &&
        any(vapply(input_candidates, function(candidate) {
          any(grepl(candidate, command_arguments_unquoted, fixed = TRUE))
        }, logical(1L)))
      command_shape <- if (identical(field, "build")) {
        length(expected_arguments) >= 4L &&
          identical(expected_arguments[1:2], c("CMD", "build"))
      } else {
        length(expected_arguments) >= 4L &&
          identical(expected_arguments[1:2], c("CMD", "INSTALL"))
      }
      list(
        verified = verified && is.list(receipt) &&
          identical(
            normalizePath(
              receipt$executable, winslash = "/", mustWork = FALSE
            ),
            expected_executable
          ) &&
          identical(
            normalizePath(
              as.character(
                attestation[[paste0(field, "_executable")]]
              )[1L],
              winslash = "/", mustWork = FALSE
            ),
            expected_executable
          ) &&
          identical(receipt$arguments, expected_arguments) &&
          identical(
            normalizePath(
              receipt$working_directory,
              winslash = "/", mustWork = FALSE
            ),
            expected_workdir
          ) &&
          command_shape && input_appears_in_command,
        receipt = receipt,
        sha256 = digest_value
      )
    }
    build_command <- read_command_receipt("build")
    install_command <- read_command_receipt("install")
    build_evidence_verified <- all(evidence_verified[c(
      "build_stdout", "build_stderr"
    )]) && isTRUE(build_command$verified)
    install_evidence_verified <- all(evidence_verified[c(
      "install_stdout", "install_stderr"
    )]) && isTRUE(install_command$verified)
    marker_path <- as.character(
      attestation$runtime_lineage_marker_path %||% NA_character_
    )[1L]
    marker_digest <- tolower(as.character(
      attestation$runtime_lineage_marker_sha256 %||% NA_character_
    )[1L])
    marker_verified <- verify_file(marker_path, marker_digest)
    marker <- if (marker_verified) {
      tryCatch(readRDS(marker_path), error = function(e) NULL)
    } else {
      NULL
    }
    runtime_lineage_marker_match <- marker_verified &&
      is.list(marker) &&
      identical(
        normalizePath(marker_path, winslash = "/", mustWork = FALSE),
        file.path(runtime_path, "RQR-RUNTIME-LINEAGE.rds")
      ) &&
      identical(
        marker$schema_version,
        "rqrgibbs_runtime_lineage_marker/1.0.0"
      ) &&
      identical(as.character(marker$package), package) &&
      identical(as.character(marker$package_version), runtime_version) &&
      identical(
        tolower(as.character(marker$source_package_sha256)),
        recorded_source_package_digest
      ) &&
      identical(
        tolower(as.character(marker$built_source_manifest_digest)),
        tolower(as.character(
          attestation$built_source_manifest_digest
        ))
      ) &&
      identical(
        tolower(as.character(
          marker$install_command_receipt_sha256
        )),
        install_command$sha256
      )
    receipt_digest <- if (is.list(attestation)) {
      tryCatch(
        .rqr_runtime_install_receipt_digest(
          source_archive_sha256 = recorded_archive_digest,
          source_package_sha256 = recorded_source_package_digest,
          built_source_manifest_digest = tolower(as.character(
            attestation$built_source_manifest_digest %||% NA_character_
          )[1L]),
          runtime_package_tree_digest = tolower(as.character(
            attestation$runtime_package_tree_digest %||% NA_character_
          )[1L]),
          build_stdout_sha256 = tolower(as.character(
            attestation$build_stdout_sha256 %||% NA_character_
          )[1L]),
          build_stderr_sha256 = tolower(as.character(
            attestation$build_stderr_sha256 %||% NA_character_
          )[1L]),
          install_stdout_sha256 = tolower(as.character(
            attestation$install_stdout_sha256 %||% NA_character_
          )[1L]),
          install_stderr_sha256 = tolower(as.character(
            attestation$install_stderr_sha256 %||% NA_character_
          )[1L]),
          build_command_receipt_sha256 = build_command$sha256,
          install_command_receipt_sha256 = install_command$sha256,
          runtime_lineage_marker_sha256 = marker_digest,
          R_version = as.character(
            attestation$R_version %||% NA_character_
          )[1L],
          platform = as.character(
            attestation$platform %||% NA_character_
          )[1L]
        ),
        error = function(e) NA_character_
      )
    } else {
      NA_character_
    }
    runtime_install_receipt_match <-
      grepl("^[0-9a-f]{64}$", receipt_digest) &&
      identical(
        receipt_digest,
        tolower(as.character(
          attestation$runtime_install_receipt_digest %||% NA_character_
        )[1L])
      )
    archive_path_normalized <- if (!is.na(archive_path) &&
        nzchar(archive_path)) {
      normalizePath(archive_path, winslash = "/", mustWork = FALSE)
    } else {
      NA_character_
    }
    source_archive_isolated_from_source <-
      !is.na(archive_path_normalized) &&
      !is.na(source_root) &&
      !path_within(archive_path_normalized, source_root) &&
      !path_within(source_root, archive_path_normalized)
    source_checkout_unchanged <- is.list(attestation) &&
      isTRUE(attestation$source_checkout_unchanged) &&
      grepl(
        "^[0-9a-f]{64}$",
        tolower(as.character(
          attestation$source_checkout_snapshot_before %||% NA_character_
        )[1L])
      ) &&
      identical(
        tolower(as.character(
          attestation$source_checkout_snapshot_before
        )[1L]),
        tolower(as.character(
          attestation$source_checkout_snapshot_after
        )[1L])
      )
    attestation_match <- is.list(attestation) &&
      all(required %in% names(attestation)) &&
      identical(
        attestation$schema_version,
        "rqrgibbs_runtime_attestation/4.0.0"
      ) &&
      identical(as.character(attestation$package), package) &&
      identical(as.character(attestation$package_version), runtime_version) &&
      identical(tolower(as.character(attestation$source_commit)),
                repository_state$git_commit) &&
      identical(tolower(as.character(attestation$source_tree_digest)),
                source_tree) &&
      identical(
        normalizePath(
          as.character(attestation$source_repo_root),
          winslash = "/", mustWork = FALSE
        ),
        normalizePath(
          repository_state$repo_root,
          winslash = "/", mustWork = FALSE
        )
      ) &&
      identical(as.character(attestation$source_subdir), source_subdir) &&
      identical(source_access_mode, "git_archive_read_only") &&
      source_archive_verified &&
      source_archive_tree_match &&
      source_package_verified &&
      source_package_archive_match &&
      build_evidence_verified &&
      install_evidence_verified &&
      runtime_lineage_marker_match &&
      runtime_install_receipt_match &&
      isTRUE(attestation$source_archive_isolated_from_source) &&
      source_archive_isolated_from_source &&
      source_checkout_unchanged &&
      identical(
        normalizePath(
          as.character(attestation$runtime_package_path),
          winslash = "/", mustWork = FALSE
        ),
        runtime_path
      ) &&
      identical(
        tolower(as.character(attestation$runtime_package_tree_digest)),
        runtime_digest
      ) &&
      identical(as.character(attestation$R_version), R.version.string) &&
      identical(as.character(attestation$platform), R.version$platform) &&
      isTRUE(attestation$runtime_isolated_from_source) &&
      runtime_isolated_from_source
  }
  version_match <- .rqr_nonmissing_text(source_version) &&
    identical(runtime_version, source_version)
  repository_eligible <- isTRUE(repository_state$provenance_complete) &&
    identical(repository_state$git_dirty, FALSE) &&
    isTRUE(repository_state$expected_git_commit_match)
  accepted_binding <- if (require_isolated_runtime) {
    attestation_match
  } else {
    direct_path_match || attestation_match
  }
  runtime_source_match <- repository_eligible && version_match &&
    accepted_binding
  runtime_complete <- .rqr_nonmissing_text(c(
    runtime_path, runtime_version, source_version, source_tree, runtime_digest
  ))
  utils::modifyList(empty, list(
    runtime_package_available = TRUE,
    runtime_package_path = runtime_path,
    runtime_package_version = runtime_version,
    source_description_version = source_version,
    source_package_path = source_path,
    source_tree_digest = source_tree,
    source_worktree_digest = empty$source_worktree_digest,
    runtime_package_tree_digest = runtime_digest,
    runtime_direct_source_path_match = direct_path_match,
    runtime_attestation_available = attestation_available,
    runtime_attestation_match = attestation_match,
    runtime_attestation_schema = attestation_schema,
    source_access_mode = source_access_mode,
    source_archive_verified = source_archive_verified,
    source_archive_tree_match = source_archive_tree_match,
    source_git_manifest_digest = source_git_manifest_digest,
    source_archive_manifest_digest = source_archive_manifest_digest,
    source_package_verified = source_package_verified,
    source_package_archive_match = source_package_archive_match,
    build_evidence_verified = build_evidence_verified,
    install_evidence_verified = install_evidence_verified,
    runtime_lineage_marker_match = runtime_lineage_marker_match,
    runtime_install_receipt_match = runtime_install_receipt_match,
    source_archive_isolated_from_source =
      source_archive_isolated_from_source,
    source_checkout_unchanged = source_checkout_unchanged,
    runtime_isolated_from_source = runtime_isolated_from_source,
    require_isolated_runtime = require_isolated_runtime,
    runtime_source_match = runtime_source_match,
    runtime_provenance_complete = runtime_complete
  ))
}

.rqr_nonmissing_text <- function(x) {
  length(x) > 0L && all(!is.na(x) & nzchar(as.character(x)))
}

.rqr_compiler_info <- function() {
  compiler <- as.character(R.version$compiler %||% NA_character_)[1L]
  if (!is.na(compiler) && nzchar(compiler)) return(compiler)
  r_bin <- file.path(R.home("bin"), "R")
  configured <- suppressWarnings(tryCatch(
    system2(r_bin, c("CMD", "config", "CXX17"), stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  ))
  status <- attr(configured, "status") %||% 0L
  if (!length(configured) || !identical(as.integer(status), 0L)) {
    return(NA_character_)
  }
  configured <- trimws(paste(configured, collapse = " "))
  if (nzchar(configured)) configured else NA_character_
}

.rqr_repository_provenance <- function(spec) {
  spec <- spec %||% list(
    repo_root = NULL, expected_git_commit = NULL,
    runtime_package = NULL, runtime_attestation = NULL, source_subdir = "."
  )
  commit_result <- .rqr_git_result(spec$repo_root, c("rev-parse", "HEAD"))
  status_result <- .rqr_git_result(spec$repo_root, c("status", "--porcelain"))
  git_commit <- if (commit_result$available) {
    tolower(commit_result$value)
  } else {
    NA_character_
  }
  expected <- spec$expected_git_commit %||% NA_character_
  commit_match <- if (is.na(expected) || is.na(git_commit)) {
    NA
  } else {
    identical(git_commit, expected)
  }
  git_dirty <- if (status_result$available) {
    nzchar(status_result$value)
  } else {
    NA
  }
  metadata_complete <- isTRUE(commit_result$available) &&
    isTRUE(status_result$available) &&
    .rqr_nonmissing_text(expected)
  repository_state <- list(
    repo_root = spec$repo_root %||% NA_character_,
    git_commit = git_commit,
    git_commit_available = isTRUE(commit_result$available),
    git_status_available = isTRUE(status_result$available),
    git_dirty = git_dirty,
    expected_git_commit = expected,
    expected_git_commit_match = commit_match,
    require_isolated_runtime = isTRUE(spec$require_isolated_runtime),
    source_subdir = spec$source_subdir %||% ".",
    provenance_complete = metadata_complete
  )
  runtime <- .rqr_runtime_package_provenance(
    spec$runtime_package %||% NULL,
    repository_state,
    spec$runtime_attestation %||% NULL
  )
  runtime_required <- !is.null(spec$runtime_package)
  provenance_complete <- metadata_complete &&
    (!runtime_required || isTRUE(runtime$runtime_provenance_complete))
  reproducibility_eligible <- provenance_complete &&
    !is.na(git_dirty) && !git_dirty && isTRUE(commit_match) &&
    (!runtime_required || isTRUE(runtime$runtime_source_match))
  utils::modifyList(
    repository_state,
    c(runtime, list(
      provenance_complete = provenance_complete,
      reproducibility_eligible = reproducibility_eligible
    ))
  )
}

.rqr_provenance <- function(data, matrices = list(), numerical_policy = NA_character_,
                            initial_seed = NULL, repo_root = NULL,
                            expected_git_commit = NULL, backend = NA_character_,
                            backend_requested = backend,
                            backend_resolved = backend,
                            objects = list(), external_repositories = list(),
                            required_external_repositories = character(0),
                            primary_runtime_attestation = NULL) {
  if (is.null(repo_root)) repo_root <- .rqr_find_repo_root()
  pkg_version <- tryCatch(as.character(utils::packageVersion("rqrgibbs")), error = function(e) NA_character_)
  required_external_repositories <- as.character(required_external_repositories)
  dependency_names <- unique(c(
    "Rcpp", "RcppArmadillo", "digest", required_external_repositories
  ))
  dependency_versions <- vapply(dependency_names, function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  }, character(1L))
  commit_result <- .rqr_git_result(repo_root, c("rev-parse", "HEAD"))
  status_result <- .rqr_git_result(repo_root, c("status", "--porcelain"))
  git_commit <- if (commit_result$available) tolower(commit_result$value) else NA_character_
  dirty <- status_result$value
  expected_git_commit <- if (is.null(expected_git_commit)) {
    NA_character_
  } else {
    tolower(as.character(expected_git_commit)[1L])
  }
  commit_match <- if (is.na(expected_git_commit) || is.na(git_commit)) {
    NA
  } else {
    identical(git_commit, expected_git_commit)
  }
  ext <- extSoftVersion()
  session <- utils::sessionInfo()
  compiler <- .rqr_compiler_info()
  BLAS <- as.character(session$BLAS %||% unname(ext["BLAS"] %||% NA_character_))
  LAPACK <- as.character(session$LAPACK %||% tryCatch(La_library(), error = function(e) NA_character_))
  backend_requested <- as.character(backend_requested)[1L]
  backend_resolved <- as.character(backend_resolved)[1L]
  backend <- backend_resolved
  rng_kind <- RNGkind()
  matrix_digests <- lapply(matrices, .rqr_digest)
  object_digests <- lapply(objects, .rqr_digest)
  data_digest <- .rqr_digest(data)
  primary_state <- .rqr_repository_provenance(list(
    repo_root = repo_root,
    expected_git_commit = if (is.na(expected_git_commit)) {
      NULL
    } else {
      expected_git_commit
    },
    runtime_package = "rqrgibbs",
    runtime_attestation = primary_runtime_attestation,
    require_isolated_runtime = !is.na(expected_git_commit),
    source_subdir = "application"
  ))
  external_names <- unique(c(
    names(external_repositories) %||% character(0),
    required_external_repositories
  ))
  external_states <- lapply(external_names, function(name) {
    .rqr_repository_provenance(external_repositories[[name]])
  })
  names(external_states) <- external_names
  required_external_complete <- all(vapply(
    required_external_repositories,
    function(name) isTRUE(external_states[[name]]$provenance_complete),
    logical(1L)
  ))
  required_external_eligible <- all(vapply(
    required_external_repositories,
    function(name) isTRUE(external_states[[name]]$reproducibility_eligible),
    logical(1L)
  ))
  basic_provenance_complete <- isTRUE(commit_result$available) &&
    isTRUE(status_result$available) &&
    !is.na(pkg_version) && nzchar(pkg_version) &&
    !is.na(R.version.string) && nzchar(R.version.string) &&
    !is.na(R.version$platform) && nzchar(R.version$platform) &&
    all(!is.na(dependency_versions) & nzchar(dependency_versions)) &&
    nzchar(data_digest) &&
    all(vapply(matrix_digests, nzchar, logical(1L))) &&
    all(vapply(object_digests, nzchar, logical(1L))) &&
    isTRUE(primary_state$runtime_provenance_complete) &&
    required_external_complete
  provenance_complete <- basic_provenance_complete &&
    .rqr_nonmissing_text(compiler) &&
    .rqr_nonmissing_text(BLAS) &&
    .rqr_nonmissing_text(LAPACK) &&
    .rqr_nonmissing_text(backend_requested) &&
    .rqr_nonmissing_text(backend_resolved) &&
    .rqr_nonmissing_text(rng_kind)
  git_dirty <- if (status_result$available) nzchar(dirty) else NA
  reproducibility_eligible <- provenance_complete &&
    !is.na(git_dirty) && !git_dirty && isTRUE(commit_match) &&
    isTRUE(primary_state$runtime_source_match) &&
    required_external_eligible
  list(
    schema_version = .rqr_schema_version(),
    package_version = pkg_version,
    git_commit = git_commit,
    git_commit_available = isTRUE(commit_result$available),
    git_status_available = isTRUE(status_result$available),
    git_dirty = git_dirty,
    expected_git_commit = expected_git_commit,
    expected_git_commit_match = commit_match,
    repo_root = repo_root,
    R_version = R.version.string,
    platform = R.version$platform,
    compiler = compiler,
    BLAS = BLAS,
    LAPACK = LAPACK,
    dependency_versions = dependency_versions,
    RNGkind = rng_kind,
    initial_seed = initial_seed,
    numerical_policy = numerical_policy,
    backend = backend,
    backend_requested = backend_requested,
    backend_resolved = backend_resolved,
    data_digest = data_digest,
    matrix_digests = matrix_digests,
    object_digests = object_digests,
    primary_repository = primary_state,
    primary_runtime_package_path = primary_state$runtime_package_path,
    primary_source_commit = primary_state$git_commit,
    primary_source_tree_digest = primary_state$source_tree_digest,
    primary_runtime_tree_digest = primary_state$runtime_package_tree_digest,
    primary_runtime_source_match = primary_state$runtime_source_match,
    primary_runtime_attestation =
      primary_state$runtime_attestation,
    external_repositories = external_states,
    required_external_repositories = required_external_repositories,
    basic_provenance_complete = basic_provenance_complete,
    provenance_complete = provenance_complete,
    reproducibility_eligible = reproducibility_eligible,
    recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

.rqr_sample_lambda_collapsed <- function(loss_sum, n, lambda_prior, learning_rate_mode) {
  pp <- .rqr_lambda_posterior_params(loss_sum, n, lambda_prior, learning_rate_mode)
  stats::rgamma(1L, shape = pp$shape, rate = pp$rate)
}

.rqr_loss_sum <- function(y, X, beta1, beta2, coverage_level) {
  eta1 <- drop(X %*% beta1)
  eta2 <- drop(X %*% beta2)
  sum(rqr_check_loss(rqr_residual_product(y, eta1, eta2), coverage_level))
}

.rqr_lambda_summary <- function(lambda_draws) {
  lambda_draws <- as.numeric(lambda_draws)
  lambda_draws <- lambda_draws[is.finite(lambda_draws) & lambda_draws > 0]
  if (!length(lambda_draws)) {
    return(list(
      mean = NA_real_, median = NA_real_, sd = NA_real_,
      q05 = NA_real_, q25 = NA_real_, q75 = NA_real_, q95 = NA_real_,
      implied_sigma_mean = NA_real_
    ))
  }
  qs <- as.numeric(stats::quantile(lambda_draws, probs = c(0.05, 0.25, 0.75, 0.95), names = FALSE, type = 8))
  list(
    mean = mean(lambda_draws),
    median = stats::median(lambda_draws),
    sd = stats::sd(lambda_draws),
    q05 = qs[1L],
    q25 = qs[2L],
    q75 = qs[3L],
    q95 = qs[4L],
    implied_sigma_mean = mean(1 / lambda_draws)
  )
}

.rqr_assert_xy <- function(y, X) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  if (!length(y) || !nrow(X) || length(y) != nrow(X)) {
    stop("y must be numeric with length equal to nrow(X).", call. = FALSE)
  }
  if (!ncol(X)) stop("X must have at least one column.", call. = FALSE)
  if (any(!is.finite(y)) || any(!is.finite(X))) {
    stop("y and X must contain only finite values.", call. = FALSE)
  }
  list(y = y, X = X)
}

.rqr_intercept_index <- function(X, tol = 1e-12) {
  X <- as.matrix(X)
  for (j in seq_len(ncol(X))) {
    xj <- X[, j]
    if (all(is.finite(xj)) && max(abs(xj - xj[1L])) <= tol && abs(xj[1L] - 1) <= tol) {
      return(j)
    }
  }
  NA_integer_
}

.rqr_init_roots <- function(y, X, coverage_level, init = list()) {
  p <- ncol(X)
  b1 <- init$beta1 %||% init$beta_root1 %||% NULL
  b2 <- init$beta2 %||% init$beta_root2 %||% NULL
  if (!is.null(b1) || !is.null(b2)) {
    if (is.null(b1) || is.null(b2)) stop("init must provide both beta1 and beta2.", call. = FALSE)
    b1 <- as.numeric(b1)
    b2 <- as.numeric(b2)
    if (length(b1) != p || length(b2) != p) stop("Initial beta vectors must have ncol(X) entries.", call. = FALSE)
    return(list(beta1 = b1, beta2 = b2))
  }
  alpha <- rqr_constants(coverage_level)$alpha
  probs <- c((1 - alpha) / 2, 1 - (1 - alpha) / 2)
  qs <- as.numeric(stats::quantile(y, probs = probs, names = FALSE, type = 8))
  beta1 <- rep(0, p)
  beta2 <- rep(0, p)
  jj <- .rqr_intercept_index(X)
  if (is.na(jj)) jj <- 1L
  beta1[jj] <- qs[1L]
  beta2[jj] <- qs[2L]
  list(beta1 = beta1, beta2 = beta2)
}

.rqr_prior_precision <- function(beta_prior_obj, state, p) {
  if (is.null(beta_prior_obj)) {
    return(rep(1 / 1e4, p))
  }
  type <- as.character(beta_prior_obj$type %||% "ridge")[1L]
  if (identical(type, "ridge")) {
    tau2 <- as.numeric(beta_prior_obj$hypers$tau2 %||% 1e4)[1L]
    if (!is.finite(tau2) || tau2 <= 0) stop("ridge tau2 must be positive.", call. = FALSE)
    return(rep(1 / tau2, p))
  }
  if (identical(type, "rhs_ns")) {
    return(.exal_mcmc_rhs_ns_precisions(state, p = p))
  }
  stop("RQR currently supports beta_prior_obj$type in {'ridge','rhs_ns'}.", call. = FALSE)
}

.rqr_prior_state_init <- function(beta_prior_obj, p, init_state = NULL) {
  type <- as.character(beta_prior_obj$type %||% "ridge")[1L]
  if (identical(type, "ridge")) return(list())
  if (identical(type, "rhs_ns")) {
    st <- .exal_mcmc_rhs_ns_prepare_state(beta_prior_obj, p = p, init = list(beta_prior_state = init_state))
    return(st)
  }
  stop("RQR currently supports beta_prior_obj$type in {'ridge','rhs_ns'}.", call. = FALSE)
}

.rqr_prior_state_update <- function(beta_prior_obj, state, beta, freeze_tau = FALSE) {
  type <- as.character(beta_prior_obj$type %||% "ridge")[1L]
  if (identical(type, "ridge")) {
    return(list(state = state, stats = list()))
  }
  if (identical(type, "rhs_ns")) {
    return(.exal_mcmc_rhs_ns_gibbs_update(state, beta, beta_prior_obj, freeze_tau = freeze_tau))
  }
  stop("RQR currently supports beta_prior_obj$type in {'ridge','rhs_ns'}.", call. = FALSE)
}

.rqr_beta_update <- function(y, X, beta_other, V, constants, prior_prec,
                             precision_beta_cfg = list(), context = list()) {
  eta_other <- drop(X %*% beta_other)
  A <- X * as.numeric(y - eta_other)
  z <- y^2 - y * eta_other - constants$xi * V
  W <- 1 / (constants$phi * constants$sigma * V)
  Prec <- crossprod(A * sqrt(W)) + diag(as.numeric(prior_prec), ncol(X))
  rhs <- crossprod(A, W * z)
  .exal_mcmc_sample_mvnorm_prec(
    rhs = as.numeric(rhs),
    Prec = Prec,
    precision_beta_cfg = precision_beta_cfg,
    context = context
  )
}

.rqr_precision_mean <- function(Prec, rhs) {
  Uc <- chol((Prec + t(Prec)) / 2)
  as.numeric(backsolve(Uc, forwardsolve(t(Uc), rhs)))
}

.rqr_fit_summary <- function(y, X, beta1_draws, beta2_draws) {
  eta1 <- X %*% t(beta1_draws)
  eta2 <- X %*% t(beta2_draws)
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  lower_mean <- rowMeans(lower)
  upper_mean <- rowMeans(upper)
  covered_by_draw <- sweep(lower, 1L, y, `<=`) & sweep(upper, 1L, y, `>=`)
  coverage_by_draw <- colMeans(covered_by_draw)
  list(
    beta_root1_mean = colMeans(beta1_draws),
    beta_root2_mean = colMeans(beta2_draws),
    lower_mean = lower_mean,
    upper_mean = upper_mean,
    midpoint_mean = rowMeans(0.5 * (lower + upper)),
    width_mean = rowMeans(upper - lower),
    coverage_posterior_mean_endpoints = mean(y >= lower_mean & y <= upper_mean),
    coverage_draw_mean = mean(coverage_by_draw),
    coverage_draw_quantiles = stats::quantile(
      coverage_by_draw, c(0.05, 0.5, 0.95), names = TRUE, type = 8
    ),
    width_mean_scalar = mean(rowMeans(upper - lower))
  )
}

#' Draw posterior beta samples from an RQR fit
#'
#' @param object An RQR fit object.
#' @param nd Number of draws. `NULL` keeps all available MCMC draws.
#' @param seed Optional RNG seed.
#' @param ... Reserved.
#' @return A list with `beta_root1`, `beta_root2`, and `nd`.
#' @export
rqr_posterior_draws <- function(object, nd = NULL, seed = NULL, ...) {
  UseMethod("rqr_posterior_draws")
}

#' Predict RQR intervals
#'
#' @param object An RQR fit object.
#' @param ... Method-specific arguments.
#' @return A list of interval draws and summaries.
#' @export
predict_interval <- function(object, ...) {
  UseMethod("predict_interval")
}
