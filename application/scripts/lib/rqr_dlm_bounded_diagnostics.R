# Shared promotion-boundary helpers for the bounded RQR-DLM runner.
#
# These functions deliberately live outside the package API. They define the
# exact validation estimands, deterministic future-root functionals, and the
# readback-verified local chain publication contract used by the bounded
# runner. They do not define a response-prediction distribution.

rqr_bounded_estimand_schema_version <- function() {
  "rqrgibbs_dlm_bounded_estimands/1.0.0"
}

rqr_bounded_expected_estimand_names <- function(
    fixture, learning_rate_mode) {
  if (!is.list(fixture) || is.null(fixture$expanded_model) ||
      is.null(fixture$future)) {
    stop("A constructed bounded fixture is required.", call. = FALSE)
  }
  learning_rate_mode <- as.character(learning_rate_mode)[1L]
  if (!learning_rate_mode %in% c(
        "fixed_rate", "learned_pseudoresidual_normalized"
      )) {
    stop("The bounded learning-rate mode is invalid.", call. = FALSE)
  }
  T <- ncol(fixture$expanded_model$FF)
  p <- fixture$expanded_model$p
  H <- fixture$future$H
  indexed <- function(prefix, n, label = "t") {
    sprintf("%s_%s%03d", prefix, label, seq_len(n))
  }
  names <- c(
    indexed("train_lower", T),
    indexed("train_upper", T),
    indexed("train_midpoint", T),
    indexed("train_width", T),
    "observed_loss",
    paste0("terminal_state_midpoint_", seq_len(p)),
    paste0("terminal_state_abs_separation_", seq_len(p)),
    paste0("time0_state_midpoint_", seq_len(p)),
    paste0("time0_state_abs_separation_", seq_len(p)),
    indexed("future_conditional_mean_lower", H),
    indexed("future_conditional_mean_upper", H),
    indexed("future_conditional_mean_midpoint", H),
    indexed("future_conditional_mean_width", H)
  )
  if (identical(
        learning_rate_mode, "learned_pseudoresidual_normalized"
      )) {
    names <- c(names, "log_lambda")
  }
  if (identical(fixture$evolution$mode, "component_scale")) {
    component_names <- fixture$evolution$component_names
    names <- c(
      names,
      paste0("log_component_scale_", component_names),
      paste0("component_innovation_energy_", component_names)
    )
  }
  if (anyDuplicated(names)) {
    stop("The expected bounded estimand schema is duplicated.", call. = FALSE)
  }
  names
}

rqr_bounded_validate_estimand_schemas <- function(
    cell_chains, fixture, learning_rate_mode) {
  if (!is.list(cell_chains) || !length(cell_chains)) {
    stop("At least one bounded estimand matrix is required.", call. = FALSE)
  }
  expected <- rqr_bounded_expected_estimand_names(
    fixture, learning_rate_mode
  )
  valid <- vapply(cell_chains, function(values) {
    is.matrix(values) &&
      nrow(values) > 0L &&
      identical(colnames(values), expected) &&
      !anyDuplicated(colnames(values)) &&
      all(is.finite(values))
  }, logical(1L))
  if (!all(valid)) {
    bad <- which(!valid)
    stop(
      paste0(
        "Bounded estimand schema mismatch in chain(s) ",
        paste(bad, collapse = ", "),
        "; expected schema ",
        rqr_bounded_estimand_schema_version(), " with ",
        length(expected), " ordered estimands."
      ),
      call. = FALSE
    )
  }
  expected
}

rqr_bounded_future_conditional_mean_roots <- function(fit, future) {
  if (!inherits(fit, "rqr_dlm_mcmc") ||
      !is.list(future) || is.null(future$FF) ||
      is.null(future$GG) || is.null(future$H)) {
    stop(
      "A fitted RQR-DLM object and constructed future contract are required.",
      call. = FALSE
    )
  }
  terminal1 <- as.matrix(fit$samp.theta_terminal_root1)
  terminal2 <- as.matrix(fit$samp.theta_terminal_root2)
  if (!identical(dim(terminal1), dim(terminal2)) ||
      any(!is.finite(terminal1)) || any(!is.finite(terminal2))) {
    stop("Terminal root-state draws are incomplete.", call. = FALSE)
  }
  p <- nrow(terminal1)
  n_save <- ncol(terminal1)
  H <- as.integer(future$H)
  FF <- as.matrix(future$FF)
  if (!identical(dim(FF), c(p, H)) || any(!is.finite(FF))) {
    stop("The future observation design is invalid.", call. = FALSE)
  }
  GG <- rqrgibbs:::.rqr_expand_cube(future$GG, H, p, "GG_future")
  eta1 <- eta2 <- matrix(NA_real_, H, n_save)
  state1 <- terminal1
  state2 <- terminal2
  for (horizon in seq_len(H)) {
    transition <- matrix(GG[, , horizon], p, p)
    state1 <- transition %*% state1
    state2 <- transition %*% state2
    eta1[horizon, ] <- drop(crossprod(FF[, horizon], state1))
    eta2[horizon, ] <- drop(crossprod(FF[, horizon], state2))
  }
  lower <- pmin(eta1, eta2)
  upper <- pmax(eta1, eta2)
  list(
    eta_root1 = eta1,
    eta_root2 = eta2,
    lower_draws = lower,
    upper_draws = upper,
    midpoint_draws = 0.5 * (lower + upper),
    width_draws = upper - lower,
    draw_index = seq_len(n_save),
    interpretation = paste(
      "Deterministic future conditional-mean interval-root functionals;",
      "no future process noise and no response simulation contract."
    )
  )
}

rqr_bounded_publish_fit_rds <- function(
    fit, path, compress = "xz", save_function = saveRDS) {
  if (!inherits(fit, "rqr_dlm_mcmc")) {
    stop("Only an rqr_dlm_mcmc fit can be published.", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path)) {
    stop("A single chain path is required.", call. = FALSE)
  }
  if (file.exists(path)) {
    stop("Refusing to overwrite an existing bounded chain.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = dirname(path)
  )
  on.exit(unlink(temporary), add = TRUE)
  save_function(
    fit, temporary, version = 3, compress = compress
  )
  restored <- tryCatch(
    readRDS(temporary),
    error = function(error) {
      stop(
        "The temporary bounded chain failed RDS readback: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  if (!inherits(restored, "rqr_dlm_mcmc") ||
      !identical(restored, fit)) {
    stop(
      "The temporary bounded chain failed class or object-identity readback.",
      call. = FALSE
    )
  }
  checkpoint_digest <- restored$checkpoint_digest
  if (!is.character(checkpoint_digest) ||
      length(checkpoint_digest) != 1L ||
      !grepl("^[0-9a-f]{64}$", checkpoint_digest) ||
      !identical(
        checkpoint_digest,
        rqrgibbs:::.rqr_digest(restored$checkpoint_state)
      )) {
    stop(
      "The temporary bounded chain failed checkpoint-digest validation.",
      call. = FALSE
    )
  }
  rqrgibbs:::.rqr_validate_continuation_history(restored)
  temporary_sha256 <- digest::digest(
    file = temporary, algo = "sha256", serialize = FALSE
  )
  temporary_bytes <- as.numeric(file.info(temporary)$size)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically publish the bounded chain.", call. = FALSE)
  }
  final_sha256 <- digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  )
  final_bytes <- as.numeric(file.info(path)$size)
  if (!identical(final_sha256, temporary_sha256) ||
      !identical(final_bytes, temporary_bytes)) {
    unlink(path)
    stop(
      "The published bounded chain failed post-rename integrity checks.",
      call. = FALSE
    )
  }
  list(
    path = path,
    bytes = final_bytes,
    sha256 = final_sha256,
    checkpoint_digest = checkpoint_digest,
    continuation_history_digest =
      restored$continuation_history_digest,
    object_digest = rqrgibbs:::.rqr_digest(restored)
  )
}
