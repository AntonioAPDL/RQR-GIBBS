#' Canonical MPI and MTI loss API
#'
#' These functions provide the MPI/MTI terminology used by the manuscript.
#' The legacy `rqr_*` functions remain available for reproducibility during
#' the pre-1.0 migration period.
#'
#' @param y Response vector.
#' @param eta1,eta2 Numeric root ordinates.
#' @param content Fixed interval content in `(0, 1)`.
#' @param mean_tilt Fixed response-scale retained-mean tilt.
#' @param details If `TRUE`, return component details.
#' @param ... Arguments passed to the legacy implementation.
#' @return Loss values, fitted objects, draws, or helper objects matching the
#'   corresponding legacy implementation.
#' @name mpi_mti_api
NULL

.mti_content <- function(content, coverage_level = NULL) {
  if (missing(content) || is.null(content)) {
    if (is.null(coverage_level)) {
      stop("content must be supplied.", call. = FALSE)
    }
    content <- coverage_level
  }
  content
}

#' @rdname mpi_mti_api
#' @export
interval_check_loss <- function(u, content = NULL, coverage_level = NULL) {
  rqr_check_loss(u, .mti_content(content, coverage_level))
}

#' @rdname mpi_mti_api
#' @export
interval_residual_product <- function(y, eta1, eta2) {
  rqr_residual_product(y, eta1, eta2)
}

#' @rdname mpi_mti_api
#' @export
mti_constants <- function(content = NULL, learning_rate = 1,
                          coverage_level = NULL) {
  rqr_constants(.mti_content(content, coverage_level), learning_rate)
}

#' @rdname mpi_mti_api
#' @export
mti_loss <- function(y, eta1, eta2, content = NULL, mean_tilt = 0,
                     details = FALSE, coverage_level = NULL) {
  rqr_mean_tilt_loss(
    y = y,
    eta1 = eta1,
    eta2 = eta2,
    coverage_level = .mti_content(content, coverage_level),
    mean_tilt = mean_tilt,
    details = details
  )
}

#' @rdname mpi_mti_api
#' @export
mpi_loss <- function(y, eta1, eta2, content = NULL, details = FALSE,
                     coverage_level = NULL) {
  mti_loss(
    y = y,
    eta1 = eta1,
    eta2 = eta2,
    content = .mti_content(content, coverage_level),
    mean_tilt = 0,
    details = details
  )
}

#' @rdname mpi_mti_api
#' @export
mti_pseudo_residual <- function(y, eta1, eta2) {
  rqr_pseudo_residual(y, eta1, eta2)
}

#' @rdname mpi_mti_api
#' @export
mti_gig_params <- function(e, content = NULL, learning_rate = 1,
                           coverage_level = NULL) {
  rqr_gig_params(e, .mti_content(content, coverage_level), learning_rate)
}

#' @rdname mpi_mti_api
#' @export
mti_sample_gig_half <- function(b, a) {
  rqr_sample_gig_half(b, a)
}

#' @rdname mpi_mti_api
#' @export
mti_mcmc_fit <- function(y, X, content = NULL, ..., coverage_level = NULL) {
  rqr_mcmc_fit(y, X, coverage_level = .mti_content(content, coverage_level), ...)
}

#' @rdname mpi_mti_api
#' @export
mti_ecm_fit <- function(y, X, content = NULL, ..., coverage_level = NULL) {
  rqr_ecm_fit(y, X, coverage_level = .mti_content(content, coverage_level), ...)
}

#' @rdname mpi_mti_api
#' @export
mti_ecm_dp_profile_action <- function(...) rqr_mti_ecm_dp_profile_action(...)

#' @rdname mpi_mti_api
#' @export
mti_ecm_path <- function(...) rqr_ecm_path(...)

#' @rdname mpi_mti_api
#' @export
mti_vb_fit <- function(y, X, content = NULL, ..., coverage_level = NULL) {
  rqr_vb_fit(y, X, coverage_level = .mti_content(content, coverage_level), ...)
}

#' @rdname mpi_mti_api
#' @export
mti_regression <- function(...) rqr_regression(...)

#' @rdname mpi_mti_api
#' @export
mti_desn_fit <- function(y, content = NULL, ..., coverage_level = NULL) {
  rqr_desn_fit(y, coverage_level = .mti_content(content, coverage_level), ...)
}

#' @rdname mpi_mti_api
#' @export
mti_as_dlm_model <- function(model) {
  rqr_as_dlm_model(model)
}

#' @rdname mpi_mti_api
#' @export
mti_dlm_fit <- function(model, ...) rqr_dlm_fit(model, ...)

#' @rdname mpi_mti_api
#' @export
mti_dlm_continue <- function(object, ...) rqr_dlm_continue(object, ...)

#' @rdname mpi_mti_api
#' @export
mti_forecast_roots <- function(...) rqr_forecast_roots(...)

#' @rdname mpi_mti_api
#' @export
mti_evolution_fixed <- function(...) rqr_evolution_fixed(...)

#' @rdname mpi_mti_api
#' @export
mti_evolution_component_scale <- function(...) rqr_evolution_component_scale(...)

#' @rdname mpi_mti_api
#' @export
mti_evolution_adaptive_working <- function(...) rqr_evolution_adaptive_working(...)

#' @rdname mpi_mti_api
#' @export
mti_freeze_discount_template <- function(...) rqr_freeze_discount_template(...)

#' @rdname mpi_mti_api
#' @export
mti_ffbs_sample <- function(...) rqr_ffbs_sample(...)

#' @rdname mpi_mti_api
#' @export
mti_ffbs_smooth <- function(...) rqr_ffbs_smooth(...)

#' @rdname mpi_mti_api
#' @export
interval_order_endpoints <- function(eta1, eta2) {
  rqr_order_endpoints(eta1, eta2)
}

#' @rdname mpi_mti_api
#' @export
interval_canonicalize_root_draws <- function(...) {
  rqr_canonicalize_root_draws(...)
}

#' @rdname mpi_mti_api
#' @export
interval_canonicalize_root_paths <- function(...) {
  rqr_canonicalize_root_paths(...)
}

#' @rdname mpi_mti_api
#' @export
mti_cf_constant <- function(...) rqr_mt_cf_constant(...)

#' @rdname mpi_mti_api
#' @export
mti_tilt_cf <- function(...) rqr_mt_tilt_cf(...)

#' @rdname mpi_mti_api
#' @export
mti_tilt_empirical_shortest <- function(...) {
  rqr_mt_tilt_empirical_shortest(...)
}

#' @rdname mpi_mti_api
#' @export
mti_tilt_empirical_equal_tailed <- function(...) {
  rqr_mt_tilt_empirical_equal_tailed(...)
}

#' @rdname mpi_mti_api
#' @export
mti_tilt_screen <- function(...) rqr_mt_tilt_screen(...)

#' @rdname mpi_mti_api
#' @export
mti_select_tilt_candidate <- function(...) rqr_mt_select_tilt_candidate(...)

#' @rdname mpi_mti_api
#' @export
mti_interval_oracle <- function(...) rqr_interval_oracle(...)

#' @rdname mpi_mti_api
#' @export
mti_interval_oracle_endpoints <- function(...) rqr_interval_oracle_endpoints(...)

#' @rdname mpi_mti_api
#' @export
mti_oracle_tilted_risk <- function(...) rqr_oracle_tilted_risk(...)

#' @rdname mpi_mti_api
#' @export
mti_oracle_risk <- function(...) rqr_oracle_risk(...)

#' @rdname mpi_mti_api
#' @export
mpi_oracle_risk <- function(...) rqr_oracle_risk(...)

#' @rdname mpi_mti_api
#' @export
mti_oracle_endpoints <- function(...) rqr_oracle_endpoints(...)

#' @rdname mpi_mti_api
#' @export
mti_oracle_roots <- function(...) rqr_oracle_roots(...)

#' @rdname mpi_mti_api
#' @export
mti_oracle_conditional_content <- function(...) {
  rqr_oracle_conditional_content(...)
}

#' @rdname mpi_mti_api
#' @export
mti_oracle_certificate <- function(...) rqr_oracle_certificate(...)

#' @rdname mpi_mti_api
#' @export
tcsp_scan_distribution <- function(...) rqr_tcsp_scan_distribution(...)

#' @rdname mpi_mti_api
#' @export
tcsp_scan_cdf_band <- function(...) rqr_tcsp_scan_cdf_band(...)

#' @rdname mpi_mti_api
#' @export
tcsp_scan_probability <- function(...) rqr_tcsp_scan_probability(...)

#' @rdname mpi_mti_api
#' @export
tcsp_scan_count <- function(...) rqr_tcsp_scan_count(...)

#' @rdname mpi_mti_api
#' @export
tcsp_calibrate_count <- function(...) rqr_tcsp_calibrate_count(...)

#' @rdname mpi_mti_api
#' @export
tcsp_calibration_boundary_map <- function(...) {
  rqr_tcsp_calibration_boundary_map(...)
}

#' @rdname mpi_mti_api
#' @export
tcsp_calibration_stability <- function(...) {
  rqr_tcsp_calibration_stability(...)
}

#' @rdname mpi_mti_api
#' @export
tcsp_shortest_window <- function(...) rqr_tcsp_shortest_window(...)

#' @rdname mpi_mti_api
#' @export
tcsp_tilt_from_window <- function(...) rqr_tcsp_tilt_from_window(...)

#' @rdname mpi_mti_api
#' @export
tcsp_mti_boundary_target <- function(...) rqr_tcsp_mti_boundary_target(...)

#' @rdname mpi_mti_api
#' @export
tcsp_fractional_tilt <- function(...) rqr_tcsp_fractional_tilt(...)

#' @rdname mpi_mti_api
#' @export
tcsp_predict_next_start <- function(...) rqr_tcsp_predict_next_start(...)

#' @rdname mpi_mti_api
#' @export
tcsp_local_correct <- function(...) rqr_tcsp_local_correct(...)

#' @rdname mpi_mti_api
#' @export
tcsp_tolerance_path <- function(...) rqr_tcsp_path(...)

#' @rdname mpi_mti_api
#' @export
mti_shortest_path <- function(...) rqr_tcsp_path(...)

#' @rdname mpi_mti_api
#' @export
tcsp_fit_univariate <- function(...) rqr_tcsp_fit_univariate(...)

#' @rdname mpi_mti_api
#' @export
tcsp_plugin_mti_fit <- function(...) rqr_tcsp_plugin_fit_univariate(...)

#' @rdname mpi_mti_api
#' @export
tcsp_hybrid_bayes_fit <- function(...) rqr_tcsp_hybrid_bayes_fit(...)

#' @rdname mpi_mti_api
#' @export
tcsp_exact_spacing_gap <- function(...) rqr_tcsp_exact_spacing_gap(...)

#' @rdname mpi_mti_api
#' @export
tcsp_split_exact_fit <- function(...) rqr_tcsp_split_exact_fit(...)

#' @rdname mpi_mti_api
#' @export
tcsp_validate_action <- function(...) rqr_tcsp_validate_action(...)

#' @rdname mpi_mti_api
#' @export
dp_base_normal <- function(...) rqr_dp_base_normal(...)

#' @rdname mpi_mti_api
#' @export
dp_base_student_t <- function(...) rqr_dp_base_student_t(...)

#' @rdname mpi_mti_api
#' @export
dp_base_empirical_normal <- function(...) rqr_dp_base_empirical_normal(...)

#' @rdname mpi_mti_api
#' @export
dp_fit <- function(...) rqr_dp_fit(...)

#' @rdname mpi_mti_api
#' @export
dp_draws <- function(...) rqr_dp_draws(...)

#' @rdname mpi_mti_api
#' @export
dp_content_probability <- function(...) rqr_dp_content_probability(...)

#' @rdname mpi_mti_api
#' @export
dp_shortest_draws <- function(...) rqr_dp_shortest_draws(...)

#' @rdname mpi_mti_api
#' @export
dp_bayes_tolerance_action <- function(...) rqr_dp_bayes_tolerance_action(...)

#' @rdname mpi_mti_api
#' @export
dpm_fit <- function(...) rqr_dpm_fit(...)

#' @rdname mpi_mti_api
#' @export
dpm_ecm_fit <- function(...) rqr_dpm_ecm_fit(...)

#' @rdname mpi_mti_api
#' @export
dpm_density <- function(...) rqr_dpm_density(...)

#' @rdname mpi_mti_api
#' @export
dpm_cdf <- function(...) rqr_dpm_cdf(...)

#' @rdname mpi_mti_api
#' @export
dpm_quantile <- function(...) rqr_dpm_quantile(...)

#' @rdname mpi_mti_api
#' @export
dpm_content_probability <- function(...) rqr_dpm_content_probability(...)

#' @rdname mpi_mti_api
#' @export
dpm_shortest_draws <- function(...) rqr_dpm_shortest_draws(...)

#' @rdname mpi_mti_api
#' @export
dpm_bayes_tolerance_action <- function(...) rqr_dpm_bayes_tolerance_action(...)

#' @rdname mpi_mti_api
#' @export
bayesian_bootstrap_shortest_draws <- function(...) {
  rqr_bayesian_bootstrap_draws(...)
}

#' @rdname mpi_mti_api
#' @export
weighted_shortest_interval <- function(...) rqr_weighted_shortest_interval(...)

#' @rdname mpi_mti_api
#' @export
mti_posterior_draws <- function(object, nd = NULL, seed = NULL, ...) {
  UseMethod("mti_posterior_draws")
}

#' @export
mti_posterior_draws.mti_mcmc <- function(object, nd = NULL, seed = NULL, ...) {
  rqr_posterior_draws.rqr_mcmc(object, nd = nd, seed = seed, ...)
}

#' @export
mti_posterior_draws.rqr_mcmc <- mti_posterior_draws.mti_mcmc

#' @export
mti_posterior_draws.mti_vb <- function(object, nd = NULL, seed = NULL, ...) {
  rqr_posterior_draws.rqr_vb(object, nd = nd, seed = seed, ...)
}

#' @export
mti_posterior_draws.rqr_vb <- mti_posterior_draws.mti_vb

#' @export
mti_posterior_draws.mti_desn_fit <- function(object, nd = NULL,
                                             seed = NULL, ...) {
  rqr_posterior_draws.rqr_desn_fit(object, nd = nd, seed = seed, ...)
}

#' @export
mti_posterior_draws.rqr_desn_fit <- mti_posterior_draws.mti_desn_fit

#' @export
mti_posterior_draws.mti_dlm_mcmc <- function(object, nd = NULL,
                                             seed = NULL, ...) {
  rqr_posterior_draws.rqr_dlm_mcmc(object, nd = nd, seed = seed, ...)
}

#' @export
mti_posterior_draws.rqr_dlm_mcmc <- mti_posterior_draws.mti_dlm_mcmc
