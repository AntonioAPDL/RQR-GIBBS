#' rqrgibbs: generalized Bayes interval regression
#'
#' The package implements fixed-design and dynamic samplers for the RQR
#' residual-product loss. Its pseudo-AL representation is a computational
#' augmentation of a loss kernel, not a response likelihood.
#'
#' @section Ordinary version-1 interfaces:
#'
#' * Fixed-design regression uses [rqr_beta_prior()], [rqr_mcmc_fit()],
#'   [rqr_mcmc_continue()], and [predict_interval()].
#' * Frozen-feature RQR-DESN uses [rqr_desn_design()], [rqr_desn_fit()],
#'   [rqr_desn_continue()], and [forecast_paths()]. The feature contract is
#'   fixed before the readout update.
#' * Dynamic interval roots use [rqr_polytrend()], [rqr_seasonal()],
#'   [rqr_regression()], [rqr_dlm_fit()], [rqr_dlm_continue()], and
#'   [rqr_forecast_roots()]. Low-level state calculations are available through
#'   [rqr_ffbs_smooth()] and [rqr_ffbs_sample()].
#' * Loss and population-oracle utilities include [rqr_check_loss()],
#'   [rqr_residual_product()], and [rqr_oracle_certificate()].
#'
#' Every fitted value above describes interval-root functionals under a
#' generalized-Bayes loss update. No interface supplies posterior-predictive
#' response draws.
#'
#' @section Compatibility and experimental code:
#'
#' [beta_prior()] is a compatibility wrapper for [rqr_beta_prior()]; it is not
#' the preferred constructor for new code. [rqr_evolution_adaptive_working()]
#' is an exported experimental working recursion and is not an exact
#' fixed-joint ordinary-v1 target. The unexported `rqr_vb_fit()` implementation
#' is an experimental research prototype outside ordinary-v1 promotion.
#' Nonzero mean tilt, CAVI/ELBO, and response simulation are outside this
#' version-1 package surface.
#'
#' See `vignette("ordinary-rqr-v1", package = "rqrgibbs")` for a compact
#' executable introduction.
#'
#' @keywords internal
#' @useDynLib rqrgibbs, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"

utils::globalVariables(c("qdesn_fit_vb"))
