\name{rqr_mti_intercept_fit}
\alias{rqr_mti_intercept_fit}
\alias{rqr_mti_empirical_tilt_bounds}
\alias{rqr_mti_fractional_window}
\alias{rqr_mti_dp_profile}
\title{Profiled Intercept MTI Estimates and Fractional Empirical Targets}
\description{Opt-in fixed-target minimization of the equal-Gaussian-precision
intercept MTI criterion, coherent fractional empirical targets, and candidate
selection using a separate direct Dirichlet-process posterior content check.}
\usage{
rqr_mti_intercept_fit(y, target_content, mean_tilt = 0,
  learning_rate = 1, prior_precision = 1e-4, prior_mean = 0,
  backend = c("cpp", "R"), control = list())
rqr_mti_empirical_tilt_bounds(y, target_content)
rqr_mti_fractional_window(y, target_content,
  allocation = c("balanced", "left", "right"))
rqr_mti_dp_profile(y, content, posterior_confidence, q_grid = NULL,
  tilt_rule = c("fractional_admissible", "historical"),
  tilt_positions = seq(0, 1, length.out = 7L), allocation = "balanced",
  historical_tilt_control = list(), learning_rate = 1,
  prior_precision = 1e-4, prior_mean = 0, dp_concentration = 1,
  dp_base_measure = rqr_dp_base_normal(mean = 0, sd = 4),
  backend = c("cpp", "R"), control = list(), return_candidate_traces = FALSE)
}
\arguments{
\item{y}{Finite univariate observations.}
\item{target_content}{Fixed fitted content in (0,1).}
\item{mean_tilt}{Fixed scalar response-scale tilt.}
\item{learning_rate}{Positive fixed loss learning rate.}
\item{prior_precision, prior_mean}{Common positive Gaussian root precision and fixed mean.}
\item{backend}{R reference or C++ calculation.}
\item{control}{Numerical controls: max_iter, parameter_tolerance,
objective_tolerance, store_trace, and fail_on_nonconvergence.}
\item{allocation}{Fractional endpoint-mass allocation on the first shortest support.}
\item{content}{Required population content.}
\item{posterior_confidence}{Required DP posterior interval-content probability.}
\item{q_grid}{Prespecified fitted contents strictly between content and one.}
\item{tilt_rule}{Empirically admissible fractional targets or historical targets for diagnosis.}
\item{tilt_positions}{Prespecified relative positions in the empirical tilt range.}
\item{historical_tilt_control}{Controls passed to the historical diagnostic tilt menu.}
\item{dp_concentration, dp_base_measure}{Fixed parameters of the separate DP response model.}
\item{return_candidate_traces}{Retain the midpoint trace for every candidate.}
}
\details{The fixed-target calculation minimizes the original endpoint criterion
by profiling the squared half-width. It returns a direct objective evaluation
and a global lower bound from fractional weights. Numerical accuracy is
assessed against that bound. The transformation is an optimization device;
it does not define a transformed-density mode.

Fractional membership mass differs from the integer count in the reported
closed interval. The latter is used in the DP posterior calculation.
The new selection rule inherits no historical ECM calibration or frequentist
tolerance guarantee. No clipping or replacement interval is applied when a
candidate fails or no eligible candidate exists.}
\value{A list containing the relevant bounds, fractional weights, fitted
endpoints, numerical diagnostics, or all candidate and selected-interval results.}
\seealso{\code{\link{rqr_ecm_fit}}, \code{\link{rqr_dp_content_probability}}}
