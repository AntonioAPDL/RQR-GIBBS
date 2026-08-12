\name{rqr_ecm_fit}
\alias{rqr_ecm_fit}
\alias{rqr_ecm_path}
\alias{predict_interval.rqr_ecm}
\alias{coef.rqr_ecm}
\alias{print.rqr_ecm}
\alias{summary.rqr_ecm}
\title{Fixed-target mean-tilted RQR ECM}
\usage{
rqr_ecm_fit(
  y,
  X,
  coverage_level,
  learning_rate = 1,
  mean_tilt = 0,
  beta_prior_obj = NULL,
  ecm_control = list(),
  init = list(),
  ...
)

rqr_ecm_path(
  y,
  X,
  target_contents,
  mean_tilts = 0,
  learning_rate = 1,
  beta_prior_obj = NULL,
  ecm_control = list(),
  warm_start = TRUE,
  global_multistart_check_every = 1L
)
}
\arguments{
\item{y}{Numeric response vector.}
\item{X}{Fixed design matrix.}
\item{coverage_level}{Fixed target content \eqn{q} in \eqn{(0,1)}.}
\item{learning_rate}{Positive generalized-Bayes learning rate.}
\item{mean_tilt}{Fixed response-scale mean tilt, either scalar or length
\code{length(y)}.}
\item{beta_prior_obj}{Optional beta prior object from \code{beta_prior()}.
The ECM backend currently supports ridge roots only.}
\item{ecm_control}{Named list controlling iterations, residual-product
safeguards, monotone backtracking, deterministic multi-start, and convergence
tolerances.}
\item{init}{Optional initial root coefficients or a \code{previous_fit} for
warm starts.}
\item{...}{Reserved.}
\item{target_contents}{Vector of fixed contents for \code{rqr_ecm_path()}.}
\item{mean_tilts}{Scalar, row-specific vector, per-target vector, or list of
fixed tilts for \code{rqr_ecm_path()}.}
\item{warm_start}{Use the previous path fit as an initialization.}
\item{global_multistart_check_every}{Frequency for full multi-start checks
along the path.}
}
\description{
\code{rqr_ecm_fit()} computes a deterministic generalized-posterior mode for a
fixed RQR content, fixed response-scale tilt, and fixed learning rate.  The
algorithm uses an expectation/conditional-maximization structure: the E-step
uses the exact inverse latent-scale moment
\eqn{E(V_i^{-1}\mid\cdot)=1/[q(1-q)|e_i|]} away from zero residual products,
and the two CM steps solve weighted Gaussian root systems.
}
\details{
The ECM objective is the negative log generalized-posterior kernel defined by
\code{rqr_mean_tilt_loss()} plus ridge penalties.  The implementation uses a
declared residual-product safeguard and exact observed-objective
backtracking.  ECM is not a response-likelihood EM algorithm, does not return
posterior draws, and does not by itself define a tolerance interval.

\code{rqr_ecm_path()} traces fixed \eqn{(q,\delta)} targets.  It does not
perform TCSP scan calibration.
}
\value{
An object of class \code{c("rqr_ecm", "rqr_fit")} for \code{rqr_ecm_fit()},
or \code{c("rqr_ecm_path", "list")} for \code{rqr_ecm_path()}.
}
\seealso{
\code{\link{rqr_mcmc_fit}}, \code{\link{rqr_tcsp_fit_univariate}},
\code{\link{rqr_mean_tilt_loss}}
}
