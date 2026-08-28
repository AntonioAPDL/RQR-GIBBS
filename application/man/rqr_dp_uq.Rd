% Package documentation for Bayesian shortest-interval summaries.
\name{rqr_dp_fit}
\alias{rqr_dp_fit}
\alias{rqr_dp_base_normal}
\alias{rqr_dp_base_student_t}
\alias{rqr_dp_base_empirical_normal}
\alias{rqr_dp_content_probability}
\alias{rqr_dp_draws}
\alias{rqr_bayesian_bootstrap_draws}
\alias{rqr_dp_shortest_draws}
\alias{rqr_dp_bayes_tolerance_action}
\alias{rqr_weighted_shortest_interval}
\title{Direct Dirichlet-process shortest-interval uncertainty quantification}
\usage{
rqr_dp_base_normal(mean = 0, sd = 1)

rqr_dp_base_student_t(location = 0, scale = 1, df = 5)

rqr_dp_base_empirical_normal(y, scale_multiplier = 2, na_rm = FALSE)

rqr_dp_fit(y, concentration, base_measure, strict_bayes = TRUE, na_rm = FALSE)

rqr_dp_content_probability(fit, lower, upper, content)

rqr_dp_draws(
  fit,
  n_draws,
  residual_mass_tol = 1e-4,
  seed = NULL,
  max_atoms = 20000L,
  parallel = FALSE
)

rqr_bayesian_bootstrap_draws(y, n_draws, seed = NULL, na_rm = FALSE)

rqr_dp_shortest_draws(draws, target_content, tolerance = 1e-12)

rqr_dp_bayes_tolerance_action(
  fit,
  content,
  posterior_confidence,
  action_class = "closed_order_statistic_intervals"
)

rqr_weighted_shortest_interval(x, weights, target_content, tolerance = 1e-12)
}
\arguments{
\item{mean, sd}{Mean and standard deviation of the Gaussian base measure.}
\item{location, scale, df}{Location, scale, and degrees of freedom of the
Student-\eqn{t} base measure.}
\item{y}{Numeric univariate response sample.}
\item{scale_multiplier}{Multiplier applied to the empirical standard
deviation in the empirical-Bayes Gaussian base measure.}
\item{na_rm}{Remove missing responses.}
\item{concentration}{Positive Dirichlet-process concentration parameter.}
\item{base_measure}{Base measure object returned by a \code{rqr_dp_base_*()}
constructor or a compatible object.}
\item{strict_bayes}{If \code{TRUE}, reject data-dependent base measures.}
\item{fit}{A fit from \code{rqr_dp_fit()}.}
\item{lower, upper}{Closed interval endpoints.}
\item{content}{Required population content.}
\item{n_draws}{Number of posterior distribution draws.}
\item{residual_mass_tol}{Residual stick-breaking mass tolerance for direct-DP
draws.}
\item{seed}{Optional random seed.}
\item{max_atoms}{Maximum atoms allowed in one truncated stick-breaking draw.}
\item{parallel}{Reserved; the current reference path is serial.}
\item{draws}{Distribution draws from \code{rqr_dp_draws()} or
\code{rqr_bayesian_bootstrap_draws()}.}
\item{target_content}{Content used for shortest-interval functionals.}
\item{tolerance}{Numerical tolerance.}
\item{posterior_confidence}{Required posterior probability that interval
content is at least \code{content}.}
\item{action_class}{Candidate interval class.  Currently only closed
order-statistic intervals are implemented.}
\item{x}{Numeric support atoms for a weighted discrete distribution.}
\item{weights}{Nonnegative atom weights.}
}
\description{
Fits the conjugate direct Dirichlet-process response-distribution model
\eqn{F ~ DP(a,H)}, \eqn{Y_i | F ~ F}, evaluates exact fixed-interval
content probabilities, and summarizes posterior shortest-interval functionals.
}
\details{
For a fixed closed interval \eqn{I=[l,u]}, conjugacy gives
\deqn{F(I) | y ~ Beta(a H(I) + N_n(I), a (1 - H(I)) + n - N_n(I)).}
\code{rqr_dp_content_probability()} evaluates this law exactly.  Candidate
actions search closed order-statistic intervals and use deterministic
first-minimum tie breaking.

This is a response-likelihood model for the full distribution \eqn{F}.  It is
not an RQR generalized-Bayes loss update.  The Bayesian bootstrap helper is
included as a diagnostic limiting comparator; it is not labeled as a proper
direct-DP response model with a fixed prior.
}
\value{
Schema-versioned fit, draw, action, or data-frame objects with provenance
information.  The direct-DP fit sets \code{response_likelihood = TRUE} and
\code{generalized_bayes = FALSE}.
}
\seealso{\code{\link{rqr_tcsp_hybrid_bayes_fit}},
\code{\link{rqr_tcsp_fit_univariate}}}
