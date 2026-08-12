\name{rqr_tcsp_split_exact_fit}
\alias{rqr_tcsp_split_exact_fit}
\alias{rqr_tcsp_exact_spacing_gap}
\title{Pilot-selected exact-spacing TCSP}
\usage{
rqr_tcsp_exact_spacing_gap(main_n, guaranteed_content, tolerance_confidence)

rqr_tcsp_split_exact_fit(
  y,
  guaranteed_content,
  tolerance_confidence,
  pilot_fraction,
  pilot_method = c("empirical_shortest", "ecm_fixed_tilt", "ecm_profile",
    "cornish_fisher"),
  split_seed,
  pilot_args = list(),
  ecm_args = list(),
  na_rm = FALSE
)
}
\arguments{
\item{main_n}{Main-sample size.}
\item{guaranteed_content}{Minimum population content \eqn{c}.}
\item{tolerance_confidence}{Repeated-sample tolerance confidence.}
\item{y}{Response sample.}
\item{pilot_fraction}{Fraction of observations assigned to the independent
pilot sample.}
\item{pilot_method}{Pilot placement method.}
\item{split_seed}{Integer seed fixing the pilot/main split before fitting.}
\item{pilot_args}{Optional pilot controls.}
\item{ecm_args}{Optional ECM controls for ECM pilot methods.  Target-defining
arguments are reserved by the wrapper.}
\item{na_rm}{Remove nonfinite observations before splitting.}
}
\description{
\code{rqr_tcsp_exact_spacing_gap()} chooses the smallest spacing gap
\eqn{d=s-r} whose Beta survival probability
\eqn{P\{Beta(d,N+1-d)\ge c\}} reaches the requested confidence.

\code{rqr_tcsp_split_exact_fit()} uses an independent pilot sample to choose a
lower-tail placement and an independent main sample to form the fixed
order-statistic interval \eqn{[Z_{(\hat r)}, Z_{(\hat r+d)}]}.
}
\details{
The indexing convention is explicit: \eqn{d=s-r} is the probability-spacing
parameter, while the closed order-statistic interval contains \eqn{d+1}
observed positions.  Conditional on the independent pilot-selected placement,
the main-sample spacing has the recorded exact Beta law under iid continuity.
The first implementation is continuous-iid-univariate and intercept-only for
the formal action.
}
\value{
A calibration list for \code{rqr_tcsp_exact_spacing_gap()}, or an object of
class \code{c("rqr_tcsp_split_exact_fit", "list")} for
\code{rqr_tcsp_split_exact_fit()}.
}
\seealso{
\code{\link{rqr_tcsp_fit_univariate}}, \code{\link{rqr_ecm_fit}}
}
