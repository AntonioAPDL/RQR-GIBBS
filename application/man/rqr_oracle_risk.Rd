\name{rqr_oracle_risk}
\alias{rqr_oracle_risk}
\title{Population RQR risk}
\usage{
rqr_oracle_risk(
  lower,
  upper,
  family,
  coverage_level,
  params = list(),
  tol = 1e-09
)
}
\arguments{
\item{lower, upper}{Ordered finite interval roots.}
\item{family}{Supported innovation family.}
\item{coverage_level}{Target interval coverage in \code{(0,1)}.}
\item{params}{Optional family parameters.}
\item{tol}{Numerical integration tolerance.}
}
\value{
A list with population risk and an estimated absolute quadrature error. The
error is a numerical estimate, not a rigorous bound.
}
\description{
Evaluates the expected check loss of the residual product under a supported
innovation law.
}
