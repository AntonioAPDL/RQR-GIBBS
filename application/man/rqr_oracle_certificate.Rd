\name{rqr_oracle_certificate}
\alias{rqr_oracle_certificate}
\title{Certified population RQR oracle}
\usage{
rqr_oracle_certificate(
  family,
  coverage_level,
  params = list(),
  tol = 1e-08,
  grid_size = 1601L
)
}
\arguments{
\item{family}{Supported innovation family.}
\item{coverage_level}{Target interval coverage in \code{(0,1)}.}
\item{params}{Optional family parameters.}
\item{tol}{Numerical tolerance.}
\item{grid_size}{Odd number of deterministic profile-grid points.}
}
\value{
A versioned certificate containing independent profile and unrestricted
optimization results, residuals, objective values, curvature, estimated
quadrature error, uniqueness status, and content digests.
}
\description{
Cross-checks a two-dimensional RQR-risk optimization against the independent
coverage-profile parameterization and refines every minimum basin detected on
the deterministic grid.
}
