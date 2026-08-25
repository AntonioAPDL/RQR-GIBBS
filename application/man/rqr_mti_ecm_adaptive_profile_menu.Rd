\name{rqr_mti_ecm_adaptive_profile_menu}
\alias{rqr_mti_ecm_adaptive_profile_menu}
\alias{rqr_delivery_lower_bound}
\alias{rqr_delivery_min_successes}
\title{Adaptive MTI-ECM profile calibration helpers}
\usage{
rqr_mti_ecm_adaptive_profile_menu(
  y,
  content,
  tolerance_confidence,
  scan_target_content = NA_real_,
  policy = c("cell", "global", "diagnostic"),
  policy_config = list(),
  calibration_rule = NULL,
  na_rm = FALSE
)

rqr_delivery_lower_bound(
  successes,
  replications,
  confidence = 0.95,
  method = c("clopper_pearson", "wilson", "jeffreys")
)

rqr_delivery_min_successes(
  replications,
  target,
  confidence = 0.95,
  method = c("clopper_pearson", "wilson", "jeffreys"),
  margin = 0
)
}
\arguments{
\item{y}{Numeric response sample.}
\item{content}{Required population content.}
\item{tolerance_confidence}{Target tolerance confidence.}
\item{scan_target_content}{Optional scan-calibrated retained fraction used as
the fitted-content anchor.}
\item{policy}{Adaptive policy label.}
\item{policy_config}{List of fitted-content grid, screen, tilt-grid, and
direct-DP settings.}
\item{calibration_rule}{Optional frozen calibration rule. When supplied, its
screen, grid, and concentration fields override the default menu.}
\item{na_rm}{Remove missing observations.}
\item{successes}{Number of successful calibration or validation replications.}
\item{replications}{Number of calibration or validation replications.}
\item{confidence}{One-sided confidence level for the lower delivery bound.}
\item{method}{Delivery-bound construction.}
\item{target}{Target repeated-sample delivery probability.}
\item{margin}{Nonnegative margin added to \code{target}.}
}
\value{
\code{rqr_mti_ecm_adaptive_profile_menu()} returns a list with fitted-content
grid, direct-DP screen, tilt-grid controls, direct-DP concentration, observable
sample diagnostics, and a provenance digest. \code{rqr_delivery_lower_bound()}
returns a numeric lower bound. \code{rqr_delivery_min_successes()} returns the
minimum success count implied by the configured calibration rule.
}
\description{
These helpers support calibrated adaptive MTI-ECM tolerance-interval studies.
The menu helper constructs candidate grids and direct-DP screens before the
MTI-ECM endpoint action is fit. The delivery helpers compute calibration
requirements from user-specified confidence-bound settings rather than from a
hard-coded observed-delivery cutoff.
}
\details{
The adaptive MTI-ECM profile menu is a deterministic construction for fixed
inputs. It does not select a final interval and does not by itself provide a
distribution-free tolerance guarantee. The repeated-sample tolerance evidence is
obtained by calibration and validation of the complete rule.
}
\keyword{models}
