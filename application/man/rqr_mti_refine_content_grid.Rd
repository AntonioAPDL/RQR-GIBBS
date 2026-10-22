\name{rqr_mti_refine_content_grid}
\alias{rqr_mti_refine_content_grid}
\title{Refine an MTI Fitted-Content Grid by Effective Observation Mass}
\usage{
rqr_mti_refine_content_grid(q_grid, sample_size, content,
  learning_rate = 1, prior_precision = 1e-4, max_added_points = NULL)
}
\arguments{
\item{q_grid}{Existing fitted-content values strictly between content and one.}
\item{sample_size}{Number of observations, at least two.}
\item{content}{Requested population content in (0,1).}
\item{learning_rate}{Positive fixed loss learning rate.}
\item{prior_precision}{Common positive Gaussian root precision.}
\item{max_added_points}{Optional nonnegative integer budget; exceeding it stops
without returning a partially refined grid.}
}
\description{Preserves supplied targets and adds one midpoint for each
unrepresented positive effective-mass cell within their range. No default
fitting method, calibration rule, or returned interval is changed.}
\details{The effective mass is \eqn{s=nq-\lambda/\omega}. A positive mass
\eqn{s\in(j,j+1]} selects radius rank \eqn{j+1}; at an integer plateau the
fixed-target solver uses the lower minimizing radius. An unrepresented cell
receives \eqn{q=(j+1/2+\lambda/\omega)/n}. Original targets are retained
exactly, including any collapsed-mass targets. This finite refinement is
not a continuous-content or continuous-tilt optimum. The resulting selection
rule requires its own calibration and independent repeated-sampling evaluation.}
\value{A list containing original and refined grids, added values, effective-mass
cell diagnostics, controls, and explicit limits on interpretation.}
\examples{
grid <- rqr_mti_refine_content_grid(c(0.93, 0.995), 50, 0.90)
grid$q_grid
grid$cells
}
