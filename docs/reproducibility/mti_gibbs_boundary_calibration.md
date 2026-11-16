# Gibbs Tilt-Boundary Assessment and Calibration

This separate study investigates posterior width near the empirical MTI tilt
limits. It preserves the completed fixed-prior article and all its fitted
results. The experiment concerns an alternative Gibbs-derived interval rule;
its results are not incorporated into the manuscript automatically.

## Statistical Specification

The two raw roots retain independent Normal(0,2^2) priors, learning rate one,
and the predeclared response coordinates. The original ECM/DP search supplies
fitted content q and an anchor tilt. Each Gibbs target either retains that
tilt or projects it into the empirical admissible interval using a fractional
margin log(n)/n or 1/sqrt(n). These two margins vanish with n while their
products with n diverge. They motivate a finite comparison of outward-loss
penalties; they do not establish posterior concentration when the interval
between empirical tilt limits also shrinks.

All targets use the existing marginal slice-within-Gibbs calculation. Four
chains discard 500 iterations and initially retain 1000 draws each, increasing
to 2000, 4000, or 8000 only when required by the unchanged mixing and mean-MCSE
criteria. The prior is never estimated from the fitting sample. Known support
of a simulated distribution is used only to diagnose width outside that
support, never to fit the new rule.

The ordinary posterior-mean endpoint interval has midpoint m and half-width
h. A calibrated reported interval is [m-s*h,m+s*h]. Scaling this decision
does not change the generalized posterior or turn the interval into a credible
region. It separately addresses the repeated-sampling content requirement.
This differs from calibrating posterior credible regions by tuning posterior
spread, as studied by [Syring and Martin (2019)](https://doi.org/10.1093/biomet/asy054).
Neither the learning rate nor the posterior spread is tuned here.

## Calibration Calculation

During simulation calibration, the true CDF determines the smallest required
radius multiplier R for content c about m. The fitter never receives this
CDF. Bisection returns an upper feasible bracket for R. For each law and
design, let R_(j) be its jth calibration order statistic from M independent
samples. The usual order-statistic identity gives

`P_calibration{P_future(R <= R_(j)) >= p} >= P{Bin(M,p) <= j-1}`.

Equality holds for continuous required-radius distributions; atoms give a
conservative bound. Choose the smallest feasible j with the right-hand side
at least 1-beta/J, and take the largest R_(j) across the eight laws within
each design. Here beta=.05, J=72, and M=200 give j=199. The union bound
provides a simultaneous calibration assurance over this declared finite
panel, given the development-selected rule and the stated complete numerical
procedure. No assurance over arbitrary distributions or the pharmaceutical
application is claimed. Missing or numerically unresolved calibration fits
stop advancement; they are never dropped or replaced by new datasets.
The calibration uses the classical order-statistic tolerance-limit argument;
see the [tolerance package's order-statistic documentation](https://search.r-project.org/CRAN/refmans/tolerance/html/nporder.html).

## Data Roles

- Audit: 72,000 already completed validation pairs, with reconstructed samples
  and a saved-sample hash check in every distribution/design case. This is a
  retrospective diagnostic analysis, not new validation evidence.
- Development: 864 original SD-two development samples. Original adequate
  chains are reused; changed interior targets are fitted separately. A
  preliminary, descriptive content-radius adjustment compares width tradeoffs.
  One target rule per design is selected using the worst then average log
  width ratio across laws, with a fixed least-change tie order. Ratios are
  development criteria; empirical width tables retain raw, law-specific lengths.
- Calibration: 14,400 fresh samples, 200 per case. Development selection is
  frozen before these samples are used. The algorithm uses only (n,c,p) to
  retrieve the resulting rule, never a generating-law label.
- Independent pilot: 7200 further samples, 100 per case, analyzed after the
  interval multipliers are frozen. Comparisons use the same samples for the
  calibrated Gibbs decision, its unscaled posterior-mean interval, ECM,
  Young--Mathew, and Wilks. Its binomial intervals reflect a smaller experiment
  than the completed 1000-per-case article study.

Both new stages have disjoint data and chain seeds, also checked against the
fixed-prior study and original archived Gibbs comparison. The pilot does not
automatically initiate another 1000-per-case comparison.

## Commands and Outputs

```sh
make test-mti-gibbs-boundary
make prepare-mti-gibbs-boundary
make check-mti-gibbs-boundary
make launch-mti-gibbs-boundary
make health-mti-gibbs-boundary
```

Preparation and checking require the pinned installed rqrgibbs and tolerance
packages, as in the original fixed-prior run. For direct commands, set
`MTI_PRIOR_R_LIBRARY` to that run's ignored `application/cache/fixed_prior_library`.
The background launcher copies those installed packages into a new ignored
cache and verifies the native binary against the original manifest. It never
builds or loads a namespace from an external source checkout. The exact source
commit, program and input hashes, pinned exdqlm reference, generator identity,
dependency versions, and RNG settings are recorded before execution.
The bounded check uses Gaussian, Student-t, exponential, and small-sample
lognormal development examples, plus a separate fresh-data rehearsal. It
saves endpoint, width, midpoint, objective, and cumulative-width traces in
`rehearsal_traces.pdf`; those samples are excluded from new calibration and
pilot streams.

The launcher uses a committed detached checkout under `.codex_work/`, at most
40 single-threaded workers, a controller lock, atomic fit records, and checksum
receipts. Completed records can be resumed only with identical source, tasks,
and policies. Unreceipted interrupted records require review. A 16 GiB disk
reserve and a development-based storage projection prevent an uncontrolled
growth in traces; historical fits are not deleted.

Outputs remain under `application/outputs/mti_gibbs_boundary_calibration_20261115/`
inside the detached checkout. They include `audit/geometry_review.pdf`,
development rule scores, calibrated design multipliers, frozen-policy hashes,
stage progress, compact posterior traces, and `pilot/pilot_review.pdf`.
The controller writes its exit status; health reporting does not continuously
poll an inactive study. The launcher records its detached checkout in an ignored
local file, so the health command also finds that run from the working checkout.
Numerical failures and resource limits preserve saved
progress and require review rather than changing statistical thresholds.

The fitted content q still comes from the original ECM anchor. This experiment
does not claim Gibbs-optimal joint search over q and tilt, global shortest
intervals, or a distribution-free theorem for the final rule. Promotion depends
on the completed pilot's attainment, within-law raw widths, uncertainty,
numerical diagnostics, and the magnitude of any improvement.
