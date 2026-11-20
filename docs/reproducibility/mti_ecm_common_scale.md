# Experimental Common-Scale MTI Computation

This isolated ECM experiment estimates the common loss scale under a proper
finite log-scale prior. It preserves the current fixed-scale results and
does not enable learned-scale Gibbs. The statistical formulation and
propriety argument are in [the derivation](../theory/mti_common_scale.md).

## Package Interface

```r
fit <- rqrgibbs::rqr_ecm_fit(
  y, matrix(1, length(y), 1), coverage_level = 0.97,
  mean_tilt = 0.1,
  beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = 4)),
  ecm_control = list(ecm_backend = "cpp", max_iter = 200),
  scale_control = list(log_rate_bounds = log(c(0.25, 4)), update = "ecm")
)
```

The bounds must be independently declared, finite and strictly enclose rate
one. With `scale_control=NULL`, the prior fixed-rate calculation is unchanged.
The optional `update="ecme"` uses observed scale maximization. Both versions
record exact observed-coordinate polishing, which checks roots at nonsmooth
data knots and scale optimality at its prior bounds. The reported joint mode
is defined in raw roots and log inverse loss scale, not in response variance.

`fit$common_scale` records the prior support, fitted rate and scale, density
parameterization, powers, and bound indicators. `objective_trace` contains
roots, width, rate, scale, tilted loss, observed objective and update phase.
`stationarity_diagnostic$common_scale_accuracy` contains independent final
coordinate/scale improvement and displacement checks. These checks establish
coordinatewise numerical accuracy, not a global mode or a tolerance theorem.

For DP profile fitting, pass the same list as `ecm_control$scale_control` to
`rqr_mti_ecm_dp_profile_action()`. Each candidate records its own fitted rate
and accuracy. A numerically inadequate candidate cannot become eligible
merely because it passes the DP content check. The direct-DP prior remains
independent of the analyzed sample.

## Tests and Rehearsal

Build/install this source under an ignored library, never under an external
exdqlm source checkout:

```sh
mkdir -p application/cache/common_scale_library
R CMD INSTALL --no-multiarch --library=application/cache/common_scale_library application
make test-mti-ecm-common-scale
make prepare-mti-ecm-common-scale
make check-mti-ecm-common-scale
```

The new tests check improper-prior rejection, density transformations,
normal-exponential integration, both latent moments, exact root and scale
optima, R/native agreement, unchanged fixed-rate behavior, DP wiring and
scientific stopping decisions. Preparation requires the completed November
19 ECM assessment and its saved independent-prior development evidence.
Rehearsal uses two previously generated development datasets, never new
validation observations. A stale installed package or changed source receipt
causes preparation to stop.

## All-Case Screening

```sh
make launch-mti-ecm-common-scale
make health-mti-ecm-common-scale
```

The launcher requires committed source, creates an isolated pinned checkout,
and writes under `/var/tmp/jaguir26-mti-studies/`. It uses at most 40 available
workers with one numerical-library thread per process, checks a 32 GiB disk
reserve and projected storage, and retains exact task identities and atomic
checksummed results. Existing verified results permit resumption; numerical
failures are preserved for review instead of overwritten. The local pointer
is `.codex_work/mti_ecm_common_scale_latest.json`. Run verification from the
recorded pinned checkout:

```sh
make verify-mti-ecm-common-scale
```

The panel has 12 reused development samples in each of the existing 72
distribution/design cases: 864 datasets. Priors retain fixed root SD2,
DP concentration1 and Normal(0,4^2) base in independent (0,1) coordinates.
Prior rate ranges are [0.5,2], [0.25,4] (primary) and [0.0625,16]. These
predeclared factors assess regularization sensitivity; they are not optimal
or unrestricted noninformative choices. Each dataset includes the actual
saved fixed-scale comparator, the three scale ranges, Young--Mathew and
Wilks. The same observations and no endpoint multipliers are used throughout.

Saved evidence includes all candidate diagnostics, selected traces, actual
sample and source receipts, internal runtime information and seventeen-digit
interval exports. The PDF gives casewise attainment, raw width ranges,
selected-scale distributions and convergence traces. With only 12 samples,
these attainment estimates are screening diagnostics, not a powered
independent assessment.

Point-target, compatibility-target and closest-nominal threshold rules are
replayed from the fitted candidate tables without refitting. Any selected
screening thresholds remain exploratory and are not transferred to the
application or article. Compatibility with p does not prove attainment >=p.
Unavailable calibrations and missing intervals remain explicit.

## Scientific Decision

A one-sided Hoeffding lower bound above one half identifies majority
upper-bound selection across the heterogeneous panel. For N returned fits
and K upper-bound modes it is
`max(0, K/N - sqrt(log(1/alpha)/(2*N)))`. Independence is sufficient;
identical probabilities across laws and designs are not required. This
panel diagnostic differs from the exact binomial intervals used separately
within each distribution/design. When broader support changes paired endpoints beyond
the existing numerical endpoint tolerance, this triggers
`scientific_stop_bound_sensitive`. This is a prior-sensitivity decision,
not a tolerance test or an implementation failure. Successful computation
can therefore finish with a scientific stop and controller exit zero.

Any numerical failure instead stops with its saved evidence and nonzero
exit. A finished screening panel does not automatically start development,
72,000 new validation datasets, Gibbs or article replacement. Advancing
requires scientific review of boundary effects, widths and calibration;
the conditional stages are described in the user's ignored research plan.
This separation avoids spending a broad validation on a model whose scale
is predominantly determined by an arbitrary finite bound.

## Separate Statistical Review

The initial screening source a32c4af used a pooled binomial lower bound
for this diagnostic. Its fitted roots, candidate tables, casewise attainment
intervals and traces are unchanged. Later analysis corrects only the
heterogeneous-panel bound, preserving all original reports and exits:

```sh
make review-mti-ecm-common-scale
make verify-reviewed-mti-ecm-common-scale
```

Run with committed analysis source and its matching installed package,
after the original controller finishes. Review verifies every original
source/fit receipt, reproduces observations and contents, rechecks selected
roots and scale, and writes separate reports under `_review_<analysis-commit>`.
It performs no fitting or sampling. The separate pointer is
`.codex_work/mti_ecm_common_scale_review_latest.json`; health displays both
the historical and reviewed assessments. Repeated review verifies the
existing outputs. Corrupted original evidence or revised reports stop it.

The panel bound follows
[Hoeffding (1963)](https://doi.org/10.1080/01621459.1963.10500830).
It supports a scale-prior diagnostic, not a distribution-free tolerance
guarantee for the fitted intervals.
