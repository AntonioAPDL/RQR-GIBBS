# Informative Common-Scale Priors for MTI-ECM

This experiment compares informative Gamma priors on inverse loss scale
with the existing fixed-scale and bounded log-uniform calculations. It
does not change the reported tolerance method or enable unknown-scale Gibbs.
The derivations and limits of conjugacy are in
[the scale theory note](../theory/mti_common_scale.md).

## Statistical Specification

Write omega=1/sigma and eta=log(omega). Gamma(shape,rate) on omega is
equivalent to inverse-Gamma(shape,rate) on sigma itself. With independent
Gaussian root priors, the density in roots and eta has kernel

    exp{(n+shape)eta-exp(eta)(L_delta+rate)-Gaussian root penalties}.

The primary prior specifications have shape=rate in {4,16,64}, truncated
to omega in [0.25,4]. These give untruncated inverse-scale coefficients of
variation 1/2,1/4,1/8 and log-density mode at one. Additional shape 16
comparisons use [0.5,2], [0.0625,16], and (0,4]. The latter removes only
the lower bound. All quantities use independently fixed units; no sample
SD, loss, oracle tilt, or validation result specifies the prior.

Root SD 2, DP concentration 1 and the Normal(0,4^2) DP base remain unchanged.
The q/delta candidate menu and inherited DP probability cutoff are held
fixed initially. A candidate's updated rate can be nonpositive even under
a proper bounded posterior. Such a conditional is a bounded
power-exponential density, not an ordinary positive-rate Gamma.

The unrestricted Gamma extension is implemented as a zero-tilt mathematical
control only. Nonzero tilt can make the loss unbounded below along a root
direction. Every unrestricted nonzero-tilt request is therefore rejected;
positive loss at the selected mode is insufficient. No negative rate is
silently clipped. Gamma and inverse-Gamma are reciprocal representations,
not separate competing methods.

## Fitting Interface

```r
fit <- rqrgibbs::rqr_ecm_fit(
  y, matrix(1, length(y), 1), coverage_level = 0.97,
  mean_tilt = 0.1,
  beta_prior_obj = rqrgibbs::beta_prior("ridge", ridge = list(tau2 = 4)),
  ecm_control = list(ecm_backend = "cpp", max_iter = 200),
  scale_control = list(
    prior = "gamma_rate", shape = 16, rate = 16,
    log_rate_bounds = log(c(0.25, 4))
  )
)
```

Use `c(-Inf,log(4))` for upper-only truncation. For exactly zero tilt,
`c(-Inf,Inf)` also gives a proper unrestricted Gamma construction. ECM
uses a mode in roots and log inverse scale; reciprocal density modes need
not match. Both latent moments, prior terms, full tilt constants and the
Jacobian enter the calculation. ECME is an optional observed scale update.

The fit summary includes the exact prior, scale, boundary indicators and
sufficient loss-ray obstruction diagnostics. Passing these ray checks
does not prove unrestricted propriety. The final numerical checks are
coordinatewise, not a global optimization theorem. No scale uncertainty
is integrated by ECM.

## Tests and Launch

```sh
mkdir -p application/cache/gamma_scale_library
R CMD INSTALL --no-multiarch --library=application/cache/gamma_scale_library application
make test-mti-gamma-scale
make prepare-mti-gamma-scale
make launch-mti-gamma-scale
make health-mti-gamma-scale
```

The tests include 48 R/native fixtures covering four small sample sizes,
three Gamma concentrations, two tilt values and ECM/ECME. Additional checks
cover conditional integration, reciprocal Jacobians, signed rates, support
limits, transformed units, strong-prior limits, and unchanged fixed-scale
behavior. The installed package must match the source and native binary.

The launcher pins committed source and copies its installed library to an
isolated ignored worktree. It waits for the preceding common-scale study
to release its controller lock, then requires its successful completion.
The previous study is already finished at preparation of this experiment.
Source/library locks prevent duplicate launches or silent mid-run changes.

The controller performs three stages automatically:

1. Audit all saved primary-range candidate losses and sufficient unbounded
   rays on the original samples; compute prior-only quantiles.
2. Rehearse all six new specifications on 24 existing datasets spanning
   eight laws and the designs (50,.90),(100,.95),(500,.99).
3. If every numerical check passes, fit all 864 screening datasets: 12
   replications in each of the 72 existing law/design cases. This is 5184
   new prior-specific profile fits. Verified rehearsal results are reused.

The eight laws and nine n,c,p designs are unchanged. The 0.99 cases begin
at n500; no new structurally infeasible comparison or multimodal case is
introduced. Fixed-scale MTI, the preceding primary log-uniform method,
Young-Mathew and Wilks use the same observations. Their verified historical
results are reused without modifying original reports.

Numerical-library threads are one per worker, with at most 40 available
workers, a disk reserve and projected-storage checks. Atomic results and
receipts permit exact resumption. Failed numerical records are retained
and block automatic advancement rather than being overwritten or replaced
by a new dataset. Heavy objects stay outside Git.

## Outputs and Decisions

The pointer `.codex_work/mti_gamma_scale_latest.json` records the frozen
source checkout and `/var/tmp/jaguir26-mti-studies/` output directory.
Verification runs from that checkout:

```sh
make verify-mti-gamma-scale
```

Reports contain candidate-domain and prior-quantile CSVs; exact interval
exports; casewise attainment confidence intervals and raw empirical width
ranges; paired endpoint changes; scale and boundary summaries; all three
relaxed-threshold comparisons; and a diagnostic PDF with selected traces
for replication1 in every law/design/prior combination.

The diagnostic attainment axis shows its full range so poor values remain
visible. Widths are never pooled across distributions. Runtime is retained
for internal scheduling, not added to the manuscript's comparison tables.
No figures or tables are automatically copied into the article.

The finite screening sample cannot select a prior and establish tolerance
validity simultaneously. The controller stops after a verified report and
writes `followup_decision.json`. Unchanged endpoints or no width gain give
an explicit scientific stop. Any gain instead requires review of prior
and support sensitivity, casewise losses/gains in attainment, and practical
width relevance. A numerical exit zero does not authorize a 72,000-dataset
validation or a new Gibbs study.

The ignored research plan describes the conditional next stages: up to two
prior finalists on 7200 development datasets, separate initial calibration
on 14,400 fresh samples, a frozen rule before 72,000 fresh validation
samples, and Gibbs only after a justified decision. None is silently
scheduled on the strength of a 12-replication comparison. Priors and
thresholds must not be retuned on validation observations. Relaxed rules
remain exploratory and do not acquire an exact tolerance guarantee through
prior concentration or lack of a statistically detected deficit.
