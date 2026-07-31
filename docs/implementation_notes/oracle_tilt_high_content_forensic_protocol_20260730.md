# High-Content Oracle-Tilt Forensic Protocol

## Purpose

This protocol governs bounded follow-up checks for the single-data-set
mean-tilted RQR illustrations at content \(c=0.95\). It is separate from the
manuscript figure generator and from any matched simulation study. Its purpose
is to distinguish:

1. ordinary Monte Carlo insufficiency;
2. slow movement or multiple path modes in the alternating-root Gibbs scan;
3. a mismatch between the implemented conditional Gaussian update and its
   declared target; and
4. a proper but weakly identified generalized posterior caused by a
   near-boundary tilt and a diffuse dynamic prior direction.

The workflow summarizes interval-root generalized posteriors. It does not
define or assess a response-predictive distribution.

## Triggering evidence

The completed local run `c095-parallel-20260729`, evaluated at source commit
`581b36896852937b5f08135ff90627bcc2313fdb`, completed all six requested
model--target cases with zero numerical repairs. Fixed-design RQR,
fixed-design ET, and DLM-RQR passed their declared diagnostics. Fixed-design
SH had a modest ESS shortfall. DLM-ET had material between-chain disagreement,
and DLM-SH produced an escaping upper root.

The DLM-SH population tilt is close to the upper end of the admissible
population tilt path. Under the declared local-linear prior, the cumulative
linear canonical site also has a large projection on the initial-slope
direction. This motivates a prior-and-target decomposition before any change
to the sampler or state prior.

## Implemented diagnostic resolution

The bounded audit implemented this protocol without changing the production
sampler, the population oracle, or the manuscript figures. The evidence
supports the following disposition.

| Question | Evidence-based disposition |
|---|---|
| Fixed-design SH shortfall | Resolved by a longer four-chain run: 24,000 retained draws, zero repairs, and all declared diagnostics passed. |
| DLM-ET disagreement | Resolved by four dispersed starts: 10,000 retained draws, zero repairs, and all declared diagnostics passed. |
| DLM-SH escaping path under `C0[2,2]=1` | Confirmed as a target/prior interaction. R and C++ conditional smoothers agree with a scale-relative dense Gaussian calculation, while the full target decomposition has a finite remote mode. |
| DLM-SH prior sensitivity | Variances `1` and `0.1` retain the remote mode; `0.01` remains start-sensitive; `0.001` is stable from oracle and prior-shift stress starts with zero repairs. |
| DLM-SH bounded remedy at `C0[2,2]=0.001` | Five dispersed starts eliminate the remote mode and pass R/C++/dense conditional checks. The 30,000-draw run misses only bulk/tail ESS gates at two local times. |
| Longer DLM-SH acceptance attempt | Computationally inconclusive. A five-worker 120,000-draw attempt reached the predeclared 180-minute cutoff without a completed worker checkpoint. It is not classified as a statistical failure. |

The candidate slope variance is scale-justified rather than selected only for
visual appearance: its prior slope standard deviation is
\(\sqrt{0.001}\simeq0.0316\), more than twice the true initial slope magnitude
in the fixture and more than five times the declared slope-innovation
standard deviation \(0.006\). It is nevertheless a revised model
specification, not a sampler correction. Manuscript promotion remains
disabled until a resource-bounded longer run clears the remaining ESS gates.

## Source and output boundary

The forensic source consists of:

- `application/scripts/33_oracle_tilt_forensic_utils.R`;
- `application/scripts/33_run_oracle_tilt_forensics.R`;
- `application/config/oracle_tilt_forensics_20260730.json`;
- `application/config/oracle_tilt_dlm_sh_acceptance_20260730.json`; and
- focused tests in
  `application/tests/testthat/test-rqr-oracle-tilt-forensics.R`.

Tracked configuration keeps execution disabled. A bounded execution requires
both a local ignored configuration with `execution_authorized=true` and the
environment confirmation

```text
RQR_ORACLE_TILT_FORENSICS_CONFIRM=YES
```

All fitted traces, logs, and closeouts belong under ignored
`application/outputs/`, `application/logs/`, or `application/cache/` roots.
No unrestricted fitted object is tracked.

The general forensic template retains the original prior and the diagnostic
grid. The DLM-SH acceptance template freezes the supported
`C0[2,2]=0.001` candidate, five stress-tested starts, 3,000 burn-in
iterations, 24,000 retained draws per chain, and one worker. The single-worker
default is intentional for constrained hosts: every completed chain is
atomically available for exact resumption before the next chain begins.

## Pre-MCMC contracts

### Population tilt geometry

For an innovation law \(F\), content \(c\), and quantile-window retained mean
\(M_c(u)\), the admissible population range is

\[
\delta_- = M_c(0)-\mu,\qquad
\delta_+ = M_c(1-c)-\mu.
\]

Every oracle target records its lower and upper margins, its normalized
position within this range, and whether either normalized margin is smaller
than a declared caution threshold. This is a diagnostic, not an automatic
replacement for the requested target.

### Dynamic prior response to the canonical tilt

For a fixed finite-horizon Gaussian state prior, let \(K\) be the prior
covariance of the root ordinates and let \(g_t=\omega_{\mathrm R}c\delta_t
f_t\) be the observed-time canonical sites. The prior-only mean displacement
is

\[
\Delta m_{\mathrm{path}} =
\operatorname{Cov}(\theta_{1:T})g,
\]

with ordinate displacement obtained by applying the observation vectors. The
forensic preflight records its terminal value, maximum absolute value, and
norm relative to the oracle width and response scale. A large displacement is
not a numerical error. It identifies a direction in which the linear tilt
must be counteracted by the product-check loss or by prior regularization.

### Escaping-direction target profile

For the local-linear fixture, the runner also evaluates the unnormalized
negative log target along the deterministic initial-slope direction while
holding the other root at its oracle path. The output separates:

- ordinary product-check loss;
- the linear tilt contribution;
- the Gaussian prior quadratic; and
- their fixed-rate total.

This profile is diagnostic. It is not an optimizer used by the sampler.

## Bounded MCMC contract

The forensic grid contains:

- fixed-design SH as a longer-draw acceptance check;
- DLM-ET with four dispersed deterministic initial profiles; and
- DLM-SH with the same four profiles.

The dynamic profiles are default, oracle-centered, narrow, and wide. They
change initial root paths but not the target. Every dynamic fit uses fixed
\(W\), fixed learning rate, exact population tilt, the fail-on-repair numerical
policy, stored state paths, and stored latent scales.

Independent chains may run in parallel on a fork-capable platform. Each
worker writes its compact trace and result envelope through a same-directory
atomic rename. The envelope binds the config, runner, helper sources, package
version, target, initialization profile, seed, MCMC control, and initial
covariance. A rerun resumes a worker only when that contract digest and trace
hash match exactly. A missing worker envelope means that the chain did not
complete; it must never be counted as a failed diagnostic or a retained draw.

For each retained draw, compact traces include:

- ordered endpoints, midpoint, and width at prespecified times;
- raw-root level and slope states at those times;
- time-zero and terminal states;
- maximum absolute root ordinate and state;
- ordinary loss, tilt contribution, tilted target loss;
- root-prior quadratic and combined negative log target up to an additive
  constant;
- selected latent-scale values; and
- the complete-root swap indicator.

The runner does not retain responses simulated from the interval roots.

## Independent conditional reference

At one retained draw per dynamic chain, the runner reconstructs each
root-specific conditional Gaussian path system. It compares:

1. the pure-R FFBS smoother;
2. the C++ FFBS smoother; and
3. an independently assembled dense Gaussian precision calculation.

The comparison covers conditional means and all same-time covariance blocks.
Missing responses contribute neither a pseudo-observation nor a tilt site.

## Diagnostics and decisions

The maintained `posterior` implementation supplies rank-normalized R-hat,
bulk ESS, tail ESS, and mean MCSE. The declared gates remain:

- R-hat at most 1.05;
- bulk ESS at least 400;
- tail ESS at least 200;
- MCSE divided by pooled standard deviation at most 0.10; and
- zero numerical repairs.

Scale-relative pathology fields are reported separately. They must not be
silently converted into an inferential correction.

Prior-sensitivity rows have two separate decisions:

- `scale_stability_pass` excludes remote-width paths and numerical repairs;
- `diagnostics_pass` applies the maintained R-hat, ESS, and MCSE gates.

`sensitivity_pass` requires both. A short run that excludes the remote mode
but has not accumulated enough effective draws is not an accepted model.

The allowed dispositions are:

- **Monte Carlo extension:** only when chain geometry is consistent and the
  failures are limited to effective sample size or MCSE.
- **Exact transition improvement:** only when the target is stable but the
  alternating-root scan moves poorly.
- **Prior respecification:** only when justified independently of the realized
  plot and frozen before a new acceptance run.
- **Limitation or omission:** when an admissible population target produces a
  proper but scientifically uninformative finite-sample dynamic generalized
  posterior under the declared prior.
- **Implementation correction:** only after a conditional-reference or
  zero-tilt-equivalence test demonstrates a discrepancy.

Clipping roots, truncating a figure, shrinking the tilt without disclosure,
or replacing an exact population oracle with a Cornish--Fisher approximation
is not an admissible remedy.

## Promotion boundary

The \(c=0.95\) figures remain local until every promoted case passes its MCMC,
numerical, artifact, and scale-relative review. The existing manuscript
figures are unchanged by this workflow. A successful bounded forensic check
would support only the exact fixture and target that it evaluated; it would
not establish coverage calibration or comparative forecasting performance.

At the present closeout, fixed-design SH and DLM-ET are accepted for this
bounded fixture. DLM-SH at the original prior is rejected, and the
scale-consistent `C0[2,2]=0.001` candidate is conditionally supported but not
yet accepted because its longer ESS run did not complete inside the declared
resource envelope.
