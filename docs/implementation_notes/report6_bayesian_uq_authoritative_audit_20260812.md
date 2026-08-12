# Report6 Bayesian UQ Integration Audit

Date: 2026-08-12

Branch: `feature/bayesian-uq-authoritative-report6-20260812`

Base commit before implementation: `f505ff55789d824556161d17cfc0c97bd13aa022`

Input hashes:

| Artifact | SHA256 |
|---|---|
| `report6.md` | `32637e1fb7f5c768437c9ff0004d17224de92baf25e1ad0674a0516594a16174` |
| `STYLE_PROFILE.md` | `b9483cf55e108f5615c203dfff6c0940b7ecc5c001aaad679507004b70352513` |
| `AGENTS.md` | `e59f5a6e579212a0b563ecdfd911b5cb95ea73e8febb50f236390567c6624e13` |

## Diagnosis

The prior TCSP-MT-RQR implementation correctly built a scan-calibrated empirical
order-statistic action and could attach MT-RQR MCMC/ECM summaries after freezing
the selected content and tilt. That plug-in attachment does not propagate
uncertainty in the selected shortest interval and is not an unconditional
Bayesian posterior for the population shortest interval. Keeping it as the
authoritative UQ story would blur three different objects:

- repeated-sampling tolerance confidence from a scan count;
- posterior content uncertainty under a response-distribution model for `F`;
- fixed-target generalized-Bayes uncertainty for an MT-RQR interval-root loss.

Report6 correctly makes the direct Dirichlet-process posterior the first
authoritative Bayesian UQ engine because it gives exact conjugate fixed-interval
content probabilities:

`F(I) | D_n ~ Beta(a H(I) + N_n(I), a(1 - H(I)) + n - N_n(I))`.

The Gaussian DPM layer is worth adding as a smooth response-distribution
alternative, but its content probabilities are Monte Carlo summaries and should
not replace the exact direct-DP fixed-interval content law as the first engine.
The Bayesian bootstrap is useful only as a diagnostic weighted-empirical
comparator.

## Implemented Decisions

- Added direct-DP base measures, strict Bayesian base validation, exact content
  probabilities, posterior stick-breaking draws, posterior shortest-draw
  summaries, and order-statistic Bayesian action search.
- Added a pure-R weighted shortest-interval engine with deterministic first
  global-minimum tie handling.
- Added Gaussian DPM MCMC, posterior density/CDF/quantile helpers, posterior
  content probabilities, posterior shortest-draw summaries, and Bayesian action
  search.
- Added deterministic Gaussian DPM ECM as a MAP diagnostic and initializer, not
  posterior UQ.
- Added `rqr_tcsp_hybrid_bayes_fit()` with direct-DP and DPM engines. The formal
  hybrid action satisfies both `N_n(I) >= k_{n,c,alpha}` and
  `Pr(F(I) >= c | D_n) >= 1 - gamma`.
- Added `rqr_tcsp_plugin_fit_univariate()` and metadata marking the old
  TCSP-MT-RQR attachment as `uq_scope = "fixed_target_plugin"` and
  `lifecycle_status = "superseded_for_unconditional_shortest_interval_uq"`.
- Updated package documentation, namespace exports, README scope, manuscript,
  supplement, and bibliography.
- Added smoke/moderate validation configuration and launcher. Confirmatory heavy
  runs remain fail-closed and unauthorized.

## Claim Boundary

Promoted:

- Direct-DP fixed-interval posterior content law.
- Direct-DP and DPM response-distribution posterior content UQ.
- Hybrid Bayesian-scan candidate selection as an implemented action contract.

Not promoted:

- posterior endpoint coverage as a tolerance-confidence theorem;
- exact scan recursion;
- selected-action shortest-window asymptotics;
- regression-family tolerance guarantees;
- MT-RQR plug-in MCMC/ECM as unconditional shortest-interval UQ;
- DPM ECM as posterior UQ.

## Validation Run

The smoke validation completed at:

`application/outputs/rqr_bayes_uq_validation_v1/smoke_20260812T081451Z`

This directory is intentionally ignored. The health check verified the required
result, summary, manifest, hash, and README artifacts and reported 56 result
rows.
