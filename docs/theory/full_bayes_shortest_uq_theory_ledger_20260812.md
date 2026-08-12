# Full Bayesian Shortest-UQ Theory Ledger

Date: 2026-08-12

## Ledger

| ID | Object | Status | Guarantee | Limitation |
|---|---|---|---|---|
| `T-DP-1` | Direct-DP posterior conjugacy | `PROVED-AND-AUDITED` | `F | D_n ~ DP(a+n,H_n)` for fixed prior `DP(a,H)` and iid sampling from `F`. | Requires fixed, non-data-dependent base measure for strict Bayesian interpretation. |
| `T-DP-2` | Fixed-interval DP content law | `PROVED-AND-AUDITED` | For fixed closed `I`, `F(I) | D_n` has the exact Beta law used by `rqr_dp_content_probability()`. | The interval must be fixed when the probability is evaluated; selected-action coverage needs separate analysis. |
| `T-DP-3` | Direct-DP Bayesian action search | `IMPLEMENTED-AUDIT-PENDING` | Exhaustive search over closed order-statistic intervals with deterministic first-width tie rule. | Bayesian content action only; not a distribution-free tolerance theorem. |
| `T-HBS-1` | Hybrid Bayesian-scan action | `IMPLEMENTED-AUDIT-PENDING` | Selects the first shortest candidate satisfying both scan count and posterior content constraints. | No posterior endpoint-coverage theorem; scan critical-value proof remains separate. |
| `T-DPM-1` | Truncated Gaussian DPM Gibbs engine | `IMPLEMENTED-AUDIT-PENDING` | Response-likelihood posterior draws for a smooth mixture approximation to `F`. | Monte Carlo and truncation approximation; not exact DP conjugacy. |
| `T-DPM-2` | DPM fixed-interval content probability | `IMPLEMENTED-AUDIT-PENDING` | Computes interval mass analytically for each retained mixture draw and averages indicator content events. | Posterior probability has MCMC error; no finite-sample tolerance theorem. |
| `T-DPM-ECM` | DPM ECM diagnostic | `IMPLEMENTED-AUDIT-PENDING` | Deterministic MAP-style fit with recorded objective trace and monotone backtracking. | Diagnostic/initializer only; not posterior UQ and not a formal tolerance action. |
| `T-BB-1` | Bayesian bootstrap diagnostic | `IMPLEMENTED-AUDIT-PENDING` | Weighted empirical posterior-shortest diagnostic for sensitivity comparison. | Not promoted as a strict direct-DP response model with fixed prior. |
| `T-SCAN-EXACT` | Exact scan recursion | `BLOCKING` | None in this implementation. | Conservative Monte Carlo/DKW calibration remains the implemented path. |
| `T-REG-TOL` | Regression-family tolerance theory | `LATER` | None. | Current validation is iid univariate only. |

## Authoritative UQ Position

The authoritative Bayesian UQ path for shortest-interval content is:

1. direct-DP response-distribution fit;
2. exact fixed-interval Beta content probabilities;
3. hybrid Bayesian-scan action when repeated-sampling scan guarding is required.

MT-RQR MCMC/ECM remains a generalized-Bayes fixed-target plug-in summary. It is
not an unconditional posterior over the population shortest interval and is not
the source of tolerance confidence.
