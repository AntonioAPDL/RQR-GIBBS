# ECM Stationarity Threshold and Tolerance-Study Promotion Plan, 2026-08-17

## Decision

The fixed-target MTI ECM convergence flag now uses
`tol_stationarity = 1e-3` as the default stationarity tolerance. This threshold
is an optimizer diagnostic for the deterministic generalized-Bayes mode. It is
not a finite-sample tolerance guarantee, and it does not change the formal TCSP
order-statistic action.

## Audit Findings

The completed follow-up runs store final stationarity, objective drop, and trace
length for the MTI ECM row. These diagnostics are sufficient to reassess the
completed runs without rerunning the full validation grid.

| Lane | ECM rows with diagnostics | Rows with stationarity <= 1e-3 | Median stationarity | Maximum stationarity | Interpretation |
|---|---:|---:|---:|---:|---|
| ECM-200 audit | 3,500 | 3,485 | 2.05e-4 | 1.62e-3 | Operationally stable; a small number of borderline fits should be reported as near-threshold rather than hidden. |
| Paper-matched 90% | 0 | 0 | NA | NA | Full-range feasibility makes fixed-target MTI engines unavailable by design. |
| Small-sample 95% | 2,400 | 2,400 | 2.19e-4 | 8.62e-4 | All completed ECM diagnostics pass the new stationarity threshold. |

Across the two lanes with ECM diagnostics, the objective traces show negligible
final relative objective movement. This supports treating the ECM row as a
stable fixed-target optimizer for the validation discussion, while preserving
the more important distinction that its endpoints are fitted summaries after
scan calibration, not the formal tolerance action.

## Source Changes Required

Completed:

- Set the package-level default in `.rqr_ecm_assert_control()` to
  `tol_stationarity = 1e-3`.
- Set the active main and follow-up validation configs to the same
  `tol_stationarity = 1e-3` for non-smoke ECM modes.
- Lock the default and config values in tests.
- Update the ECM help page, follow-up protocol note, main manuscript, and
  supplement language so the threshold is described as a numerical diagnostic.
- Make the manuscript table build robust to clean worktrees: the Makefile now
  regenerates the tolerance table when the ignored run summary is available and
  otherwise reuses the committed table with an explicit message.
- Add generated supplemental follow-up tables for lane-level performance,
  \(10^{-3}\)-stationarity ECM diagnostics, and small-sample content-level
  behavior.

## Promotion Plan

1. Regenerate article-facing tolerance tables from completed primary and
   follow-up results, using the stored stationarity diagnostics and the `1e-3`
   convention for ECM reporting. Do not edit run outputs by hand. The current
   table generators now implement this path.
2. Keep the main article table focused on the comparison that changes the
   scientific message: TCSP, hybrid DP, Young--Mathew, Wilks, and the MTI
   fixed-target summaries where feasible. Put feasibility and stationarity
   diagnostics in the supplement.
3. In the main text, state that TCSP remains the formal finite-sample action,
   hybrid DP is a Bayesian-screened scan action, and MTI Gibbs/ECM are
   generalized-Bayes fitted summaries conditional on the calibrated scan target.
4. Report the small-sample lesson explicitly: at infeasible or near-threshold
   settings, methods that always return an interval can under-cover, whereas
   TCSP fails closed when the requested content/confidence pair cannot be
   certified.
5. Treat the iid univariate tolerance validation as ready for manuscript
   promotion after table regeneration and one final build. Regression,
   DESN-forecast, and dynamic tolerance claims remain out of scope until they
   have separate validation protocols.

## Article-Writing Next Step

The remaining writing work is editorial rather than computational: integrate one
compact table and a short reader-centered paragraph explaining feasibility,
interval width, and the role of ECM diagnostics. The article should avoid
framing ECM convergence as a tolerance-validity condition; the proof and
software contract assign that role to the scan-calibrated action.
