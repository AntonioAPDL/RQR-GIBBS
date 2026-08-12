# TCSP Benchmark Registry

Date: 2026-08-12
Status: competitor-readiness registry, not a promotion record

## Scope

This registry documents methods that the TCSP iid univariate validation harness
can evaluate or deliberately leaves disabled. A method appearing here is not a
manuscript claim. Promotion requires a clean run, artifact audit, and review of
assumptions and failure behavior.

## Active Default Methods

| Method ID | Source | Target | Certificate status | Notes |
|---|---|---|---|---|
| `tcsp_dkw` | internal | first shortest closed order-statistic window after DKW retained-count calibration | conservative distribution-free fallback | Can be infeasible when the DKW buffer requires `k > n`. |
| `tcsp_mc` | internal | first shortest closed order-statistic window after Uniform scan calibration | simultaneous Massart-DKW empirical-CDF lower band over simulated scan distribution | Numerical calibration, not exact scan recursion. |
| `wilks_symmetric` | internal | symmetric order-statistic interval | exact beta-spacing calculation in harness | Fixed symmetric order-statistic action. |
| `wilks_minmax` | internal | sample range | exact beta range calculation in harness | Often conservative/range-wide. |
| `equal_tailed_tcsp_content` | internal | equal-tailed empirical diagnostic using TCSP DKW count | no formal certificate | Diagnostic only. |
| `normal_howe` | internal | normal-theory two-sided Howe approximation | approximate under normality | Used as parametric sensitivity, not robust evidence. |
| `oracle_shortest` | internal | population shortest interval under known DGP | oracle reference | Width reference only, not a feasible method. |

## Optional Wrappers

These wrappers are implemented in the validation harness but disabled in the
default pilot and full-pilot config.

| Method ID | Source package | Package function | Package method | Certificate status | Enable condition |
|---|---|---|---|---|---|
| `young_mathew` | `tolerance` | `nptol.int` | `YM` | interpolated/extrapolated order statistic, not independently audited here | Enable only for competitor-smoke and later reviewed pilots. |
| `wald_order` | `tolerance` | `nptol.int` | `WALD` | package-nominal Wald order-statistic interval, not independently audited here | Enable only for competitor-smoke and later reviewed pilots. |
| `hahn_meeker` | `tolerance` | `nptol.int` | `HM` | package-nominal Hahn-Meeker interval, not independently audited here | Enable only for competitor-smoke and later reviewed pilots. |
| `normal_exact_tolerance` | `tolerance` | `normtol.int` | `EXACT` | package-nominal exact normal-family k-factor | Enable only as a correctly specified normal-family comparator and misspecification sensitivity. |

When a package method returns multiple rows, the harness selects the
minimum-width interval with a first-tie rule and records `selected_output_row`,
`output_rows`, and `row_selection`.

## Disabled Methods

| Method ID | Reason |
|---|---|
| `smallest_nonparametric_tolerance_regions` | Must first be audited against Di Bucchianico, Einmahl, and Mushkudiani for identity or difference relative to the TCSP closed-window action and retained-count calibration. Do not present an identical action as an independent competitor. |
| `cal_gibbs_tolerance_gibbsTI` | Requires a pinned `gibbsTI` release, source SHA-256, isolated library, deterministic tiny run, runtime budget, and replication subset before any comparative pilot. |
| `calibrated_bnp_gibbs` | No tracked local implementation, validated runtime, or learning-rate calibration exists in this repository. |
| `dp_tolerance_intervals` | Promising Bayesian/model-based future comparator only if verified public code, license, action definition, and published-result replication become available. |

## Identity Audits Required Before Full Launch

The smallest-nonparametric-tolerance-region literature is a theoretical
ancestor of the TCSP action. Before a full launch, record an identity/difference
audit covering retained-count calibration, endpoint convention, strict/weak
inequalities, minimum-width comparison class, finite-sample guarantee,
asymptotic result, and multivariate extension. If the univariate action is
mathematically identical to TCSP after convention reconciliation, cite it as
source theory rather than treating it as a separate competitor.

## Launch Boundary

The optional wrappers may be used in tiny competitor-smoke tests after the
hardened pilot passes required gates. They should not be added to the default
full pilot until their smoke run is audited and their assumptions are accepted.
