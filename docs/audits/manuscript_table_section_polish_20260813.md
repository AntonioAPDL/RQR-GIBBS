# Manuscript Table and Section Polish, 2026-08-13

## Purpose

This audit records the reader-facing manuscript polish that removed
ledger-style tables from the main article and aligned main and supplement
headings with the current MPI/MTI framing.

## Decisions

| Object | Decision | Rationale |
|---|---|---|
| Main inferential-map table | Removed from the main article and replaced by prose | The distinction among target, generalized posterior, reported summary, tolerance calibration, and response-distribution Bayes is important, but the five-column table interrupted the introduction and was not referenced later. |
| Main model-family architecture table | Removed from the main article and replaced by prose | The table functioned as an internal capability ledger. The computation section now states the supported endpoint adapters and exact-update scope directly. |
| Main TCSP theory-scope table | Removed from the main article and replaced by prose | The claim boundaries are essential, but the long limitation table made the tolerance section read like a proof ledger. The main article now gives the boundary statement in prose, while the supplement support map and proof ledger retain the recoverable scope. |
| Main section headings | Renamed toward reader-facing statistical objects | Headings now foreground interval targets, generalized-Bayes endpoint computation, dynamic endpoint-state computation, calibrated tolerance intervals, and numerical evidence. |
| Supplement headings | Renamed to match the main article | Labels are preserved, but visible headings now use the same MPI/MTI and endpoint terminology as the main text. |

## Main-Article Heading Contract

The main article now uses the following visible sequence:

1. Introduction
2. Fixed-Content Interval Targets
3. The Mean-Preserving Interval Target
4. Mean-Tilted Intervals
5. Empirical Balance and Static Uncertainty
6. Generalized-Bayes Endpoint Computation
7. Dynamic Endpoint-State Computation
8. Calibrated Minimum-Width Tolerance Intervals
9. Validation Scope and Numerical Evidence
10. Discussion

## Claim Boundary Preserved

The table removals do not change the mathematical or computational claims.
The main text still states that MPI and MTI are fixed-content population
targets; generalized-posterior endpoint uncertainty is not a response
posterior predictive distribution; tolerance calibration is an external
content--confidence requirement; direct-DP and Gaussian-DPM intervals are
ordinary response-distribution Bayesian alternatives; and exact scan recursion,
posterior-to-action transfer, selected-action asymptotics, and
regression-family tolerance calibration remain separate certification tasks.

## Wiring Updates

The manuscript-language validator, MPI/MTI naming validator, TCSP theory-wiring
test, and main--supplement support map were updated to enforce the new prose and
heading contract rather than the removed table labels.
