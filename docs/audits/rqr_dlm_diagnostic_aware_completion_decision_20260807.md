# RQR-DLM diagnostic-aware maximum-run decision

## Decision

The main RQR-DLM simulation will be completed under a separately versioned
**diagnostic-aware completion** policy. The original confirmatory flag remains
false. Frozen R-hat, bulk-ESS, tail-ESS, and MCSE thresholds remain unchanged
and are still evaluated for every MCMC fit. A threshold failure is retained as
an `mcmc_diagnostic_warning`, but it no longer discards finite primary metrics,
stops the same scenario, or blocks later waves.

This policy responds to the explicit 2026-08-07 user direction to obtain the
complete planned result even when conservative chain diagnostics do not all
pass. It does not redefine a diagnostic failure as a pass.

## Evidence behind the transition choice

The bounded multicomponent comparison at source commit
`c6fd8b05ec839cc75873f30af7244c501dc8fa6c` completed all 48 fits. All fits
used the same fixed schedule, succeeded under the exact joint target, required
zero numerical repairs, passed their resource limits, and preserved the target
identity checks. Across all four candidates, 523 of 572 diagnostics passed.

No candidate cleared every conservative diagnostic. The strongest candidate
was `directional1_joint1`, with 140 of 143 diagnostics passing. Its three
failures were confined to M11, S10, replication 166: `mean_upper`,
`mean_width`, and `log_q_2`. The second joint elliptical cycle was worse, so
the maximum run uses one directional scale interweave and one joint state
elliptical cycle rather than adding another transition mechanically.

The ignored evidence is authenticated by:

| Artifact | SHA-256 |
|---|---|
| comparison manifest | `0793c2d137bf3d296809b04d8fbaeed8d53d196952d95a50d3f0149ff3f6da47` |
| candidate summary | `e845d6baf0047ed130b7a2e10605eb1814464fcdd0a1863da0a26c7e6c133ea6` |
| recursive artifact manifest | `74d05a5fc4f20c69cceb0e334310157a3939b99e1a61be9d5a2fb8d8ab54caf3` |

## Frozen maximum design

The design itself is unchanged:

| Quantity | Maximum-plan count |
|---|---:|
| DGP replication tasks | 8,400 |
| Canonical waves | 110 |
| Included method--scenario cells | 89 |
| Logical endpoint/model fits | 49,200 |
| Standard MCMC chain executions | 38,400 |
| Additional preselected sentinel chains | 2,538 |
| Total MCMC chain executions | 40,938 |

The earlier precision-based stopping calculations are still produced as
descriptive evidence, but cannot truncate this run. Every maximum-plan wave is
attempted in canonical order.

## Stop and continuation rules

The following remain hard failures: invalid DGP generation; source, runtime,
provenance, seed, or artifact mismatch; diagnostic-construction failure;
nonfinite or unordered primary endpoints; loss of exact-target eligibility;
numerical repairs; and resource/process failure. Ordinary non-systemic fit
failures retain the predeclared intention-to-run denominator and missing metric
values. There is no reseeding, selective chain extension, or retry to obtain a
more favorable diagnostic result.

For a finite fit with a diagnostic warning:

1. the full diagnostic row and failed compact chain diagnostic are retained;
2. primary simulation metrics remain in the aggregate analysis;
3. the result and worker are marked explicitly as warning-bearing;
4. a warning-rate table is produced by method--scenario cell; and
5. a sensitivity table compares all finite rows with the subset that has no
   MCMC diagnostic warning.

## Interpretation

Completion will establish a fully attempted, reproducible simulation result
under the frozen generalized-Bayes interval-root update. It will not establish
that all Markov chains converged to the same distribution or that Monte Carlo
error is negligible in every cell. Any manuscript table or figure derived
from the run must carry that qualification and must expose the warning-rate and
warning-exclusion sensitivity analyses.

The outputs remain interval-root and loss-based quantities. They are not
posterior-predictive response draws, and the run does not create a response
likelihood or response-simulation contract.
