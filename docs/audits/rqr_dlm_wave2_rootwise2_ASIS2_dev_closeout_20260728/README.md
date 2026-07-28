# RQR-DLM rootwise2-ASIS2 M01 wave-2 development closeout

Date: 2026-07-28

## Executive decision

The fresh complete M01 local-level wave-2 development gate for the selected
`rootwise2_ASIS2` transition **passed**.

This result supports moving to a fail-closed exact-promotion pass from a fresh
isolated runtime.  It does **not** authorize the main simulation by itself.
The main simulation flag remains false, and this development output must not be
reused as promotion output.

## Source and run boundary

```text
source commit: 440b772af56f19515d3433627e019f45fe1bc825
package:       rqrgibbs 0.1.0.9027
wave:          local_level_gaussian_T200__target0200__sentinel
output root:   application/cache/rqr_dlm_wave2_rootwise2_ASIS2_dev_20260728_20260728T092537Z
```

The run used the selected component-scale kernel:

```text
symmetric_rootwise_partially_collapsed = TRUE
collapsed_integrated_roots             = root1, root2
collapsed_cycles                       = 2
interweave_cycles                      = 2
transition_order                       = rootwise_then_interweave
selected_candidate                     = rootwise2_ASIS2
```

The generalized-Bayes target, priors, DGPs, seeds, diagnostics, and thresholds
were unchanged.  No response likelihood or posterior-predictive response
contract is introduced by this gate.

## Gate result

| Check | Result |
|---|---:|
| Tasks | 25 / 25 |
| Chains | 49 / 49 |
| Diagnostics | 1150 / 1150 passed |
| Fits succeeded | true |
| Exact target-preserving kernel | true |
| Failed outputs reused | false |
| Comparative simulation metrics used | false |
| Maximum process peak RSS | 761,756 KiB |
| Declared worker memory ceiling | 1,572,864 KiB |
| Resource margin | pass |
| Started | 2026-07-28 09:26:30 UTC |
| Completed | 2026-07-28 14:18:25 UTC |

## Hard-case behavior

The three hard/guard replications that motivated the correction all passed:

| DGP | Replication | Role | `log_q_1` bulk ESS | `log_q_1` tail ESS | `log_q_1` MCSE/SD |
|---|---:|---|---:|---:|---:|
| S03 | 13 | persistent hard case | 290.33 | 608.20 | 0.0587 |
| S03 | 55 | repaired guard case | 359.11 | 677.59 | 0.0527 |
| S03 | 94 | persistent hard case | 291.56 | 676.75 | 0.0587 |

The minimum `log_q_1` bulk ESS across the full gate was 290.33, the minimum
tail ESS was 608.20, and the maximum MCSE/SD was 0.0587.  These clear the
predeclared one-chain thresholds.

## Interpretation

The previous failures were computational mixing failures in the component-scale
transition, not model or target failures.  This complete affected-wave gate
confirms that repeating the full symmetric rootwise partial-collapse
composition, while retaining the ASIS interweaving, clears the local-level M01
hard cases under the fixed schedule.

This is development evidence for the implementation path.  Promotion still
requires an isolated runtime built from the exact committed source and fresh
exact gates.

## Required next step

Proceed to exact promotion only after confirming a clean worktree and exact
source state.  The next promotion sequence is:

1. Build a fresh isolated primary runtime from the exact fail-closed commit.
2. Run M01 wave 1.
3. Run M01 wave 2.
4. Run M02 wave 1.
5. Run M02 wave 2.
6. Run the horizon/fixed-design gate.
7. Run the resource envelope.
8. Run package/native/TeX/literature checks.
9. If all promotion gates pass, create a separate flag-only authorization
   commit.
10. Launch a fresh main simulation under a new run ID.

Any failure in promotion keeps execution false.

## Tracked compact artifacts

- `run_summary.csv`
- `wave2_M01_summary.csv`
- `wave2_M01_diagnostics.csv`
- `diagnostic_summary_by_estimand.csv`
- `hardest_log_q_tasks.csv`
- `validation_manifest.json`
- `source_artifact_hashes.csv`
- `artifact_hashes.csv`

The heavy chain evidence RDS remains local-only under `application/cache/`.

