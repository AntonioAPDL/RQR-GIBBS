# RQR-DLM two-ASIS failure audit and next-correction plan

Date: 2026-07-27

## Executive decision

The symmetric rootwise, two-ASIS candidate is not sufficient to authorize a
main RQR-DLM launch. The development gate
`application/cache/rqr_dlm_wave2_two_ASIS_cycles_dev2_20260727` completed all
49 chains and all 25 tasks, but only 1,147 of 1,150 diagnostics passed.

This is a fail-closed computational mixing result. It is not a statistical
target failure, not a numerical-repair failure, not an exdqlm failure, and not
a resource failure. The execution flag must remain false. No chain extension,
threshold weakening, selective retry, or reuse of failed outputs is allowed.

## Authenticated local evidence

| Artifact | SHA-256 |
|---|---|
| validation manifest | `049f8270a17f7d54e8acfa6cc30f7e80916284de28d556db3db3dfb870adc3e4` |
| diagnostics CSV | `0ef95537e8ed3061941ff27bf29c52a2b216b968a3726be6859aec2f382b5a3d` |
| summary CSV | `7d62387b6145034a041561c206e8901c6beb972cc27c21e61dee84f02f1439a3` |
| artifact manifest | `ca282695e935e15f8f6583b7d7bb1252ba381d619c3e9323d6dacd7dfac31a59` |
| chain evidence RDS | `1ad78f970e430302d4d88a00992871a5ae10f07230440934dbb5ef04c2592039` |

The gate manifest records package version `0.1.0.9025`, fit schema
`rqrgibbs_fit/1.13.0`, 32 workers, one numerical thread per worker, resource
margin pass, all fits succeeded, and no comparative simulation metrics used.
The source was intentionally dirty because this was a development gate, not a
promotion gate.

## Failure pattern

The remaining failures are concentrated in the ordinary one-chain local-level
S03 tasks. No four-chain sentinel task failed. No M02, horizon, or resource
gate was involved in this development run.

| Gate | Failed diagnostics | Failed tasks | Failed task IDs |
|---|---:|---:|---|
| one-root exact promotion at `e9c8068` | 6 / 1,150 | 5 / 25 | S03 rep 13, 55, 94, 165, 166 |
| symmetric rootwise, one ASIS cycle | 3 / 1,150 | 2 / 25 | S03 rep 13, 94 |
| symmetric rootwise, two ASIS cycles | 3 / 1,150 | 2 / 25 | S03 rep 13, 94 |

The two-ASIS candidate improved the distribution of component-scale
diagnostics overall but did not remove the floor failure:

| Gate | Minimum bulk ESS for `log_q_1` | Median bulk ESS for `log_q_1` | Maximum MCSE/SD for `log_q_1` |
|---|---:|---:|---:|
| one-root exact promotion | 94.42 | 342.78 | 0.1032 |
| symmetric rootwise, one ASIS cycle | 108.33 | 398.17 | 0.0964 |
| symmetric rootwise, two ASIS cycles | 117.73 | 437.29 | 0.0923 |

The failing two-ASIS rows are:

| DGP | Replication | Sentinel | Estimand | Bulk ESS | Tail ESS | MCSE/SD |
|---|---:|---|---|---:|---:|---:|
| S03 | 13 | false | observed loss | 188.78 | 439.19 | 0.0737 |
| S03 | 13 | false | `log_q_1` | 117.73 | 318.81 | 0.0923 |
| S03 | 94 | false | `log_q_1` | 165.68 | 366.90 | 0.0776 |

For the same two replications, the lag-one autocorrelation of `log_q_1`
remains approximately 0.93--0.94. Adding a second ASIS cycle improves the
median behavior but is not monotone in the hard cases: S03 replication 13 had
bulk ESS 199.37 under one ASIS cycle and 117.73 under two. Therefore the next
correction should not be a blind increase in ASIS cycles.

## Working diagnosis

The persistent boundary is a scale--trajectory coupling in the local-level
S03 fixed-rate component-scale role. The failures occur in ordinary one-chain
standard tasks, so the diagnostic is a within-chain effective-sample-size and
MCSE boundary, not a between-chain R-hat problem.

The symmetric rootwise partial collapse helped because it updated the scale
under both one-root Kalman marginals. However, the remaining failures suggest
that the scale update is still not moving far enough through the joint
dependence among the component scale, latent RQR scales, endpoint width, and
observed loss within the fixed 6,000-retained schedule.

Because the hard cases are few but persistent, the next step should separate
three mechanisms before selecting a correction:

1. **Transition-order dependence.** Determine whether the location of the
   centered inverse-Gamma update, rootwise partial-collapse blocks, ASIS
   blocks, latent-scale refresh, and label swap creates unnecessary
   persistence in `log_q_1`.
2. **Kernel-strength dependence.** Determine whether repeating the entire
   rootwise partial-collapse composition is more effective than repeating only
   ASIS.
3. **Schedule dependence.** Determine whether a uniform schedule increase can
   clear the hard cases at acceptable cost, after stronger exact transitions
   have been tested.

## Rejected immediate actions

The following actions are not justified:

1. Launch the main simulation from the two-ASIS candidate.
2. Commit the two-ASIS candidate to `main` as a validated implementation.
3. Relax the single-chain ESS, tail-ESS, or MCSE/SD thresholds.
4. Extend only S03 replications 13 and 94.
5. Retry with different seeds and treat a lucky pass as promotion evidence.
6. Patch or rebuild the protected exdqlm source.
7. Add more ASIS cycles without testing whether full rootwise-cycle
   repetition is the actual mixing bottleneck.

## Proposed investigation protocol

### Stage A: forensic audit script

Implement a read-only audit script that consumes the three completed M01
wave-2 evidence roots and emits compact tracked CSV summaries. The script
should report:

- failed diagnostics by gate, DGP, replication, role, and estimand;
- full quantiles of `log_q_1`, observed loss, mean width, midpoint, and
  terminal-state separation;
- autocorrelation at lags 1, 5, 10, 25, and 50 for the hard estimands;
- effective draws per second for `log_q_1`;
- whether failures cluster by replication metadata, realized response
  features, root width, or loss surface summaries;
- manifest and artifact hashes for every input evidence root.

This script must not mutate outputs and must not launch fits.

### Stage B: targeted transition comparison

Run a new development-only comparison on the two persistent hard cases,
S03 replications 13 and 94, plus one previously repaired case such as
replication 55 as a guard against overfitting. Candidate transitions should be
fixed before looking at the new diagnostics:

1. current symmetric rootwise partial collapse plus two ASIS cycles;
2. two complete rootwise partial-collapse compositions per MCMC iteration,
   followed by one ASIS cycle;
3. two complete rootwise partial-collapse compositions per MCMC iteration,
   followed by two ASIS cycles;
4. a reordered exact scan that inserts the centered inverse-Gamma update
   before ASIS and repeats the rootwise collapsed blocks afterward;
5. a uniform retained-draw schedule increase only as a diagnostic comparator,
   not as an adaptive extension.

Every candidate must be target-preserving and must leave the generalized
posterior, priors, seeds, thresholds, DGPs, and estimands unchanged.

### Stage C: selection gate

Select a correction only if it clears the hard-case comparison without
creating new failures in the guard replication and without increasing the
resource envelope beyond the declared margin. Preference should be given to
the lowest-cost exact transition that improves both `log_q_1` ESS and
observed-loss ESS.

If no transition clears the hard cases, use a prospective uniform schedule
increase for the whole affected component-scale role only after documenting
that transition improvements were insufficient. That schedule change must be
applied uniformly before a new complete-wave gate; it cannot be applied only
to failed replications.

### Stage D: complete affected-wave gate

After selecting a candidate, run the complete M01 local-level wave-2 gate
again from a fresh ignored output root. It must pass all 1,150 diagnostics.
If it fails, execution remains false and the plan returns to Stage A.

### Stage E: exact promotion and launch boundary

Only after a complete development pass should the implementation be validated,
committed fail-closed, built into an isolated runtime, and promoted through
the exact gates:

- M01 wave 1;
- M01 wave 2;
- M02 wave 1;
- M02 wave 2;
- horizon/fixed-design;
- resource envelope;
- native tests, package check, smoke, literature manifest, and TeX builds.

Only a clean exact-promotion pass can justify a separate flag-only
authorization commit and a fresh detached main simulation launch.

## Git policy for this failure

The failed two-ASIS candidate should be preserved on an audit branch, not
merged to `main` as a validated implementation. The branch may contain the
failed candidate, the failure evidence references, and this plan so the state
is reproducible. The main branch should remain at the last pushed fail-closed
source until the next correction is selected and validated.
