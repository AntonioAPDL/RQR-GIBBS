# RQR-DLM wave-2 recovery integration and promotion plan

Date: 2026-07-31  
Scope: confirmatory RQR-DLM simulation  
State: fail-closed until every gate below succeeds

## Executive decision

The failed second canonical wave contains two computationally different
problems and therefore should not receive one generic correction.

1. **M03 fixed-design RQR:** S03 replication 117 exhibits persistent
   four-chain basin separation. Longer burn-in, longer retention, and repeated
   ordinary Gibbs compositions do not resolve it. The selected correction is
   exact likelihood-tempered replica exchange with inverse temperatures
   `(1, .45, .20, .09)`, adjacent swaps every iteration, and retention of the
   cold replica only.
2. **M08 true-fixed-W RQR-DLM:** S03 replication 13 is a marginal one-chain
   precision failure: bulk ESS is 193.86 against the frozen threshold of 200,
   while tail ESS and MCSE/SD pass. The selected correction is the uniform
   M08 schedule `burn=1000, retain=4000, thin=1`.

Both changes preserve the declared generalized posterior. Neither changes the
loss, prior, data, seeds, response laws, estimands, or diagnostic thresholds.
Development outputs are used only to choose a computational transition and a
fixed method-level schedule; they are never reused as scientific results or
promotion evidence.

## Evidence and diagnosis

The comparison was bound to source commit
`c2d560d761aae35554cadfe417e11a65ef540043`, the reviewed maximum seed ledger
SHA-256
`3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f`,
and unchanged diagnostics.

For M03, all ordinary candidates failed the hard replication even with burn-in
3,000, retention 6,000, or two complete local kernels. The four replica-
exchange ladders all passed the hard and three guard cases; the preregistered
least-cost rule therefore selected REX4. Across its four cases, minimum bulk
ESS was 1,537.9, maximum rank-normalized R-hat was 1.0039, minimum adjacent
swap acceptance was .342, and every fit recorded hundreds of complete
hot-to-cold round trips with zero numerical repairs.

For M08, the original 2,000-draw schedule reproduced six duplicated failures
of the same terminal/future lower-root scalar in replication 13. The uniform
4,000-draw schedule passed all 45 monitored estimands in replications 13, 55,
and 94. Its smallest bulk ESS was 385.6, its smallest tail ESS was 1,057.6,
and its largest MCSE/SD was .0522, with zero repairs.

The compact development evidence is recorded in
`docs/audits/rqr_dlm_wave2_candidate_selection_20260731/`. Raw scalar chains,
fits, logs, and runtime files remain under ignored local roots.

## Exactness of the M03 correction

For ridge prior density `p0(beta1,beta2)`, fixed learning rate `omega_R`, and
RQR loss `L`, replica `j` targets

```text
p0(beta1,beta2) exp{-t_j omega_R L(beta1,beta2)},
```

where `1=t_1>...>t_J>0`. The local pseudo-AL Gibbs transition uses learning
rate `t_j omega_R` and preserves that marginal root target. An adjacent swap
of root states has log Metropolis ratio

```text
(t_i-t_j){omega_R L(x_i)-omega_R L(x_j)}.
```

The common prior cancels. Auxiliary pseudo-AL scales are not part of the swap:
each subsequent local transition completely refreshes them conditional on its
current root state. Only draws at `t_1=1` are retained. Thus the cold chain has
the original fixed-design RQR generalized posterior; this is not a response
likelihood or a posterior-predictive response simulation.

The public implementation remains restricted to fixed-rate, zero-tilt ridge
RQR without stored latent scales. Learned loss scales, RHS priors, dynamic
state paths, and continuation fail closed until separately derived and tested.

## Frozen integration contract

- Apply REX4 to every M03 role, including standard and sentinel fits.
- Retain the existing role-specific M03 schedules: 500/3,000 for standard
  chains and 500/1,500 for sentinel chains.
- Initialize the cold replica from the already declared chain profile. Hot
  replicas use profiles A, C, and D in that order; this is deterministic and
  consumes no new seed.
- Record attempts, accepts, acceptance by edge, cold-label visits, and round
  trips. Energy-trace storage may be disabled in the main study because it does
  not alter the transition or its RNG stream.
- Apply 1,000/4,000 only to M08. M06 and other dynamic RQR methods keep their
  existing schedules.
- Multiply M03 primitive-iteration budgets by four replicas and reproduce the
  complete initial, central, and maximum budgets from the incidence matrix.
- Keep both authorization flags false throughout implementation and promotion.

## Validation state machine

### Gate A: source and unit contracts

1. Exact swap-ratio algebra and malformed-control rejection.
2. Disabled replica exchange bitwise-identical to the ordinary sampler.
3. Integrated M03 construction reproduces the frozen ladder and deterministic
   hot starts for standard and A--D profiles.
4. M08 alone receives the 4,000-draw schedule.
5. Budget, schema, provenance, and serialization tests pass.

### Gate B: targeted stress promotion

Run fresh production-routed fits for M03 S03 replications 117, 13, 90, and 185
with four chains, and M08 S03 replications 13, 55, and 94 with one chain.
Require every unchanged diagnostic, exact-target flag, numerical-repair check,
and replica-exchange operational check to pass. Candidate chains cannot be
reused.

### Gate C: complete affected-wave development validation

Run every included M03 and M08 fit in the full local-level wave-2 task set from
a fresh ignored output root. Every diagnostic must pass. The subsequent exact-
runtime promotion also repeats the 1,150-check M01 wave-2 gate. A failure stops
the lane; there is no per-replication extension, reseeding, or threshold change.

### Gate D: exact-runtime promotion

Commit the fail-closed implementation, build a fresh isolated runtime from its
full commit, and run M01 waves 1--2, M02 waves 1--2, horizon/fixed-design,
targeted M03/M08 stress, resource, package/native, TeX, literature, and fresh
canonical-wave gates. Promotion artifacts must identify the exact source,
runtime tree, config, incidence, seed ledger, and hashes.

### Gate E: authorization and launch

Only a tracked passing closeout may be followed by a separate flag-only commit.
The authorization diff must change `confirmatory_execution_authorized` from
false to true and nothing else. A fresh run ID then starts the 8,400-task study
from zero; no failed or development output is resumed.

## Rejected alternatives

- More ordinary M03 iterations or local-kernel repetitions: empirically failed
  the frozen hard case and do not bridge the observed basins.
- Selective chain extension, reseeding, or choosing a lucky rerun: invalidates
  the prospective method-level Monte Carlo contract.
- Weaker diagnostics: conceals rather than corrects the failure.
- Applying M08's schedule to M06: unnecessary cost and unsupported coupling of
  two distinct method contracts.
- Promoting candidate evidence: it is tuning evidence from named hard cases,
  not independent promotion evidence.

## Failure and reproducibility policy

Every gate writes atomically, records structured failure evidence before
stopping, and hashes compact artifacts. Heavy fits and scalar-chain evidence
remain ignored. A nonzero process exit, missing manifest, dirty source state,
hash mismatch, numerical repair, non-exact target, failed diagnostic, or
incomplete replica-exchange traversal is terminal for that run. The exdqlm and
Q-DESN repositories remain read-only references.
