# Codex handoff after ChatGPT Pro Output-16

## Reviewed immutable states

```text
access path: attached exact archive
input archive SHA-256:
  7fa1c754940e9001cee2b8069297e70f6615b3d74647da826504f9a237fdcbef
reconciliation branch tip:
  4b95064b01350a6b020146b189dc9d44936aed8f
reviewed implementation commit:
  7b7c47204801032e5eb4fe6c9fd332aaaedead43
independently reconstructed application tree:
  29f938a8359e0c8bf23c41584f91c0b1fd38e25b
package:
  rqrgibbs 0.1.0.9017
```

Do not modify exdqlm or the Q-DESN repository. Preserve the generalized-Bayes interval-root interpretation. Do not introduce a response likelihood, response-predictive RQR scores, or a disposable pilot.

## Decision

```text
Create flag-only authorization commit now: NO-GO
Rebuild authorized runtime now:           NO-GO
Launch confirmatory study now:            NO-GO
Run another disposable pilot:             NO-GO
Implement the two narrow corrections:     GO
```

The design matrix, DGP construction, complete RNG allocation, comparator contracts, population oracle, runtime lineage, flag-only authorization check, replication MCSE rules, collection contract, and tracked preflight/reference evidence were accepted. Two concrete launch blockers remain.

## Blocker B1 — enforce canonical wave progression

### Current defect

`application/scripts/17_launch_rqr_dlm_confirmatory_wave.R` accepts any caller-selected canonical `wave_id`. It verifies that the supplied full plan is canonical and that the selected wave's mode matches, but it does not verify that:

- prior required waves completed;
- a standard wave's same-batch sentinel wave passed; or
- the preceding complete paired batch decision authorized the next batch.

The canonical sort in `rqr_confirm_wave_plan()` is planning metadata, not an execution state machine.

### Required minimal implementation

Implement an authorization-bound, append-only run/wave state contract or a single `launch-next-wave` coordinator.

Required raw fields per wave:

```text
run_id
authorization_commit
reviewed_implementation_commit
runtime_tree_digest
config/incidence/seed/task_plan digests
wave_id
canonical_wave_index
mode
batch_group
batch_target
phase
required_predecessor_wave_ids
predecessor_manifest_sha256 values
same_batch_sentinel_pass
prior_batch_decision_sha256
prior_batch_next_action
worker/task counts
wave decision
artifact-manifest SHA-256
```

Required rules:

1. Only the next canonical wave may create an output root.
2. A standard wave requires the same-batch sentinel pass.
3. A later batch requires the previous complete paired-batch decision to say `add_complete_paired_DGP_batch`.
4. A stopped precision group cannot launch later waves.
5. Replay, skipping, cross-run evidence, and duplicate wave IDs fail before publication.

Required deterministic negative tests:

```text
standard before sentinel
standard after failed sentinel
skip a prior batch
replay an already completed wave
use predecessor evidence from another run/authorization/runtime
launch after precision_pass_stop
```

## Blocker B2 — diagnose terminal and future root functions

### Current defect

`rqr_confirm_scalar_draws()` keeps only time-averaged lower/upper/midpoint/width, observed loss, optional log-lambda, and optional log-component-scales. `rqr_confirm_chain_diagnostics()` uses an intersection and requires only those aggregate values.

A compensating time-local path can preserve all aggregate diagnostic variables while changing the terminal state and future roots materially.

### Required minimal implementation

Define one explicit method-aware diagnostic schema. Do not infer it from observed intersections.

For dynamic RQR sentinel chains require:

```text
terminal lower/upper/midpoint/width
future conditional-mean lower/upper/midpoint/width at h=1,5,10,20
training lower/upper/midpoint/width at a frozen time grid
  including first, last, break boundary, missing boundary, and scale boundary
log(lambda) for learned-scale mode
log(q_j) for every component-scale state block
observed RQR loss and existing time averages
```

Using all training-time endpoint functions for sentinel chains is acceptable and preferable if artifact size remains within the existing compact limits. Future diagnostic functions must propagate each retained terminal state without process noise or generated responses.

For single standard chains, use the corresponding scalar set with maintained ESS/MCSE gates. Fixed lambda remains an exact identity check and is excluded from stochastic diagnostics.

Required tests:

```text
missing required terminal variable -> fail
missing required future-horizon variable -> fail
all chains omit the same required variable -> fail
compensating path with equal aggregate means but different terminal/future roots -> detected
component-scale and learned-scale required fields enforced by method
```

Replace `Reduce(intersect, ...)` with exact required-set validation. Extra sidecars may be allowed only under an explicit schema policy.

## Nonblocking correction

Refresh the stale Output-16 prompt row in:

```text
docs/audits/rqr_dlm_output15_reference_evidence_20260725/
  tracked_evidence_hashes.csv
```

The final prompt in the reviewed archive is 14,932 bytes with SHA-256:

```text
57b9fdd6aba650887a308cde99d19305bc0da805288b4aa040839de6f8f73a1f
```

This is documentation/evidence housekeeping and is not a scientific blocker.

## Validation required after the patch

Do not run the confirmatory study. Run and record:

```text
source parse and exact diff review
full native and confirmatory contract tests
R CMD check --no-manual
main and supplement builds
exact isolated primary runtime build and attestation
22-gate preflight or its reviewed successor
15-gate oracle/reference suite or its reviewed successor
wave-order/state-machine negative tests
expanded terminal/future diagnostic tests
direct and wave fail-closed tests with authorization flag FALSE
protected-repository before/after guards
```

Return a compact reconciliation packet and a new independent-review prompt. A favorable later review may approve the same flag-only authorization design directly on top of the corrected implementation. No pilot or scope expansion is requested.
