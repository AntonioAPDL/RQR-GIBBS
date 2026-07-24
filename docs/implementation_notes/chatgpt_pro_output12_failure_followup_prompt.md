# ChatGPT Pro Output-12 failure follow-up prompt

Perform an independent source and evidence audit of the first authorized
bounded RQR-DLM execution attempt and its time-zero state correction in the
public `AntonioAPDL/RQR-GIBBS` repository. Treat the Codex reconciliation,
tests, and tracked execution artifacts as claims to verify, not as proof.

Do not ask me to upload files. Read every required source and evidence file
from the GitHub remote at the exact commits below. Do not edit implementation,
configuration, manuscript, or existing evidence. Do not execute the 24-fit
grid. Your only write action is to push the four review deliverables specified
at the end to a new review branch.

## Exact states

```text
Output-12 review:
  branch: chatgpt-pro/output12-audit-20260724
  commit: b10816bce5c06917fcd61832b7b2687803a067a0

first authorization:
  00d489d686b44622454333d225f5ce55e1f760a5

authorized launch source:
  0deebc753bdb29e541d5fcd34e39917b5d17774e

time-zero correction:
  da4d265af6d8c6d6f9be06bfe2a91bfae88501d8

authorization revocation:
  0d64331732fe4118e7234f6f23a851f5d98e6614

compact reconciliation evidence:
  139ba53746c4a940612ef28449c78666dba08465

corrected package:
  rqrgibbs 0.1.0.9012

corrected fit schema:
  rqrgibbs_fit/1.9.0

runtime attestation schema:
  rqrgibbs_runtime_attestation/5.0.0

estimand schema:
  rqrgibbs_dlm_bounded_estimands/1.0.0
```

The committed bounded execution flag at the evidence commit is `FALSE`. The
prior authorization cannot authorize changed source, and no second grid
attempt has been made.

Protected read-only references:

```text
exdqlm:
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article:
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

Do not propose or make changes in either protected repository.

## Fixed scientific interpretation

Preserve these distinctions:

- RQR is a generalized-Bayes interval-root loss update.
- The normal-exponential pseudo-AL construction augments a pseudo-residual
  loss kernel; it is not an ordinary response likelihood.
- Root and state draws are not posterior-predictive response draws.
- Fixed `W`, frozen discount-template, and shared component-scale modes use
  exact alternating root-specific FFBS full-conditionals for their declared
  fixed joint targets.
- Adaptive conditional discounting is an experimental working recursion and
  is excluded from the bounded validation.
- Same-data learned `lambda` is not automatic coverage calibration or a
  response variance.
- Bounded validation does not establish empirical coverage calibration,
  comparative forecast performance, or production readiness.

No loss, pseudo-AL augmentation, root-state FFBS conditional, component-scale
conditional, missing-observation rule, discount construction, or response
interpretation was intentionally changed by the correction.

## Read and rehash the complete reconciliation packet

Read completely:

```text
docs/audits/chatgpt_pro_output12_bounded_failure_reconciliation_20260724.md
docs/audits/chatgpt_pro_output12_bounded_failure_reconciliation_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output12_failed_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output12_failed_failure_log_20260724.csv
docs/audits/rqr_dlm_output12_failed_run_status_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_preflight_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_reference_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_reference_gates_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_continuation_cells_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_failclosed_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_benchmark_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_benchmark_diagnostics_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_benchmark_local_chain_hashes_20260724.csv
docs/audits/rqr_dlm_output12_time0fix_validation_summary_20260724.csv
```

Recompute all 13 byte counts and SHA-256 values in the outer manifest. Inspect
the recursive manifests structurally and rehash every compact tracked artifact
available in Git. Full chain RDS objects, runtimes, and run directories are
intentionally ignored; do not treat their absence from Git as a defect.

## 1. Authenticate the failed launch

At `0deebc753bdb29e541d5fcd34e39917b5d17774e`, verify that the exact
configuration had:

```text
bounded_dynamic_execution_authorized = TRUE
3 fixtures
2 learning-rate modes
4 chains
2,000 burn-in
6,000 retained draws
thin 1
backend cpp
numerical policy fail
cell-level stopping
```

Use the tracked failure ledger, run status, and recursive manifest to verify:

```text
failed cell:
  fixed_W_local_level / fixed_rate / chain 1 / seed 84201

error:
  expected 117 ordered estimands

completed fits:
  0

failed fits:
  1

unstarted fits:
  23

published chain RDS files:
  0

final PGID empty:
  true
```

Inspect the runner and publication helper at the launch commit. Decide whether
the schema failure necessarily occurs before final RDS publication and whether
the cell-level stop rule necessarily prevents every later chain and cell.
Report an exact counterexample only if one is reachable from this execution
path.

Independently confirm the 117-versus-115 calculation for the local-level
fixture:

```text
4 * 24 training root functionals = 96
observed loss                    =  1
terminal state functionals       =  2
time-zero state functionals      =  2
4 * 4 future root functionals    = 16
total                            = 117
```

Confirm that the former fixed-W fit supplied no retained time-zero states and
that the former execution extractor omitted those two columns conditionally.

## 2. Audit the statistical time-zero correction

Inspect the complete correction diff at
`da4d265af6d8c6d6f9be06bfe2a91bfae88501d8`, especially:

```text
application/R/rqr_evolution.R
application/R/rqr_dlm_fit.R
application/R/rqr_utils.R
application/scripts/lib/rqr_dlm_bounded_diagnostics.R
application/scripts/08_run_rqr_dlm_bounded_validation.R
application/scripts/09_run_rqr_dlm_bounded_cells.R
application/tests/testthat/test-rqr-native-sampler.R
application/tests/testthat/test-rqr-dlm-bounded-config.R
application/DESCRIPTION
Makefile
application/README.md
```

For

```text
theta_0 ~ N(m0, C0)
theta_1 | theta_0 ~ N(G1 theta_0, W1)
R1 = G1 C0 G1' + W1,
```

derive independently and verify the implemented conditional:

```text
theta_0 | theta_1 ~ N(h0, H0)

h0 = m0 + C0 G1' R1^+ (theta_1 - G1 m0)
H0 = C0 - C0 G1' R1^+ G1 C0.
```

Audit both numerical branches:

1. positive-definite `R1` solved by Cholesky;
2. singular positive-semidefinite `R1` solved on its supported subspace.

Check dimensions, triangular-solve orientation, Moore-Penrose construction,
rank tolerance, innovation support test, materially indefinite rejection,
conditional covariance construction, PSD sampling, and absence of undeclared
jitter. Give a deterministic numerical counterexample if any branch samples
from the wrong conditional or accepts a state outside singular support.

Verify the scope:

- fixed-W and frozen discount-template modes complete `theta_0` only when
  state draws are stored;
- those completion draws are ancillary and are not fed into later fixed-W or
  frozen-template state updates;
- component-scale mode keeps its existing time-zero role in the scale
  conditional;
- adaptive conditional discounting remains excluded.

Decide whether adding these ancillary draws changes the statistical target or
only completes the declared joint state path. Also verify the important RNG
consequence: the additional draws change the stream, so version 1.9.0 results
must be validated on their own and cannot be claimed bitwise identical to
version 1.8.0.

## 3. Audit continuation and saved-state semantics

Inspect the fit checkpoint and continuation paths. Verify that:

- both time-zero root states are in the checkpoint;
- root-label swaps also swap the time-zero states;
- fixed-W, discount-template, and component-scale retained draws have the
  declared `p x n_save` orientation;
- continuation restores the RNG and both time-zero states;
- `6` uninterrupted draws and `2+2+2` segmented draws compare every saved
  stochastic field, including time-zero arrays; and
- all six fixture/mode cells validate generation sequence, final checkpoint,
  and complete estimand schema.

Look specifically for a storage-dependent RNG inconsistency between
`store_state_draws=TRUE` and continuation. The bounded run always requests
state draws, so distinguish a general API observation from a reachable bounded
launch blocker.

## 4. Audit the shared estimand boundary

Verify that reference and execute modes now call the same
`rqr_bounded_chain_estimands()` implementation and that the old duplicate was
removed.

Check that actual matrices must match the independently constructed expected
column names exactly and in order. No expected column may be conditionally
omitted, silently intersected, duplicated, reordered, or nonfinite.

For all three fixtures and both learning-rate modes, independently reconstruct
the expected column counts. Confirm the extra learned `log_lambda` and
component-scale/innovation-energy columns occur only in their declared modes.

Explain precisely why the earlier reference and representative benchmark
missed the defect, then decide whether the new six-cell actual-schema gate
closes that coverage gap.

## 5. Audit corrected exact-source evidence

At the corrected implementation commit, verify the recorded identities:

```text
application tree:
  5b50eb8fcb5e4748fbdc40662c81b0657edfad38

source archive SHA-256:
  2d2e50c8895360a00b9e165080608723bfc14fb9ef6215607f0d624b01ca2d9e

source package SHA-256:
  9d42c933663bf6b584130e5ace4a0561bddf4d3ac62326bb7ff8524cd18761fa

runtime tree digest:
  09ee9a6774f24aa35cf0e196e44d4cd36cb9c11294c827b80d7f6f0dace05363

runtime attestation SHA-256:
  bb2a82cc744b7f078cfb0061f9b5d662061838d33595ac1d81134e265593e435

runtime toolchain digest:
  13e4a079e76d87a6542fdc6e8718899258f541f20879677d51bb770a17f6cee9
```

Audit every row of:

```text
43 / 43 reference gates
6 / 6 continuation/schema cells
150 / 150 benchmark diagnostics
```

Confirm:

```text
maximum R-hat:       1.004908 <= 1.01
minimum bulk ESS: 1,411.395   >= 1,000
minimum tail ESS: 2,041.284   >= 1,000
numerical repairs:   0
forecast repairs:    0
failure rows:        0
```

Treat these as exact-commit tracked local execution evidence, not as an
independent rerun if your environment lacks R. Do not overstate the benchmark:
it is one representative four-chain cell, not the 24-fit grid.

Verify that the exact corrected reference bundle passes the no-confirmation
fail-closed check with zero chain files and that the committed flag is false at
`0d64331732fe4118e7234f6f23a851f5d98e6614` and at the evidence commit.

## 6. Protected scope and interpretation

Confirm from Git history and tracked evidence that neither protected repository
was mutated by this pass. The exdqlm test must remain archive-only under the
ignored RQR-GIBBS cache.

Audit the reconciliation language for the generalized-Bayes, root-versus-
response, exact-versus-working, and bounded-versus-empirical distinctions.
Recommend a wording correction only when a specific sentence is mathematically
wrong or materially overclaims the evidence.

## Required decision

Return an explicit finding-by-finding disposition and one of:

```text
A. correction rejected; exact blocker and reproducer supplied;
B. correction accepted but another reachable bounded-launch blocker remains;
C. correction and evidence accepted; conditional go to create one new,
   separate authorization commit and rebuild exact evidence before retry.
```

Do not make hypothetical or adversarial edge cases launch blockers without a
reachable path and material consequence for this bounded runner. Nonblocking
hardening suggestions must be labeled separately.

Even under decision C:

- do not authorize the old true-flag commit for the changed source;
- do not run the 24 fits from a false-flag commit;
- require a fresh exact authorization commit, isolated runtime, preflight,
  complete reference bundle, monitor suite, fail-closed check, and explicit
  confirmation environment value;
- retain cell-level stopping and forbid retries, retuning, seed changes, or
  threshold changes;
- do not authorize matched/production simulation;
- defer CAVI/ELBO; and
- defer RQR-DESN.

## Required remote deliverables

Create this branch from exact evidence commit
`139ba53746c4a940612ef28449c78666dba08465`:

```text
chatgpt-pro/output13-audit-20260724
```

Push exactly these four new files:

```text
external_reviews/chatgpt_pro_output13_20260724/
  chatgpt_pro_output13_audit_20260724.md
  chatgpt_pro_output13_codex_handoff_20260724.md
  chatgpt_pro_output13_findings_20260724.csv
  chatgpt_pro_output13_artifact_hashes.csv
```

The artifact manifest must contain the relative path, byte count, and SHA-256
for the other three deliverables. The Codex handoff must be copy-paste ready
and include exact commits, accepted and rejected findings, any remaining
blocker, and the next permitted action.

Do not modify any other file. Do not merge the branch to `main`.

Your final chat response must contain only:

```text
chatgpt-pro/output13-audit-20260724
<full 40-character pushed branch tip SHA>
```
