# ChatGPT Pro Output-13 independent audit of the failed bounded launch and time-zero completion

**Audit date:** 2026-07-24 UTC  
**Repository:** `AntonioAPDL/RQR-GIBBS`  
**Review base/evidence commit:** `139ba53746c4a940612ef28449c78666dba08465`  
**Corrected implementation:** `da4d265af6d8c6d6f9be06bfe2a91bfae88501d8`  
**Corrected package:** `rqrgibbs 0.1.0.9012`  
**Corrected fit schema:** `rqrgibbs_fit/1.9.0`  
**Runtime-attestation schema:** `rqrgibbs_runtime_attestation/5.0.0`  
**Estimand schema:** `rqrgibbs_dlm_bounded_estimands/1.0.0`

## 1. Evidence boundary

I audited the GitHub source and compact evidence at the exact commits specified
in the Output-12 failure follow-up. I did not execute the 24-fit grid. The audit
environment did not contain the ignored Jerez runtimes, complete run
directories, or full-chain RDS objects, so the reported local execution values
remain exact-commit tracked evidence rather than a fresh MCMC rerun.

The following exact states were inspected:

```text
Output-12 review:
  b10816bce5c06917fcd61832b7b2687803a067a0

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
```

Commit ancestry is coherent:

- `00d489...` changes the bounded flag from false to true.
- `0deebc...` is one commit ahead and changes only the executable modes of the
  two bounded-runner shell scripts.
- `da4d265...` is one implementation commit ahead of the launch source.
- `0d6433...` revokes authorization for the changed source.
- `139ba5...` is one evidence/documentation commit ahead of the revocation;
  the bounded configuration remains false there.

The protected commits exist and were treated as read-only references:

```text
AntonioAPDL/exdqlm:
  dffb71ee70b597d6a716ee74be1cbc99731cd453

AntonioAPDL/Article-Q-DESN---Version-2:
  f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

## 2. Executive decision

```text
Output-12 review packet:                         AUTHENTIC
First authorization protocol:                    FOLLOWED
First bounded execution:                         FAILED CLOSED AS CLAIMED
Completed grid fits:                              0 / 24
Published grid-chain RDS files:                   0
Time-zero conditional derivation:                 CORRECT
Positive-definite implementation branch:          CORRECT
Singular supported-subspace branch:               CORRECT
Bounded-fixture reachability of singular branch:  NONE
Statistical target after completion:              UNCHANGED
RNG/fit-schema consequence:                       CORRECTLY RECOGNIZED
Continuation and saved-state contract:            PASS
Shared reference/execute estimand boundary:        PASS
Corrected reference suite:                        PASS AS TRACKED, 43 / 43
Corrected continuation/schema cells:              PASS AS TRACKED, 6 / 6
Corrected benchmark:                              PASS AS TRACKED, 150 / 150
Current execution authorization:                  FALSE
```

**Required decision: C.** The correction and exact-commit evidence are
accepted. There is a conditional go to create one new, separate authorization
commit and rebuild all exact-source evidence before retrying the bounded grid.
The old authorization is not valid for the changed source.

This decision does not authorize a matched or production simulation. It does
not establish empirical coverage calibration, comparative forecast
performance, response-predictive validity, or production readiness.

## 3. Reconciliation-packet rehash

All thirteen objects listed by the outer manifest were fetched from
`139ba53746c4a940612ef28449c78666dba08465`. Their reconstructed byte counts and
SHA-256 values matched the outer manifest exactly:

| Path | Bytes | SHA-256 | Result |
|---|---:|---|---|
| `docs/audits/chatgpt_pro_output12_bounded_failure_reconciliation_20260724.md` | 13201 | `509b9f6c6f0f028d270dd4902c1b15fb77a351c32cede6f3736687b79d7b2c9e` | match |
| `docs/audits/rqr_dlm_output12_failed_artifact_hashes_20260724.csv` | 1209 | `cd34a7cafb12dff5dea53e8a89360a457457ef6be4aeba25ed1c7e026ff319f6` | match |
| `docs/audits/rqr_dlm_output12_failed_failure_log_20260724.csv` | 530 | `e7b77c2e03f37e756d7e68744a07e6b0c3f92ccb4d553ce930fea786366296ca` | match |
| `docs/audits/rqr_dlm_output12_failed_run_status_20260724.csv` | 3937 | `1e3fc2ef04d3da4de2f803bca9c6a2a5be2e302a9cf1bde98fbfdf4bfa98fbc6` | match |
| `docs/audits/rqr_dlm_output12_time0fix_benchmark_artifact_hashes_20260724.csv` | 3085 | `ac7652c86ccf54e8a0babcf27f0b40600d0650d15791d26c6061a36b7ba77ddf` | match |
| `docs/audits/rqr_dlm_output12_time0fix_benchmark_diagnostics_20260724.csv` | 26527 | `35367129aeb291429fda0cb077510f0beabbf7fd4e4ee0e89685d8a9d3333369` | match |
| `docs/audits/rqr_dlm_output12_time0fix_benchmark_local_chain_hashes_20260724.csv` | 1703 | `447873699d5a9b725c60a867f0697f3c53e68cdb6f0dec2f1d33a7e6ec57d05d` | match |
| `docs/audits/rqr_dlm_output12_time0fix_continuation_cells_20260724.csv` | 1998 | `260b251bfa9e3b90c1e01ce7b573cf127d25bfd059ab8983a339f587c0f6fc03` | match |
| `docs/audits/rqr_dlm_output12_time0fix_failclosed_artifact_hashes_20260724.csv` | 1026 | `906df10f66ed9b9a2bd064b630c7ee35c839ea20491ac87747ce29ae7cf490d2` | match |
| `docs/audits/rqr_dlm_output12_time0fix_preflight_artifact_hashes_20260724.csv` | 1025 | `a9444ea66c4fb0c57c25eb8600df2c421a64c4e00d3cbd239034ee3742011dca` | match |
| `docs/audits/rqr_dlm_output12_time0fix_reference_artifact_hashes_20260724.csv` | 2035 | `04c77bb704e43edc1c58355778d7a198b76c3c5a3cea98e1c07ee7892c4624f5` | match |
| `docs/audits/rqr_dlm_output12_time0fix_reference_gates_20260724.csv` | 5270 | `0de3ccced9f813fd62244d601fc2561593f5fe47dc2c3cc6b1c86fb623527f05` | match |
| `docs/audits/rqr_dlm_output12_time0fix_validation_summary_20260724.csv` | 4307 | `06cdde04bb98e79369893fd191563f81618b172454093de8568e31f445d45149` | match |

The recursive manifests are structurally coherent:

- the failed-run manifest lists the failure/status/provenance/monitor artifacts
  and no chain RDS path;
- the corrected preflight and fail-closed manifests contain no chain object;
- the reference manifest binds its gate table, reference bundle, runtime
  toolchain, session information, monitor, and continuation artifacts;
- the benchmark manifest lists the four ignored chain paths, while the tracked
  local-chain table preserves their byte counts, SHA-256 values, checkpoint
  digests, and history digests.

The ignored RDS objects and runtimes cannot be rehashed from Git and are not
claimed to have been independently recreated here. Their absence is consistent
with the declared storage policy.

## 4. Authentication of the first launch

At `0deebc753bdb29e541d5fcd34e39917b5d17774e`, the configuration is exactly:

```text
bounded_dynamic_execution_authorized = TRUE
fixtures                             = 3
learning-rate modes                  = 2
chains per cell                      = 4
burn-in per chain                    = 2,000
retained draws per chain             = 6,000
thin                                 = 1
backend                              = cpp
numerical policy                     = fail
state draws stored                   = TRUE
```

The plan therefore contains 24 prospective fits.

The tracked run-status table identifies exactly one started row:

```text
fixture:             fixed_W_local_level
learning-rate mode:  fixed_rate
chain:               1
seed:                84201
status:              failed
elapsed:             127.383 seconds
message:             expected 117 ordered estimands
```

The remaining 23 rows are still `planned`, not started. The failure ledger
contains the same schema error and records
`final_pgid_empty=TRUE`.

### 4.1 Why no chain could be published

The launch-source execution order is:

```text
fit
forecast and future checks
construct estimand matrix
strict estimand-schema validation
construct final RDS path
publish/readback/hash the RDS
```

The schema error is raised by
`rqr_bounded_validate_estimand_schemas()` before
`rqr_bounded_publish_fit_rds()` is called. Therefore the execution path cannot
create either a temporary publication object or a final chain file before this
specific error.

### 4.2 Why no later fit could start

The chain loop catches the error, records the failed status, writes the status
table, and immediately calls `stop(result)`. That exits the sourced bounded-cell
stage before the next chain index and before any later fixture/mode cell. The
23 `planned` rows independently corroborate this source-level control flow.

I found no reachable counterexample in this path to the zero-publication or
cell-level-stop claim.

### 4.3 Independent 117-versus-115 count

For the local-level fixture, \(T=24\), \(p=1\), and \(H=4\). The required fixed
mode schema is:

\[
4T+1+4p+4H
=96+1+4+16
=117.
\]

Equivalently:

```text
training lower/upper/midpoint/width:  96
observed loss:                         1
terminal midpoint/separation:          2
time-zero midpoint/separation:         2
future lower/upper/midpoint/width:     16
total:                                117
```

Before the correction, fixed-W and frozen-template fits returned
`samp.theta0_root1=NULL` and `samp.theta0_root2=NULL`. The old execution-only
extractor appended time-zero columns only inside a non-NULL conditional, so it
returned 115 columns while the independently constructed expected schema still
required 117.

## 5. Independent derivation of the time-zero conditional

For one root, write

\[
\theta_0\sim N(m_0,C_0),\qquad
\theta_1\mid\theta_0\sim N(G_1\theta_0,W_1),
\]

and

\[
R_1=G_1C_0G_1^\top+W_1.
\]

The joint first two moments are

\[
E\begin{pmatrix}\theta_0\\\theta_1\end{pmatrix}
=
\begin{pmatrix}m_0\\G_1m_0\end{pmatrix},
\qquad
\operatorname{Cov}
\begin{pmatrix}\theta_0\\\theta_1\end{pmatrix}
=
\begin{pmatrix}
C_0 & C_0G_1^\top\\
G_1C_0 & R_1
\end{pmatrix}.
\]

Gaussian conditioning therefore gives, on the support of \(\theta_1\),

\[
\theta_0\mid\theta_1\sim N(h_0,H_0),
\]

where

\[
h_0=m_0+C_0G_1^\top R_1^+
       (\theta_1-G_1m_0),
\]

and

\[
H_0=C_0-C_0G_1^\top R_1^+G_1C_0.
\]

The implemented formula is exactly this result.

## 6. Numerical implementation audit

### 6.1 Dimensions and validation

The helper converts `theta1` and `m0` to length-\(p\) vectors and requires
`G1`, `C0`, and `W1` to be \(p\times p\). `C0` and `W1` pass the common
finite/symmetry validator before use. The computed forecast and conditional
covariances are symmetrized with the overflow-safe half-plus-half operation.

### 6.2 Positive-definite branch

R's `chol(R1)` returns an upper-triangular matrix \(U\) with
\(U^\top U=R_1\). The implementation computes

```text
backsolve(U, forwardsolve(t(U), value))
```

which is \(U^{-1}U^{-\top}value=R_1^{-1}value\). The triangular-solve
orientation is correct.

The conditional covariance is sampled with

```text
conditional_mean + t(chol(H0)) %*% z
```

which has covariance \(H_0\). This orientation is also correct.

The bounded fixed-W local-level fixture has

\[
R_1=4.04,\qquad H_0=4-\frac{16}{4.04}
\approx0.03960396>0.
\]

For the frozen trend-seasonal fixture, independent reconstruction of its first
discount slice gives a positive-definite \(R_1\) and

\[
\operatorname{eigen}(H_0)
\approx(0.04,0.16,0.16,0.16,0.16).
\]

Thus both bounded fixed-evolution fixtures use the Cholesky branch, not the
singular fallback.

### 6.3 Singular positive-semidefinite branch

When Cholesky fails, the implementation:

1. eigendecomposes the symmetric \(R_1\);
2. rejects a materially negative eigenvalue;
3. defines a matrix-relative rank tolerance;
4. forms the Moore-Penrose inverse from strictly retained positive eigenpairs;
5. checks
   \((I-R_1R_1^+)(\theta_1-G_1m_0)\) against a scale-aware support tolerance;
6. rejects an innovation outside the singular Gaussian support;
7. constructs the same \(h_0\) and \(H_0\);
8. rejects a materially indefinite \(H_0\); and
9. samples from its positive eigenspace.

For the included deterministic test

```text
G1 = diag(1,0)
C0 = I
W1 = 0
theta1 = (1,0),
```

the conditional fixes the first coordinate of `theta0` at 1 and leaves the
second coordinate distributed as its prior. The implementation has that
behavior.

No jitter is added in either the forecast pseudoinverse branch or the
conditional PSD draw. Small negative eigenvalues attributable to finite
arithmetic are truncated only after the material-indefiniteness check.

I found no deterministic counterexample in which the implemented singular
branch samples from the wrong conditional or accepts a materially
out-of-support state. The branch is also not reached by the three frozen
bounded fixtures under their declared first-slice covariances.

## 7. Statistical scope of the correction

For fixed-W and frozen-template fits with state storage enabled, the existing
FFBS draw is a draw of \(\theta_{1:T}\) with \(\theta_0\) integrated through
\((m_0,C_0)\). Sampling
\(\theta_0\mid\theta_1\) afterward is an ancillary completion of that exact
joint state path.

The completed \(\theta_0\) draws are:

- not fed back into the next fixed-W/frozen-template root FFBS update;
- stored only when state draws are requested;
- swapped together with the corresponding complete root label; and
- included in the retained full-state summaries and checkpoint.

The component-scale mode is different: its existing time-zero draw remains
part of the innovation-energy and inverse-Gamma scale update. That block was
not reinterpreted by this correction. Adaptive conditional discounting remains
excluded from the bounded fixtures.

Consequently, the correction does not change the generalized-Bayes target or
the marginal root-path chain. It completes the declared joint state path.

It does consume additional random numbers. The package and fit schema were
therefore bumped, and version-1.9.0 output was revalidated independently rather
than being described as bitwise identical to version 1.8.0.

## 8. Continuation and saved-state semantics

The corrected checkpoint includes:

```text
theta_root1
theta_root2
theta0_root1
theta0_root2
latent_v
lambda
evolution_scale
rng_state
```

A global root swap exchanges both complete root paths and both time-zero
states. `rqr_dlm_continue()` restores the two time-zero states and the saved
RNG state.

For the corrected reference suite, each of the six fixture/mode cells compares:

- root ordinates;
- full retained state arrays;
- terminal state arrays;
- time-zero arrays;
- lambda;
- component-scale draws and retained conditional parameters where applicable;
- final checkpoint and checkpoint digest; and
- continuation-history generations \(0,1,2\).

All six tracked rows report:

```text
time0_draws_complete                 TRUE
estimand_schema_complete             TRUE
all_saved_stochastic_fields_bitwise  TRUE
final_checkpoint_bitwise             TRUE
three_segment_history                TRUE
```

### Nonblocking general-API observation

For fixed-W and frozen-template modes, time-zero completion is intentionally
conditional on `store_state_draws=TRUE`. Changing that storage flag changes RNG
consumption and therefore can change later bitwise draws, even though it does
not change the statistical target. The bounded protocol always freezes state
storage to `TRUE`, and all segmented comparisons use that same setting, so
this is not a reachable bounded-launch blocker.

For general API clarity, a future patch may either forbid storage-policy
changes when making a bitwise continuation claim or explicitly downgrade that
claim when the policy changes.

## 9. Shared estimand boundary

The old execution-local extractor was removed. Both reference and execute
paths now use the same two helpers:

```text
rqr_bounded_chain_estimands()
rqr_bounded_validate_estimand_schemas()
```

The expected names are generated independently from the constructed fixture.
Actual matrices must have exactly identical ordered column names, no
duplicates, and finite values. Time-zero states are now required rather than
conditionally omitted.

The independently reconstructed counts are:

| Fixture | Fixed | Learned |
|---|---:|---:|
| fixed-W local level: \(T=24,p=1,H=4\) | 117 | 118 |
| frozen trend-seasonal: \(T=36,p=5,H=4\) | 181 | 182 |
| component trend-regression: \(T=30,p=3,H=3,J=2\) | 149 | 150 |

The learned count adds only `log_lambda`. The component-scale count adds
\(J\) log scales and \(J\) innovation energies, here four columns total.

The earlier reference suite missed the defect because its fixed-W and
frozen-template continuation comparisons allowed `NULL` to match `NULL`. The
representative benchmark used the component-scale fixture, for which time-zero
states already existed. The corrected suite applies the real extractor and
strict schema to all six fixture/mode cells, so it closes this specific
coverage gap.

## 10. Corrected exact-source evidence

The recorded corrected identities are internally consistent:

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

### Reference gates

Every one of the 43 tracked rows is present and marked `TRUE`. They include
dense Gaussian FFBS truth, R/C++ parity, complete cross-time sampled moments,
canonical missingness, public future-root checks, varying component-scale
orientation, analytic component-scale conditionals, all six continuation
cells, 27 rehashed history mutations, and an active process monitor.

### Continuation/schema cells

All six tracked rows pass, with equal uninterrupted/segmented checkpoint
digests and complete time-zero and estimand contracts.

### Benchmark diagnostics

All 150 ordered rows are present and marked `TRUE`. The extrema are:

```text
maximum R-hat:
  1.00490775707187
  log_component_scale_trend

minimum bulk ESS:
  1411.39507093261
  log_component_scale_trend

minimum tail ESS:
  2041.28415820644
  log_component_scale_regression
```

The six time-zero midpoint/separation diagnostics are present and pass.
Numerical repairs, forecast repairs, and failure rows are all zero.

This is one representative four-chain component-scale/learned-rate cell. It is
not evidence that the other five cells have already mixed successfully, and it
is not the 24-fit grid.

### Fail-closed status

The corrected reference bundle passed the no-confirmation execute check and
published no chain. The execution flag is false in the revocation commit and
remains false at `139ba537...`.

## 11. Protected scope and scientific language

The RQR-GIBBS commit sequence contains no changes to either protected
repository. The tracked validation identifies the pinned exdqlm commit and
records an archive-only compatibility run under the ignored RQR-owned cache.
The Q-DESN repository is not an execution dependency for this bounded run.

The reconciliation language is appropriately restricted:

- generalized-Bayes loss update, not response likelihood;
- pseudo-AL loss augmentation, not a response model;
- interval-root/state draws, not posterior-predictive responses;
- exact fixed-joint modes versus excluded adaptive working recursion;
- same-data learned lambda, not automatic calibration;
- bounded target/mixing validation, not comparative or production evidence.

I found no sentence requiring a mathematical wording correction.

## 12. Finding disposition

| ID | Finding | Disposition | Launch effect |
|---|---|---|---|
| F1 | Exact commit ancestry and authorization/revocation sequence | Pass | none |
| F2 | Thirteen-file outer evidence rehash | Pass | none |
| F3 | Failed cell/seed/error/status authentication | Pass | none |
| F4 | Schema error occurs before RDS publication | Pass | none |
| F5 | Cell-level stop prevents every later fit | Pass | none |
| F6 | Independent 117-versus-115 diagnosis | Pass | none |
| F7 | Gaussian time-zero conditional derivation | Pass | none |
| F8 | Positive-definite triangular solves and covariance draw | Pass | none |
| F9 | Singular pseudoinverse/support/PSD branch | Pass | none |
| F10 | Fixed/frozen completion is ancillary | Pass | none |
| F11 | Component-scale and adaptive-mode scope | Pass | none |
| F12 | Schema/RNG consequence and independent revalidation | Pass | none |
| F13 | Checkpoint, swap, continuation, and orientation | Pass | none |
| F14 | Shared strict estimand boundary | Pass | none |
| F15 | Six-cell continuation/schema evidence | Pass as tracked | none |
| F16 | 43 reference gates | Pass as tracked | none |
| F17 | 150 benchmark diagnostics | Pass as tracked | none |
| F18 | Current false authorization and protected scope | Pass | none |

No reachable bounded-launch blocker remains in the audited correction.

## 13. Nonblocking hardening

These are not launch blockers:

1. Add a direct singular outside-support rejection test and a Monte Carlo
   conditional-moment test for `.rqr_draw_initial_state()`.
2. Persist the time-zero completion strategy (`chol` or supported-subspace
   eigen draw) as a diagnostic if singular models are promoted later.
3. Freeze the state-storage policy in the continuation target/bitwise contract,
   or downgrade bitwise claims when that policy changes.

The three bounded fixtures use fixed state storage and positive-definite
first-slice forecast covariances, so none of these observations supplies a
reachable failure path for the approved bounded protocol.

## 14. Required next action

Decision C permits only this sequence:

1. create a new false-to-true authorization commit based on the corrected
   source—not the old authorization commit;
2. keep every source/config change confined to that explicit authorization;
3. build a fresh isolated runtime for the exact authorized SHA;
4. rerun preflight;
5. rerun the complete reference suite and recursive rehash;
6. rerun all monitor/finalization tests;
7. rerun the no-confirmation fail-closed check;
8. require the exact confirmation environment value;
9. execute the six four-chain cells sequentially;
10. stop immediately after any chain, publication, or four-chain-cell failure.

No retries, seed replacements, extensions, retuning, or threshold changes are
permitted. The failed first attempt must remain part of the audit trail.

```text
Bounded 24-fit validation after the fresh gate sequence:
  CONDITIONAL GO

Matched or production RQR-DLM simulation:
  NO-GO

CAVI/ELBO:
  DEFER

RQR-DESN:
  DEFER
```
