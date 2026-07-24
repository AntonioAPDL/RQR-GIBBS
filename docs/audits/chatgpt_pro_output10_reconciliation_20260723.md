# ChatGPT Pro Output-10 reconciliation

## Scope and exact states

This report reconciles the independent Output-10 review with the source and
execution evidence on Jerez. The review was treated as a set of claims to
reproduce, not as proof.

Reviewed external packet:

```text
branch: origin/chatgpt-pro/output10-audit-20260723
tip:    8474834219d9ba827529073bc592e02f352dd516
```

Implementation history:

```text
primary implementation: aa45e9a5aec19ecf8febe7175ea1bf1b671ed069
monitor correction:      e24feb411b2e30586d1bfdc18bf6acb1fb568c70
exact validated source:   e24feb411b2e30586d1bfdc18bf6acb1fb568c70
package:                  rqrgibbs 0.1.0.9010
fit schema:               rqrgibbs_fit/1.8.0
history schema:           rqrgibbs_continuation_history/4.1.0
fixture schema:           rqrgibbs_dlm_bounded_fixtures/5.0.0
run schema:               rqrgibbs_dlm_bounded_run/3.0.0
reference bundle:         rqrgibbs_reference_bundle/2.0.0
estimand schema:          rqrgibbs_dlm_bounded_estimands/1.0.0
```

Protected references remained unchanged:

```text
exdqlm:
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article:
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

The full before/after status of both protected repositories, including ignored
files, was identical. The exdqlm tests executed only from an immutable Git
archive built and installed below the ignored RQR-GIBBS cache.

## Executive disposition

```text
Generalized-Bayes target and alternating-FFBS mathematics: PASS, unchanged
Runtime lineage version 5:                               PASS, unchanged
Continuation history version 4 raw recursion:            PASS, hardened
Signal/error finalization:                               PASS after correction
Independent required-estimand schema:                    PASS after correction
Future retained-draw identity:                           PASS after correction
Deterministic future mixing targets:                     PASS after correction
Full-chain RDS publication/readback:                     PASS after correction
Varying component-scale future reference:                PASS after correction
6,000-retained one-cell benchmark:                       PASS
24-fit bounded grid:                                     NOT RUN; still disabled
Matched/production simulation:                           NOT AUTHORIZED
CAVI/ELBO:                                               DEFERRED
RQR-DESN:                                                DEFERRED
```

No change was made to the loss, pseudo-AL augmentation, sequential root-specific
FFBS scan, component-specific discount construction, shared component-scale
conditional, missing-data treatment, or interpretation. Output-10 supplied no
evidence that those statistical objects were wrong.

## Finding-by-finding reconciliation

| Output-10 finding | Independent audit | Correction and evidence |
|---|---|---|
| PGID signal/error exits skipped closeout artifacts | Confirmed by sending `TERM` after monitor readiness; the old wrapper exited 143 without resource, closeout, failure, or artifact files | Replaced the cleanup path with one idempotent finalizer used by normal, error, `EXIT`, `INT`, `TERM`, and `HUP` paths. It drains the PGID, waits/reaps, verifies final emptiness, records failure, and atomically writes resource, closeout, and recursive hashes |
| Required estimands were checked only for cross-chain equality | Confirmed; all four chains could omit the same block | Added an independently constructed ordered schema based on fixture dimensions and learning-rate/evolution mode. Every chain must equal it exactly before publication and again at cell diagnosis |
| `nd=n_save` permuted future draw identity | Confirmed directly; `sample.int()` permuted all saved rows | The runner now calls stochastic forecasting with `nd=NULL`, asserts `draw_index=1:n_save`, and checks draw-specific component-scale rows |
| Stochastic future paths add process-noise Monte Carlo to mixing diagnostics | Confirmed as avoidable diagnostic noise | Primary diagnostics now use deterministic future conditional-mean root functionals for each retained terminal-state draw. Stochastic future state paths remain a clearly labeled sidecar |
| Current 4,000-retained benchmark missed two bulk-ESS gates | Confirmed from tracked results: 962.08 and 971.87 | Frozen schedule increased to 6,000 retained draws with the same 2,000 burn-in, seeds, starts, thinning, and gates. No retry or post hoc retuning is allowed |
| A 45-minute whole-grid timeout was shorter than measured projected work | Confirmed | Frozen whole-grid ceiling increased to 240 minutes; process/thread/RSS ceilings are unchanged |
| Full-chain RDS files were renamed without readback validation | Confirmed | Temporary files are read back and checked for class, exact object identity, checkpoint digest, continuation history, size, and SHA-256 before atomic rename; post-rename size/hash equality is required |
| Aggregate continuation booleans used `isTRUE()` without raw scalar validation | Confirmed | All five aggregate status fields must be nonmissing logical scalars before recursive comparisons |
| RNG checkpoint values were coerced before validation | Confirmed | The complete finite integral range is validated before `as.integer()` |
| Future component-scale reference repeated one scale row | Confirmed | Added two distinct saved scale profiles, analytic group-specific moment checks, exact sequential draw-index verification, and exact scale-row orientation |

## Required-estimand contract

The exact fixed-rate counts are:

| Fixture | Fixed rate | Learned rate |
|---|---:|---:|
| fixed-W local level | 117 | 118 |
| frozen trend plus seasonal discount | 181 | 182 |
| shared component-scale trend plus regression | 149 | 150 |

The order is independently generated as:

1. lower, upper, midpoint, and width at every training time;
2. observed-data RQR loss;
3. terminal-state midpoint and absolute separation by state coordinate;
4. time-zero-state midpoint and absolute separation by coordinate;
5. deterministic future conditional-mean lower, upper, midpoint, and width at
   every horizon;
6. `log(lambda)` for the learned-rate mode;
7. named log component scales and component innovation energies for the
   component-scale mode.

Negative tests remove the same training, time-zero, future, learned-lambda, or
component-scale column from all four chains. Every omission is rejected.

These are interval-root generalized-posterior functionals. They are not
posterior-predictive response draws.

## Monitor correction and fault evidence

The wrapper now has:

- an idempotent finalizer;
- signal-specific exit status;
- TERM followed by bounded KILL escalation;
- final non-zombie PGID enumeration;
- safe zero-sample maxima;
- explicit monitor/PGID/finalizer status rows;
- a structured wrapper failure record;
- atomic resource and closeout files; and
- a final recursive artifact manifest.

`make test-dlm-monitor` passed all five scenarios:

```text
TERM after monitor readiness
monitor-command failure injection
group leader exits before a descendant
TERM-resistant child requiring KILL
zero-sample startup failure
```

Each scenario returned nonzero, left no live PGID member, produced a failure
ledger/resource summary/closeout/hash manifest, and rehashed successfully.

An exact-source preflight of `aa45e9a...` exposed one additional shell defect:
the Bash special variable `_` had been used as a drain-loop counter and was
overwritten with `0.2`. That preflight ran zero fits. The counter was renamed,
the fault suite was strengthened to reject the corresponding stderr, and the
new exact source `e24feb4...` was rebuilt and revalidated.

## Exact isolated-runtime validation

The primary runtime was built from a Git archive of
`e24feb411b2e30586d1bfdc18bf6acb1fb568c70`.

```text
application tree: f668225ea9e79fc4d3722a70ad2a78fcfdddb1b2
package:          rqrgibbs 0.1.0.9010
runtime lineage:  all required version-5 gates passed
```

### Preflight

```text
status:                    passed
prospective fits:          24
retained draws per chain:  6,000
fits executed:             0
peak sampled processes:    1 / 3
peak sampled threads:      2 / 4
peak sampled RSS:          143,884 KiB / 4,194,304 KiB
final PGID empty:          true
```

### Expanded reference-only suite

```text
reference gates:           43 / 43 passed
failure records:           0
dynamic fits:              0
peak sampled processes:    3 / 3
peak sampled threads:      4 / 4
peak sampled RSS:          189,252 KiB / 4,194,304 KiB
final PGID empty:          true
```

New varying-component-scale results:

| Gate | Value | Limit |
|---|---:|---:|
| future mean standardized error | 1.187000 | 5 |
| future variance standardized error | 1.142024 | 6 |
| sequential draw/scale orientation | pass | pass |

Key hashes:

```text
reference artifact manifest:
  ea30edebdb4a11932c009d1f4afbadfff66401fd3d762dff9412cda5f64e445e
reference gates:
  ec6964a73dc0c3c36ef974b82a2cf40e12a3e7084ffc2f7ea043e7776fe10a0f
reference bundle:
  4b2ca428686149f6361b8943d16c5ef2c0adda7c31dd77ad69fdba2944011f9d
reference run manifest:
  aa5d75fb387d2c3e45a4a3ecb242d02c40bd84c8cb3ea17a2e880c8fc1e81799
```

### Frozen 6,000-retained one-cell benchmark

The only fitted validation cell was the preauthorized shared
component-scale trend-plus-regression fixture with learned normalized loss
scale:

```text
chains:                     4
burn-in per chain:          2,000
retained per chain:         6,000
thinning:                   1
required diagnostics:       150
diagnostics passing:        150
maximum R-hat:              1.004914
minimum bulk ESS:           1,456.575
minimum tail ESS:           2,107.193
numerical repairs:          0
forecast repairs:           0
failure records:            0
```

The previously weak quantities now pass:

| Estimand | R-hat | Bulk ESS | Tail ESS |
|---|---:|---:|---:|
| log regression component scale | 1.000130 | 1,456.575 | 2,107.193 |
| regression component innovation energy | 1.000038 | 1,459.698 | 2,145.628 |

All four full chain files passed independent readback, checkpoint digest,
continuation-history validation, exact expected-commit provenance, and
artifact rehashing.

```text
peak sampled processes:    1 / 3
peak sampled threads:      2 / 4
peak sampled RSS:          396,188 KiB / 4,194,304 KiB
artifact manifest:
  2b7e84e2caa6d7c4749f7111f0a63f0d7b8c33ee4de55a3dfe86d20baeeb1dc2
diagnostics:
  a8c143287bc678b40736e5d8f962c82c78b6dab7e1d24a4f18ad9226d2c264f0
local chain hashes:
  f51c2b3f8d76ec6d4919fe475fe7f34e84e5710a0803ad198b1d292cd534bd8b
run manifest:
  0bc3ec4c8fe7967c5574b9177be9c4f883816c16bd4753ce2ae56217306c780d
```

Benchmark mode records prospective diagnostic gates descriptively. The new
result is nevertheless direct evidence that the predeclared longer schedule
resolved the two Output-10 ESS failures in the hardest declared cell.

### Fail-closed execution proof

Execute mode was supplied:

- the exact reviewed source SHA;
- the matching reference directory and artifact-manifest SHA;
- the matching runtime/toolchain; and
- the exact 24-fit confirmation phrase.

It returned status 1 with:

```text
status:                      blocked_by_execution_contract
reference_binding_verified:  true
execution_authorized:        false
full chain files:            0
final PGID empty:            true
```

Thus the 24 fits cannot be started by changing environment variables alone.

## Other completed validation

```text
R CMD check --no-manual:      Status: OK
bounded config tests:         pass
native R/C++ tests:           pass
shell syntax/diff checks:     pass
environment smoke:            pass
main PDF:                     pass, 9 pages
supplement PDF:               pass, 10 pages
literature manifest:          pass, 18 local PDFs
exdqlm focused RQR tests:      pass from archive runtime
protected repository guards:  unchanged, including ignored files
```

No heavy chain file, TeX build product, compiler object, fitted model, or
simulation output is tracked. The tracked CSV/JSON files accompanying this
report are compact copies of the exact evidence tables and manifests.

## What was intentionally not adopted

1. The public `rqr_forecast_roots(nd=...)` API was not redefined. Explicit
   `nd` continues to request sampling/resampling; `nd=NULL` is the documented
   all-draw sequential path. The runner now uses the latter and asserts it.
2. No cgroup-hard memory claim was added. Jerez uses the reviewed PGID sampled
   fallback, with sampled RSS clearly labeled as telemetry.
3. The 24-fit authorization flag was not enabled.
4. No matched/production coverage study was launched.
5. No CAVI/ELBO derivation or implementation was started.
6. No RQR-DESN work was resumed.
7. Neither protected reference repository was edited, built in place, or
   loaded from source.

## Current decision and next gate

The source and evidence are ready for another independent review. They are not
self-authorization for the 24-fit grid.

The next reviewer should decide whether:

1. the idempotent wrapper finalizer and five fault scenarios close the monitor
   blocker;
2. the independently generated exact estimand schema closes the completeness
   blocker;
3. deterministic future conditional-mean diagnostics plus sequential
   stochastic sidecars close the draw-identity blocker;
4. readback-verified RDS publication closes the chain-integrity blocker;
5. the 43/43 reference suite and 150/150 one-cell benchmark support a
   conditional go for the bounded 24-fit grid.

If independent review gives a go, authorization must still be a separate,
explicitly reviewed source commit. Enabling the configuration changes its
digest, so that commit must receive a new exact isolated runtime, preflight,
reference bundle, and user confirmation before execution.

Even a successful 24-fit bounded run would validate target mechanics,
numerics, provenance, continuation, and mixing only. It would not establish
empirical coverage calibration or forecasting performance. The matched
simulation protocol remains a later, separate gate. CAVI/ELBO remains after
the MCMC contract and matched RQR-DLM design are stable; RQR-DESN remains
deferred until the RQR-DLM program is complete.
