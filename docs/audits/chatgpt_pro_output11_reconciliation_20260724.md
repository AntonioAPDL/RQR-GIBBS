# ChatGPT Pro Output-11 reconciliation

## Scope and exact states

This report treats the Output-11 review as a set of claims to reproduce rather
than as proof. The exact remote review packet was:

```text
branch: origin/chatgpt-pro/output11-audit-20260723
tip:    7b242071a7e7d33ebdead540bfadd4969e312411
base:   99b6f92911a5cd323b735598063da72766ad9095
```

All three delivered review files matched the byte counts and SHA-256 values in
the fourth delivered hash manifest. The four review commits were integrated
without merging the review branch's older ancestry.

The exact corrected implementation is:

```text
implementation commit:
  53dc71d873ef12ebba91cbc3d6813682e0987960

package:
  rqrgibbs 0.1.0.9011

fit schema:
  rqrgibbs_fit/1.8.0

continuation schema:
  rqrgibbs_continuation_history/4.1.0

runtime attestation:
  rqrgibbs_runtime_attestation/5.0.0

bounded fixture schema:
  rqrgibbs_dlm_bounded_fixtures/5.0.0

bounded run schema:
  rqrgibbs_dlm_bounded_run/3.0.0

reference bundle:
  rqrgibbs_reference_bundle/2.0.0

estimand schema:
  rqrgibbs_dlm_bounded_estimands/1.0.0

wrapper closeout:
  rqrgibbs_dlm_wrapper_closeout/2.1.0
```

The 24-fit flag remains:

```text
bounded_dynamic_execution_authorized = FALSE
```

No member of the 24-fit grid was executed.

Protected repositories remained read-only:

```text
exdqlm
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

The exdqlm compatibility path created a Git archive and built and loaded only
from the ignored RQR-GIBBS cache. Its guarded before/after checkout comparison,
including ignored compiler objects, passed. A separate corrected snapshot
check around the installed-archive smoke test also passed. An earlier auxiliary
snapshot command attempted to hash ignored paths from the wrong working
directory; its digest was explicitly discarded and is not evidence used here.

## Executive disposition

```text
Generalized-Bayes target and interpretation:          PASS; unchanged
Alternating root-specific FFBS construction:          PASS; unchanged
Runtime lineage version 5:                            PASS; unchanged
Required-estimand and future-root contracts:           PASS; unchanged
Continuation-history validation:                      PASS; unchanged
Wrapper enumeration failure propagation:              PASS after correction
Canonical output target handling:                     PASS after correction
Manifest/actual regular-file-set equality:             PASS after correction
Full-chain post-rename rollback:                       PASS after correction
Supported base RNG-kind state length:                  PASS after hardening
Exact reference suite:                                PASS, 43/43
Exact 6,000-retained benchmark:                       PASS, 150/150
Fail-closed 24-fit execution proof:                    PASS, zero fits
Package, document, literature, and exdqlm checks:      PASS
Create authorization commit now:                      NO; independent review first
Run 24-fit bounded grid now:                           NO
Matched/production simulation:                        NO
CAVI/ELBO:                                             DEFER
RQR-DESN:                                              DEFER
```

Output-11 did not identify a defect in the loss, pseudo-AL augmentation,
fixed-joint blocked Gibbs target, missing-data treatment, component-specific
discount construction, shared component-scale conditional, or root/response
interpretation. None of those objects was changed.

## Finding-by-finding audit and correction

| Output-11 finding | Independent audit | Disposition |
|---|---|---|
| F1 commit/schema reconciliation | The stated implementation/evidence commits and package/schema values were verified | Pass |
| F2 statistical target | The pseudo-residual loss augmentation remains distinct from a response likelihood; future roots are not response-predictive draws | Pass; no target change |
| F3 PGID drain/reaping | The idempotent signal/error finalizer, TERM-to-KILL drain, reap, and final non-zombie census were retained | Pass |
| F4 process-substitution producer failure | Reproduced: failure in the old `find | sort` process substitution did not determine the function status | Replaced with an explicitly checked NUL path-list pipeline |
| F4 ambiguous canonical targets | Reproduced: ordinary `mv temporary existing-directory` can return zero without creating the canonical regular file | Fresh empty output directory required; symlink output roots rejected; checked `mv -T` regular-file publication added |
| F5 manifest completeness | Reproduced: the old verifier rehashed only rows that existed, so a header-only manifest could pass its row loop | Exact sorted manifest-path versus actual-regular-file equality is now mandatory |
| F6 required estimands | Exact ordered fixture-derived schemas and all omission-negative tests remain present | Pass |
| F7 future diagnostics | `nd=NULL`, sequential draw identity, exact scale-row orientation, deterministic future conditional means, and stochastic sidecar separation remain present | Pass |
| F8 RDS rollback | Confirmed statically: an exception after rename could bypass the old temporary-only cleanup | Added `renamed`/`committed` rollback guard and injected post-rename failure test |
| F9 continuation/RNG | Continuation raw validation passed; Output-11's optional RNG semantic-length hardening was useful and bounded | Added exact state lengths for all supported base RNG kinds; user-supplied RNG kind remains variable-length by definition |
| F10 varying-scale reference | Two distinct component-scale profiles and correct draw-specific future orientation remain present | Pass |
| F11 reference bundle | Fresh exact-source calculation passed all 43 gates | Pass |
| F12 benchmark | Fresh exact-source four-chain calculation passed all 150 diagnostics | Pass |
| F13 execute authorization | Correct binding and exact confirmation still cannot override the committed false flag | Pass; negative test returned nonzero with zero chains |
| F14 protected repositories | Archive-only exdqlm path and Q-DESN read-only scope remained intact | Pass |
| F15 24-fit launch | Output-11 required correction plus rerun and another independent review | Still no-go pending the next review |

## Wrapper evidence-publication correction

The monitored wrapper now requires a new empty output directory. It rejects a
symbolic-link output root and any pre-existing entry before installing traps or
launching R.

Artifact enumeration now follows this sequence:

1. create a temporary NUL-delimited path list;
2. execute `enumerate_artifact_paths | sort -z` as a directly checked pipeline;
3. stop and remove both temporary files if enumeration or sorting fails;
4. hash and stat every listed regular file;
5. stop and remove the partial manifest on any row failure;
6. publish with checked target-as-file `mv -T`;
7. require a regular non-symlink canonical file.

Resource and closeout files use the same checked publisher. Maxima parsing is
validated before arithmetic. A final-evidence failure sets
`finalizer_error=TRUE`, makes the resource decision false, appends a structured
failure when possible, rewrites resource and closeout evidence, and attempts
the manifest last so a successful manifest cannot be made stale by a later
write.

The monitor fault suite now passes eight deterministic scenarios:

```text
1. TERM after readiness
2. injected monitor error
3. group leader exits before descendant
4. TERM-resistant child requiring KILL
5. zero-sample startup
6. artifact enumeration producer failure
7. artifact row-processing failure after one row
8. canonical resource-summary path is a directory
```

The first five require exact manifest/actual regular-file-set equality, every
required file, a minimum row count, and per-row rehashing. The enumeration and
row failures must return nonzero, record finalizer failure, and publish no
artifact manifest. The canonical-directory case must return nonzero, retain
the collision rather than moving into it, and publish an exact manifest for
the remaining regular files.

## Full-chain RDS rollback correction

`rqr_bounded_publish_fit_rds()` now:

- computes all object-only evidence before rename;
- sets `renamed=TRUE` immediately after a successful rename;
- removes the final path on exit unless `committed=TRUE`;
- sets `committed=TRUE` only immediately before successful return; and
- retains the existing class/identity readback, checkpoint digest, continuation
  validation, byte count, and pre/post SHA-256 checks.

The new injected post-rename hook proves that the final path exists at the
fault boundary, the injected exception is observed, and both the temporary and
final files are absent afterward. The ordinary successful publication and the
pre-rename invalid-checkpoint rejection continue to pass.

## Optional RNG hardening

Before integer conversion, `.rqr_restore_rng()` already required finite,
integral, in-range numeric state. It now decodes the base RNG-kind code and
requires the documented state lengths used by this R runtime:

| RNG kind code | State length |
|---:|---:|
| 0, Wichmann-Hill | 4 |
| 1, Marsaglia-Multicarry | 3 |
| 2, Super-Duper | 3 |
| 3, Mersenne-Twister | 626 |
| 4, Knuth-TAOCP | 102 |
| 6, Knuth-TAOCP-2002 | 102 |
| 7, L'Ecuyer-CMRG | 7 |

Code 5 is the base user-supplied RNG hook and has no universal state length, so
it retains the prior complete finite-integer boundary. Unknown codes and a
truncated Mersenne-Twister state are rejected. The bounded run uses internally
created Mersenne-Twister checkpoints.

## Exact isolated-runtime evidence

The exact application subtree at the implementation commit was archived,
built, installed, and attested under the disjoint ignored primary runtime:

```text
source commit:
  53dc71d873ef12ebba91cbc3d6813682e0987960

application tree:
  6ac932d6a8f6e96322e827204cb0df0696d0407d

source package SHA-256:
  c169605bc29c6214ef83bb95a22ac58fee1a517d3b3780d13a6523c0014b542b

runtime tree digest:
  b34d95fdc87f195e101d5e32490d7a9cec92bee59db4222a1886da1cc4a11490

attestation SHA-256:
  ffc1fcd43971e1f06f34de9ed4c45547978ded084cdd8d2457bb9d20f24bf6de
```

Every runtime-lineage gate in the preflight, reference, benchmark, and
fail-closed execute run was true.

## Fresh exact-source validation

### Preflight

```text
prospective fits:          24
fits executed:             0
peak sampled processes:    1 / 3
peak sampled threads:      2 / 4
peak sampled RSS:          142,884 / 4,194,304 KiB
final PGID empty:          true
finalizer error:           false
```

### Reference-only validation

```text
reference gates:           43 / 43
failure records:           0
dynamic fits executed:     0
peak sampled processes:    3 / 3
peak sampled threads:      4 / 4
peak sampled RSS:          193,144 / 4,194,304 KiB
final PGID empty:          true
```

Key hashes:

```text
artifact manifest:
  b22379853fd616ad28de0de8eaa1c5ab0c4007afae7f5656a4bf5dfecf130475

reference gates:
  ec6964a73dc0c3c36ef974b82a2cf40e12a3e7084ffc2f7ea043e7776fe10a0f

reference bundle:
  87d0f02b192c7e7088d58348d78dffd080f946f722280995c9b432336dab02c5

run manifest:
  eeb523d27bb2ad003ee52dd7a0fd23893392a87b288400854043aac201926d27
```

The reference-gate CSV hash is exactly equal to the previously tracked
Output-10 reference-gate CSV. Thus the wrapper/publication correction and RNG
input hardening did not change any of the 43 calculated gate values.

### Frozen 6,000-retained benchmark

The only fitted cell was again the preauthorized shared-component-scale
trend-plus-regression fixture with learned normalized loss scale:

```text
chains:                     4
burn-in per chain:          2,000
retained per chain:         6,000
thinning:                   1
diagnostics:                150 / 150 passed
maximum R-hat:              1.00491399373516
minimum bulk ESS:           1456.57476438759
minimum tail ESS:           2107.19279393563
numerical repairs:          0
forecast repairs:           0
failure records:            0
peak sampled processes:     3 / 3
peak sampled threads:       4 / 4
peak sampled RSS:           399,956 / 4,194,304 KiB
final PGID empty:           true
```

Each chain independently passed exact source/runtime provenance, exact target
status, required estimand schema, sequential future identity, RDS readback,
checkpoint/history validation, post-rename integrity, and recursive rehashing.
The four chain files total 45,304,968 bytes and remain ignored.

Key hashes:

```text
artifact manifest:
  548229e7c6ef79281c70ba7258a0068e92d3a209d9b2b8fb5e5bf0fd3c4387be

diagnostics:
  a8c143287bc678b40736e5d8f962c82c78b6dab7e1d24a4f18ad9226d2c264f0

estimand schema:
  e97b2469790929883bcb9097991fa3a231809dc724ffdeaab78555f93790f419

missing/future checks:
  be587be306fb23d89c53629f365f91c4b8c71443d43df1c3414bde5eacbb91f4

local chain-hash table:
  aa47cdca3cf1f46a8e72dd27b725f73c44c7d38777208deb82702b7e08f7596e

run manifest:
  694a3718fe183dab0b81904eeee834160058e5428877fcfc99489ca17f55c8fc
```

The diagnostic, estimand-schema, and missing/future CSV hashes are exactly
equal to their tracked Output-10 counterparts. This is direct evidence that
the new publication-boundary code exercised the unchanged scientific draws
and diagnostic contract. Chain-file hashes differ as expected because fit
provenance binds the new source commit and package version.

### Fail-closed execute negative test

Execute mode received:

- the exact implementation SHA;
- the exact new reference directory and artifact-manifest hash;
- the identical isolated runtime/toolchain;
- the reviewed-runner SHA; and
- the exact 24-fit confirmation phrase.

It returned status 1 because the committed authorization flag is false:

```text
status:                       blocked_by_execution_contract
reference binding verified:   true
authorization flag:            false
chain files created:           0
final PGID empty:              true
artifact manifest:
  0706a7c4491e38f862e39b7053ac8862360cf7090074563716bf288d67614384
run manifest:
  41e68faecc953b21af4d768a394e8d82dbf4bcb102a939bf4b914980b676f509
```

## Other validation

```text
bash syntax:                           pass
monitor/finalization fault suite:      8 / 8 pass
bounded-config test assertions:        74 / 74 pass
native package assertions:             396 / 396 pass
R CMD check --no-manual:               Status: OK
environment smoke:                     pass
main PDF:                              9 pages
supplement PDF:                        10 pages
literature manifest:                   18 local PDFs
pinned exdqlm archive-runtime smoke:   pass
heavy tracked artifacts:               none
```

Generated PDFs, TeX logs, package tarballs/check directories, chain objects,
runtime caches, and run directories remain ignored.

## Tracked compact evidence

The exact recursive manifests and local chain hashes are tracked as:

```text
docs/audits/rqr_dlm_output11_validation_summary_20260724.csv
docs/audits/rqr_dlm_output11_reference_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output11_benchmark_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output11_benchmark_local_chain_hashes_20260724.csv
docs/audits/rqr_dlm_output11_failclosed_artifact_hashes_20260724.csv
```

The full 43-row reference and 150-row diagnostic tables are not duplicated
because their new hashes are byte-for-byte equal to the already tracked files:

```text
docs/audits/rqr_dlm_output10_reference_gates_20260724.csv
docs/audits/rqr_dlm_output10_benchmark_diagnostics_20260724.csv
docs/audits/rqr_dlm_output10_benchmark_estimand_schema_20260724.csv
docs/audits/rqr_dlm_output10_benchmark_missing_future_checks_20260724.csv
```

## Decision and next bounded step

The Output-11 blockers are corrected and reproduced. The source and fresh
evidence are ready for one final independent review.

Do not create the authorization commit or run the 24-fit grid in this
reconciliation pass. If the next review gives a clean conditional go:

1. create a separate commit whose substantive configuration change is the
   false-to-true bounded execution flag;
2. build a new exact isolated runtime for that authorization commit;
3. regenerate preflight and the reference bundle because the config digest
   changes;
4. require the user's explicit confirmation phrase;
5. execute the six four-chain cells sequentially with cell-level fail-fast,
   no retries, and no retuning; and
6. audit the bounded result before designing or launching the matched
   simulation.

Even a successful 24-fit run would validate bounded target mechanics,
numerics, provenance, continuation, and mixing. It would not establish
empirical coverage calibration, forecasting superiority, or a response
simulation contract.
