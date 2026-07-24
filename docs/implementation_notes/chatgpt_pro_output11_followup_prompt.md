# ChatGPT Pro Output-11 follow-up prompt

Perform one final independent, source-level audit of the standalone
`AntonioAPDL/RQR-GIBBS` repository. Treat the Codex reconciliation and tracked
execution evidence as claims to verify, not as proof.

Do not ask me to upload repository files. Use the connected GitHub repository
at the exact commits below. Do not modify implementation, manuscript, config,
or evidence files. Your only write action is to push the four review
deliverables described at the end to a new review branch.

## Exact scope

```text
corrected implementation:
  53dc71d873ef12ebba91cbc3d6813682e0987960

compact reconciliation evidence:
  85658e9378d25b12335bf70e7de936b889ef74dd

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

Protected read-only references:

```text
exdqlm:
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article:
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

The protected repositories were not changed. exdqlm was built and tested only
from a Git archive under the ignored RQR-GIBBS cache. Do not propose or make
changes in either protected repository.

## Fixed statistical interpretation

Preserve these distinctions:

- the RQR posterior is a generalized-Bayes loss update;
- the normal-exponential pseudo-AL construction augments the pseudo-residual
  loss and is not an ordinary response likelihood;
- interval-root state draws are not posterior-predictive response draws;
- fixed `W`, frozen discount-template, and shared component-scale modes use
  exact alternating root-specific FFBS full-conditionals for the declared
  fixed joint target;
- adaptive conditional discounting remains an experimental working recursion
  and is excluded from this bounded validation;
- empirical coverage calibration and comparative forecasting performance are
  not established by this bounded validation.

No statistical target, Gibbs conditional, state equation, missing-data rule,
component discount rule, component-scale conditional, or response
interpretation changed in the Output-11 correction.

## What Output-11 accepted

The prior independent review accepted:

```text
statistical target and interpretation
runtime lineage version 5
PGID drain and reaping
required-estimand completeness
future retained-draw identity
deterministic future conditional-mean diagnostic target
continuation-history raw validation
varying component-scale future reference
43 reference gates as tracked evidence
6,000-retained benchmark, 150/150 diagnostics
reference/runtime/toolchain execute binding
protected-repository isolation
```

It identified exactly three launch-blocking publication-boundary findings:

```text
F4:
  process-substitution producer failure was not propagated;
  canonical output path directories could make ordinary mv look successful.

F5:
  the fault verifier rehashed rows that existed but did not require exact
  manifest/actual file-set equality.

F8:
  an exception after chain RDS rename could leave the final path published.
```

It also suggested nonblocking RNG-kind state-length hardening.

## Corrected implementation to audit

Inspect the complete implementation diff leading to
`53dc71d873ef12ebba91cbc3d6813682e0987960`, especially:

```text
application/scripts/08_run_rqr_dlm_bounded_validation.sh
application/scripts/10_test_rqr_dlm_monitor_wrapper.sh
application/scripts/lib/rqr_dlm_bounded_diagnostics.R
application/tests/testthat/test-rqr-dlm-bounded-config.R
application/R/rqr_utils.R
application/tests/testthat/test-rqr-native-sampler.R
application/DESCRIPTION
Makefile
README.md
```

### 1. Shell enumeration and publication boundary

Verify independently that:

1. the output directory must be new and empty;
2. a symbolic-link output root is rejected;
3. artifact enumeration writes through a temporary NUL path list;
4. `enumerate_artifact_paths | sort -z` is a directly checked pipeline, not an
   unchecked process substitution;
5. any enumeration, sort, SHA-256, stat, row-write, or publish failure removes
   partial temporary evidence and returns failure;
6. canonical file publication rejects directories and symlinks;
7. GNU target-as-file `mv -T` cannot silently move into a directory;
8. maxima output is checked before arithmetic;
9. resource, closeout, failure-ledger, and artifact writes are ordered so a
   successful manifest is never made stale by a later evidence write;
10. finalizer failure forces a nonzero wrapper result.

Look for another shell-status or target-type counterexample. If you find one,
give an exact deterministic reproducer. Do not report a theoretical issue
without identifying a reachable source path.

### 2. Monitor fault verifier

Verify that the normal five scenarios require:

```text
exact sorted manifest path set == exact sorted actual regular-file set
all required files represented
minimum expected manifest rows
per-row byte and SHA-256 agreement
final non-zombie PGID emptiness
structured failure evidence
```

Verify the three new deterministic fault cases:

```text
artifact enumeration producer failure
artifact row-processing failure after one completed row
canonical resource-summary path precreated as a directory
```

The first two must publish no artifact manifest and must return nonzero with
structured finalizer evidence. The collision case must not move a temporary
file into the directory, must return nonzero, and must publish an exact
manifest for the remaining regular files.

Decide whether the 8/8 suite now closes F4/F5 for this Jerez GNU/Bash runner.

### 3. RDS post-rename rollback

Audit `rqr_bounded_publish_fit_rds()` for the invariant:

```text
any exception before successful return leaves neither a temporary file nor a
new final chain file
```

Verify:

- `renamed` and `committed` are initialized before the cleanup handler;
- object-only evidence is computed before rename;
- `renamed=TRUE` is set immediately after rename;
- any exception from the injected hook, final SHA-256, final metadata, or
  evidence construction removes the final path;
- `committed=TRUE` is set only immediately before successful return;
- successful class/identity readback, checkpoint digest, continuation history,
  byte count, pre/post SHA-256, and no-overwrite behavior remain intact.

Inspect the injected post-rename failure test and try to identify any untested
post-rename exception path that can retain the final file.

### 4. Optional RNG hardening

Check the supported base RNG-kind length table in `.rqr_restore_rng()`:

```text
0 Wichmann-Hill          length 4
1 Marsaglia-Multicarry   length 3
2 Super-Duper            length 3
3 Mersenne-Twister       length 626
4 Knuth-TAOCP            length 102
5 user-supplied          variable length
6 Knuth-TAOCP-2002       length 102
7 L'Ecuyer-CMRG          length 7
```

Confirm that numeric, finite, integral, representable values are checked before
integer conversion, unknown kind codes and truncated supported states are
rejected, and code 5 is not incorrectly assigned a universal length.

Decide whether this hardening is correct for the bounded internally generated
Mersenne-Twister checkpoints. Do not make this a blocker merely because the
base user-supplied RNG hook is inherently variable length.

## Tracked reconciliation and evidence

Read completely:

```text
docs/audits/chatgpt_pro_output11_reconciliation_20260724.md
docs/audits/chatgpt_pro_output11_reconciliation_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output11_validation_summary_20260724.csv
docs/audits/rqr_dlm_output11_reference_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output11_benchmark_artifact_hashes_20260724.csv
docs/audits/rqr_dlm_output11_benchmark_local_chain_hashes_20260724.csv
docs/audits/rqr_dlm_output11_failclosed_artifact_hashes_20260724.csv
```

Verify the reconciliation hash manifest against the other six tracked files.
Verify the recursive artifact manifests structurally and against every compact
artifact that is available in Git.

The fresh reference and diagnostic CSVs are byte-identical to these previously
tracked full tables:

```text
docs/audits/rqr_dlm_output10_reference_gates_20260724.csv
  SHA-256:
  ec6964a73dc0c3c36ef974b82a2cf40e12a3e7084ffc2f7ea043e7776fe10a0f

docs/audits/rqr_dlm_output10_benchmark_diagnostics_20260724.csv
  SHA-256:
  a8c143287bc678b40736e5d8f962c82c78b6dab7e1d24a4f18ad9226d2c264f0

docs/audits/rqr_dlm_output10_benchmark_estimand_schema_20260724.csv
  SHA-256:
  e97b2469790929883bcb9097991fa3a231809dc724ffdeaab78555f93790f419

docs/audits/rqr_dlm_output10_benchmark_missing_future_checks_20260724.csv
  SHA-256:
  be587be306fb23d89c53629f365f91c4b8c71443d43df1c3414bde5eacbb91f4
```

Inspect all 43 reference rows and all 150 diagnostic rows. Confirm that the
unchanged hashes justify reusing these already tracked full tables as the
fresh run's compact remote evidence.

The exact archive-built runtime was:

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

## Fresh validation claims to audit

```text
monitor fault suite:
  8 / 8 pass

bounded-config tests:
  74 / 74 pass

native package tests:
  396 / 396 pass

R CMD check --no-manual:
  Status: OK

reference-only:
  43 / 43 gates
  0 failures
  0 dynamic fits
  peak 3 processes, 4 threads, 193,144 KiB sampled RSS
  final PGID empty

one-cell benchmark:
  4 chains
  2,000 burn-in + 6,000 retained per chain
  150 / 150 diagnostics
  max R-hat 1.00491399373516
  min bulk ESS 1456.57476438759
  min tail ESS 2107.19279393563
  0 numerical repairs
  0 forecast repairs
  0 failures
  peak 3 processes, 4 threads, 399,956 KiB sampled RSS
  final PGID empty

fail-closed execute negative:
  complete reference/runtime/toolchain binding true
  exact confirmation phrase supplied
  committed authorization false
  wrapper status 1
  zero chain files

documents:
  main PDF 9 pages
  supplement PDF 10 pages

literature:
  18 local PDFs in manifest

protected repositories:
  exact pinned commits and guarded unchanged states
```

The local 45 MB chain objects and full run directories are intentionally
ignored. Do not treat their absence from GitHub as evidence that they should be
committed.

## Decisions required

Give explicit, evidence-based decisions for:

1. statistical target and interpretation;
2. shell finalization/publication contract;
3. exact artifact file-set verification;
4. RDS post-rename rollback;
5. supported RNG-kind state validation;
6. exact runtime/reference binding;
7. 43-gate reference result;
8. 6,000-retained one-cell benchmark;
9. protected-repository isolation;
10. whether a separate authorization commit may now be created;
11. whether the 24-fit bounded grid may run after rebuilding the exact
    authorization runtime/reference bundle and obtaining user confirmation;
12. whether matched/production simulation remains deferred;
13. whether CAVI/ELBO and RQR-DESN remain deferred.

If any launch blocker remains, provide:

- the exact file and function;
- a deterministic counterexample or violated invariant;
- why an existing test does not catch it; and
- the smallest correction and test required.

Do not recommend changing seeds, starts, burn-in, retained draws, thinning,
R-hat/ESS gates, resource ceilings, or the statistical target unless you
identify a concrete mathematical or computational defect.

Do not run or authorize the 24 fits yourself. Do not edit source.

## Required remote deliverables

Create this branch directly from evidence commit
`85658e9378d25b12335bf70e7de936b889ef74dd`:

```text
chatgpt-pro/output12-audit-20260724
```

Push exactly these four files under:

```text
external_reviews/chatgpt_pro_output12_20260724/
```

Files:

```text
chatgpt_pro_output12_audit_20260724.md
chatgpt_pro_output12_codex_handoff_20260724.md
chatgpt_pro_output12_findings_20260724.csv
chatgpt_pro_output12_artifact_hashes_20260724.csv
```

The hash CSV must contain the SHA-256, byte count, and repository-relative path
for the other three files. Do not modify any existing file.

After pushing, your final chat response must contain only:

```text
chatgpt-pro/output12-audit-20260724
<full 40-character pushed commit SHA>
```

Do not attach files in chat and do not give sandbox download links. Codex will
fetch the branch and verify the deliverables directly.
