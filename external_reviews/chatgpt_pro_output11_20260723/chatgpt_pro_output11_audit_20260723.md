# ChatGPT Pro Output-11 independent audit of RQR-GIBBS

**Audit date:** 2026-07-24 UTC  
**Implementation inspected:** `e24feb411b2e30586d1bfdc18bf6acb1fb568c70`  
**Evidence inspected:** `99b6f92911a5cd323b735598063da72766ad9095`  
**Package/schema:** `rqrgibbs 0.1.0.9010`; fit `1.8.0`; continuation `4.1.0`; runtime attestation `5.0.0`; bounded fixtures `5.0.0`; run `3.0.0`; reference bundle `2.0.0`; estimands `1.0.0`

Protected references were read only at `dffb71ee70b597d6a716ee74be1cbc99731cd453` (exdqlm) and `f9f22804eff3871bb5350c8add04b7c9f4d4957b` (Q-DESN). This review did not write to either repository.

## Evidence boundary and commit reconciliation

GitHub access succeeded. The evidence commit is one commit ahead of, and has merge base equal to, the implementation commit. Its changes are the reconciliation and compact `rqr_dlm_output10_*` evidence files; it does not replace the implementation being audited. The later public-main commit `dd937fe7cc15b38170ec2ae4711dd0b193a86db8` was not used as an inferred expected SHA and is outside this exact-commit audit.

The audit environment did not contain `R` or `Rscript`, and the ignored Jerez chain objects and run directories were unavailable. I therefore did not rerun package, R/C++, TeX, isolated-runtime, reference, benchmark, or monitor commands. I inspected the exact implementation, every tracked Output-10 evidence table, all 43 reference rows, all 150 benchmark diagnostic rows, and the compact hash contracts. Execution values below remain exact-commit tracked evidence rather than a fresh rerun.

No bounded-grid fit was launched.

## Executive verdict

```text
Statistical target and interpretation:           PASS
Runtime lineage version 5:                       PASS, controlled local scope
Monitor process-group drain/reaping:              PASS
Monitor/final evidence publication:               PARTIAL — blocker
Required-estimand completeness:                   PASS
Future draw identity and diagnostic target:       PASS
Full-chain RDS publication:                       PARTIAL — blocker
Continuation/history raw validation:              PASS
Saved RNG numeric coercion boundary:               PASS; semantic-length hardening remains
Varying component-scale future reference:         PASS
43-gate reference bundle:                         PASS as tracked exact-commit evidence
6,000-retained one-cell benchmark:                PASS, 150/150
Reference/runtime/toolchain execute binding:      PASS
Environment-only execution bypass:                REJECTED
Change only the execution flag now:               NO-GO
Create authorization commit now:                  NO-GO
Run the bounded 24-fit grid now:                   NO-GO
Matched/production RQR-DLM simulation:            NO-GO
CAVI/ELBO:                                        DEFER
RQR-DESN:                                         DEFER
```

The remaining no-go is not evidence of an incorrect statistical target, broken sampler, inadequate 6,000-draw schedule, or weak reference calculation. It is caused by two narrow publication-boundary defects: shell artifact finalization can report success after an incomplete manifest, and an exception after RDS rename can leave a published chain behind.

## Finding disposition

| ID | Area | Verdict | Blocks 24 fits? | Finding |
|---|---|---|---:|---|
| F1 | Commit/schema reconciliation | Pass | No | Exact commits and all declared package/schema versions match source; evidence is a descendant of implementation. |
| F2 | Statistical target | Pass | No | Loss/likelihood, root/response, fixed-joint/adaptive, and learned-scale distinctions remain correct. |
| F3 | PGID drain/reaping | Pass | No | One idempotent finalizer, TERM-to-KILL drain, wait/reap, non-zombie census, signal status, and zero-sample maxima are implemented. |
| F4 | Shell artifact finalization | Partial | **Yes** | Failure of `find | sort` inside process substitution is invisible to the parent function; output-path directory collisions can also make `mv` return zero without creating the canonical file. |
| F5 | Fault-suite verification | Partial | **Yes** | The five scenarios rehash rows that are present but do not require manifest-file-set equality; a header-only manifest passes that loop. |
| F6 | Required estimands | Pass | No | Canonical-fixture schema, exact order, prepublication check, repeated cell check, and same-omission negative tests are present. |
| F7 | Future diagnostics | Pass | No | `nd=NULL`, sequential `draw_index`, component-row identity, deterministic conditional means, and stochastic sidecar separation are correct. |
| F8 | RDS publication rollback | Partial | **Yes** | Pre-rename failures and explicit post-rename mismatch are cleaned, but an exception during post-rename hash/metadata/object-digest work leaves the final path unguarded. |
| F9 | Continuation/RNG | Pass | No | Aggregate logicals and numeric state are checked before coercion; mutation tests are substantive. RNG kind-specific length validation is optional hardening. |
| F10 | Varying component-scale reference | Pass | No | Two distinct scale profiles, exact row orientation, draw-specific covariance reconstruction, and correct Gaussian MCSE formulas are used. |
| F11 | 43 reference gates | Pass | No | All rows are present and pass; recursive evidence binds source, runtime, config, toolchain, gates, and files. |
| F12 | 6,000 benchmark | Pass | No | Every one of the 150 ordered diagnostics passes the frozen R-hat and ESS thresholds with zero repairs/failures. |
| F13 | Execute authorization | Pass | No | Exact reference binding and confirmation text cannot bypass the committed false flag; the negative run produced zero chains. |
| F14 | Protected repositories | Pass | No | exdqlm is archived read-only and built in the ignored RQR cache; Q-DESN is not an execution dependency and remains read only. |

## Statistical interpretation

The declared object is coherent. For each observation, the loss is applied to

```text
e = (y - eta1)(y - eta2).
```

The normal-exponential construction is an augmentation of the exponentiated pseudo-residual loss. It does not become a response likelihood merely because a Gaussian conditional is used internally. Future state/root draws therefore remain interval-root state draws, not posterior-predictive response draws.

For fixed `W`, frozen discount templates, and shared component scales, the two root paths have a fixed joint Gaussian state prior. The augmented measurement contribution contains the square of a product of two affine root ordinates and is quartic jointly. Conditional on one complete root path, however, it is quadratic in the other path. Alternating root-specific FFBS draws are therefore exact blocked full-conditionals for those fixed-joint modes. The bounded config excludes adaptive conditional discounting. I found no concrete mathematical error requiring a target rewrite.

## Monitor and finalization audit

### What is correct

`application/scripts/08_run_rqr_dlm_bounded_validation.sh` routes `EXIT`, `INT`, `TERM`, `HUP`, ordinary errors, and normal completion through one idempotent `finalize_wrapper()`. It drains the whole process group, escalates TERM to KILL, waits for the root, ignores zombies in the final census, rejects signal/nonzero-runner/monitor/PGID/finalizer/KILL outcomes, and defines zero-sample maxima as zero. The Bash special `_` counter defect is removed.

The embedded leader-exit test requires an actually surviving TERM-resistant descendant, KILL escalation, and final emptiness. The external test script covers TERM after readiness, monitor failure, leader-first exit, TERM-resistant child, and zero-sample startup.

### Blocker F4: process-substitution failure is not propagated

`write_artifact_manifest()` ends with:

```bash
while ...; do
  ...
done < <(
  find "$output_dir" ... -print0 | sort -z
)
mv "$artifact_tmp" "$artifact_csv"
```

Bash does not make the exit status of the process-substitution producer the status of the `while` command or function. With `pipefail`, the producer pipeline can fail and the parent can still execute a successful `mv`, publishing a partial or header-only manifest and returning zero.

The underlying shell behavior is deterministic:

```bash
f() {
  echo header >tmp
  while IFS= read -r -d '' x; do :; done < <(bash -c 'exit 42')
  mv tmp out
}
f
echo "$?"   # 0
```

This is the same class of silent shell-boundary defect the prompt required checking after the `_` correction.

### Blocker F4: canonical output paths are not type-checked

The wrapper accepts an existing `RQR_DLM_OUTPUT_DIR`, then writes temporary files and calls ordinary `mv`. On GNU systems, if `resource_summary.csv`, `wrapper_closeout.csv`, or `artifact_hashes.csv` already exists as a directory, `mv temporary target-directory` returns zero and places the temporary file inside that directory. The canonical path remains a directory, yet the finalizer can retain `resource_pass=TRUE`.

Deterministic shell behavior:

```text
mv-to-directory status: 0
canonical target is regular file: false
```

The wrapper should require an empty/fresh output directory or reject non-regular canonical targets, and use checked no-ambiguity publication such as `mv -T` where available.

### Blocker F5: the fault verifier does not prove manifest completeness

`verify_artifacts()` verifies required files separately and then rehashes each manifest row. It never compares the sorted manifest paths with the sorted actual file set. A CSV containing only the header executes the row loop zero times and passes the rehash portion. Consequently, the current five scenarios do not detect the process-substitution failure above.

### Minimal correction

1. Generate the sorted NUL path list into a temporary file and explicitly check the `find | sort` pipeline before reading it; do not depend on process-substitution status.
2. Check every maxima calculation, redirection, and `mv`; any failure must set `finalizer_error=TRUE` and force nonzero exit.
3. Reject directory/symlink collisions at every canonical output path or require a fresh empty output directory; use checked target-as-file publication.
4. In the fault suite, compare the manifest path set exactly with all actual regular files except the manifest itself, require at least the expected rows, and verify that every required file is represented.
5. Add deterministic fault injections for producer-pipeline failure and canonical-target collision, in addition to the existing five process scenarios.

**Decision: PARTIAL; launch blocker.**

## Required-estimand completeness

The independently generated schema has count

```text
4*T + 1 + 4*p + 4*H + I(learned) + 2*J*I(component-scale),
```

where `T` is training length, `p` is state dimension, `H` is future horizon, and `J` is the number of component scales.

| Fixture | Calculation, fixed | Fixed | Learned |
|---|---:|---:|---:|
| fixed-W local level (`T=24,p=1,H=4`) | `96+1+4+16` | 117 | 118 |
| frozen trend-seasonal (`T=36,p=5,H=4`) | `144+1+20+16` | 181 | 182 |
| component trend-regression (`T=30,p=3,H=3,J=2`) | `120+1+12+12+4` | 149 | 150 |

`rqr_bounded_expected_estimand_names()` derives the exact ordered names from the constructed fixture, not chain columns. `rqr_bounded_validate_estimand_schemas()` requires exact ordered identity, no duplicates, and finite values. The runner invokes it on each chain before RDS publication and again across exactly four chains before cell diagnostics. Tests remove the same training, time-zero, future, lambda, or component quantity from all four matrices and still require rejection. Orientation and order, not merely counts, are checked.

**Decision: PASS.**

## Future draw identity and primary diagnostic target

The old issue is reproducible directly from public source: any non-NULL `nd` uses `sample.int(n_save, nd, ...)`, so `nd=n_save` generally permutes all indices. The bounded runner now calls stochastic forecasting with `nd=NULL`, requires `draw_index` to equal `1:n_save`, and requires component-scale diagnostic rows to be identical to saved rows in that order.

Primary mixing variables no longer include newly sampled future process noise. For each retained terminal state, `rqr_bounded_future_conditional_mean_roots()` recursively multiplies by future `GG`, maps through future `FF`, then orders roots. Omitting `W` is mathematically correct for a conditional mean because future Gaussian innovations have zero mean. Ordering with `pmin`/`pmax` makes lower, upper, midpoint, and width invariant to global root-label exchange. These are ordered conditional-mean root functionals, not expectations of stochastic ordered endpoints; the implementation labels that distinction accurately.

Stochastic process-noise root paths remain a finite, zero-repair sidecar and explicitly state that no response simulation contract is implied.

**Decision: PASS.**

## Full-chain RDS publication

### What is correct

`rqr_bounded_publish_fit_rds()` accepts only `rqr_dlm_mcmc`, refuses an existing path, saves in the destination directory, removes the temporary file on exit, reads it back, requires exact class and object identity, recomputes the checkpoint digest, validates continuation history, records temporary size/SHA-256, renames, and compares final size/SHA-256. The caller performs the exact-target/runtime provenance gate and exact estimand-schema gate before publication. Reconstructing another provenance field inside the publisher is not necessary: exact object identity preserves the already gated provenance, and duplicating the environment check would introduce a second time-of-check boundary.

Publication errors are inside the runner's `fit_and_forecast` `tryCatch`, so they enter the structured failure ledger and stop the cell.

### Blocker F8: rollback is incomplete after rename

After `file.rename(temporary, path)` succeeds, cleanup still protects only `temporary`. If `digest::digest(file=path)`, `file.info(path)`, or the final `object_digest` evaluation raises an exception, the function exits with a published final file remaining. Only an explicit hash/size mismatch calls `unlink(path)`.

That refutes the stated invariant that *any* failure leaves no published final file. The run would fail, but a stale final chain could remain and be mistaken for a valid completed chain by subsequent manual inspection.

### Minimal correction

Use a rollback guard:

```r
renamed <- FALSE
committed <- FALSE
on.exit({
  unlink(temporary)
  if (renamed && !committed) unlink(path)
}, add = TRUE)
```

Compute all object-only evidence before rename, set `renamed <- TRUE` immediately after rename, perform post-rename checks, then set `committed <- TRUE` only immediately before successful return. Add an injected post-rename hash/metadata failure test and assert that neither temporary nor final path remains. A no-clobber lock is optional for this sequential runner, but would close the check/rename race.

**Decision: PARTIAL; launch blocker.**

## Continuation and RNG hardening

The continuation validator now proves all five aggregate status fields are length-one, nonmissing logicals before using `isTRUE()`. Per-segment logicals receive the same raw check. Generation, repair, cumulative, contract, model, and completed-iteration counters pass a finite scalar whole nonnegative in-range boundary before integer conversion. Recursive exactness, target eligibility, environment eligibility, backend changes, cumulative repair state, mismatch ledger, reproducibility, and promotion are reconstructed.

Saved RNG state is checked as numeric, length at least two, nonmissing, finite, integral, and within the non-`NA` integer range before `as.integer()`. Tests cover fractional and infinite RNG entries. Recomputed-digest tests cover nonlogical numeric, missing, vector-valued aggregate booleans; fractional, negative, infinite, and overflowed counts; completed-iteration mutation; and semantic repair, mismatch, backend, target, and parent-link contradictions.

Low-severity hardening remains: `.Random.seed` completeness is currently represented by `length >= 2`. The first RNG-kind code could be used to validate the exact expected vector length before assignment. This is not a bounded-grid blocker because bounded checkpoints are internally generated, digested, and immediately consumed, and an invalid shortened state fails when the RNG is used.

**Decision: PASS for bounded use.**

## Varying component-scale future reference

The new reference uses two distinct vectors, `0.5*q_initial` and `2*q_initial`, with 2,000 sequential saved rows per group. `nd=NULL` preserves row identity; the saved component-scale diagnostic matrix must exactly equal the constructed rows. For each group, the reference reconstructs draw-specific future `W`, propagates the Gaussian state mean and covariance, and compares empirical root means and sample variances using

```text
SE(mean)     = sqrt(variance / group_n)
SE(variance) = sqrt(2 * variance^2 / (group_n - 1)).
```

These are the correct Gaussian Monte Carlo scales. Tracked maxima are 1.187000 for means and 1.142024 for variances, with exact orientation.

**Decision: PASS.**

## Reference bundle and benchmark evidence

### Forty-three reference gates

I inspected all 43 rows. They cover dense conditional mean/covariance, R/C++ parity, sampled FFBS mean/full/adjacent covariance, missing-measurement omission, canonical placeholder invariance, public future means/variances for all fixtures, three varying-scale checks, scalar and canonical component-scale conditionals, all six uninterrupted-versus-`2+2+2` cells with checkpoint/history checks, 27 digest-consistent history mutations, and the active monitor declaration. Every row is `TRUE`.

The reference verifier requires exact artifact file-set equality and rehashing, required files, source commit, config digest, runtime tree, runtime attestation hash, toolchain digest, reference-gate and bundle hashes, bundle-file hashes, gate success, resource success, schema versions, and the estimand/future-target declarations. The tracked manifest and reconciliation are internally consistent. I did not regenerate the binary RDS artifacts or rerun the reference suite in this environment.

**Decision: PASS as exact-commit tracked evidence.** The active-monitor row does not cure F4/F5, which are independent source findings.

### Six-thousand-retained benchmark

I inspected all 150 ordered diagnostic rows, not only the summary. Every row is finite and marked `TRUE`. The extrema are:

```text
maximum R-hat:    1.00491399373516  (log_component_scale_trend)
minimum bulk ESS: 1456.57476438759  (log_component_scale_regression)
minimum tail ESS: 2107.19279393563  (log_component_scale_regression)
```

The related regression innovation energy has bulk ESS 1459.698181 and tail ESS 2145.628363. The four fits use unchanged seeds 84201--84204, have elapsed times 177.763--181.447 seconds, total 718.919 seconds, total chain bytes 45,304,976, zero numerical and forecast repairs, exact target/provenance/promotion status, and no failures. The stochastic sidecar preserves sequential draw identity for all four chains. Peak sampled RSS is 396,188 KiB.

The wrapper duration of about 740 seconds for one cell projects six equal-cost cells to about 74 minutes. The frozen 240-minute ceiling provides more than a threefold time margin without changing process/thread/RSS limits. The hardest declared learned component-scale cell now passes every predeclared gate with material ESS margin. I found no concrete reason to lengthen the schedule again, weaken a threshold, replace a seed, or retune after seeing the result.

**Decision: PASS; the 6,000-retained schedule is sufficient evidence for the bounded validation stage.**

## Execute binding and protected-repository isolation

`verify_reference_bundle()` binds execute/benchmark mode to the exact source, config, runtime, attestation, toolchain, complete reference directory, recursive artifact manifest, gates, resources, and declared estimand/future contracts. Execute authorization additionally requires `isTRUE(config$bounded_dynamic_execution_authorized)` and the exact confirmation phrase. Since the committed flag is false, environment variables alone cannot enable execution. The tracked negative manifest reports reference binding true, authorization false, and zero chains.

The exdqlm builder captures a checkout guard including ignored files, requires the pinned branch/commit and clean state, creates a read-only `git archive`, verifies mode/blob/path lineage, builds and installs only the extracted archive under the ignored RQR-owned cache, and checks the before/after guard. No runner path loads or compiles from the checkout. Q-DESN is a historical/read-only reference and is not invoked by this bounded runner.

**Decision: PASS.**

## Required decisions and authorization sequence

| Decision | Result |
|---|---|
| statistical target and interpretation | PASS |
| monitor/finalization contract | PARTIAL; blocker F4/F5 |
| estimand completeness contract | PASS |
| future draw/diagnostic contract | PASS |
| RDS publication contract | PARTIAL; blocker F8 |
| continuation/RNG hardening | PASS for bounded use |
| 43-gate reference bundle | PASS as tracked exact-commit evidence |
| 6,000-retained one-cell benchmark | PASS |
| change only the execution flag | NO-GO |
| create a separate authorization commit after this review | NO-GO until blockers are patched, rerun, and reviewed |
| run the bounded 24-fit validation | NO-GO now; conditional only after the sequence below |
| matched/production RQR-DLM simulation | NO-GO |
| CAVI/ELBO | DEFER |
| RQR-DESN | DEFER |

The smallest safe sequence is:

1. Keep `bounded_dynamic_execution_authorized=FALSE`.
2. In one implementation commit, close F4/F5 and F8; add deterministic tests for producer-pipeline failure, manifest set equality, target-path collisions, and post-rename RDS failure. Optionally add RNG-kind/length validation.
3. At that exact commit, rebuild the isolated runtime and rerun shell syntax, the fault suite, native/package checks, preflight, all 43 references, the unchanged 6,000-draw benchmark, and the fail-closed execute negative test. Do not reuse chains, change seeds, retune, or weaken gates.
4. Obtain an independent review of that exact source and evidence.
5. Only then create a separate authorization commit whose substantive config change is the false-to-true execution flag.
6. Because that changes the config digest, build a new exact isolated runtime and generate a new exact preflight and reference bundle for the authorization commit.
7. Require the user's explicit confirmation phrase and execute sequentially, diagnosing each four-chain cell before any later cell, with no retries or retuning.
8. Audit the bounded result before defining or launching any matched/production simulation.

A successful bounded grid would establish mechanics, numerics, provenance, continuation, and mixing for the declared fixtures. It would not establish empirical interval calibration, comparative forecasting performance, or response-predictive validity.
