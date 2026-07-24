# ChatGPT Pro Output-10 follow-up prompt

You are performing the next independent technical audit of the standalone
RQR-GIBBS project. Treat every reconciliation statement and tracked result as
a claim to verify against source and compact evidence, not as proof by
assertion.

## Repository and exact commits

Canonical public repository:

```text
https://github.com/AntonioAPDL/RQR-GIBBS
branch: main
```

Audit these exact commits:

```text
statistical/runner implementation:
  e24feb411b2e30586d1bfdc18bf6acb1fb568c70

tracked Output-10 reconciliation and compact evidence:
  99b6f92911a5cd323b735598063da72766ad9095
```

The evidence commit must be a descendant of the implementation commit. Do not
infer an expected SHA from whichever branch happens to be checked out.

Protected read-only references:

```text
exdqlm:
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article:
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

Do not edit either protected repository. Do not ask the user to upload source
files that already exist at the public commits.

## Fixed interpretation

RQR is a generalized-Bayes update based on an interval-root loss. The pseudo-AL
normal-exponential construction augments a pseudo-residual. It is not an
ordinary response likelihood. RQR-DLM state/root draws are not
posterior-predictive response draws.

For fixed `W`, frozen discount templates, and shared component-scale modes, the
two root trajectories have a joint Gaussian state prior. The augmented
observation kernel is quartic jointly in both root states, so one ordinary
simultaneous Gaussian FFBS draw is unavailable. Alternating root-specific FFBS
draws are exact blocked Gibbs full-conditionals for those fixed-joint modes.
The adaptive conditional-discount recursion remains an explicitly working,
non-exact mode and is excluded from this bounded validation.

Do not propose a statistical-target rewrite unless you identify a concrete
mathematical error.

## Package and schema state

At the implementation commit:

```text
package:             rqrgibbs 0.1.0.9010
fit schema:          rqrgibbs_fit/1.8.0
history schema:      rqrgibbs_continuation_history/4.1.0
runtime attestation: rqrgibbs_runtime_attestation/5.0.0
fixture schema:      rqrgibbs_dlm_bounded_fixtures/5.0.0
run schema:          rqrgibbs_dlm_bounded_run/3.0.0
reference bundle:    rqrgibbs_reference_bundle/2.0.0
estimand schema:     rqrgibbs_dlm_bounded_estimands/1.0.0
```

The 24-fit execution flag remains `FALSE`.

## Start with these files

Read the complete reconciliation:

```text
docs/audits/chatgpt_pro_output10_reconciliation_20260723.md
```

Verify its compact evidence manifest:

```text
docs/audits/chatgpt_pro_output10_reconciliation_artifact_hashes_20260723.csv
```

Inspect these implementation files:

```text
application/R/rqr_utils.R
application/R/rqr_dlm_fit.R
application/config/rqr_dlm/rqr_dlm_bounded_dynamic_fixtures_20260723.R
application/scripts/lib/rqr_dlm_bounded_fixtures.R
application/scripts/lib/rqr_dlm_bounded_diagnostics.R
application/scripts/06_preflight_rqr_dlm_bounded_fixtures.R
application/scripts/08_run_rqr_dlm_bounded_validation.R
application/scripts/08_run_rqr_dlm_bounded_validation.sh
application/scripts/09_run_rqr_dlm_bounded_cells.R
application/scripts/10_test_rqr_dlm_monitor_wrapper.sh
application/tests/testthat/test-rqr-dlm-bounded-config.R
application/tests/testthat/test-rqr-native-sampler.R
```

Inspect all tracked evidence named:

```text
docs/audits/rqr_dlm_output10_*
```

The local full-chain RDS files are intentionally ignored. Their SHA-256,
byte counts, checkpoint/history digests, runtime provenance, and compact
diagnostics are tracked. Do not mistake absence of heavy chain objects from
Git for absence of an evidence contract.

## Claims that require independent audit

### 1. Signal/error finalization

Audit the shell wrapper as an adversarial failure boundary. Confirm or refute:

- one idempotent finalizer handles normal exit, error, `EXIT`, `INT`, `TERM`,
  and `HUP`;
- the finalizer drains the full PGID, escalates TERM to KILL, waits/reaps, and
  verifies no non-zombie PGID member remains;
- zero monitor samples do not break maxima calculation;
- monitor/PGID/finalizer errors fail closed;
- a structured failure row, resource summary, wrapper closeout, and recursive
  artifact hashes are produced on every tested failure path;
- normal execution cannot pass after a signal, monitor failure, required KILL
  escalation, nonzero runner status, or incomplete finalization;
- the five fault scenarios actually test the claimed cases and verify the
  produced hashes rather than only checking file existence.

Look especially for shell semantics that can silently continue after an
arithmetic, pipeline, trap, `wait`, `ps`, or hashing failure. The prior exact
preflight exposed and corrected misuse of Bash's special `_` variable; verify
that no analogous weakness remains.

### 2. Required-estimand completeness

Independently derive the expected ordered counts:

```text
fixed-W local level:
  fixed 117, learned 118

frozen trend plus seasonal:
  fixed 181, learned 182

shared component-scale trend plus regression:
  fixed 149, learned 150
```

Confirm that the schema is built from the canonical fixture rather than from
the observed chain columns, that each chain must equal it exactly before RDS
publication, and that the cell-level diagnostic repeats the check.

Audit the omission-negative tests. All four chains omitting the same training,
time-zero, future, learned-lambda, or component-scale quantity must be rejected.
Check orientation and name order, not only counts.

### 3. Future draw identity and diagnostic target

Reproduce the old issue: `nd=n_save` invokes the explicit sampling path and can
permute saved rows.

Then verify the correction:

- stochastic sidecar forecasting uses `nd=NULL`;
- `draw_index` must be exactly `1:n_save`;
- draw-specific component-scale rows must equal the saved rows in order;
- primary mixing variables are deterministic future conditional-mean
  interval-root functionals obtained by propagating each terminal state
  through future `GG` without adding process noise;
- stochastic process-noise root paths remain sidecars;
- neither object implies a response-simulation contract.

Check that excluding `W` from the conditional mean is mathematically correct,
and that all future functions remain label-invariant after endpoint ordering.

### 4. Full-chain RDS publication

Audit the temporary-save/readback/rename contract. Confirm:

- only `rqr_dlm_mcmc` fits are accepted;
- existing final paths cannot be overwritten;
- temporary readback is class- and object-identical;
- checkpoint digest is independently recomputed;
- continuation history and digest are validated;
- size and SHA-256 are recorded before rename and unchanged afterward;
- any failure removes the temporary object and leaves no published final file;
- the fit/forecast failure ledger captures publication errors.

Decide whether any additional provenance field needs independent
reconstruction before publication, given that exact object identity and the
pre-save target/runtime provenance gate are both required.

### 5. Continuation and RNG raw validation

Confirm that aggregate history booleans are validated as length-one,
nonmissing logical values before `isTRUE()` comparisons. Confirm that saved
RNG state is finite, integral, within the representable non-`NA` integer range,
and complete before integer coercion.

Audit the recomputed-digest negative tests for numeric, missing, vector-valued,
fractional, infinite, overflow, and semantic mutations.

### 6. Varying component-scale future reference

Verify that the new reference uses two distinct saved component-scale vectors,
keeps their rows aligned with `draw_index`, reconstructs draw-specific future
covariances, and compares group-specific Gaussian means and variances with the
correct Monte Carlo standard errors.

Tracked results report:

```text
mean standardized error:      1.187000 <= 5
variance standardized error:  1.142024 <= 6
orientation:                  pass
```

### 7. Frozen schedule and one-cell evidence

The prospective schedule is:

```text
chains:                 4
burn-in:                2,000 per chain
retained:               6,000 per chain
thin:                   1
seeds/starts:           unchanged
backend:                cpp
numerical policy:       fail
R-hat gate:             <= 1.01
bulk/tail ESS gates:    >= 1,000
whole-grid timeout:     240 minutes
retry/retune:           prohibited
```

The one-cell benchmark is the shared component-scale trend-plus-regression
fixture with learned normalized loss scale. Independently audit all 150 rows,
not just the minima.

Tracked summary:

```text
150 / 150 diagnostic rows pass
maximum R-hat:       1.004914
minimum bulk ESS:    1,456.575
minimum tail ESS:    2,107.193
zero numerical/forecast repairs
zero failure rows
four exact-source/provenance-eligible fits
four chain RDS readbacks pass
wrapper sampled duration: approximately 740 seconds
sum of per-fit elapsed times: 718.919 seconds
full-chain bytes: 45,304,976
peak sampled RSS: 396,188 KiB
```

The previously failing regression component quantities report bulk ESS
1,456.575 and 1,459.698.

Decide whether this is sufficient evidence for the 6,000-retained schedule or
whether a concrete, predeclared concern remains. Do not recommend weakening
the gates or changing seeds after seeing the result.

### 8. Reference and execute binding

Verify all 43 reference gates, their tracked hash manifest, exact
runtime/toolchain/config binding, and the fail-closed execute manifest.

The negative execution test supplied the correct source SHA, reference
artifact SHA, runtime/toolchain, and confirmation phrase. It still produced:

```text
status: blocked_by_execution_contract
reference binding: true
authorization: false
chain files: 0
```

Confirm that environment variables alone cannot enable the 24 fits.

### 9. Protected-repository isolation

Confirm from source that exdqlm is never compiled, installed, or loaded from
its checkout. It must be archived read-only and built under the ignored cache,
with before/after guards including ignored files. Confirm that Q-DESN is
read-only. Do not modify either repository during this review.

## Required decisions

Give explicit, separate decisions for:

```text
statistical target and interpretation
monitor/finalization contract
estimand completeness contract
future draw/diagnostic contract
RDS publication contract
continuation/RNG hardening
43-gate reference bundle
6,000-retained one-cell benchmark
change only the execution flag
create a separate authorization commit after review
run the bounded 24-fit validation
matched/production RQR-DLM simulation
CAVI/ELBO
RQR-DESN
```

Do not authorize matched/production simulation merely because bounded
mechanics and mixing pass. Do not authorize CAVI/ELBO or RQR-DESN in this
review.

If the 24-fit bounded grid receives a conditional go, state the exact remaining
authorization sequence. At minimum it should require a separate reviewed
commit that changes the flag, a new exact isolated runtime, a new preflight and
reference bundle for the changed config digest, explicit user confirmation,
cell-by-cell fail-fast execution, and no retries/retuning.

## Remote deliverable protocol

Do not ask the user to download, rename, upload, or manually transmit your
audit files or SHA-256 values.

Create this review branch directly on the public remote from evidence commit
`99b6f92911a5cd323b735598063da72766ad9095`:

```text
chatgpt-pro/output11-audit-20260723
```

Add exactly these compact deliverables:

```text
external_reviews/chatgpt_pro_output11_20260723/
  chatgpt_pro_output11_audit_20260723.md
  chatgpt_pro_output11_codex_handoff_20260723.md
  chatgpt_pro_output11_findings_20260723.csv
  chatgpt_pro_output11_artifact_hashes_20260723.csv
```

The artifact-hash CSV must contain SHA-256 and byte count for the other three
deliverables. Do not include the hash CSV in its own hash list. Do not commit
heavy/generated objects, chain files, TeX outputs, compiler products, caches,
or simulation outputs. Do not modify implementation, manuscript, config, or
protected-reference files on the review branch.

Commit and push the four deliverables. Verify they exist on the remote before
responding. Your final chat response should be only:

```text
chatgpt-pro/output11-audit-20260723
<full 40-character pushed commit SHA>
```

That intentionally short response is the successful handoff signal. Codex will
fetch and verify the branch and artifacts directly, avoiding manual file
transfer and transcription errors.
