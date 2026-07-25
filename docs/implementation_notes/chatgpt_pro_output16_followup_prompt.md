# ChatGPT Pro Output-16 independent launch review

You are performing the final independent pre-authorization review of the
standalone RQR-GIBBS confirmatory RQR-DLM simulation.

Treat every Codex statement and tracked summary as a claim to verify from the
source. Do not infer a pass from the existence of a report. Be critical, but
distinguish a genuine launch blocker from an optional improvement that can be
made after the confirmatory run.

## Repositories and immutable scope

Primary public repository:

```text
https://github.com/AntonioAPDL/RQR-GIBBS
branch: main
exact implementation commit:
7b7c47204801032e5eb4fe6c9fd332aaaedead43
```

Compact reconciliation/evidence branch:

```text
branch: codex/output15-reconciliation-20260725
```

Resolve and report the exact tip of that review branch before using it.

GitHub access is read-only for this review. Use the connected GitHub app when
it is available. If the app is unavailable, use ordinary web access to fetch
the immutable public GitHub archive URL for the exact reconciliation commit
supplied in the user's message. An attached Codex-prepared archive with a
reported SHA-256 is only a last-resort fallback when public web retrieval also
fails. State which access path you used. The access route changes only
transport: it does not relax any source inspection, mathematical verification,
or decision requirement below.

Protected read-only references:

```text
exdqlm:
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN:
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

Do not modify RQR-GIBBS source, the exdqlm repository, or the Q-DESN
repository. Your output must be review-only files on a separate branch.

## Fixed scientific interpretation

The RQR update is a generalized-Bayes loss update, not an ordinary response
likelihood. The pseudo-AL construction augments the interval-loss update.
Interval-root draws are not posterior-predictive response draws.

Keep distinct:

1. conditional-mean RQR interval roots;
2. realized dynamic root paths; and
3. future generated responses used to evaluate operating characteristics.

Do not request response-predictive density scores for RQR unless a separate
response-simulation distribution is explicitly introduced. Do not convert the
same-data learned generalized-Bayes scale into a claim of empirical coverage
calibration.

The stacked two-root state has a joint Gaussian prior, but the augmented
observation term is quartic jointly in the two root states. One ordinary
simultaneous Gaussian FFBS draw is therefore unavailable. Alternating
root-specific FFBS updates are the exact blocked Gibbs steps for the fixed
joint target modes implemented here.

## Output-15 contract to verify

The design must reproduce exactly:

```text
incidence rows:                         208
included rows:                           89
explicitly omitted rows:                119
maximum software calls:              43,800
maximum logical fits:                 49,200
standard MCMC chains:                 38,400
extra embedded sentinel chains:        2,538
maximum total MCMC chains:            40,938
candidate/tuning fits:                     0
future response subreplications:           20
```

There is no disposable pilot. The preselected four-chain sentinels are part of
the final confirmatory run and its evidence.

Review these Output-15 files under
`external_reviews/chatgpt_pro_output15_20260724/`:

```text
chatgpt_pro_output15_audit_20260724.md
chatgpt_pro_output15_codex_handoff_20260724.md
chatgpt_pro_output15_findings_20260724.csv
chatgpt_pro_output15_final_design_matrix_20260724.csv
chatgpt_pro_output15_run_budget_20260724.csv
chatgpt_pro_output15_launch_gates_20260724.csv
chatgpt_pro_output15_artifact_hashes.csv
```

Review the new compact evidence under:

```text
docs/audits/rqr_dlm_output15_reference_evidence_20260725/
docs/audits/chatgpt_pro_output15_reconciliation_20260725.md
```

## Implementation areas to inspect

Inspect at least:

```text
application/config/rqr_dlm/rqr_dlm_main_simulation_20260724.R
application/scripts/lib/rqr_dlm_confirmatory_simulation.R
application/scripts/15_run_rqr_dlm_confirmatory_simulation.R
application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh
application/scripts/16_test_rqr_dlm_confirmatory_monitor.sh
application/scripts/17_launch_rqr_dlm_confirmatory_wave.R
application/tests/testthat/test-rqr-dlm-confirmatory-contract.R
application/R/rqr_oracle.R
application/tests/testthat/test-rqr-native-oracle.R
application/DESCRIPTION
Makefile
README.md
```

Also inspect the existing runtime-lineage, continuation, FFBS, model, fit,
forecast, and numerical-boundary files that these paths call.

## Required review questions

### 1. Design fidelity

Recompute the 208/89/119 incidence counts and all initial, central, and maximum
run budgets. Verify that the method/scenario/coverage/sample-size incidence
matrix implements the intended ADEMP comparisons and omissions without a
hidden tuning study.

Confirm that the six DGPs and their common-random-number pairing are
statistically coherent. In particular, verify:

- the stochastic harmonic seasonal innovation;
- the heteroscedastic heavy-tail construction;
- root-alignment versus quantile-target distinctions;
- state-driving versus response-driving stream separation;
- 20 future response substreams; and
- separation among conditional roots, realized roots, and generated
  responses.

### 2. RNG and sentinel integrity

Audit the complete L'Ecuyer-CMRG state allocation. Confirm that every task uses
a collision-checked full state, that future subreplications use independent
substreams, and that sentinel selection is performed before data realization.

Verify that the deterministic maximum task/wave plan contains every authorized
replication exactly once, orders sentinel work before standard work within
each batch, and cannot silently omit a required method or replication.

### 3. RQR-DLM and comparator correctness

Audit the RQR-DLM methods, fixed/learned generalized-Bayes scale modes,
fixed-W/frozen-discount/component-scale evolution variants, component-specific
discount construction, alternating FFBS scan, state/scale orientation, and
forecast-root contract.

Verify that exdqlm 1.1.0 and quantreg 6.1 are built/loaded only from isolated
CRAN artifacts and that the protected exdqlm checkout cannot be an execution
source. Confirm that:

```text
exdqlmMCMC uses the reduced-AL DQLM with dqlm.ind=TRUE
quantreg uses rq(..., method="br")
```

Check that raw endpoint forecasts are retained and ordering is applied only
for interval metrics.

### 4. Oracle/reference correctness

Independently audit the population-risk oracle for:

```text
Gaussian
skewed
standardized Student-t with 5 df
Gaussian-plus-t break mixture
```

at 80% and 90% coverage.

Check the analytic truncated first and second moments, infinite-boundary
handling, coverage and moment root equations, unrestricted/profile objective
agreement, and location-scale transformation.

Important nuance: the blocking high-precision gate compares attained
objectives, root-equation residuals, coverage residuals, and uniqueness.
Endpoint differences from two independent optimizations are sidecars because
the population-risk basin can be very flat. Decide whether this is
mathematically justified. Do not demand an arbitrary endpoint tolerance unless
you can show that it is required for the scientific estimand or reveals an
incorrect root.

The tracked exact evidence reports:

```text
preflight:       22/22 gates
oracle/reference: 15/15 gates
```

Verify the compact hashes and source/runtime binding.

### 5. Runtime lineage and authorization

Audit `rqrgibbs_runtime_attestation/5.0.0` and its builder/verifier:

- complete Git archive versus package file-set comparison;
- build and install receipts;
- exact single package input and library;
- successful exit statuses;
- source-package, runtime-tree, command, log, and marker binding;
- exact source commit and application-tree identity; and
- negative mixed-lineage tests.

Confirm that promotion execution requires an isolated runtime and cannot be
authorized from `pkgload::load_all()` or a separately installed package.

Audit the flag-only authorization check. A favorable review should authorize
one commit directly on top of:

```text
7b7c47204801032e5eb4fe6c9fd332aaaedead43
```

whose only semantic diff is:

```diff
- confirmatory_execution_authorized = FALSE
+ confirmatory_execution_authorized = TRUE
```

No execution is requested from you.

### 6. Wave execution and collection

Audit:

- eight sentinel workers and 32 standard workers at most;
- one declared numerical thread per worker;
- distinct sampled OS-thread envelopes;
- process-group cleanup and signal handling;
- atomic worker/wave writes;
- no retry and no reseeding;
- stop-after-failed-cell/wave behavior;
- exact task-set equality;
- recursive byte/file-count verification;
- symlink rejection;
- common provenance/runtime/config/seed binding;
- failure retention in the denominator; and
- refusal to analyze an incomplete run.

The OS thread distinction is deliberate:

```text
numerical threads per worker:              1
ordinary sampled OS-thread envelope:       2
oracle-reference sampled envelope:         4
```

All BLAS/OpenMP-related environment values are fixed at one. `NLWP` counts
non-numerical helper threads as well. The oracle reference invokes `R CMD
config` while recording the compiler toolchain, so it has a separately
declared helper-process envelope. Decide whether the implementation records
and enforces this distinction honestly.

### 7. Diagnostics, estimands, and precision stopping

Verify that the runner uses maintained `posterior` R-hat, bulk/tail ESS, and
MCSE implementations with correct draw shapes; fixed lambda is checked by
identity and excluded from stochastic diagnostics.

Confirm that diagnostics and compact outputs cover the required time-specific,
missing-boundary, terminal-state, future-root, component-scale, loss, width,
midpoint, and endpoint functionals.

Audit the replication-level MCSE calculations, paired contrasts, batch
completion, and precision-based stopping. Empirical coverage TOST is
descriptive/non-stopping. Performance signs must not drive continuation.

### 8. Evidence and failure contracts

Check that worker, wave, collection, and audit artifacts are atomic,
schema-checked, recursively hashed, and complete. Confirm that a failure cannot
be converted into a success by deleting a shard, changing a manifest, or
intersecting away a missing diagnostic column.

Audit the two fail-closed paths: direct execution and wave launch must exit
before creating their requested output roots while the flag is false.

## Exact local evidence reported by Codex

The compact tracked binding records:

```text
main source commit:
  7b7c47204801032e5eb4fe6c9fd332aaaedead43
application tree:
  29f938a8359e0c8bf23c41584f91c0b1fd38e25b
package:
  rqrgibbs 0.1.0.9017
runtime tree:
  608e59fd99c9f16e13d1dd9965d599a34e39e7dbdfbeca024ecd3221567c90c2
runtime attestation SHA-256:
  d07343d2f2f77fc17d34401e6a42a83c6de97e93276614ba96c9812897ae7598
```

Reported final validation:

```text
R CMD check:                  Status: OK
native R/C++ suite:           PASS
standalone contract suite:    PASS
main article:                 PASS, 9 pages
supplement:                   PASS, 10 pages
direct fail-closed:           PASS
wave fail-closed:             PASS
confirmatory fits executed:   0
```

The full ignored seed ledger is about 87.6 MB and is intentionally not tracked.
Its SHA-256 and those of the full task/wave plans are in the compact evidence.

If you cannot rerun R code in your environment, say so explicitly. Source
inspection and independent mathematical recomputation are still useful, but
do not describe tracked execution evidence as independently rerun.

## Required decisions

Give an explicit verdict for each:

```text
Statistical target and interpretation
Output-15 design fidelity
Oracle/reference mathematics
RNG and sentinel contract
Runtime lineage
Authorization contract
Wave/process/resource contract
Collection/failure contract
Diagnostics and precision stopping
Repository validation evidence
```

Then answer these exact launch questions:

```text
Create the one-line flag-only authorization commit: GO / NO-GO
Rebuild and attest the runtime at that commit:       GO / NO-GO
Launch the complete confirmatory study afterward:   GO / NO-GO
Launch another disposable pilot first:              GO / NO-GO
Matched/confirmatory evidence interpretation:       bounded to what?
```

If you issue a no-go, provide:

1. a concrete source-level or mathematical counterexample;
2. the exact file/function/contract involved;
3. why the issue can change validity, reproducibility, or launch recovery; and
4. the smallest justified correction.

Separate blockers from nonblocking improvements. Do not expand scope into
CAVI/ELBO or RQR-DESN. Those remain deferred until the RQR-DLM confirmatory
simulation is complete.

## Review deliverables and Codex handoff

The standard ChatGPT GitHub app is read-only. Do not attempt to create a
branch, commit, push, open a pull request, or modify any repository. Codex will
perform all Git writes after verifying your returned bytes.

Create one downloadable ZIP named:

```text
chatgpt_pro_output16_review_bundle_20260725.zip
```

Place the review files directly under this directory inside the ZIP:

```text
external_reviews/chatgpt_pro_output16_20260725/
```

Permitted deliverables:

```text
chatgpt_pro_output16_audit_20260725.md
chatgpt_pro_output16_codex_handoff_20260725.md
chatgpt_pro_output16_findings_20260725.csv
chatgpt_pro_output16_launch_decision_20260725.json
chatgpt_pro_output16_artifact_hashes.csv
```

You may omit the JSON only if the same machine-evaluable fields are included
in the findings CSV. Do not change source, config, manuscript, tests, or prior
review files.

Create the artifact-hash manifest after the other deliverables are final. It
must list the relative path, byte count, and SHA-256 of every permitted
deliverable except the hash manifest itself. Build the ZIP from exactly those
final files. Compute the ZIP SHA-256 after the ZIP is final.

Return:

```text
access path used: GitHub app / attached exact archive
reviewed main commit: 7b7c47204801032e5eb4fe6c9fd332aaaedead43
reviewed reconciliation branch tip: <exact 40-character SHA>
bundle: chatgpt_pro_output16_review_bundle_20260725.zip
bundle SHA-256: <exact 64-character SHA-256>
```

Attach the single ZIP to the response. Do not ask the user to download,
rename, copy, paste, or compute individual files or SHA-256 values. The user
will attach this one ZIP to Codex; Codex will verify the member paths, file
set, byte counts, component hashes, and bundle hash, then create and push the
review-only branch.
