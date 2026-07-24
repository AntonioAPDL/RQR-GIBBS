# ChatGPT Pro Output-15 follow-up prompt

You are performing an independent, adversarial-but-constructive review of the
standalone RQR-GIBBS repository after Codex reconciled your Output-14 audit.
Treat every Codex statement and tracked closeout as a claim to verify from
source and compact evidence.

## Repositories and exact scope

Primary repository:

```text
repository: AntonioAPDL/RQR-GIBBS
branch: main

implementation and reference-run source:
  6ba47d1d686e7f47d90bf3110fbbe77f8da96fee

compact evidence and reconciliation:
  a5a08811912d7175bbbcec98e8f8af254fd51f51
```

The latest `main` may contain only this follow-up prompt after the evidence
commit. Review the implementation at `6ba47d1...` and the evidence packet at
`a5a0881...`; report any later prompt-only commit separately.

Protected read-only references:

```text
AntonioAPDL/exdqlm
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

AntonioAPDL/Article-Q-DESN---Version-2
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

Do not modify either protected repository. Do not ask Codex to compile, load,
or install from an exdqlm checkout. The only allowed simulation-comparator
runtime sources are the pinned CRAN archives materialized under ignored
RQR-GIBBS cache directories.

Output-14 source review:

```text
branch: chatgpt-pro/output14-audit-20260724
commit: 2be17bd5710e62168970577796c8ddc1872ffde6
directory:
  external_reviews/chatgpt_pro_output14_20260724/
```

## Fixed statistical interpretation

Do not reinterpret the project as an ordinary response-likelihood model.

- RQR uses a loss-based generalized-Bayes update.
- The pseudo-AL representation augments the pseudo-residual product; it is
  not an asserted response likelihood.
- State draws describe the two interval-root processes.
- Ordered root summaries are not posterior-predictive response draws.
- Response coverage is a repeated-sampling operating characteristic of a
  point interval.
- A future response-simulation distribution is absent unless explicitly
  defined later.
- The stacked two-root prior is Gaussian, but the joint augmented observation
  kernel is quartic. One ordinary simultaneous Gaussian FFBS draw is not
  available. Sequential root-specific FFBS updates are exact blocked Gibbs
  steps for the declared fixed-joint evolution modes.
- Adaptive conditional-discount recursion remains a working update, not exact
  Gibbs for a fixed joint target.

Reject any source, report, or proposed empirical metric that violates these
distinctions.

## What Output-14 concluded

Output-14 accepted the completed 24-fit bounded RQR-DLM validation with three
nonblocking corrections to the reusable promoter. It rated the preliminary
simulation design `B`, requiring revisions before implementation. It did not
authorize a diagnostic pilot or confirmatory simulation.

The accepted bounded result remains:

```text
fits: 24 / 24
diagnostics: 897 / 897
maximum rank-normalized R-hat: 1.00490775707187
minimum bulk ESS: 1116.97123864205
minimum tail ESS: 1657.19298205554
numerical repairs: 0
forecast repairs: 0
failed fits: 0
```

Codex did not rerun that bounded grid.

## Codex changes to audit

### 1. Reusable bounded-evidence promoter

Inspect:

```text
application/scripts/11_promote_rqr_dlm_bounded_evidence.R
application/scripts/lib/rqr_dlm_evidence_promotion.R
application/config/rqr_dlm/
  rqr_dlm_output13_bounded_expected_bundle_20260724.json
application/tests/testthat/
  test-rqr-dlm-bounded-evidence-promotion.R
```

Verify independently that:

1. promotion requires exact equality with an external expected primary
   commit, application tree, config digest, reference manifest digest,
   runtime tree, runtime attestation, and toolchain digest;
2. each reopened fit is rehashed;
3. checkpoint-state and continuation-history digests are recomputed rather
   than copied;
4. the package continuation-history validator is actually invoked;
5. the exact 24 fit IDs are derived from the fit plan;
6. all relevant compact tables require unique exact fit-ID set equality; and
7. deterministic tests genuinely reject mismatches, missing/duplicate/extra
   IDs, semantic mutations, and validator failures.

Look for a reachable false positive, not only missing assertions in prose.
Decide whether P1--P3 are fully closed for future reuse. Do not use a concern
about future reuse to retroactively reject the already accepted bounded run
unless you find a concrete counterexample that changes its conclusion.

### 2. Preliminary simulation schema 0.2.0

Inspect:

```text
application/config/rqr_dlm/
  rqr_dlm_main_simulation_preliminary_20260724.R
  rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv
  rqr_dlm_main_simulation_preliminary_methods_20260724.csv
docs/implementation_notes/
  rqr_dlm_main_simulation_preliminary_spec_20260724.md
application/tests/testthat/
  test-rqr-dlm-main-simulation-preliminary-config.R
```

Audit whether the revision actually closes Output-14 findings S03--S22 where
required at this stage:

- identifiable and fully parameterized DGPs;
- positive scale and minimum separation;
- matched Gaussian/skewed trend-seasonal pair;
- common-evolution ablation;
- one response law for the independent-root sensitivity;
- target-aligned endpoint errors;
- separately labeled cross-target distances;
- conditional-mean versus realized future root targets;
- 90% TOST coverage qualification inside `[-0.02, 0.02]`;
- paired width comparisons only after coverage qualification;
- Monte Carlo replication and MCSE rules;
- training-only frozen tuning and equal search budgets;
- diagnostic-pilot four-chain subset;
- confirmatory within-chain and preselected sentinel rules; and
- fail-closed execution flags.

Check all scenario formulas, dimensions, state laws, innovation covariance
specifications, scale formulas, structural-break rules, mixture parameters,
transition probabilities, and shared-random-number claims. Identify any DGP
that is mathematically incomplete, internally inconsistent, nonidentifiable
for its claimed contrast, or impossible to simulate deterministically from
the declared contract.

Critically assess whether the design is sufficiently aligned with standard
statistical simulation-study practice (ADEMP, target alignment, Monte Carlo
error, transparent qualification, and fair tuning) without making it
needlessly large. Recommend reductions only if they preserve the main
scientific contrasts.

### 3. Oracle implementation and certificates

Inspect:

```text
application/R/rqr_oracle.R
application/man/rqr_oracle_risk.Rd
application/man/rqr_oracle_certificate.Rd
application/tests/testthat/test-rqr-native-oracle.R
```

The implemented families are Gaussian, Student-\(t\), centered standardized
lognormal, and standardized skewed normal-\(t\) mixture.

Verify:

- the population RQR risk and stationary equations;
- location-scale equivariance;
- normalization of the non-Gaussian laws;
- coverage-profile construction and basin search;
- the independent unrestricted midpoint/log-width multistart optimization;
- parameter transformations and integration domains;
- objective-gap, coverage, moment, curvature, and separation checks;
- the meaning of the minimizer-set and uniqueness fields;
- deterministic distribution/solver digests; and
- that quadrature error is described as estimated, not rigorously bounded.

The tracked exact-runtime stage reports eight passing
family-by-coverage certificates. Recompute selected roots and objectives
independently if your environment permits. If not, distinguish source audit
from independent numerical execution.

### 4. Comparator sources and adapters

Inspect:

```text
application/scripts/12_prepare_exdqlm_cran_runtime.R
application/scripts/12_prepare_quantreg_cran_runtime.R
application/scripts/lib/rqr_dlm_main_simulation.R
application/scripts/13_run_rqr_dlm_main_simulation_references.R
```

The frozen sources are:

```text
exdqlm 1.1.0
  https://cran.r-project.org/src/contrib/exdqlm_1.1.0.tar.gz
  51bc968f617721c9ab1dcfc6ec14857d30827fcd36659f3de45337cc3c82bd14

quantreg 6.1
  https://cran.r-project.org/src/contrib/quantreg_6.1.tar.gz
  f42292c5ab25a15f39295b93391deafef192fe09eefde563399a64eba7e0169a
```

Verify source identity, single-input installation, ignored isolated paths,
runtime-tree rehashing, and the absence of protected-checkout use. Audit the
exdqlm reduced-AL/DQLM MCMC argument contract, combined component orientation,
forecast horizon orientation, and raw quantile retention. Audit the
`quantreg::rq(method="br")` endpoint orientation fixture and crossing policy.

Decide whether these attestations are sufficient for a bounded diagnostic
pilot. Do not require a claim of bit-for-bit reproducible compilation across
machines unless that claim is actually made.

### 5. DGP, seeds, atomic artifacts, and runner modes

Inspect:

```text
application/scripts/lib/rqr_dlm_main_simulation.R
application/scripts/13_run_rqr_dlm_main_simulation_references.R
application/scripts/14_promote_rqr_dlm_main_simulation_references.R
application/tests/testthat/test-rqr-dlm-main-simulation-reference.R
```

Verify:

- SHA-keyed independent seed streams;
- paired response laws across coverage levels;
- all nine DGP constructors;
- missing/future boundaries where relevant;
- atomic write rollback under injected failure;
- recursive compact artifact manifests;
- exact isolated primary-runtime binding;
- exact comparator binding;
- byte-identical tiny replay;
- strict failure of unimplemented execution modes before output;
- exact stage artifact-set verification; and
- identical runtime binding across promoted stages.

Try to defeat the promoter with an omitted/extra artifact, altered stage
manifest, mixed primary runtime, comparator substitution, false authorization
flag, failed oracle row, nonzero repair, or executable pilot plan.

### 6. Tracked compact evidence

The promoted packet is:

```text
docs/audits/
  rqr_dlm_main_simulation_reference_evidence_20260724/
```

The reconciliation is:

```text
docs/audits/chatgpt_pro_output14_reconciliation_20260724.md
```

Verify the outer and per-stage artifact manifests against the tracked bytes.
Verify these claims:

```text
primary source commit:
  6ba47d1d686e7f47d90bf3110fbbe77f8da96fee

application tree:
  be1d908103c1d4e32b3fec6275a9dcd732d58b13

primary runtime tree digest:
  9b21f4cde66058aea8885cd9feab01836f76096e042c39f204916bf2c164f458

stages:
  preflight: pass
  oracle-reference: pass, 8/8
  tiny-end-to-end: pass, byte-identical, zero repairs
  diagnostic-pilot-preflight: pass, 672 planned fits, none authorized

diagnostic-pilot execution authorized: false
confirmatory execution authorized: false
```

Confirm that no stage is mislabeled as a simulation result and that no
response-predictive claim appears.

## Validation claims to assess

Codex reports:

```text
make smoke: pass
make pdf: pass
make supplement: pass
make test-native: pass
make test-standalone-contracts: pass
make package-check: Status: OK
```

Raw build products and local cache outputs are ignored. If you cannot rerun
these checks, say so and audit the tracked source without presenting Codex's
run as your independent execution.

## Required decisions

Give explicit verdicts for:

1. Output-14 promoter corrections P1--P3;
2. oracle mathematics and global-minimum certificate;
3. DGP identification and response-law completeness;
4. method fairness and target-aligned estimands;
5. TOST and Monte Carlo precision rules;
6. exdqlm and quantreg isolation/adapters;
7. seed, atomic-write, and artifact-promotion contracts;
8. the four exact-runtime reference-stage results;
9. whether Codex may now implement a diagnostic-pilot runner;
10. whether Codex may execute that pilot after another source review;
11. confirmatory main simulation; and
12. continued deferral of CAVI/ELBO and RQR-DESN.

Use this decision vocabulary:

```text
PASS
PASS WITH NONBLOCKING CORRECTIONS
REVISION REQUIRED
GO TO IMPLEMENT DIAGNOSTIC-PILOT RUNNER
CONDITIONAL GO TO EXECUTE DIAGNOSTIC PILOT
NO-GO
DEFER
```

If you authorize implementation, provide the smallest exact runner contract:
cells, chains, burn-in, retained draws, initialization, tuning subset,
diagnostic estimands, R-hat/ESS/MCSE thresholds, cell-level stop rules,
resource monitoring, atomic artifact schemas, failure behavior, and the
separate false-to-true authorization step. Do not authorize confirmatory
execution merely because reference stages passed.

## Required remote deliverables

Do not return downloadable sandbox links as the primary handoff. Create a new
branch in `AntonioAPDL/RQR-GIBBS` based exactly on evidence commit
`a5a08811912d7175bbbcec98e8f8af254fd51f51`:

```text
chatgpt-pro/output15-audit-20260724
```

Add exactly these four files:

```text
external_reviews/chatgpt_pro_output15_20260724/
  chatgpt_pro_output15_audit_20260724.md
  chatgpt_pro_output15_codex_handoff_20260724.md
  chatgpt_pro_output15_findings_20260724.csv
  chatgpt_pro_output15_artifact_hashes.csv
```

Requirements:

- The audit is complete and self-contained.
- The Codex handoff is copy-paste ready and lists exact blockers and allowed
  next actions.
- The findings CSV has stable finding IDs, area, verdict, severity,
  required stage, launch-blocker status, evidence, and disposition.
- The artifact-hash CSV records SHA-256 and byte count for the other three
  deliverables.
- Commit and push the branch.
- Do not modify `main`.
- Do not modify either protected repository.
- In the chat response, return only:

```text
chatgpt-pro/output15-audit-20260724
<full 40-character commit SHA>
```

If GitHub write access is unavailable, state that explicitly instead of
claiming that the branch was pushed.
