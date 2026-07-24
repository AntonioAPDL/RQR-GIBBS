# ChatGPT Pro Output-15 follow-up prompt

You are the independent senior Bayesian computation and statistical
simulation reviewer for the standalone RQR-GIBBS project. Codex reconciled
your Output-14 audit; your task is now to convert the revised design into the
smallest scientifically strong, computationally feasible, implementation-ready
confirmatory simulation contract.

This is a decision-oriented launch review, not an invitation to begin another
open-ended audit cycle. Treat every Codex statement and tracked closeout as a
claim to verify, but distinguish:

- a concrete defect that can change the scientific conclusion, target,
  execution safety, or reproducibility;
- a correction required before the main run;
- a useful nonblocking refinement; and
- an aspirational hardening idea that does not justify delaying the study.

A launch blocker must have a reachable counterexample or a clear scientific
consequence. Do not move the acceptance boundary because a theoretically
stronger provenance or numerical system could be imagined. Conversely, do
not approve an underspecified or computationally infeasible design merely
because the bounded sampler validation passed.

Your successful output is one unambiguous recommended main-study design,
including an exact scenario-by-method incidence matrix, MCMC schedule,
replication/Monte Carlo precision rule, computational budget, embedded
sentinel order, artifact contract, stop rules, and explicit go/no-go decision.
Codex should be able to implement it without guessing.

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

The latest `main` may contain only post-evidence direction documents after the
evidence commit. Review the implementation at `6ba47d1...` and the evidence
packet at `a5a0881...`; report the later direction-only commit separately.

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

## User decision after Output-14

The user does **not** want a separate diagnostic pilot. Do not propose or
authorize a standalone pilot grid.

The requested path is:

```text
independent review of the frozen design and reference evidence
  -> implement the confirmatory main-simulation runner
  -> independent source/preflight review
  -> separate false-to-true main-run authorization commit
  -> execute the main simulation with embedded safeguards
```

The main run may begin with a predeclared sentinel batch and cell-level stop
gates, but that batch must be part of the frozen confirmatory run:

- its replications are selected before execution;
- they use the final methods, seeds, schedules, estimands, and output schemas;
- passing sentinel replications remain in the final analysis;
- no tuning or design change may be made after inspecting them;
- a failed gate stops the run and precludes confirmatory claims; and
- restarting after a correction requires a new source commit, authorization,
  and complete run rather than silently continuing a changed design.

The tracked `diagnostic-pilot-preflight` artifact is therefore historical
planning evidence only. Its 672 rows are neither an authorized execution plan
nor the final main-run plan. Audit its useful seed, comparator, and
fail-closed machinery, but do not ask Codex to execute it.

### Why this is a defensible choice

The separate pilot is being omitted for substantive reasons:

1. The 24-fit bounded validation already exercised all three declared dynamic
   evolution structures, both learning-rate modes, four chains per cell,
   missing observations, future states, continuation, C++ FFBS, exact-runtime
   provenance, and modern MCMC diagnostics.
2. The exact-runtime oracle and tiny end-to-end stages independently check the
   new main-study DGP, seed, output, and comparator plumbing.
3. A small performance pilot would be too imprecise to answer the study's
   coverage and width questions and could encourage outcome-driven redesign.
4. Predeclared sentinel replications inside the final run provide the needed
   scaling and convergence protection without discarding valid simulations or
   creating a second, differently configured experiment.

Audit whether those premises are true. If an essential risk is not covered,
specify the narrowest embedded sentinel or preflight gate that closes it.
Do not relabel that safeguard as a standalone pilot.

## Scientific questions the simulation must answer

The final study should be no larger than needed to answer these questions:

1. Does dynamic RQR-DLM recover and forecast population RQR interval roots
   under static, locally evolving, multicomponent, asymmetric, and stressed
   mechanisms?
2. When component evolution genuinely differs, what is gained by
   component-specific evolution relative to a common-evolution RQR-DLM?
3. What is gained by dynamic roots relative to fixed-design RQR and frozen
   discount evolution?
4. How do RQR-targeted intervals compare with dynamic and static
   equal-tailed quantile intervals in repeated-sampling coverage, width, loss,
   endpoint recovery relative to each method's own target, failures, and
   computation?
5. Which conclusions are robust to learning-rate choice, training length,
   heteroscedasticity, and a composite break/heavy-tail stress mechanism?

The study must not claim that:

- RQR roots are equal-tailed quantiles under asymmetry;
- generalized-Bayes root draws are response-predictive draws;
- learned lambda is coverage calibration or a response variance;
- one composite stress DGP separately identifies every kind of
  misspecification; or
- a method is globally superior outside the simulated operating regimes.

## Literature grounding

Use the repository bibliography and the project's supplied primary sources
where they materially affect the design:

```text
refs.bib
Bissiri--Holmes--Walker on generalized Bayes
Yu--Moyeed and Kozumi--Kobayashi on Bayesian quantile regression/AL augmentation
Goncalves--Migon--Bastos on dynamic quantile linear models
Gneiting--Raftery on proper scoring rules
the supplied RQR/calibration papers
standard ADEMP and Monte Carlo simulation-study guidance
```

Check current package/API facts against the pinned source and official
documentation. Cite primary methodological sources for recommendations on
simulation design, Monte Carlo error, equivalence/qualification, MCMC
diagnostics, and interval scoring. Keep the literature review targeted: a
source should change or justify a design choice, not merely lengthen the
report.

If common practice does not uniquely determine a choice, state the tradeoff
and recommend one frozen value. Do not leave Codex with a menu of equally
possible designs.

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
- embedded confirmatory multichain sentinel subset;
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

### Required design optimization

Do not assume that every method belongs in every DGP or that every sensitivity
requires the same replication count as the core comparison. Construct an
explicit incidence matrix and classify each cell as:

```text
core confirmatory
targeted ablation
targeted sensitivity
noncompetitive oracle/reference
omit, with reason
```

At minimum, determine:

- which DGPs are essential, redundant, or sensitivity-only;
- which methods are scientifically meaningful for each DGP;
- whether both coverage levels are needed in every sensitivity;
- where the true-\(W\) oracle is informative;
- where the common-evolution ablation identifies a real contrast;
- where the Gaussian response-model sensitivity is appropriate;
- whether fixed-design and rolling empirical baselines need every dynamic
  mechanism;
- whether learning-rate sensitivities can be restricted to representative
  mechanisms without weakening the conclusions; and
- whether training-length sensitivities should use one representative
  coverage level and a smaller targeted method set.

Use paired common random numbers wherever valid and base primary comparisons
on paired replication-level contrasts. Preserve failed fits in denominators.
No method may receive test-data tuning, a larger tuning budget, favorable
retries, outcome-driven chain extensions, or different response draws.

Compute the exact number of:

- DGP replications;
- method fits;
- separate lower/upper quantile fits;
- MCMC chains;
- training-validation candidate fits;
- forecast evaluations; and
- expected compact and ignored artifacts.

Use the bounded elapsed-time evidence as a lower-level benchmark, while
acknowledging that \(T=200\), tuning, and comparator fits can cost more.
Provide optimistic, central, and conservative wall-time and storage estimates
under realistic Jerez concurrency. If the current design is impractical,
return a scientifically justified reduced design rather than merely saying it
is too large.

### Replication and Monte Carlo precision

Assess whether the present 500-minimum, 250-batch, 2,500-maximum rule is
appropriate for each estimand and stratum. The stopping rule may use only
predeclared Monte Carlo precision—not performance ranking, favorable signs,
significance, or coverage qualification.

Specify:

- the initial confirmatory replication count;
- batch size and maximum;
- formulas for MCSE of coverage, means, failure rates, and paired contrasts;
- how MCSE is aggregated across horizons;
- which primary summaries must meet precision before stopping;
- what happens when only some strata meet precision;
- how TOST is reported if precision is insufficient at the maximum; and
- whether the existing nominal 1,083/609 and approximate 1,600/900 counts
  should be replaced or reconciled.

Do not confuse the 90% TOST interval inside \([-0.02,0.02]\) with proof of
exact nominal coverage. Explain what the qualification supports and how width
comparisons are conditioned on it.

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

Decide whether these attestations are sufficient for the confirmatory main
simulation. Do not require a claim of bit-for-bit reproducible compilation
across machines unless that claim is actually made.

Include the full computational cost of the dynamic quantile comparator:
separate lower and upper MCMC fits, training-only tuning, and forecast
generation. Check that the proposed comparison gives it matched state
components, covariates, origins, horizons, and a fair tuning budget without
pretending its equal-tailed target equals the RQR target.

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
flag, failed oracle row, nonzero repair, or a historical preflight plan
incorrectly treated as an authorized main-run plan.

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

## Exact main-run contract you must return

Whether your verdict is go or revise, return a concrete recommended contract
with the following sections.

### A. Frozen ADEMP table

State the final aims, DGPs, estimands, methods, and performance measures.
Label every primary, secondary, diagnostic, sensitivity, and
noncompetitive-oracle item.

### B. Scenario-by-method incidence matrix

Give one row per DGP × coverage × method family, with:

```text
cell_id
DGP
coverage
method
role
replication_rule
chains_per_replication
tuning_rule
forecast_horizons
primary_estimands
paired_contrast_group
include_or_omit_reason
```

This matrix must determine the run size exactly.

### C. MCMC and initialization schedule

Freeze burn-in, retained draws, thinning, chains, initialization,
within-chain diagnostics, multichain sentinels, continuation policy, fixed
lambda handling, component-scale handling, and failure behavior. Justify any
departure from the schedule that passed bounded validation.

One-chain main fits are acceptable only if you justify maintained
within-chain ESS/MCSE gates and preselected multichain sentinels. Sentinel
selection must be seed-derived before data generation. A sentinel failure must
stop its declared cell before later batches run.

### D. Ordered execution plan

Specify a deterministic order such as:

```text
exact-runtime preflight
embedded sentinel replications
core batch 1
cell diagnostics and integrity gates
additional precision-only batches
targeted ablations
targeted sensitivities
closeout and promotion
```

Passing sentinels are final-study observations. No post-sentinel retuning is
allowed. A source or design correction invalidates the incomplete run and
requires a new complete run from a new authorization commit.

### E. Resource and failure envelope

Specify concurrency, BLAS/OpenMP limits, wall-time ceilings, process-group or
cgroup monitoring, sampled RSS interpretation, free-space checks, maximum
artifact size, atomic writes, checkpoint frequency, failure ledger, retry
policy, and emergency stop behavior. Distinguish a failed method fit retained
for operating-characteristic analysis from infrastructure failure that
invalidates a replication.

### F. Analysis and reporting plan

Freeze:

- target-aligned endpoint errors;
- cross-target distances labeled as such;
- conditional-mean and realized-future root errors;
- empirical coverage and width;
- held-out RQR loss;
- secondary central interval score;
- TOST coverage qualification;
- paired width and loss contrasts;
- failure probability;
- elapsed time and memory;
- horizon-specific and aggregated summaries;
- MCSE and uncertainty intervals; and
- multiplicity or hierarchy language for any inferential comparisons.

Recommend figures and tables that communicate the main scientific findings
without cherry-picking.

### G. Launch checklist

Give a binary checklist whose every row can be evaluated automatically before
the authorization commit. Separate:

```text
must pass before implementation is accepted
must pass before execution is authorized
must pass batch by batch during the main run
must pass before evidence promotion
nonblocking future hardening
```

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
9. whether Codex may now implement the confirmatory main-simulation runner
   without a separate pilot;
10. whether the frozen main run may execute after another source/preflight
    review and a separate authorization commit;
11. whether the embedded sentinel and cell-level stop design adequately
    replaces a standalone pilot; and
12. continued deferral of CAVI/ELBO and RQR-DESN.

Also provide:

13. a letter grade for the current schema-0.2 design;
14. a letter grade for your recommended optimized design;
15. exact total fit/chain/tuning counts for the recommended design;
16. central and conservative Jerez time/storage estimates; and
17. the three most important scientific messages the final design can
    legitimately support.

Use this decision vocabulary:

```text
PASS
PASS WITH NONBLOCKING CORRECTIONS
REVISION REQUIRED
GO TO IMPLEMENT CONFIRMATORY MAIN-SIMULATION RUNNER
CONDITIONAL GO TO EXECUTE CONFIRMATORY MAIN SIMULATION
NO-GO
DEFER
```

If you authorize implementation, provide the smallest exact runner contract:
full scenario/method/coverage/replication cells, embedded sentinel order,
chains, burn-in, retained draws, initialization, training-only tuning,
diagnostic estimands, R-hat/ESS/MCSE thresholds, cell-level stop rules,
Monte Carlo precision stop rules, resource monitoring, atomic artifact
schemas, failure behavior, restart prohibition after design changes, and the
separate false-to-true authorization step. The main-run runner must remain
unexecutable until that implementation and its exact preflight bundle receive
another review.

If you require revision, do not stop at criticism. Supply exact replacement
values, rows, thresholds, or algorithms for every blocking item so Codex can
implement the correction in one pass. Mark optional preferences explicitly as
nonblocking.

## Required remote deliverables

Do not return downloadable sandbox links as the primary handoff. Create a new
branch in `AntonioAPDL/RQR-GIBBS` based exactly on evidence commit
`a5a08811912d7175bbbcec98e8f8af254fd51f51`:

```text
chatgpt-pro/output15-audit-20260724
```

Add exactly these seven files:

```text
external_reviews/chatgpt_pro_output15_20260724/
  chatgpt_pro_output15_audit_20260724.md
  chatgpt_pro_output15_codex_handoff_20260724.md
  chatgpt_pro_output15_findings_20260724.csv
  chatgpt_pro_output15_final_design_matrix_20260724.csv
  chatgpt_pro_output15_run_budget_20260724.csv
  chatgpt_pro_output15_launch_gates_20260724.csv
  chatgpt_pro_output15_artifact_hashes.csv
```

Requirements:

- The audit is complete and self-contained.
- The Codex handoff is copy-paste ready and lists exact blockers and allowed
  next actions.
- The findings CSV has stable finding IDs, area, verdict, severity,
  required stage, launch-blocker status, evidence, and disposition.
- The final design matrix is the exact recommended scenario × coverage ×
  method incidence matrix and contains no prose-only placeholders.
- The run-budget CSV reconciles DGP replications, fits, chains, tuning fits,
  lower/upper comparator fits, expected wall time, concurrency, and storage.
- The launch-gates CSV has machine-evaluable gate IDs, stages, thresholds,
  failure actions, and blocking status.
- The artifact-hash CSV records SHA-256 and byte count for the other six
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
