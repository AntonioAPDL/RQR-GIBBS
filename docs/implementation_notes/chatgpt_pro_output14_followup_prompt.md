# ChatGPT Pro Output-14 follow-up prompt

Perform an independent source, evidence, and statistical-design audit of the
completed bounded RQR-DLM validation and the preliminary matched-simulation
specification in the public `AntonioAPDL/RQR-GIBBS` repository. Treat the
Codex closeout, manifests, diagnostics, and design rationale as claims to
verify, not as proof.

Do not ask me to upload files. Read all required files from the GitHub remote
at the exact commits below. Do not modify implementation, configuration,
manuscript, evidence, exdqlm, or the Q-DESN article. Do not run or authorize a
main simulation. Your only write action is to push the four review
deliverables specified at the end to a new review branch.

## Exact states

```text
Output-13 review:
  branch: chatgpt-pro/output13-audit-20260724
  commit: d13ec6f3b9761349d7283b17f47724c0f42532cf

accepted corrected implementation:
  da4d265af6d8c6d6f9be06bfe2a91bfae88501d8

one-time bounded authorization and execution source:
  afc9c5fed14c66317b684fc9b9f6d01079c307cd

post-launch authorization revocation:
  82cb02dc96e3642864d2bc187640ee8fc50678bd

bounded closeout, compact evidence, and preliminary simulation design:
  e9db0bc8ba16d5e6ed76ef9378ca875b2ccf0769

package at execution:
  rqrgibbs 0.1.0.9012

fit schema:
  rqrgibbs_fit/1.9.0

runtime-attestation schema:
  rqrgibbs_runtime_attestation/5.0.0

bounded estimand schema:
  rqrgibbs_dlm_bounded_estimands/1.0.0

preliminary simulation schema:
  rqrgibbs_dlm_main_simulation_preliminary/0.1.0
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

Do not propose or make changes in either protected repository. If the eventual
dynamic quantile comparator uses CRAN exdqlm 1.1.0, require a pinned source
tarball and isolated build; do not load, compile, or install from any exdqlm
checkout.

## Fixed scientific interpretation

Preserve these distinctions:

- RQR is a generalized-Bayes interval-root loss update.
- The normal-exponential pseudo-AL construction augments an exponentiated
  pseudo-residual loss kernel; it is not an ordinary response likelihood.
- Ordered root and state draws are not posterior-predictive response draws.
- Fixed `W`, frozen discount-template, and shared component-scale modes use
  exact alternating root-specific FFBS full-conditionals for their declared
  fixed joint targets.
- The adaptive conditional-discount recursion is a working method and remains
  excluded.
- Same-data learned `lambda` is not automatic coverage calibration or a
  response variance.
- The bounded run can validate target mechanics, numerical behavior, mixing,
  continuation, provenance, missingness, and future-root operations. It cannot
  establish empirical coverage or comparative performance.
- The preliminary simulation is RQR-DLM MCMC scope. RQR-DESN and CAVI/ELBO
  remain deferred.

## Read and authenticate the completed bounded evidence

Read completely:

```text
external_reviews/chatgpt_pro_output13_20260724/
docs/audits/rqr_dlm_output13_bounded_closeout_20260724.md
docs/audits/rqr_dlm_output13_bounded_20260724/
application/scripts/11_promote_rqr_dlm_bounded_evidence.R
application/config/rqr_dlm/rqr_dlm_bounded_dynamic_fixtures_20260723.R
application/scripts/08_run_rqr_dlm_bounded_validation.sh
application/scripts/09_run_rqr_dlm_bounded_cells.R
application/scripts/lib/rqr_dlm_bounded_diagnostics.R
```

At `afc9c5f...`, verify that the only scientific-control authorization change
relative to its parent was:

```text
bounded_dynamic_execution_authorized:
  FALSE -> TRUE
```

Verify the exact configuration:

```text
3 fixtures
2 learning-rate modes
4 chains per cell
2,000 burn-in per chain
6,000 retained draws per chain
thin 1
C++ backend
fail numerical policy
sequential cell-level stopping
```

Verify that `82cb02d...` returned the configuration and its test to false.
The successful result must remain attributed to `afc9c5f...`, not to the
later evidence commit.

Rehash every file in
`docs/audits/rqr_dlm_output13_bounded_20260724/evidence_manifest.csv`.
Confirm that the promoted files which are direct copies agree with the
original run artifact manifest. Inspect the evidence-promotion script and
decide whether it independently checks:

- the complete original recursive file set, bytes, and SHA-256 values;
- all 24 ignored RDS identities;
- saved object, checkpoint, and continuation-history digests;
- 24 completed statuses and zero failure rows;
- exact target, numerical, provenance, missingness, and future-root gates;
- the maintained diagnostic thresholds; and
- the component-conditional reduction.

The full RDS objects, the 12 MB conditional table, and raw process monitor are
intentionally ignored and unavailable through GitHub. Do not claim to rehash
those local objects independently. Instead, decide exactly what is supported
by the tracked manifest and by the source of the promotion audit.

Independently recompute from the tracked compact files:

```text
completed fits:                      24 / 24
failed fits:                          0
diagnostics:                        897 / 897
maximum rank-normalized R-hat:        1.00490775707187
minimum bulk ESS:                  1116.97123864205
minimum tail ESS:                  1657.19298205554
numerical repairs:                    0
forecast repairs:                     0
full ignored chain bytes:     272,089,116
aggregate fit seconds:             4078.65
sampled peak RSS:              474,732 KiB
final PGID empty:                   TRUE
```

Check all six fixture/mode cells separately. Verify that fixed-rate lambda is
excluded from stochastic diagnostics and that root swaps are a sidecar rather
than mixing evidence. Inspect future-root summaries and confirm that no
response simulation is asserted.

Decide whether the bounded result is:

```text
ACCEPTED
ACCEPTED WITH NONBLOCKING CORRECTIONS
REJECTED WITH AN EXACT REACHABLE COUNTEREXAMPLE
```

Do not manufacture promotion-grade certainty from ignored artifacts. Equally,
do not reject a compact evidence design merely because intentionally ignored
fit objects are absent from Git.

## Audit the preliminary matched-simulation design

Read completely:

```text
docs/implementation_notes/rqr_dlm_main_simulation_preliminary_spec_20260724.md
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_20260724.R
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_methods_20260724.csv
application/tests/testthat/test-rqr-dlm-main-simulation-preliminary-config.R
main.tex
rqr-gibbs-supplement.tex
refs.bib
STYLE_PROFILE.md
AGENTS.md
```

Use primary statistical sources, including:

```text
Morris, White, and Crowther (2019): ADEMP and Monte Carlo error
Pouplin et al. (2024): original RQR target and comparisons
Goncalves, Migon, and Bastos (2020): dynamic quantile linear models
Gneiting and Raftery (2007): proper scoring and interval score
Romano, Patterson, and Candes (2019): conformalized quantile regression
Xu and Xie (2021): dynamic time-series conformal intervals
Barber et al. (2023): conformal prediction beyond exchangeability
```

Do not accept citations or design claims merely because they appear in
`refs.bib`. Check the original sources when a decision depends on them.

### 1. Aims and estimands

Decide whether the proposed aims isolate the intended messages:

- direct interval roots under asymmetry;
- no artificial advantage in Gaussian symmetric controls;
- dynamic versus fixed roots;
- component-specific evolution;
- fixed versus learned generalized-Bayes rate; and
- computation and failure behavior.

Check that population RQR roots, equal-tailed endpoints, empirical coverage,
width, endpoint error, and future-root summaries are kept distinct.

### 2. Oracle construction

Audit the location-scale argument:

```text
Y_t = mu_t + s_t Z_t,  s_t > 0

L_t,c = mu_t + s_t a_c
U_t,c = mu_t + s_t b_c
```

where `(a_c,b_c)` globally minimizes population RQR loss. Verify that the two
coverage and truncated-moment equations are necessary stationary conditions
but do not prove uniqueness or a global minimum. Decide whether the proposed
multiple-start objective verification, independent solver, and nonunique-root
fallback are adequate.

Check finite-second-moment requirements, centering and scaling, positivity of
`s_t`, path separation, and whether any mechanism inadvertently favors
RQR-DLM. Identify a concrete alternative DGP if a main claim is not
identifiable from the current set.

### 3. DGP matrix

Critique the six core and two sensitivity mechanisms. In particular, assess:

- the two Gaussian negative controls;
- centered standardized log-normal asymmetry;
- trend plus seasonal scale variation;
- unequal trend and regression evolution;
- the structural-break heavy-tail stress;
- the provisional state variances and break magnitude;
- `T=200`, `H=20`, horizons 1, 5, 10, and 20;
- 80% and 90% coverage levels; and
- reduced `T=100` and `T=400` sensitivities.

Recommend removals, additions, or parameter changes only when they sharpen an
identified scientific contrast or prevent a degenerate design.

### 4. Comparators and tuning

Audit:

- component-scale fixed-rate RQR-DLM;
- matched dynamic equal-tailed quantile intervals;
- frozen-discount RQR-DLM;
- fixed-design RQR;
- static quantile regression;
- rolling empirical intervals;
- noncompetitive true-`W` and population oracles;
- learned-rate and Gaussian-DLM sensitivities; and
- the optional time-series-valid conformal comparator.

Decide whether CRAN exdqlm 1.1.0 is the appropriate DQLM comparator and state
the exact archive/runtime validation required. Decide whether a conformal
method belongs in the core study, a sensitivity, or should be omitted. Do not
recommend ordinary iid split CQR for dependent data without an explicit
validity argument.

Critique the preliminary discount grid, training-only selection rule,
standardized fixed-rate rule, priors, and fairness across methods.

### 5. Performance measures

Audit the primary use of:

```text
held-out RQR loss
coverage and coverage error
width at comparable coverage
lower- and upper-root RMSE
```

and the secondary use of midpoint/width error, stratified coverage, central
interval score, failure rate, runtime, memory, and MCMC efficiency.

Decide whether the proposed width rule

```text
abs(coverage - nominal) <= 0.01 + 1.96 * MCSE(coverage)
```

is statistically defensible or should be replaced by equivalence testing,
coverage-standardized width, or a coverage-width frontier. Preserve the
warning that the central interval score targets equal-tailed endpoints and
that RQR has no response log-score contract.

### 6. Replications and Monte Carlo error

Audit:

```text
pilot:                  25 replications per core cell
confirmatory minimum:  500
batch size:            250
confirmatory maximum: 2500
coverage MCSE target: 0.01
stopping criterion:   Monte Carlo precision only
independent unit:     replication
within-replication horizons: clustered
```

Decide whether these targets are proportionate and computationally feasible.
Give explicit replacement formulas or numbers if they are not. Ensure no
stopping rule can depend on method rank, effect direction, or significance.

### 7. MCMC-in-simulation contract

Critique the proposed compromise:

```text
four-chain diagnostic pilot for every core cell
schedule frozen from diagnostics, not performance
one chain per confirmatory fit
preselected four-chain sentinels for 5% of replications
no reseeding, silent rescue, or outcome-driven extension
```

Decide whether this is adequate after the completed bounded validation.
If not, specify a computationally feasible alternative. Distinguish
between-chain convergence evidence from within-chain ESS checks.

### 8. Forecast interpretation

The evaluated interval uses posterior means of ordered future root functions.
Future root-state uncertainty is not converted into a response distribution.
Decide whether empirical held-out coverage and realized-oracle endpoint
recovery are meaningful under the proposed state mechanisms and how the paper
must phrase them. Provide an exact counterexample if the proposed forecast
estimand makes a core claim uninterpretable.

### 9. Reproducibility architecture

Review the proposed modes, seed mapping, atomic outputs, failure denominator,
runtime attestations, compact artifacts, process/thread limits, and
fail-closed pilot and confirmatory flags. Identify the minimum implementation
gates before a diagnostic pilot can run.

## Required decision

Return separate decisions:

```text
bounded RQR-DLM validation:
  ACCEPT / ACCEPT WITH NONBLOCKING CORRECTIONS / REJECT

preliminary simulation design:
  A = implement reference gates and diagnostic pilot
  B = revise specified design elements before implementation
  C = blocked by a statistical target or comparator defect

confirmatory main simulation:
  NO-GO at this review stage

CAVI/ELBO:
  DEFER

RQR-DESN:
  DEFER
```

List every proposed change as:

```text
required before implementation
required before diagnostic pilot
required before confirmatory execution
nonblocking
rejected or unnecessary
```

Do not broaden a correction beyond the evidence supporting it.

## Remote-only deliverables

Create a new branch from exactly
`e9db0bc8ba16d5e6ed76ef9378ca875b2ccf0769`:

```text
chatgpt-pro/output14-audit-20260724
```

Push exactly these four files:

```text
external_reviews/chatgpt_pro_output14_20260724/
  chatgpt_pro_output14_audit_20260724.md
  chatgpt_pro_output14_codex_handoff_20260724.md
  chatgpt_pro_output14_findings_20260724.csv
  chatgpt_pro_output14_artifact_hashes.csv
```

The artifact manifest must contain byte counts and SHA-256 values for the
other three deliverables. Do not merge the branch into `main`. Do not add
implementation changes.

When finished, reply to me only with:

```text
chatgpt-pro/output14-audit-20260724
<full 40-character commit SHA>
```

Codex will fetch, authenticate, and integrate the four files. I should not
manually copy, download, upload, or transcribe any deliverable or checksum.
