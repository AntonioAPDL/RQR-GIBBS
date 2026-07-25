# ChatGPT Pro Output-16 independent pre-authorization audit

**Review date:** 2026-07-25  
**Access path used:** attached exact archive  
**Input archive SHA-256:** `7fa1c754940e9001cee2b8069297e70f6615b3d74647da826504f9a237fdcbef`  
**Reviewed reconciliation branch tip:** `4b95064b01350a6b020146b189dc9d44936aed8f`  
**Reviewed main implementation commit:** `7b7c47204801032e5eb4fe6c9fd332aaaedead43`  
**Independently reconstructed `application/` Git tree:** `29f938a8359e0c8bf23c41584f91c0b1fd38e25b`  
**Package represented by the reviewed source:** `rqrgibbs 0.1.0.9017`

## 1. Scope, source authentication, and execution boundary

The supplied ZIP was hashed before extraction. Its SHA-256 exactly matched the declared value, and a complete ZIP integrity test passed. I rejected unsafe archive member patterns before extraction. The extracted snapshot contains the Output-16 review prompt and the source/evidence packet at the reconciliation tip named above.

Because a GitHub ZIP does not contain `.git`, I authenticated the scientific source through three independent bindings available in the packet:

1. the immutable reconciliation archive identity;
2. the compact runtime binding to implementation commit `7b7c472...`; and
3. a fresh reconstruction of the Git tree for the extracted `application/` directory, which produced `29f938a...`, exactly matching the tracked runtime evidence.

This establishes that the reconciliation snapshot preserves the reviewed implementation tree rather than silently substituting the later mode-only `main` state.

All six files listed by `external_reviews/chatgpt_pro_output15_20260724/chatgpt_pro_output15_artifact_hashes.csv` matched their recorded byte counts and SHA-256 values. Seven of the eight rows in `docs/audits/rqr_dlm_output15_reference_evidence_20260725/tracked_evidence_hashes.csv` also matched. The sole mismatch is the Output-16 prompt itself: the manifest records an earlier 13,661-byte prompt, whereas the reviewed branch contains the final 14,932-byte prompt. This is a reconciliation-documentation mismatch caused by a later prompt edit; it does not alter the implementation, design matrix, oracle evidence, runtime binding, or launch code. It should be refreshed, but it is not a scientific launch blocker.

The active review environment did not provide R or `Rscript`. I therefore did **not** independently rerun `R CMD check`, native R/C++ tests, TeX builds, preflight, or the oracle/reference executable. Their 22/22 and 15/15 results are accepted only as tracked execution evidence whose compact files and source contracts were audited. I independently recomputed the design budgets, the application tree, and the population RQR oracles described below.

## 2. Executive decision

| Review object | Verdict |
|---|---|
| Statistical target and interpretation | **PASS** |
| Output-15 design fidelity | **PASS** |
| Oracle/reference mathematics | **PASS** |
| RNG state allocation and pre-data sentinel selection | **PASS** |
| Canonical wave-plan ordering | **PASS** |
| Enforced sentinel/wave launch sequence | **NO-GO — blocker B1** |
| RQR-DLM and comparator contracts | **PASS** |
| Runtime lineage and flag-only authorization | **PASS** |
| Wave/process/resource implementation | **PARTIAL — blocked by B1** |
| Collection/failure contract | **PASS** |
| MCMC diagnostic scope | **NO-GO — blocker B2** |
| Precision-based replication stopping | **PASS** |
| Repository validation evidence | **ACCEPTED AS TRACKED, NOT RERUN** |

Exact launch answers:

```text
Create the one-line flag-only authorization commit: NO-GO
Rebuild and attest the runtime at that commit:       NO-GO at the current source
Launch the complete confirmatory study afterward:   NO-GO
Launch another disposable pilot first:              NO-GO
```

The next justified action is a narrow source correction for B1 and B2, followed by the existing preflight/reference and contract gates and one further independent source review. No standalone pilot is needed.

## 3. Statistical target and literature reconciliation

The implementation and manuscript preserve the necessary inferential distinction. The update is of the form

\[
\Pi(d\vartheta\mid y)\propto \Pi_0(d\vartheta)
\exp\{-\omega\mathcal L_c(\vartheta;y)\},
\qquad
\mathcal L_c=\sum_t\rho_c\{(y_t-\eta_{1t})(y_t-\eta_{2t})\}.
\]

This is the coherent loss-based update described by Bissiri, Holmes, and Walker, not a response likelihood. The normal--exponential representation borrowed algebraically from asymmetric-Laplace quantile regression augments the pseudo-residual product. Yu--Moyeed, Kozumi--Kobayashi, and the dynamic quantile linear-model paper use an asymmetric-Laplace **response** model; the reviewed RQR implementation explicitly does not transfer that interpretation.

The packet also keeps three different future objects separate:

1. conditional-mean RQR roots;
2. realized future dynamic roots; and
3. independently generated future responses used only to estimate repeated-sampling operating characteristics.

This separation is explicit in `rqr_confirm_generate_dgp()` and its returned schema (`application/scripts/lib/rqr_dlm_confirmatory_simulation.R`, lines 1290--1433). No response log score or CRPS is attributed to RQR. The standard interval score remains secondary because its canonical elicited interval is equal-tailed; this is consistent with Gneiting and Raftery's interval-score discussion. The same-data learned inverse loss scale is not called a response variance or an empirical-coverage calibration procedure. The calibrated-tolerance-interval paper uses a separate bootstrap/Robbins--Monro calibration scheme and therefore does not justify that stronger interpretation here.

The original RQR article supplies the residual-product loss and population coverage argument, but its disputed finite-sample theorem is not used as evidence for the confirmatory study.

**Verdict:** PASS.

## 4. Output-15 design fidelity and run budget

The imported incidence matrix has 208 rows: 16 scenario/coverage cells crossed with 13 method slots. The inclusion codes contain 80 `i` rows and 9 frozen `x` rows, giving 89 included rows; the remaining 119 rows are explicitly omitted with documented reasons. No hidden candidate or tuning row exists.

Independent summation of the incidence and budget files reproduced the required totals:

| Quantity | Initial | Central | Maximum |
|---|---:|---:|---:|
| Independent DGP replications | 1,800 | 3,600 | 5,400 |
| Software calls | 15,800 | 29,800 | 43,800 |
| Logical fits | 17,600 | 33,400 | 49,200 |
| Standard MCMC chains | 14,000 | 26,200 | 38,400 |
| Extra embedded sentinel chains | 882 | 1,710 | 2,538 |
| Total MCMC chains | 14,882 | 27,910 | 40,938 |

The fixed contract uses 20 future response subreplications, and candidate/tuning fits equal zero. The method/scenario omissions are explicit rather than generated from observed performance. The matrix therefore implements a confirmatory ADEMP comparison rather than a concealed tuning exercise.

The scientific comparisons are aligned to method targets:

- RQR methods are evaluated against population RQR roots;
- dynamic and static quantile methods are evaluated against their equal-tailed quantile roots;
- held-out RQR loss is a common interval-functional criterion;
- width claims require both methods to satisfy the declared coverage qualification and a Holm-adjusted directional comparison;
- interval score is secondary; and
- failures remain in the intention-to-run denominator.

**Verdict:** PASS.

## 5. DGP coherence, common random numbers, and sentinel allocation

### 5.1 DGPs

The six DGP families and their sensitivity variants are coherent with the stated questions.

- The seasonal state is a two-dimensional harmonic rotation with an independent two-dimensional Gaussian innovation at every time (`rqr_confirm_generate_training_state()`, lines 903--932; future analogue, lines 1056--1082). It is stochastic rather than a deterministic sinusoidal offset.
- The heteroscedastic mechanism uses a local level plus an AR(1) log-scale covariate and a standardized Student-t innovation (`rqr_confirm_error_draw()`, lines 819--838; state construction, lines 980--1003).
- The break stress combines a single frozen coefficient break with a standardized 90/10 normal/shifted-t mixture. The raw mean is 0.2 and the standardization variance is `1.6 - 0.2^2`, which is correct for the stated mixture.
- Root alignment evolves two ordered roots at the reference 80% RQR target, recovers the corresponding location and scale once, and then derives other-coverage RQR and quantile targets from the shared response law. It therefore tests root-prior alignment without pretending that RQR roots equal quantiles under asymmetry.
- Training state, training response, future state, and future response use distinct task keys. Conditional roots, realized roots, and generated responses are separately stored and checked not to collapse in dynamic cases.

### 5.2 RNG contract

`rqr_confirm_stream_states()` uses full seven-integer L'Ecuyer-CMRG states and `nextRNGStream()` (`rqr_dlm_confirmatory_simulation.R`, lines 330--356). Sentinel selections are generated from dedicated selection streams before data realization (`rqr_confirm_sentinel_map()`, lines 397--431). Future state and response parents are separated, and each of the 20 subreplications uses `nextRNGSubStream()` (`lines 638--675`). The complete ledger rejects duplicate task keys and duplicate full-state digests and verifies the canonical key set (`lines 657--815`).

The maximum wave plan itself is deterministic and orders sentinel rows before standard rows within a batch (`rqr_confirm_wave_plan()`, lines 3263--3350). Each authorized replication appears once in the canonical task set.

**Verdict for RNG allocation and canonical planning:** PASS.

## 6. RQR-DLM and comparator correctness

The declared dynamic RQR methods retain fixed-joint modes only:

- component-scale evolution with shared component scales;
- frozen component discounts;
- a common-evolution ablation;
- selected true fixed-`W` cells;
- fixed-rate sensitivities; and
- the normalized learned inverse-loss scale.

The adaptive conditional-discount recursion is excluded. The implementation continues to use alternating root-specific FFBS because the stacked prior is Gaussian but the augmented observation term is quartic jointly in the two roots.

For the frozen-discount method, the full `T+H` template is generated, and the first `T` slices must equal the independently generated training template before the last `H` slices are used for forecasting (`rqr_confirm_dynamic_fit()`, lines 2681--2710). Component-scale rows are aligned to the selected retained draw at the forecast boundary.

Comparator contracts are source-pinned and isolated:

- dynamic quantile endpoints use exdqlm 1.1.0 with reduced-AL DQLM and `dqlm.ind=TRUE`;
- static quantile regression uses quantreg 6.1 and `rq(..., method="br")` (`rqr_confirm_static_quantile()`, lines 2861--2912);
- raw lower and upper quantile forecasts are retained, and ordering is applied only for interval metrics.

The protected exdqlm checkout is not an execution source.

**Verdict:** PASS.

## 7. Independent population-oracle audit

For an unrestricted interior RQR interval \((a,b)\) under a mean-zero innovation law, the population first-order equations reduce to

\[
F(b)-F(a)=c,
\qquad
M(b)-M(a)=0,
\]

where \(M(x)=E\{Y\mathbf 1(Y\le x)\}\). Parameterizing \(a=F^{-1}(u)\), \(b=F^{-1}(u+c)\) gives a one-dimensional coverage profile. I independently solved this equation and directly integrated the RQR risk for all four reference families.

| Innovation | Coverage | Lower root | Upper root | Profile probability `u` | Direct risk |
|---|---:|---:|---:|---:|---:|
| Gaussian | 0.80 | -1.281551565545 | 1.281551565545 | 0.100000000000 | 0.449820324077 |
| Gaussian | 0.90 | -1.644853626951 | 1.644853626951 | 0.050000000000 | 0.339286064279 |
| Centered standardized lognormal (`logsd=.75`) | 0.80 | -0.739284016403 | 2.067306109465 | 0.159602571070 | 0.485995836023 |
| Centered standardized lognormal | 0.90 | -0.842696737148 | 3.147349091330 | 0.083494311394 | 0.385125461982 |
| Standardized Student-t(5) | 0.80 | -1.143214868406 | 1.143214868406 | 0.100000000000 | 0.543651900685 |
| Standardized Student-t(5) | 0.90 | -1.560849758344 | 1.560849758344 | 0.050000000000 | 0.465899970553 |
| Standardized 90/10 Gaussian + shifted-t(3) mixture | 0.80 | -1.083210543312 | 1.313235068608 | 0.114580163360 | 0.509966192033 |
| Standardized mixture | 0.90 | -1.382458268614 | 1.795147926377 | 0.059013720383 | 0.422437778217 |

Coverage residuals were at most approximately `1e-14`, and truncated-first-moment residuals were at numerical zero. I used analytic first partial moments for the Gaussian, lognormal, Student-t, and mixture components and direct split quadrature for the final risk. The Student-t scale `sqrt(3/5)` and mixture standardization agree with `rqr_confirm_oracle_spec()` (`rqr_dlm_confirmatory_simulation.R`, lines 819--872).

The source's blocking oracle contract—coverage and moment residuals, objective agreement, positive separation, curvature/uniqueness, and unrestricted/profile agreement—is mathematically appropriate. In a flat but uniquely minimized risk basin, two valid optimizers can have endpoint differences larger than machine epsilon while attaining indistinguishable objective values. Treating endpoint disagreement as a sidecar is justified **provided** the residual, objective, and uniqueness gates remain blocking, as they do here.

The extracted compact evidence reports 22/22 preflight and 15/15 reference gates, with the exact source/runtime/toolchain binding. These executable results were not rerun in this environment.

**Verdict:** PASS.

## 8. Runtime lineage and flag-only authorization

The version-5 runtime lineage represented in the packet compares the complete Git archive and built package file sets, requires one successful package build/install into the declared isolated library, records command and log receipts, binds source-package and runtime-tree digests, and verifies a runtime marker. The negative mixed-lineage tests are part of the tracked contract. Promotion requires an isolated runtime; `pkgload::load_all()` or a separately installed package cannot satisfy the promotion path.

The authorization function binds execution to:

- the reviewed implementation commit;
- an authorization commit whose only diff is the one Boolean flag;
- a clean primary worktree;
- the isolated runtime tree;
- preflight and reference manifests;
- seed and task-plan hashes;
- comparator source/runtime hashes;
- toolchain identity; and
- an explicit user confirmation.

`rqr_confirm_flag_only_authorization_diff()` verifies the exact changed file and exact removed/added lines (`rqr_dlm_confirmatory_simulation.R`, lines 4066--4108). Both direct and wave paths fail closed while the flag is false.

**Verdict:** PASS.

## 9. Blocker B1 — the canonical wave order is not enforced by the launcher

### Reachable counterexample

The canonical plan correctly places sentinel work before standard work. The launcher does not enforce that historical order.

`application/scripts/17_launch_rqr_dlm_confirmatory_wave.R`:

- recomputes and requires equality with the complete canonical plan (lines 48--58);
- selects whatever caller-supplied canonical `wave_id` is requested (lines 60--64); and
- immediately creates the output and launches its workers (lines 66--166).

It never requires evidence that earlier canonical waves completed, that the same-batch sentinel wave passed before a standard wave, or that a prior paired-batch decision authorized a later batch. The stop at lines 213--215 applies only after a worker failure **inside the currently requested wave**.

After a future flag-only authorization, the following source-reachable sequence bypasses the frozen sentinel contract:

```text
17_launch_rqr_dlm_confirmatory_wave.R \
  execute-confirmatory <complete-canonical-plan.csv> \
  <a-later-standard-wave-id> <fresh-output-root>
```

The requested standard wave is canonical, so the current checks accept it even when its sentinel wave was never run or failed.

### Consequence

The study's sentinel safeguards are part of the confirmatory design, not an optional pilot. An out-of-order standard wave can produce results before the convergence/infrastructure guard for that batch, complicate recovery, and create evidence that must later be invalidated manually. The implementation therefore does not yet guarantee the declared stop-after-failed-sentinel/batch semantics.

### Smallest justified correction

Implement one authorization-bound coordinator or append-only wave-state contract that exposes only the **next** canonical wave. At minimum:

1. A standard wave must verify a passing, recursively hashed sentinel manifest for the same batch target.
2. A later batch must verify the prior complete paired-batch decision authorizing continuation.
3. All earlier required wave IDs must have exact, nonduplicated completion records.
4. The run/source/runtime/config/seed/task-plan bindings must match across wave records.
5. Arbitrary skipping, replay, or out-of-order IDs must fail before the output root is created.

Add deterministic negative tests for standard-before-sentinel, sentinel failure followed by standard launch, skipped batch, replayed wave, and a prior-wave manifest from another authorization/runtime.

**Verdict:** launch blocker.

## 10. Process/resource and collection/failure contracts

Within one wave, worker limits and numerical-thread controls are explicit: at most eight sentinel workers, at most 32 standard workers, and one declared BLAS/OpenMP numerical thread per worker. The larger sampled `NLWP` envelopes are honestly labeled as OS helper-thread telemetry rather than numerical parallelism. The shell wrapper traps signals, drains process groups, uses atomic publication, and retains failures.

Collection requires exact planned task equality, recursive bytes and file counts, no symlinks, shared provenance/config/seed/runtime bindings, and failure retention in the denominator. It refuses analysis of an incomplete run. Atomic CSV/JSON and worker/wave manifests substantially close deletion and shard-substitution paths.

The only blocking wave/process defect identified is B1: the cross-wave ordering/history contract is absent. No independent process-monitor run was executed here.

**Verdict:** PASS within a wave; overall wave contract NO-GO until B1 is corrected.

## 11. Blocker B2 — convergence diagnostics omit time-local, terminal, and future root functionals

### Reachable counterexample

For dynamic RQR fits, `rqr_confirm_scalar_draws()` returns only:

```text
mean_lower
mean_upper
mean_midpoint
mean_width
observed_loss
log_lambda (learned mode)
log_q_j (component-scale mode)
```

(`rqr_dlm_confirmatory_simulation.R`, lines 3353--3388). `rqr_confirm_chain_diagnostics()` then intersects available columns and requires only the five aggregate root/loss variables (`lines 3454--3527`).

Construct two chains with compensating time-local deviations: one has a positive lower/upper displacement near the terminal time and an offsetting negative displacement earlier; another has the reverse pattern. Their time-averaged endpoints, average midpoint, average width, and even aggregate loss can agree while their terminal roots and forecast-origin states differ materially. All frozen diagnostics can pass, yet `rqr_forecast_roots()` propagates those different terminal states into different future interval roots. This directly affects reported horizon-specific coverage, endpoint recovery, and paired contrasts.

### Consequence

The confirmatory estimands include horizon-specific future lower, upper, midpoint, and coverage summaries. Mixing only in time averages does not establish mixing for the terminal-state functions that generate those future roots. This can change scientific conclusions even when aggregate training summaries look converged.

### Smallest justified correction

Replace the column intersection with a frozen, method-aware required diagnostic schema. For every sentinel dynamic fit include, at minimum:

1. terminal lower, upper, midpoint, and width;
2. future conditional-mean lower, upper, midpoint, and width at all reported horizons `1,5,10,20`;
3. predeclared time-specific training lower, upper, midpoint, and width, including the first/last time, structural-break boundary where applicable, and missing/scale-boundary times; using all training times for sentinel chains is computationally feasible and preferable;
4. `log(lambda)` for learned-scale fits; and
5. `log(q_j)` for component-scale fits.

The future diagnostic quantities must be deterministic functions of each retained terminal state and the declared future matrices, without process-noise or response simulation. Standard one-chain MCSE diagnostics should use the corresponding scalar schema. Missing required columns must stop rather than be removed by `Reduce(intersect, ...)`.

Add two negative tests:

- an omitted required terminal/future column; and
- the compensating-path example in which aggregate summaries agree but terminal/future functions disagree.

**Verdict:** launch blocker.

## 12. Replication MCSE, paired contrasts, and stopping

At the replication level, `rqr_confirm_mcse()` uses the standard error of the independent replication mean, `sd/sqrt(n)` (`lines 3529--3536`). Paired method contrasts merge on replication and use common-random-number differences. Precision continuation is based on MCSE thresholds, complete paired batches, and failure-free task accounting (`rqr_confirm_batch_decisions()`, lines 3893--4063). Performance signs do not drive continuation, and empirical-coverage TOST is explicitly descriptive/non-stopping.

Width claims are allowed only when both methods pass the coverage qualification and the Holm-adjusted directional comparison. This is conservative and target-aligned.

A nonblocking reproducibility refinement is to call `rqr_forecast_roots(..., nd=NULL)` when every retained draw is desired. The current `nd=retain` path may permute all retained rows, although terminal-state and component-scale rows remain aligned and distributional summaries are unchanged.

**Verdict:** PASS, conditional on B2's expanded MCMC diagnostic schema.

## 13. Evidence disposition and nonblocking corrections

### Accepted evidence

The following are accepted as tracked compact evidence, not independently rerun:

```text
preflight:                  22/22
oracle/reference:           15/15
R CMD check:                Status OK
native/contract tests:      PASS
article/supplement builds:  PASS
direct fail-closed:         PASS
wave fail-closed:           PASS
confirmatory fits:          0
```

The source commit, reconstructed application tree, package version, runtime tree, attestation digest, and protected reference identities are internally consistent.

### Nonblocking corrections

1. Refresh `tracked_evidence_hashes.csv` for the final Output-16 prompt bytes/hash.
2. Prefer `nd=NULL` where forecast draw identity rather than a random permutation is desired.
3. Retain endpoint-difference sidecars for oracle diagnostics, but do not promote them to blockers unless objective/residual/uniqueness checks fail.

## 14. Final launch interpretation

The current study is **not** authorized. After B1 and B2 are corrected and the existing gates pass under a new reviewed implementation commit, the intended one-line authorization path remains defensible. No disposable pilot should be added.

Any eventual confirmatory evidence is bounded to:

- the 16 frozen scenario/coverage cells represented by the six declared DGP mechanisms and their sample-size/coverage sensitivities;
- the 89 included method incidences and their frozen priors, discounts, learning-rate modes, MCMC schedules, comparator versions, and batch rules;
- repeated-sampling coverage, width, RQR loss, method-target endpoint recovery, failures, and computation under those mechanisms; and
- interval-root forecasting, not posterior-predictive response-density inference.

It cannot establish universal superiority, finite-sample coverage guarantees, response-predictive validity, or automatic calibration of the learned loss scale.
