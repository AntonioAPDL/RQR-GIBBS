# ChatGPT Pro Output-15 independent main-study design review

**Review date:** 2026-07-24  
**Repository:** `AntonioAPDL/RQR-GIBBS`  
**Implementation/reference source:** `6ba47d1d686e7f47d90bf3110fbbe77f8da96fee`  
**Compact evidence/reconciliation source:** `a5a08811912d7175bbbcec98e8f8af254fd51f51`  
**Prompt-only direction commit inspected separately:** `da7e2125f9bed8041a0f9e4b05f2ac17cf9c07fd`  
**Output-14 review:** `2be17bd5710e62168970577796c8ddc1872ffde6`  
**Protected references:** exdqlm `dffb71ee70b597d6a716ee74be1cbc99731cd453`; Q-DESN `f9f22804eff3871bb5350c8add04b7c9f4d4957b`

## 1. Executive decision

```text
accepted bounded 24-fit RQR-DLM evidence:
  PASS; do not rerun

bounded evidence promoter P1-P3:
  PASS

current schema-0.2 main-study design:
  B+

recommended optimized design:
  A- after the launch-blocking corrections below

main-study runner implementation:
  GO TO IMPLEMENT WITH EXACT CORRECTIONS

confirmatory execution:
  CONDITIONAL GO only after the implemented runner, exact incidence matrix,
  oracle/comparator references, seed ledger, and launch bundle receive another
  independent review and a separate authorization commit

separate disposable performance pilot:
  NOT REQUIRED

CAVI/ELBO:
  DEFER

RQR-DESN:
  DEFER
```

The bounded validation is credible evidence that the declared fixed-joint RQR-DLM samplers can execute, mix, continue, forecast root states, and preserve numerical and provenance contracts on the three bounded structures. It does not establish repeated-sampling coverage, endpoint recovery, comparative forecasting performance, or learning-rate calibration.

A separate small performance pilot is not recommended. It would have poor Monte Carlo precision and would create an opportunity for outcome-driven redesign. The final design instead embeds preselected four-chain sentinel replications inside each replication batch. Sentinel observations remain part of the final study. They may stop a declared cell for a predeclared MCMC or infrastructure failure, but may not change a DGP, method, schedule, threshold, or scientific claim.

The confirmatory design is feasible only after several concrete implementation defects are fixed. The most consequential are: a scenario/code mismatch in the trend-seasonal and independent-root mechanisms; failure to separate future-state streams and future-root estimands; a collision-prone 28-bit seed mapping; lack of actual fitted comparator references; and the absence of the exact incidence/budget/runner contract delivered with this review.

## 2. Evidence and source audit

### 2.1 Bounded evidence and promoter

The accepted bounded packet remains tied to authorization source `afc9c5fed14c66317b684fc9b9f6d01079c307cd`; the normal branch is fail closed after `82cb02dc96e3642864d2bc187640ee8fc50678bd`. The compact packet records 24 completed fits, 897 passing maintained diagnostic rows, maximum rank-normalized R-hat 1.0049077571, minimum bulk/tail ESS 1116.971/1657.193, zero numerical or forecast repairs, and 24 provenance-eligible exact-target fits.

At implementation commit `6ba47d1...`, `application/scripts/11_promote_rqr_dlm_bounded_evidence.R` and `application/scripts/lib/rqr_dlm_evidence_promotion.R` close the three Output-14 promoter findings:

1. an external expected bundle freezes source, application tree, config, reference, runtime, attestation, and toolchain identities;
2. exact unique fit-ID set equality is required across all fit-level compact tables; and
3. reopened fits have checkpoint and continuation-history digests recomputed, the continuation validator invoked, and the whole-object digest compared.

The associated tests reject altered bundles, missing bundle fields, omitted/duplicated/extra fit IDs, semantic checkpoint/history mutations, and validator failure. **Disposition: pass.**

### 2.2 Reference stages

The exact-runtime reference packet at `a5a0881...` records four nonexecution stages using one isolated primary runtime:

```text
preflight
oracle-reference
tiny-end-to-end
diagnostic-pilot-preflight
```

Every manifest keeps pilot and confirmatory execution false. The oracle packet has eight family-by-coverage certificates. The tiny fixture is byte reproducible and uses no repairs. The comparator preflight pins CRAN exdqlm 1.1.0 and quantreg 6.1 and verifies orientation and runtime identity. These stages are planning/reference evidence, not simulation results.

## 3. Scientific target and legitimate claims

The study concerns a pair of interval-root functions under an RQR generalized-Bayes loss update. It must preserve all of the following:

- RQR roots are not equal-tailed quantiles under asymmetric laws.
- The pseudo-asymmetric-Laplace representation augments an exponentiated loss kernel; it is not a response likelihood.
- Root-state draws are not posterior-predictive response draws.
- The normalized learned `lambda` is an inverse loss scale under a declared same-data generalized target; it is not coverage calibration or response variance.
- The adaptive conditional-discount recursion is excluded from exact-method rankings.
- Held-out RQR loss is target aligned for RQR and therefore a home-target measure, not a target-neutral universal score.
- The central interval score is a secondary proper score for the central/equal-tailed interval functional.
- Quantile methods must be evaluated against quantile endpoint truth for target-aligned endpoint recovery. Their distance to RQR roots may be shown only as an explicitly labeled cross-target distance.

The final design can legitimately support three principal scientific messages:

1. **Dynamic-root value:** whether RQR-DLM improves RQR-root tracking and held-out interval operating characteristics relative to fixed-design RQR when root functions evolve.
2. **Target contrast under asymmetry:** how direct RQR intervals and equal-tailed quantile intervals differ in RQR loss, repeated-sampling coverage, width, and their own target-aligned endpoint recovery, with Gaussian negative controls that should show no artificial asymmetry advantage.
3. **Evolution structure:** whether component-specific root evolution improves over common evolution and frozen discounts when state components genuinely move at different rates.

It cannot support a global superiority statement outside the declared mechanisms.

## 4. Launch-blocking implementation findings

### 4.1 Trend-seasonal state mismatch

`rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv` declares `q_seasonal=0.002`, but `rqr_main_generate_dgp()` uses a deterministic sine wave and has no seasonal state innovation. Implement a two-state harmonic block,

\[
\gamma_t =
R(2\pi/12)\gamma_{t-1}+e_t,\qquad
e_t\sim N(0,0.002 I_2),
\]

with the same state stream, initial law, scale path, and future-transition law in the Gaussian and log-normal matched pair. Set the location contribution to the first harmonic coordinate. The deterministic scale modulation and its 0.35 floor may remain.

### 4.2 Independent-root contract mismatch

The scenario table states initial roots `(-1.5,1.5)` and innovation variances `0.01`; the constructor uses `(-2,2)` and `0.001`. Freeze the safer implemented values:

```text
L0 = -2
U0 =  2
q_lower = q_upper = 0.001
minimum separation = 0.10
reference coverage = 0.80
```

Generate one response law from the reference roots and derive all other coverage roots from that same `(mu_t,s_t)` path.

### 4.3 Future-state and response streams

The constructor calculates a forecast seed but generates the future state path under the training-state seed. Replace the current scalar seed mapping with a complete deterministic task ledger:

1. enumerate every task key before authorization;
2. sort keys canonically;
3. initialize `RNGkind("L'Ecuyer-CMRG")` from master seed `2026072401`;
4. assign one complete `.Random.seed` state per task using `parallel::nextRNGStream`;
5. use `nextRNGSubStream` for the 20 future subreplications; and
6. require unique SHA-256 digests of all full RNG states.

Keys must distinguish training states, training errors, future state subreplication, future response errors, methods, initialization, forecast, oracle, and sentinel selection. The existing seven-hex-digit mapping is only 28 bits and has a material collision risk at the planned task count.

### 4.4 Conditional-mean versus realized future roots

The current DGP object assigns the same array to `realized_root_path` and `oracle_conditional_mean_root`. These are different estimands. For every dynamic DGP, store:

```text
oracle_conditional_mean_root:
  E(root_{T+h} | training state, DGP parameters)

realized_root_path[s,h]:
  root after future state innovations in future subreplication s
```

Use conditional-mean roots for forecast-estimand bias/RMSE, realized roots for realized-state forecast error, and generated future responses for empirical interval coverage.

### 4.5 Comparator references

The isolated source pins are appropriate, but the preflight currently validates interfaces and synthetic orientation more strongly than actual fits. Before runner acceptance:

- fit and forecast a real tiny lower/upper AL-DQLM pair from the exact exdqlm 1.1.0 runtime;
- fit a real tiny quantreg 6.1 pair and verify raw endpoint orientation and ordered interval output;
- freeze every exdqlm argument, including `dqlm.ind=TRUE`, `init.from.vb=FALSE`, `sig.init=1` after standardization, `fix.sigma=FALSE`, `PriorSigma` equal to the attested package default, state priors, structural discounts, burn-in, retained draws, and forecast indexing;
- record the serialized formals/defaults digest;
- freeze the complete dependency-library manifest and loaded namespace paths, not only the top-level package source hash.

The protected exdqlm checkout remains read only and is never an execution source.

## 5. Optimized frozen ADEMP design

| ADEMP element | Frozen recommendation |
|---|---|
| **Aims — primary** | Dynamic RQR-root recovery/forecasting; RQR versus equal-tailed interval operating characteristics; component-specific versus common/frozen evolution; dynamic versus fixed-design RQR. |
| **Aims — sensitivity** | Fixed rates 0.5/2, normalized learned rate, T=100/400, known heteroscedastic scale, and root-prior alignment. |
| **DGPs — core** | Static Gaussian; local-level Gaussian; local-level log-normal; trend-seasonal Gaussian; trend-seasonal log-normal; unequal trend-regression log-normal; composite break/heavy-tail stress. |
| **DGPs — sensitivity** | Known-scale heteroscedastic t5; independent-root alignment; local-level log-normal at T=100 and T=400. |
| **Coverage** | 0.80 and 0.90 for the main negative controls/signals; 0.80 only for targeted mechanisms and sensitivities specified in the incidence matrix. |
| **Forecast** | H=20, reported horizons 1,5,10,20; 20 future state/response subreplications per training data set. |
| **Estimands — RQR** | Population RQR roots, conditional-mean future roots, realized future roots, midpoint/width, held-out RQR loss, repeated-sampling coverage. |
| **Estimands — quantile** | Population equal-tailed endpoints and raw lower/upper quantile fits; ordered pair only for interval metrics. |
| **Common operating measures** | Coverage, width, held-out RQR loss, secondary central interval score, failures, elapsed time, memory. |
| **Methods — core** | Component-scale fixed-rate RQR-DLM; paired dynamic AL-DQLM quantiles; rolling empirical interval. |
| **Ablations** | Fixed-design RQR, static quantile regression, frozen discount RQR-DLM, common-evolution RQR-DLM. |
| **Noncompetitive oracle** | True-W RQR-DLM in three selected c=0.80 cells. |
| **Sensitivities** | Fixed rates 0.5 and 2 and learned normalized rate in two representative asymmetric c=0.80 cells. |
| **Omitted** | Gaussian response DLM and conformal interval from this first confirmatory study; adaptive discount, RQR-DESN, and CAVI/ELBO. |
| **Tuning** | No data-driven candidate search. Structural discounts, priors, rates, and empirical window are frozen before confirmatory seeds. |
| **Performance — primary** | Paired held-out RQR loss; empirical coverage; coverage-qualified paired width; target-aligned endpoint RMSE; failure probability. |
| **Performance — secondary** | Midpoint/width RMSE, central interval score, horizon summaries, cross-target distances, computation and ESS/second. |

The exact 208-row incidence matrix is in `chatgpt_pro_output15_final_design_matrix_20260724.csv`. It has 89 included rows and 119 explicit omissions.

### Frozen structural settings

```text
component-scale RQR:
  q_j ~ IG(shape=2.5, rate=0.025)
  identity templates by component
  fixed standardized rate = 1

common-evolution RQR:
  one shared q ~ IG(2.5, 0.025)

fixed-design RQR:
  ridge variance = 25
  fixed standardized rate = 1

rolling empirical interval:
  trailing window = 100

frozen RQR and dynamic AL-DQLM discounts:
  local level: 0.95
  trend-seasonal: trend 0.98, seasonal 0.95
  trend-regression: trend 0.98, regression 0.90
  break/regression stress: level 0.98, regression 0.90
```

There are **zero** confirmatory training-validation candidate fits. Rates 0.5, 1, 2 and learned normalized lambda are separate predeclared methods, never selected using test coverage.

## 6. Exact run size

### Replications and future observations

| Planning point | Independent DGP replications | Future paths | Future responses |
|---|---:|---:|---:|
| Initial | 1,800 | 36,000 | 720,000 |
| Central | 3,600 | 72,000 | 1,440,000 |
| Maximum | 5,400 | 108,000 | 2,160,000 |

Core DGPs start at 200 replications, add batches of 100, and stop at 600. Sensitivity/T-variant DGPs start at 100, add 50, and stop at 300. The batch decision is made at the DGP level so that both coverages and all precision-extended methods remain paired. True-W and learning-rate sensitivities are fixed to the first 200 replications and do not control precision stopping.

### Fits and chains

| Count | Initial | Central | Maximum |
|---|---:|---:|---:|
| Method interval evaluations | 15,800 | 29,800 | 43,800 |
| Logical endpoint/model fits | 17,600 | 33,400 | 49,200 |
| Software fit calls | 15,800 | 29,800 | 43,800 |
| Lower quantile fits | 4,600 | 9,200 | 13,800 |
| Upper quantile fits | 4,600 | 9,200 | 13,800 |
| Standard MCMC chains | 14,000 | 26,200 | 38,400 |
| Extra sentinel chains | 882 | 1,710 | 2,538 |
| Total MCMC chains | 14,882 | 27,910 | 40,938 |
| Training-validation candidate fits | 0 | 0 | 0 |
| Forecast interval vectors | 15,800 | 29,800 | 43,800 |
| Future-path interval comparisons | 316,000 | 596,000 | 876,000 |
| Scalar horizon coverage evaluations | 6,320,000 | 11,920,000 | 17,520,000 |

A dynamic quantile interval requires two separate MCMC fits, one per endpoint. Static quantreg uses one software call with two tau values but counts as two logical endpoint fits.

## 7. Monte Carlo precision and stopping

For replication `r`, future subreplication `s=1,...,20`, and horizon `h=1,...,20`, define the aggregate coverage contribution

\[
C_r=(400)^{-1}\sum_{s=1}^{20}\sum_{h=1}^{20}
1\{Y_{rsh}\in I_{rh}\}.
\]

The replication is the independent Monte Carlo unit. Estimate

\[
\operatorname{MCSE}(\bar C)=s(C_1,\ldots,C_R)/\sqrt R.
\]

Horizon-specific coverage uses `C_{r,h}=20^{-1} sum_s I_{rsh}` and its across-replication standard error. Means, failure probabilities, and paired contrasts use the analogous replication-level empirical standard deviation divided by `sqrt(R)`. Horizons are never treated as independent Monte Carlo replicates.

### Precision-only batch gates

Core DGPs may stop after the minimum 200 only when every predeclared primary precision-extended summary satisfies:

```text
aggregate coverage MCSE <= 0.010
coverage MCSE at h=1,5,10,20 <= 0.020
endpoint/midpoint mean and paired-contrast MCSE <= 0.02 training-response SD
width mean and paired-contrast MCSE <= 0.02 mean oracle RQR width
standardized held-out RQR-loss paired-contrast MCSE <= 0.010
```

Sensitivity DGP thresholds are 0.015 for aggregate coverage and 0.030 by horizon; other normalized thresholds are unchanged. When some summaries fail, the entire DGP receives the next paired batch. At the maximum, stop and report unmet precision rather than altering the design.

TOST is **not** a stopping rule. Coverage qualification uses a 90% `t` interval for coverage error wholly inside `[-0.02,0.02]`. It supports practical coverage equivalence within the declared margin, not exact nominal coverage. A narrower-width claim is permitted only when both methods qualify and the 95% paired width-difference interval supports the direction.

The nominal counts 1,083/609 concern TOST feasibility at exact nominal coverage; 1,600/900 concern an absolute coverage MCSE of about 0.01. They answer different questions. This design uses adaptive precision batches rather than treating either pair as a universal fixed sample size.

## 8. Frozen MCMC and initialization schedule

### Standard fits

```text
component-scale, frozen, common, true-W, fixed-rate RQR-DLM:
  burn-in 1,000
  retained 2,000
  thin 1
  one chain per standard fit

learned normalized-rate RQR-DLM:
  burn-in 1,500
  retained 3,000
  thin 1

dynamic AL-DQLM quantile endpoint:
  burn-in 1,000
  retained 2,000
  thin 1

fixed-design RQR:
  burn-in 500
  retained 1,500
  thin 1

quantreg and empirical intervals:
  no MCMC
```

No chain is extended, restarted, or reseeded after observing diagnostics or performance.

### Initialization

A standard fit uses the empirical central interval as the initial root pair, training median midpoint, lambda 1, and prior-median evolution scales. Sentinel replications use four frozen overdispersed profiles:

```text
A: empirical midpoint; 1.00x half-width; lambda/q multiplier 0.5
B: midpoint -0.50 training SD; 0.75x half-width; multiplier 1
C: midpoint +0.50 training SD; 1.25x half-width; multiplier 2
D: training-median midpoint; 1.75x half-width; multiplier 4
```

Apply analogous state shifts and sigma multipliers to the dynamic quantile endpoint fits. A label swap is not an overdispersed profile.

### Embedded sentinels

For every replication batch, select two sentinel replications per scenario×coverage×MCMC method using the frozen RNG ledger. Each sentinel model gets four chains total. For dynamic quantiles, both endpoint models get four chains. Capped true-W/rate sensitivities receive two sentinels total.

This yields 882, 1,710, and 2,538 extra chains at the initial, central, and maximum designs. Sentinel selection precedes data generation. Passing sentinel observations remain in the analysis.

Sentinel gates:

```text
rank-normalized R-hat <= 1.01
bulk ESS >= 400
tail ESS >= 400
zero numerical repairs
exact runtime/provenance
finite predeclared scalar diagnostics
```

Single-chain gates are bulk ESS >=200, tail ESS >=100, and MCSE/SD <=0.08 on the frozen scalar set. A sentinel failure stops the declared scenario-coverage cell before later batches. A source, runtime, reference, or systemic numerical failure stops the entire run.

## 9. Ordered execution plan

1. **Exact-runtime preflight.** Verify source, config, incidence, oracle, comparator, dependency, seed-ledger, free-space, and artifact schemas.
2. **Reference gates.** Run oracle certificates, location-scale checks, actual tiny comparator fits/forecasts, two-replication byte reproduction, and rollback/monitor faults.
3. **Embedded sentinels for batch 1.** Execute the two preselected sentinel replications first in deterministic scenario/coverage/method order. These are final-study observations.
4. **Core batch 1.** Run remaining core replications in this DGP order: static Gaussian, local-level Gaussian, local-level skewed, trend-seasonal Gaussian, trend-seasonal skewed, unequal evolution, composite stress.
5. **Cell diagnostics and integrity.** Stop a failing declared cell before any later batch. No retuning follows sentinel inspection.
6. **Sensitivity batch 1.** Run known heteroscedastic scale, root alignment, T=100, and T=400 cells.
7. **Precision-only additions.** Add complete paired DGP batches in the same order until all primary precision rules pass or the cap is reached.
8. **Closeout.** Collect exact denominators, paired summaries, failures, resource records, and hashes.
9. **Promotion.** Reopen/verify compact and sentinel artifacts, recreate all analysis summaries, revoke the authorization flag, and promote only compact evidence.

Any source or design correction invalidates the incomplete run and requires a new full run from a new authorization commit. Completed data from an old source are not spliced into the new run.

## 10. Resource and failure envelope

Use one BLAS/OpenMP thread per process. Start the embedded sentinel wave with 8 workers. If its measured peak remains at or below 0.75 GiB per worker and no infrastructure gate fails, use exactly 32 workers for the remainder. Do not increase to 48 during the run.

Planning envelope:

| Case | CPU hours | Workers | Expected wall time |
|---|---:|---:|---:|
| Initial optimistic | 947 | 48-equivalent benchmark | about 24 h |
| Central recommended | 2,663 | 32 | about 96 h |
| Maximum conservative | 6,768 | 16-equivalent conservative model | about 487 h |

The operational plan uses 32 workers centrally and a 14-day hard global ceiling. Each worker has a 1.5 GiB hard memory ceiling. Require at least 50 GiB free before launch and before every batch; 100 GiB is recommended.

Expected storage:

```text
compact local:                0.3 / 0.7 / 1.5 GiB
ignored sentinel diagnostics: 5 / 12 / 30 GiB
tracked promoted packet:      <=250 MiB
```

Standard fit objects are discarded after extracting and validating compact sufficient outputs. Sentinel chain/checkpoint objects remain ignored and hashed.

Every replication publishes atomically:

```text
replication_results.csv
replication_manifest.json
replication_artifact_hashes.csv
```

Checkpoint MCMC every 500 iterations and at burn-in completion. A method fit failure is retained in the intention-to-run denominator and is never retried. A replication-level infrastructure failure invalidates that replication for all methods and triggers the predeclared infrastructure action. Source/runtime/reference corruption, seed collision, artifact mismatch, or systemic numerical repair stops the global run.

## 11. Analysis and reporting

### Target alignment

- RQR endpoint RMSE uses population RQR roots.
- Quantile endpoint RMSE uses population equal-tailed endpoints.
- Distance of a quantile method to RQR roots is `cross_target_distance`, never quantile estimator bias.
- Conditional-mean root error and realized-future-root error are separate.
- Empirical coverage uses generated future responses and posterior-mean ordered root intervals; it is a repeated-sampling operating characteristic, not posterior-predictive coverage.

### Paired contrasts

Use common data and paired replication-level differences. Primary contrasts are:

1. component-scale RQR versus dynamic quantile interval;
2. component-scale RQR versus empirical interval;
3. component-scale versus fixed-design RQR;
4. component-scale versus frozen discount where included;
5. component-specific versus common evolution in the unequal-evolution cell.

True-W and rate methods are noncompetitive/sensitivity contrasts.

### Multiplicity and hierarchy

Do not report an omnibus winner. For directional claims, use Holm-adjusted 95% paired intervals within each predeclared DGP×coverage contrast family. Coverage qualification is an eligibility classifier, not a significance result. Endpoint target-aligned errors and cross-target distances are reported with Monte Carlo intervals but are not pooled into a cross-target ranking.

### Figures

1. Coverage-width frontier by DGP, coverage, and method.
2. Paired held-out RQR-loss differences with Monte Carlo intervals.
3. Target-aligned endpoint RMSE panels; separate RQR and quantile targets.
4. Conditional-mean versus realized future-root error by horizon.
5. Component-specific versus common/frozen evolution contrasts.
6. Failure probability and computation/ESS-per-second panels.
7. Learning-rate sensitivity on the two predeclared mechanisms.

### Tables

1. ADEMP and exact incidence summary.
2. DGP/oracle certificate table.
3. Coverage qualification and paired width table.
4. Paired RQR-loss table.
5. Target-aligned endpoint recovery table.
6. Failure/resource/diagnostic table.
7. Precision and replication stopping table.

## 12. Exact size and feasibility

The machine-readable budget is in `chatgpt_pro_output15_run_budget_20260724.csv`.

Central planning requires 3,600 independent DGP replications, 29,800 interval evaluations/software calls, 33,400 logical model/endpoint fits, 26,200 standard MCMC chains, 1,710 extra sentinel chains, and about 2,663 CPU hours. At 32 workers with overhead, the central estimate is about four days. The conservative maximum is roughly 6,768 CPU hours and about 20 days at 16-worker-equivalent conservative throughput.

This is large but feasible on Jerez if the runner is replication-parallel, one-threaded, fail closed, and compact-output only. The optimized incidence matrix removes scientifically redundant method×DGP×coverage cells and all data-driven tuning fits.

## 13. Grades and final decisions

```text
current schema-0.2 design:
  B+
  strong conceptual revision, but code/contract mismatches and no exact
  incidence, collision-free seed ledger, complete comparator references, or
  final computational envelope

recommended optimized design:
  A-
  focused scientific contrasts, explicit omissions, precision-only stopping,
  exact run counts, embedded safeguards, and feasible central budget

runner implementation:
  GO TO IMPLEMENT WITH THE EXACT CORRECTIONS IN THIS PACKET

confirmatory execution:
  CONDITIONAL GO AFTER IMPLEMENTATION, REFERENCE PASS, ANOTHER INDEPENDENT
  REVIEW, A SEPARATE FALSE-TO-TRUE AUTHORIZATION COMMIT, AND USER CONFIRMATION

matched/production claims before that:
  NO-GO

CAVI/ELBO and RQR-DESN:
  DEFER
```

## 14. Literature-grounded choices

- Morris, White, and Crowther (2019), DOI `10.1002/sim.8086`: ADEMP, replication-level Monte Carlo error, transparent failures, and seed discipline.
- Bissiri, Holmes, and Walker (2016): the generalized posterior is defined by a loss update, not by silently inventing a response likelihood.
- Yu and Moyeed (2001) and Kozumi and Kobayashi (2011): AL augmentation informs computation for quantile loss, but does not convert the RQR pseudo-residual into a response model.
- Gonçalves, Migon, and Bastos (2020): matched dynamic-quantile state components and FFBS conventions.
- Gneiting and Raftery (2007), DOI `10.1198/016214506000001437`: the central interval score is proper for its equal-tailed functional and remains secondary for the RQR target.
- Schuirmann (1987), DOI `10.1007/BF01068419`: two one-sided tests motivate practical-equivalence classification.
- Vehtari et al. (2021), DOI `10.1214/20-BA1221`: rank-normalized R-hat and bulk/tail ESS for multichain sentinel diagnostics.
- Pouplin et al. (2024), PMLR 235: the residual-product RQR target and direct interval motivation.

## 15. Required next repository action

Codex should implement the blockers and the complete fail-closed main runner, but keep diagnostic-pilot and confirmatory authorization false. The implementation pass must produce:

- the exact incidence-matrix hash and run-budget hash in the config;
- corrected DGP and seed contracts;
- complete oracle and comparator references;
- deterministic fit/chain/sentinel ledgers;
- hardened atomic replication outputs and fault tests;
- process/resource preflight;
- a fail-closed negative execute test; and
- a new independent review prompt.

No confirmatory fit may be executed in the implementation pass.
