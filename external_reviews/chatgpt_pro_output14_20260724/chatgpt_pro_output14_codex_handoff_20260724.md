# Copy-paste-ready Codex handoff after ChatGPT Pro Output-14

Independently authenticate this handoff and the accompanying Output-14 audit
against the exact GitHub branch before implementing anything. Modify only
`AntonioAPDL/RQR-GIBBS`. Do not modify, load, compile, or install from the
protected exdqlm or Q-DESN source checkouts.

## Reviewed states

```text
review base:
  e9db0bc8ba16d5e6ed76ef9378ca875b2ccf0769

bounded execution source:
  afc9c5fed14c66317b684fc9b9f6d01079c307cd

post-run revocation:
  82cb02dc96e3642864d2bc187640ee8fc50678bd

accepted implementation:
  da4d265af6d8c6d6f9be06bfe2a91bfae88501d8

Output-13 review:
  d13ec6f3b9761349d7283b17f47724c0f42532cf
```

Protected references remain:

```text
exdqlm:
  dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article:
  f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

## Pro decisions

```text
bounded RQR-DLM validation:
  ACCEPTED WITH NONBLOCKING CORRECTIONS

preliminary simulation design:
  B — revise specified design elements before implementation

confirmatory main simulation:
  NO-GO

CAVI/ELBO:
  DEFER

RQR-DESN:
  DEFER
```

Do not run a diagnostic pilot or confirmatory simulation in the same pass as
the design correction. Keep all pilot and confirmatory authorization flags
false.

## 1. Preserve the bounded result

Do not rerun the completed 24-fit bounded grid. Its compact result is accepted:

```text
24 / 24 fits completed
897 / 897 diagnostics passed
maximum R-hat 1.00490775707187
minimum bulk ESS 1116.97123864205
minimum tail ESS 1657.19298205554
zero numerical repairs
zero forecast repairs
272,089,116 ignored chain bytes
4,078.65 aggregate fit seconds
474,732 KiB sampled peak RSS
final PGID empty
```

Retain the interpretation:

```text
generalized-Bayes interval-root validation
not response-likelihood validation
not posterior-predictive response validation
not empirical coverage calibration
not comparative simulation evidence
```

### Nonblocking promoter hardening

Patch `application/scripts/11_promote_rqr_dlm_bounded_evidence.R` before it is
reused:

1. accept an externally frozen expected-bundle file and require exact equality
   for primary commit, application tree, config digest, reference-bundle
   digest, runtime tree, attestation, and toolchain;
2. after reopening each fit, recompute the checkpoint-state digest, recompute
   the continuation-history digest, and invoke the continuation-history
   validator; and
3. derive the expected 24 fit IDs from `fit_plan.csv` and require exact unique
   set equality across fit audit, run status, checkpoints, local hashes,
   missing/future checks, and provenance.

Add deterministic negative tests for each omission. These changes do not
require another bounded run.

## 2. Version and revise the preliminary simulation design

Advance the preliminary simulation schema from `0.1.0` to a new draft version,
for example:

```text
rqrgibbs_dlm_main_simulation_preliminary/0.2.0
```

Update together:

```text
docs/implementation_notes/rqr_dlm_main_simulation_preliminary_spec_20260724.md
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_20260724.R
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv
application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_methods_20260724.csv
application/tests/testthat/test-rqr-dlm-main-simulation-preliminary-config.R
```

Do not write a runner until all required-before-implementation items below are
machine readable and tested.

## 3. Oracle contract

Retain the location-scale construction, but add an independent global
certificate.

For `Y_t = mu_t + s_t Z_t`, require `s_t > 0` and verify:

```text
L_t,c = mu_t + s_t a_c
U_t,c = mu_t + s_t b_c
risk_Y(L,U) = s_t^2 risk_Z(a,b)
```

For every standardized error law:

1. solve the unrestricted two-dimensional population RQR objective with
   multiple starts and event-boundary-aware integration;
2. independently profile all coverage-c intervals by
   `a(u)=F^{-1}(u)`, `b(u)=F^{-1}(u+c)`, `u in [0,1-c]`;
3. refine and optimize every detected profile basin;
4. compare objective values, coverage residual, truncated-moment residual,
   order, separation, local curvature, and quadrature error;
5. store solver and distribution digests; and
6. record uniqueness versus a minimizer set.

If global minimizers are unresolved, remove single-pair endpoint RMSE for that
cell. Use excess population RQR risk and distance to the minimizer set.

Add unit tests for affine transformation, finite risk, scale positivity, and
endpoint separation.

## 4. DGP revisions

Keep the static Gaussian, local-level Gaussian, local-level log-normal, unequal
trend-regression, structural stress, and sensitivity roles.

Add these required contrasts:

```text
trend_seasonal_gaussian
  same location, seasonal scale, components, state variances, horizons, and
  seeds as trend_seasonal_skewed

rqr_dlm_common_evolution_ablation
  common scale or common discount on the unequal trend-regression mechanism
```

The first separates multicomponent/time-varying-scale behavior from asymmetry.
The second identifies the value of component-specific evolution.

Move every provisional DGP value into the machine-readable scenario table:

```text
initial-state law
predictor law
innovation variances/covariances
seasonal period/amplitude/phase
positive scale formula and floor
break component, location, and magnitude
error-mixture weights/shifts/scales
training and future transition rules
minimum root separation
```

The structural-break heavy-tail mechanism remains a composite stress test. Do
not attribute its result separately to break or tail robustness.

For the independent-root sensitivity, define ordered paths `L_t < U_t` and
generate a response law through

```text
s_t  = (U_t - L_t) / (b_c - a_c)
mu_t = (b_c L_t - a_c U_t) / (b_c - a_c)
Y_t  = mu_t + s_t Z_t
```

A root pair alone is not a response DGP. Recover `mu_t,s_t` once from a
declared reference coverage and derive all other coverage-level roots from the
same response process; do not generate a different DGP for 0.80 and 0.90.

Predeclare DGP-only nondegeneracy gates. Any pilot change to DGP parameters
must occur before confirmatory seeds are opened and cannot use comparative
method rankings.

## 5. Estimands and target-aligned error

Replace any generic endpoint-RMSE definition with explicit target alignment:

```text
RQR methods:
  compare endpoints with population RQR roots

quantile methods:
  compare endpoints with population equal-tailed quantiles

all methods:
  held-out RQR loss, empirical coverage, width, interval score, failure,
  computation

optional:
  distance of a quantile interval to the RQR roots, labeled cross-target
  distance rather than quantile-estimator bias
```

Keep both oracle truth objects in every replication output.

Held-out RQR loss is a primary target-specific measure, not a target-neutral
score. The central interval score is secondary and canonically equal-tailed.
Do not add a response log score or CRPS for RQR.

## 6. Replace the coverage-width qualification rule

Delete:

```text
abs(coverage - nominal) <= 0.01 + 1.96 * MCSE(coverage)
```

Use:

```text
main practical coverage margin:
  Delta_C = 0.02

coverage qualification:
  the 90% TOST interval for (coverage - nominal) lies wholly inside
  [-Delta_C, +Delta_C]

narrower-width claim:
  both methods are coverage-qualified and the paired width-difference interval
  supports the direction

mandatory display:
  coverage-width frontier for every method
```

The 0.02 margin is compatible with the current cap: at exact nominal coverage,
approximately 1,083 replications are needed for `c=0.80` and 609 for `c=0.90`.
A 0.01 margin would need approximately 4,330 and 2,436, respectively. Increase
the cap before opening seeds if the smaller margin is scientifically required.

Do not calibrate or standardize width using test responses.

## 7. Comparator contracts

### Dynamic quantile comparator

Freeze one engine. A suitable core option is CRAN `exdqlm` 1.1.0 only if:

```text
exact exdqlm_1.1.0.tar.gz is cached
SHA-256 is recomputed and frozen
build/install occur in a fresh isolated runtime
no exdqlm checkout is loaded, compiled, or installed
reduced AL/DQLM MCMC is used at fixed quantile levels
state components, covariates, priors/discounts, origins, and horizons are
matched
adapter tests verify indexing and forecast orientation
```

Do not silently use exAL skewness learning, LDVB, or a different state design.
Record raw lower/upper quantile crossings and freeze the ordering/reporting
rule.

Freeze one static quantile implementation; remove “frequentist or Bayesian”
ambiguity from the config.

### Conformal sensitivity

Do not put ordinary iid split CQR in the dependent core. EnbPI or a weighted
nonexchangeable method may enter only as a sensitivity on mechanisms whose
assumptions and tuning rule have been explicitly checked. It is acceptable to
omit conformal methods from the first diagnostic pilot.

### Tuning

Freeze:

```text
training/validation windows
rolling origins
common versus block-specific discounts
maximum search combinations
target-specific validation criterion
deterministic ties
failure rule
refit rule
equal computational budget
fixed literature-standard sensitivity
```

RQR may tune on training-only RQR loss and quantile methods on training-only
check loss, but describe this as target-specific tuning. Never tune a
generalized-Bayes rate using test coverage.

## 8. Monte Carlo and MCMC contracts

Retain:

```text
confirmatory start:   500 replications
batch:                250
maximum:              2500
coverage MCSE target: 0.01
independent unit:     replication
stopping:             precision only
```

Clarify that 0.80 coverage needs about 1,600 replications and 0.90 about 900 at
MCSE 0.01.

Replace the ambiguous 2% rule with:

```text
endpoint/midpoint error MCSE:
  <= 0.02 * training-response SD

mean width and paired width-difference MCSE:
  <= 0.02 * mean oracle RQR width

held-out standardized RQR loss:
  separately frozen absolute or relative MCSE target
```

A near-zero oracle width is a failed DGP, not a reason to switch denominators.

Define the diagnostic pilot as at least two preselected replications per
mechanism-by-coverage cell and MCMC method, four chains each. Remaining pilot
replications may use one chain only after the schedule is frozen.

Confirmatory one-chain fits must have within-chain ESS, finite-state,
zero-repair, and exact-provenance gates. Four-chain sentinels must be selected
before generation, stratified by mechanism, coverage, method, and 250-run
batch, using 5% with a minimum of two per stratum. A sentinel failure follows a
predeclared cell stop; it never triggers reseeding or adaptive extension.

## 9. Forecast estimands

Store separately:

```text
realized_root_path
  roots after generated future state innovations

oracle_conditional_mean_root
  E(root_{T+h} | information at T, DGP parameters)
```

Use the conditional-mean oracle for bias/RMSE of the method's posterior-mean
root forecast. Use the realized path for realized forecast error. Keep the
true-W method noncompetitive.

The exact reason is the local-level counterexample:

```text
L_{T+1} = L_T + epsilon
E(epsilon) = 0
Var(epsilon) = q > 0
```

A perfect conditional-mean forecast is `L_T` but has RMSE `sqrt(q)` against
the realized root. Do not call that irreducible innovation an estimation bias.

Empirical coverage of the point interval against generated future responses is
a valid repeated-sampling operating characteristic. It is not posterior-
predictive response coverage.

## 10. Reproducibility and reference implementation stage

After the design revision, implement only:

```text
preflight
oracle-reference
tiny-end-to-end
diagnostic-pilot-preflight
```

Do not enable or run `diagnostic-pilot` in the same pass.

Required gates:

```text
versioned scenario/method/estimand schemas
oracle solver and transformation references
DGP moment/positivity/separation checks
matched comparator adapters
full seed ledger and independent streams
exact source/runtime attestations
atomic outputs and rollback
failure denominator
process/thread/RSS/time monitor
compact per-replication schemas
recursive hashes
all execution flags false
```

Return a new independent-review prompt. The next review decides whether the
diagnostic pilot may execute.

## 11. Manuscript scope

No immediate manuscript target correction is required. Preserve:

```text
RQR is generalized Bayes under an interval-root loss.
The pseudo-AL construction is not an ordinary response likelihood.
Root/state draws are not response draws.
Fixed W, frozen templates, and shared component scales use exact
root-conditional FFBS for their declared fixed joint targets.
Adaptive discount is a working method.
Learned lambda is not automatic coverage calibration.
```

Do not add simulation-result prose until the confirmatory study is completed
and independently audited.

## 12. Validation for this implementation pass

Run and report:

```text
source/config parse
all new config tests
native package tests
R CMD check --no-manual
main and supplement builds
promoter negative tests
oracle-reference deterministic tests
DGP construction/moment tests
comparator source/runtime preflight
two-replication byte-reproduction fixture
monitor/output fault tests
fail-closed diagnostic and confirmatory negative tests
```

No heavy simulation, CAVI/ELBO, or RQR-DESN work is authorized.
