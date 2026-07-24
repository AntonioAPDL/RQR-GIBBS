# Preliminary matched RQR-DLM simulation specification

Date: 2026-07-24
Schema: `rqrgibbs_dlm_main_simulation_preliminary/0.2.0`
Status: design revision and reference implementation; diagnostic-pilot and
confirmatory execution disabled

## Statistical scope

The first matched study evaluates MCMC inference for the RQR-DLM before adding
RQR-DESN or a variational approximation. Its inferential object is a pair of
interval-root functions under the RQR loss. The generalized posterior and the
pseudo-AL augmentation do not define a response likelihood or a
posterior-predictive response distribution.

The design follows the ADEMP organization for statistical simulation studies:
aims, data-generating mechanisms, estimands, methods, and performance
measures. Replication uncertainty is reported using Monte Carlo standard
errors \citep{MorrisWhiteCrowther2019Simulation}. The central interval score
is secondary because its canonical endpoints are equal-tailed quantiles,
which generally differ from population RQR roots under asymmetry
\citep{GneitingRaftery2007}.

The machine-readable sources are:

```text
application/config/rqr_dlm/
  rqr_dlm_main_simulation_preliminary_20260724.R
  rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv
  rqr_dlm_main_simulation_preliminary_methods_20260724.csv
```

Only `preflight`, `oracle-reference`, `tiny-end-to-end`, and
`diagnostic-pilot-preflight` are implemented. The latter constructs a plan;
it does not run a pilot. Both execution flags remain false.

## Aims

The confirmatory study, if later authorized, will address six questions.

1. How accurately do RQR methods estimate population RQR roots, and how
   accurately do quantile methods estimate population equal-tailed
   quantiles?
2. How do the methods compare in held-out RQR loss, repeated-sampling response
   coverage, interval width, and the central interval score?
3. Does a dynamic root model improve on fixed-design RQR when the target roots
   evolve?
4. Does the trend-seasonal comparison change when only the standardized error
   law changes from Gaussian to skewed?
5. Does component-specific evolution improve on a common-evolution RQR-DLM
   when trend and regression components have unequal innovation scales?
6. What numerical, MCMC, computational, and infrastructure failures occur
   when all intended fits remain in the reporting denominator?

The structural-break heavy-tail mechanism is a composite stress test. Its
results cannot identify separate break robustness and tail robustness.
Learned generalized-Bayes scale remains a sensitivity, not automatic coverage
calibration or response variance.

## Population oracle

Let

\[
Y_t=\mu_t+s_tZ_t,\qquad s_t>0,
\]

where \(Z_t\) follows a declared standardized continuous law. For coverage
\(c\), an ordered candidate interval can be parameterized by

\[
a(u)=F_Z^{-1}(u),\qquad
b(u)=F_Z^{-1}(u+c),\qquad 0\le u\le 1-c.
\]

The population RQR risk is

\[
\mathcal R_c(a,b)
=
\mathbb E\!\left[
  \rho_c\{(Z-a)(Z-b)\}
\right].
\]

The implementation searches the one-dimensional coverage profile on a
deterministic grid, identifies and refines every detected minimum basin, and
independently minimizes the unrestricted two-dimensional objective using a
midpoint and log-width parameterization with multiple starts. Each reference
records:

- the distribution and solver digests;
- both endpoint pairs and objective values;
- the coverage and truncated-first-moment residuals;
- the objective gap between the searches;
- endpoint separation and local profile curvature;
- all detected profile minima;
- a uniqueness or minimizer-set declaration; and
- an estimated quadrature error, explicitly not a rigorous error bound.

The location-scale relation is

\[
L_{t,c}=\mu_t+s_ta_c,\qquad
U_{t,c}=\mu_t+s_tb_c,
\]

and positive homogeneity of the check loss gives

\[
\mathcal R_{Y,c}(L_{t,c},U_{t,c})
=s_t^2\mathcal R_{Z,c}(a_c,b_c).
\]

If the global minimizer is unresolved or nonunique, endpoint RMSE against a
single pair is removed for that cell. Excess population RQR risk and distance
to the minimizer set replace it.

## Data-generating mechanisms

All numerical values are frozen in the scenario CSV rather than embedded
only in prose. Each row records the initial-state law, predictor law,
innovation covariance, seasonal structure, positive scale rule and floor,
break specification, mixture parameters, training and future transitions,
reference coverage, minimum root separation, and claim scope.

The core mechanisms are:

1. static Gaussian negative control;
2. local-level Gaussian dynamic control;
3. matched local-level skewed mechanism;
4. trend-seasonal Gaussian multicomponent control;
5. matched trend-seasonal skewed mechanism;
6. trend-regression mechanism with unequal component evolution; and
7. structural-break and heavy-tail composite stress.

The two trend-seasonal mechanisms have the same location path, scale path,
state components, innovation variances, seasonal period, amplitude, phase,
training horizon, forecast horizon, and state seed. Only the standardized
error law differs. The unequal trend-regression mechanism includes a
competitive common-evolution RQR-DLM ablation; a true-\(W\) oracle alone
cannot identify the benefit of component-specific evolution.

Two sensitivity mechanisms cover a known heteroscedastic scale covariate and
prior alignment with two root paths. A root pair alone does not define a
response process. For the latter mechanism, reference-coverage paths
\((L_t,U_t)\) determine

\[
s_t=\frac{U_t-L_t}{b_{\mathrm{ref}}-a_{\mathrm{ref}}},
\qquad
\mu_t=
\frac{b_{\mathrm{ref}}L_t-a_{\mathrm{ref}}U_t}
     {b_{\mathrm{ref}}-a_{\mathrm{ref}}}.
\]

The response is then generated as \(Y_t=\mu_t+s_tZ_t\). Roots for 0.80 and
0.90 coverage are derived from this same response law; changing coverage does
not generate a new dataset.

Every generated path must pass DGP-only finite-value, scale-floor, endpoint
separation, moment, and nondegeneracy gates. DGP parameters cannot be changed
because of comparative method rankings.

## Target-aligned estimands

Endpoint error is target specific:

```text
RQR methods:
  endpoint error relative to population RQR roots

quantile methods:
  endpoint error relative to population equal-tailed quantiles

all methods:
  held-out RQR loss
  empirical response coverage
  interval width
  central interval score
  failure and computation
```

A quantile interval may additionally be compared with the RQR roots, but the
result is labeled a cross-target distance rather than quantile-estimator
bias. Held-out RQR loss is the RQR home-target measure, not a target-neutral
score. The central interval score is secondary and equal-tailed-targeted.
RQR is not assigned a response log score, CRPS from an invented response
distribution, or a posterior-predictive-density score.

## Coverage and width

Coverage and width are always displayed jointly. The primary practical
coverage margin is

\[
\Delta_C=0.02.
\]

A method is coverage qualified only when its 90% two-one-sided-test interval
for the coverage error lies wholly inside
\([-\Delta_C,\Delta_C]\). A narrower-width statement additionally requires
both methods to be coverage qualified and a paired width-difference interval
that supports the stated direction. Coverage-width frontiers are mandatory.
No method may use test responses for post hoc width calibration.

At exact nominal coverage, the chosen margin requires approximately 1,083
replications for \(c=0.80\) and 609 for \(c=0.90\). A one-percentage-point
margin would require approximately 4,330 and 2,436, respectively, and is
incompatible with the present 2,500-replication cap at 0.80 coverage.

## Methods and matching

The primary RQR method uses component-specific inverse-Gamma evolution scales
and fixed generalized-Bayes rate one after training-only response
standardization. Its main dynamic comparator comprises separate lower and
upper reduced-AL DQLM MCMC fits at
\((1-c)/2\) and \((1+c)/2\).

The dynamic quantile comparator is frozen to CRAN `exdqlm` 1.1.0:

```text
source:
  https://cran.r-project.org/src/contrib/exdqlm_1.1.0.tar.gz

SHA-256:
  51bc968f617721c9ab1dcfc6ec14857d30827fcd36659f3de45337cc3c82bd14

allowed engine:
  reduced AL/DQLM MCMC at fixed quantiles

excluded:
  exAL skewness learning
  LDVB
  loading, compiling, or installing from an exdqlm checkout
```

The source tarball is installed in a fresh isolated library under
`application/cache/`. The state components, covariates, priors or discounts,
forecast origins, and horizons must match their RQR counterparts or have a
recorded justification. Raw lower and upper quantile forecasts are retained;
ordering is applied separately for interval metrics.

Static quantile regression is frozen to `quantreg::rq` from CRAN `quantreg`
6.1, source tarball SHA-256
`f42292c5ab25a15f39295b93391deafef192fe09eefde563399a64eba7e0169a`.
It uses a separate ignored isolated runtime and retains the raw endpoint fits;
ordering occurs only for interval scoring. The previous “frequentist or
Bayesian” ambiguity has been removed. Ordinary iid split CQR
is omitted from the dependent core and the first diagnostic pilot. A
time-series conformal sensitivity can be reconsidered only after its
dependence assumptions and tuning contract have been verified
\citep{RomanoPattersonCandes2019CQR,XuXie2021EnbPI,BarberCandesRamdasTibshirani2023BeyondExchangeability}.

## Training-only tuning

The config freezes rolling validation origins, validation horizon, common and
block-specific discount grids, a maximum of 16 candidates, target-native
criteria, deterministic ties, failed-candidate treatment, full-training
refitting, and equal search budgets. RQR methods use validation RQR loss;
quantile methods use validation check loss. This is target-specific tuning,
not a common response score. Test coverage cannot tune a generalized-Bayes
rate or any interval width.

## Monte Carlo precision

The confirmatory plan retains a minimum of 500 replications, increments of
250, and a maximum of 2,500. Stopping can depend only on the frozen precision
criteria:

```text
coverage:
  MCSE <= 0.01

endpoint and midpoint errors:
  MCSE <= 0.02 * training-response SD

mean width and paired width difference:
  MCSE <= 0.02 * mean oracle RQR width

standardized held-out RQR loss:
  MCSE <= 0.01
```

The replication is the independent Monte Carlo unit. Coverage MCSE 0.01
requires approximately 1,600 replications at 0.80 and 900 at 0.90. A
near-zero oracle width is a failed DGP rather than a reason to change the
denominator. A cell that reaches the cap reports its remaining uncertainty.
Stopping never uses rank, sign, significance, or a favorable result.

## MCMC evidence

The diagnostic pilot will use four dispersed chains for at least two
preselected replications per mechanism, coverage, and MCMC method. The final
schedule can depend on the worst predeclared diagnostic summaries, but not on
comparative performance.

A future one-chain confirmatory fit must pass finite-state, zero-repair,
exact-provenance, and within-chain bulk and tail ESS gates for predeclared
functionals. Four-chain sentinels are selected before data generation within
mechanism, coverage, method, and 250-replication-batch strata. The sentinel
rate is 5% with at least two sentinels per stratum. A failure invokes the
predeclared cell stop; it does not cause reseeding or outcome-driven
extension.

## Forecast objects

Two future root objects are stored:

```text
realized_root_path:
  roots after generated future state innovations

oracle_conditional_mean_root:
  expected roots conditional on information at the forecast origin and the
  DGP parameters
```

Bias and RMSE of a posterior-mean root forecast use the conditional-mean
oracle. Realized forecast error uses the realized path. For a local-level
root \(L_{T+1}=L_T+\epsilon_{T+1}\), a perfect conditional-mean forecast
equals \(L_T\) but has RMSE \(\sqrt{\operatorname{Var}(\epsilon_{T+1})}\)
against the realized root. That innovation error is not estimator bias.

Empirical coverage of a point interval against generated future responses is
a repeated-sampling operating characteristic. It is not
posterior-predictive response coverage.

## Reproducibility stages

This implementation pass contains four fail-closed stages:

```text
preflight
oracle-reference
tiny-end-to-end
diagnostic-pilot-preflight
```

The stages validate versioned schemas, deterministic independent seed
streams, oracle references, DGP construction, a two-replication compact
byte-reproduction fixture, atomic publication, recursive hashes, and the
isolated CRAN comparator runtime. Large fits remain ignored. Compact outputs
must retain target, response, numerical, MCMC, infrastructure, and provenance
failure classifications.

The diagnostic-pilot preflight writes an explicit plan with
`execution_authorized=FALSE`. There is no diagnostic-pilot or confirmatory
execution branch. Another independent review is required before either can be
enabled.

## Work deliberately deferred

- diagnostic-pilot execution;
- confirmatory simulation;
- simulation-result manuscript prose;
- CAVI and ELBO derivation;
- RQR-DESN implementation or comparison; and
- response-distribution scores for RQR.

The accepted bounded 24-fit validation is not rerun. Its promoter is hardened
separately by binding an externally frozen source/runtime/reference bundle,
recomputing internal fit digests after reopening, invoking the continuation
validator, and requiring exact 24-fit set equality across every relevant
compact manifest.
