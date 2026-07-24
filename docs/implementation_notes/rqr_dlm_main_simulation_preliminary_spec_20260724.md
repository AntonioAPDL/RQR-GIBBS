# Preliminary matched RQR-DLM simulation specification

Date: 2026-07-24
Status: draft for independent review; pilot and production execution disabled

## Scope

The first matched study should evaluate the MCMC RQR-DLM before adding
RQR-DESN or a variational approximation. The statistical target is a pair of
interval-root functions under the RQR loss. The study will not evaluate a
response density because the generalized posterior and its pseudo-AL
augmentation do not define one.

The plan follows the ADEMP organization recommended for statistical
simulation studies: aims, data-generating mechanisms, estimands, methods, and
performance measures. Replication uncertainty will be reported through Monte
Carlo standard errors rather than hidden behind a nominal replication count
\citep{MorrisWhiteCrowther2019Simulation}. Proper scoring-rule principles
motivate the secondary use of the central interval score, but that score does
not share the RQR endpoint target under asymmetry
\citep{GneitingRaftery2007}.

The machine-readable draft is
`application/config/rqr_dlm/rqr_dlm_main_simulation_preliminary_20260724.R`.
Its pilot and production authorization fields are false.

## Aims

The confirmatory study should answer six questions.

1. Does RQR-DLM recover population RQR roots and nominal coverage in repeated
   samples when root functions evolve over time?
2. Under asymmetric conditional distributions, does direct root estimation
   reduce held-out RQR loss or width at comparable coverage relative to
   preassigned equal-tailed quantile endpoints?
3. Under a symmetric Gaussian negative control, is RQR-DLM competitive without
   an artificial asymmetry advantage?
4. Do dynamic roots improve over fixed-design RQR when the interval functions
   change over time?
5. Do component-specific evolution scales or frozen component discounts help
   when trend, seasonal, and regression blocks evolve at different rates?
6. How sensitive are point summaries, mixing, and computation to the fixed
   versus normalized learned generalized-Bayes rate?

The study is not designed to establish that the learned rate calibrates
coverage, that RQR roots are posterior-predictive response intervals, or that
one method dominates outside the declared mechanisms.

## Data-generating mechanisms

### Common location-scale construction

Let \(Z\) have a declared continuous standardized distribution, and let
\(Y_t=\mu_t+s_t Z_t\), where \(s_t>0\). For coverage \(c\), define
\((a_c,b_c)\) as a population stationary RQR pair for \(Z\):

\[
\Pr(a_c<Z<b_c)=c,\qquad
\mathbb E\{Z\,\mathbb I(a_c<Z<b_c)\}
=c\,\mathbb E(Z).
\]

Then the population RQR roots for \(Y_t\), conditional on the generated
location and scale paths, are

\[
L_{t,c}=\mu_t+s_t a_c,\qquad
U_{t,c}=\mu_t+s_t b_c.
\]

This construction gives oracle endpoints without pretending that RQR targets
equal-tailed quantiles. Each standardized root pair will be computed by
global minimization of the population RQR loss over \(a<b\), using multiple
starts and event-boundary-aware numerical integration. The two displayed
moment equations are necessary stationary conditions, not by themselves a
global-minimum or uniqueness proof. An independent solver will verify the
objective value, stationarity, endpoint order, and local curvature at tighter
tolerances. A mechanism with unresolved multiple global minimizers will not
use endpoint RMSE as a primary measure. The equal-tailed quantiles of \(Z\)
will be stored separately as comparator truth.

Training-only centering and scaling will be applied to every fitted method and
inverted before evaluation. This is important because the RQR loss has squared
response units and the generalized-Bayes rate consequently has inverse-squared
units.

### Core mechanisms

| ID | Statistical role | Location and scale | Error distribution | Main contrast |
|---|---|---|---|---|
| static Gaussian | negative control | static linear location, constant scale | standard normal | RQR and central equal-tailed roots coincide by symmetry |
| local-level Gaussian | dynamic negative control | Gaussian local level, constant scale | standard normal | dynamic tracking without an asymmetry advantage |
| local-level skewed | primary RQR signal | same local level, constant scale | centered and standardized log-normal | direct roots versus preassigned quantiles |
| trend and seasonal skewed | multicomponent signal | local linear trend with seasonal location and positive seasonal scale | centered and standardized log-normal | component dynamics with asymmetric intervals |
| trend and regression with unequal evolution | component-scale signal | slowly evolving trend and faster dynamic regression coefficient | centered and standardized log-normal | component-specific versus common evolution |
| structural-break heavy-tail stress | misspecification | one predeclared level or coefficient break | centered, standardized skewed heavy-tail mixture | failure and recovery under an abrupt change |

Preliminary state scales are deliberately visible rather than hidden in code:
a local-level innovation variance near 0.02; trend and seasonal block scales
near 0.005 and 0.002; and trend and regression scales near 0.005 and 0.05.
The break is provisionally placed at 70% of the training path with magnitude
1.5 training-standard-deviation units. These values are not frozen. The
diagnostic pilot must confirm that the paths neither degenerate nor make the
comparison trivial, after which they may be changed only before confirmatory
seeds are opened.

The skewed heavy-tail law is provisionally a centered and variance-standardized
mixture with 90% standard-normal mass and 10% shifted \(t_3\) mass. The
log-normal law is centered and divided by its population standard deviation.
All laws have finite second moments, as required by the population
characterization used in the article.

Two sensitivities are proposed:

- a known heteroscedastic scale covariate with standardized \(t_5\) errors; and
- a single-coverage mechanism generated from two nearly noncrossing independent
  root-state paths, which checks prior-path alignment separately from the
  location-scale construction.

Core training and forecast horizons are \(T=200\) and \(H=20\), with results
at horizons 1, 5, 10, and 20. Sample-size sensitivity uses \(T=100\) and
\(T=400\) on a reduced set of mechanisms. Coverage levels are 0.80 and 0.90.
A 0.95 level is deferred unless the Monte Carlo budget can estimate its tail
coverage with adequate precision.

Future state innovations are part of the generated mechanism. Oracle endpoint
recovery is evaluated against the realized conditional root path. Empirical
coverage evaluates the point interval formed by posterior means of the
ordered future root functions. This is an operating-characteristic check of
the endpoint forecast; it is not posterior-predictive response coverage.

## Estimands

For every replication, method, coverage level, and horizon, retain:

- the population RQR lower and upper roots;
- the estimated lower and upper roots, defined as posterior means of ordered
  root functions for Bayesian RQR methods;
- root midpoint and width;
- the interval-membership indicator for the generated response;
- the held-out RQR loss;
- the population equal-tailed endpoints; and
- failure, elapsed-time, memory, and diagnostic indicators.

The replication, not an individual horizon, is the independent Monte Carlo
unit. Horizon-level observations remain clustered within replication.

## Methods

### Primary comparison

1. **RQR-DLM with component-specific scales and fixed learning rate.** This is
   the primary proposed method. Component templates are fixed, their positive
   scales have the implemented inverse-Gamma updates, and the two roots share
   each component scale. The standardized-scale primary rate is one.
2. **Matched dynamic equal-tailed quantile interval.** Fit dynamic quantile
   models at \((1-c)/2\) and \((1+c)/2\) using the same state components,
   covariates, training window, and forecast origins. The implementation must
   be independently validated. If CRAN exdqlm 1.1.0 is used, its source
   tarball and SHA-256 must be pinned and built outside every protected source
   checkout.

The dynamic quantile comparator targets different endpoints under asymmetry.
That difference is intentional and must be visible in the results.

### Ablations and simple baselines

- RQR-DLM with a frozen component-discount template;
- fixed-design RQR with the same observed covariates;
- static equal-tailed quantile regression; and
- a rolling empirical equal-tailed interval.

A true-\(W\) RQR-DLM on selected mechanisms is an oracle evolution reference,
not a competitor. It cannot enter method rankings.

### Sensitivity-only methods

- the normalized learned-rate RQR-DLM;
- fixed rates 0.5, 1, and 2 on the standardized scale;
- a Gaussian DLM response interval, clearly labeled as a different
  response-likelihood object; and
- a time-series-valid conformal interval if its dependence assumptions and
  tuning protocol are approved.

Ordinary split conformalized quantile regression assumes exchangeability and
is not automatically valid for the dynamic mechanisms
\citep{RomanoPattersonCandes2019CQR}. EnbPI or a weighted method designed for
nonexchangeable data is a more plausible sensitivity comparator, but its
assumptions must be checked rather than inferred from the word “conformal”
\citep{XuXie2021EnbPI,BarberCandesRamdasTibshirani2023BeyondExchangeability}.

The adaptive conditional-discount RQR recursion is excluded because it is a
working update rather than exact Gibbs for a declared fixed joint target.
RQR-DESN and CAVI are also excluded from this first study.

## Tuning and fairness

All transformations, priors, windows, discount grids, and stopping rules use
training data only. The held-out responses cannot select a method,
hyperparameter, chain length, or rescue action.

The preliminary discount grid is
\(\{0.90,0.95,0.98,0.99\}\). Independent component discounts may be selected
by a predeclared blockwise training-validation rule. Before implementation,
the Pro review should decide whether this rule is sufficiently fair relative
to the learned component-scale model or whether a single literature-standard
discount should be frozen.

The primary fixed generalized-Bayes rate is one after training-only response
standardization. The learned normalized rate is a sensitivity. Its posterior
must not be interpreted as response variance or automatic coverage
calibration.

## Performance measures

### Primary

- mean held-out RQR loss;
- empirical coverage, signed coverage error, and absolute coverage error;
- mean width under the coverage-comparability rule;
- lower-root RMSE; and
- upper-root RMSE.

Coverage and width will always be shown together. A width claim is permitted
only when both methods satisfy

\[
|\widehat C-c|\le 0.01+1.96\,\operatorname{MCSE}(\widehat C).
\]

Otherwise width is descriptive and the pair is placed on a coverage-width
plot without a “sharper” conclusion. This preliminary one-percentage-point
equivalence margin requires external review.

### Secondary

- midpoint and width RMSE;
- coverage by horizon and by predeclared true-scale or state-change strata;
- the central interval score, with an explicit equal-tailed-target caveat;
- state-component recovery on aligned mechanisms;
- fit and numerical-failure rates;
- elapsed time and sampled memory; and
- R-hat, bulk ESS, tail ESS, MCSE, and ESS per second on the multichain
  diagnostic subset.

RQR will not receive a response log score, CRPS from an invented response
distribution, or posterior-predictive-density metric.

All method contrasts are paired by replication through common generated data.
Tables will include Monte Carlo standard errors and confidence intervals for
the simulation summaries. Failed fits remain in the intention-to-run
denominator; conditional-on-success summaries are secondary.

## Replications and Monte Carlo precision

A 25-replication pilot per core cell is proposed for runtime, variability, and
diagnostic planning only. It cannot be used to select the favorable method or
remove an unfavorable mechanism.

The confirmatory study starts with 500 independent replications per core cell
and adds predeclared batches of 250, up to 2,500. Stopping depends only on
Monte Carlo precision:

- coverage MCSE at most 0.01;
- root-error and width MCSE at most 2% of the mechanism's oracle width or
  training response scale; and
- reported MCSE for every primary paired contrast.

If a cell reaches 2,500 replications without meeting its precision target, it
stops at the cap and reports the remaining uncertainty. No stopping rule uses
the sign, ranking, or statistical significance of a performance difference.
The pilot must estimate cluster-level variance before these thresholds are
frozen.

## MCMC validation within a many-replication study

Four chains for every method and replication may be unnecessarily expensive
after the bounded validation, but one unexamined chain per fit would be too
weak. The preliminary compromise is:

1. a four-chain diagnostic pilot on every core mechanism and both coverage
   levels;
2. a frozen schedule chosen from the hardest pilot cells using diagnostics,
   not performance rankings;
3. one chain per confirmatory fit; and
4. four chains for a preselected 5% sentinel set of replications, chosen by
   seed before results are observed.

The sentinel set uses maintained rank-normalized R-hat and bulk and tail ESS.
Every fit uses the C++ backend and fail numerical policy. A failed fit is not
reseeded or silently extended. This compromise remains an open review item;
the independent audit may require two or four chains more broadly.

## Reproducibility and execution contract

The future runner should expose:

```text
preflight
oracle-reference
diagnostic-pilot
execute-confirmatory
collect
audit
```

Both pilot and confirmatory modes must fail closed behind separate
configuration and environment confirmations. Each replication receives:

- a deterministic data seed keyed by scenario and replication;
- separate deterministic method seeds;
- a configuration and DGP digest;
- exact source and isolated-runtime identities;
- atomic compact output;
- a structured failure record; and
- recursive artifact hashes.

Large fit objects remain ignored. Compact per-replication endpoints, losses,
coverage indicators, diagnostics, failures, timings, and provenance are
tracked. Jobs parallelize only across independent replications, with one
BLAS/OpenMP thread per process and worker counts set from a measured memory
benchmark.

The simulation implementation cannot load or compile from an exdqlm checkout.
Any exdqlm comparator must use a pinned archive or CRAN source tarball and an
isolated runtime. The Q-DESN article repository remains read only.

## Gates before any main run

1. Independent verification of the global objective, stationary equations,
   uniqueness status, and numerical error for every standardized RQR oracle.
2. Unit tests showing location-scale transformation of the root equations.
3. DGP moment, positivity, nondegeneracy, and path-crossing checks.
4. Matched-design tests for every competitor.
5. A frozen training-only tuning rule.
6. A small end-to-end two-replication fixture with exact seed reproduction.
7. A four-chain diagnostic pilot with no outcome-driven retuning.
8. A reviewed replication-precision calculation and compute budget.
9. A fail-closed production flag and exact-source runtime attestation.
10. An independent review of the runner and compact artifact schemas.

The next authorized action after review is to implement these gates and the
diagnostic pilot. The confirmatory simulation remains unauthorized.

## Primary references

- Morris, White, and Crowther (2019), “Using Simulation Studies to Evaluate
  Statistical Methods,” DOI `10.1002/sim.8086`.
- Pouplin et al. (2024), “Relaxed Quantile Regression: Prediction Intervals
  for Asymmetric Noise,” PMLR 235.
- Gonçalves, Migon, and Bastos (2020), “Dynamic Quantile Linear Models: A
  Bayesian Approach,” DOI `10.1214/19-BA1156`.
- Gneiting and Raftery (2007), “Strictly Proper Scoring Rules, Prediction, and
  Estimation,” DOI `10.1198/016214506000001437`.
- Romano, Patterson, and Candès (2019), “Conformalized Quantile Regression.”
- Xu and Xie (2021), “Conformal Prediction Interval for Dynamic Time-Series.”
- Barber et al. (2023), “Conformal Prediction Beyond Exchangeability,” DOI
  `10.1214/23-AOS2276`.
