# Oracle Mean-Tilt Validation V1 Protocol

## Decision and scope

This protocol defines a repeated-DGP validation of exact fixed tilts for RQR,
equal-tailed (ET), and shortest-contiguous (SH) interval functionals. It covers
proper-Gaussian ridge fixed-design root regression and fixed-(W_t) RQR-DLM.
It does not cover DESN, RHS-NS, exAL, VB/CAVI, learned loss scale, adaptive
discounting, component-scale evolution, tilt estimation, or tilt selection.

The asymmetric-Laplace law is the known response DGP. It is not asserted to be
the fitted response likelihood: computation remains a generalized-Bayes loss
update. Oracle tilts are fixed before data generation and are not selected
from realized recovery.

## ADEMP contract

### Aims

1. Estimate repeated-sample endpoint recovery for each target's own oracle.
2. Estimate exact DGP conditional content of fitted point-summary intervals.
3. Estimate midpoint, width, and target-specific excess-risk recovery.
4. Describe generalized-posterior endpoint credible-summary inclusion as a
   secondary frequentist diagnostic.
5. Record convergence, failures, numerical repairs, exact-target status,
   runtime, and storage.
6. For DLMs, assess time-local and prespecified missing-window recovery;
   future-root results remain secondary.

### Data-generating mechanisms

The six prespecified scenarios cross symmetric, reflected-skew, primary-skew,
higher-content, high-content-stress, and information-growth roles. Each
scenario is evaluated for both model families and all three targets, producing
a 36-row explicit incidence matrix.

The fixed-design DGP is

\[
Y_i=\mu(x_i)+s(x_i)Z_i,
\]

on deterministic design and evaluation grids. The symmetric control is
linear and homoscedastic; the remaining scenarios use an eight-dimensional
cubic B-spline span for both location and positive varying scale. Thus every
target endpoint is in the fitted root-design span.

The dynamic DGP evolves only the local location block. Skewed scenarios add a
declared deterministic four-cycle seasonal location component and a
deterministic, state-span varying scale. The fitted root model uses the same
four-state observation span and fixed evolution covariance. Missing responses
are passed to the fit as `NA`; complete generated responses remain DGP truth
only.

### Methods

Every generated dataset is shared by a paired target triplet:

| Label | Fixed input | Target |
|---|---|---|
| RQR | `delta=0` | mean-preserving content interval |
| ET | exact site/time-scaled oracle tilt | equal-tailed interval |
| SH | exact site/time-scaled oracle tilt | shortest contiguous interval |

All fits use a fixed learning rate, proper Gaussian ridge prior, and a common
computation schedule. A target is evaluated against its own population
endpoints and its own tilted loss.

### Estimands

Primary replication-level estimands are lower/upper bias and MAE, joint
endpoint RMSE, midpoint bias, width bias and ratio, exact conditional-content
error, and known-DGP excess expected tilted loss. Secondary quantities include
pointwise endpoint credible-summary inclusion, missing-window recovery, and
resource/diagnostic summaries. Dataset replication is always the Monte Carlo
unit.

## Random-number and failure contract

- L'Ecuyer-CMRG substreams are derived from one recorded master seed.
- A scenario/family/replication uses one DGP stream shared across RQR, ET, and
  SH and distinct target-specific MCMC streams.
- No seed screening, response replacement, selective rerun, or fit-based
  exclusion is permitted.
- Every task obtains either a validated atomic result or a structured terminal
  failure record. Failures remain in denominators.
- Resume requires the same task key, source SHA, config digest, runtime digest,
  and RNG-stream digest.

## Precision and resources

Candidate group-sequential checkpoints are 250, 500, 750, 1,000, 1,500, and
2,000 dataset replications. The tracked config intentionally contains no
authorized replication count. Production-shape benchmarks must first freeze
runtime, storage, concurrency, and an excess-risk practical margin. The final
rule requires all prespecified primary precision metrics to pass at a declared
checkpoint and prohibits target- or result-specific extension.

Four overdispersed chains are required for every sentinel cell. Ordinary
replications use one fixed-budget chain only after all sentinels pass. A
standard fit failing its prespecified diagnostics remains a failure; it is not
rescued with a new seed.

## Workflow

The monitored runner implements these modes:

```text
preflight
reference-only
benchmark
sentinel
execute-wave
precision-check
verify-closeout
health-check-read-only
```

Strict read-only collection and compact packaging are separate entry points,
`61_collect_oracle_mean_tilt_validation.R` and
`62_package_oracle_mean_tilt_validation.R`. Keeping them outside the fit
runner prevents a collection or packaging request from being mistaken for an
execution mode.

Tracked config keeps `replication_schedule_frozen=false` and
`execution_authorized=false`. Consequently benchmark requires explicit review
confirmation and an isolated runtime, while sentinel and execution fail closed
until a later reviewed config freezes the schedule. Execution additionally
requires a separate environment confirmation and bound preflight, reference,
benchmark, and sentinel bundles.

Each bound bundle is accepted only when its inner artifact manifest, monitored
wrapper manifest, wrapper closeout, source SHA, config digest, runtime-tree
digest, and clean final process-group state agree. Execute-wave resume also
binds every atomic task envelope to the generated fit plan and DGP/MCMC RNG
ledger. A terminal status without its matching task artifact, or an orphan
task artifact without the matching terminal status, is an integrity failure.

Raw task artifacts, logs, runtimes, and chains remain under ignored local
roots. Only compact schemas, replication estimands, diagnostics, failure
counts, precision decisions, closeouts, and hashes can be promoted.

## Readiness checklist

- [x] Exact schema-2 oracle and 27 AL certificates implemented.
- [x] The 18-row oracle table and 36-row incidence matrix are generated.
- [x] All 72 endpoint representability checks pass below (10^{-10}).
- [x] Tail counts and endpoint densities are machine-readable.
- [x] Exact conditional-content and expected-risk evaluators are implemented.
- [x] Paired L'Ecuyer stream derivation and task identities are implemented.
- [x] Atomic task, failure, collection, precision, and packaging code exists.
- [x] Public-oracle unit tests include quadrature, reflection, affine, risk,
  asymmetric nonzero-mean, and minimizer-set checks.
- [x] Tiny real fixed-design and fixed-W DLM fits exercise both public engines.
- [x] Source-level package check and the native package suite pass.
- [x] Monitored no-fit preflight and the 44-gate reference suite pass locally.
- [x] Sentinel and execute-wave fail closed before a fit in the tracked config.
- [x] Atomic resume, collection, and packaging validate source, runtime, plan,
  RNG, resource, and recursive artifact identities.
- [x] Execution is fail closed in the tracked config.
- [ ] Production-shape benchmarks complete under a reviewed isolated runtime.
- [ ] Excess-risk practical margin, replication counts, wave size, timeouts,
  process-tree memory, and storage ceilings are frozen from benchmark evidence.
- [ ] Every four-chain sentinel passes.
- [ ] Independent review approves the exact source.
- [ ] A flag-only authorization commit is created.
- [ ] Repeated-DGP waves execute and close.
