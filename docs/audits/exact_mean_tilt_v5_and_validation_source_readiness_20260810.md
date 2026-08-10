# Exact Mean-Tilt V5 and Repeated-Validation Source Readiness

## Scope and decision

This audit records the source-readiness result for two deliberately separate
workflows:

1. an append-only V5 correction of the six existing single-data illustrations;
2. a future repeated-DGP validation of RQR, equal-tailed (ET), and shortest
   contiguous (SH) interval targets for ridge fixed-design regression and a
   fixed-evolution RQR-DLM.

The public oracle, runners, monitors, collectors, packagers, tests, and
documentation are implemented. Heavy execution remains disabled. This is a
source-readiness result, not empirical evidence and not launch authorization.

## Statistical correction

For an interval `[Q(u), Q(u+c)]`, the fixed response-scale mean tilt is

\[
\delta(u)=E\{Y\mid Q(u)\leq Y\leq Q(u+c)\}-E(Y).
\]

The historical V1--V4 illustration helper omitted division of the truncated
first moment by `c`. The fixed-tilt Gibbs kernels did not contain this error;
only the historical illustration-oracle inputs were mis-scaled. The correction
therefore uses a new schema and append-only evidence root. It never rewrites or
relabels prior runs.

## Implemented contracts

| Area | Implemented boundary |
|---|---|
| Oracle | Public schema-2 certificate for RQR, ET, and SH; exact AL partial moments; numerical quadrature cross-check; conditional content; expected tilted risk; minimizer and uniqueness evidence; deterministic digests |
| V5 illustrations | Same V3 DGPs, designs, priors, seeds, and MCMC schedules; corrected oracle is the only scientific change; preflight/reference/benchmark/execute/package stages; legacy evidence rejection |
| Repeated validation | Six prespecified scenarios, two model families, three target functionals, paired target triplets, ridge/fixed-W scope, 36-row incidence contract, 72 representability checks |
| RNG | L'Ecuyer-CMRG master stream; one DGP stream per scenario/family/replication; distinct target MCMC streams; immutable RNG ledger; no seed screening or response replacement |
| Computation | Four-chain sentinels; one-chain ordinary replications only after sentinel passage; maintained `posterior` R-hat, bulk/tail ESS, and mean MCSE; zero-repair exact-target requirements |
| Persistence | Atomic task envelopes; explicit terminal statuses; structured failure ledger; resumable bounded waves; recursive artifact manifests; strict compact packaging |
| Provenance | Clean exact source SHA and isolated runtime binding for promotion stages; source/config/runtime/plan/RNG digests checked at every boundary |
| Resources | Monitored process group, pre-start thread limits, timeout and sampled process-tree bounds, final process-group sweep, free-space and output-size checks |
| Interpretation | Generalized-Bayes loss update only; interval-root summaries are not response-predictive draws; exact known-DGP tilts are not estimated or selected |

## Validation completed at the source stage

| Gate | Result | Meaning |
|---|---:|---|
| Public-oracle tests | Pass | Analytic/numerical moments, 27 AL target certificates, transformations, reflection, tilted risk, minimizer set, invalid inputs, and legacy rejection |
| V5 workflow tests | Pass | Historical-design invariance, schema-2 enforcement, fail-closed execution, and packaging contracts |
| Repeated-validation tests | Pass | Scenario construction, incidence/RNG contracts, precision/resource validation, atomic integrity, and tiny real fixed-design/DLM fits |
| Native package suite | Pass | Existing and new native R/Rcpp contracts; one expected optional-DESN skip; one documented adaptive-discount warning |
| `R CMD check --no-manual` | Pass | Package version 0.1.0.9035; status OK |
| `make smoke` | Pass | Repository smoke gate |
| Article and supplement builds | Pass | Both TeX documents compile; no manuscript evidence was promoted |
| V5 preflight | Pass, zero fits | Frozen-design and corrected-oracle source checks |
| V5 reference | 36/36 pass, zero fits | Oracle/design/reference contracts only |
| Repeated-validation preflight | Pass, zero fits | 18 oracle rows, 36 incidence rows, and 72 representability audits |
| Repeated-validation reference | 44/44 pass, zero fits | Deterministic DGP, truth, RNG, fit-plan, oracle, and artifact contracts |
| Disabled-mode probe | Pass | Sentinel and execute-wave stop before producing a fit while authorization/schedule remain false |

The local no-fit runs used ignored cache directories. Their resource telemetry
is operational evidence only: it is not a production benchmark and is not
tracked as a scientific result.

## Deliberately open gates

- [ ] Commit and independently review the exact source state.
- [ ] Build a fresh isolated primary runtime from that complete commit.
- [ ] Run V5 production-shape benchmarks and freeze its launch decision.
- [ ] Execute and close all six corrected V5 illustration cells.
- [ ] Promote V5 compact evidence and only then update article illustrations.
- [ ] Benchmark the repeated-DGP production shape without competing heavy work.
- [ ] Freeze the excess-risk practical margin, replication checkpoints, wave
  size, timeouts, process-tree memory ceiling, and storage ceiling.
- [ ] Pass every repeated-validation four-chain sentinel.
- [ ] Obtain independent review and create a flag-only authorization commit.
- [ ] Execute bounded repeated-DGP waves and apply the prespecified stopping
  rule using dataset replication as the Monte Carlo unit.

The repeated-DGP workflow is not launched by this implementation commit. V5
and the repeated validation also remain separate from the active ordinary
zero-tilt RQR-DLM study and from future DESN, exAL, RHS-NS, learned-scale,
adaptive-discount, VB/CAVI, tilt-estimation, and tilt-selection work.
