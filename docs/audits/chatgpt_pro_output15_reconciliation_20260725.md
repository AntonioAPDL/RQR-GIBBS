# Output-15 implementation and reference reconciliation

## Decision

The Output-15 design has been implemented, its exact preflight and
oracle/reference stages pass, and the repository-wide validation matrix is
green. The full confirmatory simulation has not started.

The next authorized action is an independent review of the implementation and
compact evidence. A favorable review may authorize a separate commit that
changes only:

```r
confirmatory_execution_authorized = FALSE
```

to `TRUE`. That commit must be followed by a new exact isolated-runtime build
and a commit-bound authorization bundle before any simulation wave starts.

The implementation commit retained on `main` is:

```text
7b7c47204801032e5eb4fe6c9fd332aaaedead43
```

The package is `rqrgibbs 0.1.0.9017`, the simulation schema is
`rqrgibbs_dlm_main_simulation/1.0.0`, and both execution flags are false.

## Output-15 import

The review branch
`chatgpt-pro/output15-audit-20260724` was verified at
`ba37b56b7027cb954a9b74753389c58373261b8e`. It differed from the declared base
by exactly the seven permitted review files. Their final-byte hashes passed.

The design contract contains:

- 208 incidence rows;
- 89 included and 119 explicitly omitted rows;
- 43,800 maximum software calls;
- 49,200 maximum logical fits;
- 38,400 standard MCMC chains;
- 2,538 additional preselected sentinel chains; and
- 40,938 total maximum MCMC chains.

No disposable performance pilot was introduced. The preselected four-chain
sentinels are embedded in the confirmatory run and remain in its evidence
contract.

## Implemented study machinery

The implementation adds a versioned confirmatory config, a shared simulation
library, a fail-closed runner, a process-group monitor, a deterministic wave
launcher, collection/audit modes, and focused tests.

The runner implements:

```text
preflight
oracle-reference
sentinel-core
execute-confirmatory
collect
audit
```

The full L'Ecuyer-CMRG ledger contains 388,957 unique complete states. Data,
initialization, fitting, forecasting, sentinel selection, and 20 future
subreplications use separated streams or substreams. Sentinel selection occurs
before data realization.

The maximum wave plan contains 8,400 replication tasks. Sentinel work precedes
standard work within each batch. Sentinel waves use no more than eight workers;
standard waves use no more than 32 workers. Every worker uses one declared
numerical thread.

The collection contract requires exact task-set equality, recursive byte and
file-count verification, no symlinks, common source/runtime/config/incidence
and seed bundles, unique replication IDs, and complete failure accounting.
Failures remain in the intention-to-run denominator. A failed four-chain cell
or wave prevents later waves; retries and reseeding are prohibited.

## Statistical scope preserved

The implementation preserves the generalized-Bayes interpretation:

- the RQR update is loss based, not an ordinary response likelihood;
- pseudo-AL augmentation is applied to the interval-loss construction;
- RQR root draws are not posterior-predictive response draws;
- conditional-mean interval roots, realized future roots, and generated future
  responses are distinct objects; and
- response-distribution scores are not attributed to RQR unless a separate
  response-simulation contract is explicitly introduced.

The canonical scenarios include static Gaussian, local-level Gaussian,
local-level skewed, stochastic seasonal, heteroscedastic heavy-tailed, and
root-alignment cases. The stochastic harmonic seasonal component uses the
declared innovation variance. Paired scenarios share only their intended
state-driving streams; response streams remain separated.

Comparator execution uses isolated CRAN source packages:

```text
exdqlm 1.1.0: reduced-AL DQLM, dqlm.ind=TRUE
quantreg 6.1: rq(..., method="br")
```

Neither comparator is loaded, compiled, or installed from the protected
exdqlm checkout.

## Oracle corrections discovered during validation

Three issues were found by executing the exact reference path.

First, Student-t and Gaussian-plus-t mixture truncated moments were replaced by
analytic first/second moment formulas, with explicit infinite-boundary
handling and numerical-integration cross-check tests.

Second, endpoint equality across two independent optimizations was removed as
a blocking high-precision gate. In a flat population-risk basin, endpoints can
differ by approximately `1e-8` while the attained objectives agree to machine
precision. Objective agreement, root-equation residuals, coverage residuals,
and uniqueness remain blocking; the endpoint difference remains reported.

Third, location-scale equivariance now compares the public transformed
endpoints with the transformation of roots returned by the same root-equation
solver. The previous test compared two independent optimizer paths at an
unjustified `1e-12` endpoint tolerance.

These were reference-certification defects, not changes to the RQR Gibbs
target.

## Resource-monitor correction

The numerical contract and OS telemetry are now separate.

All of the following remain fixed at one per worker:

```text
OMP_NUM_THREADS
OPENBLAS_NUM_THREADS
MKL_NUM_THREADS
VECLIB_MAXIMUM_THREADS
NUMEXPR_NUM_THREADS
RCPP_PARALLEL_NUM_THREADS
```

`ps`/NLWP counts every operating-system thread, including non-numerical helper
threads. Ordinary workers therefore have a hard sampled process-group envelope
of two OS threads. The oracle-reference stage has an envelope of four because
toolchain recording briefly invokes `R CMD config` helper processes. The final
reference run observed only two threads. These envelopes are recorded
separately and do not authorize numerical parallelism.

The monitor remains fail closed for wall time, process-group RSS, and the
mode-specific sampled thread envelope. It traps termination signals, drains
the process group, writes telemetry atomically, and has an injected
zero-thread-ceiling failure test.

## Exact reference evidence

The final isolated runtime was built from:

```text
source commit:
  7b7c47204801032e5eb4fe6c9fd332aaaedead43
application tree:
  29f938a8359e0c8bf23c41584f91c0b1fd38e25b
runtime tree:
  608e59fd99c9f16e13d1dd9965d599a34e39e7dbdfbeca024ecd3221567c90c2
attestation schema:
  rqrgibbs_runtime_attestation/5.0.0
attestation SHA-256:
  d07343d2f2f77fc17d34401e6a42a83c6de97e93276614ba96c9812897ae7598
```

The exact preflight passed 22/22 gates in 116 seconds. It observed one process,
one thread, and 542,492 KiB sampled peak RSS.

The exact oracle/reference stage passed 15/15 gates in 109 seconds. It observed
one process, two OS threads, and 791,388 KiB sampled peak RSS. Its configured
reference envelope was four threads and 1,572,864 KiB RSS.

The reference includes:

- four innovation families at 80% and 90% coverage;
- coverage and moment root-equation residuals;
- unrestricted/profile objective agreement;
- high-precision objective agreement;
- location-scale equivariance;
- all canonical DGP constructions;
- two exact serialized DGP reproductions;
- actual isolated exdqlm and quantreg fits;
- dependency-runtime digests; and
- the compiler, BLAS/LAPACK, package, platform, and R toolchain.

The compact evidence is in
`docs/audits/rqr_dlm_output15_reference_evidence_20260725/`. Heavy ledgers,
runtime builds, and generated outputs remain ignored.

## Repository validation

The following passed at `rqrgibbs 0.1.0.9017`:

- environment smoke;
- full native R/C++ FFBS, model, oracle, and sampler tests;
- all bounded, main-study, and confirmatory contract tests;
- `R CMD check --no-manual` with `Status: OK`;
- main article build, 9 pages;
- supplement build, 10 pages;
- no TeX warning, undefined-reference, overfull, or underfull diagnostic;
- direct execution fail-closed, with no output directory; and
- wave execution fail-closed, with no output directory.

## Protected repositories

Read-only final states were:

```text
exdqlm reference
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453
  clean: yes

Q-DESN reference
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
  clean: yes
```

No protected checkout was mutated.

## Execution decision

No confirmatory fit was launched. Both execution flags remain false, and
direct/wave attempts stop before publishing output.

After an independent go decision, the next sequence is:

1. create one flag-only authorization commit directly on top of
   `7b7c47204801032e5eb4fe6c9fd332aaaedead43`;
2. rebuild and attest an isolated runtime at that authorization commit;
3. rerun/bind the exact preflight and reference bundle as required by the
   authorization schema;
4. create the reviewed authorization bundle;
5. launch the complete sentinel-first wave plan under a detached supervisor;
6. stop after any failed sentinel cell, worker, or wave;
7. perform health checks from immutable status, process-group, resource, and
   artifact manifests; and
8. collect/analyze only when the authorized task set is complete.

The matched main run—not another pilot—is the intended next execution.
