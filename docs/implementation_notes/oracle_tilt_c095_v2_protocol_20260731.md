# Version-2 protocol for the 95% oracle-tilt illustrations

## Scope

This protocol governs a replacement of the fixed-design and dynamic-linear
single-data illustrations. It does not govern a repeated-sample simulation
study. The fitted objects are generalized posteriors induced by the RQR loss;
the asymmetric-Laplace distribution below is used only to generate an
illustrative data set and compute population-oracle interval targets. It is not
the response likelihood asserted by the fitted RQR models, and the interval
draws are not posterior-predictive response draws.

The earlier `AL_0.99` evidence remains immutable. Version 2 is promoted only if
its complete staged contract passes; otherwise the manuscript remains
unchanged.

## Frozen population and target contract

The innovation is generated from `AL_0.80(0,1)` and then affinely standardized
to mean zero and variance one. The source-law receipts are

```text
raw mean       -3.75
raw standard deviation  5.153882032022076
standardized skewness   -1.7976169855634085
```

The interval content is `c=0.95`. Ordinary RQR, equal-tailed (ET), and
shortest-interval (SH) tilts are obtained from exact population quantiles and
truncated first moments. Cornish--Fisher approximations are excluded. At the
frozen sample sizes, every family/target cell has at least ten expected
observations in its rarer endpoint tail.

## Fixed-design construction

The fixed design contains 1,200 equally spaced values on `[-1,1]`. The signal
and innovation scale are

```text
mu(x) = 0.35 + 0.85 x - 0.30 x^2,
s(x)  = 0.52 + 0.15 (x+1)/2.
```

The raw quadratic columns are converted to an empirical orthogonal basis with
`X'X/n=I`. Column signs are fixed relative to the raw basis so the transform is
deterministic. This keeps all six endpoint truths in the fitted span while
making the common ridge scale interpretable. The ridge variance is chosen
before observing responses from the frozen candidates
`(0.5,1,2,4,8,16,25)`: use the largest candidate whose 95% prior half-width is
at least 2.5 at the design center and at most 6 over the entire design. The
unique selected variance is 1.

## Dynamic-linear construction

The DLM has 1,200 measurements on the fixed physical horizon `[0,1]`, with
measurement times `t_i=i/T` and an initial state at time zero. Two fixed
physical windows are missing, yielding 22 missing and 1,178 observed values.
The response innovation scale is 0.55.

The latent mean follows the continuous-time local-linear system

```text
d level(t) = slope(t) dt + sqrt(q_level) dB_level(t),
d slope(t) = sqrt(q_slope) dB_slope(t).
```

For `dt=1/T`, its exact discrete transition is

```text
G(dt) = [1 dt; 0 1],
W(dt) = [q_level dt + q_slope dt^3/3, q_slope dt^2/2;
         q_slope dt^2/2,            q_slope dt].
```

The initial and innovation scales are selected from a frozen Cartesian grid by
data-independent prior-predictive constraints. The componentwise-largest
passing values are

```text
C0 = diag(1, 0.25),  q_level = 0.04,  q_slope = 0.09.
```

The covariance propagated to physical time one is invariant, to floating-point
tolerance, when the time grid changes. Five chains use default,
oracle-centered, narrow, wide, and slope-stress starts. The last start applies
a physical-time slope perturbation rather than an index-scale perturbation.

## Staged validation

The workflow has four modes and fails closed between them.

1. `preflight` reconstructs both DGPs, all exact targets, the 27-chain plan,
   tail counts, basis and projection audits, prior selections, missingness, and
   fixed-horizon covariance audit.
2. `reference-only` checks the fixed-design Gaussian coefficient conditional
   against independent moments and checks R/C++ FFBS means, marginal
   covariances, selected cross-time sampled covariances, missing-measurement
   omission, and repair counts against a dense Gaussian reference.
3. `benchmark` runs one complete SH chain per family under the exact isolated
   runtime and verifies elapsed time, artifact size, provenance, zero
   numerical repairs, and deliberately loose recovery/pathology screens. The
   loose screens stop a grossly invalid fit without substituting for the
   strict multi-chain recovery and diagnostic gates used by `execute`.
4. `execute` runs six cells sequentially. Only the chains inside the current
   cell may run in parallel. Every four- or five-chain cell must pass before the
   next cell starts.

Benchmark mode binds exact-runtime preflight and reference manifests. Execute
mode additionally binds the benchmark manifest. Every binding includes the
complete source SHA, configuration SHA-256, runtime tree digest, closeout hash,
and artifact-manifest hash.

## MCMC and recovery gates

The fixed-design cells use four chains with 1,500 warm-up and 6,000 retained
iterations. Two complete exact Gibbs transitions are composed between retained
fixed-design draws. This prospectively frozen thinning-by-transition corrects
the modest local bulk-ESS shortfall found with one transition while preserving
the same target and retained evidence shape. DLM cells use five chains with
2,500 warm-up and 6,000 retained iterations, one complete transition per
iteration, the C++ backend, stored state paths, and stored latent variables.
The numerical policy is `fail`.

Maintained diagnostics are computed for aggregate loss and interval summaries
and for explicit endpoint, midpoint, and width ordinates, including missing
window boundaries. All variables must satisfy rank-normalized
`R-hat <= 1.01`, bulk and tail ESS at least 1,000, and mean MCSE divided by
posterior standard deviation at most 0.05. Every fit must have zero repairs,
an exact fixed-joint target, an exact source/runtime match, and promotion
eligibility. DLM cells additionally require R/C++ conditional parity and no
remote-path pathology.

Recovery is assessed against the known population interval roots. Frozen gates
cover endpoint RMSE relative to oracle width, mean-width ratio, joint inclusion
of both oracle endpoints in pointwise 95% posterior summaries, endpoint bias,
and the fixed-design edge-to-center RMSE ratio. These are validation criteria
for the single frozen illustration, not estimates of repeated-sample coverage.

## Resource and artifact contract

The shell wrapper sets BLAS/OpenMP thread variables before R starts, executes R
in a new process group, samples group RSS/process/thread totals every 0.2
seconds, installs signal cleanup, enforces elapsed and sampled resource limits,
and performs a final process-group sweep. Sampled RSS is telemetry rather than
a kernel-hard memory ceiling. The wrapper recursively hashes local artifacts.

Full worker envelopes remain under ignored `application/outputs/`. Successful
promotion copies only the compact allowlist, monitored-resource records, and
hash manifests into `figures/data/oracle_tilt_c095_v2/`. Runtime paths are
removed, and prior-bundle paths are reduced to basenames. Figure generation
and manuscript editing occur only after compact evidence publication.
