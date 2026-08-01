# Oracle-tilt illustration protocol, version 3

## Purpose and evidence boundary

This protocol replaces the visually simple version-2 single-data examples
with two more informative, still exactly specified illustrations. It is not a
Monte Carlo simulation study. One frozen fixed-design response and one frozen
dynamic response are reused for the ordinary RQR, equal-tailed (ET), and
shortest-window (SH) targets. The fitted objects are generalized-posterior
interval-root summaries under the RQR loss update. They are not response
likelihood fits and their endpoint ribbons are not posterior-predictive
response bands.

The version-2 evidence remains immutable until every version-3 construction,
reference, resource, MCMC, recovery, and provenance gate passes. Promotion is
atomic: a failed or incomplete version-3 run cannot change the manuscript
figures.

## Audit conclusions and design choice

The prior examples were statistically valid but visually modest:

- the static mean was quadratic and the scale changed only mildly;
- the dynamic model contained only a local level and slope;
- the dynamic innovation scale was constant.

The fixed-design root regression can represent heteroscedastic interval width
provided the endpoint design contains the scale function. The RQR-DLM can also
represent time-varying width because its two root states evolve separately,
but pronounced recurring variation is better supported by an explicit
seasonal state than by asking a local-linear component to approximate four
cycles. An explicit Fourier block is therefore preferable to an unmodeled
seasonal DGP or a higher-dimensional generic trend.

The adopted construction is deliberately exactly representable by each fitted
model. This makes disagreement interpretable as finite-sample or computational
behavior, rather than hidden mean/scale misspecification. It also avoids the
opposite extreme of adding arbitrary flexibility solely to make a plot look
busy.

## Common population law and target contract

Both data sets use content `c = 0.95`, fixed learning rate `lambda = 1`, and an
affinely standardized asymmetric-Laplace source law with index `0.80`. The
index describes the illustrative response innovation law; it is not the
coverage content. If `Z` denotes that standardized law, each response has the
location-scale form

```text
Y_i = mu_i + s_i Z_i.
```

For each target, the innovation-scale lower and upper endpoints are computed
from exact population quantiles and truncated first moments. The observation-
specific tilt is `s_i * delta_Z`. Thus the three model fits use:

- ordinary RQR: the exact zero tilt;
- ET: the exact population equal-tailed recovery tilt;
- SH: the exact population shortest-window recovery tilt.

No Cornish--Fisher approximation, estimated tilt, or automatic tilt selection
enters these fits.

## Fixed-design construction

The design contains `n = 2400` equally spaced points on `[-1, 1]`. This size
was selected before the full grid because a bound one-chain diagnostic at
`n = 1200` narrowly under-recovered the prespecified scale contrast; doubling
the fixed design moved endpoint, high-scale, and contrast errors inside their
strict gates without changing the population functions or fitted model. A cubic
B-spline basis with four fixed internal knots and eight columns defines both
the nonlinear mean and positive scale functions. The population functions are
linear in the raw B-spline basis. The fitted design is a deterministic QR
orthogonalization of that same basis, scaled so that `X'X/n = I`. The frozen
raw-to-orthogonal transform proves exact representability to numerical
tolerance.

The scale function ranges from `0.38` to approximately `0.834`, a ratio of
about `2.19`. Its pattern is not monotone, so the interval width changes across
several parts of the covariate domain. A data-independent prior-predictive scan
selects the largest ridge variance satisfying both a central minimum half-
width and a maximum row-wise half-width. The uniquely selected value is
`tau^2 = 0.25`.

Four chains retain 6,000 draws after 1,500 warm-up transitions. Each recorded
iteration composes two complete exact fixed-design Gibbs transitions. Latent
draws are not retained.

## Dynamic construction

The DLM uses `T = 1200` observations over the physical horizon `[0, 1]` and
contains four states:

```text
local level, local slope, seasonal cosine coefficient, seasonal sine coefficient.
```

The local-linear block uses the exact continuous-time discretization at
`dt = 1/T`. The mean adds one Fourier harmonic with period 300 observations,
so four full cycles appear. Its amplitude is `0.62`. The population seasonal
path is deterministic, while the fitted seasonal state uses the small fixed
innovation covariance `10^-6 I_2`. This regularization leaves the declared
harmonic clearly dominant but keeps the FFBS conditional strictly
nondegenerate under the fail-on-repair numerical policy.

The response scale is

```text
s_t = 0.55 + 0.22 cos(2*pi*t/300 - 0.50),
```

which ranges from approximately `0.33` to `0.77`, a ratio of about `2.33`.
This scale path is representable by the same Fourier state block, so both
endpoint paths lie in the declared four-state model. Two fixed windows omit 22
responses in total; missing observations contribute no loss, latent-scale, or
tilt site.

The local prior/evolution and seasonal initial variance are chosen by frozen,
data-independent prior-predictive rules. Five chains use distinct, recorded
initialization profiles. Each retains 6,000 draws after 2,500 warm-up
transitions using the C++ FFBS backend. Full state paths and latent scales are
not retained because endpoint-ordinate draws and checkpoint states suffice for
the declared diagnostics; this reduces the resource envelope without changing
the Markov transition.

## Deterministic and independent reference gates

Before MCMC, the workflow must pass all deterministic design gates:

1. exact content and target reproduction;
2. eight-column static rank and orthonormal Gram matrix;
3. exact static endpoint projection;
4. exact four-state dynamic endpoint projection;
5. fixed-horizon local-linear covariance invariance;
6. exact seasonal covariance recursion under the frozen innovation variance;
7. full four-state observability;
8. static and dynamic scale floors and ratios;
9. exact missing mask;
10. at least ten expected observations in every rarer endpoint tail;
11. at least two expected rarer-tail observations in every scale quintile;
12. a 27-chain fit plan with unique seeds; and
13. absence of Cornish--Fisher use.

The conditional reference suite contains 24 gates. It checks the eight-
dimensional static Gaussian root conditional by analytic moments and Monte
Carlo standard errors, checks a nonsingular local-linear FFBS conditional
against a dense Gaussian precision calculation, and checks the four-state
seasonal conditional against an independent innovation-coordinate Gaussian
calculation. The latter reference admits positive-semidefinite evolution
covariances without injecting artificial jitter and is applied to the frozen
four-state seasonal model. Both R and C++ smoothers must agree with the
independent means and marginal covariances, omit missing measurements, and
report zero numerical repairs. Separate R and C++ draws must also be finite
and repair-free under the actual seasonal innovation covariance.

## Execution stages and fail-closed controls

The stages are ordered and cryptographically bound:

```text
source tests
  -> exact-runtime preflight
  -> exact-runtime reference-only
  -> representative fixed-design SH and DLM-SH benchmark
  -> six-cell / 27-chain execution
  -> compact evidence packaging
  -> figure and table regeneration
  -> manuscript promotion.
```

Benchmark and execution require a full reviewed source SHA, a verified
isolated-runtime attestation, and explicit confirmation variables. Execution
binds the exact preflight, reference, and benchmark bundles. Each family-target
cell finishes and passes before the next begins. Worker envelopes are written
atomically under ignored output storage and can be resumed only when their
complete contract digest matches.

The monitored wrapper fixes numerical-library thread variables before R
starts, creates a separate process group, samples total RSS/process/thread
counts, distinguishes R processes from provenance helpers, installs signal
and exit cleanup, verifies the process group is empty at closeout, enforces an
eight-hour execution timeout, and checks for at least 20 GiB of free space
before launch. Its RSS telemetry is sampled and is not described as a kernel-
hard peak.

## MCMC, pathology, and recovery gates

Primary diagnostics use maintained `posterior` implementations of rank-
normalized R-hat, bulk ESS, tail ESS, and MCSE. The thresholds are

```text
R-hat <= 1.01
bulk ESS >= 1000
tail ESS >= 1000
MCSE / posterior SD <= 0.05.
```

The estimand set includes global loss and width summaries plus endpoint,
midpoint, and width draws at deterministic covariate/time indices chosen to
cover spline knots, scale extrema, seasonal extrema, regular grid locations,
and missing-window boundaries. DLM cells must also pass R/C++ conditional
parity and remote-path screens. Every fit must use the exact joint target,
incur zero numerical repairs, and satisfy isolated source/runtime provenance.

Recovery gates are descriptive checks for these frozen data sets, not claims
about repeated-sample coverage. They include endpoint RMSE relative to oracle
width, mean-width ratio, endpoint-summary inclusion, normalized endpoint bias,
static edge/center behavior, low- and high-scale local-width RMSE, recovery of
the high/low width contrast, and dynamic seasonal width amplitude and phase.

## Evidence and manuscript promotion

Raw fits remain under ignored `application/outputs/`. The packager accepts only
a complete strict-passing run with 27 worker receipts, six strict-passing
cells, 24 passing reference gates, two passing benchmark cells, exact runtime
binding, wrapper closeout, and recursive hashes. It publishes compact CSV/JSON
evidence to `figures/data/oracle_tilt_c095_v3/`; it never publishes fitted
objects.

Only after packaging passes should the figure generator switch from the
immutable version-2 evidence to version 3. The article captions must then state
the nonlinear spline/heteroscedastic static construction and the seasonal,
time-varying-scale DLM construction. The supplement should report the added
heterogeneity diagnostics. If any gate fails, version 2 remains the manuscript
source and the version-3 result is retained only as an ignored diagnostic run.

## Reproduction commands

From the repository root:

```bash
make test-oracle-tilt-publication-v3

# Set these to one reviewed clean commit and its isolated-runtime receipt:
export RQR_EXPECTED_PRIMARY_COMMIT=<40-character SHA>
export RQR_PRIMARY_RUNTIME_ATTESTATION=<absolute attestation path>

make oracle-tilt-v3-preflight
make oracle-tilt-v3-reference

export RQR_ORACLE_TILT_V3_PREFLIGHT_DIR=<completed preflight directory>
export RQR_ORACLE_TILT_V3_REFERENCE_DIR=<completed reference directory>
export RQR_ORACLE_TILT_V3_BENCHMARK_CONFIRM=YES
make oracle-tilt-v3-benchmark

export RQR_ORACLE_TILT_V3_BENCHMARK_DIR=<completed benchmark directory>
export RQR_ORACLE_TILT_V3_CONFIRM=YES
make oracle-tilt-v3-execute

make oracle-tilt-v3-package-evidence \
  ORACLE_TILT_V3_RUN_DIR=<completed execute directory>
```

Exact output directories and artifact hashes belong in the eventual validation
closeout, not in this prospective protocol.
