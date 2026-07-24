# Copy-paste-ready Codex handoff after ChatGPT Pro Output-15

Authenticate this handoff and the accompanying Output-15 deliverables against the exact review branch before changing source. Modify only `AntonioAPDL/RQR-GIBBS`. Do not modify, compile from, install from, or load from the protected exdqlm or Q-DESN source checkouts.

## Reviewed states

```text
implementation/reference source:
  6ba47d1d686e7f47d90bf3110fbbe77f8da96fee

compact evidence/reconciliation:
  a5a08811912d7175bbbcec98e8f8af254fd51f51

prompt-only direction:
  da7e2125f9bed8041a0f9e4b05f2ac17cf9c07fd

Output-14 review:
  2be17bd5710e62168970577796c8ddc1872ffde6

protected exdqlm:
  dffb71ee70b597d6a716ee74be1cbc99731cd453

protected Q-DESN:
  f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

## Output-15 decision

```text
bounded 24-fit evidence:
  accepted; do not rerun

schema-0.2 design grade:
  B+

optimized design grade:
  A- after blockers

main-run implementation:
  GO TO IMPLEMENT WITH EXACT CORRECTIONS

confirmatory execution:
  NOT AUTHORIZED IN THIS PASS

standalone disposable performance pilot:
  not required

CAVI/ELBO and RQR-DESN:
  deferred
```

The exact design is not prose-only:

```text
chatgpt_pro_output15_final_design_matrix_20260724.csv
  208 rows
  89 included
  119 explicit omissions

chatgpt_pro_output15_run_budget_20260724.csv
  initial, central, and maximum fit/chain/forecast/resource counts

chatgpt_pro_output15_launch_gates_20260724.csv
  machine-evaluable implementation, authorization, batch, and promotion gates
```

Implement those contracts without executing any confirmatory fit.

## Mandatory implementation corrections

### 1. Correct and version the DGPs

#### Trend-seasonal pair

Replace the deterministic-only seasonal location with an actual two-state harmonic component:

```text
gamma_t = R(2*pi/12) gamma_{t-1} + e_t
e_t ~ N(0, 0.002 I2)
location seasonal contribution = gamma_t[1]
```

Use the identical training/future state streams, initial law, deterministic scale path, and state parameters for the Gaussian and log-normal pair.

#### Independent-root sensitivity

Freeze:

```text
L0 = -2
U0 =  2
q_lower = q_upper = 0.001
minimum separation = 0.10
reference coverage = 0.80
```

Recover one `(mu_t,s_t)` path at the reference coverage and derive all other coverage roots from the same response law.

#### Future states and oracles

Generate training state innovations and each future state/response subreplication from distinct full RNG streams. Store separately:

```text
oracle_conditional_mean_root
realized_root_path[future_subreplication,horizon]
generated_future_response[future_subreplication,horizon]
```

Do not set the first two equal.

Apply strict integer validation before converting replication, horizon, batch, or count inputs.

### 2. Replace the collision-prone seed mapping

Do not derive operational seeds from the first seven SHA hex digits.

Implement:

```text
RNGkind("L'Ecuyer-CMRG")
master seed = 2026072401
canonical sorted task-key ledger
one full .Random.seed state per task via nextRNGStream
future subreplications via nextRNGSubStream
SHA-256 digest of every complete state
exact unique state-digest gate
```

Enumerate all data, state, error, future state, future response, method, initialization, forecast, oracle, and sentinel keys before authorization.

### 3. Replace data-driven tuning by frozen settings

Set training-validation candidate fits to exactly zero.

```text
component q prior: IG(shape=2.5, rate=0.025)
common q prior:    IG(shape=2.5, rate=0.025)
fixed-design ridge variance: 25
rolling empirical window: 100

discount profiles:
  local level: 0.95
  trend-seasonal: trend 0.98, seasonal 0.95
  trend-regression: trend 0.98, regression 0.90
  break/regression stress: level 0.98, regression 0.90
```

Rates 0.5, 1, 2 and learned normalized lambda are distinct methods, never selected using test coverage.

### 4. Implement the exact incidence matrix

Import or reproduce the exact 208-row matrix and freeze its SHA-256 in the versioned config. Require exact unique row/set equality in preflight and collection.

The optimized grid has 16 scenario-coverage cells, 13 method families, 89 included rows, and 119 explicit omissions. Do not silently add a method to an omitted cell.

### 5. Harden oracle references

For promotion-grade oracle files:

```text
profile grid >= 1601
adaptive refinement of every candidate basin
independent unrestricted midpoint/log-width multistart
independent higher-precision objective check
coverage residual <= 1e-8
moment residual <= 1e-7
objective gap <= 1e-8
estimated numerical error clearly labeled nonrigorous
```

If uniqueness is unresolved, disable endpoint RMSE and use excess risk/distance to the minimizer set.

### 6. Complete comparator reference tests

Build exact isolated runtimes from the pinned CRAN tarballs and freeze full dependency-library manifests.

For exdqlm 1.1.0, add a real tiny lower/upper fit and forecast. Freeze:

```text
dqlm.ind = TRUE
init.from.vb = FALSE
fix.sigma = FALSE
sig.init = 1 after standardization
n.burn/n.mcmc from the frozen schedule
matched FF/GG/m0/C0/df/dim.df
raw lower/upper outputs retained
ordering only for interval metrics
```

Record the complete formals/defaults digest. Do not use exAL gamma learning, LDVB, or response-predictive draws.

For quantreg 6.1, add a real tiny fit with `rq(method="br")`, raw endpoint checks, and ordered interval checks.

### 7. Implement the frozen schedules

```text
component/frozen/common/true-W/fixed-rate dynamic RQR:
  burn 1000; retain 2000; thin 1

learned normalized-rate dynamic RQR:
  burn 1500; retain 3000; thin 1

dynamic AL-DQLM endpoint:
  burn 1000; retain 2000; thin 1

fixed-design RQR:
  burn 500; retain 1500; thin 1
```

Standard fits use one chain/model. Dynamic quantile intervals have two endpoint models. Preselected sentinel model tasks use four chains.

Standard initialization uses the empirical central interval. Freeze sentinel profiles A-D exactly as specified in the audit. No extension, reseed, threshold change, or retry is permitted.

### 8. Implement precision batches and embedded sentinels

Core DGPs:

```text
initial 200
batch 100
maximum 600
```

Sensitivity/T variants:

```text
initial 100
batch 50
maximum 300
```

Generate 20 independent future subreplications, H=20. The DGP is the batch stopping unit; methods and coverages remain paired.

Select two sentinel replications per batch and MCMC method before data generation. Capped true-W/rate methods receive two sentinels total. A sentinel failure stops its scenario-coverage cell. Passing sentinels remain final-study observations.

Use the exact maintained and within-chain gates in the launch-gates CSV.

### 9. Implement the fail-closed runner

Required modes:

```text
preflight
oracle-reference
sentinel-core
execute-confirmatory
collect
audit
```

Both execution flags remain false throughout this implementation pass.

Reuse the hardened bounded-run infrastructure:

- fresh output directory;
- atomic temporary write, read-back validation, hash, and rename;
- rollback fault tests;
- process-group/cgroup monitoring;
- one thread per worker;
- exact failure ledger and denominator;
- recursive file-set/hash equality;
- no full standard fit retention;
- ignored, hashed sentinel chain/checkpoint objects;
- compact per-replication results sufficient to reproduce every aggregate.

### 10. Implement exact analysis rules

Replication is the independent Monte Carlo unit. Implement aggregate and horizon MCSE formulas from the audit. The batch decision uses precision only.

Coverage qualification:

```text
90% t interval for coverage error wholly inside [-0.02,0.02]
```

It is not a stopping rule and is not proof of exact coverage.

A narrower-width statement requires both methods coverage-qualified and a paired 95% width-difference interval supporting the direction.

Separate:

```text
RQR target-aligned endpoint error
quantile target-aligned endpoint error
cross-target distance
conditional-mean root forecast error
realized future-root error
generated-response coverage
```

No response log score or CRPS is permitted for RQR.

## Implementation validation required

Before creating the next Pro prompt, run:

```text
source parse and contract tests
complete native R/C++ suite
R CMD check --no-manual
main and supplement builds
DGP moment/state/scale/separation tests
full RNG ledger collision test
oracle high-precision references
actual exdqlm/quantreg tiny fits and forecasts
two-replication byte reproduction
atomic/rollback/process-monitor fault tests
exact incidence and budget recomputation
fail-closed sentinel and confirmatory negative tests
```

Return:

1. implementation and evidence commits;
2. complete changed-file list;
3. exact config/incidence/budget digests;
4. DGP and oracle reference results;
5. comparator runtime and fit-reference evidence;
6. complete seed/sentinel ledgers;
7. runner modes and empty/sample artifact schemas;
8. resource preflight;
9. all validation results and compact hashes; and
10. a copy-paste-ready next Pro prompt.

Do not run a standalone pilot or any confirmatory replication. Do not authorize CAVI/ELBO or RQR-DESN.
