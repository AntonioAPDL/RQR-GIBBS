# Static version-3 mixing reconciliation

## Scope

This audit concerns the single frozen fixed-design illustration with content
0.95 and standardized asymmetric-Laplace innovations at source index 0.80. It
is not a repeated-sample simulation study. The posterior object is the
loss-based generalized posterior for two interval roots; it is not a response
likelihood or a posterior-predictive response distribution.

The first exact version-3 execution used source commit
`2c6a783488acb1d3ea5861c21f02e1f2e38b0a5f`. Preflight, 24 independent
reference gates, and both representative benchmarks passed. Execution then
stopped as designed after the first family--target cell,
`fixed_design/RQR`, failed its four-chain diagnostics and recovery contract.
No later fixed-design or DLM cell was launched.

## Failure diagnosis

All four RQR workers completed normally:

- 6,000 retained draws per chain;
- two complete exact Gibbs transitions per recorded iteration;
- zero numerical repairs;
- exact-joint-target and promotion-eligible provenance;
- no resource-limit failure.

The pooled cell nevertheless had maximum rank-normalized R-hat 1.223, minimum
bulk ESS 13, minimum tail ESS 105, and maximum MCSE/posterior-SD ratio 0.314.
The posterior-mean endpoint RMSE was 0.234 times the population-oracle mean
width, and the fitted high/low width contrast was too small.

The four chains separated into two pairs. Chains 1--2 had normalized endpoint
RMSE near 0.244 and fitted width contrast near 0.88; chains 3--4 had RMSE near
0.225 and fitted contrast near 1.01. None recovered the exactly representable
population curve adequately. More decisively, the empirical RQR loss at each
chain's posterior-mean curve exceeded the loss at the population-oracle curve
by 59--77 units. The oracle curve incurred only about 2--3 additional units of
ridge penalty. Hence the failed chains were not identifying a scientifically
meaningful prior-dominated compromise. They were trapped far from a much
higher generalized-posterior region.

This evidence rules out four tempting but unjustified responses:

1. relaxing the convergence or recovery thresholds;
2. reducing the heteroscedastic contrast to make the plot easier;
3. reducing the eight-column spline even though it represents the truth
   exactly; or
4. interpreting the failure as an inability of root regression to represent
   heteroscedastic width.

## Candidate corrections

Likelihood-tempered replica exchange is an exact cold-target option for
zero-tilt ridge RQR. It was not selected here because the same mechanism is
currently unavailable for nonzero fixed tilts, while the illustration requires
one comparable workflow for RQR, ET, and SH. It would also multiply the RQR
cost without addressing the common initialization problem.

Initializing every chain at the population endpoint curves was also rejected
as the publication workflow. Such starts are legitimate for a known-DGP
mechanical check, and a bounded audit confirmed that they reach the intended
region, but they make the apparent recovery depend unnecessarily on the
ground-truth curves.

The selected correction is a deterministic moment pilot constructed from the
observed response and the declared innovation law:

1. estimate the spline conditional mean by least squares;
2. project the absolute residuals on the same spline basis;
3. divide that projection by the known standardized first absolute moment,
   `E|Z| = 0.7332205895378953`, to obtain a scale pilot;
4. combine the fitted mean and scale with the target's known standardized
   population endpoint anchors; and
5. form centered, narrow, wide, and shape-stress starts.

The pilot uses the population endpoint *constants* already implied by the
fixed oracle tilt, but it does not use the population endpoint curves,
population mean curve, or population scale curve. Initialization changes only
the starting state. The learning rate, loss, fixed tilt, ridge prior, and every
retained-draw target remain unchanged. Ordinary RQR is normalized to exact
`mean_tilt = 0`, removing solver-scale numerical residue from its target
metadata.

## Bounded reconciliation evidence

The correction was first assessed with four chains retaining 2,000 draws after
1,000 warm-up iterations. Each recorded draw composed two complete Gibbs
transitions. These were diagnostic runs under ignored local storage, not
manuscript evidence.

| Target | Maximum R-hat | Minimum bulk ESS | Minimum tail ESS | Maximum MCSE/SD | Recovery summary |
|---|---:|---:|---:|---:|---|
| RQR | 1.0082 | 1,083.8 | 2,050.3 | 0.0303 | normalized endpoint RMSE 0.059--0.062; width-contrast error 0.157--0.166 |
| ET | 1.0093 | 1,080.1 | 1,698.0 | 0.0311 | normalized endpoint RMSE 0.066--0.067; width-contrast error 0.190--0.193 |
| SH | 1.0173 | 546.3 | 966.0 | 0.0429 | recovery passed, but the deliberately short diagnostic schedule did not satisfy the final R-hat/ESS thresholds |

The short SH result supports retaining the previously frozen 6,000-draw
schedule; it does not justify a shorter schedule or a diagnostic waiver. The
full exact-source execution must still pass all final gates before any result
is promoted.

## Implementation and validation contract

The revised workflow must:

- freeze all four static profile definitions in the versioned JSON config;
- verify the innovation absolute-moment receipt;
- require positive initial width at every design point;
- record twelve distinct target/profile initialization digests;
- publish `static_initialization_audit.csv` in compact evidence;
- bind initialization through the config, DGP, target, and worker-contract
  digests;
- retain the original strict MCMC, recovery, heterogeneity, resource, and
  provenance gates; and
- preserve the fail-closed cell order.

The required validation order is source tests, exact-runtime preflight,
independent reference checks, representative benchmark, and only then the
complete 27-chain illustration. A successful run may replace the version-2
figures only after compact evidence packaging and manuscript-build checks.

## Decision

The original exact run is a valid fail-closed diagnostic and is not eligible
for publication. The data-derived moment initialization is the smallest common
target-preserving correction for all three static tilts. Replica exchange,
easier population functions, fewer spline columns, and relaxed gates are not
adopted. Final promotion remains conditional on a new exact-source run passing
the complete unchanged scientific and computational contract.
