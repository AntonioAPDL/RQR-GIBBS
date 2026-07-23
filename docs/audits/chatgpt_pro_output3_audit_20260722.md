# ChatGPT Pro Output 3 Audit and Implementation Resolution

Date: 2026-07-22
Repository: `AntonioAPDL/RQR-GIBBS`
Base commit audited: `68b049ce4bd751b434bca581786ee2148c21c349`
Pro output: local-only `Chatgpt_output3`
Pro output SHA-256: `e8313ef4dc41965d9f73506cdc5f85a0d13c8cdcff6b8c9b5fd2b6fd083b05b6`
Implementation commit: the Git commit containing this report

## Scope and decision

The Pro report was read in full and checked against the R implementation, the
C++ FFBS kernel, generated interfaces, native tests, manuscript, supplement,
local literature inventory, and the pinned exdqlm reference checkout. The
protected exdqlm and Q-DESN repositories were read only.

The central mathematical assessment in Output 3 is correct. The fixed-design
target, fixed-`W` DLM target, frozen-template target, partially collapsed Gibbs
order, and shared component-scale extension remain coherent as generalized
Bayes constructions. The adaptive conditional-discount mode remains a working
sequential algorithm rather than an exact Gibbs kernel for one fixed joint
target.

Eight residual implementation/documentation findings were actionable. All
eight were addressed in this patch series. No production simulation was
launched. The project is ready to freeze a bounded validation protocol, but it
is not yet ready to promote simulation or application evidence.

## Source reconciliation

| Source | State used in this audit |
|---|---|
| RQR-GIBBS | `main` at base commit `68b049ce4bd751b434bca581786ee2148c21c349`; origin was identical before editing |
| exdqlm reference | clean `feature/rqr-desn-readout-20260716` at `dffb71ee70b597d6a716ee74be1cbc99731cd453` |
| Q-DESN article reference | clean local `main` at `f9f22804eff3871bb5350c8add04b7c9f4d4957b`; not mutated |
| RQR introduction paper | Pouplin et al. PDF in the local-only literature inventory |
| Uploaded `calib.pdf` | Pourmohamad, Richardson, and Sansó, *Calibrated Bayesian Nonparametric Tolerance Intervals*, not Syring and Martin |
| Syring--Martin paper | a distinct local-only PDF, `SyringMartin2019GeneralPosteriorCalibration__calibrating-general-posterior-credible-regions.pdf` |

The calibration-source correction is independently consistent with arXiv
record `2603.10924` for Pourmohamad, Richardson, and Sansó and DOI
`10.1093/biomet/asy054` for Syring and Martin. The two papers must not be
treated as the same uploaded source.

## Mathematical audit

### Confirmed without a model change

1. The loss residual is
   `e_t=(y_t-eta_1t)(y_t-eta_2t)`, and the check loss targets interval roots,
   not a response density.
2. At distinct interior roots under endpoint continuity, the population score
   equations imply target conditional interval coverage and the stated first-
   moment balance. With endpoint atoms, the open/closed coverage inequality is
   the correct subgradient statement. Coincident roots target a conditional
   mean under a finite second moment.
3. Fixed rate, normalized learned scale, and pure learned scale are three
   different generalized-posterior targets. In fixed mode, `learning_rate` is
   exactly `omega_R` and does not depend on `s_L`.
4. The pseudo-AL normal-exponential identity, GIG `(p=1/2,a,b)` conditional,
   exact `b=0` Gamma limit, and reciprocal inverse-Gaussian parameter order are
   correct.
5. The normalized fully augmented `lambda` conditional has shape
   `a_lambda+3n/2`; the pure target has shape `a_lambda+n/2`. The implemented
   collapsed update and complete latent-scale refresh occur in the valid order.
6. The root-specific pseudo-observations, filter recursion, backward gain,
   `G_{t+1}` indexing, Cholesky orientations, and corrected use of one repaired
   `R_star` are correct in R and C++.
7. The two-time mixed-derivative argument establishes generic incompatibility
   of the advertised pair of adaptive Gaussian full conditionals. Its stated
   degeneracies and limited scope are appropriate.
8. For shared component scales, the inverse-Gamma shape increment `T d_j`,
   time-zero conditional, quadratic-form orientation, and Gibbs order are
   correct.

### Interpretation limits retained

- `lambda` is not a response variance or an automatically calibrated learning
  rate.
- Training-derived `s_L` and training-derived frozen discount templates are
  empirical-Bayes choices and must be frozen and disclosed.
- Root or endpoint draws are not posterior predictive response draws.
- Restricted regression and state-space classes imply projected/pathwise score
  equations, not universal pointwise conditional coverage.
- No new finite-sample RQR coverage theorem is claimed.

## Finding-by-finding implementation resolution

### F1: fail mode allowed a small eigenvalue projection

Status: fixed in R and C++.

`numerical_policy="fail"` is now threaded explicitly into every covariance
draw. Exact positive-semidefinite covariances with zero eigenvalues may use the
eigendecomposition without being counted as repairs. Any negative eigenvalue,
including one inside the former `1e-10` tolerance, stops instead of being
projected. `record_repair` retains the bounded projection and records the clamp
count. A dedicated internal C++ diagnostic wrapper gives the native tests a
direct, deterministic regression fixture.

### F2: frozen-template construction lacked a complete repair ledger

Status: fixed.

`rqr_freeze_discount_template()` now stores one row per alteration with the
same fields used by FFBS:

```text
stage, time, strategy, jitter, relative_jitter,
min_eigenvalue, matrix_scale, clamped_eigenvalues
```

The stage is `discount_template_filter_covariance`. Legacy time and jitter
vectors are derived from the ledger for compatibility.

### F3: fixed-design precision repairs lacked policy and eligibility metadata

Status: fixed.

`rqr_mcmc_fit()` now has the same `fail`/`record_repair` contract as the DLM
sampler. Precision-factor information includes absolute and relative jitter,
pre-repair minimum eigenvalue, matrix scale, and clamp count. Fit objects store
the complete repair rows, repair count, numerical exactness, target/numerical
eligibility, reproducibility eligibility, and final promotion eligibility.
The DESN adapter passes these controls through explicitly.

### F4: public draw controls silently truncated fractional values

Status: fixed and broadened consistently.

Fixed-design MCMC and VB posterior-draw methods now use the shared strict
scalar-integer validator for `nd` and `seed`. VB fit controls use the same
validation for seed, iteration count, and retained approximation draws. `NA`
or fractional draw counts no longer mean “all” or silently truncate.

### F5: unknown Git status was treated as clean

Status: fixed through the explicit-run-manifest route.

Git command success is now recorded separately from command output, so an
empty successful status means clean while an unavailable or failed status
remains `NA`. Provenance includes commit/status availability, dirty state,
expected commit, expected-commit match, completeness, and reproducibility
eligibility.

An expected commit must be a full 40-character SHA supplied through
`provenance_control`. This avoids pretending that an installed package outside
a checkout has a verified source commit. Promotion now requires:

1. a declared exact target;
2. zero numerical repairs;
3. complete provenance;
4. a clean detected checkout; and
5. equality with the explicitly expected commit.

Exploratory fits can still run without an expected commit, but they cannot be
marked promotion eligible.

### F6: continuation did not enforce its same-environment claim

Status: fixed.

The fit/checkpoint schema is now `rqrgibbs_fit/1.1.0`. Continuation always
stops on schema, data-digest, or model/evolution-matrix-digest mismatch. It also
stops by default when package version, R version, platform, compiler,
BLAS/LAPACK, dependency versions, backend, or detected source commit differ.
`allow_environment_mismatch=TRUE` is an explicit portability escape hatch; it
warns that bitwise continuation is not claimed. The saved RNG and complete
Markov state remain the continuation source.

### F7: component-scale forecasts ignored posterior scale draws

Status: fixed without changing the statistical target.

`rqr_forecast_roots()` accepts exactly one of:

- explicit `W_future`, preserving the prior conditional forecast contract; or
- `component_templates_future`, for a component-scale fit.

In the second route, each selected posterior `q_j` draw is combined with the
fixed future `Q_jt` templates to construct draw-specific `W_t`. The result
returns the selected fit indices, scale draws, evolution mode, and numerical
repair diagnostics. It remains a future root-state draw, not response
simulation.

### F8: fixed-rate documentation gave `s_L` an incorrect role

Status: fixed.

Source comments, generated Rd files, READMEs, manuscript, and supplement now
state that fixed `learning_rate` is `omega_R` and is unaffected by `s_L`.
Learned modes use `omega_R=lambda/s_L`, and their target strings include the
division by `s_L`.

## Additional validation added

- The GIG mean tests now use four estimated Monte Carlo standard errors rather
  than fixed absolute tolerances.
- A moderate-parameter empirical CDF is checked against independent numerical
  integration of the normalized GIG density.
- A separate executed check compared 50,000 native R draws with SciPy 1.13.1's
  `geninvgauss` after the exact scale transformation: mean discrepancy was
  `-1.50` Monte Carlo standard errors, KS statistic `0.00557`, and KS
  `p=0.0894`. This is supporting evidence, not a CI p-value gate.
- Component-template materialization is checked deterministically.
- Direct R/C++ eigen-projection fixtures check fail, record, and exact-PSD
  behavior.
- Continuation tests cover exact split-chain identity, schema rejection,
  data-digest rejection, environment rejection, and the explicit warning
  override.
- Unknown Git status is tested as distinct from clean status.

## Validation evidence

All commands were run on Jerez under R 4.5.3.

| Gate | Result |
|---|---|
| `make smoke` | pass |
| native package install and focused tests | pass |
| `make package-check` | pass, `Status: OK` |
| `make pdf` | pass, 8-page article |
| `make supplement` | pass, 9-page supplement |
| `make test-exdqlm-rqr` | pass at pinned exdqlm commit |
| `make literature-manifest` | pass, 18 local PDFs inventoried |
| R/C++ direct numerical-policy regression | pass |
| dense Gaussian FFBS reference | pass as part of native tests |
| exact same-environment continuation | pass as part of native tests |
| independent SciPy GIG comparison | pass as supporting diagnostic |

Generated PDFs, TeX logs, package tarballs/check directories, compiled objects,
literature manifests, and Pro handoff files remain ignored. No fitted model or
simulation output is tracked.

## Residual risks and deliberate deferrals

These items are not silently reclassified as solved:

1. Same-data learned `lambda` remains a declared hierarchical convention, not
   an operational calibration theorem. Fixed-rate sensitivity remains primary.
2. A collapsed-versus-fully-augmented learned-scale reference-chain comparison
   remains part of the bounded pilot. The conditional formulas and update order
   are verified, but no long invariant-distribution experiment was added to the
   routine package check.
3. General singular multivariate state systems are not supported by every
   fail-fast Cholesky path. The first pilot should use positive-definite
   evolution templates except for dedicated tested limits.
4. Weak identification between loss scale and evolution scales, small
   pseudo-design rows, and root-path mixing require multiple-chain diagnostics
   under the frozen pilot.
5. No matched RQR/RQR-DESN/RQR-DLM simulation or application evidence exists
   yet. The retired Muscat run remains non-promotable.
6. GitHub-hosted CI evidence is still absent at the time of this local audit.
   The complete local Jerez gate is recorded above; remote CI should be added
   only with a pinned dependency-install contract for the exdqlm reference.

## Go/no-go recommendation

Go for freezing the bounded validation protocol and its small fixtures. Do not
go for a production simulation, adaptive-discount exactness claim, promoted
learned-scale result, or response-predictive interpretation. The bounded pilot
must require the exact expected commit, a clean checkout,
`numerical_policy="fail"`, zero repairs, fixed seeds, MCSE-based stochastic
criteria, and explicit
failure recording.
