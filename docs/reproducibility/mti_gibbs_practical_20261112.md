# Practical Fixed-Target MTI Posterior Assessment

## Purpose and Existing Evidence

The original Gaussian/GIG campaign at
`44afea30784540ae913b0a558a5c572790fd7493` completed 174 datasets and 696 chains.
Twenty datasets satisfied all four-variable diagnostic requirements. There
were no failed executions or numerical repairs. The prepared full comparison
did not start. This new study leaves the original observations, selected q
and delta, chain records, configuration, and assessment unchanged.

The current article reports genuine finite-iteration ECM calculations. These
calculations are not posterior predictive draws, Gibbs summaries, or proven
global modes. The present computation study neither replaces them nor
transfers their repeated-sampling tolerance calibration to new endpoints.

## Posterior and Two Samplers

For fixed sample y, fitted content q, and response-scale tilt delta, the
negative log generalized posterior, up to an additive constant, is

```text
J(a,b) = omega * sum_i {rho_q((y_i-a)(y_i-b))
                       - q*delta*(a+b-2*y_i)}
         + (a*a+b*b)/(2*tau2).
```

The study retains omega=1 and tau2=10,000 from the archived calculation.
Neither q nor delta is reoptimized during sampling. The direct DP content
assessment used to select the archived target is separate from these root
draws. Target-selection uncertainty is outside this conditional calculation.

The original algorithm augments product residuals by latent scales and
alternates Gaussian root and GIG scale conditionals. Its installed runtime
and frozen source remain pinned to the original commit. Exact continuation
is verified by comparing split and uninterrupted runs at zero and nonzero
tilt, including their final RNG state. A continuation has no fresh seed or
additional burn-in; saved parameter, prior, latent, and RNG states are
restored. New segments record their parent chain hash and iteration offset.

The experimental intercept-only sampler integrates out the scales and uses
a univariate slice update for each raw root conditional on the other. This
is **marginal slice-within-Gibbs**, not an independent draw from each complete
conditional or the original conjugate Gaussian/GIG algorithm. It uses exactly
the same J and independent Gaussian priors. Ordered endpoints are obtained
only when recording draws. The generic package sampler is unchanged.

### Conditional Calculation

For fixed other root b, sort the observations as z_1,...,z_n. The unpenalized
loss is piecewise affine as a function of a. Its initial slope is

```text
s0 = -q*sum_i(z_i-b) + sum_i min(z_i-b,0) - n*q*delta.
```

The slope increases by abs(z_i-b) at each observation. Hence the conditional
negative log density, up to a constant, is

```text
precision*a*a/2 + omega*{s0*a + sum_i abs(z_i-b)*(a-z_i)_+}.
```

It is strongly convex for positive Gaussian precision, including at limiting
and outside-range empirical tilts. Given the current root, sample an
exponential height above its conditional energy. The sublevel set is one
bounded interval. Prefix sums locate its boundary segments, and stable
quadratic root formulas give both endpoints. A uniform draw on this slice
defines an invariant conditional transition; alternating the two updates
preserves the specified root posterior. This does not establish rapid mixing.

The implementation is vectorized R. A short check took approximately four
seconds for one 6,000-cycle chain. A new compiled implementation is not
required before assessing effective samples per second. Numerical assertions
check slice boundaries and finite objectives; failures are recorded rather
than replaced by repaired draws.

Tests compare conditional energies with independent direct product-loss
evaluations, conditional distributions with numerical quadrature, and joint
posterior moments with a two-dimensional density grid in a controlled
example. Actual diagnostic examples also compare conditional quantiles
with independently integrated direct-loss densities. That conditional check
is not a proof that every joint posterior chain has mixed.

## Practical Assessment

The exploratory screen uses R-hat <=1.05, bulk and tail ESS >=100, and
MCSE(mean)/posterior SD <=.10. Preferred diagnostics retain R-hat <=1.01,
ESS >=400, and MCSE/SD <=.05. Neither screen proves convergence.

For every endpoint, width, and midpoint, retain mean, median, SD, R-hat,
bulk/tail ESS, MCSE of the mean, and MCSE of the .025, .5, and .975 quantiles.
Also retain successive-half mean differences. Response-unit reporting
resolution is explicitly configured as 1% of median posterior width. This
is a numerical reporting choice, not a tolerance-confidence threshold.

An individual quantity is classified as adequate for the recorded summaries
only if preferred diagnostics and mean/median/credible-endpoint precision
requirements hold. An exploratory pass without this precision is provisional.
Others remain unresolved. Dataset classification prioritizes both ordered
endpoints and width; midpoint diagnostics are retained. Visual and substantive
review remains necessary, particularly for provisional results. No automatic
rule promotes every exploratory pass into an article result.

The retrospective four-variable sensitivity counts are 20/174 under the
original requirements, 23/174 with R-hat relaxed to 1.05 alone, and 38/174
under the exploratory screen. These counts do not overwrite the old decision.
Only the 18 exploratory-pass/original-fail datasets receive one initial
4,000-draw extension per original chain, with no indefinite doubling.

## Design and Computing Limit

The balanced comparison uses the first five archived simulation replications
in all 72 distribution/design cells, and partition 1 in every application
split scheme for both responses. Identities do not depend on method
performance. There are 370 datasets and 1,480 chains. Union with the complete
174-dataset diagnostic set adds 86 distinct stress datasets, giving 456
datasets and 1,824 chains. Six of the earlier 92 stress examples already
belong to the first five replications. Diagnostic examples are reused once
in that union. All fitting holds the archived target and sample fixed. New
slice chains use distinct deterministic seeds, separate from dataset and
original-chain seeds.

Five Gibbs replications per cell support a limited computation/uncertainty
comparison, not precise tolerance-confidence estimates. The full 72,000-data
ECM validation remains unchanged. A full Gibbs tolerance comparator would
need a declared endpoint summary, its own calibration, and independent
repeated-sampling validation.

Up to 50 single-threaded workers are used, after CPU, memory, and storage
checks. Fifty workers are not fifty physical cores on this server. No new
sampling batches start after the configured two-hour sampling deadline.
Already running finite chains finish and preserve their results, so shutdown
can extend beyond the deadline by a final batch. Assessment and reports can
also finish afterwards. The limit is not a promise that all chains converge
or that every stage completes.

Every chain has a task/sample identity and a SHA-256 completion record.
Completed compatible results are reused, errors remain visible, and missing
datasets remain in progress counts. Final controller status says
`calculation_complete_review_required` or `bounded_run_incomplete`, never
automatically that all posterior results are adequate. Original and new source
commits and runtime identities are recorded separately.

## Commands and Outputs

Use the repository's required private archives and the original installed
runtime under ignored RQR-owned cache paths. Protected exdqlm source checkouts
are neither loaded nor modified. Before/after guards include ignored files.

```bash
make test-mti-gibbs-practical
make preflight-mti-gibbs-practical
bash application/scripts/109_launch_mti_gibbs_practical.sh --dry-run \
  --confirm-article-commit=CONFIRMED_40_CHARACTER_COMMIT --workers=50
bash application/scripts/109_launch_mti_gibbs_practical.sh \
  --confirm-article-commit=CONFIRMED_40_CHARACTER_COMMIT --workers=50
```

Script 109 creates a detached exact-commit source and starts one locked
background controller. It uses the original attested package only for
Gaussian/GIG continuation; the marginal sampler is committed script source,
not a relabeled build of that package. Script 108 records the exact analysis
source and configuration before sampling. Its `--mode=health` reads existing
outputs without starting new chains.

Important outputs under the ignored study directory:

- `manifest.json`, `design.csv`, and `diagnostic_examples.csv`: source,
  settings, task membership, and diagnostic examples.
- `original_assessment/`: practical summaries of unchanged original draws.
- `continuation_equivalence.csv`, `continuations/`, and
  `continuation_assessment/`: verified original-sampler extensions and summaries.
- `independent_conditional_reference.csv`: actual-case quadrature comparisons.
- `slice/`: marginal-slice chains and complete parameter/dataset summaries.
- `width_comparison.csv`: posterior width summaries versus archived ECM,
  explicitly including unresolved diagnostic classifications.
- `marginal_slice_examples.pdf`: compact diagnostic trace report.
- `campaign_status.json`, `controller.log`, and `controller_exit.json`: progress
  or terminal outcomes. No continuous polling process is needed.

Provisional and unresolved summaries remain available for diagnosis; neither
is silently discarded to improve apparent performance. These scripts do not
change the article or dissertation.

## Completed Assessment

The frozen study at `877366f36ba1490f6e75a8f5bac9471a75e41eec` completed
all 456 datasets and 1,824 marginal chains, together with 72 original-sampler
continuation segments. The subsequent [completed numerical review](mti_gibbs_review_20261112.md)
verifies those records, compares posterior and ECM summaries, examines empirical
tilt limits and prior sensitivity, and documents the findings now included
in the manuscripts. The reported tolerance results remain ECM based.
