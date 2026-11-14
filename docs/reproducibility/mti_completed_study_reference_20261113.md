# Completed MTI Study: Manuscript and Dissertation Reference

## Statistical Contributions

The tolerance article develops a loss-defined family of interval functionals,
a generalized posterior for its roots, conditionally conjugate augmentation,
Gibbs and ECM computations, and an empirically calibrated tolerance procedure.
The mean-preserving interval is the zero-tilt member of the mean-tilted family.
The residual-product loss originates with Pouplin et al.; the article gives
the attribution and distinguishes this loss from the response likelihood.

This document describes the completed work. It does not revise the dissertation
or the regression/dynamic companion article. The independently reviewed article
integration is commit `a400ecbaa2654b97667cf9a9e39732217c38a8e2`.
Subsequent packaging/documentation commits retain the same numerical results
and manuscript text. Each source package records its own exact commit.

## Where to Find the Derivations

Use the LaTeX labels below rather than copying section numbers, which may
change during dissertation integration. Main and supplement are separate
documents with their own numbering and bibliographies.

| Subject | Source and stable label |
| --- | --- |
| MPI characterization | `main.tex`, `prop:population-characterization` |
| MTI generalization and placement | `main.tex`, `sec:mti-loss` |
| Generalized posterior and conditionals | `main.tex`, `sec:static-theory-statements` |
| Full posterior and propriety proof | `rqr-gibbs-supplement.tex`, `sec:supp-mti-posterior` |
| Gaussian/GIG Gibbs conditionals and algorithm | `rqr-gibbs-supplement.tex`, `sec:supp-mti-gibbs` |
| Implemented marginal root sampler | `rqr-gibbs-supplement.tex`, `sec:supp-mti-marginal` |
| ECM maximizations and algorithm | `rqr-gibbs-supplement.tex`, subsection `ECM Conditional Maximizations` |
| Empirical tilt limits | `rqr-gibbs-supplement.tex`, `sec:supp-mti-empirical-limits` |
| DP fixed-interval content law | `rqr-gibbs-supplement.tex`, `sec:supp-direct-dp-check` |
| Calibrated interval selection | `main.tex`, `sec:mti-tolerance` |
| Independent tolerance validation | `main.tex`, `sec:evaluation` |
| Full Gibbs assessment | `tables/mti_gibbs_full_assessment_supplement.tex` |
| Full paired simulation comparisons | `tables/mti_gibbs_full_validation_supplement.tex` |
| Historical-partition Gibbs comparison | `rqr-gibbs-supplement.tex`, `sec:supp-pharma-gibbs` |

The original algorithms, proofs, and bibliography remain in those sources.
The manuscript's population theory and its empirical tolerance assessment
have distinct assumptions and conclusions.

## What Was Actually Computed

For a fixed fitted content q and tilt delta, the generalized posterior is
proportional to the exponential of minus the learning-rate-scaled MTI loss
times independent proper Gaussian priors on the raw roots. Sorting the two
roots gives the interval endpoints. The latent normal/exponential
representation produces Gaussian root and generalized-inverse-Gaussian
scale conditionals for both zero and nonzero fixed tilt.

The full 300,016-chain study uses **marginal slice-within-Gibbs**, integrating
out the scales while retaining the same root posterior. It uses four chains
per dataset, 2000 burn-in cycles and 4000 retained cycles per chain, learning
rate 1, and independent zero-mean Gaussian raw-root priors of variance 10,000.
Each dataset's ECM-selected q and delta remain fixed throughout its chains.
Implementation is `mgp_slice_fit()` in
`application/scripts/lib/mti_gibbs_practical.R`, called by the full-study
controller in `application/scripts/lib/mti_gibbs_full_comparison.R`.

The original **Gaussian/GIG augmented sampler** was implemented and evaluated
in the earlier diagnostic study. It completed 696 chains on 174 datasets;
20 datasets met its original diagnostic criteria. Matched posterior means
from these reliable cases agree with the marginal sampler within the
reported Monte Carlo uncertainty. Present this smaller comparison with its
own scope. The full study uses the marginal implementation.

**ECM** replaces latent inverse scales by conditional expectations and
alternates deterministic root maximizations. The reported calibrated rule
uses at most 100 cycles, a stationarity tolerance of 1e-3, residual-product
flooring, and objective backtracking. These are finite-iteration estimates;
their numerical accuracy is assessed separately from the interval's content.
The earlier 456-example assessment found appreciable objective gaps in 96
ECM fits. That result describes those examples and cannot be extrapolated
as a full-study failure count.

The **DP response-distribution posterior** supplies a separate Beta content
probability for a fitted interval. ECM candidates vary over fitted content
q and tilt delta, and the shortest eligible candidate is selected. The
scan reference initializes the content grid. The cutoff is fixed through
separate simulation calibration before independent validation. The full
Gibbs comparison conditions on the resulting selected target; it does not
resample the DP distribution or optimize delta inside each root chain.

## Verified Results and Their Interpretation

The full study comprises 75,004 datasets: 72,000 independent simulation
samples and 3,004 historical training partitions, with 300,016 chains. Every dataset meets
the recorded preferred diagnostic criteria. Maximum rank-normalized R-hat
is 1.006109; minimum bulk and tail ESS are approximately 1,628 and 2,343.
No required extensions or unresolved datasets remain.

The primary Gibbs interval uses the means of the ordered endpoints. Its
width equals posterior mean width. The difference of endpoint medians is
a separate sensitivity summary; posterior median width is another quantity.
Empirical 95% width ranges in the comparison tables are the 2.5th and 97.5th
percentiles across repeated datasets within a distribution and design.
Posterior credible intervals instead describe uncertainty within one dataset.

| Procedure | Observed attainment range over 72 simulation settings |
| --- | ---: |
| MTI-ECM | 95.1%--99.9% |
| Gibbs posterior-mean endpoints | 98.2%--100.0% |
| Young--Mathew | 93.6%--98.0% |
| Wilks | 95.1%--100.0% |

Attainment means that the produced interval has true population content at
least c. Its repeated-sampling frequency is compared with nominal tolerance
confidence 0.95. Binomial Monte Carlo intervals quantify simulation precision;
they do not establish a distribution-free tolerance theorem for MTI selection.

Mean Gibbs width exceeds ECM and Young--Mathew mean width in every setting.
The Gibbs interval is shorter in mean than Wilks in ten settings. Limiting
empirical tilts can leave an outward root weakly constrained by the loss;
diffuse root priors then allow substantial posterior spread. Good mixing
and broad posterior uncertainty can occur together. The current posterior
summary is useful for uncertainty assessment but is not a demonstrated
improvement over the calibrated ECM short-interval procedure.

An additional reporting-resolution criterion, relative to 1% of posterior
median width, is met for all primary summaries in 22,089 datasets. The
remaining 52,915 retain quantity-specific Monte Carlo errors and meet the
preferred mixing/mean-error criteria. Application partitions reuse historical
batches and support descriptive held-out comparisons rather than independent
binomial tolerance evidence.

## Reproduction and Transfer

Sampling source: `5d2e0f361503bded17e1bdf3b2fd2c72a98a4ed3`.
Pinned reference exdqlm source: `dffb71ee70b597d6a716ee74be1cbc99731cd453`.
The complete sampling records stay in the ignored local campaign. The compact
publication evidence is committed under `figures/data/mti_gibbs_full_20261113/`.
Its scope and exact hashes are described in
`docs/reproducibility/mti_gibbs_full_publication_20261113.md`.

```sh
make verify-mti-gibbs-review
make verify-mti-gibbs-full-publication
make verify-mti-ecm-article
make test-manuscript-language
make arxiv-source
make article-handoff
```

The last two commands compile a committed snapshot without simulation caches
or raw chains. The first prepares the main article's source ZIP; the second
includes both root documents. Generated bibliographies and actual LaTeX
dependencies are included. Standalone PDFs, this reference, and the exact
source receipt are stored beside each source ZIP under ignored
`application/cache/`. Source ZIPs contain neither compiled article PDFs nor
private data, raw chains, local plans, or raw simulation records.

The dissertation coordinator should preserve the generalized-posterior
definition, the distinction between the two Gibbs implementations, the finite
ECM settings, and the separate calibration and posterior-uncertainty results.
This reference supports accurate integration; it does not authorize a new
sampling campaign or claim that Overleaf or dissertation sources were changed.
