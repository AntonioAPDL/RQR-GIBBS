# Fixed-Prior MTI Study: Current Manuscript Reference

This reference supersedes the empirical results in
`mti_completed_study_reference_20261113.md`. The earlier diffuse-prior
study remains available as historical evidence. It is not combined with
the current study. This update does not modify the dissertation or the
regression/dynamic companion repository.

## Statistical Specification

Raw roots have independent Normal(0, 2^2) priors and learning rate one.
The simulation laws use predeclared population centering and scaling;
individual samples do not supply prior locations, scales, or learning rates.
Application responses retain the public file's numerical reporting units,
with the same fixed zero location and scale one. These are working
regularization assumptions, not externally established manufacturing
specifications or physical measurement definitions.

The DP response-distribution model has a fixed Normal(0, 4^2) base and the
preassigned concentration rule. It supplies posterior content probabilities
for fitted closed intervals. The MTI generalized posterior instead updates
root priors using the residual-product loss. It is not a response likelihood.

For each sample, the preassigned content/tilt menu is fitted by ECM and
checked one-root conditional minimization. The shortest eligible candidate
is selected using its own DP content probability. The cutoffs are .995 in
eight designs and .996 at n=1000, c=.99, inherited unchanged before the new
assessment. This empirical rule has no established distribution-free
finite-sample tolerance theorem.

Four-chain **marginal slice-within-Gibbs** sampling targets the root posterior
at the selected q and delta. These remain fixed throughout its chains.
This is conditional uncertainty for a selected loss target, rather than a
joint posterior integrating target selection. Sorting each root pair before
averaging gives the reported Gibbs mean interval. Its eligibility and
attainment do not follow from the selected ECM interval's checks.

## Derivations and Algorithms

Use stable LaTeX labels when transferring material:

| Subject | Source label |
| --- | --- |
| MPI characterization | `prop:population-characterization` |
| MTI generalization | `sec:mti-loss` |
| Generalized posterior and conditionals | `sec:static-theory-statements` |
| Full posterior and propriety | `sec:supp-mti-posterior` |
| Gaussian/GIG augmented Gibbs algorithm | `sec:supp-mti-gibbs` |
| Actual marginal sampler | `sec:supp-mti-marginal` |
| Fixed-target numerical assessment | `sec:supp-mti-posterior-assessment` |
| Empirical tilt limits | `sec:supp-mti-empirical-limits` |
| DP content law | `sec:supp-direct-dp-check` |
| Candidate selection | `sec:mti-tolerance` |
| Independent validation | `sec:evaluation` |
| Paired Gibbs comparisons | `sec:supp-mti-full-validation` |
| Historical-partition Gibbs comparisons | `sec:supp-pharma-gibbs` |

The normal/exponential augmentation, Gaussian/GIG full conditionals,
propriety proof, and ECM derivations remain in the article and supplement.
The current full study uses the marginal sampler. Earlier augmented-sampler
comparisons retain their original data, priors, and interpretation.

## Completed Numerical Work

- Development: 1728 paired fits on 864 samples, each evaluated under fixed
  prior SDs one and two; SD two was declared primary before validation.
- Calibration assessment: 14,400 independent paired fits, 200 per case.
- Independent validation: 72,000 paired fits, 1000 per case, eight laws and
  nine feasible sample-size/content designs, confidence .95.
- Application: 3004 paired fits, 1502 historical partitions for each response.
- Longer-chain checks: 12 existing Student-t samples, four per outcome group,
  with four independently seeded chains and 8000 retained draws per chain.
- Precision extensions: five application fits, 8000 retained draws per chain;
  every original sampling prefix, selected target, prior, and ECM fit is
  preserved. No further sampling jobs remain.

Every validation/application fit passes the final coordinatewise and
posterior-mean accuracy checks. Maximum rank-normalized R-hat is 1.009028;
minimum bulk and tail ESS are approximately 1092.8 and 1099.1. Chains discard
500 draws and initially retain 1000, extending only where required. Mean
MCSE is bounded relative to posterior SD and 1% of posterior median width.
The final possible one-coordinate objective gain is below 7.55e-9 in the
simulation and 8.50e-12 in the application. This establishes recorded
coordinatewise numerical accuracy, not global joint minimization.

One original Gamma candidate failed a floating-point knot check. The saved
same-sample repair changes neither prior nor candidate menu. Five
precision-only flags receive the extensions above. Original results remain
unchanged. Application summaries are rebuilt from fitted objects and retain
17-digit endpoint precision because ordinary rounding can change inclusion
of tied observations. Saved held-out contents are verified against original
partitions before any manuscript summaries are written.

## Results and Interpretation

| Method | Attainment range over 72 cases |
| --- | ---: |
| MTI-ECM | 94.8%--99.7% |
| Gibbs posterior-mean endpoints | 93.1%--100.0% |
| Young--Mathew | 93.9%--97.6% |
| Wilks | 94.8%--100.0% |

ECM mean width is smaller than Young--Mathew in 39/72 cases. Gibbs mean
width is larger than ECM in 70/72. Width percentiles are computed separately
within each law and design; no pooled raw-width averages are reported.
The ECM gain occurs in six of nine designs for each asymmetric law, one for
the Gaussian, and two for Student t(3). At (n,c)=(50,.90), (100,.95), and
(500,.99), ECM and Wilks endpoints agree in all 1000 replications of every
law. These comparisons use matched sample identifiers and both endpoints,
with tolerance 1e-12 times the greater of one and the absolute reference
endpoint. The saved summaries are `width_patterns.csv` and
`wilks_interval_agreement.csv` in the current evidence directory.

For Student t(3), n=500, c=.99, ECM attains content in 952/1000 samples;
Gibbs means in 931/1000; Young--Mathew in 946/1000. The Gibbs pointwise
95% interval is [.9135,.9459]. Its 72-case simultaneous interval includes
.95, but that multiplicity adjustment does not remove the pointwise warning.
All 48 longer-chain parameter comparisons fall within twice combined MCSE,
and all 12 sample-level content classifications remain unchanged. These
outcome-stratified checks assess numerical stability, not another attainment
estimate. No validation-driven prior or cutoff change is made.

ECM remains the primary tolerance method alongside Young--Mathew and Wilks.
Gibbs is reported separately as selected-target posterior uncertainty and a
paired mean-interval comparison. Application partitions overlap and supply
descriptive historical evidence, not independent tolerance replications.
For nonnegative weight RSD, Gibbs mean lower endpoints are negative in
772/1000 random partitions, with median -.901816. The reported endpoints and
widths are unchanged: unrestricted Gaussian root priors permit this behavior.
`application_support.csv` records endpoint signs separately by response,
partition scheme, and method. A future support restriction would be a change
in method requiring its own scientific justification and assessment.

## Source Identities and Reproduction

Simulation source: `9b53485fc9e4d6ab43a92d80a1b0931bf58c910e`.
Repair/application/diagnostic source: `abd14dbb8c4aa06f5622f256edb609646acd273b`.
Precision sampling source: `9fd9ca40b5645342a8b15243f3bca14425ae4d04`.
Verified finalization: `9569be98ce285fe2359c939a645651bc00a14858`.
Pinned exdqlm reference: `dffb71ee70b597d6a716ee74be1cbc99731cd453`.
No external source checkout is built, installed, or modified.

Small reproducible summaries are in `figures/data/mti_fixed_prior_20261115/`;
heavy objects remain ignored. The separate evidence and rendering receipts
record input, source, and output hashes. Main and supplementary empirical
tables and figures refer only to this fixed-prior study.

```sh
make test-mti-fixed-prior test-mti-fixed-prior-closure
make test-mti-fixed-prior-finalization test-mti-fixed-prior-reporting
make report-mti-fixed-prior verify-mti-fixed-prior-report
make pdf supplement
make arxiv-source
make article-handoff
```

Rendering requires only tracked summaries. Recollecting them from local fits
uses `Rscript application/scripts/125_report_mti_fixed_prior.R collect`.
The source-package commands require committed source and include actual
LaTeX dependencies and fresh bibliographies, with review PDFs kept separately.
