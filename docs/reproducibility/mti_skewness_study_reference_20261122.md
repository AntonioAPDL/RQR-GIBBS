# Expanded MTI Comparison: Completed-Study Reference

## Evidence and Statistical Roles

The primary comparison includes MTI-ECM, Young--Mathew interpolation, and Wilks
sample-range intervals. It combines the original eight distributional settings
with four additional settings, retaining all original results. Each of the
12 settings has 1,000 replications at each of nine designs, giving 108 cases,
108,000 datasets and 324,000 method evaluations. Common samples within each
case allow paired comparisons. This primary study supplies no new Gibbs or
application fits. A separate completed fixed-target Gibbs extension now adds
36,000 fits on its saved samples and extends that comparison to 108 cases;
see [its publication reference](mti_gibbs_skewness_publication_20261122.md).
The application and separately calibrated Gibbs study retain their original
designs and uncertainty assessments.

Tolerance confidence is .95. Designs are (n,c) = (50,.90), (100,.90),
(100,.95), (500,.90), (500,.95), (500,.99), (1000,.90), (1000,.95),
and (1000,.99). Sample-bounded, two-sided distribution-free intervals require
n >= 46, 93 and 473 for content .90, .95 and .99 respectively. These are
reference-class feasibility limits, not a theorem for the model-based MTI rule.

## Fixed Method and Added Distributions

The added laws are log-normal with log SD 1.5, Weibull with shape .5 and
scale one, Lomax with shape 3.5 and scale one, and inverse Gaussian with mean
one and shape .25. Raw draws are centered and scaled using analytic population
moments specified before sampling. Root priors remain independent N(0,2^2),
with learning rate and loss scale one. The DP base remains N(0,4^2), with
concentration one. The content/tilt candidate rule, native ECM calculation,
conditional numerical refinement, and probability cutoffs (.995, except .996
at n=1000,c=.99) are unchanged. Neither the response sample nor validation
outcomes select prior parameters or probability cutoffs. Known population
CDFs score the fitted intervals; they do not supply fitting targets.

Gamma(.25,1) and Beta(.25,8) were deferred before comparative fitting because
double-precision representation at the standardized lower support exceeded
the declared sample-level numerical risk budget (.001 at n=1000). They were
not excluded based on comparative results, and no milder replacements were
introduced. The original Gamma(.5,1) and Beta(1,8) settings remain.

## Numerical Verification and Decimal Precision

The original study completed all 36,000 new fits with exit status zero.
Every candidate passed the final coordinatewise accuracy and observation-count
stability checks, with maximum objective gap 9.999288e-7 and maximum
width-scaled endpoint gap 9.999588e-6. All initial strict ECM stopping flags
remain false; they are not relabeled as convergence. The final conditional
checks establish numerical accuracy at the declared tolerances, not global
joint optimality or population-content attainment. All 108,000 new method
records contain returned intervals; saved warnings number zero.

The original CSV decimal rounding perturbed Weibull CDF scores near its lower
support by up to 1.34e-9, with no attainment classification changes. Collection
reads every original binary fit and verifies its exact content before writing
a separate export using the existing 17-significant-digit CSV helper. Both
endpoints, width, content, fitted content and tilt round-trip exactly. Original
exports and fitted objects remain unchanged. The corrected replication-level
file stays under ignored `application/outputs/`; only compact case summaries
and their SHA-256 receipts are committed.

## Results and Interpretation

Across all 108 cases, ECM attainment is 94.8%--99.7%, Young--Mathew
93.1%--97.6%, and Wilks 94.8%--100%. ECM mean width is smaller than
Young--Mathew in 63/108 cases. Among the 36 added cases it is smaller in
24, with casewise reductions of 14.6%--35.7%, and larger by 3.3%--11.7% in
the 12 boundary cases. Every added distribution shows the gain in six of nine
designs. At (50,.90), (100,.95), and (500,.99), ECM and Wilks endpoints match
to numerical precision. The extension does not establish a gain there.

Attainment is the replication proportion for which the known population
content reaches c, counting nonreturns as failures. Pointwise exact binomial
95% intervals quantify Monte Carlo uncertainty. Two-sided Bonferroni-adjusted
intervals use a family of 108 cases per primary method: 73 ECM lower bounds
exceed .95, 35 include .95, and none is wholly below it. The expanded fixed-target
Gibbs comparison also uses 108 cases; the separate calibrated Gibbs study
retains 72. Young--Mathew attainment for inverse Gaussian samples at
n=500,c=.95 is 931/1000, with pointwise interval [.9135,.9459]; its 108-case
simultaneous upper bound is .9559. Report both the pointwise warning and its
multiplicity qualification, without asserting a general comparator defect.

Width ranges are type-7 empirical .025 and .975 quantiles of the 1,000 raw
widths within each distribution/design/method case. Crosses mark casewise
mean widths. No raw widths are pooled across distributions. The dashed
population-shortest content-c references use known F and are width lower
references, not competing sample-based tolerance procedures.

## Files and Reproduction

- `application/config/mti_skewness_extension_20261121.json`: frozen fitting design.
- `application/config/mti_skewness_publication_20261122.json`: publication layout and data selection.
- `application/scripts/147_publish_mti_skewness_extension.R`: collect/render/verify commands.
- `figures/data/mti_skewness_extension_20261121/`: compact summaries, numerical diagnostics,
  distribution admission, population references, data hashes and rendering hashes.
- `tables/mti_skewness_extension_20261121/`: primary ranges, derived numerical values,
  and granular added-setting tables.
- `figures/generated/mti_skewness_extension_20261121/`: attainment figure and three
  continued width panels (small samples, n=500 and n=1000).

From the repository root:

```bash
make report-mti-skewness-publication
make verify-mti-skewness-publication
make test-mti-skewness-publication
make pdf
make supplement
```

Rendering and verification require the compact committed evidence, R,
data.table, jsonlite and digest, with helper dependencies already used by the
original reporting code. They do not read raw fits, private sample files or
an external package checkout. Collection alone needs the preserved local study
pointer `.codex_work/mti_skewness_extension_latest.json` and its frozen R
library. It uses four workers, reads one case per worker, and launches no fits.
After committing the manuscript sources, `make article-handoff` builds both
documents from that exact commit and discovers source dependencies through
the TeX recorder. Its receipt distinguishes original sampling/application
commits from the additional primary-study commit.

Added-study source commit: `5abbdcc4c81cbb777b1c81247a32e1bbbe9fe22a`.
Pinned exdqlm reference: `dffb71ee70b597d6a716ee74be1cbc99731cd453`.
The original study provenance and numerical package commit remain in
`figures/data/mti_skewness_extension_20261121/provenance.json`.
The original study used 40 single-threaded workers; it has finished and
released those resources. Publication integration neither installs from nor
modifies exdqlm, Q-DESN, hydrology, or the regression companion repository.
