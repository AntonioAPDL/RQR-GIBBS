# Fixed Gaussian Priors for the ECM and Gibbs Comparison

## Statistical Specification

The new study uses the same fixed Gaussian root prior for ECM estimation and
Gibbs sampling. Declare a reference center `m0` and positive scale `s0` before
observing a fitting sample. In reference units `z=(y-m0)/s0`, the two raw roots
have independent `Normal(0, kappa^2)` priors. The primary specification has
`kappa=2`, so the root SD is 2, variance is 4, and precision is 1/4. SD 1 is a
prespecified development sensitivity. The primary prior does not change with
sample size, distribution, requested content, or fitting method.

The learning rate is 1 in reference units, equivalently `1/s0^2` in the original
units. The original-unit root prior is `Normal(m0, (kappa*s0)^2)`. Neither prior
parameters nor the learning rate use the fitting-sample mean or SD. Under a
change of measurement units, transform the reference, tilt, and DP base measure
as well as the observations. These transformations preserve the statistical
specification.

The simulation laws are already defined in common centered, unit-variance
study coordinates. Every simulation uses the declared reference `(0,1)`;
individual samples are not standardized again. This is a correctly centered
reference assumption in this experiment, not evidence of robustness to an
incorrect scientific reference. The fitter receives neither the true CDF nor
the family name, oracle interval, or population content. True content is
computed only after fitting.

The pharmaceutical analysis also fixes `(m0,s0)=(0,1)`, in the numerical units
of the public file. This is an explicit working regularization assumption.
It is not an elicited manufacturing specification, an MPa conversion, or a
reference estimated from the 187 batches. For SD 2, the induced prior width has
mean `4/sqrt(pi)`, approximately 2.26 reporting units, and a 95th percentile
approximately 5.54 units. This quantifies the assumption; it does not establish
its scientific suitability. Prior sensitivity and actual refitting results
must be considered before revising the application's substantive interpretation.

The DP base remains the previously specified `Normal(0,4^2)` in original
units. Its concentration is determined by the fixed design rule, not estimated
from the fitting sample. The Gaussian root prior and the DP prior play distinct
roles: the former regularizes the interval functional; the latter assesses
uncertainty about the response distribution's content in a proposed interval.

## Posterior Interpretation

For each fixed fitted content `q` and tilt `delta`, the generalized posterior
is proportional to the exponential of minus cumulative MTI loss times the
Gaussian root prior. The residual-product check loss is nonnegative. The tilt
adds a term linear in the roots. Consequently the negative log density is
bounded below by a positive quadratic minus a linear function of the root
magnitudes, plus a data-dependent constant. The normalizing integral and all
polynomial root moments are therefore finite for every finite fixed tilt.
This also covers empirical limiting and outside-range tilts. Propriety does
not guarantee narrow intervals or frequentist tolerance performance.

The existing ECM and marginal slice-within-Gibbs implementations use this
same fixed-target objective. The marginal sampler integrates the augmentation
variables out; it targets the root posterior of the Gaussian/GIG augmentation,
rather than a different response model. Four dispersed chains are run for
each selected target. Reported Gibbs endpoints are posterior means of ordered
roots, not posterior predictive responses.

Fitted content and tilt are still selected from the training observations by
the existing ECM profile search and DP probability rule. This is a
selected-target generalized-Bayes analysis. It is not a joint posterior
integrating uncertainty in `q` and `delta`. Sample-dependent target grids,
initial values, and Monte Carlo accuracy criteria do not alter the fixed root
prior. Frequentist evaluation includes this entire selection procedure.

The earlier prior screen used `mean(y)` and `sd(y)` to construct a prior and
learning rate. It remains available as data-adaptive development evidence;
its results are not estimates under the new fixed prior.

## Numerical Assessment

All profile candidates are checked against exact one-root conditional modes.
An adequate candidate has maximum conditional objective improvement at most
`1e-6`, endpoint displacement at most `1e-5*max(1,width)`, and unchanged closed
interval count under either conditional-mode update. These are coordinate
accuracy checks, not a proof of a global joint minimum.

If necessary, the native ECM continues for bounded additional budgets of 800,
2,000, and 10,000 cycles. Candidate stopping flags and the selected fit's
original and continued trajectories are retained. Unresolved candidate
trajectories are saved when they prevent a return.
Remaining inaccurate candidates receive at most 100 alternating exact
conditional-mode cycles on the same objective, each checked with the stable
objective increment. These refinements are recorded separately from ECM
cycles; they address flat directions where the augmented ECM can stop with
small objective change but an unresolved endpoint. They neither change the
prior nor establish a global joint minimum.
An endpoint very near an observed knot can be represented by that exact knot
only when it is the analytical conditional mode and a stable objective
increment verifies nonincrease within a floating-point error allowance. A
distant endpoint is never snapped to the sample range. Failed or unresolved
candidates stop selection for that sample; there is no scan fallback or
replacement sampling until a fit succeeds.

Gibbs starts with 500 burn-in cycles and 1,000 retained draws per chain.
Prespecified mixing and posterior-mean Monte Carlo accuracy checks can extend
sampling to 2,000 or 4,000 retained draws. The final longer run replaces the
shorter estimate; duplicated prefixes are not averaged as new observations.
Tail-quantile accuracy is recorded separately from the accuracy of the
reported posterior-mean endpoints. Both ECM and Gibbs intervals receive their
own DP content-probability assessment after endpoint construction.

## Independent Assessment

The eight distributions and nine `(n,c,p)` designs are unchanged. All designs
use `p=0.95`. The stages are fixed before the new results are examined:

| Stage | Data per design | Root SDs | Paired ECM/Gibbs fits |
| --- | ---: | --- | ---: |
| Development | 12 | 2 and 1 | 1,728 |
| Calibration assessment | 200 fresh samples | 2 | 14,400 |
| Final validation | 1,000 fresh samples | 2 | 72,000 |
| Application | 1,502 existing partitions, two responses | 2 | 3,004 |

Development deliberately reuses the preceding development observations.
Calibration and validation have disjoint data and chain seeds, also checked
against the original validation data seeds. The primary prior is frozen;
neither development nor final validation automatically selects a different
prior or tunes the inherited DP probability thresholds.

Automatic progression requires adequate primary numerical results. The
calibration assessment also stops progression if a simultaneous one-sided
95% upper bound for any primary method/design/distribution attainment rate
is below the requested confidence. These upper bounds use a Bonferroni
adjustment over the primary cell comparisons. A failure to detect
underattainment is not a proof that all attainment probabilities exceed
0.95. Pointwise lower bounds are reported separately; the program does not
repeatedly add replications until they pass. This stage assesses the existing
rule rather than claiming to have optimized new probability thresholds.

Independent validation reports successes among all attempted samples,
interval return, actual population content, and raw within-distribution
width summaries. Young--Mathew and Wilks are evaluated on exactly the same
observations. Young--Mathew uses `tolerance` 3.0.0; Wilks uses the full-sample
min/max interval when its exact finite-sample feasibility condition holds.
Failures remain in the denominator. Summary PDFs show binomial intervals for
attainment and the 2.5th--97.5th percentiles of widths, with crosses at mean
widths. Overlapping application partitions are descriptive comparisons, not
independent binomial evidence for tolerance confidence.

## Reproduction

From a committed checkout:

```bash
make test-mti-fixed-prior
make check-mti-fixed-prior
make prepare-mti-fixed-prior
make launch-mti-fixed-prior
```

The launcher creates a detached exact-commit worktree, copies the already
verified installed `rqrgibbs` and `tolerance` packages into its ignored private
library, and starts a detached controller. No external source checkout is
compiled or loaded. At most 40 available workers run one paired dataset each;
each worker runs its four chains serially with one BLAS/OpenMP thread.
The recorded external exdqlm reference remains
`dffb71ee70b597d6a716ee74be1cbc99731cd453`; this study does not load exdqlm.

The manifest records the article commit, relevant program/configuration and
input hashes, installed-package and native-library hashes, generator hash,
R and package versions, RNG algorithm, priors, and references. Fits are saved
atomically with hashes and per-task identifiers. Restart reuses only verified
compatible results. Failures preserve available diagnostics instead of
silently changing observations or statistical settings.

Outputs are under the frozen worktree's
`application/outputs/mti_fixed_prior_20261114/`. Its `progress.json`,
`status.json`, `pipeline_status.json`, fit files, CSV summaries, and
`study_review.pdf` distinguish saved attempts from numerical adequacy and
statistical performance. Run `make health-mti-fixed-prior` in that worktree
for a one-time progress check. The log is
`application/logs/mti_fixed_prior_20261114/run.log`.

Before each later stage, available disk space is checked against estimated
additional fit storage, preserving a 16-GiB reserve. Space is checked again
before each worker batch. A numerical, source-identity, or resource problem
stops the controller with completed evidence retained. No article table,
figure, prior description, or numerical conclusion is replaced automatically.
Verified new results must precede manuscript revision.
