# Additional skewed distributions: reproducible ECM comparison

## Statistical specification

This separately identified assessment extends the existing eight-distribution
simulation comparison. MTI-ECM, Young--Mathew and sample-range Wilks receive the
same observations. No new Gibbs results, application refits or calibration
selection are included. Proper root priors remain N(0,2^2), loss scale and
learning rate remain 1, and the DP concentration and Gaussian base remain 1
and N(0,4^2). The existing design-specific probability threshold, q/tilt menu,
deterministic selection and numerical refinement are unchanged.

Data are transformed using their prespecified population mean and standard
deviation. No analysis-sample estimate specifies the prior. This known-law
simulation convention is not a prescription for constructing application priors.
The true CDF is used to assess the produced endpoints, not to select a fitted
interval. For each replication, attainment means `F(U)-F(L) >= c`; its repeated-
sampling probability is compared with confidence p=.95.

The four priority additions are lognormal with log-SD 1.50, Weibull with shape
.50 and scale 1, Lomax with shape 3.50 and scale 1, and inverse Gaussian with
mean 1 and shape .25. All are continuous unimodal laws, allowing a boundary
mode. The Lomax has finite third but infinite fourth moment. The sample-size/
content designs are (50,.90), (100,.90/.95), and (500/1000,.90/.95/.99), with
1,000 replications per law/design: 36,000 new datasets and 108,000 method results.

Gamma(.25,1) and Beta(.25,8) are independently considered conditional additions.
They are currently deferred by a numerical rule fixed before comparative fitting.
The implementation finds the raw interval mapped exactly to the standardized
lower support in double precision, evaluates its probability m, and calculates
`1-(1-m)^n` at the largest design n. The admission budget is `1/R`, where R=1000
assessment replications. It limits a numerical boundary-collapse event; it is
neither a tuning parameter nor a bound on attainment error. The observed risks
are about .0651 and .0644, respectively, compared with budget .001. This does not
show that the statistical procedures fail for these laws. It defers them until
their representation in the fixed double-precision fitting calculation can be
resolved. No shape changes, data jitter or replacement samples are used.

## Historical evidence and method identity

The historical component is the existing article evidence in
`figures/data/mti_fixed_prior_20261115/`, verified against `provenance.json` and
its original interval/comparator exports. The newer `ecm_primary` assessment
from November 20 is a separate replication, not a replacement for the article's
paired ECM/Gibbs samples. The old eight-law results and application remain intact.

The private library contains the pinned `rqrgibbs` computation and tolerance
3.0.0. Distribution dependencies are statmod 1.5.2, actuar 3.3-7 and expint
0.2-1, with source-archive SHA256 values in the study configuration. Installed
code, dependencies, configuration and sources are hashed in the study manifest.
Historical fixed-scale compatibility is tested on one saved article-reference
dataset in each of the nine designs before the new assessment starts.

YM is the two-sided `tolerance::nptol.int(...,method="YM")` result, not a modified
interpolation rule. Wilks is specifically `[min(y),max(y)]`; it is not the
package's alternative rank-selected `method="WILKS"` rule. Its feasibility
probability is `pbeta(c,n-1,2,lower.tail=FALSE)`, which exceeds .95 in all nine
designs. That identity does not establish MTI feasibility or a new MTI theorem.

## Commands

From the committed implementation checkout:

```bash
make mti-skewness-runtime
make test-mti-skewness
make check-mti-skewness
make prepare-mti-skewness
make launch-mti-skewness
make health-mti-skewness
```

The runtime preparation uses ignored private storage and pinned source archives;
it does not install into the user library or external source checkouts. Heavy
execution requires clean committed source, records the MTI/exdqlm reference
commits, and uses a frozen worktree and private package library. The controller
is detached from the terminal. `MTI_SKEWNESS_WORKERS` may reduce the ceiling of
40 workers; each worker fits all three methods to one dataset, with one BLAS/
OpenMP thread and no nested pools. At least 32 GiB free disk is reserved.

The default output is
`/var/tmp/jaguir26-mti-studies/mti_skewness_extension_20261121_<commit>/`.
The ignored `.codex_work/mti_skewness_extension_latest.json` points to its exact
source checkout and output. A different storage root can be specified with
`MTI_SKEWNESS_STORAGE_ROOT` before launch. Do not move or change active inputs.

## Saved observations, diagnostics and recovery

Task-specific L'Ecuyer-CMRG states make data generation independent of scheduling
and worker count. The historical fitting RNG convention is restored before
ECM. Numerical-check samples use a separate allocation and are excluded from
the assessment. Task keys include study, law, n, c, p and replication; job IDs
alone are not unique across studies.

Each saved dataset contains raw and standardized observations, their hash, the
candidate table, selected q/tilt, selected ECM trajectory and knot adjustments,
raw stopping flags, final numerical checks, DP probability, all method endpoints,
true population content and warnings/failure reasons. No response predictive
draws or new MCMC objects are created. A strict initial ECM stopping flag alone
does not reject a fit that passes the inherited final accuracy checks.

Completed fits have atomic hash receipts. Restarting the identical frozen
controller resumes verified completed tasks without refitting them. Unexpected
partial objects, altered tasks, source identities or bad hashes require review;
they are never silently overwritten. Missing numerical accuracy is distinct
from a valid empty eligible set. Isolated unresolved calculations are saved
while other tasks finish, and block final verification/report release. A code
correction needs a new recorded source identity and a review of all affected
tasks, never a redraw or a correction selected by comparative performance.

## Verification and presentation

The controller automatically verifies every task after fitting, reproducing
observations and comparator calculations, reselecting the ECM result from saved
candidates, independently checking closed-interval DP probabilities and checking
actual content/attainment. Successful verification writes `verification.json`.
It then prepares a new local `report/` directory; neither manuscript is replaced.

The report includes combined interval-level data, full casewise summaries,
paired attainment/width differences, population shortest-content width references,
granular LaTeX tables and a nine-page comparison PDF. The PDF gives pointwise
binomial 95% Monte Carlo intervals for attainment and empirical 2.5th--97.5th
percentile width ranges with mean-width crosses. Wilks is solid black; methods
within a law are grouped closely. Population shortest-content widths appear only
on the width panels and do not represent tolerance procedures.

Widths are not averaged across distributions or divided by TCSP width. A valid
no-return counts as nonattainment; width summaries use returned finite intervals
and retain that denominator. Pointwise attainment intervals and any simultaneous
bounds have separate definitions. Simultaneous bounds are calculated separately
by method across all included law/design cases, not copied from an older study.

Run `make verify-mti-skewness` for an independent recheck and
`make export-mti-skewness` for report regeneration, using the frozen checkout
and its `MTI_SKEWNESS_OUTPUT_ROOT`/`MTI_SKEWNESS_R_LIBRARY` settings. After results
finish, review every case before updating the main/supplement text, tables and
figures. Preserve the Gibbs assessment's original eight-law sample pairing and
follow `STYLE_PROFILE.md` for any manuscript changes. No automatic promotion of
favorable distributions, new calibration or article claims is performed.
