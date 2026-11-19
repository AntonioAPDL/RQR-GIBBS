# ECM Assessment Near Nominal Tolerance Confidence

This separate exploratory study leaves both manuscripts and their reported
results unchanged. It studies eligibility thresholds for the existing MTI
generalized-posterior ECM fits. It neither samples posterior roots nor
modifies the loss, priors, DP model, numerical accuracy rules, or interval
endpoints after fitting.

## Statistical Comparison

The target is the repeated-sampling probability that a reported interval
contains at least population proportion c. Confidence intervals quantify
Monte Carlo uncertainty in estimating this probability; their lower limits
are not required to exceed the requested probability p in this exploration.
Compatibility with p does not establish a tolerance guarantee.

For each of nine feasible (n,c) designs at p=.95, 100 saved development
samples per distribution supply exact threshold-selection regions. The
eight distributions and existing q/tilt candidate construction are retained.
Root priors have fixed SD2 in the independently specified (0,1) coordinates;
the DP concentration is 1 and its base is Normal(0,4^2). The comparator is
the actual primary ECM cutoff, not a separately tuned Gibbs cutoff.

Three deterministic rules are selected on development data only:

- Point target minimizes width subject to observed attainment >= p in
  every distribution.
- Compatibility target minimizes width subject to every one-sided exact
  upper confidence limit including p. This deliberately weaker criterion
  is exploratory and can select a procedure that underattains.
- Closest nominal minimizes the largest absolute attainment deviation from
  p, then the mean absolute deviation, then width. It does not assert that
  the resulting probability is at least p.

All proposals require complete development returns and no increase in mean
width in any distribution relative to primary ECM. Ties are deterministic.
Unavailable proposals are recorded without substituting a different method.
No empirical rate such as .935 is hard-coded. The compatibility cutoff is
derived from its declared confidence level and the replication count.

The selected thresholds are frozen before 1000 fresh datasets per
distribution and design are generated: 72,000 dataset jobs. Every dataset
has one ECM candidate fit, replayed for all available rules, and paired
Young--Mathew and Wilks comparisons. No validation outcome reselects a rule.
True distribution functions enter development assessment and final scoring,
not the sample-specific fitter or interval-selection functions.

## Commands

Use the pinned installed library through `MTI_PRIOR_R_LIBRARY`, as in the
preceding fixed-prior studies. Never load an external source checkout.
Before a manual prepare/rehearsal, set `MTI_ECM_NOMINAL_OUTPUT_ROOT` to an
ignored scratch directory on a filesystem with at least 32 GiB free. The
launcher creates its own `/var/tmp` destination and clears that override.

```sh
make test-mti-ecm-nominal
make prepare-mti-ecm-nominal
make check-mti-ecm-nominal
make launch-mti-ecm-nominal
make health-mti-ecm-nominal
```

The launcher requires committed source and a successfully completed joint
study at 6a67d41f507b7fe234c208c0d718ab3b76146615. It uses a detached
checkout of the new source commit, output under `/var/tmp`, a single
controller lock, at most forty available workers, and one numerical-library
thread per worker. Existing receipts permit deterministic resumption;
numerical failures are retained for review rather than overwritten.

The local pointer is `.codex_work/mti_ecm_nominal_latest.json`. Run verification
from its pinned checkout after completion:

```sh
make verify-mti-ecm-nominal
```

## Saved Evidence

The manifest records source/package provenance, the pinned exdqlm reference,
generator hashes, all rules and task hashes, disjoint recorded seed
allocations, and unchanged manuscript hashes. Development receipts check
the original mode files and sample hashes. Fresh fitted objects preserve
candidate tables, numerical diagnostics, the baseline selected ECM trace,
and the distinction between that baseline trace and other selected targets.
They contain no Gibbs draws.

Final outputs include law-specific interval results, exact binomial
confidence intervals, empirical 2.5th--97.5th width percentiles, paired
width differences, and an exploratory comparison PDF. Missing intervals
count as nonattainment. Raw widths are never pooled across distributions.
Classical comparator errors in the declared feasible designs stop the
study for review. Lack of an eligible ECM candidate is recorded separately
from numerical failure and receives no substitute interval.

The final verification reproduces contents, widths, method counts, and
summaries while checking frozen rules and unchanged manuscript files.
Results remain exploratory, including any underattainment or lack of
improvement. Any later manuscript inclusion requires scientific review;
there is no automatic article replacement or application transfer.
