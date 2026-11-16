# MTI Gibbs Interval Selection and Radius Calibration

This separate experiment examines whether the calibrated posterior-mean
interval can be shortened without changing its independent priors. The
published ECM results, Gibbs boundary assessment, and pharmaceutical
application remain unchanged. This document specifies the calculation;
local planning notes remain ignored by Git.

## Statistical Specification

The root prior is independently fixed at Normal(0,2^2), in the predeclared
simulation coordinates, with learning rate one. The direct DP prior and
existing ECM candidate grid are unchanged. The DP distribution posterior
and loss-based root posterior remain distinct. The fitted content q and
tilt delta are selected once and then held fixed during Gibbs sampling.
The interval uses the means of ordered posterior endpoints, not outer
credible limits or posterior predictive responses.

There are nine designs: (50,.90), (100,.90), (100,.95), (500,.90),
(500,.95), (500,.99), (1000,.90), (1000,.95), and (1000,.99).
The requested attainment probability is .95. Each design uses the same
eight laws as the completed study: Gaussian, Student t(3), exponential,
asymmetric Laplace, two-piece Gaussian, beta, gamma, and lognormal.
Each design-specific rule is shared by all eight laws. Neither the family
name nor its population CDF is supplied to candidate fitting or sampling.

## Bounded Exploratory Comparison

The first 100 saved calibration replications in each of 72 cases supply
7200 exploratory datasets. Their previous role and reuse are explicit:
they are no longer independent confirmation for any revised rule. The
earlier development data already established the starting tilt projection
and independent-prior sensitivity assessment; additional development fits
are unnecessary for this comparison.

Every stored candidate table must reproduce the old selected q, tilt,
and candidate identity exactly. Eligible candidates retain their checked
numerical accuracy and the original width, q, tilt-distance, lower-endpoint,
and candidate-index tie order. Sorting eligible candidates by this order
and retaining successive record DP probabilities gives the exact selection
path. Threshold equality includes the candidate. A change occurs just
above its probability. No monotonicity of population attainment is assumed.

At most two alternatives are retained per design. One minimizes the worst
law-specific ECM mean-width ratio among saved probability breakpoints with
exploratory attainment at least .95 in every law. The other minimizes the
same criterion without that preliminary restriction. Mean log ratio and
then the larger threshold resolve ties. The original threshold is included.
No arbitrary probability grid or guessed probability threshold is used.

Gibbs sampling evaluates the actual posterior-mean decision for these
choices. Identical targets are reused only after matching observations,
q, projected tilt, independent prior, learning rate, and sampling settings.
At most 14,400 additional targets are required. Four chains use 500 burn-in
and 1000 retained draws initially, with the existing bounded numerical
continuation up to 8000 retained draws. Diagnostics are preserved, including
draws, parameter summaries, trace information, Rhat, ESS, and MCSE. Numerical
failures stop the study for review; data are never replaced until successful.

The saved 200-replication radius scores also quantify the separate effects
of the old rank, the proposed rank, and the maximum across laws. Quantile
uncertainty from these reused scores is descriptive and is not a new
calibration guarantee.

## Advancement Decision

The full-study decision is declared in the configuration before new Gibbs
comparisons. A design advances only with at least 5% reduction in the
geometric mean of six asymmetric-law width ratios relative to the original
calibrated Gibbs decision, and no more than 2% increase in any law's mean
width. Width ratios compare within a law; raw widths are never pooled
across laws. These percentages are computational cost-benefit criteria,
not tolerance levels or probability guarantees.

Among eligible choices, minimize the worst law-specific ratio, followed by
the asymmetric-law geometric mean ratio. Prefer the unchanged posterior
targets if their asymmetric-law ratio is within 2% of the best eligible
alternative. This favors a calibration-only revision when it is comparably
effective. Unsuccessful designs retain their original posterior target.
If no design advances, stop with a documented negative assessment.

Exploratory comparison estimates radius quantiles on the probability scale
973/1001 for the planned calibration. It does not treat these estimates as
certified attainment statements. The final design rules are saved and
hashed before any fresh calibration or validation calculations.

## Independent Calibration and Validation

If at least one design advances, use 1000 new datasets per law/design for
calibration, and a separate 1000 per law/design for final validation.
Compare the original posterior target under a new radius calibration with
the selected target, deduplicating identical calculations. Every design
receives a new radius for both study decisions, including designs retaining
the original target. This is necessary for their common simultaneous
assurance; an older calibration cannot be silently included in the new error
allocation. Also retain the original radius decision as an attribution
check, with its separate earlier assurance. Current ECM,
Young--Mathew, and Wilks are evaluated on common validation data.

For midpoint m and half-width h, define the required radius score as the
smallest s with F(m+s*h)-F(m-s*h) >= c. The CDF is known only in this
simulation assessment. Use rank j=973, the smallest rank satisfying

    P{Binomial(1000,.95) <= j-1} >= 1-.05/144.

The allocation protects both prespecified method/case statements across
72 cases, even when their posterior fits coincide. The design multiplier
is the maximum of its eight law-specific order statistics. Nonreturns have
infinite scores and are retained; an infinite multiplier yields no finite
interval. This is a finite-panel assurance, not a distribution-free result
or an assurance for the pharmaceutical application. Recompute the DP
content probability for the actual mean-endpoint and rescaled intervals.
The initial candidate's DP probability does not carry over automatically.

The first full batch contains n=500,c=.99. It is not a new tuning pilot.
No rule changes are permitted after its results or final validation are
seen. Per-case outputs include attainment with exact binomial intervals,
raw 2.5%--97.5% width ranges, mean widths, population contents, paired width
changes and attainment discordances, and posterior diagnostics. Independent
validation provides assessment, not a new selection opportunity. Results
and a diagnostic PDF are staged locally for scientific review; no background
process rewrites manuscript prose, figures, tables, or application results.

## Reproduction and Recovery

Configuration: `application/config/mti_gibbs_decision_calibration_20261116.json`.
The installed fixed-prior library from the archived study is used read-only.
The controller records its exact Git commit, the pinned exdqlm reference,
package and source checksums, frozen data-generator identity, prior-study
identities, and independently allocated data/chain seeds. Recorded seeds
from older studies are excluded deterministically; collisions are reassigned
within separate reserved study blocks before any outcomes are generated.

Commands from the committed repository:

```sh
make test-mti-gibbs-decisions
bash application/scripts/132_launch_mti_gibbs_decisions.sh
make health-mti-gibbs-decisions
Rscript application/scripts/131_assess_mti_gibbs_decisions.R verify
```

The launcher creates an immutable detached worktree and runs preflight,
four real-sample checks plus a complete ECM-to-Gibbs refit, and the staged
experiment. It uses at most 40 available workers and one numerical thread
per worker. The latest pointer under ignored `.codex_work/` records the
actual source and output location; health reporting reads that pointer.

Because `/data` is nearly full, the default output location is
`/var/tmp/jaguir26-mti-studies/mti_gibbs_decision_<commit>`. An ignored link
under the frozen checkout retains the usual output path. Override the
storage parent with `MTI_DECISION_STORAGE_ROOT` before first launch.
Preserve a 32 GiB reserve and check projected usage from observed file sizes.
No prior results are deleted. `/var/tmp` is local experimental storage,
subject to system retention policies: archive completed evidence to durable
storage before cleanup. It is not a substitute for a long-term archive.

Each result has an atomic RDS file and checksum receipt. Restarting the
same launcher/source preserves completed fits. Changed source, rules,
observations, damaged files, or unresolved numerical results are refused.
Progress counts datasets separately from newly sampled posterior targets;
elapsed throughput supplies an ETA. The full-stage cost is approximately
24--48 hours at 40 workers before new throughput is observed, conditional
on the advancement decision. The short-list comparison is much smaller.
Source, tests, and this reproduction specification are tracked; heavy fits,
experimental evidence, logs, and planning documents remain local.
