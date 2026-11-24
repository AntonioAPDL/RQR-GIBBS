# Fixed-Target Gibbs Comparison Across Twelve Distributions

## Statistical Assessment

The original 72,000 fixed-target Gibbs fits are combined with 36,000 new
fits for log-normal log SD 1.5, Weibull shape .5, inverse Gaussian shape .25
and Lomax shape 3.5. Each of 108 distribution/design cases has 1,000 samples.
The nine feasible designs and tolerance confidence .95 are unchanged.
The primary ECM/Young--Mathew/Wilks comparison and pharmaceutical application
are preserved. The separate calibrated Gibbs rule retains its 72-case study.

Gibbs sampling conditions on each saved ECM-selected fitted content q and
tilt delta. Four marginal slice-within-Gibbs chains target the root generalized
posterior under independent N(0,2^2) priors and learning rate one. The interval
averages draw-wise ordered endpoints. It describes selected-target posterior
uncertainty; selection uncertainty and a joint DP/root posterior are outside
this assessment. The DP content probability is evaluated separately at the
actual mean endpoints. All simulation data were transformed using analytic
population moments fixed before sampling, with no sample-based prior choice.

The added run completed 36,000 numerically adequate fits, 144,000 chains,
with zero sampling errors or warnings. Maximum R-hat is 1.009611; minimum
bulk/tail ESS are 1056.64/1134.83. The stopping rule concerns the endpoint
means used in the comparison. Stricter posterior tail-quantile MCSE records
are separate; the .025--.975 width ranges in tables and figures instead
describe variation across the 1,000 fitted sample intervals.

## Findings and Interpretation

New-case Gibbs attainment is 99.2--100%, with mean widths 2.3--125.1% larger
than matched ECM widths. At n=1000,c=.90, lower-endpoint changes account for
99.1--99.3% of the mean width difference, mostly below the population support.
At n=500,c=.99, roughly one third of the smaller width difference comes from
the lower endpoint. Gaussian root priors allow below-support endpoint mass;
the use of posterior means and modes can consequently have distinct effects
on interval width. These are within-case comparisons, never averages of raw
widths across different distributions.

The exact conditional modes agree with the selected ECM endpoints for the
first sample of every added case. These deterministic checks and the saved
sampling diagnostics assess numerical accuracy, not global joint optimality.
Known population support is used only to interpret endpoint differences.
No fitting rule or interval was retrospectively truncated or recalibrated.

Across all 108 cases, Gibbs attainment ranges from 93.1% to 100%, with mean
width above ECM in 106 cases. The original Student-t(3), n=500,c=.99 warning
remains: 931/1000, pointwise exact interval [.9135,.9459]. Its simultaneous
upper bound over 108 cases is .9559. The original longer-chain evidence
remains separately available; it is not repeated or replaced here.

In 12,576 new samples the mean-endpoint DP content probability is below the
ECM selection cutoff, although all exceed .95. Thus empirical attainment and
the ECM probability criterion are assessed separately for the reported Gibbs
interval. The expanded comparison supplies no new general tolerance theorem.

## Evidence and Reproduction

The frozen Gibbs source is `a35a9cfcdafd9a17e3ceb523a88ee5e245e5aa3a`, parent
study `5abbdcc4c81cbb777b1c81247a32e1bbbe9fe22a`, and pinned exdqlm reference
`dffb71ee70b597d6a716ee74be1cbc99731cd453`. The numerical package and private
library identities are retained in the copied completed manifest. No protected
source checkout is compiled, loaded as a package or modified.

`application/scripts/151_publish_mti_gibbs_skewness.R` provides collect,
render and verify commands. Collection validates the completed controller
exit, all ten report hashes, task index, saved sample identities, numerical
summaries and paired comparisons. It reproduces compact case summaries and
adds a separate local replication export with case identifiers filled from
the frozen task index. The original report and fitted objects remain unchanged.
Only 36 original parent samples are read for exact conditional-mode checks;
no MCMC or ECM fitting routine is called. The first Lomax replication at
n=1000,c=.90 also supplies a saved-draw diagnostic example. Its four chain
files and assessment are checked against their completion receipts, sample
identity and reported endpoint means. Every fourth retained draw is exported
for display; pooled-rank counts use every retained draw. Source hashes are
recorded, while the full chains remain local.

Compact evidence is in `figures/data/mti_gibbs_skewness_20261122/`, including
432 case/method summaries, paired added-case contrasts, endpoint changes,
case-level numerical summaries, conditional-mode checks, a thinned diagnostic
trace and complete pooled-rank counts, original completion
records and source/data hashes. Full replication exports and fitted objects
remain ignored. The primary evidence directories are separately checked for
unchanged contents.

```bash
# Collection requires the completed local study identified by its ignored pointer.
make collect-mti-gibbs-skewness-publication

# These commands work from committed compact evidence, without local fitted objects.
make report-mti-gibbs-skewness-publication
make verify-mti-gibbs-skewness-publication
make test-mti-gibbs-skewness-publication
make test-mti-gibbs-skewness

# Compile using verified, already rendered figures and tables.
make -o theory-figures -o theory-tables pdf supplement
```

The matching tables and figure directories contain twelve distribution-level
comparisons, generated numerical values, six readable two-distribution width
figures and a twelve-law attainment-range figure. Width crosses mark means;
dashed gray segments mark population-shortest content-c references only in
width figures. Four methods are grouped closely within each design, with
separation between designs; Wilks lines are solid black. Attainment segments
span the observed proportions across twelve laws. Pointwise and 108-case
simultaneous binomial bounds remain in the compact case data.

The combined numerical-assessment table covers 108,000 simulation fits and
3004 historical application fits. Across them, maximum R-hat is 1.009611,
minimum bulk ESS is 1056.64 and minimum tail ESS is 1099.06. These are the
summaries used in the main computation section. Original 72,000-fit records
remain unchanged. The new population-reference figure covers all twelve
laws and three contents, using the primary study's existing 36 references.
It bounds the width of a realized interval attaining the requested content;
it is not a lower bound for every failed interval or for expected width.

Source-package receipts distinguish the original fixed-prior study, the
three-method primary extension and this separate Gibbs extension. The upload
archives contain only actual TeX dependencies and generated bibliographies;
the three reproduction guides accompany the local review copies separately.

The new publication configuration does not modify the primary configuration's
`sample_gibbs=false`: that declaration still correctly describes its original
three-method study. All calibrated Gibbs counts and the application remain
separate. To reproduce the frozen numerical run, use its original detached
checkout and runtime; the publication commands do not rerun it with changed
reporting source or overwrite its completion records.
