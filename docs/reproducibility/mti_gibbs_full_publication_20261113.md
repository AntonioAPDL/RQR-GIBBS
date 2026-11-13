# Completed MTI Posterior Assessment

## Scientific Specification

The full study uses all 72,000 archived tolerance-validation samples and
3,004 pharmaceutical training partitions. Four independently seeded
intercept marginal slice-within-Gibbs chains per dataset use 2,000 burn-in
and 4,000 retained cycles. Every chain keeps the archived ECM-selected
fitted content q and tilt delta fixed, with learning rate 1 and independent
zero-mean Gaussian raw-root priors of variance 10,000. The reported primary
interval uses the posterior means of the ordered endpoints. Its width is
the posterior mean width. Endpoint-median intervals are a separately
recorded sensitivity summary.

Sampling source is `5d2e0f361503bded17e1bdf3b2fd2c72a98a4ed3`; its reference
exdqlm commit is `dffb71ee70b597d6a716ee74be1cbc99731cd453`. Publication
generation does not build, install, or load any external source checkout,
nor does it load the archived installed reference package. No new fits,
optimization, calibration, priors, or target selection are introduced.

The completed study contains 300,016 chains. Every dataset meets the
preferred recorded diagnostics, with maximum R-hat 1.006109 and minimum
bulk/tail ESS 1,627.783/2,343.152. No additional sampling was needed. The
additional all-primary mean/median/credible-endpoint reporting-resolution
criterion is met in 22,089 datasets; that resolution is distinct from
mixing adequacy.

## Numerical Evidence

Versioned compact files live in `figures/data/mti_gibbs_full_20261113/`.
They contain simulation cell summaries, paired differences, empirical-tilt
counts, median-endpoint sensitivity, parameter precision counts, application
partition summaries, and independent-chain agreement with the earlier
456-example study. Raw widths are summarized within a distribution and
design. Across-dataset empirical width quantiles are distinct from the
within-posterior width quantiles for an individual sample.

Simulation content is evaluated using the known distribution. Its attainment
indicator is 1{F(U)-F(L)>=c}. Exact binomial confidence intervals describe
independent replications. Simultaneous intervals allocate 0.05/(2K) to each
tail, where K is the number of simulation settings for that method (72 in
this study). These are separate 72-setting families for each method, not a
joint confidence claim across every comparator and not a distribution-free
tolerance theorem. Application summaries describe overlapping historical
partitions; they have no independent-binomial confidence intervals.

The four illustrative posterior cases were specified in the earlier review.
Their data digests, q/delta, priors, and learning rate are checked before
reusing the separately minimized loss references. Each illustrated full-study
chain is hash-checked. Compact empirical CDF points retain 513 ordered width
locations. One prespecified example additionally stores every fourth retained
draw for visual display and exact 20-bin pooled-rank counts from all draws.
This display subsampling does not change inference or diagnostic calculations.
Heavy fitted objects and complete draws remain ignored in the campaign.

The independently seeded full-study chains agree with the earlier means
within twice combined MCSE in 1,734 of 1,824 comparisons. The earlier study's
original augmented-sampler comparisons and prior reweighting retain their
own 456-example scope. Their cases overlap the full study and are not added
to its sample count.

## Reproduction

Run from this repository's implementation checkout:

```sh
make test-mti-gibbs-full-publication
make verify-mti-gibbs-review
make verify-mti-gibbs-full-publication
make pdf
make supplement
```

Verification and PDF builds use only committed compact evidence. To regenerate
publication tables and figures from that evidence:

```sh
make render-mti-gibbs-full-publication
```

Rendering updates `render_provenance.json` because PDF creation information
may change their byte hashes. The source statistics remain unchanged. Inspect
and commit any deliberately regenerated displays together with that record.

Reconstruct the compact evidence only when the completed local campaign is
available and the reporting programs/configuration have been committed:

```sh
make report-mti-gibbs-full-publication
```

The reporting configuration gives paths relative to the common RQR Git root.
The command checks the successful terminal state, the independent full-report
review, the complete design, and the frozen historical evidence before
collecting summaries and selected illustrations. It writes new versioned
publication files and does not modify the frozen launch checkout or original
ECM results. It never launches another sampler or background monitor.

`provenance.json` records the reporting source commit, frozen sampling commit,
program/configuration hashes, local input hashes, named compact-output hashes,
and R session. `render_provenance.json` links those statistics to the exact
generated table, macro, and figure files. The historical record stores hash
arrays without names: the new verifier checks their documented file order,
including all eleven outputs and three programs. It does not silently skip
unnamed arrays or rewrite the historical provenance.

## Manuscript Interpretation

The primary calibrated tolerance comparison remains MTI-ECM, Young--Mathew,
and Wilks. The main text states the full Gibbs findings, and the supplement
gives every four-method comparison. Gibbs mean intervals attain content in
98.2%--100.0% of replications across the simulated settings, with larger mean
width than ECM and Young--Mathew in all 72 settings. Empirical limiting tilts
help explain substantial outward endpoint dispersion under the diffuse prior.

This is evidence about the specified conditional generalized posterior and
the actual finite-chain summary. It does not transfer the ECM rule's
calibration to the Gibbs means. Changing the prior, learning rate, fitted
target rule, or optimizer would define different reported intervals and would
need separately justified validation. No further screening or relaunch is
required to present the completed study accurately.
