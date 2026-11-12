# Completed Fixed-Target Posterior Review

## Question and Study Scope

The review asks whether differences between archived ECM endpoints and root
posterior summaries arise from incomplete optimization, posterior dispersion,
or incorrect sampling. It uses the completed, frozen 456-dataset marginal
slice study at `877366f36ba1490f6e75a8f5bac9471a75e41eec` and the original
Gaussian/GIG study at `44afea30784540ae913b0a558a5c572790fd7493`.
No new chains, target selection, or tolerance calibration are performed.

The 370 balanced datasets comprise five replications in each of 72 simulation
cells and ten application partitions. The remaining 86 datasets were selected
as extreme-target diagnostics; they are reported separately. The study holds
the archived ECM-selected content q and tilt delta fixed. It does not average
over their selection uncertainty.

## Checks and Comparisons

1. Reconstruct each sample and verify its digest, target, chain receipt, file
   digest, and exact direct-objective evaluations at 24 saved draws per chain.
   Include the 696 original chains and 72 continuation segments as well as
   the 1,824 marginal chains. Recompute the published numerical summaries.
2. Compute exact empirical outward loss slopes. Classify each tilt as
   interior, limiting, or outside the corresponding empirical range.
3. Use the archived, installed fixed-target numerical solver as an independent
   penalized-loss minimum reference. Record its objective bound and the gap
   from the finite ECM iterate. These calculations do not replace ECM fits
   and do not enter the tolerance results.
4. Compare original and marginal sampler means using combined Monte Carlo
   errors, identifying which original chain sets meet the stated diagnostics.
   Retain comparisons involving poorly mixed chains without treating them
   as stationary posterior estimates.
5. Examine tighter Gaussian root priors by bounded importance reweighting.
   Record both weight ESS and a chain-aware ratio-estimator Monte Carlo
   error. Unsupported reweighting results remain flagged. This is a local
   prior-sensitivity calculation, not a newly calibrated interval method.
6. Plot posterior width distributions with ECM and penalized-loss minimum
   references for prespecified examples. Raw widths are never pooled across
   response distributions or sample-size/content settings.

## Reproduction

```sh
make test-mti-gibbs-review
make review-mti-gibbs
make verify-mti-gibbs-review
make verify-mti-ecm-article
make pdf
make supplement
```

`application/config/mti_gibbs_review_20261112.json` specifies the frozen input
and the analysis. The script checks the original installed-runtime attestation
and the protected external checkout, including ignored files. It never loads
an external source checkout as an installed package. Analysis uses 12 workers
and does not interrupt or mutate the completed studies.

Small CSV summaries, a provenance record, and generated manuscript prose are
tracked. The chain objects, local PDF copy, and working plan remain ignored.
`provenance.json` records the analysis commit, program hashes, input manifest,
runtime hash, artifact hashes, and R session. The verify command checks the
tracked output without requiring local chain objects.

## Verified Findings and Decision

- All 2,592 chain records pass the identity, numerical-status, digest, and
  direct-objective checks. The independently recomputed summaries agree with
  their saved values. This total includes continuation segments, not 2,592
  independent chains.
- All 456 marginal-sampler datasets meet preferred mixing diagnostics. Across
  ordered roots, width, and midpoint, maximum R-hat is 1.003104, minimum bulk
  ESS is 1,781, and minimum tail ESS is 2,824. Both endpoints and width meet
  the additional mean, median, and credible-endpoint reporting resolution in
  125 datasets. The remaining precision labels do not constitute convergence
  failures and do not trigger blanket extensions.
- The balanced simulation subset contains 170 interior, 129 limiting, and
  61 outside-range empirical tilts. The application subset has three, one,
  and six, respectively. The stress subset contains 20, 37, and 29. These
  are counts for the recorded fitted targets, not population-admissibility
  classifications or full-study prevalence estimates.
- The 20 original Gaussian/GIG datasets meeting the original criteria give
  80 matched posterior-mean comparisons: 77 lie within twice combined MCSE;
  the maximum standardized difference is 2.466. Nine extended datasets meet
  preferred criteria, with all 36 mean comparisons within twice combined
  MCSE. Comparisons involving inadequately mixed original chains do not
  establish a difference between posterior targets.
- An independent penalized-loss reference resolves every sample to a relative
  objective bound below 5e-13. The archived ECM relative objective gap exceeds
  1e-4 in 96 samples: 94 outside-range and two interior. The remaining ECM
  fits can still differ substantially from posterior means. For example,
  one limiting-tilt Normal sample has essentially identical ECM and minimum
  widths of 4.623, but posterior mean width 75.08. This is posterior spread,
  rather than an optimization gap.
- Tighter-prior reweighting demonstrates substantial sensitivity in many
  limiting-tilt cases. Four of the 456 half-variance estimates and six of
  the tenth-variance estimates lack sufficient overlap or precision under
  the declared checks. These estimates remain flagged, and no prior choice
  is selected from this calculation.

The completed checks support reporting the conditional posterior study while
retaining the original ECM tolerance results unchanged. The Gaussian/GIG and
ECM derivations remain in both manuscripts. The supplement additionally
derives the intercept marginal sampler, explains the empirical tilt limits,
and shows four prespecified posterior-width examples. `cells.csv` separates
all simulation distributions, sample sizes, contents, and balanced/stress
roles; it never pools raw widths across different distributions.

No further broad run is warranted simply to make Gibbs intervals resemble ECM
intervals. A possible next methodological study would first specify an
empirically interior target rule and scientifically scaled priors, then
calibrate that changed interval estimator on independent data. It would need
fresh tolerance validation. It must not retroactively relabel or replace the
present calibrated ECM evidence. Committee/dissertation updates can cite the
completed computation findings and their limits now, without waiting for
another sampling campaign.
