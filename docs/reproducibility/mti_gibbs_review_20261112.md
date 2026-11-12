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

The scientific assessment and resulting next-step decision are added after
these checks finish. The existing repeated-sampling ECM tolerance results
remain unchanged throughout.
