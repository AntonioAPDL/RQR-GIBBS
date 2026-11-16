# Gibbs Boundary Study Closeout

This assessment preserves the completed Gibbs study and the original article
comparison. It first replays all 100 Gaussian n=500,c=.95 pilot samples and
the first 12 original validation replications through the old ECM call order.
Posterior sampling is unnecessary for this replay because it follows ECM
selection and cannot change the saved ECM interval. Every candidate and the
selected interval must match within 1e-10; eligibility and counts must agree.

If replay agrees, an independent, preassigned 2000-sample confirmation fits
ECM, Young--Mathew, and Wilks at n=500,c=.95,p=.95. The fixed root-prior SD
remains two. The Gaussian law, response coordinates, DP base, candidate rule,
and numerical controls are unchanged. New data seeds start at 1920000001,
and engine seeds use 1930000000+100*job_id. Neither priors nor cutoffs are
retuned. The original 88/100 outcome is retained separately.

Use the original pinned installed library, for example by setting
`MTI_PRIOR_R_LIBRARY` to the completed fixed-prior run's
`application/cache/fixed_prior_library`. No external source package checkout
is installed, compiled, or loaded.

```sh
make test-mti-gibbs-closeout
make prepare-mti-gibbs-closeout
make run-mti-gibbs-closeout
make render-mti-gibbs-closeout
make verify-mti-gibbs-closeout
```

Numerical execution requires clean committed source. Its manifest records
the Git commit, programs, configuration, installed package hashes, R and RNG
settings, pinned exdqlm reference, and completed-study identity. Forty workers
are permitted, with one numerical thread each and a 16 GiB free-disk reserve.
Atomic fitted objects and receipts permit identical-source resumption;
failed fits are preserved and stop advancement. The calculation is bounded
and normally finishes within the interactive work session. It does not
initiate a new full Gibbs study.

Compact evidence is extracted under `figures/data/mti_gibbs_boundary_20261116`.
It includes the confirmation observations and summaries, complete paired
pilot results, calibrated multipliers, numerical summaries, replay differences,
and source/input hashes. Fitted objects stay under ignored outputs. The
18-page diagnostic PDF uses conventional distribution names and a separate
legend area; widths retain their raw 2.5%--97.5% sampling ranges and mean
crosses. Attainment intervals are pointwise exact binomial confidence intervals.
These are distinct uncertainty summaries.

The Gibbs pilot concerns a rule selected by design, conditional on ECM's
fitted content and anchor tilt. Simulation-derived radius calibration is
restricted to the declared finite distribution panel. Its assurances cannot
be transferred automatically to arbitrary distributions or the application.
The main 1000-replication article comparison is not replaced by this smaller
pilot or by the single-case confirmation.
