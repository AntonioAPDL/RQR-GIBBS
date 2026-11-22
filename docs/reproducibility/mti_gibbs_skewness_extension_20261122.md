# Fixed-Target Gibbs Assessment for the Added Skewed Distributions

## Statistical Question

This assessment extends the original mean-tilted interval (MTI) comparison of
Gibbs sampling and expectation/conditional maximization (ECM) to the four
additional distributional settings: log-normal log SD 1.5, Weibull shape .5,
Lomax shape 3.5 and inverse Gaussian mean one/shape .25. The nine designs,
tolerance confidence .95 and 1,000 replications per design are unchanged.
It adds 36,000 Gibbs fits (four chains per fit) to the original 72,000.
All original Gibbs, ECM, Young--Mathew, Wilks and application results remain
intact. The primary comparison remains ECM/Young--Mathew/Wilks until review.

For each saved sample, Gibbs sampling targets the generalized posterior of
the two interval roots at its already selected fitted content q and tilt
delta. The ECM endpoints initialize four dispersed chains. The root priors
remain independent N(0,2^2); learning rate and loss scale remain one. Sample
standard deviations enter only chain initialization, never prior specification.
Population centering/scaling was fixed before the parent samples were drawn.

The implementation reuses the marginal slice-within-Gibbs sampler. It
integrates out the normal--exponential latent variables and targets the same
root generalized posterior as the augmented Gibbs derivation. It is an MCMC
calculation for a loss-based posterior, not a response likelihood or a source
of posterior-predictive response draws. No q/delta search, ECM refinement,
prior tuning, Dirichlet-process (DP) concentration/base adjustment or radius multiplier is run.
The fixed q/delta posterior does not integrate uncertainty in target selection.

The reported Gibbs interval averages draw-wise sorted lower and upper roots.
Its actual DP content probability and true population content are evaluated
after fitting. ECM eligibility does not transfer to posterior averages. A
Gibbs interval below the ECM probability cutoff still remains a returned
interval, with its own attainment result recorded. Broad posterior intervals,
below-support endpoints and statistical underattainment are retained.

## Computation and Numerical Assessment

Effective sample size (ESS) and Monte Carlo standard error (MCSE) describe
sampling precision. The existing four-chain protocol discards 500 iterations and initially retains
1,000 per chain. Preferred diagnostics require rank-normalized R-hat <=1.01,
bulk/tail ESS >=400, mean MCSE/posterior SD <=.05, and mean MCSE <=1% of the
posterior median width, for lower root, upper root, width and midpoint.
Only when needed, chains continue to 2,000 and then 4,000 retained iterations
using the saved raw roots and exact RNG state. All prefixes are preserved.
Tail/median precision diagnostics are recorded separately from the mean
accuracy needed by the reported posterior-mean interval. Reaching the maximum
budget does not cause an unresolved fit to be relabeled as converged.

Before launch, tests compare conditional energy to the direct MTI posterior,
continue chains against uninterrupted draws, and verify checkpoints and
summary calculations. One unchanged-kernel numerical/timing check uses the
first saved sample in each added case, with a distinct test-only seed. It does
not supply a performance estimate or select model settings.

After source freeze, the first replication of all 36 cases receives the full
four-chain calculation. These are official study results and are reused in
the full run. Qualification requires finite sampling without errors and the
existing exploratory screen (R-hat <=1.05, ESS >=100, MCSE/SD <=.10). Preferred
accuracy remains separately recorded. Qualification does not require favorable
attainment or widths. No new datasets are sampled to replace difficult cases.

## Reproduction and Safe Resume

Run from the repository root:

```bash
make prepare-mti-gibbs-skewness-runtime
make test-mti-gibbs-skewness
make check-mti-gibbs-skewness
# Commit the approved source before launch.
make launch-mti-gibbs-skewness
make health-mti-gibbs-skewness
```

The runtime command copies installed packages into an isolated ignored
`application/cache/gibbs_skewness_library/` and records every file hash. It
adds posterior and its dependencies without modifying the parent's attested
library. It neither installs nor loads from an exdqlm source checkout. The
launcher creates a detached checkout of the exact launch commit and copies
the isolated library there. An exclusive lock prevents duplicate controllers.
The default output is under `/var/tmp/jaguir26-mti-studies/`; the ignored
`.codex_work/mti_gibbs_skewness_latest.json` identifies the exact directory,
source commit, checkout and parent study.

At most 40 single-threaded dataset workers run concurrently. The four chains
are serial within each worker, preventing nested process multiplication.
CPU/memory/storage are checked before sampling; storage reserves at least
32 GiB and budgets 1.5 MB per remaining dataset for all saved chain stages.
Receipts record task, sample, source, numerical library and fitted-object
identities. Parent samples and selected targets are verified before fitting
and again during completion checks. Every chain stage and completed dataset
is atomically saved. A valid complete atomic file with a missing receipt can
be recovered after validation; incompatible or corrupted files are rejected.

To request a graceful stop, create `STOP_REQUESTED` in the run directory.
The current worker batch finishes and completed chains remain available.
After removing that request, use `make resume-mti-gibbs-skewness`. Resume uses
the recorded frozen source and seeds, even if the main checkout has advanced.
Never change a running manifest, prior or sampling policy to reuse its files.
The status, progress, qualification and controller-exit records support an
on-demand health check; no continuous monitoring process is required.

## Completion Outputs and Interpretation

The controller automatically verifies all 36,000 assessments and retained
chains, reconstructs mean endpoints and their content probabilities, and
writes a new local `report/` directory containing:

- 144,000 added four-method replication records and 36-case summaries;
- 432 combined distribution/design/method rows covering 108 cases per method;
- paired added-case Gibbs contrasts against ECM, Young--Mathew and Wilks;
- all parameter diagnostics, numerical flags and error records;
- a nine-page four-method attainment/width comparison and a supplementary
  table prepared with the original distribution-level table generator;
- convergence plots for every added case's first replication and up to 12
  further samples with the largest R-hat, including warm-up, sorted endpoints,
  width, midpoint and posterior objective. q and delta are fixed, not traces;
- a numerical/scientific summary and artifact SHA-256 receipts.

Known population CDFs are used only for scoring. Attainment is the fraction
of 1,000 samples whose fitted interval contains population content at least c;
nonreturns count as nonattainment. Pointwise exact binomial intervals and
108-case simultaneous intervals per method quantify Monte Carlo uncertainty.
The original 72-case estimates are preserved; only their simultaneous bounds
are recomputed in the separate expanded report. Primary ECM/YM/Wilks estimates
must reproduce the committed comparison. Width ranges are type-7 empirical
.025--.975 quantiles within each distribution/design/method, never pooled
across laws. Mean crosses and population content-width references retain
their previous interpretation. Wilks lines remain solid black.

All fits, reports and traces remain ignored. There is no automatic manuscript
replacement. Review statistical attainment separately from numerical accuracy;
an unresolved numerical flag prevents declaring the completed comparison
fully adequate. Any subsequent calibration needs an independent design and
new validation rather than tuning these validation outcomes away.

Parent source: `5abbdcc4c81cbb777b1c81247a32e1bbbe9fe22a`.
Historical sampler source: `9b53485fc9e4d6ab43a92d80a1b0931bf58c910e`.
Pinned exdqlm reference: `dffb71ee70b597d6a716ee74be1cbc99731cd453`.
The actual launch commit is recorded by the launcher. Protected external
repositories and the regression companion are unchanged.
