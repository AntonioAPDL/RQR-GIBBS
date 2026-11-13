# Full Fixed-Target MTI Gibbs Comparison

## Scientific Specification

This study extends the completed 456-dataset posterior assessment to every
archived ECM simulation sample and application training partition. It uses
intercept marginal slice-within-Gibbs, with exactly the existing MTI
generalized posterior: archived fitted content q and tilt delta, learning
rate 1, and independent zero-mean Gaussian raw-root priors of variance
10,000. The latent scales are integrated out for sampling. The original
Gaussian/GIG implementation and its derivations remain unchanged.

There are 72,000 simulation samples: eight distributions, nine sample-size/
content designs, and 1,000 replications per distribution/design. There are
3,004 application training partitions. Four chains per dataset give 300,016
base chains. Each uses 2,000 burn-in cycles, 4,000 retained draws, and no
thinning. Data, targets, seeds, and the final summary rule are frozen before
sampling. No cases are removed because of empirical limiting tilts or broad
posterior uncertainty.

The primary reported interval uses posterior means of the ordered endpoints.
Its width equals posterior mean width. Posterior endpoint medians are a
prespecified sensitivity summary; their difference generally differs from
the posterior median of width. These summaries do not inherit the ECM
selection procedure's tolerance calibration. The full comparison assesses
their actual repeated-sampling behavior on the paired samples. It does not
reoptimize q/delta, add a new prior, or transfer a tolerance guarantee.

## Automatic Stages

The controller first verifies the completed scientific evidence and runs the
recovery, comparison, practical, review, and full-controller tests. It then
replays the 12 prespecified diagnostic examples, four chains each, through
the new storage and assessment code. Draws and final raw-root/RNG states
must exactly match the frozen marginal study. The bridge also exercises
the complete comparison report and its figures. Its outputs are explicitly
identified as integration checks, rather than full-study results.

Only a qualification record matching the current source, settings, inputs,
runtime, and full design allows the base queue to start. This transition is
automatic. No condition requires narrower intervals, favorable tolerance
attainment, or removal of boundary cases.

After all base chains finish, the controller assesses every dataset. It
allows one additional 4,000-draw segment for each chain of a dataset failing
the practical screen, subject to the actual storage budget. Continuation
restores the saved raw roots and RNG, adds no fresh burn-in, and records its
parent digest. There is no indefinite extension or silent method replacement.

Preferred diagnostics are R-hat<=1.01, bulk/tail ESS>=400, and mean MCSE/SD<=.05.
The practical screen is R-hat<=1.05, bulk/tail ESS>=100, and mean MCSE/SD<=.10.
Both ordered endpoints, width, and midpoint are recorded; primary endpoint/
width flags and the secondary midpoint flag are separate. Quantile MCSEs
and the extra 1%-of-median-width reporting precision remain descriptive.
They do not automatically trigger further sampling.

Calculation completeness, diagnostic adequacy, and scientific performance are
reported separately. Worker failures stop scheduling at the batch boundary
and preserve successful records and error details. Unresolved diagnostics
remain visible after the bounded continuation. No claim of convergence is
made merely because a process exits successfully.

## Resources and Storage

Use up to 50 eligible fit workers, one chain per worker. Thread limits are
set before R starts. Assessment runs separately with at most 12 workers,
within the same overall allocation. The controller checks memory, free disk,
and inodes before scheduling each batch. A 16 GiB disk reserve and an
additional 1 GiB reporting allowance are retained. There is no separate
continuous-monitoring process.

Full records retain every ordered endpoint draw, terminal raw roots/RNG,
objective summaries and selected direct-objective checks, numerical status,
and source/target identities. Large duplicated warm-up and objective arrays
are omitted. Complete diagnostic traces remain in the frozen original
marginal study, whose bridge trajectories are reproduced exactly. Full-study
retained endpoint and objective traces can be inspected or reconstructed
from the saved draws and observations.

Records are sharded deterministically, written atomically, round-trip checked,
and assigned SHA-256 completion receipts. Unreceipted files are preserved
before recomputation; corrupt or incompatible completed files are rejected.
No completed study is deleted to make room. Resource shortages preserve work
and require explicit resumption after the shortage is resolved.

The planning estimate was approximately 18 idealized sampling hours at 50
workers, plus assessment, I/O, and any extensions. Progress estimates use
actual completed batch timings. This is not a promised completion time.

## Commands

Run from a clean committed implementation checkout:

```sh
make test-mti-gibbs-full
make preflight-mti-gibbs-full
commit=$(git rev-parse HEAD)
bash application/scripts/112_launch_mti_gibbs_full_comparison.sh --dry-run \
  --confirm-article-commit="$commit" --allow-full=true --workers=50
bash application/scripts/112_launch_mti_gibbs_full_comparison.sh \
  --confirm-article-commit="$commit" --allow-full=true --workers=50
```

The wrapper creates a detached exact-commit worktree under `.codex_work/`.
Its output directory is printed at launch. The R-script sampler is identified
by its committed source hashes and R/dependency versions. The separately
attested original installed package is used only for reference calculations;
it is never relabeled as the marginal sampler's compiled implementation.
The protected exdqlm checkout is read with optional locks disabled, with
before/after guards covering ignored files. No external checkout is built,
installed, or loaded as a package namespace.

Read-only health checks and graceful stopping:

```sh
make health-mti-gibbs-full GIBBS_OUTPUT_DIR=/absolute/campaign/directory
make stop-mti-gibbs-full GIBBS_OUTPUT_DIR=/absolute/campaign/directory
```

A stop request is checked between batches. To resume, use the ORIGINAL frozen
worktree and source commit, inspect/remove its `STOP_REQUESTED` file when
appropriate, and repeat the wrapper command with the same output directory
and `--resume`. A different source/configuration is rejected. Dead locks are
recoverable only after checking the original process identity; a live
controller cannot be displaced.

## Outputs and Interpretation

- `manifest.json`, `jobs.rds`, and `design.csv`: exact full-study identity.
- `qualification.json`, `bridge.csv`, and `bridge/`: linked scientific
  evidence, exact replay checks, compact-format assessment, and report check.
- `chains/` and `extensions/`: immutable base and continuation records.
- `base_progress.json`, `extension_progress.json`, and `campaign_status.json`:
  computation progress and terminal state.
- `dataset_summary.csv` and `parameter_summary.csv`: complete conditional
  posterior summaries and diagnostic flags.
- `interval_comparison.csv`, `paired_simulation_comparison.csv`, and
  `comparison_summary.csv`: paired original ECM/Young--Mathew/Wilks and Gibbs
  endpoint summaries, evaluated on identical observations.
- `gibbs_ecm_comparison.pdf`: all simulation settings and representative
  retained traces/rank plots, plus application partition sensitivity.
- `numerical_reference_examples.csv`: separate same-target objective minima
  for diagnostic examples; these never replace ECM intervals.
- `final_summary.json` and `report_provenance.json`: actual completion,
  unresolved diagnostics, and hashes of the generated analysis files.

Simulation interval content is evaluated under the known distribution.
Its attainment indicator is 1{F(U)-F(L)>=c}; binomial confidence intervals
describe independent simulation replications within a cell. Application
content is held-out empirical content. Overlapping application partitions
are summarized separately by partition scheme, without independent-binomial
confidence claims.

Raw widths are summarized within each distribution/sample-size/content/
confidence setting. The .025/.975 width range across datasets is distinct
from posterior width quantiles within one dataset. Posterior-root draws are
not posterior predictive response draws. The DP content check used in
archived target selection does not automatically qualify new posterior-mean
endpoints as tolerance intervals.

The controller generates local analysis and stops with scientific review
required. It does not overwrite the current article tables/figures, change
ECM labels, or modify the dissertation or companion repository. Draw objects,
logs, and working reports remain ignored. Manuscript integration uses only
reviewed actual results and follows STYLE_PROFILE.md.

## Independent Review of Completed Reports

The separate report reviewer can be prepared and tested while the frozen
campaign is running. It reads only the manifest, controller status, and
progress until the controller reports completion and exits successfully.
It does not scan or reassess raw chains, load the reference package, write
inside the campaign, or start an additional process in the background.

```sh
make test-mti-gibbs-full-report-review
make review-mti-gibbs-full-report GIBBS_OUTPUT_DIR=/absolute/campaign/directory
```

For an explicit frozen-source check, use:

```sh
Rscript application/scripts/113_review_mti_gibbs_full_report.R \
  --output-dir=/absolute/campaign/directory \
  --expect-commit=EXACT_FROZEN_LAUNCH_COMMIT --require-complete
```

Without `--require-complete`, an unfinished campaign returns `pending` and
exit code 0. With that flag, it returns the same pending description and
exit code 2. Invalid or inconsistent evidence is an error, with nonzero
exit status. A completed report that passes consistency checks still
requires scientific interpretation; it is not automatically accepted for
the article.

The completed checks verify:

- Successful controller exit, the complete base queue, expected dataset and
  extension counts, and the frozen design hash.
- The report's source and manifest identities and every declared report
  file hash. The frozen writer serializes the eight artifact hashes as an
  array; the reviewer uses its documented file order rather than assuming
  those names survived JSON serialization.
- Fixed fitted content and tilt, dataset identities, endpoint/width
  relationships, retained-draw counts, and separate posterior width and
  endpoint-median summaries.
- Practical and preferred diagnostic flags recomputed from the recorded
  R-hat, ESS, MCSE ratios, and frozen policy. Unresolved diagnostics remain
  visible and do not by themselves invalidate report consistency.
- Complete paired comparisons on the same dataset identities, width
  differences, raw per-cell width means and .025/.975 quantiles, and
  attainment counts and proportions. Simulation binomial intervals are
  independently checked through beta quantiles; application intervals of
  that kind must remain absent for the overlapping partitions.

These checks verify the consistency of saved summaries, not a second
independent assessment from raw draws or a proof of tolerance validity.
They impose no favorable-performance requirement. The 12 saved integration
examples exercise the completed-report calculations, but are explicitly
rejected as a completed full campaign by the top-level reviewer.

Manuscript integration remains a separate decision after reviewing actual
results. Preserve the existing ECM tolerance results and distinguish
posterior uncertainty, mode estimation, and repeated-sampling content
attainment. Any changed prior, recalibration, or new target-selection rule
requires a separately justified study. No new simulation or tuning stage is
required merely to perform this report review.
