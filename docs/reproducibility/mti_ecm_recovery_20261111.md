# ECM Results and the Gibbs Comparison

The current tolerance comparison reports genuine finite-iteration MTI-ECM fits,
with a separate direct Dirichlet-process posterior for interval content. The
fixed-target root update is a loss-based generalized posterior. ECM approximates
its mode; Gibbs sampling targets the full fixed-target distribution. A DP
content probability is distinct from a generalized-posterior root probability.

## Retained Theory and Algorithms

Main Section 5 and supplement Part III state the normalized fixed-target
generalized posterior, its normal-exponential augmentation, the Gaussian
conditional distributions for both roots, and the GIG scale conditional with
its Gamma limit at a zero residual. The supplement proves propriety and finite
polynomial moments under proper Gaussian root priors. Fixed nonzero tilt shifts
the conditional Gaussian means without changing their precisions, so the
conditional conjugacy applies both to MPI and to fixed-tilt MTI. This does not
make the joint posterior Gaussian or define a response likelihood.

The ECM E-step and both conditional maximizations are derived from that same
augmented posterior. The selected empirical fits retain their original finite
iteration limits, residual flooring, prior precision, and learning rate. The
population MPI/MTI identification results, empirical score identities, scan
reference, and direct DP content law are retained. The numerical convex-profile
solver theorem and its exposition are removed from the manuscripts; their code
and historical evidence remain in the repository as separate numerical work.

The original RQR article of Pouplin et al. (2024), Section 3.2 and Appendix H.1,
introduces empirical product-loss minimization and reports Adam optimization.
Its general discussion of Bayesian methods in Appendix A does not derive the
present generalized posterior or its Gaussian/GIG full conditionals. The
manuscript credits that original loss, generalized-Bayes updating, the
normal-exponential representation, and EM/ECM to their respective sources.

`tables/mti_gibbs_comparison_main.tex` and
`tables/mti_gibbs_comparison_supplement.tex` are empty inclusion points. They
print no claims or values. Completed Gibbs results can be incorporated there
only after checking posterior diagnostics and the matched-target comparison;
the emptiness check in the manuscript validator must then be revised together
with the assessed results. No Gibbs/ECM agreement or computing-time claim is
inferred from the ECM archive alone.

The manuscript revision was checked with:

```sh
make test-mti-ecm-recovery test-manuscript-language
make verify-mti-ecm-article test-theory-tables test-theory-figures
make test-pharma-mti-application test-pharma-source
Rscript application/scripts/104_recover_mti_ecm_article.R --mode=verify --full=true
make preflight-mti-gibbs
make all-pdf
```

The mathematical tests check both root conditionals at zero and nonzero tilt,
the GIG/Gamma scale kernels, the integrated normal-exponential identity, and
the two ECM conditional maximizations with fixed E-step weights. The Gibbs
preflight creates a design without sampling. Application tests omit their
optional raw-data provenance check because that check belongs to the separate
application preflight; the tracked evidence, paired tables, figures, and source
structure checks pass. The revised main article has 25 pages and the supplement
41 pages. Their final compilation logs contain no undefined references,
undefined citations, or overfull boxes.

## Reproducing the Simulation Comparison

`application/config/mti_ecm_article_20261111.json` identifies the archived
72,000 ECM fits, their SHA-256 digest, the numerical commit, and the previously
fixed probability cutoffs. Large archived objects remain local. On a machine
holding that archive, run from this repository:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 Rscript \
  application/scripts/104_recover_mti_ecm_article.R --mode=recover \
  --archive-root=/data/muscat_data/jaguir26/RQR-GIBBS --workers=32
Rscript application/scripts/104_recover_mti_ecm_article.R --mode=tables
Rscript application/scripts/104_recover_mti_ecm_article.R --mode=independence \
  --archive-root=/data/muscat_data/jaguir26/RQR-GIBBS
Rscript application/scripts/104_recover_mti_ecm_article.R --mode=verify --full=true
```

The recovery reads the DGP definitions at the archived commit without executing
the historical study launcher. Each sample is reconstructed from its stored
seed and checked against its recorded skewness and tail ratio. Stored endpoint
content is checked against the specified population CDF. Young--Mathew and
Wilks are evaluated on those same samples, and each method row retains a sample
digest. Earlier comparator results with different seeds are not paired with the
ECM fits. Completed distribution/design cases are checkpointed atomically.
The Gamma distribution's density is singular at its lower boundary. Its CDF
check allows only the change implied by a small floating-point perturbation of
the saved decimal endpoints. All recorded attainment indicators agree with the
recomputed CDF classifications. A completed, verified recovery is reused
without refitting comparators when the recovery command is repeated.
The calibration archive contains 720,000 method rows on 72,000 dataset seeds;
none of these seeds occurs among the 72,000 independent validation samples.
The corresponding source hashes and counts are recorded separately.

Small case summaries, paired differences, convergence summaries, and input
hashes are committed under `figures/data/mti_ecm_validation_20261111/`. Full
replication-level results remain in the ignored cache. Width ranges are empirical
2.5% and 97.5% quantiles within a distribution/design case. Attainment estimates
use the true population CDF. An ECM stopping flag is reported separately from
interval validity or repeated-sampling attainment; a finite iterate is not
automatically a global mode.

## Gibbs Study: Authorized Staged Launch

The user has explicitly authorized the staged Gibbs simulation and application
comparison. Each launch still requires confirmation of the exact committed
article source. Authorization does not establish that a stage has run or passed.
The comparison holds data, fitted content, tilt, learning rate, and Gaussian
priors fixed across Gibbs and ECM. It assesses root posterior summaries,
objective values, chain mixing, effective sample sizes, and computing cost.
Endpoint posterior means or medians need not equal an ECM mode. Any new
tolerance procedure based on these summaries requires its own assessment; a
scan guarantee does not automatically transfer from an order-statistic interval
to different endpoints.

The launch settings are in
`application/config/mti_gibbs_ecm_comparison_20261111.json`; the campaign logic is
in `application/scripts/lib/mti_gibbs_comparison.R`, called by script 105.
Diagnostic and full stages use four dispersed chains, each with 2000 burn-in
cycles and 4000 retained draws, with no thinning. Preflight prepares the paired
design without Gibbs sampling:

```sh
make preflight-mti-gibbs
Rscript application/scripts/105_compare_mti_gibbs_ecm.R --mode=preflight --stage=full
```

The diagnostic design includes the base replication in each simulation case
and base partitions for each application response/partition type. It also adds
the archived maximum-q and maximum-absolute-tilt samples in each simulation
case, with deterministic first-occurrence tie breaking. Overlapping selections
are retained once. The measured diagnostic design contains 174 datasets:
82 base cases plus 92 additional deterministic stress cases. Four chains per
dataset give 696 independent chains. Preflight and the frozen
`jobs.csv`/`jobs.rds` record the design identities. These measured design totals
are not completed Gibbs calculations or evidence of diagnostic passage.

The campaign first performs sampler smoke checks, then short timing benchmarks
at eligible worker counts from 32, 40, and 50, limited by `--workers` and the
available allocation. The timing design selects up to 36 simulation datasets
with four chains, 50 burn-in cycles, and 150 retained draws per chain. These
short runs measure throughput, not convergence or Gibbs/ECM agreement. The
campaign records `worker_benchmark.csv` and uses the fastest measured worker
count for the diagnostic and any permitted full stage. Each worker uses one
BLAS/OpenMP thread; script 107 sets thread limits before R starts.

### Runtime and Frozen Source

`make prepare-mti-gibbs-runtime` calls script 106 from a clean committed source
checkout. Script 106 creates or reuses a detached worktree at that exact commit
under the common RQR repository's `.codex_work/mti_gibbs_launch_<commit>/`.
Its `application/cache` links to the preparing checkout's RQR-owned ignored
cache, sharing runtime artifacts and recovered ECM inputs. The detached source
remains fixed while the working checkout can evolve.

From that frozen worktree, script 106 calls the existing
`application/scripts/04_prepare_primary_runtime.R` with the expected commit,
explicit detached-launch-source authorization, and the runtime root set to
`application/cache/mti_gibbs_attested` in the preparing checkout. The builder
uses `git archive` of the exact committed `application/` subtree, then builds
and installs only in the isolated cache under
`application/cache/mti_gibbs_attested/<commit>/`. It writes the full
`rqrgibbs_runtime_attestation/5.0.0` record at
`attestations/rqrgibbs_<commit>.rds` within that directory. The attestation binds
the Git source and archive, built source package, build/install command
receipts and logs, installed package tree and lineage marker, unchanged frozen
source, and R/platform identity.

Script 106 exposes
`application/cache/mti_gibbs_runtime_<commit>/library` as a symlink to the
attested library and writes the attestation location to
`application/cache/mti_gibbs_runtime_<commit>/attestation_path.txt`.
The runner checks the loaded namespace path, attested source commit and runtime
path, and a fingerprint covering runtime files and the attestation. `mgc_fit`
passes the attested source root, expected commit, and
`primary_runtime_attestation` to the sampler. A chain is accepted only when
`fit$provenance$reproducibility_eligible` is true, in addition to numerical
eligibility and zero numerical repairs; a commit marker alone is insufficient.

The required `posterior` and `parallelly` packages must also be available.
No external source checkout is compiled, installed, or loaded. The exact RQR
commit and pinned external exdqlm commit are recorded before sampling, along
with an ignored-file-inclusive before/after checkout guard.
`EXDQLM_RQR_REPO` can identify the protected external checkout.

The standalone `application/scripts/107_launch_mti_gibbs.sh` wrapper also
requires a clean committed source checkout. It verifies and reuses the same
detached launch worktree and linked RQR-owned cache, without using an external
source checkout as a runtime.

Set `commit` below to the explicitly approved 40-character article-source SHA.
Runtime preparation installs the package but does not sample. The wrapper's
dry run prints the frozen source, commit, output directory, worker ceiling,
and full-stage permission without creating a worktree or starting the campaign:

```sh
commit=CONFIRMED_40_CHARACTER_ARTICLE_COMMIT
make prepare-mti-gibbs-runtime
bash application/scripts/107_launch_mti_gibbs.sh --dry-run \
  --confirm-article-commit="$commit" --workers=50 --allow-full=true
```

Choose one launch command; do not start competing controllers. Without
`--allow-full=true`, the campaign stops after diagnostics. With that flag, it
may proceed to the full comparison only after the diagnostic gate passes:

```sh
# Smoke checks, timing benchmarks, and diagnostics only.
bash application/scripts/107_launch_mti_gibbs.sh \
  --confirm-article-commit="$commit" --workers=50

# Alternatively, permit the identity-gated full stage as well.
bash application/scripts/107_launch_mti_gibbs.sh \
  --confirm-article-commit="$commit" --workers=50 --allow-full=true
```

The wrapper starts a detached controller and prints its PID, campaign directory,
and log path. An optional `--output-dir` must be absolute. By default, campaign
outputs are under the frozen worktree's ignored `application/outputs/` tree.
`controller.log`, `controller.pid`, `controller_exit.json`, and
`campaign_status.json` record controller progress and exit status. These commands
document the launch procedure; they are not a record of completed runs.

### Assessment, Storage, and Resumption

The full-stage gate requires a complete, passing diagnostic assessment whose
commit, settings, diagnostic design, input hashes, and runtime fingerprint
match the requested study. A successful external-checkout guard is also
required for assessment passage. For ordered endpoints, width, and midpoint,
every diagnostic dataset must have rank-normalized split R-hat at most 1.01,
bulk and tail effective sample sizes at least 400, and Monte Carlo standard
error of the mean at most 0.05 times its posterior standard deviation. These
are diagnostic criteria, not a theorem of convergence. A failed diagnostic
stage stops the campaign without silently increasing the sampling budget.

Compact full-stage records preserve every retained ordered-endpoint draw,
penalized-objective summaries, sample identity, zero-repair status, and elapsed
time. Width and midpoint are derived during assessment. Diagnostic records
add warmup root traces, the full target-loss trace, retained penalized
objectives, and the sampler's last state, including its RNG state. Full fitted
objects and latent-draw histories are not saved. Disk projections include an
overhead allowance and reserve; chains run in bounded batches, with
incremental dataset assessment.

Repeating the wrapper command for the same frozen campaign resumes its stages.
The direct script-105 run interface also supports `--resume=true`. Source,
settings, jobs, inputs, and runtime identity must agree; completed chain files
and their checksum receipts are validated and reused, and only unfinished
chains are scheduled. This is completed-chain reuse, not continuation of a
partially sampled chain. Study locks reject competing controllers.

Use script 105's `--mode=health` with an explicit stage `--output-dir` for
progress, diagnostic status, disk availability, and a stratified remaining-time
estimate. `--mode=assess` regenerates summaries from frozen records;
`--mode=report` writes a convergence PDF, which the campaign produces for
diagnostics. The study never replaces manuscript inputs automatically.
Sampling at the ECM-selected target isolates numerical computation and
conditional posterior uncertainty; it is not a fresh search over every
content/tilt candidate, and the ECM probability cutoff is not assumed to
calibrate Gibbs endpoint summaries.

The historical convex-profile calculation and its results are preserved as a
numerical reference. They are not relabeled as ECM or Gibbs results. External
repositories and dissertation sources are not modified by this recovery.
