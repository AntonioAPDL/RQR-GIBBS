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

## Gibbs Study: Awaiting Confirmation

No new Gibbs simulation or application study is authorized by the article
recovery. The user must first confirm the revised article. The next comparison
will hold data, fitted content, tilt, learning rate, and Gaussian priors fixed
across Gibbs and ECM. It will assess root posterior summaries, objective values,
chain mixing, effective sample sizes, and computing cost. Endpoint posterior
means or medians need not equal an ECM mode. Any new tolerance procedure based
on these summaries requires its own assessment; a scan guarantee does not
automatically transfer from an order-statistic interval to different endpoints.

The prepared settings are in
`application/config/mti_gibbs_ecm_comparison_20261111.json`. Preflight is safe
before confirmation:

```sh
make preflight-mti-gibbs
Rscript application/scripts/105_compare_mti_gibbs_ecm.R --mode=preflight --stage=full
```

The diagnostic stage contains 82 datasets and 328 independent chains: one
replication in each of 72 simulation cases and one partition for each
response/partition type in the application. Four dispersed chains use 2000
burn-in cycles and 4000 retained draws each. The full prepared design contains
75,004 datasets and 300,016 chains. These counts are a design, not completed
calculations. Its duration must be measured from the diagnostic stage before
the full run is authorized.

After explicit user confirmation, freeze the article commit, install this
repository's package from `git archive` under its ignored cache, and launch
only the diagnostic stage. Up to 50 workers are supported, with one chain per
worker and one BLAS/OpenMP thread. Start with 32 or 40 according to current
server load. No external source checkout is compiled, installed, or loaded.
The external exdqlm reference commit and an ignored-file-inclusive before/after
checkout comparison are recorded, even though the ridge sampler does not use
that package. `EXDQLM_RQR_REPO` can identify its protected checkout.

```sh
make prepare-mti-gibbs-runtime
# Run only after the author has confirmed this article commit.
commit=$(git rev-parse HEAD)
R_LIBS="application/cache/mti_gibbs_runtime_$commit/library" \
  OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
  Rscript application/scripts/105_compare_mti_gibbs_ecm.R --mode=run \
  --stage=diagnostic --workers=40 --confirm-article-commit="$commit"
```

The diagnostic assessment uses rank-normalized split R-hat, bulk effective
sample size, and tail effective sample size for ordered endpoints and width.
The settings require R-hat at most 1.01 and both effective sample sizes at
least 400 in every diagnostic dataset before a full run is allowed. These are
diagnostic criteria, not a theorem of convergence. Atomic per-chain files
preserve draws, target-loss traces, retained penalized objectives, sample
identity, numerical repair records, and elapsed time. An interrupted run can
resume with `--resume=true` only when its commit, settings, jobs, targets, and
seeds agree. Completed chains are retained.

Use `--mode=health` for progress and a throughput-based remaining-time estimate,
or `--mode=assess` to regenerate the posterior summary after completion. The
study never replaces manuscript inputs automatically. Sampling at the
ECM-selected target isolates numerical computation and conditional posterior
uncertainty; it is not a fresh search over every content/tilt candidate, and
the ECM probability cutoff is not assumed to calibrate Gibbs endpoint summaries.

The historical convex-profile calculation and its results are preserved as a
numerical reference. They are not relabeled as ECM or Gibbs results. External
repositories and dissertation sources are not modified by this recovery.
