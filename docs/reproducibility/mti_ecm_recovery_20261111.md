# ECM Results and the Gibbs Comparison

The current tolerance comparison reports genuine finite-iteration MTI-ECM fits,
with a separate direct Dirichlet-process posterior for interval content. The
fixed-target root update is a loss-based generalized posterior. ECM approximates
its mode; Gibbs sampling targets the full fixed-target distribution. A DP
content probability is distinct from a generalized-posterior root probability.

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
```

The recovery reads the DGP definitions at the archived commit without executing
the historical study launcher. Each sample is reconstructed from its stored
seed and checked against its recorded skewness and tail ratio. Stored endpoint
content is checked against the specified population CDF. Young--Mathew and
Wilks are evaluated on those same samples, and each method row retains a sample
digest. Earlier comparator results with different seeds are not paired with the
ECM fits. Completed distribution/design cases are checkpointed atomically.

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

The historical convex-profile calculation and its results are preserved as a
numerical reference. They are not relabeled as ECM or Gibbs results. External
repositories and dissertation sources are not modified by this recovery.
