# Young-Mathew Add-On for the Main Bayesian-UQ Validation

This note records the source-controlled procedure for adding the
Young-Mathew nonparametric tolerance comparator to the active main Bayesian-UQ
validation without mutating the frozen 10-method launch.

## Decision

- Keep the main confirmatory run frozen at its original method grid.
- Relaunch incomplete main waves from the canonical source commit recorded by
  the first completed waves.
- Run Young-Mathew separately with the same DGP grid, sample sizes, target
  contents, tolerance confidence, posterior-threshold duplication, replication
  count, and paired dataset seed rule.
- Combine YM with the collected main results only after the main wave run has
  finished and has been collected.

## Rationale

The main run already contains valid completed waves under a fixed source
commit. Adding YM directly to that frozen run would invalidate the row-count
contract and would require rerunning all completed main methods. A separate
add-on gives the same repeated-sample comparison while preserving the completed
main evidence and making the external package dependence explicit.

Young-Mathew is recorded as an external nonparametric tolerance comparator via
`tolerance::nptol.int(method = "YM")`. Its package-level nominal construction is
included as empirical validation evidence. It is not promoted as an
independently audited TCSP scan theorem.

## Commands

The main relaunch uses the canonical commit from the completed wave manifests:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
VECLIB_MAXIMUM_THREADS=1 RCPP_PARALLEL_NUM_THREADS=1 \
R_LIBS_USER="/data/muscat_data/jaguir26/RQR-GIBBS/application/cache/r_libs/rqrgibbs_79a002c87969e6a39089fc3c5d9c5b3315b7dc84:/home/jaguir26/R/x86_64-redhat-linux-gnu-library/4.5" \
Rscript /data/muscat_data/jaguir26/.rqr_gibbs_launch_sources/rqr_bayes_uq_79a002c87969e6a39089fc3c5d9c5b3315b7dc84/application/scripts/71_manage_rqr_bayes_uq_main_waves.R \
  --action=launch \
  --mode=confirmatory \
  --run-dir=/data/muscat_data/jaguir26/RQR-GIBBS/application/runs/rqr_bayes_uq_validation_main_20260813/wave_main_20260813T103232Z \
  --max-concurrent=40 \
  --poll-seconds=60
```

The YM add-on is launched independently:

```sh
Rscript application/scripts/74_validate_rqr_bayes_uq_young_mathew_addon.R \
  --mode=confirmatory \
  --main-run-dir=/data/muscat_data/jaguir26/RQR-GIBBS/application/runs/rqr_bayes_uq_validation_main_20260813/wave_main_20260813T103232Z \
  --workers=8
```

After the main run is collected, bind the two result sets with:

```sh
Rscript application/scripts/75_collect_rqr_bayes_uq_with_young_mathew.R \
  --main-run-dir=/data/muscat_data/jaguir26/RQR-GIBBS/application/runs/rqr_bayes_uq_validation_main_20260813/wave_main_20260813T103232Z \
  --young-mathew-dir=<published-young-mathew-addon-dir>
```

## Expected Scope

For the current confirmatory grid, YM contributes 12,600 rows:

- 7 DGP families;
- 2 sample sizes, `n = 500, 1000`;
- 3 target contents, `0.90, 0.95, 0.99`;
- 1 tolerance confidence, `0.95`;
- 3 posterior-threshold labels retained for schema compatibility;
- 100 replications.
