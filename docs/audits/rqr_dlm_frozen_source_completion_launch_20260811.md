# RQR-DLM frozen-source diagnostic-aware completion launch

Date: 2026-08-11

## Source and runtime

| Field | Value |
| --- | --- |
| Source commit | `3e8e50b262d3101bf9d253620961207579748b59` |
| Source branch contract | clean detached exact launch-source worktree |
| Detached launch worktree | `/data/muscat_data/jaguir26/.rqr_gibbs_launch_sources/rqr_dlm_3e8e50b262d3101bf9d253620961207579748b59` |
| Isolated primary runtime | `/data/muscat_data/jaguir26/.rqr_gibbs_launch_sources/.rqr_gibbs_primary_runtime/3e8e50b262d3101bf9d253620961207579748b59/library/rqrgibbs` |
| Primary runtime digest | `d470d0bc4c437a5015041036afc4eefea91cff627b1274d4e31664fcd3c57a72` |
| Primary runtime attestation | `/data/muscat_data/jaguir26/.rqr_gibbs_launch_sources/.rqr_gibbs_primary_runtime/3e8e50b262d3101bf9d253620961207579748b59/attestations/rqrgibbs_3e8e50b262d3101bf9d253620961207579748b59.rds` |

The detached launch worktree was validated with the generated environment
file.  Direct binding and a package-level provenance probe both returned
`primary_runtime_source_match=TRUE` and `reproducibility_eligible=TRUE`.

## Fresh run

| Field | Value |
| --- | --- |
| Run ID | `rqr_dlm_frozen_source_completion_20260811_3e8e50b` |
| Run root | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_frozen_source_completion_20260811_3e8e50b/run` |
| Input root | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_frozen_source_completion_20260811_3e8e50b/inputs` |
| Supervisor logs | `/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_frozen_source_completion_20260811_3e8e50b/logs/supervisor` |
| Coordinator PID at launch | `269449` |

## Prelaunch gates

| Gate | Status |
| --- | --- |
| Detached launch-source runtime build | passed |
| Runtime binding smoke | passed |
| Confirmatory preflight | passed |
| Oracle/reference stage | passed |
| Diagnostic-aware authorization bundle | passed |
| Hermetic promotion checks retry2 | passed |

The complete promotion check bundle passed:

```text
install_check_runtime
make_smoke
test_standalone_contracts
test_native
test_native_mean_tilt
test_oracle_tilt_illustrations
package_check
prepare_pinned_exdqlm_runtime
test_exdqlm_rqr
prepare_document_source
make_pdf
make_supplement
make_literature_manifest
```

The first interactive promotion-check attempt was interrupted by a SIGTERM
during `R CMD check`; a detached `setsid` retry completed and is the
promotion evidence for this launch.

## Initial health snapshot

The first health check after the coordinator entered wave execution reported:

| Quantity | Value |
| --- | ---: |
| Coordinator running | TRUE |
| Terminal waves | 0 / 110 |
| Active canonical wave | `static_gaussian_T200__target0200__sentinel` |
| Next canonical wave | `local_level_gaussian_T200__target0200__sentinel` |
| Terminal DGP-replication tasks | 0 / 8400 |
| Completed method-replication results | 0 / 43800 |
| Frozen diagnostics passed | 0 / 0 |
| Failed waves | 0 |

## Interpretation

This launch changes execution isolation only.  It does not change the
RQR-DLM target, MCMC transition kernels, diagnostic thresholds, response
generation design, or scientific interpretation.  RQR-DLM remains a
loss-based generalized-Bayes update for interval-root state paths, not an
ordinary response-likelihood model and not a posterior-predictive response
simulator.
