# M02 recovery validation and fresh maximum-run launch

## Verdict

The M02 retained-draw correction passed the complete validation ladder and a
fresh diagnostic-aware maximum-design run was launched. The replacement run
uses no fit, chain, checkpoint, wave state, or scientific metric from the
failed `ea8ea8d` run.

The execution source is
`58913f81e2c74fe23eefff737308ff666390b552`, package version
`0.1.0.9035`, and application tree
`65f1913cb76699f11b99d7ab024854dc559225c2`. The primary runtime was built
from the exact committed `application/` Git archive in an isolated library.

## Validation results

| gate | result |
|---|---:|
| standalone RQR-DLM contract suite | pass |
| full `R CMD check --no-manual` | pass, 0 errors / 0 warnings / 0 notes |
| smoke, article, supplement, literature manifest | pass |
| exact-runtime preflight | 23 / 23 |
| oracle/reference checks | 15 / 15 |
| canonical M02 production-path tasks | 20 / 20 |
| interval chains | 44 / 44 |
| endpoint fits | 88 / 88 |
| M02 diagnostics | 900 / 900 |

The smallest bulk ESS in the production-path M02 gate was 465.56, the
smallest tail ESS was 1,069.49, and the largest MCSE/SD was 0.04657. Every
predeclared diagnostic passed its unchanged gate. The validation used eight
parallel processes with numerical libraries restricted to one compute thread
per process.

## Launch

The fresh run is:

```text
run ID: rqr_dlm_diagnostic_aware_maximum_20260808_58913f8
run root: /data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/
          rqr_dlm_diagnostic_aware_maximum_20260808_58913f8/run
control root: /data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/
              rqr_dlm_diagnostic_aware_maximum_20260808_58913f8/control
```

The first published health state reported an active coordinator at PID
`3215900`, zero terminal waves, no failures, and the first canonical sentinel
wave active. The maximum contract contains 110 waves, 8,400
DGP-replication tasks, and 43,800 method-replication results.

This is diagnostic-aware execution. Frozen MCMC diagnostic failures are
retained as nonblocking metadata, while diagnostic-construction, provenance,
source/seed/artifact, target/numerical, nonfinite/order, and resource failures
remain global-stop conditions. Completion will not by itself imply a response
likelihood, posterior-predictive response distribution, or automatic
scientific promotion.

## Protected repositories

The exdqlm reference checkout remained clean at
`dffb71ee70b597d6a716ee74be1cbc99731cd453` and was not loaded or compiled in
place. Its attested CRAN-compatible runtime was used from ignored cache. The
Q-DESN article repository remained read-only and clean.
