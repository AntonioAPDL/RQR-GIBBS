# Second confirmatory startup failure

Date: 2026-07-25

## Status

The fresh run:

```text
run ID:                 rqr-dlm-main-20260725-a570ae3
authorization commit:   a570ae35c7c22e09f9886e9adcca0a3973df6ea8
preflight gates:        22/22 passed
oracle/reference gates: 15/15 passed
```

successfully passed both corrected primary-RDS boundaries, created its
authorization-bound wave contract, and entered the first embedded sentinel
wave. Three of eight sentinel workers then stopped under the process-group
thread monitor. Their sampled peak was three or four operating-system threads
against the declared ceiling of two. The other workers were terminated after
the wave became irrecoverably failed, avoiding unnecessary computation.

The ignored run and supervisor roots are preserved:

```text
application/runs/rqr_dlm_main_20260725_a570ae3/
application/logs/rqr_dlm_main_20260725_a570ae3_supervisor/
```

No wave received a terminal pass, and no result from this run is reusable.

## Cause

The numerical thread contract was obeyed: BLAS/OpenMP and related libraries
were fixed at one numerical thread per worker. The process-group telemetry
counts all operating-system threads and child processes, not only numerical
compute threads.

Each execution worker recomputed the toolchain manifest using two calls to
`R CMD config`. The main R process can contain two operating-system threads,
and the short-lived configuration child processes raised the sampled
process-group total to three or four. Because the monitor samples every 0.2
seconds, some workers observed the transient peak and failed while others did
not. The two-thread envelope was therefore incompatible with the reviewed
worker initialization itself.

## Correction

The execution flag is reset to `FALSE`. Two operational changes are made:

1. `rqr_confirm_toolchain_manifest()` no longer spawns `R CMD config`
   subprocesses. It reads `CC` and `CXX17` directly from the active
   `R_HOME/etc/Makeconf` and binds the complete Makeconf file by SHA-256.
   Reference and execution manifests must remain identical.
2. The sampled process-group envelope is set to four for every monitored
   mode. This remains distinct from numerical compute parallelism, which is
   fixed at one thread per worker. Four is the empirically observed
   fail-closed operating-system envelope for the R runtime plus brief
   validation helpers.

The change does not modify a DGP, method, estimand, seed, replication count,
MCMC schedule, stopping rule, or scientific gate. A new one-line
authorization commit and new exact-runtime preflight/reference evidence are
required before another fresh run ID is launched.
