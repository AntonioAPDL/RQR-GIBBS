# RQR-DLM main-run wave-1 exdqlm adapter closeout

Date: 2026-07-25 (America/Los_Angeles; execution recorded on 2026-07-26 UTC)

## Scope and disposition

The confirmatory run rooted at
`application/runs/rqr_dlm_main_20260725_cb0c7bb` was stopped deliberately
during its first canonical sentinel wave. The ignored run tree is retained as
failure evidence. It is not eligible for continuation, in-place retry, or
promotion.

The run established that the preceding primary-runtime provenance correction
worked: M01 fits reached the metric and diagnostic publication boundary under
the exact isolated `rqrgibbs` runtime. It then exposed a separate, deterministic
M02 interface defect before the wave completed. Stopping the process group
prevented a known-invalid RQR-DLM versus dynamic-quantile comparison from
consuming the full run budget.

## Exact source and observed outcome

- Authorized source:
  `cb0c7bbbd64195671f515eaf9c027c1eca98f1de`.
- Package version: `0.1.0.9019`.
- Wave: `static_gaussian_T200__target0200__sentinel`.
- Planned wave tasks: 20.
- Workers launched: 8.
- Replications that completed before the stop: S02 replications 69 and 87.
- M01 outcome: completed its fit and published replication-level evidence.
- M02 failure class: `simpleError`.
- Common M02 message SHA-256:
  `ba3212abe25f1e8858a4c0cd61452a4907a17fa6524bad8e5731e293301b29a1`.
- Exact message represented by that digest:
  `Model must be an 'exdqlm' object. To create an 'exdqlm', use functions
  as.exdqlm(), seasMod(), or polytrendMod().`
- Wave completion record: absent because the coordinator process group was
  stopped while the first wave was active.

The two standard-initialization M01 fits were also classified as analyzed
MCMC-diagnostic failures for low `log(q_1)` efficiency. Those outcomes are
legitimate intention-to-run denominator results, not the systemic source
failure that motivated the stop.

## Resource evidence

All sampled `ceiling_exceeded` indicators were zero. Across the eight worker
monitors:

- sampled peak process count ranged from 1 to 3;
- sampled peak thread count ranged from 2 to 4, within the four-thread OS
  envelope;
- sampled peak RSS ranged from 917,512 to 1,028,284 KiB, below the
  1,572,864 KiB ceiling; and
- numerical execution remained limited to one thread per worker.

## Root cause

`rqr_confirm_dynamic_quantile()` removed the native model class with
`unclass()` and passed the resulting ordinary list directly to
`exdqlmMCMC()`. CRAN exdqlm 1.1.0 deliberately validates the model boundary and
accepts only objects created or normalized through its `exdqlm` constructors.
The native list contained the required `m0`, `C0`, `FF`, and `GG` fields, but
field compatibility does not replace the package's class and validation
contract.

## Correction and prevention

The runner now converts each native training model through the attested CRAN
1.1.0 `as.exdqlm()` function. The conversion helper:

1. requires the four state-space fields;
2. uses constructors from the already attested exdqlm namespace;
3. requires the returned object to satisfy `is.exdqlm()`; and
4. verifies, with zero tolerance, that conversion preserves `m0`, `C0`, `FF`,
   `GG`, and their dimensions.

The exdqlm reference gate now exercises the adapter for local-level,
trend-plus-seasonal, and trend-plus-regression structures. A regression test
also rejects a constructor that changes the observation design. The package
development version advances to `0.1.0.9020`.

The read-only health check is separately extended to validate and report one
currently active append-only wave start. Strict replay and continuation remain
fail closed by default.

## Correction validation

The fail-closed correction was validated before authorization:

- `make smoke`: passed;
- the complete standalone DLM contract suite: passed;
- `make test-native`: passed;
- `R CMD check --no-manual`: `Status: OK`, with 444 native expectations,
  zero failures, zero warnings, and zero skips;
- an exact reduced-schedule S02/replication-69 M02 fit and forecast through
  CRAN exdqlm 1.1.0: passed;
- the attested exdqlm reference, including a real adapted fit/forecast and
  local-level, trend-plus-seasonal, and trend-plus-regression conversion
  checks: passed;
- deterministic theory-figure checks: passed;
- `main.pdf`: built successfully at 15 pages;
- `rqr-gibbs-supplement.pdf`: built successfully at 19 pages; and
- the focused pinned-exdqlm RQR smoke suite: passed, with the protected source
  checkout unchanged.

The health checker also read the real stopped run and reported its one active
canonical start, zero terminal waves, and stopped coordinator without
modifying the evidence tree.

## Relaunch rule

The checked-in authorization flag is reset to `FALSE`. A replacement run must
use:

1. a new exact correction commit and freshly built isolated primary runtime;
2. newly rebuilt comparator attestations;
3. passing package, contract, preflight, reference, manuscript, and monitor
   gates;
4. a separate flag-only authorization commit; and
5. a fresh ignored run root and run identifier.

Neither the `cb0c7bb` run nor either completed replication may be reused.
