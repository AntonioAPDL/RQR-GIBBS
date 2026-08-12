# TCSP Pilot and Competitor-Readiness Closeout

Date: 2026-08-12
Branch: `feature/tcsp-pilot-competitor-readiness-20260812`
Baseline source: `main@8d7b959aa664a491448e814f6939cd246ad6a3d3`
Status: hardened pilot passed required gates; optional competitors wired but
disabled by default

## Hardened Pilot Result

The clean-source hardened pilot was run from the separate `main` worktree at
commit `8d7b959aa664a491448e814f6939cd246ad6a3d3`.

Output root:
`application/outputs/tcsp_validation_v1/pilot_main_8d7b959_p0_20260812`

The health check passed and the compact audit bundle was copied to:
`docs/audits/tcsp_validation_pilot_p0_20260812`

Audit summary:

- mode: `pilot`;
- rows: 576;
- summary rows: 72;
- failures: 72;
- required gate failures: 0;
- promotion blockers: 1;
- source state clean: true;
- confirmatory ready: false.

The single promotion blocker is expected: this is an 8-replication compact
pilot, not a full-pilot precision run.

## Decision

The pilot passed the gates needed to proceed with competitor wiring. It does
not justify manuscript performance claims, confirmatory execution, or a full
pilot by itself.

## Competitor Wiring

The validation harness now supports optional `tolerance` package wrappers:

- `young_mathew` via `tolerance::nptol.int(method = "YM")`;
- `wald_order` via `tolerance::nptol.int(method = "WALD")`;
- `hahn_meeker` via `tolerance::nptol.int(method = "HM")`;
- `normal_exact_tolerance` via
  `tolerance::normtol.int(method = "EXACT")`.

These wrappers record package provenance, certificate status, selected output
row, number of output rows, and a declared `minimum_width_first_tie` rule when
the package returns multiple candidate intervals.

The default validation config keeps these methods disabled. They are available
for explicitly authorized competitor-smoke and reviewed pilot designs.

## Validation

Passed:

- hardened pilot run;
- hardened pilot health check;
- hardened pilot audit publication;
- copied audit-bundle SHA-256 verification;
- `make test-tcsp-validation`, including optional competitor wrapper smoke;
- `make smoke`;
- `make test-manuscript-language`;
- `R CMD build application`;
- `R CMD check --no-manual --no-build-vignettes rqrgibbs_0.1.0.9035.tar.gz`
  with `Status: OK`.

## Next Step

The next scientific step is not confirmatory execution. The next step is to
review whether the optional wrapper smoke behavior is acceptable and then
decide whether to launch a new full pilot with a deliberately selected
competitor set.
