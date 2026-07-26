# RQR-DLM main-run wave-1 provenance closeout

Date: 2026-07-25 (America/Los_Angeles; completion recorded on 2026-07-26 UTC)

## Scope and disposition

The confirmatory run rooted at
`application/runs/rqr_dlm_main_20260725_224758e` stopped at its first
canonical wave. The run is retained under the ignored local run tree as
failure evidence and is not eligible for continuation or reuse. No later wave
was launched.

This was a promotion-boundary failure, not evidence of a statistical,
numerical, or resource failure in the RQR-DLM sampler. Every worker completed
its first M01 fit and then rejected it because its provenance record was not
promotion eligible.

## Exact source and outcome

- Authorized source: `224758ebb6000c332eaf5ddf173d3e5eb2e5e6c3`.
- Wave: `static_gaussian_T200__target0200__sentinel`.
- Workers used: 8.
- Planned wave tasks: 20.
- Worker exit statuses: eight nonzero statuses.
- Completion decision: `failed`.
- Completion time: `2026-07-26 01:48:10 UTC`.
- Failure class: `simpleError`.
- Common message SHA-256:
  `3e8f305d695e6ac5d4a60b5ebb207a0303d1a1475777e759b6a20cb45e86ec2d`.
- Exact message represented by that digest:
  `A fitted method failed runtime provenance eligibility.`
- Failed fits: M01 in C01 for replications 39, 50, 71, and 72, and M01 in
  C02 for replications 69, 85, 86, and 87.

The wave completion artifact records
`wave_artifact_hashes_sha256 =
c17d68c51c6240234060b57c0ec294673a0757c7322b8d57bec9252c4f90fb0b`.

## Resource evidence

All eight process-group monitors reported `ceiling_reason = none`. Across
workers:

- elapsed time ranged from 432 to 486 seconds;
- sampled peak process count ranged from 1 to 3;
- sampled peak thread count ranged from 2 to 4, within the four-thread OS
  envelope;
- numerical threads per worker remained 1;
- sampled peak RSS ranged from 1,023,560 to 1,026,216 KiB, below the
  1,572,864 KiB ceiling.

The prior correction separating one numerical thread from the four-thread OS
envelope is therefore validated by an actual eight-worker wave.

## Root cause

The confirmatory worker constructed
`provenance_control$primary_runtime_attestation` with
`readRDS(primary_attestation_path)`. The public package provenance contract
requires the path to the RDS file, not its deserialized contents. The package
normalizer consequently coerced the object to a non-path string. The
independent fit-time lineage verifier could not re-read the attestation, so
`fit$provenance$reproducibility_eligible` was false and the promotion boundary
stopped the wave.

The exact immutable primary runtime had already passed the launch-time
binding. The defect was confined to the fit-time handoff of that binding.

## Correction and prevention

The runner now creates its fit provenance control through
`rqr_confirm_primary_provenance_control()`. The helper:

1. requires a full 40-character source commit;
2. requires one existing RDS path;
3. reads the file only to check schema and source-commit consistency; and
4. passes the normalized file path to `rqr_dlm_fit()`, allowing the package to
   independently re-read and verify the complete lineage.

A regression test verifies that the path is retained and that passing the
deserialized object is rejected before any fit begins. The package development
version is advanced to `0.1.0.9019`.

Overleaf had also removed executable modes from seven launch and validation
scripts while merging manuscript updates. Their executable modes are restored
in the same fail-closed correction.

## Relaunch rule

The checked-in confirmatory flag is reset to `FALSE`. A replacement run must:

1. use a new exact correction commit and freshly built isolated runtime;
2. pass package, contract, preflight, reference, manuscript, and monitor gates;
3. use a separate flag-only authorization commit; and
4. write to a fresh ignored run root.

The failed `224758e` run must never be resumed, retried in place, or promoted.
