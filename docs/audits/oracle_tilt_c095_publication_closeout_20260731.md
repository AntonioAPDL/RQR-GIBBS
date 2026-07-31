# Closeout for the 95% oracle-tilt publication illustrations

Date: 2026-07-31

## Decision

The balanced single-data illustration grid completed all 27 planned chains.
Five family/target cells passed every strict diagnostic gate. The dynamic
shortest-window cell passed every hard gate and is retained with an explicit
ESS-only warning. All six cells are therefore eligible for the bounded
didactic illustrations defined by the protocol. The run does not authorize a
coverage-calibration claim, response-predictive interpretation, data-driven
tilt rule, or comparative simulation conclusion.

## Exact source and runtime

| Item | Value |
|---|---|
| Scientific source commit | `90707fcd3f79661fca7358456c6d9a0415bd17a3` |
| Application Git tree | `9c383ff31d0b39ce563369944797e9af90efbe10` |
| Package | `rqrgibbs 0.1.0.9028` |
| Runtime-attestation schema | `rqrgibbs_runtime_attestation/5.0.0` |
| Runtime-tree SHA-256 | `7f9d45dcc5cb80d1e01312b61ee36ec67469bb2a90f857f49409fc2de3be391a` |
| Runtime-attestation SHA-256 | `28f6b45d1a9f368e7758e99505d46ffe5b0659155c050b77ea926a7e54724509` |
| Configuration SHA-256 | `9c0b97e73697a67c99394f94da9a3f639eb903845d814ac453923282f176beb0` |
| R | `4.5.3` on `x86_64-redhat-linux-gnu` |

The run began from a clean `main` worktree at the exact scientific commit and
used an isolated package runtime built from that commit. Every fit reported an
exact fixed-joint target, zero numerical repairs, a matching primary runtime,
and promotion-eligible provenance.

## Frozen design

- content: 0.95;
- innovation: standardized asymmetric Laplace with quantile index 0.99;
- targets: ordinary RQR, exact population equal-tailed tilt, and exact
  population shortest-interval tilt;
- Cornish--Fisher use: none;
- fixed design: 540 observations, four chains per target, 1,000 burn-in and
  6,000 retained draws per chain;
- DLM: 100 times with missing responses at 35, 36, and 70, five initialization
  profiles per target, 2,000 burn-in and 6,000 retained draws per chain;
- common DLM prior: `C0=diag(4, 0.001)` for every target; and
- numerical policy: fail on repair.

## Results and diagnostic disposition

| Family | Target | Endpoint RMSE | Width RMSE | Realized coverage | Mean width | Disposition |
|---|---:|---:|---:|---:|---:|---|
| Fixed design | RQR | 0.825 | 1.177 | 0.967 | 3.484 | strict pass |
| Fixed design | ET | 0.456 | 0.667 | 0.980 | 2.617 | strict pass |
| Fixed design | SH | 0.383 | 0.625 | 0.959 | 2.324 | strict pass |
| Dynamic linear roots | RQR | 0.411 | 0.660 | 0.979 | 3.202 | strict pass |
| Dynamic linear roots | ET | 0.428 | 0.758 | 0.969 | 2.739 | strict pass |
| Dynamic linear roots | SH | 0.887 | 1.516 | 0.959 | 3.148 | ESS-only warning |

For dynamic SH, five of 20 monitored summaries failed only the ESS component
of the strict gate. They were lower, upper, and width summaries at time 51 and
lower and width summaries at time 71. Across that cell:

- maximum rank-normalized R-hat: 1.020224;
- minimum bulk ESS: 214.844;
- minimum tail ESS: 58.345;
- maximum MCSE/SD: 0.064102;
- remote-draw fraction: 0 for every chain;
- maximum width/oracle-width ratio: 10.173, below the declared threshold 20;
  and
- maximum endpoint/response-SD ratio: 11.109, below the declared threshold
  20.

Thus the warning is localized mixing evidence, not a numerical repair,
cross-chain location failure, conditional-reference failure, or remote-scale
mode. It is disclosed in the article and supplement.

## Artifact reconciliation

The scientific runner initially returned a nonzero exit after writing its
complete closeout because its final manifest verifier compared a named R
vector with an unnamed CSV vector using `identical()`. An independent
value-level check confirmed that all 18 compact files matched both their
recorded byte counts and SHA-256 values. Commit
`fd90818` replaced the attribute-sensitive comparison with elementwise value
comparison and added a regression test. No sampler, seed, fit, diagnostic, or
compact scientific result changed, and the 27 chains were not rerun.

The packaged compact evidence is under
`figures/data/oracle_tilt_c095/`. It excludes full fit objects, raw latent
variables, worker envelopes, and local filesystem paths. Its internal evidence
manifest has SHA-256
`e22e6d461300bfb930ec2564ec79d1683f238abc30521ed51f31c05f433cb6da`.

The deterministic renderer verifies the evidence manifest before producing:

- `fig04_fixed_design_oracle_tilt_c095.pdf`;
- `fig05_dlm_oracle_tilt_c095.pdf`;
- `figS03_fixed_design_endpoint_error_c095.pdf`;
- `figS04_dlm_endpoint_error_c095.pdf`; and
- `oracle_tilt_c095_illustration_summary.tex`.

The two main-figure SHA-256 values at closeout are, respectively,
`90de40ccd64523e71f666d009407804094191fe8b6ce5142802789ef1b36387c`
and
`bf4945e9096576d115d244d46904da2c68af166f7dab2c49c59fbe89e6210f2e`.
The figure manifest records all rendered outputs and is regenerated whenever
the plotting source changes.
