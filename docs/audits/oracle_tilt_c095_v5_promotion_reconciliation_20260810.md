# Corrected V5 Oracle-Tilt Illustration Promotion

## Decision

The corrected V5 single-data illustration campaign is the active manuscript
evidence. All 27 chains and all six fixed-design/DLM by RQR/ET/SH cells
completed. The campaign passes its hard computational, provenance,
conditional-reference, finite-output, pathology, and broad recovery
requirements with zero numerical repairs. Five cells are strict passes. The
DLM/SH cell is retained as a diagnostic-aware pass with its prespecified
warnings reported below.

This decision does not claim repeated-sample calibration, comparative
superiority, a response likelihood, or posterior-predictive response
simulation. It authorizes only the two frozen-data method illustrations and
their interval-root summaries.

## Immutable identities

| Object | Identity |
|---|---|
| Source commit | 24065941c44a836d2f385b9fe4cf28fcd18d08bd |
| Configuration SHA-256 | e0a603d05e01aecc8f6402d3303d90f62de20b4cdee1fa69f7118419b438f893 |
| Isolated runtime-tree digest | 20ca720b6d0874b11cdab342fcdfddd9be3c271fe81c5724ea6c3ca43a9c3614 |
| Evidence schema | rqrgibbs_oracle_tilt_evidence/5.1.0 |
| Oracle schema | rqrgibbs_interval_oracle/2.0.0 |
| Tilt definition | conditional_retained_mean_minus_population_mean |
| Compact evidence directory | figures/data/oracle_tilt_c095_v5_exact_delta/ |
| Source evidence-manifest SHA-256 | 64db5ac65e7d3645fdeda1c3504735b161340caa17b3b54537f57b7dbdafd186 |
| Source evidence-receipt SHA-256 | 415c543122fe18a8ac1b99e3d9e7713f6a2d7f64bda30f4fd55e127174b79b5c |

The tracked compact bundle contains no fitted-model objects, chain objects,
shared libraries, logs, or response draws. The raw 4.7-GiB run remains under
the ignored application output tree.

## Scientific contract

Both illustrations use content \(c=0.95\) and affinely standardized
asymmetric-Laplace innovations with source index \(0.80\). The exact
innovation-scale tilts are:

| Target | Lower-tail index | Exact tilt |
|---|---:|---:|
| RQR | 0.0120825038669297 | 0 |
| ET | 0.025 | 0.0560608464325982 |
| SH | 0.04 | 0.114721863064410 |

Each tilt is the conditional retained mean of the corresponding probability
window minus the population mean. No Cornish--Fisher approximation is used.
The fixed-design illustration has 2,400 observations and four chains per
target. The DLM illustration has 1,200 times, 1,178 observed responses, two
missing windows, and five chains per target. Every chain retains 6,000 draws.

## Cell results

| Family | Target | Diagnostic rows | Endpoint RMSE/oracle width | Mean-width ratio | Disposition |
|---|---:|---:|---:|---:|---|
| Fixed design | RQR | 93/93 | 0.0605 | 0.9745 | Strict |
| Fixed design | ET | 93/93 | 0.0680 | 0.9749 | Strict |
| Fixed design | SH | 93/93 | 0.0721 | 1.0013 | Strict |
| DLM | RQR | 137/137 | 0.0751 | 0.9509 | Strict |
| DLM | ET | 137/137 | 0.0791 | 0.9559 | Strict |
| DLM | SH | 132/137 | 0.0891 | 1.0220 | Diagnostic-aware |

The DLM/SH warnings are confined to bulk ESS:

| Estimand | R-hat | Bulk ESS | Tail ESS | MCSE/SD |
|---|---:|---:|---:|---:|
| Mean upper endpoint | 1.0058 | 801.2 | 1455.6 | 0.0361 |
| Mean width | 1.0055 | 894.0 | 1773.3 | 0.0340 |
| Upper endpoint, time 481 | 1.0038 | 995.7 | 1225.4 | 0.0342 |
| Upper endpoint, time 780 | 1.0041 | 943.9 | 1166.9 | 0.0354 |
| Upper endpoint, time 1080 | 1.0039 | 984.1 | 1277.2 | 0.0346 |

Thus all DLM/SH R-hat, tail-ESS, and MCSE/SD checks pass. Its
width-contrast relative error is 0.207503 against the narrow 0.20 diagnostic
threshold, while remaining below the prospectively frozen broad 0.40
envelope. No threshold was relabeled, no data seed was screened or replaced,
and no chain was selectively extended.

## Resource and artifact closeout

The monitored execution lasted 17,020 seconds (4 h 43 min 40 s). Sampled
maxima were 7,752,024 KiB RSS, six processes, three R processes, and seven
threads, all below their frozen limits. The runner exited with status zero,
did not time out, did not trigger a sampled resource limit, and left an empty
process group. Sampled RSS is telemetry, not a kernel-hard maximum.

The packager independently reverified the top-level run manifest, all six cell
manifests, all 27 three-phase provenance groups, prerequisite bindings,
reference and benchmark gates, wrapper inventory, resource closeout, and
source/runtime identities before publishing the compact bundle atomically.

## Publication reconciliation

- The campaign registry keeps V2, V3, and the V3 adjudication as historical
  closed campaigns and designates V5 as the active closed campaign.
- The renderer accepts only the explicitly supported V3 and V5 evidence
  schemas and applies schema-specific fail-closed invariants.
- The manuscript and supplement report five strict cells and one
  diagnostic-aware DLM/SH cell; historical V3 revised-tolerance language is
  not reused for V5.
- The generated table labels DLM/SH Diagnostic-aware.
- Figure and table regeneration is deterministic from the tracked compact
  evidence and does not refit a model.

## Validation checklist

- [x] Compact evidence manifest, receipt, and all included hashes verify.
- [x] Campaign registry validates and blocks all further heavy V5 actions.
- [x] Renderer accepts V5 and preserves explicit historical V3 support.
- [x] V5 promotion tests verify identities, cells, warnings, and exact tilts.
- [x] Native/package regression suite passes after the publication changes.
- [x] Main manuscript and supplement rebuild after figure regeneration.
- [x] Final Git diff contains no raw run objects or unrelated simulation edits.

The completed checks were:

| Check | Result |
|---|---|
| Environment smoke test | Pass |
| Exact interval-oracle tests | Pass |
| V5 workflow tests | Pass |
| V5 promotion and registry tests | Pass |
| Historical V3 renderer compatibility | Pass |
| Native R/C++ test suite | Pass, one expected optional-DESN skip |
| Native mean-tilt tests | Pass, one expected optional-DESN skip and two documented warnings |
| Theory figure and table oracle tests | Pass |
| R CMD check --no-manual | Pass, status OK |
| Main manuscript build | Pass, 22 pages |
| Supplement build | Pass, 29 pages |
