# Oracle-tilt illustration seed-screen protocol, version 4

## Purpose and interpretation boundary

Version 4 is a prospectively declared, best-of-three screen for clear
single-data illustrations. It is not a repeated-sample simulation study, a
coverage-calibration experiment, or evidence of typical performance. The
underlying fits remain generalized-posterior updates for interval-root
functionals under the declared RQR loss. They are not response-likelihood fits,
and their endpoint draws are not posterior-predictive response draws.

The screen is intentionally separate from the closed version-3 campaign. It
uses new candidate seeds, a new configuration schema, a new output root, and a
new source/runtime receipt. Version-3 evidence remains the manuscript source
unless a complete version-4 result is independently reviewed and explicitly
promoted in a later commit.

## Audit conclusion

Running one replacement seed for each target would make the visual comparison
scientifically incoherent: RQR, equal-tailed (ET), and shortest-window (SH)
panels would no longer describe the same data. The appropriate selection unit
is therefore an entire model family. One fixed-design candidate supplies all
three fixed-design targets, and one DLM candidate supplies all three dynamic
targets. Target-specific seed selection is prohibited.

Three candidates provide a bounded compromise between improving a
single-data illustration and avoiding an open-ended search for favorable
realizations. The candidate set, random streams, selection rule, thresholds,
and tie breakers are frozen before any production data are generated. No
fourth candidate may replace an inconvenient result.

## Frozen scientific construction

The configuration is
`application/config/oracle_tilt_c095_publication_v4_seed_screen_20260805.json`.
It preserves the version-3 scientific construction:

- content `c = 0.95` and fixed learning rate `lambda = 1`;
- standardized asymmetric-Laplace innovations with index `0.80`;
- exact population RQR, ET, and SH tilts, with no Cornish--Fisher
  approximation;
- the 2,400-point, eight-dimensional orthogonalized cubic-spline regression;
- the 1,200-time local-linear plus Fourier-seasonal DLM, including 22 missing
  observations and the declared time-varying scale;
- ridge regression for the fixed design and fixed evolution covariance for the
  DLM;
- the frozen priors, deterministic initialization profiles, diagnostic
  thresholds, and population-recovery summaries.

Each DLM/SH chain retains 12,000 draws from the outset because the closed V3
adjudication established that this schedule is needed for stable SH mixing.
The other DLM targets retain 6,000 draws. Fixed-design chains retain 6,000
draws. There are 81 chains in total:

| Family | Targets | Candidates | Chains per cell | Total chains |
|---|---:|---:|---:|---:|
| Fixed design | 3 | 3 | 4 | 36 |
| DLM | 3 | 3 | 5 | 45 |
| Total | 6 | 3 | — | 81 |

## Random-number contract

Each candidate has one frozen master seed and two separate chain-seed bases.
The master seed defines three named L'Ecuyer-CMRG streams:

1. fixed-design response innovations;
2. DLM state innovations; and
3. DLM response innovations.

The complete seven-integer stream state and its digest are recorded. Stream
materialization preserves the caller's RNG kind and state. Within a candidate
and family, RQR, ET, and SH use exactly the same response and design objects.
Across candidates, response streams differ. The deterministic fixed design,
DLM time grid, scale path, missing mask, and model matrices remain identical.

The canonical data envelope binds source commit, configuration digest,
candidate, family, data digest, target digest, and initialization digest. A
worker cannot resume against an altered envelope.

## Computation and storage

The grid has 18 cells. The production orchestrator starts one process per cell
and each cell runs its chains sequentially. Thus at most 18 fit processes are
active and no cell creates nested chain workers. Numerical-library thread
variables are fixed to one before R starts.

Worker files are atomic and resumable only after validation of their complete
source/config/runtime/data/chain contract. They retain ordered lower and upper
endpoint draws but not latent variables or full state paths. Cell summaries
reconstruct midpoint and width draws exactly, write a cell receipt, and hash
all compact artifacts. Finalization occurs only after all 18 cell processes
exit successfully and every cell bundle is revalidated.

The monitored wrapper uses a dedicated process group, signal and exit traps,
sampled process/R-process/thread/RSS telemetry, a final process-group sweep,
and a 12-hour timeout. It also requires at least 50 GiB free storage, 100 GiB
available memory, 24 estimated idle logical CPUs, and no concurrent heavy
RQR-GIBBS validation. The monitor permits 19 R processes but up to 64 total
processes/threads so short-lived Git and lineage-verification helpers from 18
simultaneous cells do not create a false resource failure. These helpers do
not increase the fit-worker count. Sampled RSS is telemetry, not a kernel-hard
peak. The optional user-level systemd launcher adds a 96 GiB memory ceiling
and a 72-task ceiling.

## Validation stages

The source stages are:

```text
focused source tests
  -> exact-runtime preflight
  -> exact-runtime reference-only suite
  -> two-cell production-schedule benchmark
  -> 18-process endpoint-storage resource rehearsal
  -> independent review
  -> flag-only authorization commit
  -> one production execution
  -> independent selector replay
  -> compact review packaging
  -> optional, separately reviewed manuscript promotion.
```

Preflight materializes all three candidate DGPs and must pass every inherited
scientific-design gate plus cross-candidate checks for 18 cells, 81 unique
chain seeds, nine unique named streams, distinct candidate responses and DLM
states, and common deterministic designs.

The reference-only suite contains the 24 V3 conditional and numerical gates
plus five V4 plan/stream gates. The benchmark runs a complete fixed-design SH
chain and a complete 12,000-draw DLM/SH chain for candidate 1 under the exact
isolated runtime. The resource rehearsal separately exercises the 18-process
cell topology and endpoint-only serialization. A scaled rehearsal is useful
only for source testing; the launch bundle requires `scale = 1`.

Preflight, reference, benchmark, and resource bundles are bound by full source
SHA, configuration SHA-256, runtime-tree digest, closeout digest, artifact
manifest, and monitored-wrapper evidence. Production cannot begin if any
bundle differs.

The runner and wrapper inventories are independently verified. The compact
runner manifest protects the declared scientific artifacts, while the wrapper
manifest must exactly enumerate and rehash every closed-bundle file other than
itself, including logs, resource telemetry, closeouts, and the runner manifest.
Missing, added, or modified files invalidate an input bundle. The host-exclusion
check recognizes R scripts invoked with either absolute or relative
`--file=.../application/scripts/` paths and covers the complete V4 script range,
preventing a second campaign from bypassing the concurrency gate merely because
it was launched from another worktree.

## Cell eligibility

Computational eligibility requires:

- every maintained R-hat, bulk-ESS, tail-ESS, and MCSE gate;
- exact target status and zero numerical repairs;
- complete entry/fit/exit runtime provenance;
- conditional R/C++ parity where applicable; and
- finite, nonpathological endpoint paths.

Strict V3 recovery gates remain reported. To prevent a near-boundary
descriptive metric from automatically deleting an otherwise informative
candidate, V4 also declares a broader gross-recovery envelope before data
generation. A candidate is selectable only if all three family-target cells
are computationally eligible and inside this gross envelope. These rules do
not calibrate empirical coverage and do not convert recovery on one generated
data set into a repeated-sample claim.

## Deterministic selection

For every eligible cell, recovery discrepancies are normalized by their
prespecified reference thresholds. Candidate ranking is performed separately
for the fixed-design and DLM families using, in order:

1. minimum worst standardized discrepancy across all targets and components;
2. minimum mean standardized discrepancy;
3. minimum mean endpoint RMSE divided by population-oracle width; and
4. lowest candidate identifier.

Realized empirical content and aesthetic judgment are excluded from the
score. A candidate with even one ineligible family-target cell is ineligible
for that family. The selector writes the complete cell audit, component table,
family ranking, and two selected candidate identifiers. An independent replay
must reproduce the result after reversing input row order.

This is truth-informed selection for an explicitly illustrative example. Any
paper use must disclose the three-candidate screen and must not describe the
winner as a typical realization or as performance evidence.

## Failure, resume, and promotion rules

- A failed cell writes a structured failure record. Finalization and selection
  stop.
- A resume uses the same source, runtime, configuration, candidate data, and
  chain contract. Completed cells and workers are verified before reuse.
- Thresholds, scores, candidate identities, and seeds cannot be changed after
  observing results.
- Production is disabled in the tracked implementation configuration. It
  requires independent review followed by a commit changing only
  `execution_authorized` to `true`.
- A completed run never changes figures or prose automatically. The packager
  creates compact review evidence only; raw workers remain ignored.
- Manuscript promotion, if approved, requires a separate registry/evidence/
  figure/prose commit and retains V3 as rollback evidence.

## Reproduction interface

Source-only gates:

```bash
make test-oracle-tilt-publication-v4
make oracle-tilt-v4-preflight
make oracle-tilt-v4-reference
```

Exact-runtime stages require a clean reviewed commit and isolated runtime:

```bash
export RQR_EXPECTED_PRIMARY_COMMIT=<40-character reviewed SHA>
export RQR_PRIMARY_RUNTIME_ATTESTATION=<absolute attestation path>
export RQR_ORACLE_TILT_V4_PREFLIGHT_DIR=<exact preflight directory>
export RQR_ORACLE_TILT_V4_REFERENCE_DIR=<exact reference directory>

export RQR_ORACLE_TILT_V4_BENCHMARK_CONFIRM=YES
make oracle-tilt-v4-benchmark

export RQR_ORACLE_TILT_V4_BENCHMARK_DIR=<exact benchmark directory>
export RQR_ORACLE_TILT_V4_REHEARSAL_CONFIRM=YES
make oracle-tilt-v4-resource-rehearsal
```

After independent review and a flag-only authorization commit:

```bash
export RQR_ORACLE_TILT_V4_RESOURCE_DIR=<full-scale resource directory>
export RQR_ORACLE_TILT_V4_CONFIRM=YES
make oracle-tilt-v4-launch

application/scripts/55_oracle_tilt_v4_health.sh \
  --unit=rqrgibbs-oracle-v4-<12-character-sha> \
  --output-dir=<execute directory>
```

After complete execution:

```bash
make oracle-tilt-v4-select ORACLE_TILT_V4_RUN_DIR=<execute directory>
make oracle-tilt-v4-package-evidence \
  ORACLE_TILT_V4_RUN_DIR=<execute directory>
```

Neither command promotes manuscript figures automatically.
