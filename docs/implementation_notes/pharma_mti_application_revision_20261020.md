# Pharmaceutical MTI Application Revision

## Revision Identity

The audited baseline was `e227777a9f76d8a91a2a7c35b5b6ac5b6eabe61e`, with
a clean checkout. Work was isolated on the local branch
`codex/pharma-mti-application-revision-20261020` in
`/data/muscat_data/jaguir26/.rqr_gibbs_worktrees/pharma_mti_application_revision_20261020`.
The original checkout and the companion MTI-EXTENSIONS checkout were left
unchanged. No push, pull request, or merge is authorized for this revision.
The companion remains at `f345d946aa5a81b94795838bec58d874a0fdd0c9`;
its before/after status includes the same ignored TeX artifacts. Its older
README framing was noted but not revised in this read-only task.

The calculation sources were tested and committed as
`9524e80bb4c62085113739d291a6c9c7a88eb186` before the full run. Uncommitted
reporting work at launch is listed in the run manifest. Calculation-source
hashes were checked before and after the run. Subsequent changes add portable
endpoint precision and reporting; they do not change fitted MTI intervals.

## Statistical Decisions

MTI-ECM is the only proposed application procedure. Young--Mathew and Wilks
min--max are external benchmarks. The new dispatcher contains no TCSP fit,
task, or result row. Historical functions and configurations remain available
for reproduction. The retained count 98, or reference fraction 0.98, is an
internal calibration anchor for the frozen MTI fitted-content search. It is
neither fitted nor reported as another method.

The frozen MTI policy, posterior probability threshold 0.995, learning rate,
prior, candidate menus, C++ fitting backend, 100-iteration cap, and stationarity
threshold 0.001 are unchanged. No application-specific tuning was performed.
The target content is 0.90, nominal tolerance confidence is 0.95, and each
training/held-out partition contains 100/87 batches.

### Data and Integrity

The public source is Zagar and Mihelic's `Laboratory.csv`, Figshare item
16676263, version 1, file 30966250, collection 5645578, licensed CC BY 4.0.
The source article is DOI `10.1038/s41597-022-01203-x`.

| Input | Integrity Record |
| --- | --- |
| Laboratory.csv | 293492 bytes; MD5 `74b091a88dc88e0fed44f79877b22b66` |
| Laboratory.csv SHA256 | `a3b0f16c7c3fa8052b38d767e38fda731a751acc4ec139b52d01577b32dc6483` |
| Historical application configuration | `7f1449cefd1e57a146c514ec8d12ef19413ef3617897e14d2312b11f97c096ae` |
| Frozen MTI policy | `9942e5c9aad8701e125b4caf0c84df292a69886b7d1e176e44f512e6ea99b773` |
| Frozen validation configuration | `c630d04dad2e45b2ea9cca4d01ce00f9e7132f74fd23f27108d2e341ae961aa7` |
| Retained-count calibration table | `53d4f403888c28362bbb07fdce40328278c7f969a8865406073dcf9960b0d3b0` |

All source and frozen-input hashes above are preserved. The source has 1005
batches and 55 fields; product code 23 provides 187 batches. Tensile strength
(`fct_tensile`) remains the main response, with tablet-core weight relative
standard deviation (`tbl_rsd_weight`) supplementary. Neither response has
missing values in the selected product. The source does not resolve the
physical unit of the derived tensile-strength measure or provide acceptance
criteria. Neither is inferred from the data.

### Partitions

The new versioned configuration is
`application/config/pharma_tolerance_application_mti_only_20261020.json`.

| Design | Partitions Per Response | Construction |
| --- | ---: | --- |
| Random | 1000 | Seeded sampling without replacement, 100 training batches |
| Forward chronology | 1 | First 100 train; last 87 held out |
| Reverse chronology | 1 | Last 100 train; first 87 held out |
| Production month | 250 | Distinct whole-group partitions with exactly 100 training batches |
| API batch | 250 | Distinct whole-group partitions with exactly 100 training batches |

Chronology uses recorded batch order and original source-row tie breaking.
All methods and both responses share memberships. Primary split seeds are
`9020902 + 101 + split_id`; method-specific offsets are explicit in the
configuration. This new version replaces the historical runner's implicit
random-number sequence; historical configurations and outputs are untouched.
Worker order and worker count cannot change a partition or fitting seed.

Grouped allocation uses suffix subset counts and fixed random seeds, without
responses. There are 8287 exact-size partitions among the 19 months and
220304429328877 among the 53 API groups. Exact training size is preferable to
an approximate size here because the frozen policy is calibrated for n=100.
Duplicate partitions are rejected; exhaustion and draw-limit failures are
explicit. No campaign labels were invented.

## Computation and Canonical Evidence

The full run comprises 3004 response-partition tasks and 9012 method rows.
It completed in 376.043 seconds with 40 single-threaded workers; all methods
returned 3004 intervals. R version was 4.5.3, rqrgibbs 0.1.0.9036, tolerance
3.0.0, Rcpp 1.1.1.1.1, RcppArmadillo 15.2.6.1, digest 0.6.39, jsonlite 2.0.0.
The manifest records compiler, BLAS/LAPACK, operating system, shared-library
hash, seeds, timestamps, and exact source identities. The configured external
exdqlm pin is `dffb71ee70b597d6a716ee74be1cbc99731cd453`; exdqlm was not
loaded or built for this application.

Mean recorded per-fit elapsed times across all schemes were 3.339 seconds for
MTI-ECM and 0.028 seconds for Young--Mathew; Wilks was below the timer's useful
resolution in many tasks. `tables/pharma_application_runtime_summary.csv`
separates timings by response, method, and partition design. Summed worker
seconds are not wall-clock runtime and should not be interpreted as serial
benchmarks under this concurrent run.

The original full run is preserved under the ignored directory
`application/outputs/pharma_mti_application_20261020/confirmatory_20261020/`.
The tracked, portable reporting evidence is
`figures/data/pharma_mti_application_20261020/`. Its `results.csv` SHA256 is
`223eeab04e8acd4dcda8657a4012f7413b81ad0f7e3cc83bada7e6e7c9e89e3c`.
The JSON manifest contains hashes of all canonical files and derived tables
and figures; its own hash is reported separately at delivery to avoid a
self-referential hash.

The result schema includes response/method roles, split scheme and IDs, all
seeds, actual sizes and group counts, requested content and confidence,
returned endpoints and width, empirical content and tail omissions,
attainment, selected fitted content and tilt, posterior content probability,
threshold, candidate and feasible counts, convergence flag, iteration count,
stationarity, failure reason, package version, and elapsed seconds. The
compressed membership CSV identifies every batch's training/held-out role.
Observed batches, frozen configuration, representative splits, summaries,
failures, progress, and sessionInfo are also tracked. Large fitted objects
are neither needed nor committed.
The hashed sessionInfo record is copied verbatim, including R's right-padding
spaces; a single-file Git whitespace attribute preserves that evidence
without exempting source code or manuscript files from whitespace checks.

### Endpoint Precision Recovery

Independent reconstruction of every held-out evaluation found five
Young--Mathew endpoints whose original CSV serialization rounded an endpoint
onto a tied observation. All five affected production-month partitions for
tensile strength. No MTI result or posterior content probability was affected.
The original in-memory empirical evaluations were correct and were retained.

`application/scripts/88_recover_pharma_application_csv_precision.R` recovers
only those five deterministic benchmark fits using their original samples,
seeds, and tolerance 3.0.0. Recovered endpoints differ from serialized values
by at most 1e-13 and reproduce every original empirical evaluation. The script
stops for any other discrepancy, writes to a fresh directory, and records
original hashes and changed rows. No MTI fit is repeated; no interval is
expanded or shifted to improve its result. `endpoint_precision_recovery.csv`
and the manifest document the recovery. The new CSV writer uses 17 significant
digits for round-trip-safe doubles, with a regression test for tied endpoints.

## Reporting and Scientific Interpretation

All method and paired summaries use the canonical results. Quantiles use R
type 8. Failures remain in attainment denominators and are excluded only from
conditional endpoint/width summaries. The selected representative split
minimizes squared distance from componentwise medians over all methods'
endpoints, width, empirical content, and tail omissions, scaled by R's
normal-consistent MAD, with IQR and then one as zero-scale replacements. The
smallest split ID breaks ties. The selected random splits are 479 (tensile)
and 540 (RSD); their indices and scores are tracked.

Tensile median widths are 0.553 (MTI-ECM), 0.533 (Young--Mathew), and 0.665
(Wilks). The median paired MTI-minus-Young--Mathew width is +0.017: there is
no median width improvement for this response. RSD median widths are 2.000,
2.262, and 3.400, respectively, but omitted upper-tail observations are
scientifically adverse. A one-sided upper limit would be more natural for a
direct quality-control decision. These results describe placement and finite
data stability, not product specifications or release limits.

Selected convergence flags are TRUE in only 90/1000 tensile and 14/1000 RSD
random-split fits; across all schemes, 170/3004 are TRUE. Flags are not
reclassified. The supplement includes numerical diagnostics and comparisons
restricted to the same flagged partitions for all methods. This conditional,
data-dependent subset is not an independent validation sample. Chronological
MTI empirical content is 88.5% for reverse tensile and 87.4% for forward RSD,
showing sensitivity to partition choice. Dependence and distributional change
cannot be distinguished with this illustration.

Held-out empirical content is not nominal tolerance confidence or posterior
content probability. Overlapping splits are not independent replications.
Controlled repeated-sampling evidence remains the separate simulation study.
The manuscript reports these limitations and does not tune the method using
the application.

## File Responsibilities

| Files | Purpose |
| --- | --- |
| New versioned configuration; `86_run_pharma_mti_application.R`; `lib/pharma_mti_application.R` | Verified data, paired partitions, three-method fitting, diagnostics, canonical evidence |
| Historical `lib/pharma_tolerance_application.R` | Existing frozen fitting and extraction; optional SHA256 verification added compatibly |
| `88_recover_pharma_application_csv_precision.R` | Narrow, reproducible endpoint serialization recovery without MTI refits |
| `lib/pharma_mti_reporting.R`; `tables/generate_pharma_mti_application_tables.R` | Generated method, paired, numerical, convergence, partition tables and tested prose macros |
| `figures/generate_pharma_mti_application_figures.R` | Actual representative intervals, paired differences, tail omissions, partition sensitivities |
| `87_finalize_pharma_mti_report.R` | Verify and record derived output hashes |
| New application, table, and figure tests; language helper and validator | Integrity, deterministic splits, failures, empirical evaluations, DP checks, representatives, methods and language |
| `main.tex`; `rqr-gibbs-supplement.tex` | Evidence-bounded application text and generated inputs |
| Makefile; README | Reproduction commands and current default application |

The obsolete pooled sensitivity TeX input is replaced by separately labeled
response-specific tables. Historical generators remain. The supplement build
no longer requires unused regression/dynamic illustration evidence; its
standalone historical target remains available.

## Reproduction Commands

From this worktree, prepare the runtime, verify data, and run preflight before
any new fits:

```bash
make pharma-mti-application-runtime
make pharma-mti-application-data
make pharma-mti-application-preflight
make pharma-mti-application-smoke PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/smoke_new
make pharma-mti-application-confirmatory PHARMA_MTI_WORKERS=40 PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/confirmatory_new
make pharma-mti-application-health PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_application_20261020/confirmatory_new
```

The completed run used `smoke_verified_source` and `confirmatory_20261020`.
The endpoint recovery, if reproducing this initial export, is:

```bash
R_LIBS=application/cache/pharma_mti_library Rscript application/scripts/88_recover_pharma_application_csv_precision.R --run-dir=application/outputs/pharma_mti_application_20261020/confirmatory_20261020 --output-dir=application/outputs/pharma_mti_application_20261020/confirmatory_precision_final_20261020
```

The canonical destination must be absent before an explicit publish. Do not
overwrite an existing canonical record. All ordinary reporting needs no fit:

```bash
make pharma-mti-application-report
make test-pharma-mti-application
make test-theory-figures
make test-theory-tables
make test-manuscript-language
make all-pdf
```

All manuscript application result numbers are generated as TeX macros from
the same source summaries. Missing canonical inputs or changed artifact
hashes stop reporting. The optional committed-input switch is explicit and
does not replace an explicitly requested missing result file.

## Verification Record

Detailed command logs are local under `.codex_work/`. Final command and exit
records accompany this implementation record after validation. Package
installation, R CMD build, and tarball R CMD check completed successfully
(`Status: OK`). Package-native tests run within that check; revised application
tests run explicitly because the package harness restricts its default filter
to native tests.

| Validation Command | Exit | Result |
| --- | ---: | --- |
| `R CMD INSTALL --preclean --clean --library=application/cache/pharma_mti_library application` | 0 | Isolated package installation |
| `make pharma-mti-application-data` | 0 | Verified source bytes, hashes, dimensions, responses |
| `make pharma-mti-application-preflight` | 0 | Frozen policy and partition checks |
| `86_run_pharma_mti_application.R --mode=smoke --workers=4 --output-dir=.../smoke_verified_source` | 0 | 60 result rows, all returned |
| `86_run_pharma_mti_application.R --mode=confirmatory --workers=40 --output-dir=.../confirmatory_20261020` | 0 | 9012 result rows, all returned |
| `88_recover_pharma_application_csv_precision.R --run-dir=.../confirmatory_20261020 --output-dir=.../confirmatory_precision_final_20261020` | 0 | Five benchmark endpoints recovered; no MTI refits |
| `86_run_pharma_mti_application.R --mode=publish --run-dir=.../confirmatory_precision_final_20261020` | 0 | Verified portable canonical evidence copied |
| `make pharma-mti-application-report` | 0 | All generated summaries, figures, and hashes |
| `make test-pharma-mti-application` | 0 | Unit tests, independent evaluation, tables, figure regeneration |
| `make test-theory-figures` | 0 | Current and historical figure tests |
| `make test-theory-tables` | 0 | Current and historical table tests |
| `make test-manuscript-language` | 0 | Generalized-Bayes and application language checks |
| `R CMD build <worktree>/application` under ignored package-check directory | 0 | Source tarball built |
| `R CMD check --no-manual --no-build-vignettes rqrgibbs_0.1.0.9036.tar.gz` | 0 | Status: OK |
| `make all-pdf` | 0 | Main 23 pages; supplement 34 pages; no LaTeX warnings or undefined references |
| `git diff --check` | 0 | Whitespace checks |

The source tarball check ran under
`application/cache/pharma_mti_package_check/`. Scripts in the command table
were invoked with Rscript and the isolated R_LIBS path, as specified in the
Makefile. Complete run-directory prefixes are given above. The main
application and supplementary application pages were inspected as rendered
images, including representative intervals, paired and grouped comparisons,
numerical diagnostics, and table formatting.

Resolved checks found an initial R_LIBS_USER dependency-path problem (fixed
by using R_LIBS), an endpoint serialization discrepancy (recovered above), and
a figure comparison attempted during regeneration (rerun after stable output).
These did not motivate statistical setting changes. No expensive MTI refit
was performed after the completed run.
