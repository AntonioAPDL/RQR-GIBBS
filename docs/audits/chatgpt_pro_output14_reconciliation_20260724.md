# ChatGPT Pro Output-14 reconciliation

Date: 2026-07-24

## Decision

The Output-14 audit was authenticated at remote branch
`chatgpt-pro/output14-audit-20260724`, commit
`2be17bd5710e62168970577796c8ddc1872ffde6`, and imported without alteration.
Its bounded-validation decision is accepted:

```text
completed 24-fit bounded RQR-DLM validation:
  accepted; do not rerun

preliminary matched-simulation design:
  revised to schema 0.2.0

diagnostic-pilot execution:
  not implemented and not authorized

confirmatory simulation:
  not implemented and not authorized

CAVI/ELBO and RQR-DESN:
  deferred
```

The audit found no defect in the generalized-Bayes target, pseudo-AL
augmentation, sequential root-specific FFBS updates, missing-observation
contract, component-specific evolution scales, or future-root
interpretation. Its required work concerned evidence reuse and the
preliminary simulation design.

## Exact repository boundary

The implementation and reference-stage source commit is
`6ba47d1d686e7f47d90bf3110fbbe77f8da96fee`. The package is
`rqrgibbs` 0.1.0.9013.

The protected repositories were inspected with optional Git locks disabled
and remain clean at their pinned states:

```text
exdqlm reference:
  branch: feature/rqr-desn-readout-20260716
  commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

Q-DESN article reference:
  branch: main
  commit: f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

Neither protected repository was loaded, compiled, installed, or modified.
The simulation comparators use exact CRAN source archives materialized only
under ignored RQR-GIBBS cache directories.

## Bounded-evidence promoter corrections

All three nonblocking Output-14 promoter findings were implemented before
the promoter can be reused.

1. An external expected-bundle JSON now freezes the accepted bounded run's
   primary commit, application tree, configuration digest, reference
   manifest digest, runtime tree, runtime attestation, and toolchain digest.
   Promotion requires exact equality rather than trusting only the run's
   internal assertions.
2. Every reopened fit is rehashed. Its checkpoint-state digest and
   continuation-history digest are recomputed, and the package continuation
   validator is invoked.
3. The exact 24 fit IDs are derived from the fit plan. Unique exact set
   equality is required across the fit audit, run status, checkpoints, local
   chain hashes, missing/future checks, and provenance checks.

Deterministic tests reject an expected-bundle mismatch, an omitted ID, a
duplicate ID, an extra ID, a semantic digest mutation, and a continuation
validator failure. The accepted bounded run was not rerun because Output-14
classified these as future evidence-reuse corrections rather than defects in
the completed grid.

## Preliminary simulation contract

The design is now schema
`rqrgibbs_dlm_main_simulation_preliminary/0.2.0`.

### Data-generating mechanisms

Nine fully specified scenario rows replace provisional prose:

- seven core mechanisms and two sensitivities;
- a matched Gaussian and skewed trend-seasonal pair;
- a common-evolution RQR-DLM ablation;
- positive scale floors and minimum root-separation gates;
- exact initial-state, innovation, predictor, seasonal, break, mixture, and
  transition specifications;
- one response law for the independent-root sensitivity, shared across
  coverage levels.

The core sample size is \(T=200\) with forecast horizon \(H=20\).

### Targets and methods

Population RQR roots and equal-tailed quantiles are distinct targets.
Endpoint error is target aligned. Cross-target distances are reported only
with that label. Realized future root paths and conditional-mean future roots
are stored as different estimands.

The primary method remains component-scale RQR-DLM with fixed generalized
Bayes rate one after training-only standardization. Fixed rate 0.5 and 2 and
learned normalized rate remain sensitivities. The matched methods include
fixed-\(W\), common-evolution, frozen-discount, fixed-design RQR,
quantile-derived intervals, and an empirical interval baseline.

The dynamic quantile comparator is pinned to:

```text
package: exdqlm 1.1.0
source: https://cran.r-project.org/src/contrib/exdqlm_1.1.0.tar.gz
SHA-256: 51bc968f617721c9ab1dcfc6ec14857d30827fcd36659f3de45337cc3c82bd14
mode: reduced AL/DQLM MCMC
```

The static quantile comparator is pinned to:

```text
package: quantreg 6.1
source: https://cran.r-project.org/src/contrib/quantreg_6.1.tar.gz
SHA-256: f42292c5ab25a15f39295b93391deafef192fe09eefde563399a64eba7e0169a
mode: rq(method = "br")
```

Both source hashes were verified. Each package was installed into its own
ignored isolated runtime. Adapter checks verify orientation, retain raw
quantile endpoints, order only for interval scoring, and do not use response
predictive draws. The exdqlm protected checkout is not an execution source.

### Tuning, Monte Carlo precision, and diagnostics

Tuning uses training data only, frozen rolling origins, target-specific
validation losses, deterministic ties, a maximum search budget, and full
training refits. Test coverage cannot tune a method.

Coverage qualification uses a 90% TOST wholly inside
\([-\Delta_C,\Delta_C]\), with \(\Delta_C=0.02\). The paired width contrast is
interpreted only after coverage qualification. Coverage MCSE planning uses
approximately 1,600 replications at coverage 0.80 and 900 at 0.90 when an
absolute 0.01 target is required. Endpoint and midpoint MCSE targets are 2%
of training response standard deviation; width uses 2% of the mean oracle RQR
width; loss uses an absolute 0.01 target.

The diagnostic pilot contract requires at least two preselected four-chain
replications per mechanism, coverage, and MCMC method. Confirmatory MCMC would
use within-chain gates plus a preselected 5% multichain sentinel set, with at
least two sentinels per 250-replication batch. None of those fits was run here.

## Oracle and reference implementation

The native oracle now exposes risk and certificate functions for Gaussian,
Student-\(t\), centered standardized lognormal, and standardized skewed
normal-\(t\) mixture errors. Each certificate combines:

- a one-dimensional coverage-constrained profile and basin search;
- an independent unrestricted midpoint/log-width multistart optimization;
- objective-gap, coverage, truncated-moment, curvature, and quadrature-error
  checks;
- explicit minimizer-set and uniqueness fields; and
- distribution and solver digests.

The numerical integration output is called an estimated quadrature error, not
a rigorous bound.

The fail-closed reference runner implements only:

```text
preflight
oracle-reference
tiny-end-to-end
diagnostic-pilot-preflight
```

Requests for `diagnostic-pilot` or `execute-confirmatory` stop before creating
output.

## Exact-runtime reference results

The final isolated primary runtime was built from commit
`6ba47d1d686e7f47d90bf3110fbbe77f8da96fee`:

```text
application tree:
  be1d908103c1d4e32b3fec6275a9dcd732d58b13

runtime tree digest:
  9b21f4cde66058aea8885cd9feab01836f76096e042c39f204916bf2c164f458
```

All four authorized stages passed with this identical runtime:

| Stage | Result | Main evidence |
|---|---|---|
| preflight | pass | contract, scenarios, methods, and seed ledger |
| oracle-reference | pass | 8/8 family-by-coverage certificates |
| tiny-end-to-end | pass | two replications; byte-identical replay; zero repairs |
| diagnostic-pilot-preflight | pass | 672 planned, unauthorized fits; both comparator adapters pass |

Both execution flags are false in every run manifest. No diagnostic-pilot or
confirmatory fit was executed.

The compact packet is
`docs/audits/rqr_dlm_main_simulation_reference_evidence_20260724/`. Its
promoter:

- verifies each stage's exact recursive artifact set, byte count, and SHA-256;
- requires identical exact primary-runtime binding across stages;
- rechecks all stage-specific gates;
- requires both comparator attestations and adapters;
- copies only compact files; and
- writes a fresh recursive evidence manifest.

Earlier exploratory or superseded exact-commit outputs were moved to
explicitly labeled ignored `*.revoked` directories and were not promoted.

## Validation

The following completed successfully:

```text
make smoke
make pdf
make supplement
make test-native
make test-standalone-contracts
make package-check
```

`R CMD check --no-manual` ended `Status: OK`. The article PDF has 9 pages and
the supplement has 10 pages. No manuscript target claim was changed because
Output-14 found the statistical interpretation sound and requested no
immediate manuscript patch.

## Remaining decision

The next independent review should decide whether the schema-0.2 design,
oracle certificates, tiny fixture, comparator isolation, seed plan, and
fail-closed evidence contract are sufficient to implement and execute a
small diagnostic pilot.

It should not authorize the confirmatory simulation merely because these
reference stages passed. A diagnostic-pilot implementation still needs its
full fit schedule, monitored resource envelope, compact diagnostic schemas,
cell-level stop rules, failure artifacts, and separate authorization commit.
CAVI/ELBO and RQR-DESN remain deferred.
