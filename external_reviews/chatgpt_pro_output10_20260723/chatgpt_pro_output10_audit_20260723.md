# ChatGPT Pro Output-10 independent audit of RQR-GIBBS

**Audit date:** 2026-07-24  
**Implementation inspected:** `2e7840388d5612f5ebe9234c80f28c650c145b9c`  
**Closeout inspected:** `f5c2f634eda05126e69d7c5cb3fd13301c8b230c`  
**Package/schema:** `rqrgibbs 0.1.0.9009`; fit `1.7.0`; continuation `4.0.0`; runtime attestation `5.0.0`; bounded fixtures `4.0.0`

Protected references were inspected only at `dffb71ee70b597d6a716ee74be1cbc99731cd453` (exdqlm) and `f9f22804eff3871bb5350c8add04b7c9f4d4957b` (Q-DESN). Neither is modified.

## Evidence boundary

I inspected exact GitHub source and tracked compact evidence. The audit environment had no `R`/`Rscript`, and ignored Jerez run directories and chains were unavailable. I therefore did not rerun R/C++, package, TeX, isolated-runtime, reference, or benchmark commands. Reported execution values remain exact-commit evidence whose producing source was audited. No 24-fit cell was launched.

## Executive verdict

```text
Scientific target and interpretation:           PASS
Runtime lineage v5:                              PASS, controlled local scope
Continuation history v4:                         PASS, promotion semantics
Expanded reference suite:                        PASS, 40/40
PGID normal-completion path:                      PASS
PGID signal/error final evidence:                 PARTIAL — blocker
Exact required-estimand schema:                   FAIL — blocker
Future diagnostic draw alignment:                 FAIL — blocker
One-cell fit/resource mechanics:                  PASS
Current 4,000-retained schedule:                  FAIL frozen ESS gate
Current 45-minute whole-grid timeout:             FAIL budget review
Enable 24 bounded fits now:                       NO-GO
Matched/production RQR-DLM:                       NO-GO
CAVI/ELBO:                                        DEFER
RQR-DESN:                                         DEFER
```

The remaining no-go is not evidence of a wrong target or broken sampler. It is caused by two runner/evidence defects, a predeclared ESS miss, and an inadequate whole-grid timeout.

## Finding disposition

| ID | Area | Verdict | Blocks? | Finding |
|---|---|---|---:|---|
| F1 | Runtime lineage v5 | Pass | No | Complete expected package file set and exact successful one-input build/install lineage are reverified. |
| F2 | Continuation v4 | Pass | No | Raw counts are validated before coercion and promotion state is reconstructed. |
| F3 | 40 reference gates | Pass | No | Dense/cross-time, missing, future, component conditional, six-cell continuation, and mutations are source-consistent. |
| F4 | PGID normal monitoring | Pass | No | Handshake, census, escalation, fault test, and final sweep are present. |
| F5 | Signal/error finalization | Partial | **Yes** | Signal/EXIT handlers drain, then bypass summary, closeout, fallback failure evidence, and recursive hashes. |
| F6 | Required schema | Fail | **Yes** | Only equality across four chain schemas is checked; one required block can disappear from every chain. |
| F7 | Future draw order | Fail | **Yes** | `nd=N` randomly permutes all forecast draw indices; diagnostics ignore `draw_index`. |
| F8 | Future mixing target | Hardening | No | Fresh future evolution noise can inflate ESS; deterministic conditional means are preferable. |
| F9 | Atomic RDS evidence | Hardening | No | Rename is atomic, but no read-back/checkpoint validation precedes publication. |
| F10 | Benchmark mechanics | Pass | No | Four exact/provenance-eligible chains completed with zero repairs and bounded sampled resources. |
| F11 | 4,000 schedule | Fail | **Yes** | Bulk ESS values 962.08 and 971.87 are below the frozen 1,000 threshold. |
| F12 | 45-minute timeout | Fail | **Yes** | Six equal-cost current benchmark cells project to 52.93 minutes. |
| F13 | Interpretation | Pass | No | Loss/likelihood, root/response, exact/working, and calibration distinctions remain intact. |

## Runtime lineage v5

`application/R/rqr_utils.R`, the two runtime builders, `isolated_runtime_lineage.R`, and the native lineage test now bind:

```text
reviewed Git subtree -> exact archive -> complete expected source package
-> one successful canonical build -> exact package -> one successful canonical install
-> pre-marker runtime -> lineage marker -> final executing runtime
```

The verifier compares expected and built file sets in both directions after committed `.Rbuildignore` and documented R-build transformations. Receipts require exit status zero, timestamps, exact one input, exact output and library, and rehashed logs. Command-shape verification excludes second package inputs and partial installs. The primary builder removes stale runtimes and removes failed-install remnants. Tests cover a real positive build plus strict-subset, mixed-package, multiple-input, and failed-install negatives.

I found no remaining concrete non-adversarial false positive in this controlled workflow. This is not signed provenance or cross-toolchain reproducible-binary proof.

**Decision: PASS.**

## Continuation history v4

The shared history-count boundary validates generation, segment/cumulative repair, contract generation, and checkpoint completed-iteration values as finite scalar whole nonnegative integers in range before coercion. The validator reconstructs segment and cumulative exactness, target eligibility, environment eligibility, backend-change semantics, repairs, mismatch ledger, reproducibility, override, and promotion; it also checks parent links, target digest, final checkpoint, model status, and provenance.

The 27 tracked corruptions comprise 24 early-generation numeric mutations plus three semantic contradictions and reject the Output-9 `0.5` case. Nonblocking hardening remains: require contract-level aggregate booleans to be nonmissing logical scalars and validate RNG entries before `.rqr_restore_rng()` coercion.

**Decision: PASS.**

## Expanded 40-gate suite

The tracked table has 40 passing rows. The dense calculation uses correct state-time flattening and Gaussian sample-covariance standard errors. Canonical missing indices 6, 17, and 11 are checked with exact placeholder invariance. The public forecast API is exercised for all fixtures. Canonical two-component inverse-Gamma conditionals are recomputed. All six fixture/mode cells compare uninterrupted six draws with `2+2+2` across roots, full/terminal/time-zero states, lambda, scales, conditional parameters, checkpoint, and history. The reference bundle binds recursive artifacts, gates, resources, source, runtime, attestation, and toolchain.

One nonblocking enhancement is useful: the component future-moment fixture repeats one `q` vector for every fake draw. Source and a native test verify selected-row indexing, but a two-row deliberately varying-`q` moment reference would strengthen independence.

**Decision: PASS.**

## PGID monitor blocker

The normal path is acceptable on the actual legacy-cgroup host and honestly labels RSS as sampled telemetry. However, `INT`/`TERM`/`HUP` and unexpected `set -e` exits drain the group and exit before ordinary post-loop finalization.

Reproducer:

```bash
out=$(mktemp -d)
RQR_DLM_OUTPUT_DIR="$out" application/scripts/08_run_rqr_dlm_bounded_validation.sh benchmark-one-cell &
pid=$!
sleep 1
kill -TERM "$pid"
wait "$pid" || true

test -f "$out/resource_summary.csv"   # currently false
test -f "$out/wrapper_closeout.csv"   # currently false
test -f "$out/artifact_hashes.csv"    # currently false
```

Add one idempotent `finalize_wrapper()` called from normal completion, `EXIT`, and signal handlers. Under nonfatal shell mode it must drain and verify the PGID, handle zero samples, record signal/runner/monitor status, ensure a structured wrapper failure row, and atomically publish resource summary, closeout, and recursive hashes. Test signal termination, monitor-command failure, leader-exits-first, zero-sample startup, and a TERM-resistant child requiring KILL.

**Decision: PARTIAL; blocker.**

## Diagnostics: exact counts, missing schema, and draw permutation

For the benchmark (`T=30`, `p=3`, `H=3`, two components, learned scale), the intended 150 variables are:

```text
4*T training endpoint functions                         120
observed loss                                             1
2*p terminal midpoint/separation                          6
2*p time-zero midpoint/separation                         6
4*H future endpoint functions                            12
log lambda                                                1
2*J component log-scale/innovation energy                 4
                                                        ---
                                                        150
```

Expected counts are:

| Fixture | Fixed | Learned |
|---|---:|---:|
| fixed-W local level | 117 | 118 |
| trend-seasonal discount | 181 | 182 |
| component-scale trend-regression | 149 | 150 |

Matrix orientation is corrected and innovation energy `2*(posterior_rate-prior_rate)` is correct. Component arrays are draw by named component.

### Blocker 1: required variables can disappear silently

`diagnose_cell()` checks only that four chains share column names. `chain_estimands()` conditionally adds time-zero and component-scale blocks. Setting those fields to `NULL` in all four fits yields the same 140-column schema and still reaches `posterior` diagnostics. Add an independently constructed ordered `expected_estimand_names(fixture, mode)` and require exact equality before diagnostics. Tests must remove each required block from all chains and fail.

### Blocker 2: future iteration order is randomized

The runner passes `nd=n_save` to `rqr_forecast_roots()`. Any non-NULL `nd` invokes `sample.int`; when `nd==n_save`, every draw is randomly permuted. The returned `draw_index` is ignored, so future columns lose MCMC iteration order and rowwise alignment, artificially improving serial diagnostics.

```r
n <- ncol(fit$samp.eta_root1)
fc <- rqr_forecast_roots(fit, ..., nd=n, seed=1)
stopifnot(!identical(fc$draw_index, seq_len(n)))
```

Call with `nd=NULL` when all draws are needed and assert sequential `draw_index`. Add a unique-terminal-marker alignment test. For primary mixing diagnostics, replace stochastic future trajectories with deterministic conditional-mean root functions; retain stochastic trajectories as finite forecast sidecars. Counts can remain unchanged.

The execute-mode cell stop rule itself is correct: a failed four-chain cell stops later cells.

## Benchmark and exact replacement schedule

The four chains had zero numerical/forecast repairs and exact target/provenance/promotion status. Total sequential chain time was 529.288 seconds (8.82 minutes), sampled peak was one process, two threads, and 325,080 KiB RSS, and the failure log was empty.

The two ESS misses are related functions of the regression component:

```text
log component scale:       R-hat 1.000803, bulk 962.08, tail 1261.34
innovation energy:         R-hat 1.000294, bulk 971.87, tail 1330.98
```

This is a minor Monte Carlo precision shortfall, not evidence of a target, repair, or chain-location failure. The frozen threshold must not be weakened.

Freeze the replacement contract:

```text
4 chains; 2,000 burn-in; 6,000 retained; thin 1
same starts and seeds; cpp; numerical policy fail
R-hat <=1.01; bulk ESS >=1000; tail ESS >=1000; zero repairs
no retry, extension, retuning, threshold change, or seed replacement
whole-grid hard timeout: 240 minutes
```

Linear planning projects the two ESS values to about 1,443 and 1,458, but the repeated benchmark must actually pass every required diagnostic. Projected component-cell chains are about 45.45 MB; six equal cells are about 260 MiB. At the new schedule six equal-cost cells project to 70.57 minutes, so the current 45-minute timeout is already noncredible.

## Scientific interpretation

The manuscript, supplement, package, fit object, runner, and forecast API continue to state that RQR is a generalized-Bayes interval-root loss update; pseudo-AL augments the loss rather than defining a response likelihood; root/state draws are not posterior-predictive response draws; fixed/frozen/shared-scale modes are fixed-joint exact modes absent repairs; adaptive discount is working/sequential; and learned lambda is not a response variance or automatic coverage calibration.

**Decision: PASS.**

## Exact next bounded work

Keep `bounded_dynamic_execution_authorized=FALSE` and implement only:

1. exact independent estimand schemas and missing-block tests;
2. sequential forecast draw order and alignment tests;
3. deterministic conditional-mean future mixing variables;
4. always-run shell finalization and fault tests;
5. RDS read-back/checkpoint validation before rename;
6. 2,000 burn-in + 6,000 retained and a 240-minute timeout;
7. aggregate-logical/RNG and varying-`q` hardening.

At the resulting exact commit rebuild the isolated runtime; rerun parse/shell checks, native R/C++, `R CMD check`, both PDFs, protected guards, preflight, the unchanged 40 gates with a new bound bundle, monitor fault tests, the same learned component-scale benchmark, and the fail-closed execute negative test. Require the exact expected schema and all diagnostics to pass. Do not reuse old chains or change seeds/thresholds.

Obtain another independent review. Only after it passes should a separate authorization commit set the flag true; because the flag changes the config digest, rebuild and rerun exact-commit preflight/reference-only before execution.

```text
Runtime lineage v5:              PASS
Continuation v4:                 PASS
40-gate suite:                   PASS
PGID fallback:                   PARTIAL; finalization patch
One-cell fit/resources:          PASS
4,000 schedule:                  NO-GO
24 bounded fits now:             NO-GO
Implement and rerun evidence:    GO
Matched/production simulation:   NO-GO
CAVI/ELBO:                       DEFER
RQR-DESN:                        DEFER
```
