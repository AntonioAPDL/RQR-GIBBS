# Codex handoff after ChatGPT Pro Output-10

Audit this handoff as claims, then modify only `AntonioAPDL/RQR-GIBBS`. Protected exdqlm and Q-DESN commits remain read-only. Do not run the 24 fits in this pass.

## Reviewed source and decision

```text
implementation 2e7840388d5612f5ebe9234c80f28c650c145b9c
closeout       f5c2f634eda05126e69d7c5cb3fd13301c8b230c
runtime v5: PASS
continuation v4: PASS
reference: PASS 40/40
fit/resource benchmark mechanics: PASS
signal/error final evidence: PATCH
exact required schema: PATCH
future draw alignment: PATCH
4,000 retained: FAIL frozen ESS gate
45-minute grid timeout: too short
24 fits in this pass: NO
```

Preserve the generalized-Bayes loss interpretation, no response-likelihood or response-draw language, exact fixed-joint versus working adaptive distinction, and learned-scale limitation.

## 1. Exact estimand schema

Add `expected_estimand_names(fixture, learning_rate_mode)` independent of fit contents and require exact ordered equality for every chain before `posterior` calls.

```text
fixed-W:       117 fixed, 118 learned
trend-seasonal:181 fixed, 182 learned
component:     149 fixed, 150 learned
```

For the benchmark: `4*T + 1 + 2*p + 2*p + 4*H + 1 + 2*J = 150`.

Tests must remove, from all four chains, time-zero fields, component fields, a training variable, a future variable, and learned log-lambda; every case must fail before diagnostics.

## 2. Future draw order and mixing targets

The current runner passes `nd=n_save`; `rqr_forecast_roots()` then calls `sample.int(n_save,n_save)` and permutes all retained indices. The runner ignores `draw_index`.

For all-draw forecast sidecars pass `nd=NULL` and require:

```r
identical(forecast$draw_index, seq_len(n_save))
```

Add a unique-terminal-marker alignment test. Replace stochastic future trajectories in primary MCMC diagnostics with deterministic conditional-mean root propagation through future `GG` and `FF`; keep stochastic root trajectories as finite-value/forecast sidecars. Retain the same diagnostic counts with new future names.

## 3. Always-finalized PGID evidence

Refactor the shell wrapper around one idempotent `finalize_wrapper()` invoked by normal completion, `EXIT`, `INT`, `TERM`, and `HUP`. Under nonfatal shell mode it must drain and verify PGID emptiness, wait when needed, handle zero samples, record signal/runner/monitor/finalizer status, ensure a structured wrapper failure row, and atomically write resource summary, closeout, and recursive hashes.

Fault tests:

```text
TERM after readiness
unexpected monitor-command failure
leader exits before descendant
TERM-resistant descendant requiring KILL
zero-sample startup failure
```

Every case must end with an empty PGID and complete hashable compact evidence.

## 4. Chain validation before publication

For full-chain RDS files: save temporary, read back, verify fit class/identity and `digest(checkpoint_state)==checkpoint_digest`, compute hash, then atomically rename. Failure leaves no final chain and records a structured error.

## 5. Freeze the replacement schedule

```text
chains 4
burn-in 2,000
retained 6,000
thin 1
same starts and seeds
cpp; numerical policy fail
R-hat <=1.01
bulk ESS >=1,000
tail ESS >=1,000
zero repairs
hard timeout 240 minutes
no retry, extension, retuning, threshold change, or seed replacement
```

Keep `bounded_dynamic_execution_authorized=FALSE`. Advance package/config/output schemas so old evidence cannot satisfy the new contract.

## 6. Small hardening

- require continuation aggregate booleans to be nonmissing logical scalars;
- validate RNG checkpoint entries as finite whole integers before coercion;
- add a future reference with deliberately different `q` rows and verify selected rows plus output moments.

## 7. Validation and return

At the new exact commit run:

```text
source/shell/git-diff checks
complete native R/C++ suite
R CMD check --no-manual
main and supplement builds
runtime-v5 positive/negative tests
protected repository guards and isolated builds
monitored preflight
unchanged 40-gate reference suite with new recursive bundle
all new monitor fault tests
same learned component-scale four-chain benchmark at 6,000 retained
fail-closed execute negative test with zero chains
```

The benchmark must have the exact expected schema, finite diagnostics, all R-hat/bulk/tail gates, zero repairs, exact provenance/promotion, empty failure log, and passing resources/hashes. Do not continue old chains across the commit.

Return implementation/closeout SHAs, changed files, schema/config diff, complete validation, 40 gates/hashes, monitor fault evidence, six exact schemas, 6,000-draw diagnostics/chain hashes, time/storage projection, fail-closed evidence, and a new Pro prompt. Do not execute the 24 fits. A later authorization must be a separate reviewed commit followed by exact-commit runtime and reference rebuilding.
