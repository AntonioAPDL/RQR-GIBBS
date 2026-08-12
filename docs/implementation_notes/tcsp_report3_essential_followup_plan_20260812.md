# TCSP Essential Follow-Up Plan After Report3/Report4 Audit

Date: 2026-08-12
Status: staged; full launch not authorized

## Objective

Use the PRO reports to improve readiness without launching the full
TCSP/competitor validation campaign. The immediate target is a coherent,
testable repository state: theorem gates documented, q=1 fail-closed behavior
hardened, and competitor scope explicit.

## Stage 1: Claim-Control Wiring

Completed in this pass.

- Expand the proof ledger with the full report3 theorem vocabulary.
- Add the theorem-gate map to the main TCSP section.
- Update the supplement support map.
- Keep all unproved finite-sample, asymptotic, oracle, and posterior-action
  claims behind their named gates.

## Stage 2: Package Contract Hardening

Completed in this pass.

- Preserve the empirical action object on q=1 MCMC infeasibility.
- Assert the fixed learning rate, intercept-only design, ridge prior, frozen
  content, frozen tilt, and `response_likelihood = FALSE` after MCMC fitting.
- Store a posterior model-spec digest in the TCSP contract when MCMC is fit.

## Stage 3: Focused Tests

Run before any additional pilot.

- Focused TCSP package tests.
- Strict TCSP validation tests.
- Manuscript language guard.
- `git diff --check`.

The focused source target is:

```bash
make test-tcsp
```

No full-pilot or confirmatory validation target is part of this stage.

## Stage 4: Competitor Smoke Planning

Ready but not launched in this pass.

- Keep Young-Mathew, Wald, Hahn-Meeker, and exact Normal wrappers optional.
- Add a smallest-nonparametric-tolerance-region identity/difference audit
  before treating it as a competitor.
- Add `gibbsTI` only after package pinning, source digest recording, isolated
  library setup, deterministic tiny run, and a published-replication subset.
- Keep Dirichlet-process tolerance intervals as future work unless verified
  implementation and reproduction evidence become available.

## Stage 5: Next User-Authorized Execution

After the current pilot artifacts are reviewed, the next safe executable step
is a wrapper-smoke or tiny validation run from a clean committed source state.
The full validation campaign remains blocked until the user explicitly
authorizes launch and compute budget.
