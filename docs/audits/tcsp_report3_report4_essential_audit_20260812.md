# TCSP Report3/Report4 Essential Audit

Date: 2026-08-12
Status: essential follow-up only; no full validation launch

## Sources Audited

The user requested the new PRO reports at `report3.md` and repeated the same
path. Because the repository also contained an adjacent untracked `report4.md`
with a TCSP implementation prompt, both files were inspected.

| Source | SHA-256 | Interpretation |
|---|---|---|
| `report3.md` | `a8f7b27b5cb8f536a356d5f58930457035f984240e331787f653ade4ecf70ac8` | Theory and proof-development contract. It is not a request to launch simulations. |
| `report4.md` | `9f3df4f2bf3cec8d1c754cf7df5575cf43cb0be7666fea0dfbdb0d700d70c0b7` | Implementation, competitor, and validation-campaign prompt. Most P0 items were already merged in the previous hardening and pilot-readiness commits. |

## Essential Diagnosis

`report3.md` is the stronger new signal. It says the repository should expose a
complete theorem dependency map before promoting TCSP claims. The current
manuscript already avoids the main overclaims: posterior summaries are not the
tolerance action, the Monte Carlo scan calibration is not called exact, and
the scan method is presented as proposed until its proof gates are closed.

The missing piece was not another full implementation pass. The missing piece
was traceability: the proof ledger did not yet list several load-bearing
theorems named by the PRO report, and the main article did not give readers a
compact map from theorem to guarantee.

`report4.md` largely matches the hardening already merged on `main`:

- Monte Carlo scan counts use one common Uniform scan distribution and a
  simultaneous Massart-DKW empirical-CDF lower band.
- Reserved `mcmc_args` cannot override the frozen TCSP target.
- The formal action remains the closed order-statistic window, not a posterior
  summary.
- Optional Young-Mathew, Wald, Hahn-Meeker, and normal-family `tolerance`
  wrappers are implemented but disabled by default.

One implementation detail was still worth tightening: for `q=1`, `fit_mcmc =
TRUE` failed closed before calling `rqr_mcmc_fit()`, but the computed empirical
action was only described in the error message. The error condition now carries
the preserved `rqr_tcsp_fit` object with `posterior_fit = NULL`.

## Accepted Changes

1. Expanded `docs/theory/tcsp_mt_rqr_proof_ledger_20260811.md` with the missing
   PRO theorem IDs:
   `T-FEAS`, `T-CT-1`, `T-CT-2`, `T-FAIL`, `T-ORACLE-CAL`, `T-REGRET`,
   `T-SCAN-ASY`, `T-GB-PROP`, `T-GB-PLUGIN`, and `T-COND-SCOPE`.
2. Added a TCSP theorem-gate table to `main.tex`. The table states what each
   theorem guarantees, what claim it unlocks, and what remains unsupported.
3. Updated the supplement support map so the supplement points to the same
   TCSP theorem-gate vocabulary.
4. Hardened q=1 posterior infeasibility handling by attaching the empirical
   action object to the `rqr_tcsp_mcmc_unavailable_error` condition.
5. Added focused tests for the q=1 preserved action, fixed learning-rate
   target audit, intercept-only design audit, and posterior model-spec digest.
6. Extended the benchmark registry with disabled/audit-required entries for
   smallest nonparametric tolerance regions, `gibbsTI` calibrated Gibbs
   tolerance intervals, and Dirichlet-process tolerance intervals.

## Deferred Or Rejected For This Pass

- No full pilot, full validation campaign, or confirmatory simulation was
  launched.
- Exact finite-n scan recursion remains deferred.
- `gibbsTI` is not enabled until it is pinned, isolated, smoke-tested, and a
  published replication subset is planned.
- Dirichlet-process tolerance intervals are not implemented from an incomplete
  paper interpretation.
- Smallest nonparametric tolerance regions are not treated as an independent
  competitor until an identity/difference audit establishes whether they are
  distinct from the TCSP univariate closed-window action.
- No performance claims, title claims, abstract claims, or simulation result
  claims were promoted.

## Readiness Position

The repository is now better prepared for the next validation step because the
article, proof ledger, code contract, and competitor registry use the same
claim boundaries. The next executable stage should be a focused wrapper smoke
or tiny validation run after reviewing the current pilot artifacts; it should
not be a full launch until the user explicitly authorizes it.
