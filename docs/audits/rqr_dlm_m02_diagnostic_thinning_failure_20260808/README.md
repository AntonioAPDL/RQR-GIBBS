# RQR-DLM M02 diagnostic-thinning failure audit

Date: 2026-08-08

## Decision

The diagnostic-aware maximum run stopped correctly at canonical wave 1.  The
failure is a deterministic M02 result-construction defect, not a failure of the
M02 state-space fit, a convergence-threshold violation, a numerical repair, a
resource ceiling, or a provenance boundary.  The stopped output root is
immutable failure evidence and is not continuation- or promotion-eligible.

## Bound evidence

| Item | Value |
|---|---|
| Run ID | `rqr_dlm_diagnostic_aware_maximum_20260807_ea8ea8d` |
| Exact source and authorization commit | `ea8ea8d17c6f7bb34b015472e4f60f62e547c942` |
| Primary runtime tree digest | `c33d23f76a71a52e3c4e0d7f1a65f79c0955562a9607d5a36c8a2ec1a2bd21cc` |
| Preflight gates | 23/23 passed |
| Reference gates | 15/15 passed |
| Failed wave | `static_gaussian_T200__target0200__sentinel` |
| Terminal waves | 1/110, with zero passed waves |
| Terminal DGP-replication tasks | 8/8,400 |
| Successful method results | 8 M01 results |
| Failed method results | 8 M02 results |
| Available frozen diagnostics | 368/368 passed |
| Final audit | absent |

The preflight recursive-manifest digest is
`8afd90b7405bd05bba6e7a1d64cd2bba69ffb594b27b7c4c752bef0fef0fba03`.
The reference recursive-manifest digest is
`0cd944fbe5221ef0a9ed9c6104ea047f89c209681a5ad4f31227522157f968c3`.
The wave recursive-manifest digest is
`1f62f7a31780bede379d50cca4d0f8170211c72a6b3390a608ef97dbfc50715c`;
all 151 declared wave artifacts were independently rehashed and matched in
both byte count and SHA-256.

## Failure signature

Every worker completed its first M01 method result and then failed while
constructing M02 scalar diagnostic draws.  The eight failures span S01 and S02
and share the message digest

```text
6495bf2b41dc1e26ee4112cd6e30200ca35aa20b34792f1e7f953b9a79210ef5
```

Reproducing the base-R matrix operation establishes that this is the SHA-256
of

```text
number of rows of matrices must match (see arg 2)
```

The worker-level evidence is recorded in `failure_signature.csv`.

## Root cause

The frozen M02 policy doubles the standard or sentinel endpoint schedule from
4,000 to 8,000 retained exdqlm draws.  `diagnostic_thin=2` then projects the
training ordinate draws back to the predeclared 4,000-draw diagnostic sample.
The same retained indices were not applied to the terminal-state arrays used
to construct conditional future-root functions.  Training and terminal
diagnostics therefore contained 4,000 and 8,000 draws, respectively.  Their
matrix binding failed before M02 could be compacted.

This diagnosis is deterministic and explains all eight workers without a
DGP-, seed-, chain-, or initialization-specific exception.  It also explains
why earlier tests missed the defect: structural diagnostics were exercised,
but the production M02 transition multiplier and the joint training/future
draw-count invariant were not tested together.

## Statistical scope

The correction changes only which already-retained exdqlm states enter the
diagnostic summary.  It does not change the M02 target, endpoint probabilities,
initialization, random-number streams, transition count, forecasts, interval
construction, or scientific metrics.  It applies the same deterministic
thinning map to training ordinates and terminal states so that each diagnostic
row represents one common MCMC iteration.

The eight M01 results show healthy computation—minimum bulk ESS 1,028.95,
minimum tail ESS 2,114.69, maximum rank-normalized R-hat 1.000549, and maximum
MCSE/SD 0.03118—but are selected first-wave evidence only.  They are not a
comparative simulation result and must not enter the article.

## Required recovery boundary

Recovery requires a new exact source commit, a fresh isolated primary runtime,
fresh preflight/reference/authorization artifacts, and a new run ID.  The
failed root cannot be resumed because its append-only ledger contains a
terminal failed wave and the protocol prohibits retry, reseeding, or selective
reuse.

