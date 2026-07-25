# Output-16 reconciliation and main-run readiness

## Decision

The two Output-16 launch blockers are closed in implementation commit
`aa1ded8c5b4db2257c6985c73626e1a0a252fc72`. The complete main confirmatory
study has not been launched: the checked-in authorization flag remains
`FALSE`, no confirmatory fit was executed, and a flag-only authorization commit
still requires a favorable independent Output-17 review.

The scientific contract is unchanged. RQR remains a generalized-Bayes
interval-root update under the stated loss. Its retained root and state draws
are not posterior-predictive response draws. The main study estimates
repeated-sampling operating characteristics only for the frozen DGP, method,
sample-size, horizon, and precision-stopping contract.

## Output-16 disposition

| Finding | Disposition | Implemented control |
|---|---|---|
| WAV-001 | Closed | Authorization-bound append-only state accepts only the next one of 110 canonical waves. It binds the run ID, reviewed and authorization commits, runtime, config, incidence, seed ledger, task plan, and wave plan. Every terminal record hashes its immutable start, all predecessor completions, predecessor artifact manifests, and its own output manifest. |
| DIA-001 | Closed | Each MCMC method has an exact diagnostic schema. It includes selected training-time endpoint functions, dynamic terminal endpoint functions, future conditional-mean endpoints at horizons 1, 5, 10, and 20, observed loss, time averages, learned `log(lambda)`, and all applicable component `log(q_j)` values. Schema intersections are forbidden. |
| SRC-002 | Closed | The stale Output-16 prompt row now records 14,932 bytes and SHA-256 `57b9fdd6...`. |
| FCST-001 | Closed | Exact retained future-root diagnostics call the forecast propagation with `nd = NULL`; no response is generated. |

The state machine additionally handles the following bounded recovery cases:

- a standard wave cannot precede its same-batch sentinel pass;
- any failed or incomplete wave permanently blocks later work;
- an add-batch decision must match the immediately preceding replication
  target and authorize the exact next target;
- a verified precision-stop decision propagates across all remaining waves for
  that group, recording immutable skips rather than silently launching or
  stalling;
- a completed wave is bound to the actual recursive artifact manifest at its
  recorded output path;
- replay, missing predecessors, cross-run evidence, changed bindings, duplicate
  records, and changed resume plans fail closed;
- the coordinator validates the entire append-only state before resume, after
  each wave, and before final audit.

## Complete-study operation

There is no disposable pilot. The embedded sentinels belong to the main
confirmatory design and use preallocated RNG streams. The resume-safe
coordinator advances one canonical wave at a time, collects complete evidence
at every global batch boundary, and supplies the verified precision decision
to later waves. A detached launcher records the exact launch inputs and
supervisor PID. A separate health-check script is read-only.

The checked-in authorization preparation script accepts only:

1. a favorable independently reviewed implementation SHA;
2. a clean one-line `FALSE` to `TRUE` authorization commit;
3. explicit user confirmation;
4. a matching isolated primary runtime;
5. all-pass preflight and reference bundles from that authorization commit;
6. the attested exdqlm 1.1.0 and quantreg 6.1 comparator runtimes.

## Validation evidence

The implementation commit was validated locally on Jerez:

- source and shell parsing passed;
- the standalone bounded/main/confirmatory contract suite passed;
- `R CMD check --no-manual` for `rqrgibbs 0.1.0.9018` returned
  `Status: OK`;
- the environment smoke test passed;
- the literature manifest covered 18 local-only PDFs;
- the main article built to 9 pages and the supplement to 10 pages, without
  undefined references or TeX warnings;
- direct and wave execution both failed closed while the authorization flag
  was `FALSE`;
- the process-group monitor fault test passed;
- the five focused pinned-exdqlm RQR smoke files passed from an isolated
  Git-archive runtime;
- the exdqlm and Q-DESN protected checkout guards were unchanged.

An isolated primary runtime was then built from the exact implementation
commit. Its application tree is
`346aa59b723002cb106aabc22a4ed1652dc29868` and its installed runtime-tree
digest is
`e633691594e676bb42b785a1a194016a0558ae863d6ea2c7260db669dc52de46`.
The exact-runtime preflight passed 22/22 gates. The exact-runtime
oracle/reference stage passed 15/15 gates using isolated exdqlm 1.1.0 and
quantreg 6.1 comparator runtimes.

The first oracle invocation intentionally demonstrated fail-closed behavior
when its explicit preflight seed-ledger path was omitted. It performed no
scientific computation. The corrected invocation supplied the exact hashed
ledger and passed all 15 gates.

Compact evidence is under
`docs/audits/rqr_dlm_output16_reconciliation_evidence_20260725/`. Heavy runtime
libraries, the 87.6 MB seed ledger, complete plans, TeX products, logs, and
future fitted objects remain under ignored local roots.

## Remaining gate

Output-17 should independently review only the corrected implementation and
the compact exact-commit evidence. A favorable review may authorize the
already specified one-line flag commit. After that commit, Codex must rebuild
and re-attest the primary runtime, rerun preflight and oracle/reference against
the authorization commit, materialize the authorization bundle, and start the
complete main study. Changing only the flag without those post-commit steps
remains prohibited.
