# RQR-DLM M11 multicomponent recovery launch audit

Date: 2026-08-06 (America/Los_Angeles)

## Decision

The development-only, bounded multicomponent transition comparison was
launched from an exact, clean, isolated source state.  This execution is not
the confirmatory simulation and cannot authorize that simulation.  The main
launch flag remains `FALSE`.

## Authenticated source and runtime

| Item | Value |
|---|---|
| Scientific launch source | `c6fd8b05ec839cc75873f30af7244c501dc8fa6c` |
| Source checkout | `/data/muscat_data/jaguir26/.rqr_gibbs_launch_checkouts/rqr_dlm_m11_multicomponent_recovery_8cb215e` |
| Source checkout status | clean; local `main` at the exact launch source |
| Package version | `0.1.0.9033` |
| Fit schema | `rqrgibbs_fit/1.19.0` |
| Correction schema | `rqrgibbs_dlm_main_correction/1.16.0` |
| Runtime attestation SHA-256 | `585711c7331e0b13b7698badc6ba2de2fee78175c4356b6a0487c68dce91f1f8` |
| Documentation/health branch at launch audit | `codex/rqr-dlm-m11-multicomponent-recovery-20260806` |
| Documentation/health tip before this audit | `c22c25bd79261f4f0d63b52544e323cd35c17622` |

The later documentation/health tip differs from the scientific launch source
only in the incremental health counter and its test.  The active scientific
process remains bound to `c6fd8b0`; it was not restarted or silently changed.

## Frozen comparison contract

| Item | Value |
|---|---:|
| Candidate transitions | 4 |
| Preselected cases | 3 |
| Chains per candidate/case | 4 |
| Planned fits | 48 |
| Planned diagnostics | 572 |
| Worker ceiling | 8 |
| Retries or reseeding | none |
| Adaptive chain extension | forbidden |
| Threshold or target change | none |
| Scientific promotion | `FALSE` |
| Confirmatory launch authorization | `FALSE` |

The cases are M11/S10 replications 166 and 167 and the passing M10/S10
replication-77 fixed-rate guard.  The deterministic candidate order is
baseline joint-state cycle 1, directional interweave plus joint-state cycle 1,
joint-state cycle 2 with coordinate interweaving, and directional interweave
plus joint-state cycle 2.  Selection requires the first predeclared candidate
that clears every case; no failed-case-only extension or favorable reseeding is
permitted.

The fixed plan digest is
`7d0fac997b12df6e09e6247670a142336d419b69d0aa53b5bd0bec0f65179c15`.
The maximum seed-ledger SHA-256 is
`3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f`.

## Launch state

| Item | Value |
|---|---|
| Coordinator PID/PGID | `1363048` / `1363048` |
| Coordinator stage | `candidate_comparison` |
| Output root | `application/cache/rqr_dlm_m11_multicomponent_candidates_c6fd8b0_20260806` in the exact launch checkout |
| Control root | `application/logs/rqr_dlm_m11_multicomponent_candidates_c6fd8b0_20260806_control` in the exact launch checkout |
| Preflight manifest SHA-256 | `abf656bebf22f854179d8ab7a790fa7f08a0185d2546042e592b0d79b4a95def` |
| Preflight artifact-manifest SHA-256 | `04c10c6e6eda11c65490c506f53589b44bde997e662969ab5545d033094f25c7` |

At the recovery health check, the coordinator and execute runner were alive,
stderr was empty, and the runner was performing deterministic seed-ledger
materialization before worker creation.  The approximately 56 MB ledger is
written through a temporary file and renamed atomically.  Zero published fit
objects during that setup phase is therefore expected and is not evidence of
a stalled or stopped launch.

## Promotion boundary and next gates

This comparison may only select an exact target-preserving transition for a
fresh affected-wave development rerun.  It does not turn the main launch flag
on.  The remaining order is:

1. require all 48 bounded fits to finish, all 572 diagnostics to be evaluated,
   zero numerical repairs, matching target/model/evolution digests, and passing
   resource checks;
2. if a candidate is eligible, rerun the complete M01 local-level wave-2 gate
   from a fresh ignored root and require all 1,150 diagnostics;
3. commit the selected correction while still fail-closed and build a fresh
   isolated runtime from that exact commit;
4. rerun the full exact-promotion matrix and the package, native, manuscript,
   literature, and resource gates;
5. only after those gates pass, create a separate flag-only authorization
   commit and launch the confirmatory study under a new run ID.

Failure at any stage remains evidence to diagnose, not a reason to weaken the
predeclared ESS, MCSE, tail-ESS, or R-hat requirements.
