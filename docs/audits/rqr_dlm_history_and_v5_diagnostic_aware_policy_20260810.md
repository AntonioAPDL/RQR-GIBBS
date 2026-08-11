# RQR-DLM History and V5 Diagnostic-Aware Completion Policy

## Purpose

This audit reconstructs the relevant RQR-DLM validation history before the
corrected exact-mean-tilt V5 illustration launch. Its purpose is narrow: avoid
repeating an indefinite fit--diagnose--retune cycle while preserving honest
diagnostic reporting. It does not reinterpret an R-hat, ESS, MCSE, recovery,
or heterogeneity failure as a pass.

## Current state at the policy decision

- The corrected V5 source existed, but no V5 family--target cell had been fit.
- The article still rendered the historical V3 illustration evidence.
- The most recent large RQR-DLM diagnostic-recovery output contained only its
  initial wave plan and start records. Its coordinator process was absent; it
  was not a live job that V5 needed to wait for.
- The host had 64 logical CPUs and sufficient memory. V5 is capped at two
  chain workers within one process-isolated cell and can coexist with other
  bounded work.

## Relevant RQR-DLM history

| Stage | Evidence | Lesson |
|---|---|---|
| Bounded dynamic validation | 897/897 strict gates passed | The fixed-joint FFBS and package boundary can pass a demanding reference suite. |
| Early main attempts | Interface, future-horizon, artifact, and provenance failures | Orchestration defects are hard failures and must be corrected rather than waived. |
| M01 one-root transition | 1,144/1,150 diagnostics | Fits were finite and exact, but a small `log_q_1` ESS/MCSE floor remained. |
| M01 one/two-ASIS transitions | 1,147/1,150 diagnostics | Repeating ASIS alone did not remove the hard cases; blind kernel repetition was not justified. |
| `rootwise2_ASIS2` promotion | 1,150/1,150 diagnostics | A target-preserving full transition correction solved the frozen M01 gate. |
| Broader multicomponent candidates | 523/572 overall; best candidate 140/143 | All 48 fits completed, while three diagnostic rows remained imperfect. Completion evidence was scientifically useful even without universal strict mixing. |
| Diagnostic-aware completion | Full finite metrics retained | Strict thresholds stayed unchanged; violations became warnings, with no replacement seeds, selective extensions, or post hoc relabeling. |
| M02 recovery | 900/900 after correction | The later blocker was diagnostic assembly/thinning alignment, not posterior mixing; this remained a hard implementation defect. |

The history separates two classes of problem. Source, target, numerical,
artifact, and diagnostic-construction defects require correction. A small
number of finite MCMC diagnostic misses, after the exact target is verified,
should remain visible evidence but need not cause an endless sequence of
same-data reruns.

## Prospective V5 policy

The V5 configuration freezes this policy before the first V5 fit.

### Hard completion gates

A cell stops the campaign if any of the following occurs:

- a chain or required artifact is missing;
- exact source/runtime/provenance binding fails;
- the exact-joint target, reproducibility, or zero-repair contract fails;
- the R/C++ conditional parity check fails;
- endpoint draws are nonfinite, unordered, or cross the declared pathology
  bound;
- process-group/resource or recursive artifact integrity fails.

### Nonblocking warnings

The original thresholds remain fixed and reported:

- rank-normalized R-hat at most 1.01;
- bulk and tail ESS at least 1,000;
- MCSE/posterior-SD at most 0.05;
- the original narrow endpoint-recovery and heterogeneity gates.

Failure of one of these gates records the exact failing rows and warning code,
but the next family--target cell is still run. No threshold is renamed or
weakened, no DGP seed is replaced, and no cell receives a selective extension.

### Manuscript-review envelope

V5 also freezes a broad suitability envelope. This envelope is not an MCMC
convergence criterion. It prevents a finite but seriously misleading fit from
being packaged automatically as an illustration. Broad limits cover endpoint
RMSE and bias relative to oracle width, fitted/oracle width ratio, low/high
scale recovery, width-contrast recovery, and dynamic seasonal amplitude and
phase. A cell outside this envelope remains a completed result requiring
review; it is not silently discarded.

## Decisions

- V5 may run independently of the other RQR-DLM campaign.
- All six V5 cells must be attempted unless a hard completion gate fails.
- The run closeout may pass its computational contract with diagnostic
  warnings.
- Compact manuscript evidence requires all six cells to be hard-complete and
  broadly suitable, but does not require every strict diagnostic row to pass.
- Article figures remain unchanged until compact evidence and a visual review
  are complete.
- This policy applies only to the single-data V5 illustration. It does not
  authorize or change the separate repeated-DGP validation study.

## Interpretation

The V5 fits target generalized posteriors induced by interval loss and fixed
oracle tilts. They do not define a response likelihood or posterior-predictive
response distribution. Completion and diagnostic labels must retain that
interpretation in every manifest and report.
