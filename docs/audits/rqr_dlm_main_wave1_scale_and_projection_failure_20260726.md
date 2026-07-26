# RQR-DLM main-wave component-scale and comparator correction audit

Date: 2026-07-26

## Decision

The confirmatory run rooted at
`application/runs/rqr_dlm_main_20260725_b8b7748` is closed and must not be
resumed. Its authorization source was
`b8b7748ab181a006611b602f64d4edf5be591de6`. The first canonical wave stopped
before completion, and neither its partial comparative estimates nor any
scientific metric is eligible for analysis or reuse. A replacement run must
start under a new run identifier, output root, authorization bundle, and
exact-commit runtime.

The failure exposed two independent computational defects:

1. the centered-only shared component-scale update mixed too slowly for the
   predeclared `log_q_1` gates; and
2. the CRAN `exdqlm` comparator adapter flattened a \(p\times T\) state mean
   instead of projecting it through the observation design to \(T\)
   ordinates.

The first defect is addressed by an exact centered--noncentered interweaving
transition and fixed role-specific schedules. The second is addressed by the
state-to-ordinate projection
`colSums(FF * posterior_state_mean_or_draw)`. Neither correction changes a
data-generating mechanism, estimand, prior, generalized-Bayes learning-rate
mode, seed, target coverage, comparator definition, or diagnostic threshold.

## Authentication and failed-run inventory

| Item | Audited value |
|---|---|
| Authorization source | `b8b7748ab181a006611b602f64d4edf5be591de6` |
| Failed wave | `static_gaussian_T200__target0200__sentinel` |
| Wave artifact-manifest SHA-256 | `c003675b037311f30df05a8ed4e9992997e4ae0cb308b93ef44592a9a871b80f` |
| Canonical tasks planned | 20 |
| Replication folders completed | 16 |
| Tasks not run | 4 |
| Completed M01 rows | 16, all `diagnostic_failed` |
| Completed M02 rows | 10, all `simpleError` |
| M02 failure-message digest | `c8f7ed064ce97e82bd2d4941eb8726a2ec338992c19b6cd8eda2eaa1bbfef3bb` |
| M01 diagnostics | 719 of 736 passed |
| M01 failed diagnostics | all 16 `log_q_1` rows and one S02/replication-85 `observed_loss` row |
| Failed-output scientific reuse | prohibited |

The original M01 `log_q_1` bulk ESS ranged from 3.456 to 128.363, tail ESS
from 17.311 to 261.632, and MCSE divided by posterior standard deviation
reached 0.535. These values diagnose a transition-kernel mixing problem; they
do not support a comparative statistical conclusion. The partial run remained
within its resource envelope: peak sampled RSS was 1,110,524 KiB, no resource
breach was recorded, and every numerical-library thread limit was one.

## Component-scale diagnosis

For component \(j\), both root paths share the multiplier \(q_j\). The
centered inverse-Gamma update is exact but couples \(q_j\) strongly to the
state innovations. Merely retaining more draws under that transition is not
an efficient general correction: linear extrapolation from the worst
2,000-draw chain would require more than 230,000 retained draws to reach bulk
ESS 400.

The implemented transition composes:

1. the existing centered inverse-Gamma full conditional;
2. standardization of both roots' component innovations;
3. coordinate slice updates of \(\log q_j\) in noncentered coordinates;
4. exact state-path reconstruction at the accepted scales; and
5. recomputation of the centered conditional parameters.

For two roots, the state-innovation transformation contributes
\(\prod_j q_j^{T d_j}\), which cancels the corresponding Gaussian
normalization. The remaining scale kernel consists of the inverse-Gamma prior
on the log scale and the augmented RQR residual-product kernel. It is an
augmented generalized-Bayes loss kernel, not a response likelihood.
`docs/implementation_notes/rqr_dlm_component_scale_interweaving_20260726.md`
gives the derivation and continuation contract.

### Alternatives considered

| Alternative | Finding | Disposition |
|---|---|---|
| Centered-only chain extension | Worst-case extrapolation is prohibitively long | Rejected |
| More slice sweeps per cycle | Four and eight sweeps did not improve worst-case ESS per second consistently | Rejected |
| Two complete interweaving cycles | Did not improve the two worst seed streams | Rejected |
| Adaptive per-chain extension or retry | Would make computation depend on realized diagnostics and violate the frozen no-extension rule | Rejected |
| One cycle and two coordinate sweeps, with fixed role-specific retained draws | Exact transition; best controlled balance of mixing and cost | Selected |

The standard component-scale schedule retains 6,000 draws after 1,000 burn-in
iterations. The learned-loss-scale version retains 9,000 after 1,500 burn-in
iterations. Four-chain embedded sentinels keep their original 2,000- and
3,000-draw schedules. These schedules apply uniformly by method and role
across the entire replacement run; there is no diagnostic-triggered
extension.

## Comparator diagnosis

CRAN `exdqlm` returns a posterior state mean with dimension \(p\times T\).
For \(p>1\), flattening this matrix yields \(pT\) values, not the required
ordinate vector. The invalid downstream endpoints therefore stopped with
“Nonfinite primary outputs or unordered interval endpoints.”

The correction verifies identical finite dimensions for `FF` and the state
summary and computes

\[
  \widehat\eta_t=F_t^{\mathsf T}\widehat\theta_t
  =\operatorname{colSums}(FF\mathbin{*}\widehat\Theta)_t.
\]

The same projection is used for posterior means and retained state draws. A
two-state reference fixture demonstrates the dimensional distinction
directly. The correction belongs entirely to RQR-GIBBS; the CRAN package and
the protected exdqlm checkout are unchanged.

All 88 endpoint fits in the first corrected full comparator gate completed.
At the original standard schedule, 885 of 900 diagnostics passed. The 15
misses were the mean lower root, upper root, midpoint, or width in five
one-chain standard tasks; all four-chain sentinels passed. Re-executing those
five exact seed streams with 4,000 retained draws passed 225 of 225
diagnostics, with minimum bulk ESS 247.630 and maximum MCSE/SD 0.0634.
Consequently, standard M02 endpoint fits use a fixed 4,000-draw schedule,
while sentinels remain at 2,000 draws.

## Reproducibility and provenance boundary

The correction-validation scripts reconstruct the exact first canonical wave
from the frozen incidence matrix and seed ledger. Each requires a new ignored
output root, writes chain evidence and compact tables atomically, produces a
recursive artifact manifest, and fails unless every fit and diagnostic passes.
When an expected source commit is supplied, the scripts additionally require
an isolated primary-runtime attestation and a clean exact checkout. The M02
gate always requires the isolated attested CRAN `exdqlm` 1.1.0 runtime.

The source-worktree sidecar digest excludes only the five declared local
output roots under `application/`: `cache/`, `data_local/`, `logs/`,
`outputs/`, and `runs/`. This prevents growing simulation trees from making a
provenance check scale with prior output. Build-relevant ignored files remain
in scope, including objects or shared libraries under `application/src/`.
The promotion-grade installed runtime is still hashed completely.

The pinned exdqlm branch and the Q-DESN article were inspected read-only. They
must remain unchanged across the final validation and launch.

## Budget consequences

The canonical design remains 110 waves, 8,400 replication tasks, and 40,938
MCMC chain executions at its maximum plan. Fixed schedule changes increase
the maximum MCMC iterations from 117,636,000 to 192,836,000. The complete
overlay is
`docs/audits/rqr_dlm_main_correction_budget_20260726.csv`; its SHA-256 is
bound by the simulation configuration. The total is also independently
reconstructed from the incidence, sentinel, method-chain, and role-specific
schedule contracts before any runner mode starts.

This reconstruction corrected an earlier hand calculation that omitted the
600 maximum-plan M07 common-scale ablation fits and their 12 selected
sentinels from the longer component-scale schedule. The machine-derived
maximum is 1,640,000 iterations larger than that draft calculation. The
validation function now fails before preflight, orchestration, or execution if
any of the initial, central, or maximum totals differs from the tracked
overlay.

Applying the largest measured ASIS per-iteration factor to the entire
corrected maximum budget gives an intentionally conservative upper envelope
of 344.6 hours at 32 workers. This is a safeguard, not a completion-time
forecast. The runner retains compact standard-fit evidence rather than full
standard chains, so the storage contract is unchanged.

## Complete precommit correction gates

Both complete first-wave gates passed from the working correction tree before
the clean implementation commit:

| Gate | Jobs and fits | Diagnostics | Worst retained diagnostic |
|---|---:|---:|---:|
| M01 component-scale | 44 chains in 20 tasks | 920/920 passed | bulk ESS 263.644; tail ESS 318.597; MCSE/SD 0.0628 |
| M02 dynamic quantile | 44 interval chains, 88 endpoint fits in 20 tasks | 900/900 passed | bulk ESS 247.630; tail ESS 417.541; MCSE/SD 0.0634 |

The M02 gate used the isolated attested CRAN `exdqlm` 1.1.0 runtime. The M01
and primary side of the M02 gate were intentionally classified as development
evidence because the working tree was not yet clean and exact-commit
attestation was unavailable. These results justify proceeding to the clean
commit; they do not replace the required exact isolated-runtime reruns.
Compact results are summarized in
`docs/audits/rqr_dlm_main_correction_validation_summary_20260726.csv`.

## Launch gates

The replacement run remains unauthorized until all of the following hold at a
clean implementation commit:

1. native R/C++ tests, standalone contracts, package check, manuscript builds,
   exdqlm smoke tests, and source formatting checks pass;
2. exact isolated-runtime reruns of the complete first-wave M01 and M02 gates
   pass with all diagnostics and zero repairs;
3. fresh confirmatory preflight and oracle/reference evidence passes and is
   bound to the exact source, runtime, comparator runtimes, config, incidence
   matrix, seed ledger, and budget overlay;
4. the protected-repository before/after guards match;
5. a separate flag-only commit changes only
   `confirmatory_execution_authorized` from `FALSE` to `TRUE`; and
6. a new authorization bundle is prepared for a new run and output root.

Only after those gates pass may the complete coordinator be started in the
background. Its embedded sentinels and cross-wave stopping rules remain part
of the main study, not a separate pilot.
