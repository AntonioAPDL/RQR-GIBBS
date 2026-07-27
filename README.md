# RQR-GIBBS

Standalone manuscript and reproducibility workspace for Bayesian relaxed
quantile regression (RQR) with Gibbs sampling. The model hierarchy begins with
static interval-root regression, adds a native ordinary-RQR
shrunken-shoulder horseshoe based on the Nishimura--Suchard augmentation
(RHS-NS), treats frozen
nonlinear DESN readouts as a fixed-design specialization, and then extends the
roots through a native linear dynamic/state-space model. The working descriptor
is coverage-targeted interval-root regression under the RQR loss.

## Purpose

The project separates RQR from the Q-DESN article because RQR has a different
inferential target. Q-DESN estimates conditional quantile ordinates. RQR
directly estimates two interval roots under a residual-product loss and a
generalized-Bayes update. The Gibbs construction arises from a pseudo-AL
augmentation of that loss. Static ridge and ordinary RHS-NS regression share
the same conditional-Gaussian root calculation; a frozen DESN feature design
reuses that static scan, whereas the DLM replaces coefficient draws with
root-specific FFBS path draws.

## Current status

The repository contains a manuscript, derivation supplement, and development R
package under **application/**:

- **main.tex** presents the static regression, frozen-feature DESN, and dynamic
  linear-root targets in that order.
- **rqr-gibbs-supplement.tex** gives the population, augmentation,
  learned-scale, FFBS, and component-discount derivations.
- **application/R/** contains the shared fixed-design transition; native
  ridge, full-Gaussian, and RHS-NS priors; observed-mask and exact-continuation
  contracts; immutable DESN training/future designs; exdqlm-compatible DLM
  model builders; pure-R FFBS; exact component-scale evolution; and the
  RQR-DLM sampler.
- **application/src/** contains the C++17/RcppArmadillo FFBS kernel and the
  noncentered component-path basis used by exact scale interweaving.
- **application/tests/** contains native package gates, standalone workflow
  contracts, and copied exdqlm reference tests. The copied tests and
  repository-level workflow tests remain available to dedicated Make targets
  but are excluded from the package source where appropriate; `R CMD check`
  runs the native package contract.
- **application/vignettes/ordinary-rqr-v1.Rmd** gives a lightweight,
  executable introduction to native fixed-design RQR and an RQR-DESN readout
  conditional on an already frozen feature design.
- **docs/implementation_notes/rqr_dlm_native_design_20260722.md** freezes the
  exact and experimental evolution-mode contracts.

The ordinary zero-tilt version-1 source candidate is implemented. Its release
claim remains pending exact-runtime package, reference, manuscript,
representative one-cell benchmark, and bounded 48-fit validation. The
validator has four separate modes: `preflight`,
`reference-only`, `benchmark-one-cell`, and `execute-bounded`. The tracked
bounded configuration is deliberately disabled; no ordinary-v1 validation
result is claimed merely from source completion.

The reference stage includes a separate four-chain learned-rate F01 sampler
oracle against deterministic collapsed quadrature. Protected RQR-DLM evidence
is consumed through a compact five-file companion generated from fresh DLM
reference and correction bundles; neither gate is counted inside the final
48-fit static/DESN grid.

Fixed evolution covariances, frozen discount templates, and shared
component-specific inverse-Gamma evolution scales define exact samplers for
their stated generalized posteriors. A mixed-derivative audit shows that the
exdqlm-style adaptive conditional-discount kernels are not generally compatible
with a common smooth joint density while retaining their simple FFBS forms.
That mode remains available only as an explicitly experimental working update.
The public constructor is deliberately named
`rqr_evolution_adaptive_working()`; exact alternatives use
`rqr_evolution_fixed()`, `rqr_freeze_discount_template()`, or
`rqr_evolution_component_scale()`.

The package defaults to fail-fast Gaussian factorizations and rejects any
negative-eigenvalue projection. An audit mode can record repairs, but
mathematical target status, numerical execution status, and reproducibility
eligibility are reported separately. Promotion requires an exact target, zero
repairs, and a clean checkout at an explicitly expected commit. Compact fit
objects retain terminal state draws, integrity-digested RNG checkpoints, a
versioned schema, Git/R/compiler/BLAS provenance, and complete
data/model/target/evolution digests; full paths are opt-in. A portability
override is durable and removes reproducibility and promotion eligibility.
Continuation also carries a separately digested cumulative history contract,
including every mismatch/override generation, and compares the requested and
resolved FFBS backends before making a bitwise claim. When an expected commit
is declared, promotion binds the executing `rqrgibbs` namespace to a verified
isolated-runtime attestation of that exact commit, not merely to a clean
checkout path or installed version string. Direct source loading remains
exploratory/test-only.
The version-5 attestation links the exact Git archive to the built source
package, rehashes the actual command receipts and logs, and requires a lineage
marker in the executing installed runtime. Continuation history derives
per-segment exactness, target eligibility, mismatch/override effects, and
promotion status from raw facts under a versioned target digest. Aggregate
history statuses and saved RNG state are validated before any coercion.
Component-scale forecasts can combine saved evolution-scale draws with fixed
future component templates.

The pinned exdqlm branch remains a read-only reference and an optional
materializer for deterministic DESN feature designs. Static ordinary-RQR
inference and RHS-NS state updates are native and do not load exdqlm. A fit
conditional on a serialized `rqr_desn_design` likewise needs no executing
exdqlm namespace. Only the explicitly selected reference materializer requires
an isolated, attested exdqlm runtime at the exact pinned commit. Direct
source-tree loading is prohibited.

The validation runner preserves that boundary per fit: standalone
fixed-design cells bind the isolated primary `rqrgibbs` runtime and require no
external repository, whereas promotion-grade D02 DESN cells require exactly
the pinned isolated `exdqlm` runtime. The D02 receipt explicitly stores
source/runtime lineage, the materializer-argument digest, and the complete
materialized-design-payload digest. Raw effective arguments and the seed remain
in the tracked configuration and compact materialization evidence rather than
being duplicated inside the receipt.
That receipt attests the training design only. A versioned DESN future-design
contract checks feature alignment, declared forecast semantics, and the
content digest of the supplied future rows, but it does not establish how
those rows were generated. The training receipt cannot be transferred to
future rows. Until a separate future-specific materialization receipt is
implemented and verified, DESN future-root objects are explicitly
nonpromotable. The bounded future checks exercise conditional root mechanics
for frozen rows; they are not evidence of scientific forecast provenance or a
response-simulation distribution.
`make prepare-exdqlm-runtime` uses read-only Git access to create an archive,
then builds entirely under the ignored `application/cache/` tree. Its
versioned attestation binds the source commit and tree, archive checksum,
installed package digest, and disjoint archive/runtime paths while recording
that the full external checkout—including ignored files—was unchanged. The
reference materializer also refuses a namespace whose package path contains
Git checkout metadata. Reference smoke tests are extracted from the attested
archive and never execute from the exdqlm checkout.

Static continuation is available only through the validated public
`rqr_mcmc_continue()` boundary. Direct calls to `rqr_mcmc_fit()` cannot inject
continuation-only state: the implementation uses a private token-bound worker
after checkpoint/history validation and rejects unknown or ambiguous `init`
and `mcmc_control` fields.

For validation-chain initialization, `lambda_initial` and the positive
`latent_v` placeholders are computationally valid starting values, but they
are not independent overdispersion dimensions. Under the mandatory
partial-collapse order, the learned rate is drawn from its collapsed
conditional and every observed latent scale is then refreshed before either
root update. Fixed-rate scans likewise refresh the latent scales before their
first root update. The prespecified overdispersed profiles therefore separate
chains through the root coefficients and, for RHS-NS fits, their complete
root-specific prior states.

The corrected frozen learned-scale bounded pilot passed at source commit
`3a37c5ee42973fd0ba1fa4792f609d1a48bcc98f`: four production collapsed chains,
four independently coded fully augmented chains, and adaptive root quadrature
agreed under the predeclared MCSE gates, with all R-hat/ESS gates satisfied and
zero numerical repairs. This is a computational target check, not evidence of
empirical coverage calibration. Exact results and the one pre-scientific
diagnostic-infrastructure failure are recorded in
`docs/audits/chatgpt_pro_output5_audit_20260722.md`.

At implementation commit
`e24feb411b2e30586d1bfdc18bf6acb1fb568c70`, the expanded bounded RQR-DLM
reference suite passed 43 of 43 gates and the frozen 6,000-retained
component-scale/learned-rate benchmark passed all 150 diagnostics with zero
numerical repairs and exact runtime provenance. The independent Output-11
review accepted those target and mixing results and identified two narrow
artifact-publication boundaries. They were corrected at
`53dc71d873ef12ebba91cbc3d6813682e0987960`; fresh exact-source validation
again passed all 43 reference gates and 150 benchmark diagnostics. Output-12
then approved a separately gated bounded launch. That launch failed closed in
its first fixed-W chain because the runner required time-zero state estimands
that fixed-W fits did not yet retain; no chain file or later fit was produced.
The correction at `da4d265af6d8c6d6f9be06bfe2a91bfae88501d8` completes exact
fixed-W and frozen-template paths at time zero, shares one estimand extractor
between reference and execution modes, and strengthens all six continuation
references. Fresh exact-source validation passed 43 of 43 reference gates and
150 of 150 benchmark diagnostics. The execution flag was revoked at
`0d64331732fe4118e7234f6f23a851f5d98e6614` pending independent review. See
`docs/audits/chatgpt_pro_output12_bounded_failure_reconciliation_20260724.md`
and `external_reviews/chatgpt_pro_output12_20260724/`.

Output-13 independently accepted the correction. A new exact-source launch at
`afc9c5fed14c66317b684fc9b9f6d01079c307cd` then completed all 24 bounded
RQR-DLM fits. All 897 predeclared R-hat and bulk/tail ESS diagnostics passed,
all fits used exact fixed-joint modes, and the run recorded zero numerical or
forecast repairs. This completes bounded target-mechanics and mixing
validation; it does not establish empirical coverage or comparative forecast
performance. The execution flag is again false. Compact evidence and the
closeout are in `docs/audits/rqr_dlm_output13_bounded_20260724/` and
`docs/audits/rqr_dlm_output13_bounded_closeout_20260724.md`. A fail-closed
ADEMP-style draft for the first matched RQR-DLM simulation is in
`docs/implementation_notes/rqr_dlm_main_simulation_preliminary_spec_20260724.md`.

Output-14 accepted the completed bounded validation with nonblocking promoter
corrections. Evidence reuse now requires an externally frozen
source/runtime/reference bundle, recomputed checkpoint-state and
continuation-history digests for every reopened fit, successful continuation
validation, and exact 24-fit set equality across the relevant compact
manifests. These checks do not require another bounded run.

Output-15 replaced the preliminary schema with the frozen
`rqrgibbs_dlm_main_simulation/1.0.0` ADEMP contract. The implementation imports
the exact 208-row incidence matrix (89 included and 119 explicitly omitted
cells), reproduces its initial/central/maximum budgets, uses collision-checked
full L'Ecuyer-CMRG states, and separates conditional-mean roots, realized
future roots, and generated future responses. The reduced-AL DQLM MCMC and
static quantile-regression comparators come only from exact isolated CRAN
`exdqlm` 1.1.0 and `quantreg` 6.1 runtimes; the protected exdqlm checkout is
never an execution source.

The confirmatory runner now has preflight, oracle-reference,
embedded-sentinel, execution, collection, and audit modes. A deterministic
wave plan assigns the embedded sentinel phase to at most eight workers and
standard phases to at most 32 one-compute-thread workers. BLAS, OpenMP, and
related numerical-library settings are fixed at one. The sampled process
monitor separately permits at most four operating-system threads per worker,
the empirically validated envelope for the R runtime and its helper
processes; this is distinct from numerical parallelism. Collection requires the
authorization-bound task plan, verifies every recursive artifact manifest,
rejects duplicate or missing worker shards and replication IDs, and requires a
common source/runtime/seed bundle before analysis. Failed fits remain in the
intention-to-run denominator. Confirmatory execution remains false pending a
new independent review and a separate flag-only authorization commit; no
standalone performance pilot is planned.

Cross-wave execution is also fail closed. An append-only run state accepts
only the next canonical wave, requires the same-batch sentinel pass before a
standard wave, and requires a verified prior precision decision before a
larger replication batch. The complete coordinator can resume only after a
terminal wave record, collects evidence at each batch boundary, and stops
permanently after a failed or incomplete wave. A separate read-only health
check reports the supervisor, terminal-wave count, latest collection, next
canonical wave, and local artifact size without changing the run.

The first authorized full-study wave at
`b8b7748ab181a006611b602f64d4edf5be591de6` stopped fail-closed. Its completed
M01 fits exposed poor centered-only mixing for the shared component evolution
scale, and its completed M02 fits exposed an invalid flattening of the
multistate `exdqlm` posterior mean. No partial scientific result is reusable.
The correction adds an exact centered--noncentered interweaving transition,
projects each comparator state through its observation design, and freezes
uniform role-specific schedules without adaptive extension. The canonical
design, priors, seeds, estimands, learning-rate modes, targets, and diagnostic
thresholds are unchanged. The correction audit, fixed budget overlay, and
validation plan are recorded in
`docs/audits/rqr_dlm_main_wave1_scale_and_projection_failure_20260726.md`,
`docs/audits/rqr_dlm_main_correction_budget_20260726.csv`, and
`docs/implementation_notes/rqr_dlm_component_scale_interweaving_20260726.md`.

A second authorization-bound run stopped at its second wave when a fixed
future-design horizon was reused for shorter training horizons.  The
prospectively declared horizon is now passed through every generator,
sentinel, execution, and collection boundary; no result from that run is
reused.  A third fresh run passed its static-Gaussian sentinel wave but stopped
fail-closed in the local-level sentinel wave.  The third stop exposed an R
dimension-dropping defect for one-state exdqlm draws, three fixed-schedule M01
component-scale mixing failures, incomplete compact publication of
post-fitting diagnostic exceptions, and insufficient memory margin caused by
immediate duplicate deserialization of an ignored sentinel sidecar.  None is
an exdqlm source defect, and neither the exdqlm nor Q-DESN repository is
modified.

The complete second-wave development gate then showed that schedule matching
alone was insufficient: all 49 M01 fits completed, but only 1,131 of 1,150
diagnostics passed and 12 of 25 tasks failed at least one gate, predominantly
for the shared component scale.  The current fail-closed correction preserves
singleton state arrays as
`p`-by-`T` matrices, matches component-scale and M02 sentinel schedules to
their already frozen standard schedules, holds the full M02 DLM target common
across chains while supplying distinct target-preserving MCMC warm starts
through the CRAN interface, moves diagnostic construction inside the
structured failure boundary, and retains only compact endpoint/diagnostic
objects instead of accumulating full sentinel fits. It also adds an exact
one-root partially collapsed component-scale transition: a deterministic C++
Kalman marginal integrates one root for the scale update, that root is redrawn
by FFBS, and the existing ASIS move follows. This changes the transition, not
the generalized posterior or iteration-count budget. A development-only
four-profile comparison selected three slice sweeps before the complete
exact-runtime wave gates; its outputs are not scientific or promotion
evidence. The maximum contract now
contains 205,658,000 MCMC iterations: 74.8257 percent above the original
Output-15 budget and 3.2949 percent above the previously launched ASIS-corrected
budget.  The execution flag remains false until exact-source projection, M01
and M02 mixing, resource, package, and document gates pass.
The exact third-run closeout and recovery boundary are recorded in
`docs/audits/rqr_dlm_main_third_launch_wave2_closeout_20260727.md`,
`docs/audits/rqr_dlm_second_wave_component_scale_diagnosis_20260727.md`,
`docs/audits/rqr_dlm_main_correction_budget_20260727.csv`, and
`docs/audits/rqr_dlm_relaunch_readiness_audit_20260727.md`, together with
`docs/implementation_notes/rqr_dlm_main_third_launch_recovery_plan_20260727.md`.
A replacement coordinator may start only from a fresh exact-commit
authorization and a new ignored run root after the complete first-wave and
affected-wave correction gates pass.

## Pinned external reference

Expected exdqlm RQR branch:

    repo: https://github.com/AntonioAPDL/exdqlm
    branch: feature/rqr-desn-readout-20260716
    expected commit: dffb71ee70b597d6a716ee74be1cbc99731cd453

## Local-only workspaces

The literature PDFs, generated manifests, ChatGPT Pro handoff files, and heavy
application data, caches, runs, logs, and outputs are intentionally ignored.
Use **application/scripts/01_build_literature_manifest.R** to recreate the local
PDF inventory and checksums.

## Basic commands

    make smoke
    make pdf
    make supplement
    make package-document
    make package-install
    make test-native
    make test-ordinary-v1
    RQR_EXPECTED_PRIMARY_COMMIT="$(git rev-parse HEAD)" \
      make materialize-ordinary-v1-desn
    RQR_EXPECTED_PRIMARY_COMMIT="$(git rev-parse HEAD)" \
      make preflight-ordinary-v1
    RQR_EXPECTED_PRIMARY_COMMIT="$(git rev-parse HEAD)" \
      make reference-ordinary-v1
    RQR_EXPECTED_PRIMARY_COMMIT="$(git rev-parse HEAD)" \
      RQR_ORDINARY_V1_BENCHMARK_CONFIRM=I_CONFIRM_ORDINARY_V1_ONE_CELL_BENCHMARK \
      make benchmark-ordinary-v1-one-cell
    make test-ordinary-v1-monitor
    make test-ordinary-v1-dlm-companion
    make test-standalone-contracts
    make prepare-exdqlm-runtime
    make test-exdqlm-rqr
    make literature-manifest
    RQR_EXPECTED_PRIMARY_COMMIT=<reviewed-full-sha> \
      RQR_BOUNDED_PILOT_CONFIRM=YES make bounded-pilot
    RQR_EXPECTED_PRIMARY_COMMIT=<reviewed-full-sha> \
      make preflight-dlm-bounded
    RQR_EXPECTED_PRIMARY_COMMIT=<reviewed-full-sha> \
      make reference-dlm-bounded

The ordinary-v1 benchmark is one four-chain D02 DESN RHS-NS learned-rate cell,
not a pilot or part of the 48-fit grid. It must pass while the source-candidate
execution flag is still false. A later flag-only authorization commit can run
`execute-ordinary-v1-bounded` only with `RQR_ORDINARY_V1_CONFIRM=YES` and
explicit `RQR_ORDINARY_V1_REFERENCE_DIR`,
`RQR_ORDINARY_V1_BENCHMARK_DIR`, and
`RQR_ORDINARY_V1_BENCHMARK_MONITOR_DIR` pointing to the exact bound evidence,
plus `RQR_ORDINARY_V1_DLM_COMPANION_DIR` pointing to the reviewed compact
protected-DLM companion.

No production simulation should be launched until its matched protocol is
frozen and explicitly approved. The bounded pilot does not provide that
approval, and the committed bounded-dynamic execution flag is false.
`make test-exdqlm-rqr` and `make bounded-pilot` prepare the isolated
runtime automatically; neither target compiles or writes inside an exdqlm
repository.
