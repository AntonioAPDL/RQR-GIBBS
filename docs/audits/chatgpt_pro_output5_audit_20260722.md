# ChatGPT Pro Output-5 audit and implementation closeout

## Executive decision

Output-5 was mathematically careful and materially useful. Its central
conclusions were confirmed:

- the stacked two-root state prior is Gaussian under the declared block
  evolution;
- the joint augmented observation kernel is quartic in the stacked root state,
  so one conventional simultaneous Gaussian FFBS draw is not available;
- alternating root-specific FFBS draws are exact blocked Gibbs updates for
  fixed-joint evolution modes;
- the adaptive conditional-discount recursion remains a working update rather
  than an exact Gibbs kernel for a demonstrated joint target; and
- three implementation defects remained at the prior handoff commit
  `4150b036048f02c1d2f270b1d014093c997ea8e0`.

All three defects were independently reproduced and corrected. The source
correction was committed as
`e267f9b73ab905c5cd71776d2c7be95354391489`; a one-line diagnostic
infrastructure correction was committed as
`6429b4698d04da43ef9c76f3ab534351e0fdae50`. The frozen bounded pilot at the
latter commit passed every predeclared gate. No production simulation or
application analysis was launched.

## Material inspected

The audit read all of local-only `Chatgpt_output5` (1,261 lines, 47,892 bytes;
SHA-256
`ea0c38670647340e74bc3301b82ec4b2b82aee0e082bfec56da2ab01fdb90dc8`)
and reconciled it against:

- the RQR-GIBBS source, tests, manuscript, supplement, and generated
  documentation at `4150b036048f02c1d2f270b1d014093c997ea8e0`;
- the clean pinned exdqlm checkout on
  `feature/rqr-desn-readout-20260716` at
  `dffb71ee70b597d6a716ee74be1cbc99731cd453`; and
- the clean Q-DESN reference checkout on `main` at
  `f9f22804eff3871bb5350c8add04b7c9f4d4957b`.

The protected exdqlm and Q-DESN repositories were read only and were not
mutated.

## Finding-by-finding disposition

| Finding | Output-5 verdict | Independent disposition |
|---|---|---|
| N1: complete continuation target digests | Pass | Confirmed; unchanged |
| N2: checkpoint and RNG integrity | Pass | Confirmed; unchanged |
| N3: durable continuation history and eligibility | Partial | Corrected and tested |
| N4: hard exdqlm provenance for DESN/RHS | Partial | Corrected with runtime binding and tested |
| N5: strict provenance completeness | Pass | Confirmed; retained |
| N6: strict DESN forecast horizon | Pass | Confirmed; retained |
| N7: symmetry validation | Partial | Corrected across actual matrix scales and tested |

### D1: executing exdqlm code was not bound to the attested checkout

The bypass was real. Loading an installed exdqlm namespace from a different
library while presenting a separate clean pinned checkout could previously
leave repository provenance complete and promotion eligible. Package version
and checkout state did not establish which code supplied the executing
namespace.

The correction adds two supported runtime contracts:

1. a direct source contract, in which `pkgload::load_all()` binds the namespace
   path to the exact clean checkout; and
2. an installed contract, in which an isolated package library is built from a
   Git archive of the pinned commit and an ignored local attestation binds the
   source commit/tree, package version, installed namespace path, source-archive
   SHA-256, and installed-tree SHA-256.

`application/scripts/04_prepare_pinned_exdqlm_runtime.R` implements the
deterministic isolated build. Fits now persist runtime path, source and runtime
tree digests, direct-path and attestation status, and
`runtime_source_match`. Required exdqlm fits are reproducibility eligible only
when that match is true.

The original mismatched installed-runtime reproducer now reports
`runtime_source_match=FALSE` and
`reproducibility_eligible=FALSE`. The direct pinned source smoke test and the
isolated installed-runtime attestation both pass. This is a reproducibility and
integrity contract, not a claim of cryptographic trust against a malicious
local actor who can rewrite both code and attestation.

### D2: tiny-scale covariance validation used a unit scale floor

The defect was real. Expressions of the form `max(1, scale)` made absolute
tolerances dominate matrices whose legitimate scale was far below one. A
materially asymmetric or indefinite tiny covariance could therefore pass
validation.

Symmetry and eigenvalue materiality now use the actual finite matrix scale in
R. Exactly zero matrices have their own explicit branch. Cholesky jitter in R
and C++ is matrix-relative for every nonzero matrix; the exactly zero case uses
a separately identified absolute fallback and records
`absolute_jitter_fallback`, `jitter_scale`, actual `matrix_scale`, and applied
jitter.

Tests span scales from `1e-300` through `1e300`, reject tiny material
asymmetry and a tiny indefinite fixed evolution covariance before filtering,
accept machine-relative roundoff, and retain the exact-zero contract. R and C++
repair ledgers use the same fields.

### D3: continuation promotion and bitwise claims were segment-local

The defect was real. A repaired parent could yield a zero-repair child marked
promotion eligible, and the prior bitwise flag could be true when source state
was dirty or unverifiable. `backend="auto"` also recorded the request without
freezing the backend actually used.

The fit schema is now `rqrgibbs_fit/1.3.0`. A continuation carries:

- parent and cumulative numerical-repair counts;
- parent and complete-chain numerical exactness;
- parent target and promotion eligibility;
- requested and resolved FFBS backend; and
- current and inherited reproducibility eligibility.

Template-construction repairs are counted once rather than once per segment.
Promotion is the conjunction of the complete chain's target/numerical status,
the parent's durable promotion status, and inherited reproducibility status.
A bitwise claim additionally requires both parent and current reproducibility
eligibility, no environment override, no environment mismatch, and the same
resolved backend. Unknown/dirty source with an explicit portability override
now persists the override and produces neither a bitwise claim nor promotion
eligibility.

## Mathematical reconciliation

Let

```text
Theta_t = (theta_1t', theta_2t')'.
```

For fixed `W`, a frozen discount template, or current shared component scales,
the evolution is block Gaussian. Stacking therefore does not invalidate the
state prior. The obstacle is the augmented observation term. Because

```text
e_t = (y_t - F_t' theta_1t)(y_t - F_t' theta_2t),
```

the squared normal--exponential augmentation residual contains `e_t^2`, which
is quartic jointly in the two root states. The joint observation kernel is not
linear Gaussian in `Theta_t`, so a standard single stacked FFBS update is not
available.

Conditional on one complete root path, however, `e_t` is affine in the other
root state. The resulting pseudo-observation has a Gaussian state-space kernel,
and one complete root path can be drawn by FFBS. Updating root 1 conditional on
root 2 and then root 2 conditional on the new root 1 is consequently an exact
two-block Gibbs scan for the declared fixed-joint modes. It is not a mean-field,
Laplace, plug-in, or separate-fit approximation. A global root-label swap
preserves exchangeability and helps traverse the symmetric labels, but the
mixing rate remains an empirical question.

The collapsed normalized learned-scale update remains

```text
lambda | roots,y ~ Gamma(a_lambda+n, b_lambda+L/s_L).
```

The validation-only fully augmented scan uses

```text
lambda | roots,v,y
  ~ Gamma(a_lambda+3n/2,
          b_lambda + sum[v+(e-xi v)^2/(2 phi v)]/s_L).
```

These are two scans of the same normalized generalized posterior. The
same-data learned scale is still a declared convention; neither generalized
Bayes theory nor this computation makes it an automatic frequentist
calibration device.

## Frozen bounded pilot

The tracked protocol is
`docs/implementation_notes/rqr_bounded_pilot_protocol_20260722.md`; the launcher
is `application/scripts/05_run_rqr_bounded_pilot.R`. It uses the deterministic
intercept-only fixture, four collapsed seeds `73201:73204`, four fully
augmented seeds `73301:73304`, 5,000 burn-in iterations, 20,000 retained
unthinned draws per chain, fail-fast numerics, and adaptive probability-scale
root quadrature split at every response value.

The first attempt at source commit
`e267f9b73ab905c5cd71776d2c7be95354391489` stopped during chain diagnostics.
The ESS helper passed logical CDF/tail indicator matrices directly to `acf()`,
which requires numeric input. The chains had completed, but no scientific
comparison or pass decision was produced. The failure was retained under the
ignored local output tree. The helper was corrected by one explicit
logical-to-double coercion, directly tested, and committed as
`6429b4698d04da43ef9c76f3ab534351e0fdae50`. No target, sampler, seed,
iteration count, estimand, or acceptance threshold changed.

The frozen rerun passed in 6.68 minutes with one worker, approximately 0.4 GB
resident memory, 0.011 MB of artifacts before the hash manifest, and an empty
failure log. All artifact hashes revalidated.

### Diagnostic extrema

| Quantity | Result | Gate |
|---|---:|---:|
| Maximum rank-normalized split R-hat | 1.000377 | 1.01 |
| Minimum bulk ESS | 15,743.75 | 2,000 |
| Minimum tail ESS | 25,966.30 | 2,000 |
| Nonfinite draws | 0 | 0 |
| Numerical repairs across eight chains | 0 | 0 |
| Maximum quadrature relative error bound | 2.78e-12 | 1e-9 |

### Mean cross-checks

| Estimand | Collapsed | Fully augmented | Quadrature |
|---|---:|---:|---:|
| lambda | 1.134805 | 1.134642 | 1.134769 |
| ordered lower root | -1.431828 | -1.431406 | -1.429561 |
| ordered upper root | 2.444920 | 2.440968 | 2.444239 |
| width | 3.876748 | 3.872374 | 3.873801 |
| midpoint | 0.506546 | 0.504781 | 0.507339 |
| total RQR loss | 10.161492 | 10.153691 | 10.163729 |

Every collapsed-versus-augmented mean difference was below four combined
MCSEs. Every sampler mean was within four of its MCSEs of quadrature. All five
predeclared CDF comparisons passed the analogous combined-MCSE gate.

The local passing artifact root is:

```text
application/outputs/rqr_bounded_pilot_20260723T052044Z_6429b4698d04
```

It remains ignored by design. The tracked protocol, this closeout, exact
commits, seeds, gates, and summarized results are sufficient to reproduce it;
heavy chain draws were not stored.

## Validation matrix

The following completed successfully after the corrections:

- all R source, script, and test files parsed;
- `make test-native`;
- `make package-check`, with `Status: OK`;
- `make smoke`;
- `make pdf`;
- `make supplement`;
- `make literature-manifest`, covering 18 local-only PDFs;
- `make test-exdqlm-rqr`;
- `make prepare-exdqlm-runtime`; and
- `RQR_BOUNDED_PILOT_CONFIRM=YES make bounded-pilot`.

Generated PDFs, TeX logs, package archives/check directories, compiled
objects, runtime libraries, manifests, and pilot outputs are ignored. No heavy
model object or simulation output is tracked.

## What is solved and what remains

The three Output-5 source blockers are solved for the declared contracts:
runtime exdqlm is bound to source, continuation eligibility is cumulative and
backend-explicit, and matrix validation is genuinely scale-relative. The
learned-scale fixed-design implementation also passed an independent
collapsed/augmented/quadrature target check.

The following are deliberately not claimed as solved:

1. The bounded fixture is not a coverage-calibration study and does not justify
   production learned-scale claims.
2. Alternating root-path mixing has only been checked on small fixtures, not
   across the matched dynamic simulation grid.
3. The exact initial RQR-DLM study excludes general singular multivariate state
   systems and the adaptive conditional-discount working update.
4. Remote CI and a second external GIG implementation remain optional
   corroboration, not local blockers.
5. The VB/CAVI algorithm and ELBO still require a separate derivation,
   implementation, and validation. Current VB output remains
   screening/initialization only.
6. RQR-DESN development remains deferred until the RQR-DLM MCMC, VB, and
   matched simulation work are complete, as requested.

## Recommended next stage

The next task should freeze the matched RQR-DLM simulation design before
running it. It should compare fixed-design RQR, fixed-`W` RQR-DLM, frozen
discount templates, and shared component-scale RQR-DLM under common
data-generating mechanisms, sample sizes, training/forecast windows, seeds, and
loss/coverage/width/oracle-root metrics. The adaptive conditional-discount mode
should be excluded from exact-method claims and, if retained, labeled as a
working sensitivity analysis.

Before production, add bounded dynamic multi-component fixtures that exercise
the declared block discount/component-scale interface, verify forecast-root
moments and continuation across multiple segments, and freeze the production
manifest at a clean commit. The CAVI/ELBO derivation should then be reviewed
against the same normalized target and validated against MCMC on bounded
fixtures before being used for screening. Production execution still requires
explicit user confirmation.
