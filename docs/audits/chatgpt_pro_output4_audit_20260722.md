# ChatGPT Pro Output 4 Audit and Implementation Resolution

Date: 2026-07-22

Repository: `AntonioAPDL/RQR-GIBBS`

Audited base: `ec0da71b33e436a70fadf929a3d1956dd9bee01c`

Output file SHA-256:
`98798bcc5766d15b9c8c22fe7403c45552732bc4a1c21c38851e9d63662537e7`

## Scope and decision

Output 4 was read completely and treated as an independent review, not as an
authority. Its mathematical derivations were reconciled with the manuscript,
supplement, native R implementation, C++ FFBS implementation, and pinned
exdqlm reference. Each new defect was also checked against the pre-patch
execution path.

The central statistical construction passes this audit:

- the fixed-rate generalized posterior is coherent for the declared RQR loss;
- the normalized and pure learned-scale targets remain distinct;
- the pseudo-AL representation augments a loss kernel rather than defining a
  response likelihood;
- the collapsed-lambda, complete-latent-refresh scan is valid in its declared
  order;
- fixed `W`, frozen templates, and shared component scales define fixed joint
  targets;
- adaptive conditional discounting remains a working sequential method rather
  than exact Gibbs for a fixed joint target;
- draw-specific component-scale forecasting is correct for future root states.

All seven Output-4 implementation findings were independently confirmed and
closed. Three recommended missing validation checks were also added. No
production simulation or application run was launched.

## Mathematical reconciliation

### Population target

For distinct roots `a < b`, subtracting the two population score equations
gives target coverage under the stated differentiability and no-endpoint-atom
conditions. Substitution gives the additional first-moment balance condition.
This is stronger than coverage alone but does not imply independence,
equal-tailed endpoints, or pointwise conditional coverage under a restricted
linear function class.

Coincident roots reduce the loss to a scaled squared-error target and therefore
target a conditional mean rather than a nontrivial interval. The article and
supplement retain these qualifications and do not claim a replacement
finite-sample theorem for the original optimization article.

### Learned scale and augmentation

The audited conditionals agree with the implementation:

- normalized collapsed shape: `a_lambda + n`;
- pure-loss collapsed shape: `a_lambda`;
- normalized fully augmented shape: `a_lambda + 3n/2`;
- pure-loss fully augmented shape: `a_lambda + n/2`;
- latent scale: `GIG(1/2, a, b_t)`;
- zero pseudo-residual limit: Gamma with shape `1/2`.

The learned `lambda` remains an analyst-declared generalized-target component.
It is not a response variance and is not automatically a calibration
parameter. Fixed-rate sensitivity remains primary.

### Stacked state and alternating FFBS

The two root paths can be concatenated into one state vector with block
evolution matrices. That notation describes the joint Gaussian state prior,
but it does not create one Gaussian measurement update. Squaring the augmented
residual-product term produces a fourth-order cross-term in the two root
ordinates. Conditional on either complete root path, the same kernel is
quadratic in the other path.

The main article and supplement now state explicitly that:

- one conventional joint Gaussian FFBS draw is unavailable;
- the implementation uses root 1 conditional on root 2, followed by root 2
  conditional on the new root 1;
- these are exact blocked Gibbs updates for the fixed-joint modes;
- the remaining concern is mixing, not target approximation;
- a general non-Gaussian joint path sampler could target the same object but
  would give up the closed-form FFBS blocks.

## Finding-by-finding resolution

### N1: continuation omitted target-defining fields

**Confirmed.** The old matrix list omitted `m0`, component metadata, coverage,
loss-scale mode and value, the lambda prior, component-scale hyperparameters,
and numerical settings.

**Resolution.**

- Added immutable model, target, and evolution contract objects.
- Stored SHA-256 digests for all three in fit provenance.
- The model contract includes `FF`, `GG`, `m0`, `C0`, component dimensions,
  component names, state dimension, and time length.
- The target contract includes coverage, rate mode, fixed rate where
  applicable, `s_L`, lambda prior, numerical policy, jitter ladder,
  exchangeability, and root-swap convention.
- The evolution digest covers the complete immutable evolution object,
  including component templates and prior hyperparameters.
- Continuation recomputes and compares all three digests before sampling.

Tests now mutate `m0`, component names, coverage, `s_L`, the lambda prior,
component-scale prior, and numerical ladder. Every mutation stops.

### N2: checkpoint and RNG integrity were unchecked

**Confirmed.** A different valid `.Random.seed` or another altered Markov state
was previously accepted.

**Resolution.**

- The complete checkpoint is now hashed after construction.
- The digest is stored outside the checkpoint as `checkpoint_digest`.
- Continuation verifies the checkpoint digest before restoring any state.
- The fit schema was advanced from `rqrgibbs_fit/1.1.0` to
  `rqrgibbs_fit/1.2.0`.

Tests independently mutate both root paths, latent scales, lambda, component
scales, both time-zero states, completed iteration count, and RNG state. Every
mutation stops with a checkpoint-integrity error.

The checksum detects accidental or ordinary object mutation. It is an
integrity check, not a cryptographic authenticity mechanism against an actor
who deliberately rewrites both the object and its digest.

### N3: environment override was not durable

**Confirmed.** The warning existed, but the returned segment did not retain the
mismatch and could recompute eligibility without inheriting the parent
history.

**Resolution.**

Every continued segment now stores:

- `continued_from_checkpoint`;
- the parent checkpoint digest and completed iteration;
- validated model/target/evolution digests;
- the exact environment mismatch fields;
- whether an override was used;
- whether bitwise continuation is claimed;
- parent and current-environment reproducibility status.

An override, or an ineligible parent checkpoint, removes reproducibility and
promotion eligibility from the returned segment. The warning remains.

### N4: DESN and RHS promotion omitted exdqlm provenance

**Confirmed.** The design and RHS adapters could depend on exdqlm while
promotion metadata described only the primary repository.

**Resolution.**

- `provenance_control` now accepts named external repositories.
- RQR-DESN MCMC and fixed-design RHS fits require the pinned exdqlm source
  state for reproducibility and promotion.
- The required commit is fixed at
  `dffb71ee70b597d6a716ee74be1cbc99731cd453`.
- Provenance records the installed exdqlm version, repository path, detected
  commit, Git availability, clean/dirty state, expected commit, and match.
- Promotion requires the external checkout to be detected, clean, and at the
  pinned commit.
- Exploratory fits may still run without a repository path, but they cannot
  become reproducibility or promotion eligible.
- The pinned-exdqlm smoke script now stops on a wrong branch, wrong commit, or
  dirty worktree rather than warning.

The protected exdqlm repository was not modified.

### N5: provenance completeness omitted toolchain fields

**Confirmed.** A direct reproducer showed `provenance_complete = TRUE` while
`compiler = NA`.

**Resolution.**

- Strict completeness now requires compiler, BLAS, LAPACK, backend, and RNG
  kind.
- Compiler metadata falls back to `R CMD config CXX17` when
  `R.version$compiler` is unavailable.
- `basic_provenance_complete` is retained separately for diagnostic use.
- Required dependency versions and required external repository metadata are
  part of strict completeness.

### N6: DESN horizon silently truncated

**Confirmed.** `as.integer(H)[1L]` changed `2.9` to `2`.

**Resolution.**

The public method now uses the common strict scalar-integer validator.
Fractional, missing, nonfinite, and nonpositive horizons fail before design
construction.

### N7: material asymmetry was silently changed

**Confirmed.** `C0` and component templates were averaged with their
transposes before material asymmetry was checked.

**Resolution.**

- Added a common scale-aware symmetry validator.
- Materially asymmetric `C0` and component templates now stop.
- Only floating-point-level asymmetry within the declared tolerance is
  averaged.
- The existing fixed-`W` cube validation uses the same helper.

Tests cover rejection of material asymmetry and acceptance of machine-level
roundoff.

## Additional validation added

### Cross-time FFBS path moments

The dense Gaussian test already constructed an independent full posterior over
the concatenated path. It now also:

- samples 5,000 complete paths with the C++ FFBS kernel;
- compares all sampled means with dense-posterior means using Monte Carlo
  standard errors;
- compares selected cross-time covariance blocks with the dense covariance
  using their analytic Monte Carlo standard errors.

This tests the joint path draw, not only marginal smoothing moments or R/C++
parity.

### Component-scale forecast moments

A scalar local-level fixture with fixed terminal state and known component
scale now compares future root-state means and variances with the analytic
random-walk values over three horizons. It also verifies scale-draw/index
alignment and zero repair records.

### Continuation mutation matrix

The continuation tests now cover all target, evolution, checkpoint, RNG,
environment-override, and eligibility behaviors listed above, in addition to
exact full-chain versus split-chain identity.

## Validation evidence

The following gates passed on Jerez after the patches:

- focused native model, FFBS, and sampler tests;
- installed-package native tests;
- `R CMD check --no-manual`, status `OK`;
- environment smoke check;
- article PDF build, 8 pages;
- supplement PDF build, 10 pages;
- strict pinned-exdqlm smoke tests at the exact branch and commit;
- literature manifest for 18 local PDFs.

The package version is now `0.1.0.9003`.

Generated package archives, check directories, shared libraries, object files,
PDFs, TeX logs, and literature manifests remain ignored. No fitted models,
application data, or simulation output were added to Git.

## Recommendations not converted into production claims

The following Output-4 recommendations are valid but intentionally remain
future gates:

1. The collapsed-versus-fully-augmented learned-scale experiment, with
   independent quadrature, remains required before learned-scale evidence is
   promoted. It is a bounded validation run rather than a source-correctness
   patch.
2. Fixed-rate sensitivity remains primary; passing the experiment would not
   make learned lambda a calibration parameter.
3. A separate frozen bounded-pilot manifest must record the post-patch commit,
   seeds, fixtures, resources, artifact hashes, and acceptance criteria before
   launch.
4. Remote CI remains desirable. It should be added only with a deterministic
   installation contract for the exact exdqlm commit; local `R CMD check`
   evidence is not represented as remote CI.
5. A second external GIG implementation is useful optional evidence. The
   actual R sampler already has analytic, quadrature-CDF, extreme-grid, and
   independent SciPy checks, so another runtime dependency is not required for
   the first bounded pilot.
6. General singular multivariate state systems remain outside the supported
   first-pilot contract.

## Updated go/no-go decision

| Feature | Decision after patches |
|---|---|
| Fixed-design fixed-rate ridge | Go for bounded validation |
| Fixed-`W` RQR-DLM | Go for bounded validation after clean-commit manifest |
| Frozen-template RQR-DLM | Go for bounded validation with zero repairs |
| Component-scale RQR-DLM | Go for bounded validation with zero repairs |
| Component-scale root forecasting | Go for bounded validation |
| Learned normalized scale | Validation only; collapsed/augmented/quadrature gate still required |
| Pure learned scale | Diagnostic only |
| RQR-DESN or RHS | Conditional go only with exact clean exdqlm provenance |
| Adaptive discount as exact Gibbs | No-go |
| Response-predictive scoring or simulation | No-go without a separate response contract |
| Production matched simulation | No-go until the bounded pilot passes |

The appropriate next action is independent review of this patch set, followed
by a separately frozen bounded-pilot manifest. It is not a production launch.
