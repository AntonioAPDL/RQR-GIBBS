# rqrgibbs 0.1.0.9026

- Added a versioned, exact-field RQR-DLM evolution contract and made
  exact-target, ordinary-v1, continuation, time-zero, and promotion semantics
  reconstructible from the canonical evolution mode rather than mutable fit
  metadata.
- Made frozen discount-template provenance reconstructible from its declared
  reference variance, reference design, numerical policy, jitter ladder, and
  Joseph-form covariance recursion.
- Hardened fitted DLM and fixed-design envelopes, coefficient-prior contracts,
  and loss-rate priors against digest-consistent semantic mutation.
- Advanced the static fit envelope to `rqrgibbs_static_fit/1.2.0` and the DLM
  fit envelope to `rqrgibbs_fit/1.14.0`.
- Added exact, content-digested static draw and interval-evaluation envelopes
  (`rqrgibbs_static_draws/1.0.0` and
  `rqrgibbs_interval_prediction/2.0.0`). Native outputs bind the retained fit,
  target, design, draw-selection operation, and RNG state; explicit coefficient
  matrices remain deliberately unbound and nonpromotable.
- Added the corresponding frozen-design DESN output envelopes
  (`rqrgibbs_desn_draws/1.0.0` and
  `rqrgibbs_desn_prediction/1.0.0`). These qualify a readout conditional on an
  already materialized deterministic feature design; they do not qualify a
  native reservoir generator, reservoir re-estimation during MCMC, or a
  response-simulation distribution.
- Restricted fitted-time DLM extraction and prediction to complete validated
  fits and introduced typed draw, fitted-evaluation, and future-root envelopes
  (`rqrgibbs_dlm_draws/1.0.0`, `rqrgibbs_dlm_prediction/1.0.0`, and
  `rqrgibbs_dlm_forecast/1.0.0`). Explicit state-only forecast fixtures remain
  unbound and nonpromotable.
- Canonicalized DLM missing-site latent placeholders and numerical jitter
  ladders. Learned-scale continuation now restores latent checkpoints without
  an unintended second division, and inactive or altered numerical controls
  cannot silently change a continued transition.

# rqrgibbs 0.1.0.9024

## Ordinary RQR version-1 candidate

- Added one native fixed-design Gibbs transition for ordinary (zero-tilt)
  RQR with fixed or normalized learned generalized-Bayes rates.
- Added closure-free ridge, full-Gaussian, and native RHS-NS coefficient-prior
  contracts, including complete root-specific prior-state swaps.
- Added observed-mask missing-response semantics, versioned checkpoints,
  integrity-checked exact continuation, and schema-bound interval prediction.
- Made fixed-design column identity fail closed: a design is either unnamed or
  has complete, nonempty, unique column names, and prediction preserves the
  same named/unnamed contract.
- Added immutable training and future DESN feature-design contracts. Frozen
  DESN readouts reuse the fixed-design transition; promotion-grade training
  materializations require the pinned isolated exdqlm runtime and a
  reverified receipt. A future contract verifies feature alignment, semantics,
  and row content, but its root outputs remain nonpromotable until a separate
  future-specific materialization receipt is implemented.
- Added a design-only materialization orchestrator, a strict two-commit
  authorization contract, same-runtime reference binding, and per-chain
  conditional future-root contract evidence. Materializer artifacts are
  validated as regular files and published by same-directory rename without
  unlinking prior evidence; symbolic-link and nonregular targets fail closed.
- Added a bounded, disabled-by-default 48-fit validation protocol, compact
  evidence schemas, process-group monitoring, and lightweight CI gates.
- Added a separate learned-rate F01 oracle: deterministic collapsed
  quadrature reproduces six posterior means and five corrected event
  probabilities, then four native chains must agree within maintained Monte
  Carlo error. Superseded Output-6 CDF values are retained only as historical
  audit evidence.
- Added a compact protected-DLM companion contract that validates fresh
  reference, two-wave M01/M02, horizon/M03, and resource-envelope evidence
  without rerunning, deserializing, or copying fitted dynamic objects. The
  bounded ordinary grid cannot be authorized without that closed five-file
  receipt backed by 55 hash-bound inputs and 23 semantic gates. On execution,
  the five validated receipt files are retained under a namespaced compact
  output and rehashed after the R process exits.
- Closed every successful mode to an exact compact file set, including hidden
  entries, and rejected symbolic links, unexpected directories, nonregular
  files, and residual progress files. The process wrapper now binds every
  R-output artifact rather than a four-file subset.
- Added a separate fail-closed partial-evidence allowlist with mandatory
  terminal failure/status rows, strict manifest schemas and nonsymlink roots,
  plus an exact six-file monitor contract exercised by ten shell fault
  scenarios.
- Added bitwise end-to-end ridge/full-Gaussian transition equivalence checks
  in both accepted rate modes and exact three-segment continuation checks with
  thinning greater than one.
- Preserved the RQR-DLM target and its interpretation. The pseudo-AL variables
  augment a generalized-Bayes loss and do not define a response likelihood or
  posterior-predictive response distribution.
- Hardened the exact RQR-DLM implementation: filter inputs and recursive
  covariances now fail closed at the R/C++ boundary; time-zero conditional
  state completion and the one-root partially collapsed component-scale step
  contribute to the repair ledger; and the complete transition kernel is
  checkpoint-, history-, and continuation-bound. Fixed-W state storage no
  longer changes the transition.
- Advanced the shared fit envelope to `rqrgibbs_fit/1.12.0` and continuation
  history to `rqrgibbs_continuation_history/5.0.0` for the versioned DLM
  transition-kernel identity and reconstructed exactness contract.
- Standardized the fitted DLM metadata as `model_spec$transition_kernel` and
  `model_spec$transition_kernel_digest` for fixed-covariance,
  frozen-discount-template, and component-scale evolutions alike. Objects
  from earlier development schemas must be refit; they are intentionally not
  coerced into the version-1 continuation contract.

Nonzero mean tilt, CAVI/ELBO, response simulation, adaptive conditional
discounting as exact Gibbs, and matched scientific simulations remain outside
this version-1 candidate.

### Pre-1.0 interface migration

- Removed DESN variational Bayes from the ordinary-v1 interface. DESN fitting
  now accepts MCMC only; experimental VB code is outside the version-1
  contract.
- Removed the unfinished fixed-design `rqr_vb_fit()` prototype from the public
  namespace. Its implementation and S3 methods remain internal for bounded
  research checks, but it is experimental and outside ordinary-v1 promotion.
- Retained `beta_prior()` as a nondeprecated compatibility wrapper for
  `rqr_beta_prior()`; new code should use the native constructor directly.
- `rqr_desn_fit(..., fit_readout = FALSE)` now returns the validated frozen
  design rather than a fitted readout.
- Static and DESN fits now use versioned fit envelopes, integrity-digested
  checkpoints, cumulative continuation history, and source/runtime provenance.
  Callers should use the public continuation and prediction methods rather
  than constructing or mutating those objects.
- A verified training-design receipt does not attest future rows. DESN future
  outputs remain nonpromotable without a future-specific materialization
  receipt, even when their versioned future-design contract validates.
- `lambda_initial` and initial latent-scale placeholders are refreshed by the
  mandatory partial-collapse order and are not independent overdispersed-start
  controls. Chain dispersion is supplied through root coefficients and, for
  RHS-NS fits, complete prior states.
