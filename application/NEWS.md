# rqrgibbs 0.1.0.9023

## Ordinary RQR version-1 candidate

- Added one native fixed-design Gibbs transition for ordinary (zero-tilt)
  RQR with fixed or normalized learned generalized-Bayes rates.
- Added closure-free ridge, full-Gaussian, and native RHS-NS coefficient-prior
  contracts, including complete root-specific prior-state swaps.
- Added observed-mask missing-response semantics, versioned checkpoints,
  integrity-checked exact continuation, and schema-bound interval prediction.
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
- Added a compact protected-DLM companion contract that validates the fresh
  reference, M01, M02, and horizon/M03 evidence without rerunning or copying
  fitted dynamic objects. The bounded ordinary grid cannot be authorized
  without that closed five-file companion. On execution, the five validated
  files are retained under a namespaced compact output and rehashed after the
  R process exits.
- Closed every successful mode to an exact compact file set, including hidden
  entries, and rejected symbolic links, unexpected directories, nonregular
  files, and residual progress files. The process wrapper now binds every
  R-output artifact rather than a four-file subset.
- Added a separate fail-closed partial-evidence allowlist with mandatory
  terminal failure/status rows, strict manifest schemas and nonsymlink roots,
  plus an exact six-file monitor contract exercised by ten shell fault
  scenarios.
- Preserved the existing RQR-DLM transition and its interpretation. The
  pseudo-AL variables augment a generalized-Bayes loss and do not define a
  response likelihood or posterior-predictive response distribution.
- Advanced the shared fit envelope to `rqrgibbs_fit/1.11.0` for the added
  scope/continuation fields and hardened fitted-draw and forecast boundaries;
  the DLM target, blocked transition, FFBS, and evolution laws are unchanged.

Nonzero mean tilt, CAVI/ELBO, response simulation, adaptive conditional
discounting as exact Gibbs, and matched scientific simulations remain outside
this version-1 candidate.

### Pre-1.0 interface migration

- Removed DESN variational Bayes from the ordinary-v1 interface. DESN fitting
  now accepts MCMC only; experimental VB code is outside the version-1
  contract.
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
