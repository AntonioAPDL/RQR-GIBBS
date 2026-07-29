# Newsuggestions integration note, 2026-07-28

This note records the bounded implementation response to the local prompt
`newsuggestions.md`, which is intentionally ignored because it is a working
handoff rather than a source artifact.

## Implemented in this pass

- Added an explicit static root-label contract.  Fixed-design MCMC now records
  that the estimand is an unordered exchangeable root pair, exposes the complete
  swap probability through `mcmc_control$root_label_control`, and stores
  canonical coefficient draws only if a post-processing audit passes on a
  declared design.
- Added `rqr_canonicalize_root_draws()` for complete-block static
  canonicalization.  The helper never modifies the chain state, never relabels
  coefficient components separately, and fails closed for crossing or ambiguous
  assignments.
- Added `rqr_canonicalize_root_paths()` for the analogous DLM ordinate-path
  diagnostic.  The keep/swap decision is one complete-path assignment per draw.
- Preserved pointwise ordered interval summaries as the primary root-label
  invariant reporting object.
- Upgraded Cornish--Fisher fixed mean-tilt pilots with moment diagnostics,
  probability-window diagnostics, boundary warnings, bootstrap stability flags,
  and explicit wording that these are first-order near-Normal anchors, not
  finite-sample empirical optimizers.
- Added an optional simultaneous-binomial held-out coverage guard for
  deterministic fixed-tilt candidate selection.
- Updated manuscript and package prose to distinguish raw labels, canonical
  post-processing, ordered endpoint summaries, and first-order tilt anchors.

## Deliberately not claimed as implemented

- The exact fractional empirical mean-tilt estimator and fractional
  subgradient-balance diagnostics remain a separate implementation item.  They
  need their own finite-sample contract and tests before they should influence
  reported estimators.
- DLM canonical state-array relabeling is not stored automatically in fitted
  objects.  The current helper works at the ordinate-path level and keeps the
  chain object unchanged.
- Nonzero mean tilt remains restricted to the already declared fixed-rate
  proper-Gaussian paths.  Learned-rate mean tilt, RHS-NS mean tilt, VB/CAVI
  tilt, and component-scale/adaptive dynamic tilt are still gated.
- The static sandwich/general covariance post-processing remains outside the
  validated package API.

## Rationale

The implemented pieces close the immediate interpretive risk: raw root labels
are exchangeable MCMC labels, not lower/upper coefficient estimates.  A global
coefficient or path chart can be useful, but it is only meaningful when the two
root functions do not cross on an explicitly declared audit domain.  The
package therefore now separates raw chain storage, invariant endpoint
functionals, and optional canonical summaries.
