# Cornish--Fisher initialization for fixed mean-tilted RQR

Date: 2026-07-27
Scope: initializer and validation layer only
Source handoff:
`mean_tilt_cornish_fisher_initialization_validation_handoff.zip`

## Purpose

Mean-tilted RQR introduces a fixed retained-mean tilt \(\delta\) that indexes a
content-\(c\) interval family. The package implementation in this pass does
not sample \(\delta\), does not enable nonzero-tilt Gibbs sampling, and does
not reuse the ordinary learned-scale update at nonzero tilt. Instead, it adds
deterministic training-sample pilots that can initialize or screen fixed
candidate tilts in later validation workflows.

The neutral default remains ordinary RQR, corresponding to \(\delta=0\).

## Cornish--Fisher pilot

For coverage \(c\), define

\[
q_c=\Phi^{-1}\{(1+c)/2\}, \qquad
K_c=q_c\phi(q_c)/c.
\]

With adjusted Fisher--Pearson skewness \(\widehat\gamma_1\) and training
standard deviation \(\widehat\sigma\), the first-order Cornish--Fisher
shortest-oriented pilot is

\[
\widehat d_{\mathrm{SH}}^{\mathrm{CF}}
=-\widehat\gamma_1K_c,
\qquad
\widehat\delta_{\mathrm{SH}}^{\mathrm{CF}}
=\widehat\sigma\widehat d_{\mathrm{SH}}^{\mathrm{CF}}.
\]

The corresponding equal-tailed retained-mean pilot is

\[
\widehat d_{\mathrm{ET}}^{\mathrm{CF}}
=\widehat d_{\mathrm{SH}}^{\mathrm{CF}}/3.
\]

These are local near-Normal approximations. They are useful as deterministic
anchors, but they are not shortest-interval guarantees for arbitrary skewed,
bounded, heavy-tailed, or boundary-shortest distributions.

## Empirical shortest-window cross-check

The empirical shortest-window pilot sorts the training sample and retains
\(m=\lceil cn\rceil\) adjacent observations with minimum width. Its tilt is
the retained-window mean minus the full training mean, optionally standardized
by the training standard deviation.

This pilot is discontinuous and sample dependent, but it is deliberately
shape robust. Boundary flags identify cases where the selected window touches
the sample support edge and the interior Cornish--Fisher approximation should
be treated cautiously.

## Screening principle

The package provides a compact candidate-grid constructor that includes:

- zero tilt;
- Cornish--Fisher equal-tailed tilt;
- one-half Cornish--Fisher shortest tilt;
- Cornish--Fisher shortest tilt;
- empirical shortest-window tilt;
- optional user-supplied candidates;
- a small expansion around the anchor range.

If an empirical feasible range is available, the grid can be clipped to that
range while retaining unclipped anchors in metadata.

Every candidate in the grid is a separate fixed estimand. Later screening must
fit each candidate separately and select by held-out width subject to held-out
coverage. If no candidate satisfies the coverage constraint, the selection
routine returns a declared failure state.

## Validation evidence

The integration includes deterministic checks for:

- the known \(K_{0.90}\) constant;
- zero tilt for symmetric samples;
- sign reversal under reflection;
- positive affine equivariance of standardized tilts;
- the one-third equal-tailed Cornish--Fisher relation;
- empirical shortest-window accounting;
- fail-closed behavior for invalid inputs;
- Normal coincidence of shortest, equal-tailed, and ordinary-RQR population
  windows;
- the Exponential lower-boundary shortest interval;
- mirrored Beta sign reversal;
- agreement with an independently computed Python population-oracle table.

The validation script writes full Monte Carlo outputs only to an explicit
output directory. Such outputs belong under ignored local roots unless a later
audit promotes compact summaries with hashes.

## Claim boundary

Safe claims:

- Cornish--Fisher supplies a cheap first-order skewness-based fixed-tilt
  initializer near Normality.
- The empirical shortest-window pilot is a useful shape-robust diagnostic
  anchor.
- Candidate tilts should be screened as fixed targets using held-out
  validation.

Unsafe claims:

- Cornish--Fisher always recovers the shortest interval.
- The tilt is a posterior draw or in-chain parameter.
- Ordinary learned-scale RQR applies unchanged at nonzero tilt.
- Nonzero-tilt RQR-DLM, RQR-DESN, or RHS-NS is implemented by this pass.
