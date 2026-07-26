# Static predictor hierarchy and Figure 2 label revision

Date: 2026-07-26

## Scope

This pass implements two reader-facing decisions:

1. place the ordinary-RQR and shortest-interval labels on the requested sides
   of their Figure 2 markers; and
2. present structured predictors in the order static regularized regression,
   frozen-feature DESN, and dynamic linear root states.

The pass began from `0ba7518b0e063d9effe3bc29bd5359cd0e6cc97c`.
It changes manuscript, supplement, bibliography, figure-source, figure-test,
and documentation files only. No tracked `application/` file, simulation
configuration, fitted object, exdqlm checkout, or Q-DESN checkout is changed.

## Figure 2

In the window-to-tilt panel, the blue label is now exactly `RQR 0.00` and is
centered below the ordinary-RQR marker. In the width-near-target-tilts panel,
the orange label is `SH` and is centered above the shortest-interval marker.
Both labels inherit their marker colors. Named placement maps and a common
offset make the layout deterministic, and the figure-oracle test checks the
literal label, positions, colors, and offset.

## Predictor architecture

The article now separates the interval target from its predictor
architecture:

- **Static interval-root regression** is the foundational computational
  model. Proper Gaussian/ridge priors give the fixed-rate RQR/MT-RQR
  conditional blocks.
- **Ordinary regularized regression** uses an external regularized-horseshoe
  adapter based on the Nishimura–Suchard augmentation (RHS-NS). The two roots
  retain separate prior states; each state is updated after its corresponding
  coefficient draw, and an exchangeable label swap moves the complete
  coefficient/prior-state blocks.
- **Frozen-feature DESN** replaces the static design matrix by a fully
  recorded deterministic reservoir-feature matrix. It reuses the static scan
  and is not presented as a new loss or a separate Gibbs algorithm.
- **Dynamic linear roots** follow as the first architecture with time-varying
  root states. Their Gaussian Markov prior and quartic joint augmented
  observation term require alternating root-specific FFBS blocks rather than
  one simultaneous Gaussian state draw.

This order is used in the abstract, contributions, roadmap, status table,
main-text sections, supplement sections, evaluation plan, discussion, README,
and implementation-status ledger.

This reader-directed order supersedes the earlier presentation preference in
finding A01 of
`docs/audits/rqr_model_family_architecture_review_disposition_20260726.csv`,
which placed the DLM before the DESN. It does not reverse that audit's
mathematical, implementation-status, or evidence-scope dispositions.

## Statistical and software boundaries

The revision preserves the following distinctions:

- the generalized posterior is a loss update, not an ordinary response
  likelihood;
- root and interval summaries are not posterior-predictive response draws;
- zero-tilt ridge, ordinary RHS-NS, ordinary frozen-DESN, and ordinary DLM
  paths are implemented;
- fixed-rate nonzero tilt is derived only for proper Gaussian/ridge static
  blocks;
- nonzero-tilt RHS-NS, DESN, and DLM software remains proposed rather than
  implemented;
- the first RHS-NS design column is a documented declared-intercept input
  requirement; runtime enforcement is deferred to a future isolated API pass;
  and
- learned inverse-loss scale remains an ordinary-RQR convention. Its
  integrability argument covers a proper complete root/prior-state law but
  does not authorize copying the Gamma update to the potentially negative
  mean-tilted loss.

The bibliography record for Nishimura and Suchard's scalable shrinkage
sampling paper was also corrected to *Bayesian Analysis* 18(2), 367--390,
DOI `10.1214/22-BA1308`.

## Validation

The source revision passed:

- `git diff --check`;
- the no-`application/` diff guard;
- the deterministic analytic, numerical, rendering, label-placement, and
  provenance checks in `make test-theory-figures`;
- `make pdf`;
- `make supplement`;
- citation and cross-reference resolution;
- zero TeX warnings, overfull/underfull boxes, undefined references, and
  errors in both final logs; and
- independent read-only audits of Figure 2, the static/DESN/DLM hierarchy,
  the RHS-NS conditional contract, and learned-scale integrability.

The source commit and provenance-bound publication-asset commit are recorded
in the final publication closeout below.

```text
source commit: pending source freeze
publication commit: pending asset publication
```
