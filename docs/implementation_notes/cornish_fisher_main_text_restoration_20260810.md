# Cornish--Fisher Main-Text Restoration Plan

## Purpose and boundary

This plan restores the first-order Cornish--Fisher (CF) population diagnostic
to the main article without changing the RQR loss, any generalized posterior,
the exact-oracle illustration fits, or the repeated-DGP validation protocol.
The CF quantities remain fixed approximation anchors for initialization or an
external screening layer. They are not estimated interval targets, sampled
parameters, response-predictive objects, or substitutes for the exact tilts
used in the V5 illustrations and planned repeated-DGP study.

## Audit findings

1. The CF formulas remain in the main article, but commit `f8935dc0` moved the
   diagnostic figure and seven-law population table to the supplement during a
   readability revision.
2. The current arXiv packager excludes the separate supplement. Consequently,
   the public main-source package contains the formulas but not their visual or
   tabular diagnostic evidence.
3. The deterministic generator uses interval content `c=0.80` and an internal
   left-skewed asymmetric-Laplace fixture with `tau_AL=0.65`. The figure
   documentation incorrectly retained an older `tau_AL=0.80` description.
4. The CF formulas and generated population values use the conditional
   retained mean, including division of the truncated first moment by content.
   They are therefore unaffected by the historical V1--V4 illustration-oracle
   normalization defect.
5. The figure intentionally exposes approximation error: for its left-skewed
   fixture the exact standardized ET and SH tilts are about `0.0810` and
   `0.1635`, whereas the CF anchors are about `0.1080` and `0.3239`.
6. The population table demonstrates the correct limited claim: exactness at
   symmetry, high accuracy for a near-Normal Gamma law, and material departures
   for stronger skewness, bounded support, and a support-boundary shortest
   interval. Skewness alone does not determine approximation accuracy.
7. The manuscript bibliography does not cite the foundational Cornish--Fisher
   article.
8. One main-text sentence still described all RQR-DLM computation as
   zero-tilt. The native API now supports fixed nonzero tilt for fixed-rate
   ridge fixed-design/RQR-DESN readouts and for fixed-covariance or frozen-
   template RQR-DLM modes. The revised prose states that bounded support while
   retaining the narrower evidence claim for repeated-sample validation.

## Final design

- Keep Figure 2 CF-free so it continues to describe exact population interval
  geometry only.
- Restore the existing two-panel CF diagnostic immediately after the
  first-order formulas as main-text Figure 3.
- Place the deterministic population comparison table immediately after the
  figure. Group its columns by ET and SH, report exact tilt, CF tilt, and
  absolute standardized gap, and mark the exponential SH boundary case.
- Keep the full near-Normal expansion, regularity qualifications, plug-in
  construction, and implementation diagnostics in the supplement, but remove
  duplicate figure and table floats there.
- Cite Cornish and Fisher (1937) in both derivations.
- Include the main-text figure and generated TeX table in the arXiv source
  contract.
- Enforce placement, uniqueness, deterministic values, reflection, boundary
  notation, and source-package inclusion in automated tests.

## Validation checklist

- [x] Main Figure 2 remains free of CF markers and CF language.
- [x] Main Figure 3 distinguishes exact filled markers from open CF markers.
- [x] The population table reports true-population quantities only.
- [x] The table retains strong-skewness and boundary cases rather than showing
      only favorable approximations.
- [x] The supplement retains derivations but does not duplicate the displays.
- [x] The bibliography contains the foundational CF source.
- [x] The arXiv source list includes both main-text dependencies.
- [x] Theory-figure tests pass.
- [x] Population-table tests pass, including full-value regression checks.
- [x] Main and supplement PDFs compile without undefined references or
      material layout warnings.
- [x] The generated arXiv archive compiles from its own extracted contents and
      contains no supplement, cache, log, or fitted-model artifacts.
- [x] Final diff, repository status, and remote commit are reconciled through
      the scoped implementation commit and post-push verification.

## Acceptance rule

The restoration is complete only when the figure and table appear exactly
once in the main article, no longer appear as floats in the supplement, their
generated values reproduce, the arXiv archive contains every main dependency,
and all prose continues to classify CF as a first-order diagnostic rather than
an exact or automatically selected target.
