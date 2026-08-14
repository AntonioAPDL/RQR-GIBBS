# Proposition, Theorem, Section 5, and Contrast-Phrase Audit, 2026-08-14

## Scope

This audit reviews the reader-facing presentation of Proposition 1, Theorem 4,
main Section 5, and repeated contrast phrasing of the form "this is X, not Y"
in `main.tex` and `rqr-gibbs-supplement.tex`. It follows the repository style
profile: preserve mathematical claims, foreground the statistical object, state
major caveats once in full, and replace repetitive caveat sentences with
statistically specific prose.

## Main Findings

| Area | Current strength | Main issue | Revision goal |
|---|---|---|---|
| Proposition 1 | Correctly identifies the MPI score target and retained-mean balance. | The statement appears before the reader has a compact map of what the two equations mean. The assumptions are dense, and the post-proposition explanation carries several consequences at once. | Make the result read as "content plus retained mean" first, then give covariance/tail-balance consequences as interpretation. |
| Theorem 4 | Correctly gives profiled identification and the admissible tilt range. | The theorem is doing too much: radius existence, envelope derivative, admissible tilt range, global minimizer, boundary behavior, and root-label invariance. The reader can lose the main message that MTI indexes fixed-content windows by retained mean. | Split the visible theorem into a shorter main theorem plus explanatory prose; push technical radius/envelope details into the supplement cross-reference. |
| Section 5 | Correctly separates finite-sample score balance from static asymptotic uncertainty. | The section joins two different purposes in one block: empirical fractional balance and sandwich calibration. The bridge sentence is too abrupt, and the caveats repeat the "not coverage" pattern. | Recast Section 5 as a bridge: training-score balance first, static uncertainty second, with one clear scope paragraph. |
| Contrast phrasing | Protects against important inferential conflations. | The article and supplement overuse sentence-final caveats such as "not a response likelihood," "not posterior predictive," and "not a tolerance certificate." This creates a repetitive audit-log rhythm. | Keep every essential boundary, but state it through positive definitions, scope clauses, and compact cross-references. |

## Proposition 1 Revision Plan

1. Add a one-sentence lead before the proposition:
   "The endpoint scores have two consequences: they set the interval content and make the retained distribution preserve the conditional mean."
2. Retain the mathematical statement, but rename the result to either
   `Mean-Preserving Score Characterization` or
   `MPI Population Score Characterization`.
3. Move the covariance and tail-balance interpretation into a short paragraph
   with one sentence per consequence:
   retained mean, tail first-moment balance, convex-order support in the supplement.
4. Avoid phrasing such as "not a binary coverage penalty" nearby; replace with
   positive wording: "The loss weights both membership and distance from each
   endpoint, so misses and covered observations contribute through the product
   residual."

## Theorem 4 Revision Plan

1. Rename the theorem from `Profiled identification of the interior target` to
   `Global Identification of the MTI Target`.
2. Shorten the main statement to the reader-facing result:
   for each admissible retained-mean tilt, the expected MTI loss has a unique
   finite ordered-root minimizer, and its probability-window index solves
   `M_c(u_delta)=mu+delta`.
3. Move or compress the radius-existence and envelope-derivative machinery into
   the paragraph before the theorem:
   "The supplement proves this by profiling over the unique radius at each
   midpoint and differentiating the profiled risk."
4. Keep the admissible range in the theorem, but explain boundary cases after
   the theorem in two sentences: boundary tilts are one-sided limiting windows;
   outside the range the unrestricted population risk has no finite target.
5. Remove root-label invariance from the theorem body and state it once in the
   computation section or root-label convention.

## Section 5 Revision Plan

1. Open with a purpose sentence:
   "This section separates two finite-sample questions: exact score balance on
   the fitted training sample and large-sample uncertainty for static endpoint
   coefficients."
2. Split the prose into two named paragraphs or subparagraphs:
   `Training-sample balance` and `Static sandwich scope`.
3. Keep the fractional allocation equations but add a short interpretation
   immediately after them.
4. Replace repeated caveats with positive scope:
   "The identity concerns the empirical estimating equations; population
   coverage requires the iid intercept-only DKW argument or additional
   regression theory."
5. For sandwich calibration, state the target first:
   empirical minimizer covariance `Sigma_F` versus generalized-posterior local
   covariance `Sigma_G`; then state when open-faced sandwich adjustment is
   available.

## Contrast-Phrase Revision Plan

Broad contrast-pattern counts are approximately 98 in the main article and 106
in the supplement. Stronger "not a / does not / cannot" patterns account for
about 54 main-text hits and 64 supplement hits. These should not be removed
mechanically; many protect essential claims. The revision should classify each
hit into one of four treatments:

| Treatment | Use when | Example replacement pattern |
|---|---|---|
| Define positively | The sentence currently says what the object is not. | "The augmentation represents the product-residual loss factor" instead of "It is not a response likelihood." |
| Integrate the boundary | The caveat belongs inside the preceding claim. | "Endpoint summaries describe fixed-target root uncertainty under the generalized posterior." |
| Cross-reference once | The same caveat repeats across sections. | "with the inferential scope stated in Section ..." |
| Keep the negative contrast | The risk of misinterpretation is high and local. | Retain a limited number of explicit boundaries for response likelihood, posterior predictive response draws, tolerance confidence, and exact Gibbs scope. |

## Priority Pass Order

1. Main article first: abstract, introduction, Proposition 1 block, Theorem 4
   block, Section 5, computation-scope paragraphs, tolerance section, evidence
   section, discussion.
2. Supplement second: guide, support map, MPI proof section, MTI proof section,
   static theory, pseudo-AL/ECM computation, DLM computation, Bayesian UQ, and
   reproducibility.
3. Update validators only after prose is stabilized, so they protect the new
   positive-scope wording rather than the old negative contrasts.
4. Rebuild `make pdf` and `make supplement`, then run
   `make test-manuscript-language`, `make test-mpi-mti-naming`, and the focused
   TCSP theory-wiring test.
