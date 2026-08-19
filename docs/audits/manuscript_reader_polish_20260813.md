# Manuscript Reader-Centric Polish Audit

Date: 2026-08-13

Files audited and edited:

- `main.tex`
- `rqr-gibbs-supplement.tex`
- `STYLE_PROFILE.md`

## Diagnosis

The manuscript was already technically careful about the central scope
distinctions: MPI/MTI targets are loss-defined interval functionals, MTI
endpoint draws are not posterior predictive responses, and tolerance confidence
is not posterior credibility. The main readability problem was density rather
than incorrectness. Several paragraphs introduced too many distinctions before
the reader had a map of the statistical problem.

The highest-impact polish targets were:

1. Front-load the fixed-content ambiguity before the algorithm.
2. Use MPI and MTI as target names, not as unexplained acronyms.
3. Make the abstract follow problem, limitation, construction, computation,
   evidence, and qualified scope.
4. Separate tolerance actions, full-distribution Bayesian content uncertainty,
   and fixed-target generalized-posterior endpoint summaries.
5. Give each main section a purpose sentence before equations or theorems.
6. Make the supplement a recovery document for proofs and computational
   contracts rather than a second narrative paper.
7. Keep final empirical claims out of the article until the confirmatory
   validation run is complete.

## Implemented Polish

The main title was revised to:

`Mean-Tilted Intervals: A Generalized-Bayes Approach to Fixed-Content and Tolerance Intervals`

This title is shorter, names the statistical target class, and avoids implying
that the paper is only a regression-computation article.

The main abstract was rebuilt around five moves:

1. Fixed-content interval ambiguity.
2. MPI and MTI target identification.
3. Gibbs and ECM generalized-Bayes computation.
4. Tolerance-action separation.
5. Bayesian content-probability layer and scope caveat.

The introduction was rewritten to make the narrative sequence explicit:

`content ambiguity -> residual-product loss -> MPI target -> MTI family ->
scan-calibrated tolerance action -> Bayesian content uncertainty -> Gibbs/ECM
computation -> dynamic roots -> contributions -> limitations -> roadmap`

The main section headings and openings were polished to emphasize their role:

- MPI population target identification.
- MTI placement family and recovery tilts.
- scan-calibrated tolerance actions.
- finite-sample balance and static uncertainty scope.
- static generalized-posterior computation.
- dynamic root-state computation.
- computational validation and evaluation scope.
- discussion and next steps.

The evaluation section was reframed into three evidence tiers:

1. Deterministic population calculations.
2. Computational fixtures and single-data oracle-tilt illustrations.
3. Confirmatory tolerance validation design.

The supplement title and abstract were aligned with the new title. Its opening
section now states that it is a recovery document for definitions, proofs,
computational identities, and claim scope. Abrupt section openings were revised
so each derivation starts by naming the result or computation it supports.

## Scope Guardrails Preserved

The polish intentionally preserves these boundaries:

- The generalized posterior is loss-based, not an ordinary response likelihood.
- MTI endpoint draws are not posterior predictive response draws.
- Posterior content probability is not tolerance confidence.
- The empirical scan action is the formal tolerance action when invoked.
- Direct-DP and Gaussian-DPM calculations belong to the response-distribution
  Bayesian layer, not to the MTI generalized posterior.
- ECM returns a fixed-target mode summary, not posterior samples.
- Nonzero tilt remains restricted to validated fixed-rate Gaussian/ridge and
  frozen-template paths.
- Final empirical superiority claims remain deferred until the confirmatory
  validation run is complete.

## Remaining Manuscript Step After Validation

After the confirmatory validation run finishes, the evaluation section should be
updated with final tables and figures for:

- empirical content success;
- width and oracle-width ratios;
- fail-closed rates;
- posterior-constraint binding rates;
- runtime;
- method rankings by DGP, content level, and sample size.

Those results should be interpreted by data-generating design rather than as
universal method dominance.
