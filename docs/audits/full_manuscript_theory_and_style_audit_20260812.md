# Full Manuscript Theory and Style Audit

Date: 2026-08-12  
Audited source: `abd48a03bf2b219ce56978aa558e4deb6af701ff`  
Manuscript type: statistical-methods and theory paper with computational extensions

## Scope and source hierarchy

This audit covers `main.tex`, `rqr-gibbs-supplement.tex`, their tables and
figures, the current TCSP proof ledger, and the implementation/evidence notes
added after the preceding editorial revision. The local files `report`,
`report2.md`, `report3.md`, and `report4.md` were read as external review
inputs and remain untracked and unchanged. Mathematical claims are controlled
by the manuscript derivations and the proof ledger; implementation tests and
simulation output are not treated as theorem proofs.

## Main inferential target

The central target is a contiguous interval functional of fixed probability
content. Ordinary RQR selects the mean-preserving member under the stated
population conditions. MT-RQR fixes a retained-mean displacement to index
other members, including distribution-specific equal-tailed and
shortest-contiguous intervals. Proper priors are updated by this loss through
a generalized posterior. The pseudo-AL representation is a computational
augmentation of the product residual and is not a response likelihood.

The scan-calibrated tolerance construction is a separate proposed frequentist
layer. Its formal action is the empirical closed order-statistic window. The
generalized posterior is conditional on a fixed content and tilt and does not
inherit tolerance validity without an action-transfer or direct-calibration
result.

## Audit findings

### Structure and narrative

- The title correctly foregrounds MT-RQR rather than promoting an unfinished
  tolerance theorem.
- The abstract has the right problem--method--theory--computation sequence,
  but its final scope statements can be made more compact.
- The introduction accurately separates interval placement, tolerance
  confidence, generalized-Bayes learning rate, and response prediction. It
  repeats several caveats and can state the contribution and scope more
  efficiently.
- Sections on ordinary RQR, MT-RQR, empirical balance, static uncertainty,
  static computation, DESN, and DLM follow a coherent target-to-computation
  progression.
- The TCSP theorem-gate table uses repository identifiers and development
  statuses in the main article. Those controls are important, but they should
  be translated into publication-facing mathematical scope.
- Two large tables are deferred until after the bibliography in the baseline
  PDF. This is a material formatting defect, not a cosmetic preference.

### Theory and claim control

- The ordinary-RQR content and retained-mean equations, quantile-window
  identification, profiled MT-RQR target, fractional empirical balance, and
  fixed-dimensional static covariance contrast are presented with explicit
  assumptions and recoverable supplement derivations.
- The shortest-path derivative formulas have the corrected signs and are
  described as formal calculations under regular interior conditions.
- The simultaneous Massart--DKW scan-calibration implementation is described
  conservatively. Neither Monte Carlo calibration nor the fallback is called
  an exact scan recursion.
- The action-matched finite-sample scan theorem, selected-action asymptotics,
  shortest-tilt plug-in theory, posterior-action transfer, and regression
  tolerance extensions remain unproved or unaudited. They must remain outside
  the title, abstract claims, and result statements.
- The `q=1` empirical range action must remain distinct from an MT-RQR
  posterior fit, which requires content strictly between zero and one.
- The phrase claiming that a first-moment consequence is unreported in the
  original RQR article is unnecessary for the argument and creates an
  avoidable literature-exhaustiveness burden.

### Bayesian and computational language

- The manuscript consistently identifies a generalized posterior rather than
  an ordinary response posterior.
- Interval-root draws are correctly described as endpoint-function draws,
  not posterior-predictive responses.
- The static and dynamic Gibbs algorithms state their invariant targets and
  correctly separate exact root-specific conditional updates from joint
  Gaussian sampling, which is unavailable because of the quartic term.
- The nonzero-tilt Gaussian/ridge scope, ordinary-only RHS-NS scope,
  ordinary-only learned-rate scope, and ordinary-only component-scale scope
  are mostly clear. Repeated implementation-status prose can be shortened
  without weakening these boundaries.
- Terms such as "audit design," "audit policy," and "placeholder latent
  scales" are repository-facing. They should be replaced by statistical or
  numerical descriptions in the article and supplement.

### Evidence and interpretation

- Population figures, deterministic CF diagnostics, and the single-data
  oracle-tilt illustrations are clearly distinguished from repeated-sample
  validation.
- The six illustrative fit cells report diagnostics without relabeling a
  warning as a strict pass. Their claims are appropriately limited to
  computational and broad-recovery checks.
- Planned matched studies are not reported as completed evidence.
- The evaluation section can be renamed and tightened so the reader can
  distinguish current illustrations from future comparative validation more
  quickly.

### Supplement

- The supplement is mathematically substantive and generally self-contained.
- Its support map should point to manuscript sections rather than an internal
  repository proof ledger.
- Several paragraphs read like software closeout notes. They should retain the
  exact scope while using publication-facing language.
- The criticism of the original RQR finite-sample argument should identify the
  technical limitation directly and avoid adversarial wording.
- The final reproducibility section is the appropriate place for repository
  provenance and evidence-boundary statements.

## Controlled revision plan

1. Rewrite the abstract and contribution/scope paragraphs for a tighter
   statistical narrative without changing the title or promoted claims.
2. Replace the TCSP development-status table with a compact mathematical-scope
   table that distinguishes established results, conditional derivations, and
   open requirements without internal theorem IDs.
3. Fix float placement so all tables appear in the sections that introduce
   them and no floats follow the bibliography.
4. Standardize section titles, publication-facing terminology, equation
   introductions, captions, and algorithm language across the article.
5. Align the supplement abstract, support map, terminology, and scope notes
   with the revised main article while preserving all derivations.
6. Extend manuscript-language tests to reject internal proof-gate identifiers
   and the most visible repository-facing terms in the public manuscript.
7. Rebuild and inspect every page of both PDFs, then run theory, table,
   language, package-focused, and arXiv source validations.

## Explicit non-actions

- Do not promote any proof-ledger item solely because software tests pass.
- Do not introduce a response likelihood or response-simulation contract.
- Do not add simulation claims from the compact TCSP pilot.
- Do not edit or track the four local review-report files.
- Do not mutate the protected exdqlm or Q-DESN repositories.

## Completed revision and validation

The controlled revision was completed without changing the mathematical
target or promoting an open result. In particular:

- the abstract, introduction, contribution statement, evaluation section, and
  discussion now use a consistent problem--target--computation--scope
  structure;
- ordinary RQR, MT-RQR, the generalized posterior, static regression, frozen
  DESN readouts, and dynamic root states retain distinct roles;
- the TCSP discussion now separates the empirical action, numerical scan
  calibration, fixed-target generalized posterior, and unresolved transfer
  results in publication-facing language;
- the main model-family and TCSP tables appear in their defining sections,
  and no figure or table follows the bibliography;
- the supplement notation and support-map tables precede the derivations they
  organize; and
- language validation now rejects internal theorem identifiers and selected
  repository-facing expressions in the public manuscript.

The following checks passed on the revised sources:

| Check | Result |
|---|---|
| Main article build | PASS; 29 pages |
| Supplement build | PASS; 29 pages |
| LaTeX warning scan | PASS; no overfull boxes, undefined references, or unresolved labels |
| Page-by-page visual review | PASS; all 58 rendered pages inspected |
| Manuscript Bayesian-language guard | PASS |
| Theory-figure oracle and reproducibility checks | PASS |
| Cornish--Fisher table checks | PASS |
| TCSP focused test suite, including claim-boundary wiring | PASS |
| Repository smoke/preflight | PASS |
| arXiv source construction | PASS |
| Git whitespace check | PASS |

Generated model-illustration binaries were regenerated during validation and
were byte-different only because of PDF metadata. Because neither their source
nor scientific content changed in this revision, the tracked publication
artifacts and their existing hash manifest were retained rather than recording
a spurious figure revision.
