# Proposition, Theorem, Section 5, and Contrast-Phrase Revision Plan, 2026-08-14

## Source State

This plan was prepared on `main` at commit
`96b0883b26068984887fd0654595e06970b3f3a9`. The working tree was clean before
the plan file was added. The root checkout at
`/data/muscat_data/jaguir26/RQR-GIBBS` remains on an older feature branch with
untracked local style files; this plan uses the clean linked `main` worktree at
`/data/muscat_data/jaguir26/.rqr_gibbs_worktrees/oracle_tilt_v3_process_isolation_20260801`.

## Audit Inputs

The audit read the repository style profile, the current main manuscript, the
supplementary proof sections, and the validation wiring. The relevant current
locations are:

| Object | Main article location | Supplement support | Validation wiring |
|---|---|---|---|
| MPI score target | `main.tex`, Section 3, Proposition 1 | `rqr-gibbs-supplement.tex`, Sections S2-S3 | `application/scripts/63_validate_manuscript_bayesian_language.R`; `application/scripts/73_validate_mpi_mti_naming_migration.R` |
| MTI global identification | `main.tex`, Section 4, Theorem 4 | `rqr-gibbs-supplement.tex`, Lemma S5 and Theorem S6 | same manuscript-language and naming validators |
| Empirical balance and static uncertainty | `main.tex`, Section 5 | `rqr-gibbs-supplement.tex`, Section S7 | same validators plus TCSP theory-wiring tests |
| Contrast phrasing | full `main.tex` and `rqr-gibbs-supplement.tex` | style profile guidance on caveats | current validators require several old negative-boundary phrases |

The broad contrast-pattern scan found 98 hits in `main.tex` and 106 in
`rqr-gibbs-supplement.tex` for patterns such as `not`, `does not`, `cannot`,
`rather than`, and `instead of`. Stronger local patterns are concentrated in
the abstract, introduction, Proposition 1 explanation, Theorem 4 boundary
paragraph, Section 5, computation scope paragraphs, tolerance section, evidence
section, and supplement guide/proof/computation sections.

## Diagnosis

| Area | Current diagnosis | Why it matters | Best next action |
|---|---|---|---|
| Proposition 1 | The mathematics is correct and central, but the assumption block and post-proposition consequences arrive before a compact reader map. | A reader should first understand that the endpoint scores impose two balances: content and retained mean. The covariance, tail-balance, and convex-order consequences are secondary interpretation. | Keep the proposition in the main article, rename it to a score-characterization title, add a lead sentence, leave the equations unchanged, and compress the explanatory paragraph into retained mean, tail first moments, and supplement convex-order support. |
| Theorem 4 | The theorem correctly gives global MTI identification, but it also carries radius existence, envelope derivative, boundary behavior, and label invariance in one visible statement. | The main result should read as "admissible retained-mean tilts index unique fixed-content windows." The proof mechanics already have a clean home in the supplement. | Shorten the main theorem to global identification and the index equation. Move radius/envelope details into pre-theorem prose with a direct supplement cross-reference. Keep boundary cases after the theorem. Preserve labels and mathematical scope. |
| Section 5 | The section combines two valid but distinct ideas: exact fractional training-score balance and fixed-dimensional iid sandwich uncertainty. | The abrupt transition makes score identities look closer to coverage guarantees than intended and makes the sandwich result feel like a caveat block. | Keep a single main section but add two run-in paragraph labels: `Training-sample balance.` and `Static endpoint uncertainty.` This improves orientation without adding a heavier subsection structure. |
| Contrast phrasing | The repeated "this is X, not Y" rhythm is doing protective work but reads mechanically. | The manuscript must preserve inferential boundaries while avoiding an audit-log style. | Classify each contrast as essential, redundant, or movable. Keep explicit negative contrasts only for high-risk conflations; rewrite the rest as positive scope statements or cross-references. |
| Validators | Current validators require phrases such as `Posterior credibility is not tolerance confidence` and `not a sampling model for \(y_i\)`. | If prose is improved but validators still enforce the old wording, the repository will pull future revisions back toward the problem. | Update validators after the rewrite to protect concepts rather than exact old sentence forms. Add a soft contrast-phrase audit target if useful. |

## Recommended Revision Strategy

### Stage 0: Provenance and Scope Guard

Work only from the clean `main` worktree. Record the starting commit, confirm
`main` matches `origin/main`, and leave unrelated files in the older feature
checkout untouched. Do not edit generated figures or run outputs unless the PDF
build regenerates tracked artifacts.

### Stage 1: Proposition 1 Reader Pass

Revise the Proposition 1 block in `main.tex` without changing its label
`prop:population-characterization` or the mathematical identities. The target
presentation should be:

1. Replace the current lead with a direct map: the endpoint scores set content
   and preserve the retained conditional mean.
2. Rename the displayed result to `Mean-Preserving Score Characterization`.
3. Keep assumptions in the proposition, but make the opening less dense by
   grouping them as regularity conditions for an interior unrestricted
   pointwise target.
4. Rewrite the explanation after the proposition as three short consequences:
   retained mean, omitted-tail first-moment balance, and convex-order support in
   the supplement.
5. Avoid adding new claims about symmetry, independence, or exact finite-sample
   coverage.

This is the optimal scope because Proposition 1 is not just proof machinery; it
is the first place the reader learns what MPI estimates.

### Stage 2: Theorem 4 Main/Supplement Split

Revise the Theorem 4 block in `main.tex` while preserving
`prop:profiled-mean-tilt`. The main theorem should state only:

1. support and regularity assumptions at the level needed for global
   identification;
2. the admissible tilt range
   `M_c(0)-mu < delta < M_c(1-c)-mu`;
3. existence and uniqueness of the finite ordered-root minimizer;
4. the quantile-window equation `M_c(u_delta)=mu+delta`;
5. the MPI special case at `delta=0`.

The pre-theorem prose should say that the supplement proves the result by
profiling over the unique radius at each midpoint and differentiating the
profiled risk. The post-theorem prose should handle boundary tilts and
escaping-root behavior in two compact sentences. Root-label invariance should
move out of the theorem body and remain in the computation/root-label
convention discussion.

This is the optimal scope because the supplement already contains
Lemma S5 (`lem:supp-profile-radius`) and Theorem S6 (`prop:supp-profiled-risk`),
so the main article can focus on the statistical conclusion without weakening
the proof chain.

### Stage 3: Section 5 Structural Pass

Revise Section 5 as a bridge between population theory and computation. Keep
the section title unless the rewrite shows a better title is needed. Do not add
numbered subsections in the main article; use run-in paragraph labels so the
section remains compact:

1. `Training-sample balance.` Introduce the fractional allocation theorem as an
   empirical estimating-equation identity. Keep the equations and immediately
   interpret `q_i` as fractional membership at roots.
2. Replace "not exact future coverage" with positive scope language: population
   coverage enters through the iid intercept-only DKW bound or additional
   regression theory.
3. `Static endpoint uncertainty.` State the target distinction first:
   `Sigma_F` is the repeated-sample covariance of the empirical minimizer and
   `Sigma_G` is the local covariance induced by a fixed-rate generalized
   posterior.
4. Keep the ordered-chart exclusions, but integrate them into one scope
   sentence instead of a long list of negations.
5. End with the role of open-faced sandwich adjustment as first-order static
   coefficient and differentiable endpoint-function calibration.

This is the optimal scope because Section 5 is short and transitional. New
subsections would make it look like a standalone theory section, while run-in
labels give the reader enough structure.

### Stage 4: Contrast-Phrase Rewrite

Apply a manuscript-wide phrase pass after the three focal blocks are stable.
Use this taxonomy:

| Treatment | Keep or revise? | Rule |
|---|---|---|
| Response likelihood boundary | Keep explicit at first definition; later cross-reference. | Preserve the generalized-Bayes versus ordinary response-likelihood distinction. |
| Posterior predictive response draws | Keep explicit where interval-root draws could be confused with response draws. | Use positive wording elsewhere: "root draws induce endpoint-function summaries." |
| Tolerance confidence | Keep explicit in the abstract/tolerance section. | Later phrasing can say "confidence is supplied by the scan calibration." |
| Exact Gibbs scope | Keep explicit for adaptive discounts, learned scales, and nonzero tilt restrictions. | Prefer "the exact sampler covers..." instead of "this is not exact..." where possible. |
| Redundant sentence-final caveats | Revise. | Integrate the boundary into the claim or replace with a cross-reference. |

The goal is not to remove every `not`. The goal is to reduce repeated
sentence-final contrasts and keep only the boundaries that prevent real
statistical misinterpretation.

### Stage 5: Supplement Alignment

After the main article is clean, revise the supplement to match the same
reader-facing contract:

1. The supplement guide should define generalized-posterior scope positively
   and retain only one explicit response-likelihood boundary.
2. The MPI proof section should mirror the new Proposition 1 language and keep
   independence/symmetry cautions only where needed.
3. The MTI proof section should retain the full radius and envelope derivative
   details because these are exactly what the main theorem will cite.
4. The empirical balance and static uncertainty section should use the same two
   concepts as Section 5, with the full theorem/proof left intact.
5. Computation sections should replace repeated negative contrasts with
   positive exactness statements and scoped restrictions.

### Stage 6: Validation Wiring

Update validation after the prose is stable:

1. Modify `application/scripts/63_validate_manuscript_bayesian_language.R` so it
   requires conceptual markers rather than exact old negative sentences.
2. Keep `application/scripts/73_validate_mpi_mti_naming_migration.R` focused on
   MPI/MTI naming, not old phrasing.
3. Update `application/tests/testthat/test-rqr-tcsp-theory-wiring.R` if the
   abstract or tolerance wording changes from exact fixed strings.
4. Optionally add a soft audit script for contrast phrases. It should report
   counts and local hits; it should not fail merely because a technical `not`
   remains.

## Acceptance Criteria

The revision is ready only if all of the following hold:

1. Proposition 1, Theorem 4, and Section 5 are easier to read while preserving
   every mathematical claim and all stable labels.
2. The main article clearly states that MTI generalizes MPI, that MPI is the
   zero-tilt member, and that admissible tilts index fixed-content windows by
   retained mean.
3. The supplement still contains the proof machinery needed by the shorter main
   theorem.
4. Essential inferential boundaries remain clear: generalized Bayes is
   loss-based, endpoint-root draws are not response-predictive draws, tolerance
   confidence comes from scan calibration, and exact Gibbs claims have bounded
   scope.
5. The phrase pass reduces repetitive local contrast wording without weakening
   those boundaries.
6. Validators protect the new wording and do not require the old repetitive
   caveat style.

## Reproducibility and Build Gates

After implementation, run:

```sh
git diff --check
make test-manuscript-language
make test-mpi-mti-naming
Rscript application/scripts/66_run_testthat_file_strict.R application/tests/testthat/test-rqr-tcsp-theory-wiring.R
make pdf
make supplement
make smoke
```

Then inspect the generated LaTeX logs for undefined references, citation
warnings, rerun warnings, fatal errors, and overfull boxes. The final commit
should include manuscript source changes, validation updates, and this plan or a
closeout note. It should not include local run artifacts from ignored
simulation directories.

## Recommendation

Proceed with this staged rewrite before doing additional simulation manuscript
integration. The manuscript is already structurally sound after the table and
heading cleanup; the highest-return polish is now local clarity around the
core theoretical objects and the removal of repetitive contrast phrasing. This
approach preserves the established theory, avoids unnecessary reorganization,
and gives validators a concrete role in keeping the article smooth after the
revision.
