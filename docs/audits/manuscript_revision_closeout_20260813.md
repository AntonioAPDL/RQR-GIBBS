# Manuscript Revision Closeout, 2026-08-13

## Repository Provenance

| Item | Value |
|---|---|
| Starting branch | `feature/bayesian-uq-authoritative-report6-20260812` |
| Starting SHA | `a92a0f9f22f3f941880dfb7095736d940dcfd225` |
| Initial working tree | `?? academic_style_wickle.txt` |
| Ending SHA | A commit is created after this closeout file is staged; the exact SHA is reported in the final response and by `git rev-parse HEAD`. |
| Final tracked changes before commit | `main.tex`, `rqr-gibbs-supplement.tex`, `application/scripts/73_validate_mpi_mti_naming_migration.R`, generated figure PDFs/manifests from bounded build targets, and five new audit files. |
| Final untracked inputs | `academic_style_wickle.txt` was present initially; `academic_style_wickle.md` was created as a byte-identical local style-authority path and intentionally left untracked as an input authority. |

No heavy simulations, active-run edits, external repository mutations, merge,
rebase, force push, or destructive git cleanup were performed.

## Architecture

Three architectures were audited:

| Architecture | Decision |
|---|---|
| Unified methodological article | Rejected because it gives tolerance and full-distribution Bayesian modules the same narrative weight as better-established MPI/MTI target theory. |
| Core MPI/MTI article with secondary material downstream | Selected because the current proofs and implementation evidence are strongest for fixed-content target identification and fixed-target generalized-Bayes computation. |
| Broad interval-inference framework article | Rejected for now because exact scan recursion, selected-action asymptotics, posterior-action transfer, and regression tolerance theory remain open. |

Major restructuring:

| File | Change |
|---|---|
| `main.tex` | Moved `Scan-Calibrated Tolerance Actions` after static and dynamic generalized-posterior computation. Added `tab:inferential-map` to distinguish population target, generalized posterior, reported summary, tolerance calibration, and ordinary full-distribution Bayes. Rebuilt the introduction contribution paragraph around four supported contributions. Updated the roadmap to match the new section order. |
| `rqr-gibbs-supplement.tex` | Moved `Full-Distribution Bayesian Shortest-Interval Uncertainty` after computation. Added five visible parts, part-orientation paragraphs, and a central status ledger. |
| `application/scripts/73_validate_mpi_mti_naming_migration.R` | Updated stale required section-title patterns to match the current MPI/MTI manuscript titles. |

The title was audited and retained:
`Mean-Tilted Intervals: Fixed-Content Targets, Generalized Bayes, and Tolerance Actions`.
It names the main object, the inferential paradigm, and the downstream action
layer without conflating generalized Bayes with ordinary response-distribution
Bayes.

## Claim Audit

Claims strengthened through clearer support:

| Claim class | Revision |
|---|---|
| MPI/MTI target theory | Made the fixed-content nonuniqueness-to-target arc the main article spine. |
| Generalized-Bayes computation | Kept the fixed-target posterior before augmentation, algorithms, and specializations. |
| ECM | Stated as fixed-target mode computation, not likelihood EM, VB, sampling, or tolerance certification. |
| Dynamic computation | Preserved exactness only for fixed-joint root-blocked FFBS settings. |

Claims narrowed:

| Claim class | Boundaries retained |
|---|---|
| Tolerance actions | External scan/split calibration is distinct from generalized-posterior credibility. Exact closed-window recursion and action-matched proof tasks remain open. |
| Full-distribution Bayes | Direct-DP/DPM model \(F\) directly; they do not validate MPI/MTI root draws as response draws. |
| Evidence | Deterministic population calculations, bounded fixtures, and single-data oracle-tilt diagnostics are not presented as broad repeated-sample performance. |
| Component-scale dynamics | Current exact scope remains MPI shared component scales; tilted interweaving requires separate proof and validation. |

Claims removed or demoted:

| Area | Action |
|---|---|
| Broad framework positioning | Not used as the main article claim. |
| Tolerance exactness | Kept as proposed action plus open theorem/numerical-certification tasks. |
| Data-driven tilt selection | Retained only as a future/external validation layer. |

## Audit Files Created

| File | Purpose |
|---|---|
| `docs/audits/manuscript_deep_revision_audit_20260813.md` | Scientific contract, inferential map, architecture decision, title audit, terminology matrix, duplication map, style-compliance matrix, and conflict log. |
| `docs/audits/manuscript_claim_evidence_matrix_20260813.csv` | Machine-readable major-claim classification with required evidence, available evidence, failure criteria, verb strength, and revision action. |
| `docs/audits/main_supplement_support_map_20260813.md` | Main theorem/proposition/algorithm/table/figure/scope claims mapped to supplement and implementation support. |
| `docs/audits/manuscript_paragraph_reverse_outline_20260813.csv` | Paragraph-level reverse outline for 206 retained manuscript paragraphs. |

## Validation

| Command or check | Exit status | Relevant warnings | Resolution |
|---|---:|---|---|
| `git diff --check` | 0 | None. | Whitespace clean. |
| `make test-manuscript-language` | 0 | None. | Bayesian/generalized-posterior language validator passed. |
| `make test-mpi-mti-naming` | 0 after validator update | Initial failure came from stale required strings in the validator. | Updated required patterns to `Mean-Preserving Interval Loss`, `Mean-Tilted Interval Family`, and `Scan-Calibrated Tolerance Actions`. |
| `make test-theory-figures` | 0 | None. | Deterministic MPI/MTI theory figure oracle checks passed. |
| `make test-theory-tables` | 0 | None. | Mean-tilt CF table checks passed. |
| `make pdf` | 0 | Final log has no undefined references/citations, overfull boxes, missing files, or rerun warnings. It retains an existing underfull table-cell warning at `main.tex` line 938 for `Fixed or MPI`. | Accepted as layout warning only. |
| `make supplement` | 0 | First pass had expected rerun/undefined-reference warnings for the new status table; final log is clean for unresolved refs/citations, overfull boxes, missing files, and rerun warnings. | Accepted after automatic reruns. |
| `make smoke` | 0 | Optional package gaps are acceptable until relevant targets are used. | Bounded environment preflight passed. |
| Focused dangerous-term search | 0 classified hits | Hits for `posterior predictive`, `response likelihood`, `valid`, `guarantee`, and legacy `RQR` are guarded, attributed, or in audit/validator contexts. | No manuscript claim required removal. |

Build-log scan found no final unresolved references, undefined citations,
multiply defined labels, overfull boxes, missing files, LaTeX errors, or natbib
warnings. The only final formatting issue is the known underfull hbox in a
compact main-text table cell.

## Visual PDF Inspection

Rendered pages were checked from both compiled PDFs:

| PDF | Pages inspected | Result |
|---|---|---|
| `main.pdf` | 1, 3, 14, 22, 27, 29, 32 | Title/abstract, inferential map, computation page, tolerance page, evidence figure/caption, discussion, and references are readable; no clipped table, empty page, orphan heading, or unreadable caption observed. |
| `rqr-gibbs-supplement.pdf` | 1, 3, 4, 23, 31, 36 | Supplement title, support map, status ledger, dynamic computation page, oracle-tilt diagnostics table, and references are readable; part headings and status ledger render correctly. |

## Generated Artifact Note

`make pdf` and `make supplement` rerun the repository's tracked figure
generation targets. The deterministic PNG hashes in the theory provenance are
unchanged, but the tracked oracle-tilt PDF figure hashes and provenance commit
fields changed because the publication figure generator rewrote PDF outputs.
No heavy evidence was generated, and no ignored LaTeX logs were added.

## Remaining Gaps

| Gap class | Remaining item |
|---|---|
| Mathematical | Exact scan recursion, action-matched closed-window finite-sample tolerance proof, posterior-to-action transfer, selected-action width regret, regression-family tolerance theory, and data-driven tilt selection theory remain open. |
| Implementation | Nonzero-tilt component-scale interweaving, learned-rate nonzero tilt, broader shrinkage-prior propriety, VB/CAVI, and adaptive dynamic discounting need separate target/propriety/validation work. |
| Empirical evidence | The manuscript still has deterministic population figures, bounded fixtures, and single-data diagnostics; confirmatory repeated-sample validation must be analyzed before performance claims are promoted. |
| Application | No real-data application claims were added. Any application UQ must keep ordinary response-distribution Bayes separate from MPI/MTI generalized posterior summaries. |
| Publication positioning | A future paper split should be reconsidered after tolerance/action and full-distribution Bayesian validation is complete. |

## Closeout Conclusion

The revision implements the selected core-MPI/MTI architecture. The main article
now carries a clearer statistical argument from fixed-content ambiguity to
target identification, generalized-Bayes computation, supported dynamic/static
specializations, tolerance/full-distribution consequences, evidence, and scope.
The supplement now mirrors that order and provides a status ledger so theorem,
algorithm, validation-fixture, experimental, planned, and open-proof claims are
not conflated.
