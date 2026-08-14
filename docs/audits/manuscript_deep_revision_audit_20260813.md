# Manuscript Deep Revision Audit, 2026-08-13

## Repository Provenance

Required preflight was run before substantive manuscript editing.

| Item | Value |
|---|---|
| Repository root | `/data/muscat_data/jaguir26/RQR-GIBBS` |
| Starting branch | `feature/bayesian-uq-authoritative-report6-20260812` |
| Starting SHA | `a92a0f9f22f3f941880dfb7095736d940dcfd225` |
| Initial working tree | `?? academic_style_wickle.txt` |
| Last five commits at start | `a92a0f9 Polish MTI manuscript narrative`; `79a002c Adopt MPI MTI naming for validation launch`; `7ac5115 Wire corrected MT-RQR Gibbs ECM validation launch`; `feebb75 Report partial Bayesian UQ wave progress`; `0de6672 Keep Bayesian UQ wave scheduler detached` |
| Required style authority | `academic_style_wickle.md` was required by the prompt; the checkout contained complete `academic_style_wickle.txt` only. I created a byte-identical local `academic_style_wickle.md` so the required path existed before prose editing. |
| Style manual line count | `academic_style_wickle.md`: 2,678 lines; `academic_style_wickle.txt`: 2,678 lines; `STYLE_PROFILE.md`: 2,143 lines. |
| Major style headings checked | `STYLE_PROFILE.md` headings 1--22, including architecture, abstract/introduction, mathematical exposition, computation, simulation, discussion, anti-AI-prose, and revision workflow; `academic_style_wickle.md` appendices A--F, including claim-audit ledger, corpus inventory, quantitative validation, qualitative evidence matrix, phrase bank, and references. |

No destructive git command, merge, rebase, force push, external-repository
mutation, active-run edit, or heavy simulation launch was used for this task.

## Scientific Contract

| Object | Contract sentence |
|---|---|
| Primary statistical problem | A fixed probability content \(c\) defines many contiguous intervals, so the paper studies how to identify, compute uncertainty for, and evaluate a particular placement within that class. |
| Primary population target | The mean-preserving interval (MPI) is the regular unrestricted contiguous content-\(c\) interval whose retained mean equals the population mean. |
| MPI loss | The residual-product MPI loss supplies score equations whose regular population minimizer is the MPI target, rather than separate lower and upper quantile targets. |
| MTI tilt | The mean-tilted interval (MTI) replaces retained-mean equality with a fixed retained-mean offset \(\delta\), indexing admissible content-\(c\) windows through a target coordinate. |
| Generalized posterior | The generalized posterior updates a prior by the exponential empirical MPI/MTI loss at fixed content, tilt, and learning rate, placing uncertainty on roots or endpoint functions. |
| Reported interval action | The reported generalized-posterior interval summary is an ordered endpoint functional or mode summary after root ordering, while the formal tolerance action is a separately defined order-statistic window. |
| Tolerance calibration | Tolerance confidence is an external repeated-sampling content guarantee attached to a scan or split action, not a consequence of generalized-posterior credible mass. |
| Ordinary full-distribution Bayes | Direct-DP and Gaussian-DPM procedures model the response distribution \(F\) and extract interval functionals from posterior draws; they are not reinterpretations of MPI/MTI root draws. |
| Evidence for principal claims | Population claims require proofs; computational claims require update derivations and reference tests; tolerance claims require action-matched calibration; numerical illustrations require reproducible provenance. |
| Main scope limitation | Exact current evidence supports MPI/MTI target theory and fixed-target computation more strongly than selected-action tolerance validity, regression tolerance theory, data-driven tilt selection, or broad repeated-sample performance. |

## Conditional and Inferential Map

The observations are response values, possibly with covariates or time/state
features. The modeled objects are two exchangeable root functions whose
pointwise ordered values define lower and upper endpoint summaries. The
population criterion is the fixed-content MPI or fixed-tilt MTI loss: it
identifies an interval placement by retained-mean balance, not by treating the
two roots as ordinary lower and upper response likelihood parameters. The
generalized posterior represents loss-based uncertainty about the root
parameters or trajectories under a specified prior, learning rate, content, and
tilt. Computation proceeds through pseudo-AL augmentation, GIG latent scales,
Gaussian conditional root updates, ECM mode updates, or root-blocked dynamic
FFBS conditionals. A reported endpoint interval is then chosen by a declared
posterior summary or optimizer. Tolerance validity enters only after a separate
scan or split rule defines an empirical action and an external content--confidence
criterion. Direct-DP and Gaussian-DPM analyses instead posit response
distribution models for \(F\), then compute fixed-interval or shortest-interval
posterior content summaries under those ordinary Bayesian models.

| Verbal component | Main article location | Supplement support |
|---|---|---|
| Observed response/covariate or time rows | Static and dynamic computation sections | Static root regression and dynamic root-state sections |
| Endpoint/root functions | `sec:posterior`, root ordering paragraphs | Pseudo-AL and static regression support |
| Population fixed-content class | `sec:interval-functionals` | `sec:supp-interval-functionals` |
| MPI criterion and target | `sec:mpi-loss` | `sec:supp-population-target` |
| MTI placement criterion | `sec:mti-loss` | `sec:supp-mean-tilt` |
| Generalized-posterior update | `sec:posterior` | `sec:supp-pseudo-al` |
| Gibbs and ECM computation | `sec:posterior` | `sec:supp-static-regression` |
| Dynamic root-state computation | `sec:dynamic` | `sec:supp-dynamic`, `sec:supp-discounts` |
| Tolerance action and scan calibration | `sec:tcsp` | Supplement support map and status ledger |
| Direct-DP and DPM response models | `sec:tcsp` | `sec:supp-bayes-uq` |
| Evidence and reproducibility | `sec:evaluation` | `sec:supp-oracle-tilt-diagnostics`, `sec:supp-reproducibility` |

The revised main introduction now contains a single inferential map
(`tab:inferential-map`) so target, posterior, action, calibration, and
full-distribution Bayes cannot substitute for one another.

## Architecture Decision

### Architecture A: Unified Methodological Article

The primary reader question would be how one article unifies target
identification, generalized-Bayes computation, tolerance decisions, and
full-distribution Bayesian UQ. This has breadth but high abstract complexity.
Theorem support is strong for MPI/MTI target theory and fixed-target
computation, weaker for selected tolerance actions. It risks overclaiming by
placing scan calibration before the core computation is established.

### Architecture B: Core MPI/MTI Article With Secondary Material Demoted

The primary reader question is why the residual-product loss targets a
mean-preserving fixed-content interval and how fixed-tilt generalized Bayes
computes uncertainty for that target. The central contribution is the
problem-to-target-to-computation spine. Tolerance and ordinary full-distribution
Bayes are retained as downstream decision/UQ layers because their evidence
status differs. This architecture matches the current proof ledgers and
validation records best.

### Architecture C: Broad Interval-Inference Framework Article

The primary reader question would compare population functionals,
loss-generalized Bayes, calibrated actions, and full-distribution Bayes as
equal framework layers. This would require stronger repeated-sample evidence,
action-matched tolerance theory, and more complete posterior-action transfer
than the repository currently contains. It also risks fragmenting the article.

### Selected Architecture

Architecture B was selected. The current proof/evidence hierarchy supports a
core article on fixed-content interval targets, MPI/MTI theory, and
fixed-target generalized-Bayes computation. The scan-calibrated tolerance layer
and full-distribution Bayesian UQ remain important, but they are placed after
the fixed-target theory and computation so their open proof obligations are
visible and do not weaken the central argument. A future paper split is
plausible if tolerance/action calibration and full-distribution Bayesian UQ grow
into a complete comparative study, but this task keeps one coherent article.

## Title Audit

| Candidate | Assessment |
|---|---|
| `Mean-Tilted Intervals: A Generalized-Bayes Approach to Fixed-Content and Tolerance Intervals` | Selected. It names the principal object, the inferential paradigm, and both the fixed-content target layer and tolerance interval layer without saying ordinary Bayesian computation. |
| `Mean-Preserving and Mean-Tilted Intervals for Loss-Based Generalized Bayes` | Accurate but less informative about tolerance actions and too focused on the update rather than the interval target. |
| `Fixed-Content Interval Targets with Generalized-Bayesian Root Regression` | Clean computational emphasis but hides the MTI placement idea. |
| `Mean-Tilted Interval Functionals and Generalized-Bayesian Computation` | Attractive but risks under-reporting the tolerance-action material retained in the article. |

The selected title was retained.

## Claim-Evidence Matrix

The machine-readable matrix is
`docs/audits/manuscript_claim_evidence_matrix_20260813.csv`. Claims were
downgraded or kept conditional where evidence is weaker than theorem-level
prose. The most consequential downgrades are tolerance scan exactness,
posterior-to-action transfer, regression tolerance guarantees, data-driven tilt
selection, adaptive dynamic discounting, and broad repeated-sample performance.

## Terminology Matrix

| Term | Definition | Permitted contexts | Prohibited or misleading contexts | Preferred first-use wording | Abbrev. / legacy policy |
|---|---|---|---|---|---|
| RQR | Pouplin et al.'s Relaxed Quantile Regression residual-product criterion. | Attribution, frozen method names, RQR-W/RQR-O labels, compatibility APIs. | Generic name for all current MPI/MTI targets. | "Pouplin et al. introduced ... under the name Relaxed Quantile Regression." | Preserve where attribution or frozen labels require it. |
| Relaxed quantile regression | Original method lineage for product-residual fitting. | Literature context and source attribution. | Replacement for current MPI/MTI target language. | Spell out at first citation. | RQR allowed only with context. |
| MPI | Mean-preserving interval; regular content-\(c\) target with retained mean equal to the population mean. | Population target, zero-tilt computation, MPI-only scale/dynamic variants. | Ordinary response likelihood, prediction interval, tolerance certificate. | "mean-preserving interval (MPI)." | Canonical. |
| Mean-preserving interval | Same as MPI. | First-use definition and target statements. | Method family name for nonzero tilt. | Use full phrase at first use. | Canonical. |
| MTI | Mean-tilted interval; content-\(c\) target with fixed retained-mean offset \(\delta\). | Fixed nonzero tilt theory/computation, tolerance plug-in target. | Random response parameter, learned target without validation. | "mean-tilted interval (MTI) family." | Canonical. |
| Mean-tilted interval | Same as MTI. | First-use definition and section titles. | Ordinary Bayesian model name. | Use full phrase at first use. | Canonical. |
| Fixed-content interval | Contiguous interval with population probability content \(c\). | Target class and population geometry. | Claiming unique placement without a rule. | "fixed-content interval class." | No abbreviation. |
| Shortest-content interval | Minimum-width contiguous interval among content-\(c\) windows. | Population geometry, recovery tilt, scan action. | Universal tilt or disconnected HD regions without qualification. | "shortest-contiguous interval." | SH may appear in figure labels. |
| TCSP | Historical tolerance/content scan path acronym. | Legacy file names and migration records if unavoidable. | Current manuscript method name. | Prefer "scan-calibrated tolerance action." | Legacy only. |
| Generalized posterior | Prior updated by exponential loss. | MPI/MTI uncertainty over roots/parameters. | Ordinary Bayesian posterior unless qualified. | "loss-based generalized posterior." | Canonical. |
| Gibbs posterior | Synonym for loss-based generalized posterior in generalized-Bayes literature. | Theoretical general-Bayes context. | MCMC sampler identity without target statement. | Use when citing generalized-Bayes lineage. | Allowed. |
| Working likelihood | Computational expression proportional to exponential loss. | Pseudo-AL algebra only if clearly qualified. | Response sampling model for \(Y\). | Prefer "working factor" or "augmentation." | Avoid when possible. |
| Pseudo-AL representation | Normal--exponential augmentation of the exponential product-residual loss. | Derivations, full conditionals, ECM moments. | Data-generating asymmetric-Laplace likelihood. | "pseudo-asymmetric-Laplace computational representation." | Pseudo-AL allowed. |
| Response likelihood | Ordinary sampling model for data. | Direct-DP/DPM and literature contrast. | MPI/MTI generalized posterior. | "ordinary response-distribution model." | Avoid for MPI/MTI. |
| Root | One of two exchangeable raw readout functions or parameter blocks. | Computation and parameterization. | Intrinsic lower/upper coefficient label. | "two exchangeable root functions." | Canonical. |
| Endpoint | Pointwise ordered lower or upper value derived from roots. | Reported intervals and endpoint summaries. | Raw coefficient block without chart audit. | "pointwise ordered endpoint." | Canonical. |
| Lower/upper coefficient | Coefficient chart after global ordering audit. | Declared global chart only. | Default label for exchangeable root blocks. | "coefficient-level lower/upper summaries are reported only when..." | Use sparingly. |
| Ordered fitted endpoint | Lower/upper endpoint after pointwise ordering of fitted root values. | Figures, summaries, evaluation. | Proof statements requiring globally labelable coefficients unless assumptions hold. | Use "pointwise ordered." | Canonical. |
| Learning rate | Fixed scalar multiplying empirical loss in the generalized posterior. | Generalized-Bayes uncertainty scale. | Tolerance factor, content, or posterior confidence level. | "fixed learning rate \(\omega_{\mathrm R}\)." | Canonical. |
| Scale | Reciprocal loss scale or dynamic evolution scale depending on symbol. | Must be symbol-specific. | Generic uncertainty term. | Define \(\sigma_{\mathrm R}\), \(\kappa\), or \(q_j\) locally. | No global abbreviation. |
| Tilt | Retained-mean offset \(\delta\). | MTI target specification. | Random response parameter or coverage adjustment. | "fixed retained-mean tilt." | Canonical. |
| Content | Population fraction \(c\) or fitted content \(q=k/n\). | Interval target and tolerance action. | Coverage/confidence/credibility. | Distinguish \(c\) and \(q\). | Canonical. |
| Coverage | Empirical or population content event. | Evaluation metrics and tolerance law. | Posterior credible mass. | "empirical content success" where repeated-sample. | No abbreviation. |
| Confidence | Repeated-sampling tolerance confidence \(1-\alpha\). | Tolerance action. | Generalized-posterior credible probability. | "tolerance confidence." | Canonical. |
| Calibration | External procedure aligning action with a target criterion. | Scan, split, learning-rate, or sandwich only with criterion. | Vague synonym for good diagnostics. | Name the calibration target. | Canonical. |
| Certification | Verified action-level content--confidence guarantee. | Tolerance actions with proof/numerical certificate. | Posterior summaries without action proof. | "external certificate." | Use narrowly. |
| Prediction interval | Interval for future response values. | Literature contrast only. | MPI/MTI endpoint credible bands. | Avoid unless truly predictive. | Not current target. |
| Tolerance interval | Random set with content/confidence statement. | Scan or split actions. | Generalized-posterior endpoint band. | "tolerance action" when decision rule matters. | Canonical. |
| Credible interval | Posterior probability interval under a posterior model. | Generalized-posterior endpoint summaries or direct-DP/DPM posterior summaries with qualification. | Tolerance confidence or response predictive interval. | Qualify the posterior type. | Allowed with qualifier. |

## Main--Supplement Support Map

The full support map is
`docs/audits/main_supplement_support_map_20260813.md`. It records every
main-text theorem, major equation family, algorithmic block, tolerance action,
figure, empirical claim, and scope statement against supplemental derivations
or implementation artifacts.

## Paragraph-Level Reverse Outline

The paragraph-level reverse outline is maintained as
`docs/audits/manuscript_paragraph_reverse_outline_20260813.csv`. It records
file, section, paragraph id, governing question, one-sentence answer, evidence
or mechanism, scope condition, transition, and recommended action. It was used
to identify the two high-impact structural actions taken here: moving the
scan-calibrated tolerance layer after fixed-target computation in `main.tex`
and moving full-distribution Bayesian UQ after computation in the supplement.

## Duplication Map

| Repeated idea | Authoritative full statement | Compact reference elsewhere |
|---|---|---|
| Generalized Bayes versus ordinary Bayes | Main introduction inferential map and `sec:posterior` opening. | "loss-based generalized posterior" or "ordinary response-distribution model." |
| Pseudo-AL versus response likelihood | Static generalized-posterior section and supplement pseudo-AL opening. | "computational representation of the exponential loss." |
| Root draws versus predictive responses | Inferential map and figure captions. | "endpoint functionals, not posterior predictive response draws." |
| Generalized-posterior uncertainty versus tolerance confidence | Main inferential map and scan-calibrated tolerance section. | "external tolerance calibration." |
| Fixed versus learned rate | Scope paragraph and learned-scale subsection. | "fixed-rate" or "MPI-only learned inverse-loss scale." |
| Exact versus experimental computation | Supplement central status ledger. | Status label and scope table. |
| Exchangeable roots versus ordered endpoints | Static generalized-posterior target subsection. | "pointwise ordered endpoints." |
| Validation fixtures versus comparative evidence | Evaluation section opening. | "bounded fixture" or "single-data illustration." |
| Full-distribution Bayes versus MPI/MTI | Main tolerance/full-distribution paragraph and supplement Bayes UQ section. | "separate response-distribution model." |
| Current evidence limitations | Discussion limitations paragraph and supplement status ledger. | "requires additional theory and validation." |

## Style-Compliance Matrix

| Guideline | Applicable? | Current failure or success | Planned implementation | Verification |
|---|---:|---|---|---|
| Problem-first motivation | Yes | Earlier structure introduced tolerance before core computation. | Put fixed-content nonuniqueness, MPI, MTI, and generalized Bayes before tolerance. | Main section order and roadmap. |
| Target before computation | Yes | Mostly successful, but scan action interrupted the path. | Move scan action after static/dynamic computation. | `main.tex` section order. |
| Decomposition before formalization | Yes | Several distinctions were present but dispersed. | Add one inferential map. | `tab:inferential-map`. |
| Calibrated claims | Yes | Tolerance and posterior UQ caveats repeated unevenly. | Centralize strong statement; use compact references later. | Main intro map and `tab:tcsp-theory-scope`. |
| Fair contrast with prior literature | Yes | RQR attribution retained correctly. | Keep RQR attribution and introduce MPI/MTI as reframing. | Introduction first-use paragraphs. |
| Avoid response-likelihood confusion | Yes | Language mostly guarded; audit retained dangerous-search workflow. | Preserve "not a response likelihood" statements where needed. | Language validator and focused search. |
| Avoid AI-style generic openings | Yes | Abstract begins with a concrete interval ambiguity. | Retain concrete opening. | Abstract. |
| Avoid inflated novelty | Yes | The article used contribution verbs rather than "novel framework." | Keep "shown", "derived", "formulated" with scope. | Contribution paragraph. |
| Equation contract | Yes | Consequential equations generally introduced. | Retain interpretation after target/action equations. | Manual review plus compile. |
| Tables perform inferential work | Yes | Main lacked one definitive inferential map. | Add map; leave software inventories outside main. | `tab:inferential-map`; supplement status ledger. |
| Figures answer scientific questions | Yes | Existing figures were scoped as deterministic or single-data. | Retain captions with explicit scope. | Figure captions and manifests. |
| Simulation claims only when evidence exists | Yes | Evaluation correctly stated confirmatory protocol as future gate. | Keep "designed", not "completed performance." | Evaluation section. |
| Discussion separates limits | Yes | Discussion already explicit. | Preserve and align with architecture. | Discussion section. |
| Wikle-style rhetorical function without imitation | Yes | The revision uses problem, decomposition, interpretation, and limitations, not copied wording. | Apply function-level guidance only. | Audit and prose review. |

## Conflict Log

| Conflict | Resolution |
|---|---|
| Prompt required `academic_style_wickle.md`, but checkout had complete untracked `academic_style_wickle.txt`. | Created byte-identical local `academic_style_wickle.md` before substantive prose editing and recorded line counts. The style file remains an input authority and was not edited for content. |
| Naming validator expected the stale phrase `Scan Calibration and Shortest MTI Tolerance Actions`. | Updated required pattern to the current canonical section title `Scan-Calibrated Tolerance Actions`. |
| Main article placed scan calibration before core generalized-posterior construction. | Moved `Scan-Calibrated Tolerance Actions` after static and dynamic computation. |
| Supplement placed full-distribution Bayesian UQ before core population proofs/computation had fully played out. | Moved full-distribution Bayesian UQ after computation and added visible Part IV. |
| Tolerance theory ledger leaves exact scan recursion and selected-action transfer open. | Main and supplement state these as open proof tasks rather than established theorems. |
| Direct-DP/DPM are ordinary response models, while MPI/MTI are generalized-Bayes loss models. | Added inferential map and preserved separate section language. |

## Audit Conclusion

The optimal current path is not a broad framework article. The strongest
publishable spine is a core MPI/MTI article: fixed-content nonuniqueness,
MPI/MTI target identification, finite-sample and static uncertainty scope,
fixed-target generalized-Bayes computation, supported static/dynamic
specializations, then tolerance/full-distribution consequences and limits.
The implemented edits make that spine explicit while preserving the downstream
tolerance and Bayesian-UQ material needed for the ongoing validation program.
