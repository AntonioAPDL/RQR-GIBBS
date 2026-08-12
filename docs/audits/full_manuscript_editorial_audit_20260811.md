# Full Manuscript Editorial Audit

Date: 2026-08-11

Manuscript: `main.tex`

Baseline repository commit: `108b136f305a2a833713b38c42459eed55b92af2`

## Purpose and editorial contract

This audit applies `STYLE_PROFILE.md` to the complete main article. The review
covers the title, abstract, every numbered section, theorem introductions and
interpretations, algorithms, tables, figure captions, evidence statements,
limitations, bibliography output, and the rendered page sequence.

The statistical contract is unchanged:

- RQR and MT-RQR define interval functionals through a loss;
- the prior is updated by that loss through generalized Bayes;
- the pseudo-asymmetric-Laplace representation is an augmentation of the
  generalized posterior, not a response likelihood;
- interval-root and state-path draws do not constitute posterior-predictive
  response draws;
- repeated-sampling tolerance confidence, generalized-posterior credibility,
  and learning-rate calibration are distinct;
- empirical and computational claims remain limited to the protocols that
  produced them.

## Principal findings and resolutions

### 1. The headline claim exceeded the proof ledger

The baseline title and abstract presented calibrated minimum-width tolerance
intervals as the article's completed central result. The contemporaneous proof
ledger still classified the action-matched finite-sample scan theorem and the
posterior-action equivalence theorem as blocking, and exact critical-count
recursion was unavailable.

Resolution:

- restored MT-RQR as the article's established central contribution;
- used the title `Mean-Tilted Relaxed Quantile Regression: Fixed-Content
  Interval Functionals and Generalized-Bayes Computation`;
- rewrote the abstract around the identified population functional, fixed-tilt
  family, augmentation, and structured Gibbs computation;
- retained the tolerance construction as an explicitly proposed extension;
- stated the unresolved scan-certification and posterior-action requirements
  directly, without internal development labels.

### 2. The tolerance section contained development-language artifacts

The baseline section used terms such as “branch-level,” “authoritative,”
“proof-gated,” “serialized,” and “current theorem ledger,” together with
source comments marking proof gates. Those terms describe a development
workflow rather than a statistical article.

Resolution:

- renamed the section `A Proposed Scan-Calibrated Tolerance Construction`;
- organized it as target definition, notation, empirical action,
  fixed-target generalized posterior, numerical calibration, shortest-path
  geometry, computation, and limitations;
- replaced repository-status prose with publication-facing statements of what
  is defined, what is implemented, and what remains unproved;
- preserved the separation between the empirical tolerance action and
  posterior endpoint summaries.

### 3. One shortest-path derivative had the wrong sign

The baseline text assigned the same positive derivative to both endpoints
under symmetric content expansion. The lower endpoint must move left and the
upper endpoint right. The corrected differentiable shortest-path identities
are

\[
L_q'=\frac{b_q}{\lambda_q(a_q-b_q)}<0,
\qquad
U_q'=\frac{a_q}{\lambda_q(a_q-b_q)}>0,
\]

where `a_q=f'(L_q)>0`, `b_q=f'(U_q)<0`, and
`f(L_q)=f(U_q)=lambda_q`. The text now also gives the asymmetric left/right
probability shares, the width derivatives, and the retained-mean tilt
derivative under their stated regularity conditions. These calculations guide
continuation; the article does not promote the pending global theorem.

### 4. “Scan” denoted two different operations

The baseline used “scan” for both the uniform interval-scan statistic and a
Gibbs update order. This became ambiguous after the tolerance section was
added.

Resolution: statistical interval scanning retains the word `scan`; MCMC text
now uses `Gibbs sampler`, `Gibbs sweep`, or `update order`.

### 5. The abstract and introduction were implementation-first

The baseline abstract opened with proof status and software state. The
introduction mixed the article's contribution with a long list of enabled and
disabled modes.

Resolution:

- the abstract now follows problem, target, result, computation, evidence, and
  limitation order in 207 words;
- the introduction first explains interval-placement nonuniqueness, then RQR,
  MT-RQR, generalized Bayes, dynamic computation, contributions, scope, and
  organization;
- implementation restrictions are retained only where they determine the
  validity of a claim.

### 6. Repeated and internal wording weakened the statistical narrative

The audit removed duplicated words (`generalized generalized`, `full full`),
generic transitions, unnecessary self-reference, and development-oriented
terms. It also shortened dense passages on support boundaries, empirical
balance, shrinkage adapters, and computational evidence.

## Section-by-section disposition

| Article component | Audit focus | Final disposition |
|---|---|---|
| Title and metadata | Claim proportionality and PDF consistency | MT-RQR title restored in the article, supplement, PDF metadata, and README |
| Abstract | Problem, contribution, computation, evidence, limitation | Rewritten; 207 words; tolerance claim explicitly prospective |
| Introduction | Narrative order, contribution hierarchy, scope | Rewritten and compressed; method identity and evidence boundary clarified |
| Fixed-content functionals | Notation before use, table interpretation | Retained with a clearer opening definition |
| Ordinary-RQR loss | Loss-first pedagogy and check-loss equivalence | Retained; already met the Bayesian and loss-first writing contract |
| Population target | Assumptions, theorem interpretation, restricted-class caveat | Tightened; fixed-endpoint and fitted-endpoint arguments separated |
| Mean-tilted RQR | Target definition, support boundaries, recovery tilts | Tightened; propriety scope made more direct |
| Cornish--Fisher material | Approximation versus target distinction | Retained as an initialization and screening diagnostic only |
| Tolerance construction | Action, calibration, derivatives, proof status | Reorganized and corrected; internal workflow language removed |
| Empirical and static uncertainty | Fractional score identity and covariance distinction | Opening rewritten; inferential limits preserved |
| Static generalized posterior | Root labels, posterior target, family table | Terminology tightened; table recast as computational scope |
| Augmentation and Gibbs sampler | Exact target before computation | Retained; latent augmentation and Gaussian full conditionals remain explicit |
| Regularized regression and DESN | Adapter scope and response interpretation | Tightened; ordinary-only RHS-NS boundary retained |
| Learned inverse-loss scale | Normalization and partial-collapse order | Retained; learned scale remains distinct from response variance |
| Dynamic linear roots | Stacked prior versus quartic joint term | Retained; sequential root-specific FFBS rationale remains explicit |
| Evaluation and illustrations | Evidence strength and diagnostic reporting | Tightened; no simulation-study or predictive claim added |
| Discussion | Synthesis, limitations, future work | Reorganized; tolerance proof obligations now appear explicitly |
| Figures and tables | Placement, self-contained captions, terminology | All captions reviewed; Figure 1 placement adjusted to remove a largely blank page |

## Display, table, and figure checks

- Every displayed quantity is introduced before or immediately after the
  display.
- The meanings of `c`, `alpha`, `q`, `delta`, and `omega_R` are separated in
  the tolerance construction.
- Root labels remain exchangeable; ordered endpoint functionals are not
  conflated with raw MCMC labels.
- Table 1 distinguishes interval placement principles.
- The Cornish--Fisher table states that it is a deterministic population
  diagnostic, not MCMC or tilt-selection evidence.
- The model-family table distinguishes predictor architecture, prior or
  evolution specification, posterior computation, and current scope.
- Illustration captions identify population-oracle endpoints,
  generalized-posterior summaries, credible ribbons, missing-response marks,
  chain counts, and the absence of a response-simulation interpretation.

## Validation results

| Check | Result |
|---|---|
| `git diff --check` | Pass |
| `make smoke` | Pass |
| `make test-manuscript-language` | Pass |
| `make test-theory-figures test-theory-tables` | Pass |
| `make pdf` | Pass; 27 pages |
| `make supplement` | Pass; 29 pages |
| Main and supplement log scan | Pass; no warnings, undefined citations or references, overfull boxes, or underfull boxes |
| Visual review | Pass; every main-article page reviewed through rendered page sheets, with pages 3--6 rechecked after the Figure 1 placement correction |
| arXiv source construction | Pass; ZIP integrity test successful |
| Isolated arXiv source compile | Pass; two `pdflatex` passes from a newly extracted archive |

The generated arXiv-source archive is local-only. Its SHA-256 for this audit
run is
`4a425797d2bcf8ed9976fa87201b115aeea60813674e04d3cc84315690d4f2fe`.

## Remaining scientific work

This editorial pass does not promote unresolved theory. The following remain
separate research tasks:

- an action-matched finite-sample theorem for the canonical closed
  order-statistic window;
- certified critical-count computation, ideally through an exact scan
  recursion or an independently established conservative procedure;
- posterior-action equivalence if a posterior mean, median, or other Bayes
  action is to inherit a tolerance statement;
- regression-family tolerance guarantees;
- nonzero-tilt propriety and validation for random shrinkage or evolution
  scales;
- externally validated data-driven tilt selection and variational inference.

The manuscript now states these boundaries in statistical language and does
not use pending theory as a headline result.
