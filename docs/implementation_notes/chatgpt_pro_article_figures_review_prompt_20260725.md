# ChatGPT Pro prompt: independent RQR-GIBBS article and figure review

Copy the complete prompt below into a **new ChatGPT Pro chat**. This is an
article-and-figures review. It is intentionally separate from the Pro chat that
is reviewing the RQR-DLM simulation launch.

```text
You are ChatGPT Pro acting as an independent Bayesian-statistics manuscript
reviewer, mathematical auditor, and scientific-figure design reviewer.

Codex is a separate downstream implementation agent. You are not Codex. Do not
describe proposed work as though you have already edited, committed, tested, or
pushed the repository. Your job in this chat is read-only review and preparation
of a compact, implementation-ready review bundle for Codex.

========================================================================
1. PROJECT AND EXACT SOURCE
========================================================================

Repository:

  AntonioAPDL/RQR-GIBBS
  https://github.com/AntonioAPDL/RQR-GIBBS

Exact article source commit to review:

  f7422c9205e9b73f5b47097fc486ae79edfa7bfe

Commit title:

  Develop mean-preserving and mean-tilted RQR theory

This full SHA is authoritative for the article review. Do not silently replace
it with a later main-branch state, an older uploaded file, a similarly named
download, or material from the separate Q-DESN article.

Preferred retrieval route:

  Use the connected GitHub app to open AntonioAPDL/RQR-GIBBS at the exact
  commit above.

Public immutable-archive fallback:

  https://github.com/AntonioAPDL/RQR-GIBBS/archive/f7422c9205e9b73f5b47097fc486ae79edfa7bfe.zip

Equivalent codeload fallback:

  https://codeload.github.com/AntonioAPDL/RQR-GIBBS/zip/f7422c9205e9b73f5b47097fc486ae79edfa7bfe

Before reviewing, confirm that the exact snapshot contains and then read these
files completely:

  AGENTS.md
  STYLE_PROFILE.md
  main.tex
  rqr-gibbs-supplement.tex
  refs.bib
  Makefile
  docs/implementation_notes/mean_tilted_rqr_article_integration_20260725.md

Also inspect, only as needed to verify manuscript statements about what is
implemented:

  application/DESCRIPTION
  application/NAMESPACE
  application/R/rqr_utils.R
  application/R/rqr_numerics.R
  application/R/rqr_dlm_model.R
  application/R/rqr_dlm_fit.R
  application/R/rqr_ffbs.R
  application/src/rqr_ffbs.cpp
  application/tests/testthat/test-rqr-native-model.R
  application/tests/testthat/test-rqr-native-sampler.R
  application/tests/testthat/test-rqr-native-ffbs.R
  docs/implementation_notes/rqr_dlm_native_design_20260722.md
  docs/implementation_notes/rqr_naming_review_20260722.md

The prompt itself may live in a later documentation commit and is not expected
inside the immutable article snapshot.

If neither the GitHub app nor either immutable archive can be read, stop before
performing the review. State:

  RETRIEVAL FAILED — ARTICLE REVIEW NOT PERFORMED

Do not substitute project-library files, cached older snapshots, or prior Pro
reports.

========================================================================
2. PROTECTED SCOPE AND ROLE BOUNDARIES
========================================================================

This review is read-only.

Do not:

  - create a GitHub branch;
  - edit or push RQR-GIBBS;
  - modify the exdqlm repository or any exdqlm branch;
  - modify the Q-DESN article repository;
  - authorize or launch the RQR-DLM simulation;
  - treat prior Codex or Pro reports as proof;
  - represent the pseudo-AL device as a response likelihood;
  - represent interval-root or state-root draws as posterior-predictive
    response draws;
  - present mean-tilted RQR as package functionality unless the exact reviewed
    source demonstrates that functionality;
  - fabricate empirical figures, simulation results, diagnostics, or
    application evidence.

This review is separate from the ongoing simulation-launch audit. Do not issue
a launch GO/NO-GO decision here. You may identify manuscript statements whose
eventual empirical support depends on that separate validation.

The protected reference identities are:

  exdqlm reference branch:
    feature/rqr-desn-readout-20260716
  exdqlm reference commit:
    dffb71ee70b597d6a716ee74be1cbc99731cd453

  Q-DESN article reference branch:
    main
  Q-DESN article reference commit:
    f9f22804eff3871bb5350c8add04b7c9f4d4957b

They are references only. The standalone RQR-GIBBS article and code must remain
self-contained.

========================================================================
3. FIXED SCIENTIFIC INTERPRETATION
========================================================================

Preserve these distinctions throughout the review:

  1. The RQR posterior is a generalized-Bayes loss update over interval roots.
     It is not an ordinary response-likelihood posterior.

  2. The pseudo-asymmetric-Laplace normal-exponential construction is an
     augmentation of the exponentiated residual-product loss kernel. It is not
     a sampling model for the original response.

  3. Root-state draws, ordered endpoints, widths, and midpoints are functions
     of the interval-root target. They are not posterior-predictive response
     draws.

  4. Ordinary RQR is the current implemented target. In mean-tilt notation it
     is delta = 0.

  5. Mean-tilted RQR is currently a proposed theoretical and computational
     extension. The fixed-rate Gaussian information-vector shift is derived in
     the manuscript, but nonzero-tilt package implementation and empirical
     validation are not claimed.

  6. The normalized learned inverse-loss scale for ordinary RQR is an
     analyst-declared hierarchical generalized target. It is not automatically
     a calibrated same-data response parameter.

  7. A fixed mean tilt indexes an interval functional. It should not initially
     be sampled as an ordinary unknown, and the ordinary learned-scale Gamma
     update must not be reused for nonzero tilt without a separately normalized
     proper target.

  8. The current RQR-DLM uses sequential root-specific FFBS blocks. Although
     the two root states can be stacked under a Gaussian prior, the joint
     augmented observation term is quartic in the two root states. One ordinary
     simultaneous Gaussian FFBS draw is therefore unavailable. Conditional on
     either complete root path, the other path is Gaussian and receives an
     exact full-conditional FFBS update in fixed-joint modes.

  9. Fixed W, frozen discount templates, and shared component-scale modes have
     declared target interpretations. Adaptive conditional discounting remains
     a working method rather than an exact Gibbs sampler for a fixed joint
     target.

========================================================================
4. INDEPENDENT MATHEMATICAL AUDIT
========================================================================

Audit every central derivation independently. Do not accept a claim merely
because it appears in both the main paper and supplement.

At minimum, verify:

  A. Ordinary RQR

  - sign of (y-a)(y-b) inside and outside the ordered interval;
  - midpoint-half-width identity
      (y-a)(y-b) = (y-m)^2 - h^2;
  - endpoint and midpoint/half-width first-order conditions;
  - content-c equation at a nondegenerate interior stationary point;
  - retained-first-moment and retained-mean equations;
  - balance of omitted lower- and upper-tail first moments;
  - distinction between the interval midpoint and the response mean;
  - endpoint-atom and coincident-root qualifications;
  - restricted-design score equations and why they do not imply pointwise
    conditional coverage or mean preservation.

  B. Quantile-window characterization

  - representation [Q(u), Q(u+c)];
  - monotonicity of
      M_c(u) = c^{-1} integral_u^{u+c} Q(v) dv;
  - existence and uniqueness assumptions;
  - symmetry corollary;
  - equality of the retained-core and excluded-tail means;
  - convex-order contraction proof and variance consequence;
  - nested interval-path derivatives;
  - zero-content mean anchor;
  - positive-affine equivariance and the corresponding transformations of the
    fixed tilt and generalized-Bayes learning rate;
  - lack of general nonlinear transformation equivariance;
  - quadratic tail growth and the strength of the finite-moment assumptions.

  C. Competing fixed-content interval targets

  - equal-tailed probability allocation;
  - ordinary-RQR first-moment balance;
  - shortest-contiguous width minimization;
  - equal-endpoint-density condition only for a regular interior optimum;
  - difference between a shortest contiguous interval and a potentially
    disconnected highest-density region;
  - why the standard interval score is not target-neutral among these
    functionals.

  D. Mean-tilted RQR

  - sign, units, and root-exchange invariance of
      rho_c{(y-a)(y-b)} - c delta (a+b-2y);
  - unchanged content equation;
  - retained-mean equation mu + delta;
  - admissible population tilt range;
  - one-to-one delta-to-quantile-window map;
  - oracle equal-tailed and shortest-contiguous tilts;
  - derivative of the width profile;
  - boundary and multimodal qualifications;
  - why population representability is not a data-driven tilt estimator;
  - why a scalar tilt generally cannot recover a pointwise conditional target
    for all covariates.

  E. RQR-W

  - coverage correction q = c + 2 lambda;
  - restriction lambda < (1-c)/2;
  - changed retained-mean equation;
  - general width-penalty coverage equation;
  - why RQR-W is a different interval functional and does not generally equal
    the shortest interval.

  F. Generalized posterior and computation

  - pseudo-residual normal-exponential mixture constants;
  - GIG latent-scale conditional;
  - normalized and pure-loss learned-scale distinctions;
  - partially collapsed update order;
  - fixed-design Gaussian precision and canonical vectors;
  - sign and scaling of the mean-tilt information-vector shift;
  - the fixed-rate-only restriction for the first nonzero-tilt implementation;
  - DLM quartic joint observation term and sequential root-specific FFBS logic;
  - component-discount and shared component-scale interpretation.

For any mathematical issue:

  - give the exact source location;
  - show the corrected derivation rather than only asserting that it is wrong;
  - classify it as blocker, major, minor, or suggestion;
  - state whether it affects the ordinary-RQR model, only the proposed
    mean-tilted extension, only exposition, or only a future implementation.

========================================================================
5. LITERATURE AND NOVELTY AUDIT
========================================================================

Use primary or published sources whenever possible. Check the bibliography and
the manuscript's attribution and novelty language against at least:

  - the original RQR article;
  - Bissiri, Holmes, and Walker on general Bayes;
  - Yu and Moyeed on Bayesian quantile regression;
  - Kozumi and Kobayashi on the Gibbs augmentation;
  - Brehmer and Gneiting on equal-tailed, shortest, and modal intervals;
  - Gneiting and Raftery on proper scoring rules;
  - the cited dynamic quantile linear model literature;
  - the cited DESN uncertainty-quantification literature.

Check:

  - titles, years, journals, DOIs, and author names in refs.bib;
  - whether every citation supports the nearby claim;
  - whether the mean-preserving, convex-order, nested-path, and mean-tilt
    statements are properly described as new derivations or proposals rather
    than established results from the original RQR paper;
  - whether any stronger prior result exists that should change the novelty
    wording;
  - whether “shortest,” “HDI,” “HDR,” “modal,” and “equal-tailed” are used
    with the required qualifications.

Do not rely on search-result snippets. Provide a direct source link or DOI for
every recommended citation addition or correction. Respect quotation limits
and summarize sources in your own words.

========================================================================
6. FULL MANUSCRIPT AND STYLE REVIEW
========================================================================

Read STYLE_PROFILE.md before making editorial recommendations.

Review main.tex and rqr-gibbs-supplement.tex as one article package. Assess:

  - scientific focus and contribution hierarchy;
  - abstract accuracy and density;
  - whether the mean-preserving result should be the conceptual center;
  - whether mean tilt is given the right prominence before implementation;
  - whether any theorem belongs only in the supplement;
  - redundancy between main paper and supplement;
  - notation consistency;
  - proposition assumptions and proof placement;
  - section order and transitions;
  - distinction among target, algorithm, validation, and empirical evidence;
  - unsupported generality or novelty claims;
  - whether the migrated preliminary RQR-DESN table should remain, move, or be
    withheld until the matched study is complete;
  - title and terminology, including whether “relaxed quantile regression”
    should remain the formal inherited name while a clearer descriptive phrase
    is used in the title or prose;
  - likely reviewer objections and the smallest defensible corrections.

Prefer compact, technical Bayesian-statistics prose. Do not rewrite merely for
stylistic preference. Preserve correct authorial choices when no substantive
gain results.

Separate all recommendations into:

  - required before the manuscript is mathematically stable;
  - recommended before a full article draft is circulated;
  - optional future refinement;
  - explicitly defer until simulation evidence exists.

========================================================================
7. FIGURE PROGRAM
========================================================================

Design a coherent publication figure program without inventing empirical
results. Classify every proposed figure as:

  - PRODUCE NOW FROM DETERMINISTIC/ORACLE THEORY;
  - PRODUCE AFTER FIXED-TILT MCMC VALIDATION;
  - PRODUCE AFTER THE MATCHED RQR-DLM SIMULATION;
  - SUPPLEMENT ONLY;
  - DO NOT PRODUCE.

Critically assess at least these candidates:

  1. Three balance principles under skewness:
     equal omitted probabilities, RQR omitted first moments, and shortest
     width at common content.

  2. Symmetry versus skewness:
     coincidence for a symmetric unimodal law and separation for a skewed law.

  3. Mean-tilt recovery map:
     M_c(u)-mu, W_c(u) or W_c(delta), and selected intervals.

  4. Loss geometry:
     residual-product sign, half-width score, and midpoint/tilt score.

  5. Generalized-Bayes computational schematic:
     pseudo-residual augmentation and Gaussian/GIG Gibbs cycle. This must not
     be drawn as a response-generating DAG.

  6. RQR-DLM blocked-state schematic:
     stacked Gaussian prior, quartic joint observation term, and two sequential
     root-specific FFBS updates.

  7. Future finite-sample target recovery:
     clearly marked as unavailable until validated output exists.

For each retained figure specify:

  - scientific question;
  - exact panels;
  - distributions, content levels, and standardized scales;
  - equations and annotations;
  - color- and grayscale-safe encoding;
  - main-paper or supplement placement;
  - complete draft caption;
  - what claim it can and cannot support;
  - exact deterministic input data required;
  - output dimensions and vector/raster format;
  - code module that should generate it.

Keep the main-paper portfolio small. Recommend the minimum figure set that
materially improves comprehension.

========================================================================
8. CANDIDATE REPRODUCIBLE FIGURE CODE
========================================================================

Prepare clean candidate R code for Codex to audit and run. It must generate
only figures that can be supported by deterministic population/oracle
calculations at the reviewed source state. Do not generate simulation-result
figures.

Provide:

  proposed_code/generate_rqr_theory_figures.R

and

  proposed_code/test_rqr_theory_figure_oracles.R

The generator should:

  - run without network access;
  - accept an explicit output directory;
  - never write into the repository by default;
  - use deterministic calculations and record any numerical tolerances;
  - use a minimal, explicitly declared dependency set;
  - compute quantile-window means, ordinary-RQR roots, oracle fixed tilts,
    equal-tailed intervals, and shortest contiguous intervals;
  - distinguish interior and support-boundary shortest solutions;
  - include at least one symmetric and one skewed distribution;
  - standardize response, tilt, and width where cross-distribution comparison
    is made;
  - export compact source-data CSV files for every panel;
  - export a machine-readable figure manifest with figure ID, source commit,
    configuration, dependencies, data hashes, and output hashes;
  - create publication-sized vector output and a review preview;
  - avoid hidden global state and use small named functions;
  - fail on nonfinite integrals, invalid quantile windows, or missed numerical
    identities;
  - label all oracle material as population theory rather than fitted evidence.

The test script should independently check, within declared tolerances:

  - exact or numerical content c;
  - M_c(u_RQR) = mu;
  - M_c(u_delta) = mu + delta;
  - symmetry coincidence;
  - shortest-width optimality over the contiguous family;
  - the width-profile derivative where regular;
  - positive-affine equivariance;
  - expected sign reversal for a mirrored skewed distribution.

Do not assume candidate code has been executed if your environment lacks R.
State exactly which code was run, which was only statically reviewed, and what
Codex must test before integration.

The repository currently ignores generated PDFs and heavy outputs. Recommend a
two-stage artifact policy:

  1. generate and validate figures under an ignored local output root;
  2. let Codex decide whether a small final publication asset, a TeX-native
     representation, or only source code/data should be promoted.

Do not silently weaken the repository's no-heavy-artifact rule.

========================================================================
9. IMPLEMENTATION-READY CORRECTIONS
========================================================================

Prepare a unified candidate patch against exactly:

  f7422c9205e9b73f5b47097fc486ae79edfa7bfe

Path:

  proposed_patch/rqr_article_revision.patch

The patch may modify only:

  main.tex
  rqr-gibbs-supplement.tex
  refs.bib

Include a change only when the audit establishes a mathematical, attribution,
scope, reproducibility, or material editorial improvement. Do not apply a
global rename, add unvalidated results, or insert figure calls whose assets do
not yet exist. If no patch is justified, provide a valid empty patch plus an
explanation in the audit.

Codex will independently inspect and selectively implement the patch. The
presence of the patch is not approval to apply it blindly.

========================================================================
10. REQUIRED REVIEW BUNDLE
========================================================================

Return one downloadable ZIP named exactly:

  chatgpt_pro_rqr_article_review_bundle_20260725.zip

It must contain exactly these compact deliverables:

  chatgpt_pro_rqr_article_audit_20260725.md
  chatgpt_pro_rqr_article_findings_20260725.csv
  chatgpt_pro_rqr_figure_and_code_plan_20260725.md
  proposed_patch/rqr_article_revision.patch
  proposed_code/generate_rqr_theory_figures.R
  proposed_code/test_rqr_theory_figure_oracles.R
  chatgpt_pro_rqr_article_artifact_hashes_20260725.csv

The audit must include:

  - retrieval and source-integrity record;
  - executive verdict;
  - mathematical finding-by-finding audit;
  - literature and bibliography audit;
  - manuscript structure and style audit;
  - claims that are safe now;
  - claims that require implementation or simulation;
  - exact recommended next actions for Codex;
  - explicit separation from the simulation-launch review.

The findings CSV must include:

  finding_id
  severity
  confidence
  source_file
  source_anchor
  area
  finding
  evidence
  recommendation
  affects_ordinary_rqr
  affects_mean_tilt
  blocks_manuscript_freeze
  proposed_patch_included

The internal artifact manifest must record every other included file's:

  relative_path
  byte_count
  sha256

The manifest cannot recursively hash itself; include one row or metadata field
stating that self-hashing is excluded by construction. Do not include heavy
fits, generated article PDFs, runtime libraries, caches, raw simulation
outputs, or TeX build logs.

After building the ZIP:

  - verify ZIP integrity;
  - verify that only the seven allowed deliverables are present;
  - recompute every internal SHA-256 from the final ZIP contents;
  - report the completed outer ZIP SHA-256 separately.

Do not attempt a GitHub write operation. Codex will verify the returned ZIP,
audit all proposed changes and code, implement only justified corrections,
run the repository's validation gates, commit, and push.

========================================================================
11. FINAL RESPONSE FORMAT
========================================================================

Your final chat response should contain only:

  1. a download link for
     chatgpt_pro_rqr_article_review_bundle_20260725.zip
  2. the complete SHA-256 of that ZIP

Do not paste the full audit into the chat response; it belongs inside the ZIP.
```
