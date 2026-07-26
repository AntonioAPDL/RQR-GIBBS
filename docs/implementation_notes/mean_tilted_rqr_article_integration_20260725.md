# Mean-tilted RQR article integration

Date: 2026-07-25

## Purpose

This note records the controlled integration of the mean-preserving
interpretation of ordinary relaxed quantile regression (RQR) and the proposed
fixed mean-tilted extension into the standalone RQR-GIBBS manuscript. It
separates results that can be stated from the current mathematical derivation
from implementation and empirical claims that require additional work.

The source synthesis supplied for this pass is retained locally and is not
intended to be copied wholesale into the article. The manuscript follows
`STYLE_PROFILE.md` and preserves the distinction between a generalized-Bayes
loss update and an ordinary response likelihood.

## Accepted mathematical statements

Under continuity, finite-moment, interchange, and nondegenerate-interior
conditions, the unrestricted pointwise ordinary-RQR first-order system gives

\[
\Pr(L_c<Y<U_c)=c
\]

and

\[
\mathbb E(Y\mid L_c<Y<U_c)=\mathbb E(Y).
\]

The second equation is equivalent to balance of the centered first-moment
contributions omitted from the lower and upper tails. For a continuous,
strictly increasing distribution with quantile function \(Q\), define

\[
M_c(u)=\frac{1}{c}\int_u^{u+c}Q(v)\,dv.
\]

Because

\[
M_c'(u)=\frac{Q(u+c)-Q(u)}{c}>0,
\]

the equation \(M_c(u)=\mathbb E(Y)\) identifies at most one content-\(c\)
quantile window, and the endpoint window averages strictly bracket the
population mean under the stated nondegeneracy and integrability conditions.
Profiling the expected loss over the half-width gives the derivative
\[
\overline R'_{c,\delta}(m)
=2c\{M_c(u_c(m))-\mu-\delta\},
\]
where \(u_c(m)\) is clamped to \(0\) or \(1-c\) when a root extends beyond a
finite support endpoint. Its negative-to-positive sign change supplies a
unique global finite-root target for every interior admissible tilt, not only
a stationary-window characterization.

For a fixed response-scale tilt \(\delta\), the proposed loss is

\[
\ell_{c,\delta}(L,U;y)
=
\rho_c\{(y-L)(y-U)\}-c\delta(L+U-2y).
\]

Its half-width equation is unchanged, while its midpoint equation gives

\[
\Pr(L_{c,\delta}<Y<U_{c,\delta})=c
\]

and

\[
\mathbb E(Y\mid L_{c,\delta}<Y<U_{c,\delta})
=\mathbb E(Y)+\delta.
\]

Thus \(M_c(u_\delta)=\mathbb E(Y)+\delta\). Interior admissible fixed tilts
index the finite-root contiguous content-\(c\) family. At an admissible
endpoint, a finite support endpoint leaves the inactive root unidentified,
whereas an infinite support endpoint produces an unattained semi-infinite
limiting member. Outside the closed admissible range, the unrestricted
population risk is unbounded below. Distribution-specific population tilts
represent the equal-tailed interval. Shortest-contiguous recovery is
set-valued unless its width minimizer is unique:
\[
\mathcal U_{\mathrm{SH}}=\arg\min_{0\le u\le1-c}W_c(u),\qquad
\Delta_{\mathrm{SH}}
=\{M_c(u)-\mu:u\in\mathcal U_{\mathrm{SH}}\}.
\]
These are population representation results, not data-driven selection
results.

For a fixed learning rate and fixed tilt vector, the pseudo-AL augmentation is
unchanged. Each Gaussian root precision is unchanged and its information
vector gains

\[
\omega c X^\top\boldsymbol\delta.
\]

The latent GIG conditional is unchanged. The sign and scale of this shift were
checked directly from the generalized-posterior exponent.

The complementary distribution outside an ordinary-RQR interval also has the
population mean. A secant-line argument therefore orders the retained core,
the full distribution, and the excluded tails in convex order. The ordinary
RQR population intervals are nested across content levels and contract to the
mean as content decreases to zero. These statements concern the complete
population path; they do not identify the mean from the midpoint of one
positive-content interval or create joint posterior draws across separately
fitted coverage levels.

For \(Y^\star=r+sY\), \(s>0\), the endpoints transform affinely, the tilt
transforms as \(\delta^\star=s\delta\), and the loss is multiplied by \(s^2\).
A corresponding fixed-rate generalized posterior therefore uses
\(\omega^\star=\omega/s^2\) and a transformed prior. The method is not
generally equivariant under nonlinear monotone transformations, and its
quadratic tail growth makes the finite-second-moment and response-scale
assumptions substantive.

## Material integrated into the article

`main.tex` now:

1. defines the fixed-content interval family before introducing the ordinary
   RQR target;
2. states the mean-preserving population interpretation in the abstract and
   introduction;
3. develops the midpoint--half-width geometry and the global profiled-risk
   identification result;
4. records the retained-core interpretation, nested population path, and
   quadratic tail sensitivity without turning the midpoint into a mean;
5. compares equal-tailed, ordinary RQR, and set-valued
   shortest-contiguous recovery;
6. introduces the fixed mean-tilted population target with explicit boundary
   and out-of-range qualifications;
7. records only the scalar fixed-rate Gaussian information-vector shift;
8. makes RQR-DLM the primary computational extension and RQR-DESN a secondary
   deterministic-feature readout;
9. records implementation and evidence status in a compact table; and
10. keeps response-predictive and calibration claims out of scope.

`rqr-gibbs-supplement.tex` now:

1. gives the ordinary-RQR score derivation, tail first-moment balance, and
   quantile-window proof;
2. proves global profiled-risk identification with one-sided boundary logic;
3. proves the retained-core convex-order contraction;
4. derives nesting, the zero-content mean anchor, positive-affine
   equivariance, and tail sensitivity;
5. distinguishes shortest-contiguous intervals from potentially disconnected
   highest-density regions;
6. proves the fixed-coverage mean-tilt characterization and set-valued
   shortest recovery;
7. derives the width-profile relationship;
8. derives the RQR-W coverage correction and changed retained-mean equation;
9. records the fixed-rate canonical-vector shift and required implementation
   tests; and
10. explicitly prevents reuse of the ordinary learned-scale Gamma update for
   the tilted loss.

The bibliography adds the published interval-functional and elicitability
reference by Brehmer and Gneiting (2021).

## Claims deliberately deferred

The following items are not presented as established or implemented:

- posterior propriety conditions specific to nonzero tilt;
- an automatically learned or jointly sampled tilt;
- the ordinary learned inverse-loss-scale update under mean tilt;
- generalized-posterior calibration for endpoint uncertainty;
- a scalar tilt that recovers a pointwise conditional target for all
  covariates;
- a stochastic dynamic tilt process;
- RQR-DLM or RQR-DESN mean-tilt implementation;
- disconnected highest-density-region recovery;
- finite-sample or asymptotic tilt-selection guarantees;
- mean inference from endpoint draws;
- the conditional-mean pseudo-outcome and its cross-fitted uncertainty module;
- width-channel shrinkage priors and feature selection;
- a reported asymmetry index before its empirical role is frozen;
- small-coverage expansions and extrapolated mean inference;
- numerical figures or empirical claims based only on oracle illustrations.

These exclusions keep the current ordinary RQR-DLM confirmatory study
scientifically interpretable.

## Disposition of `moreideasonrqrgibbs.md`

The complete local source was reviewed rather than sampled. Its central
ordinary-RQR characterization, interval-functional comparison, mean-tilted
population family, RQR-W distinction, fixed-rate Gibbs consequence, and
figure-planning principles are represented in the manuscript, supplement, or
the staged plan above.

The document also proposes several credible but independent projects:
generalized-posterior calibration, a cross-fitted mean pseudo-outcome,
width-channel priors, Bayesian or validation-based tilt selection, dynamic
tilt processes, and full-density or moment-condition shortest-interval
methods. They are not silently promoted into the current article because each
changes the inferential contract, requires additional theory or code, or would
compete with the ordinary RQR-DLM validation now under review. The local source
remains untracked; this note is the versioned audit trail for what was retained
and what was deferred.

## Required implementation sequence

1. Add fixed-rate, intercept-only mean tilt with exact zero-tilt
   backward-compatibility.
2. Verify the canonical-vector shift, unchanged precision, unchanged GIG
   conditional, and root-exchange invariance.
3. Compare intercept-only Gibbs marginals with direct two-dimensional
   quadrature at fixed tilts.
4. Verify oracle identities for symmetric, right-skewed, left-skewed,
   boundary, and multimodal distributions.
5. Promote fixed-design scalar/vector tilt with target and checkpoint digests.
6. Evaluate oracle fixed-tilt recovery before introducing any data-driven
   selection rule.
7. Use separate training, validation, and test roles for tilt selection.
8. Consider dynamic or DESN mean tilt only after fixed-design acceptance gates
   pass.

The first implementation must reject learned-rate modes for nonzero mean tilt.

## Article figure allocation

The retained deterministic theory-figure sequence is:

1. equal-tail probability, RQR tail first-moment balance, and shortest width
   at common content;
2. the quantile-window mean and width paths indexed by standardized tilt;
3. the pseudo-residual augmentation and blocked Gibbs scan;
4. the cross-distribution recovery matrix;
5. pointwise loss geometry; and
6. the blocked DLM scan.

The former standalone symmetry-versus-skewness figure is removed because the
cross-distribution matrix contains the same comparison. Captions and the
figure manifest classify all distributional calculations as deterministic
population illustrations, not finite-sample evidence.

## Overleaf release checklist

Before committing the article revision:

1. reconcile the current remote `main` branch without accepting its
   executable-bit regressions in simulation launchers;
2. rerun `make smoke`, `make pdf`, and `make supplement`;
3. require no undefined citations, references, or TeX overflow warnings;
4. inspect the rendered pages containing the new propositions and table;
5. confirm that only manuscript source, deterministic figure source/tests,
   publication assets, and manuscript audit documentation are staged;
6. ensure generated PDFs, TeX logs, and local source syntheses remain ignored;
7. commit the article revision separately from any simulation authorization;
   and
8. push only after the exact diff is reviewed.
