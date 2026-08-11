# Scan-Calibrated Minimum-Width Tolerance Intervals with MT-RQR

## Authoritative method, current RQR-GIBBS repository alignment, theoretical development plan, manuscript-reframing proposal, and Codex implementation prompt

**Prepared:** 2026-08-11
**Project:** `AntonioAPDL/RQR-GIBBS`
**Method name used in this report:** **Scan-Calibrated Tolerance-Calibrated Shortest-Path Mean-Tilted Relaxed Quantile Regression**, abbreviated **SC-TCSP-MT-RQR** or, when no confusion is possible, **TCSP-MT-RQR**
**Purpose:** This is an implementation and manuscript-design report. It does not replace the formal proofs still required for publication-level guarantees.

---

# 1. Executive decision

## 1.1 Authoritative statistical construction

The authoritative finite-sample univariate construction should be:

\[
\boxed{
\text{calibrate the retained sample count by a distribution-free scan statistic}
\;\longrightarrow\;
\text{select the shortest interval containing that count}
\;\longrightarrow\;
\text{compute its MT-RQR shortest-path tilt}
\;\longrightarrow\;
\text{fit the fixed-target generalized posterior}.
}
\]

For desired population content \(c\in(0,1)\), tolerance confidence
\(1-\alpha\), and an iid continuous sample \(Y_{1:n}\), define the formal
tolerance target

\[
\Pr_F^n\!\left[
C_F(\widehat T_n)\ge c
\right]\ge 1-\alpha,
\qquad
C_F([L,U])=F(U)-F(L).
\]

The scan calibration determines the smallest retained count
\(k_{n,c,\alpha}\) that can receive this content-confidence certificate. The
reported frequentist action is then the shortest observed interval containing
exactly \(k_{n,c,\alpha}\) observations. Its target content is

\[
q_{n,c,\alpha}=\frac{k_{n,c,\alpha}}{n}\ge c,
\]

and its content buffer is

\[
t_{n,c,\alpha}=q_{n,c,\alpha}-c.
\]

The corresponding shortest-path mean tilt is computed from the selected
window and is then frozen as target metadata for MT-RQR-GIBBS.

This construction integrates validity and shortestness in the correct order:

\[
\boxed{
\text{minimum width subject to tolerance validity}.
}
\]

It does not attach an arbitrary symmetric margin to an already fitted
interval. The scalar content buffer controls how much total probability must
be targeted; the shortest-path fit decides, generally asymmetrically, where
that probability lies.

## 1.2 Exact role of the generalized posterior

For fixed calibrated content \(q\), fixed shortest-path tilt \(\delta\), fixed
learning rate \(\omega\), and interval-root parameter \(\theta\),

\[
\Pi_{q,\delta,\omega}(d\theta\mid D_n)
\propto
\exp\{-\omega L_{q,\delta,n}(\theta)\}\Pi_0(d\theta).
\]

This is coherent generalized-Bayesian inference for a loss-defined interval
functional. It is **not** the source of the tolerance certificate. The
statistical roles must remain separate:

\[
\begin{aligned}
c &:\text{ minimum population content to guarantee},\\
1-\alpha &:\text{ repeated-sample tolerance confidence},\\
q &:\text{ calibrated fitted content},\\
\delta &:\text{ asymmetric MT-RQR placement coordinate},\\
\omega &:\text{ loss-to-prior concentration scale}.
\end{aligned}
\]

The formal finite-sample tolerance action is the empirical shortest calibrated
window unless a later action-equivalence theorem or a direct certification
theorem transfers the guarantee to a posterior summary.

## 1.3 Recommended article title

If tolerance calibration becomes the central contribution of the article, the
preferred title is:

> **Calibrated Minimum-Width Tolerance Intervals with Generalized-Bayesian Relaxed Quantile Regression**

This is preferable to “A Bayesian Approach for Minimum Width Tolerance
Intervals” because the validity statement is repeated-sample frequentist
tolerance confidence, whereas the Bayesian component is a generalized
posterior for the fixed interval functional.

Strong alternatives are:

1. **Tolerance-Calibrated Shortest-Path Relaxed Quantile Regression**
2. **Generalized-Bayesian Inference for Calibrated Minimum-Width Tolerance Intervals**
3. **Scan-Calibrated Shortest Intervals with Mean-Tilted Relaxed Quantile Regression**
4. **Minimum-Width Content–Confidence Intervals by Generalized Bayesian Updating**

The first recommended title is the clearest for a statistics audience.

## 1.4 Manuscript strategy

The current article is already a substantial paper on fixed-content MT-RQR,
generalized-posterior theory, Gibbs computation, frozen DESN readouts, and
dynamic root states. A tolerance-centered revision is therefore a **major
reframing**, not a minor additional subsection.

The safest plan is two-stage:

1. **Now:** create a dedicated feature branch, add a rigorous proposed
   TCSP section, add the software and theorem-development contracts, and
   prepare candidate title/abstract/introduction revisions.
2. **After proof and software gates pass:** promote TCSP to the article’s
   primary contribution and activate the tolerance-centered title, abstract,
   and claims.

If the full Tier-1 theorem package cannot be completed without delaying the
existing MT-RQR paper substantially, the statistically cleaner alternative is
a separate tolerance-focused article. The Codex prompt in Section 15 asks for
a branch-level integrated revision while preserving this gate.

---

# 2. Current RQR-GIBBS repository audit

## 2.1 Live repository state

The public `main` branch was inspected on 2026-08-11. The GitHub page showed
328 commits, but the exact current commit hash was not exposed by the rendered
page. Codex must record the exact local hash before changing anything:

```bash
git rev-parse HEAD
git log -1 --oneline --decorate
git status --short --branch
```

The current live manuscript is titled:

> **Mean-Tilted Relaxed Quantile Regression: Fixed-Content Interval Functionals and Generalized-Bayes Computation**

The live article already contains:

- the fixed-content probability-window representation;
- the distinction among equal-tailed, ordinary mean-preserving RQR, and
  shortest-contiguous intervals;
- the mean-tilted loss and recovery tilts;
- Cornish–Fisher shortest-tilt and equal-tailed-tilt anchors;
- exact fractional empirical balance;
- static fixed-target sandwich and generalized-posterior uncertainty;
- pseudo-asymmetric-Laplace augmentation;
- fixed-design, frozen-DESN, and dynamic root-state computation;
- explicit warnings that the augmentation is not a response likelihood and
  that root draws are not posterior-predictive response draws.

The current live manuscript does **not** contain a tolerance-interval section
or a formal content-confidence construction.

## 2.2 Existing code that should be reused

The live package already provides the main building blocks.

### Empirical shortest-window tilt pilot

`application/R/rqr_mean_tilt_init.R` implements

```r
rqr_mt_tilt_empirical_shortest()
```

It:

- sorts the response;
- uses
  \[
  m=\lceil qn\rceil;
  \]
- scans the closed windows
  \[
  [Y_{(j)},Y_{(j+m-1)}];
  \]
- selects the first minimum under a deterministic numerical tie rule;
- stores the realized content, endpoints, width, retained mean, boundary
  status, tie count, and raw/standardized tilt.

This is already very close to the required shortest-action engine.

### Cornish–Fisher pilot and screening grid

The same file implements:

```r
rqr_mt_tilt_cf()
rqr_mt_tilt_screen()
```

The shortest-oriented first-order standardized anchor is

\[
\widehat d_{\rm SH}^{\rm CF}(q)
=
-\widehat\gamma_1
\frac{z_q\phi(z_q)}{q},
\qquad
z_q=\Phi^{-1}\{(1+q)/2\}.
\]

The code correctly labels this as an initialization or screening anchor, not
an automatic shortest-interval guarantee.

### Existing candidate selector

The live code also contains

```r
rqr_mt_select_tilt_candidate()
```

which selects minimum held-out width subject to either point empirical
coverage or a simultaneous Normal lower-bound guard. This function is useful
as exploratory infrastructure but is **not** the authoritative scan-calibrated
tolerance procedure. In particular:

- the Normal lower bound is approximate;
- it targets validation coverage of a finite tilt grid rather than the exact
  intercept-only scan action;
- it should not be relabeled as a finite-sample tolerance certificate.

### Fixed nonzero-tilt MCMC

`application/R/rqr_mcmc_fit.R` already accepts `mean_tilt`. Nonzero tilt is
gated to:

- `learning_rate_mode = "fixed_rate"`;
- ridge Gaussian coefficient priors;
- the supported fixed-design and frozen-feature paths.

The test suite verifies that a fixed tilt changes only the Gaussian
information vector and leaves the conditional precision unchanged.

### Whole-root exchangeability

The MCMC scan swaps complete root blocks, and post-processing uses the
root-label contract to decide whether coefficient-level lower/upper labels are
valid. Ordered endpoint functionals remain available by pointwise sorting.
This is the correct architecture for TCSP.

## 2.3 Missing authoritative layer

The live `main` branch does not currently include:

- a scan-statistic critical-count implementation;
- a `q` calibration object carrying \((c,\alpha,k,q,t)\);
- a formal TCSP fit object;
- a theorem/software action contract for the calibrated shortest interval;
- an exact or rigorously conservative numerical critical-value engine;
- a tolerance-centered manuscript section;
- a repeated-sample tolerance validation pipeline for the final action.

The earlier TCSP theory handoff exists in project materials, but its
`docs/theory/tcsp_mt_rqr/` overlay is not present on the current public
`main` branch. It must be reconciled with, not blindly copied over, the much
more advanced current repository.

## 2.4 Canonical action decision

Earlier theory notes used a half-open spacing

\[
(Y_{(j)},Y_{(j+k)}],
\]

which contains \(k\) observations. The current live code and the simulation
work use the closed \(k\)-observation window

\[
\boxed{
\widehat I_{j,k}
=
[Y_{(j)},Y_{(j+k-1)}],
\qquad
j=1,\ldots,n-k+1.
}
\]

For a continuous distribution, endpoint probability is zero, so both
conventions can be made valid with consistent indexing. They are nevertheless
different software actions.

**Recommendation:** adopt the closed \(k\)-observation window as the canonical
TCSP action because it matches:

- the current `rqr_mt_tilt_empirical_shortest()` implementation;
- the simulations already performed;
- the intuitive statement “shortest interval containing \(k\) observations.”

The scan statistic, proofs, serialized metadata, tests, tables, and manuscript
must all use this exact convention. No theorem should be written for one
action and implemented with the other.

---

# 3. Statistical target and notation

## 3.1 Six distinct probability concepts

The article must distinguish:

1. **Population content**
   \[
   C_F([L,U])=F(U)-F(L).
   \]

2. **Empirical training content**
   \[
   P_n([L,U])=\frac1n\sum_{i=1}^n
   \mathbf 1\{L\le Y_i\le U\}.
   \]

3. **Predictive coverage** for a future observation.

4. **Repeated-sample confidence**
   over repeated training samples.

5. **Posterior credibility**
   under the generalized posterior.

6. **Tolerance content-confidence**
   \[
   \Pr_F^n\{C_F(\widehat T_n)\ge c\}\ge1-\alpha.
   \]

“Coverage” should not be used without a qualifier.

## 3.2 Population MT-RQR coordinates

For a continuous distribution \(F\) with quantile function \(Q\), finite mean
\(\mu\), and target content \(q\), write every contiguous content-\(q\)
interval as

\[
I_q(u)=[Q(u),Q(u+q)],
\qquad
0\le u\le1-q.
\]

Define

\[
W_q(u)=Q(u+q)-Q(u)
\]

and

\[
M_q(u)=\frac1q\int_u^{u+q}Q(v)\,dv.
\]

The MT-RQR tilt coordinate is

\[
\delta(q,u)=M_q(u)-\mu.
\]

The shortest content-\(q\) path is

\[
u_{\rm SH}(q)\in
\arg\min_{0\le u\le1-q}W_q(u),
\]

\[
I_q^{\rm SH}=I_q\{u_{\rm SH}(q)\},
\]

\[
\delta_{\rm SH}(q)
=
M_q\{u_{\rm SH}(q)\}-\mu.
\]

The tolerance problem is therefore a path-selection problem:

\[
\text{choose the calibrated }q
\quad\text{and then use}\quad
\delta_{\rm SH}(q).
\]

---

# 4. Distribution-free scan calibration

## 4.1 Scan statistic

Conceptually transform the data by the unknown CDF:

\[
U_i=F(Y_i)\sim {\rm Uniform}(0,1).
\]

For desired guaranteed content \(c\), define

\[
M_n(c)
=
\sup_{0\le u\le1-c}
\sum_{i=1}^n
\mathbf 1\{u\le U_i\le u+c\}.
\]

This is the largest number of observations that can fall in any population
interval having content at most \(c\). Its distribution depends only on
\((n,c)\), not on the unknown continuous \(F\).

Choose

\[
\boxed{
k_{n,c,\alpha}
=
\min\left\{
k:
\Pr\{M_n(c)<k\}\ge1-\alpha
\right\}.
}
\]

Set

\[
q_{n,c,\alpha}=\frac{k_{n,c,\alpha}}n.
\]

## 4.2 Why it certifies the selected shortest interval

On the event

\[
M_n(c)<k_{n,c,\alpha},
\]

no interval with true population content at most \(c\) contains
\(k_{n,c,\alpha}\) observations. Therefore every sample interval containing
that many observations has population content greater than \(c\). In
particular, the shortest such interval satisfies the content requirement.

Thus the target theorem is

\[
\boxed{
\Pr_F^n\!\left[
C_F\!\left(
\widehat I^{\rm SH}_{n,k_{n,c,\alpha}}
\right)\ge c
\right]\ge1-\alpha.
}
\]

The selection of the shortest window is harmless because the scan event is
uniform over all intervals.

## 4.3 Why the smallest calibrated count yields the narrowest scan action

Let

\[
\widehat W_n(k)
=
\min_j
\left\{
Y_{(j+k-1)}-Y_{(j)}
\right\}.
\]

Then

\[
\widehat W_n(k+1)\ge \widehat W_n(k).
\]

Every interval containing \(k+1\) ordered observations contains a
\(k\)-observation subwindow no wider than itself. Therefore:

1. at fixed \(k\), selecting the shortest window is sample-wise width optimal;
2. among scan-certified counts, selecting the smallest valid \(k\) is
   sample-wise width optimal.

The resulting exact claim is still restricted:

> The method is minimum width within the declared class of contiguous empirical
> \(k\)-observation intervals certified by the same scan rule and measured on
> the declared response scale.

It is not a theorem of global optimality over every possible tolerance
procedure, randomized rule, disconnected set, or parametric model.

## 4.4 Critical-value computation

Three levels should be supported.

### Publication-grade primary mode

Use either:

- an exact finite-\(n\) scan-statistic recursion; or
- a rigorously conservative numerical algorithm with certified error bounds.

### Rigorously conservative Monte Carlo mode

If an exact recursion is unavailable, estimate the scan probability under
Uniform\((0,1)\) and select \(k\) using a one-sided lower confidence bound for

\[
\Pr\{M_n(c)<k\}.
\]

The Monte Carlo confidence allocation must be declared. This mode is
distribution free but numerically conservative; it should not be called
“exact.”

### Fallback bounds

Retain:

- a Kuiper/uniform interval-mass bound once its finite-\(n\) normalization is
  independently verified;
- a DKW fallback for conservative feasibility checks;
- the large-sample content formula only as an initializer.

The new scan theorem should become the primary finite-sample theorem. The
previously planned Kuiper theorem becomes a useful general-purpose fallback
and benchmark.

## 4.5 Feasibility

If no \(k\le n\) satisfies the calibrated scan requirement, the method must
return an infeasibility object rather than silently use the sample range or
lower the requested confidence.

The object should record:

- `n`;
- `guaranteed_content = c`;
- `tolerance_confidence = 1-alpha`;
- the largest attainable scan confidence;
- the limiting count considered;
- the reason for failure.

---

# 5. Minimum-width interval and tilt estimation

## 5.1 Exact empirical shortest interval at calibrated \(q\)

Sort the data:

\[
Y_{(1)}\le\cdots\le Y_{(n)}.
\]

At the scan-calibrated count \(k\), compute

\[
w_j(k)=Y_{(j+k-1)}-Y_{(j)},
\qquad
j=1,\ldots,n-k+1.
\]

Use

\[
\widehat j_k
=
\min\arg\min_j w_j(k)
\]

as the deterministic first-minimum tie rule. Report

\[
\widehat L_k=Y_{(\widehat j_k)},
\qquad
\widehat U_k=Y_{(\widehat j_k+k-1)},
\]

\[
\widehat W_k=\widehat U_k-\widehat L_k.
\]

The response scale on which width is minimized must be declared before
looking at results.

## 5.2 Empirical shortest-path tilt

The empirical retained mean is

\[
\widehat M_k
=
\frac1k
\sum_{r=\widehat j_k}^{\widehat j_k+k-1}
Y_{(r)}.
\]

The raw-scale shortest-path tilt is

\[
\boxed{
\widehat\delta_{\rm SH}(q)
=
\widehat M_k-\overline Y.
}
\]

The standardized tilt is

\[
\widehat d_{\rm SH}(q)
=
\frac{\widehat\delta_{\rm SH}(q)}{s_Y}.
\]

This is exactly the quantity already computed by the current empirical
shortest-window pilot.

The tilt is not a second tolerance parameter. It is the MT-RQR coordinate of
the selected shortest content-\(q\) interval.

## 5.3 Fractional content targets

The scan rule naturally returns integer \(k\), so fractional endpoint weights
are unnecessary for the formal U-FS action. Fractional weights remain useful
for:

- smooth path plots;
- analytic \(q\)-grid comparisons;
- exact subgradient identities;
- large-sample interpolation.

If fractional windows are implemented, their endpoint weighting, realized
empirical content, and retained-mean definition must be fixed explicitly and
must not replace the canonical integer scan action without a new theorem.

## 5.4 Cornish–Fisher initialization

The current Cornish–Fisher shortest-tilt approximation should remain an
initializer:

\[
\widehat d_{\rm SH}^{\rm CF}(q)
\approx
-\widehat\gamma_1
\frac{z_q\phi(z_q)}q.
\]

Its proper uses are:

- warm start for an external tilt profile;
- center of a training-only candidate grid;
- near-Normal diagnostic;
- comparison against the empirical shortest-window tilt.

It is not:

- a tolerance calibration;
- a minimum-width certificate;
- a posterior draw;
- a substitute for the global shortest-window scan.

## 5.5 Regression qualification

In intercept-only data, the empirical shortest window determines one scalar
tilt.

In regression, the pointwise shortest conditional tilt generally depends on
\(x\):

\[
\delta_{\rm SH}(q,x).
\]

A single scalar tilt in a restricted root model is therefore usually a
model-relative integrated target, not the pointwise shortest interval for
every covariate value. The first authoritative paper should make the
finite-sample univariate result primary and treat regression as:

- marginal tolerance over a new \((X,Y)\) draw;
- a finite candidate family frozen before independent calibration;
- or predeclared stratum-specific tolerance.

---

# 6. Crucial relationships among shortest tilts across contents

## 6.1 Equal-density characterization

Assume:

- connected support;
- continuously differentiable density \(f\);
- a unique interior mode;
- strict increase before the mode and strict decrease after it;
- positive interior endpoint densities.

Then the unique shortest content-\(q\) interval

\[
[L_q,U_q]
\]

satisfies

\[
\boxed{
F(U_q)-F(L_q)=q,
\qquad
f(L_q)=f(U_q)=\lambda_q.
}
\]

The interval is the connected upper density-level set at threshold
\(\lambda_q\).

## 6.2 Nestedness

Under those assumptions,

\[
q_1<q_2
\quad\Longrightarrow\quad
[L_{q_1},U_{q_1}]
\subset
[L_{q_2},U_{q_2}].
\]

Writing

\[
u_q=F(L_q),
\qquad
v_q=F(U_q)=u_q+q,
\]

nestedness gives

\[
u_q\text{ nonincreasing},
\qquad
v_q\text{ nondecreasing}.
\]

For \(q_{\rm new}=q+h\),

\[
\boxed{
u_q-h\le u_{q+h}\le u_q.
}
\]

This bracket is the basic continuation tool.

## 6.3 Endpoint derivatives

Let

\[
a_q=f'(L_q)>0,
\qquad
b_q=f'(U_q)<0.
\]

Differentiating the content and equal-density equations gives

\[
\boxed{
L_q'
=
\frac{b_q}
{\lambda_q(a_q-b_q)}
<0,
}
\]

\[
\boxed{
U_q'
=
\frac{a_q}
{\lambda_q(a_q-b_q)}
>0.
}
\]

On the probability scale,

\[
\boxed{
u_q'
=
\frac{b_q}{a_q-b_q}\in(-1,0),
}
\]

\[
\boxed{
v_q'
=
\frac{a_q}{a_q-b_q}\in(0,1).
}
\]

## 6.4 Asymmetric allocation of added content

When content increases by \(dq\), the probability added on the left and right
has first-order shares

\[
\boxed{
p_L(q)
=
-u_q'
=
\frac{-b_q}{a_q-b_q},
}
\]

\[
\boxed{
p_R(q)
=
v_q'
=
\frac{a_q}{a_q-b_q},
}
\]

with

\[
p_L(q)+p_R(q)=1.
\]

Only under local symmetry, \(a_q=-b_q\), are both shares \(1/2\). This is the
formal reason scalar content calibration preserves asymmetric RQR placement.

## 6.5 Width derivative and convexity

The shortest width

\[
W_{\rm SH}(q)=U_q-L_q
\]

satisfies

\[
\boxed{
W_{\rm SH}'(q)=\frac1{\lambda_q}.
}
\]

The marginal width price of additional content is the reciprocal endpoint
density.

Moreover,

\[
\lambda_q'
=
\frac{a_qb_q}
{\lambda_q(a_q-b_q)}
<0
\]

and

\[
\boxed{
W_{\rm SH}''(q)
=
-\frac{a_qb_q}
{\lambda_q^3(a_q-b_q)}
>0.
}
\]

Thus the shortest width is increasing and convex in content. High-content
increments are increasingly expensive when endpoints move into low-density
tails.

## 6.6 Shortest-tilt derivative

Define the retained mean

\[
m_q
=
\frac1q
\int_{u_q}^{v_q}Q(s)\,ds
=
\mu+\delta_{\rm SH}(q).
\]

Then

\[
\boxed{
\delta_{\rm SH}'(q)
=
\frac{
p_L(q)L_q+p_R(q)U_q-\{\mu+\delta_{\rm SH}(q)\}
}{q}.
}
\]

Interpretation:

- \(p_LL_q+p_RU_q\) is the mean value of the infinitesimal probability being
  added;
- \(\mu+\delta_{\rm SH}(q)\) is the current retained mean;
- the tilt increases when the new boundary mass has a larger mean than the
  retained interval, and decreases otherwise.

The tilt path need not be monotone. Therefore path estimation should be based
primarily on \(u_q\), \(L_q\), and \(U_q\), not on an imposed monotonic model
for \(\delta_{\rm SH}(q)\).

For symmetric distributions,

\[
\delta_{\rm SH}(q)=0
\quad\text{for all }q.
\]

Under a unique interior mode \(m_0\),

\[
\delta_{\rm SH}(q)\to m_0-\mu
\quad\text{as }q\downarrow0,
\]

and, under finite first moment,

\[
\delta_{\rm SH}(q)\to0
\quad\text{as }q\uparrow1.
\]

## 6.7 Local width curvature in the tilt coordinate

At a regular minimum,

\[
\frac{\partial W}{\partial\delta}=0.
\]

The local curvature is

\[
\boxed{
\kappa_\delta(q)
=
\left.
\frac{\partial^2W(q,\delta)}{\partial\delta^2}
\right|_{\delta=\delta_{\rm SH}(q)}
=
\frac{
q^2(a_q-b_q)
}{
\lambda_q^3W_{\rm SH}(q)^2
}
>0.
}
\]

Hence

\[
W(q,\delta)
=
W_{\rm SH}(q)
+
\frac12\kappa_\delta(q)
\{\delta-\delta_{\rm SH}(q)\}^2
+
o\!\left(
\{\delta-\delta_{\rm SH}(q)\}^2
\right).
\]

A small tilt-prediction error has only second-order width cost. This explains
why interpolation is effective as a warm start while still requiring a final
optimization.

---

# 7. Estimating a new minimum from one or several previous minima

## 7.1 One previous minimum

Suppose the shortest solution is available at \(q\), and the new target is

\[
q^\star=q+h.
\]

### Distribution-free bracket

Use

\[
u_q-h\le u_{q^\star}\le u_q.
\]

### Derivative predictor

If endpoint-density derivatives are estimated,

\[
\boxed{
u_{\rm pred}
=
u_q
+
h\frac{b_q}{a_q-b_q}.
}
\]

Clip it to the nested bracket.

Endpoint predictions are

\[
L_{\rm pred}
=
L_q
+
h\frac{b_q}{\lambda_q(a_q-b_q)},
\]

\[
U_{\rm pred}
=
U_q
+
h\frac{a_q}{\lambda_q(a_q-b_q)}.
\]

Width and tilt predictions are

\[
W_{\rm pred}
=
W_q+\frac{h}{\lambda_q},
\]

\[
\delta_{\rm pred}
=
\delta_q+
\frac{h}{q}
\left[
p_LL_q+p_RU_q-(\mu+\delta_q)
\right].
\]

Use these only as predictors; correct them by optimization at \(q^\star\).

## 7.2 Several previous minima

Suppose estimates are available at

\[
q_1<\cdots<q_K
\]

and store

\[
(q_k,u_k,L_k,U_k,W_k,\delta_k).
\]

### Interpolation within the grid

For \(q_j<q^\star<q_{j+1}\), the exact nesting bracket is

\[
u_{j+1}\le u(q^\star)\le u_j.
\]

Use a monotone interpolation of \(u(q)\), preferably a shape-preserving cubic
or constrained local linear fit. Clip the predictor to the bracket.

### Extrapolation above the grid

Estimate the local slope using several recent points:

\[
\widehat s
=
\arg\min_{s\in[-1,0],a}
\sum_{r=K-m+1}^{K}
w_r
\left[
u_r-a-s(q_r-q_K)
\right]^2.
\]

Then

\[
u_{\rm pred}
=
u_K+\widehat s(q^\star-q_K),
\]

clipped to

\[
[u_K-(q^\star-q_K),u_K].
\]

### Width interpolation

Fit \(W_{\rm SH}(q)\) under the constraints:

\[
W'(q)\ge0,
\qquad
W''(q)\ge0.
\]

A convex spline or convex regression is preferable to an unconstrained
polynomial.

### Tilt interpolation

Do not impose monotonicity on \(\delta(q)\). Recover it from the predicted
window whenever possible:

\[
\delta(q)
=
M_q\{u(q)\}-\mu.
\]

## 7.3 Quantities that must be stored

For each content level, store:

- `target_content`;
- `retained_count`;
- `lower_tail_index`;
- `window_start`;
- `window_end`;
- `lower_endpoint`;
- `upper_endpoint`;
- `width`;
- `retained_mean`;
- `delta_raw`;
- `delta_standardized`;
- `tie_count`;
- `boundary_status`;
- `global_verified`;
- `local_search_radius`;
- `local_boundary_hit`;
- `mode_switch_flag`.

A grid of only \((q,\delta,W)\) discards the information needed for rigorous
bracketing and path diagnostics.

---

# 8. Local optimization and sequential path tracing

## 8.1 Important computational fact

For one univariate target \(k\), a global shortest-window scan after sorting is
only \(O(n)\). Therefore the formal univariate tolerance action should always
be globally verified.

Local continuation is useful for:

- dense coverage-path plots;
- repeated candidate fits;
- regression screening;
- expensive Gibbs fits at many \((q,\delta)\) targets;
- initialization near \(q\approx1\).

It is an accelerator, not the validity proof.

## 8.2 Discrete nested-expansion predictor

Suppose the previous interval contains \(k\) observations and the new target
contains

\[
k_{\rm new}=k+d.
\]

If the old window starts at index \(j\), enumerate

\[
a=0,\ldots,d,
\]

where \(a\) new observations are added on the left and \(d-a\) on the right.
The candidate start is

\[
j-a,
\]

subject to sample boundaries. Select the narrowest feasible nested expansion.

This produces an exact predictor within the class of expansions containing the
previous interval.

## 8.3 Adaptive local correction

Starting from the predicted index:

1. choose a trust radius \(r\), for example
   \[
   r=\max(2d,r_{\min});
   \]
2. evaluate all windows in
   \[
   [j_{\rm pred}-r,j_{\rm pred}+r];
   \]
3. choose the narrowest;
4. if the minimizer lies on an interior trust-region boundary, double \(r\);
5. repeat until the minimizer is interior or the full domain is reached;
6. perform a full global scan for the formal U-FS output.

## 8.4 Multiple-coverage algorithm

```text
INPUT:
    sorted observations
    ordered contents q_1 < ... < q_K
    corresponding retained counts k_1 < ... < k_K

1. Solve the smallest content globally.
2. Store its complete window and tilt metadata.
3. For r = 2,...,K:
     a. predict the new start by nested expansion;
     b. optionally combine with a derivative or shape-constrained path predictor;
     c. run adaptive local correction;
     d. run the scheduled global verification;
     e. store local regret and all path diagnostics.
4. Return both:
     - exact global empirical path;
     - continuation path and computational savings.
```

## 8.5 When local continuation must fail closed

Do not rely on a local path when:

- multiple windows are tied within numerical tolerance;
- the selected window jumps between separated modes;
- shortest windows are not nested;
- a boundary mode becomes active;
- ties or atoms invalidate the continuous-action contract;
- the optimizer repeatedly lands on the trust-region boundary;
- the response transformation changes the scientific meaning of width;
- the fitted regression root chart crosses.

---

# 9. Generalized-Bayes computation after calibration

## 9.1 Fixed-target loss

For target content \(q\), fixed tilt \(\delta_i\), and roots
\(\eta_{1i},\eta_{2i}\), use

\[
\ell_{q,\delta_i}
=
\rho_q\!\left[
(Y_i-\eta_{1i})(Y_i-\eta_{2i})
\right]
-
q\delta_i(\eta_{1i}+\eta_{2i}-2Y_i).
\]

In the intercept-only authoritative mode, use the scalar empirical
\(\widehat\delta_{\rm SH}(q)\). In regression, \(\delta_i\) may be a frozen
training-only vector under an explicitly declared model-relative rule.

## 9.2 Working-mixture constants

For every changed target content \(q\), recompute

\[
\xi_q=\frac{1-2q}{q(1-q)},
\qquad
\phi_q=\frac{2}{q(1-q)}.
\]

The latent-variable update remains generalized inverse Gaussian, and the two
root blocks remain conditionally Gaussian.

## 9.3 Mean-tilt information shift

For a fixed-design root update, a nonzero fixed tilt adds

\[
\omega q X^\top\delta
\]

to the Gaussian information vector while leaving the precision matrix
unchanged. The current test suite already checks this identity.

## 9.4 Supported first implementation

Tolerance mode should initially require:

- fixed learning rate;
- proper Gaussian/ridge root priors;
- fixed-design or frozen-DESN readout;
- DLM only with fixed \(W_t\) or a pre-frozen discount template;
- whole-root swap and canonical endpoint ordering;
- no in-chain random \(q\) or \(\delta\).

It should reject:

- learned inverse-loss scale;
- nonzero-tilt RHS-NS;
- adaptive or component-scale dynamic tilt;
- VB/CAVI;
- replica exchange for nonzero tilt;
- post-calibration refitting;
- a response-predictive interpretation.

## 9.5 Formal action versus posterior summaries

Store two distinct fields:

```text
formal_tolerance_action
posterior_summary_action
```

For the first paper:

```text
formal_tolerance_action =
    canonical scan-calibrated empirical shortest window
```

Posterior means, medians, modes, endpoint bands, and sandwich-adjusted draws
are supplementary uncertainty summaries. They do not inherit the finite-sample
scan guarantee automatically.

---

# 10. Strongest defensible claims

## 10.1 Finite-sample univariate claim

After the scan theorem and critical engine pass audit:

> For continuous iid observations, the scan-calibrated shortest
> \(k_{n,c,\alpha}\)-observation interval contains at least population fraction
> \(c\) with repeated-sample confidence at least \(1-\alpha\).

## 10.2 Exact width claim

By construction:

> At the calibrated retained count, the reported interval is the shortest
> contiguous empirical interval containing that count. Because shortest-window
> width is nondecreasing in retained count, the use of the smallest
> scan-certified count gives minimum sample width within the declared
> scan-certified empirical window class.

## 10.3 Population and asymptotic claim

Under unique strict unimodality, regular endpoint densities, common
first-order content calibration, and the required uniformity:

> The shortest content-\(q_n\) MT-RQR target has minimum population width
> within the declared regular contiguous MT-RQR family at the same calibrated
> content and response scale; consequently the procedure is first-order
> asymptotically minimum width in that class.

## 10.4 Claims that remain prohibited

Do not state:

- globally shortest among every tolerance procedure;
- exact minimum expected width at finite \(n\);
- shortest disconnected minimum-volume region;
- transformation-invariant shortestness;
- posterior probability \(1-\alpha\) equals tolerance confidence;
- posterior mean endpoints inherit the empirical scan certificate;
- one scalar \(q\) gives pointwise conditional tolerance for every \(x\);
- \(\omega\) is the tolerance factor;
- endpoint-root draws are posterior-predictive responses.

---

# 11. Theory still required

This report does not derive these results. It specifies the proof package
needed before strong claims enter the article.

## 11.1 Tier 1: indispensable

### T-SCAN-1: finite-sample scan theorem

Prove the exact content-confidence result for the canonical closed
\(k\)-observation action, including measurability, endpoint convention, and
strict-versus-weak inequalities.

### T-SCAN-2: critical-count engine

Provide:

- exact recursion, or
- rigorously conservative numerical certification.

Prove that the returned integer is the smallest count satisfying the requested
confidence after accounting for numerical error.

### T-ACTION: action matching

Show that the theorem action, R output, serialized metadata, figures, and
manuscript all use the same interval convention.

### T-SH-1: shortest-path existence and uniqueness

Under a named strict-unimodality assumption set, prove existence, uniqueness,
upper-density-level-set characterization, nesting, and boundary-mode
qualifications.

### T-SH-2: path derivatives

Prove the endpoint, probability-allocation, width, threshold, and tilt
derivatives in Section 6.

### T-OPT-1: scan-class width optimality

Prove monotonicity of empirical shortest width in \(k\) and the exact
sample-wise optimality statement within the scan-certified window class.

### P0-MT: fixed-target sampler gate

Close the deterministic and numerical proof-to-code gate for every branch
allowed in tolerance mode.

## 11.2 Tier 2: required for a strong statistics paper

### T-MIXED: nonregular shortest-location theory

Reconcile the empirical shortest interval with shorth theory:

- consistency of the selected set;
- possible cube-root location behavior;
- root-\(n\) width behavior;
- induced tilt behavior;
- nonunique/set-valued limits.

### T-CONTENT-IF: fixed-target content influence function

Prove, for the exact empirical action under the declared regularity,

\[
\operatorname{IF}_C(Y)
=
q-\mathbf 1\{Y\in I_q\},
\]

and variance

\[
q(1-q).
\]

### T-ORTH: same-sample shortest-selection orthogonality

Prove that nonregular placement does not affect first-order total content under
consistency, exact empirical mass, and stochastic equicontinuity.

### T-QASY: asymptotic content–confidence map

Establish the triangular-array theorem for

\[
q_n
=
c+
z_{1-\alpha}
\sqrt{\frac{c(1-c)}n}
\]

and its implicit refinement. Keep it as initialization until proved.

### T-WIDTH: first-order tolerance-width price

Use

\[
W_{\rm SH}'(c)=1/\lambda_c
\]

to derive the first-order width cost of tolerance confidence.

### T-OPT-2: first-order MT-RQR optimality

Prove minimum width within the declared regular contiguous MT-RQR class under
a common first-order buffer.

### T-LOCAL: continuation correctness

Specify when the nested/derivative predictor remains in the basin of the
global shortest solution and define a computational certificate based on
adaptive expansion plus global verification.

### T-GB-ACTION: posterior action equivalence

Required only if posterior mean, median, or another Bayes action is promoted
to the formal tolerance action. Total-variation BvM alone is not sufficient
for posterior-mode equivalence.

### T-SANDWICH: endpoint uncertainty

Complete the repository-specific plug-in consistency audit for the sandwich
map. Keep this separate from tolerance validity.

## 11.3 Tier 3: regression and structured extensions

Develop:

- finite-family marginal regression certification on independent calibration
  data;
- exact simultaneous one-sided binomial lower bounds;
- fixed-sequence improvements for genuinely nested candidate paths;
- predeclared stratum-specific tolerance;
- cluster/block calibration under dependence;
- dynamic tolerance paths;
- covariate-dependent shortest tilt \(\delta(q,x)\);
- simultaneous conditional tolerance only as a later, substantially stronger
  goal.

## 11.4 Boundary and failure appendix

The supplement must cover:

- atoms and ties;
- fractional/randomized boundary allocation;
- finite and semi-infinite support;
- boundary modes;
- flat modes;
- multimodal mode switching;
- disconnected highest-density regions;
- infeasible \((n,c,\alpha)\);
- positive-affine equivariance;
- failure of nonlinear transformation invariance.

---

# 12. Theory and manuscript placement

## 12.1 Main article

Once proved, the main article should include concise statements of:

1. tolerance content-confidence definition;
2. scan-calibrated count theorem;
3. shortest-window action and exact class-restricted optimality;
4. content–tilt coordinate;
5. shortest-path equal-density and nestedness theorem;
6. endpoint/width/tilt derivative theorem;
7. generalized-Bayes fixed-target construction;
8. exact action-separation statement;
9. finite-sample scope and limitations;
10. asymptotic content and width theorem if completed.

## 12.2 Supplement

Place:

- full scan proof;
- critical-value recursion and numerical error proof;
- all endpoint-convention details;
- shortest-path derivative algebra;
- boundary and multimodal cases;
- nonregular mixed-rate theory;
- content influence-function proof;
- asymptotic triangular-array proof;
- local optimization proof and diagnostics;
- posterior action-equivalence proof;
- sandwich estimators;
- complete software-action schema;
- full simulation protocols and Monte Carlo error accounting.

## 12.3 Current manuscript material to retain

Retain and reuse:

- fixed-content interval geometry;
- ordinary mean-preserving RQR;
- MT-RQR loss;
- recovery tilts;
- pseudo-AL/GIG/Gaussian computation;
- root-exchangeability and label cautions;
- fixed-rate restrictions;
- static sandwich distinction;
- frozen-DESN and DLM only if the final paper remains computationally broad.

If tolerance becomes central, compress some dynamic detail into the supplement
to avoid presenting two competing paper identities.

---

# 13. Proposed article reframing

## 13.1 Recommended title

```text
Calibrated Minimum-Width Tolerance Intervals with
Generalized-Bayesian Relaxed Quantile Regression
```

## 13.2 Candidate abstract after Tier-1 proof and software gates

> Tolerance intervals are random bounds designed to contain at least a
> specified population fraction \(c\) with repeated-sample confidence
> \(1-\alpha\). Standard two-sided nonparametric constructions commonly use
> central order statistics and can be unnecessarily wide under skewness. We
> develop a content-defined procedure that first calibrates an enlarged fitted
> content \(q\ge c\) through a distribution-free interval scan statistic and
> then selects the shortest contiguous interval at that calibrated content.
> The selected interval determines a shortest-path mean tilt on the
> mean-tilted relaxed quantile regression (MT-RQR) content–tilt surface. For
> fixed calibrated content and tilt, a generalized posterior based on the RQR
> residual-product check loss admits a pseudo-asymmetric-Laplace
> normal–exponential augmentation with generalized-inverse-Gaussian latent
> scales and Gaussian root updates. Under continuous iid sampling and a valid
> finite-sample scan critical count, the empirical shortest action has the
> requested content-confidence property. Under strict unimodality, the
> population shortest intervals are unique and nested; increasing content
> allocates mass asymmetrically according to endpoint-density geometry, and
> the marginal width cost is the reciprocal endpoint density. We provide
> continuation algorithms across content levels, separate the frequentist
> tolerance action from generalized-posterior endpoint uncertainty, and
> compare the method with exact order-statistic, interpolated
> nonparametric, calibrated-Gibbs, and parametric tolerance procedures.

Do not use the sentence claiming the finite-sample property until T-SCAN-1,
T-SCAN-2, and T-ACTION pass. Before then, replace “has” with “is designed to
have” and label the result as proposed.

## 13.3 Proposed introduction draft

### Paragraph 1: tolerance problem

Tolerance intervals answer a content-confidence question that differs from
parameter confidence, posterior credibility, and predictive coverage. For
specified \(c\in(0,1)\) and \(1-\alpha\), a random interval
\(\widehat T_n=[\widehat L_n,\widehat U_n]\) is required to satisfy

\[
\Pr_F^n\{F(\widehat U_n)-F(\widehat L_n)\ge c\}\ge1-\alpha.
\]

This repeated-sample requirement is central in quality control,
manufacturing, environmental monitoring, and scientific reference-range
construction. It is also demanding outside parametric models because the
reported endpoints are data dependent and must jointly contain enough of an
unknown population.

### Paragraph 2: limitation of central intervals

Classical nonparametric procedures obtain distribution-free validity from
order statistics. Their strength is robustness; their common two-sided form,
however, fixes a central or nearly central allocation of omitted probability.
Content alone does not require such placement. When the population is skewed,
two intervals can contain the same probability while having very different
response-scale widths. Consequently, centrality can impose a substantial
efficiency cost that is unrelated to the tolerance requirement itself.

### Paragraph 3: direct interval estimation

Relaxed quantile regression estimates two interval roots directly by applying
a check loss to the product of their residuals. The sign of this product
records interval membership, avoiding the need to prescribe two endpoint
quantile probabilities. Population analysis shows that ordinary RQR selects a
mean-preserving member of the fixed-content interval class. Mean-tilted RQR
extends this geometry: at fixed content \(q\), the tilt \(\delta\) indexes the
retained mean and hence the placement of a contiguous interval. The
equal-tailed and shortest-contiguous intervals are distribution-specific
members of this content–tilt surface.

### Paragraph 4: central proposal

The central proposal is to calibrate tolerance in the **content coordinate**
rather than by adding a symmetric response-space envelope. Let
\(\widehat I_q^{\rm SH}\) denote the fitted shortest interval at target content
\(q\). The ideal calibrated target is

\[
q^\star
=
\inf\left\{
q\ge c:
\Pr_F^n[
C_F\{\widehat I_q^{\rm SH}\}\ge c
]\ge1-\alpha
\right\}.
\]

The final action is \(\widehat I_{q^\star}^{\rm SH}\). Increasing \(q\)
provides the content buffer needed for tolerance confidence; shortest
placement minimizes width at the chosen content. The buffer is scalar, but
the interval expansion is not symmetric. Under regular unimodality, the
additional probability is allocated between the two tails according to the
local slopes of the density at the shortest-interval endpoints.

### Paragraph 5: scan calibration

For continuous iid univariate data, the required content inflation can be
calibrated without knowing \(F\). On the probability scale, the maximum number
of observations lying in any interval of population content \(c\) has a
distribution-free scan law. Choosing the smallest retained count
\(k_{n,c,\alpha}\) that exceeds this scan count with confidence
\(1-\alpha\), and then reporting the shortest observed interval containing
that count, gives a direct minimum-width content-defined construction. The
uniform scan event remains valid after selecting the shortest placement from
the same data.

### Paragraph 6: generalized-Bayes layer

After the calibrated count, target content, and shortest-path tilt are fixed,
we place a generalized posterior on the interval-root functional. This update
is coherent for the declared loss but does not posit an ordinary response
likelihood. A pseudo-asymmetric-Laplace normal–exponential augmentation yields
generalized-inverse-Gaussian latent scales and conditionally Gaussian root
blocks. A fixed tilt changes only the Gaussian information vectors, so the
existing RQR-GIBBS machinery can be reused with strict target and learning-rate
guards.

### Paragraph 7: path geometry and computation

The shortest solutions over content levels form a structured path under
strict unimodality. The intervals are nested, their endpoint densities agree,
and their width derivative is the reciprocal common endpoint density. These
relations permit predictor–corrector continuation: a previously fitted
minimum brackets and initializes the next one, while adaptive local
optimization and global verification preserve the exact empirical action.
The same path geometry explains why tolerance inflation preserves asymmetric
tail allocation.

### Paragraph 8: contributions

The paper makes the following contributions. First, it formulates
content-defined tolerance as calibrated movement along the MT-RQR
content–tilt surface. Second, it gives a distribution-free scan-calibrated
shortest empirical action for continuous iid data and states precisely the
class in which its finite-sample width optimality holds. Third, it develops
the population shortest-path geometry, including nesting, endpoint movement,
asymmetric allocation of added probability, and marginal width cost. Fourth,
it attaches fixed-target generalized-Bayesian inference while separating the
formal tolerance action from posterior endpoint uncertainty. Fifth, it
develops path-continuation algorithms and a validation program against exact
order-statistic, interpolated nonparametric, calibrated-Gibbs, and parametric
tolerance procedures.

Every contribution sentence must be reconciled with the final proof ledger.
Unproved items should be written as goals rather than completed results.

### Paragraph 9: scope

The finite-sample theorem is initially univariate, iid, continuous, and
response-scale specific. Shortestness refers to one contiguous interval.
Multimodal distributions can produce nonunique or mode-switching shortest
intervals, and disconnected minimum-volume regions are outside the first
method. Regression guarantees are initially marginal or stratum specific
through independently calibrated frozen candidates; one scalar content target
does not provide pointwise conditional tolerance over a continuous covariate
domain. The generalized posterior provides loss-based inference for fixed
targets and should not be interpreted as a response-predictive distribution.

### Paragraph 10: organization

Section 2 defines the tolerance contract and the fixed-content MT-RQR
geometry. Section 3 develops scan-calibrated target content and the formal
shortest empirical action. Section 4 studies shortest-path geometry and
continuation across contents. Section 5 develops generalized-posterior
computation. Section 6 gives finite- and large-sample theory. Section 7
presents simulation comparisons and applications. Section 8 discusses
extensions and limitations.

## 13.4 Recommended main-paper section architecture

```text
1. Introduction
2. Tolerance content-confidence and the MT-RQR content–tilt surface
3. Scan-calibrated shortest empirical tolerance intervals
4. Shortest-path geometry across content levels
5. Generalized-Bayesian inference at the calibrated target
6. Algorithms, local continuation, and action diagnostics
7. Finite-sample and asymptotic theory
8. Simulations and application
9. Discussion
```

The dynamic and DESN material can remain, but if the paper becomes
tolerance-centered it should be compressed into one structured-extension
section or moved mainly to the supplement.

---

# 14. Repository implementation plan

## 14.1 Reuse rather than duplicate

Extend the current:

```r
rqr_mt_tilt_empirical_shortest()
rqr_mt_tilt_cf()
rqr_mt_tilt_screen()
rqr_mcmc_fit()
rqr_root_label_*
```

Do not create parallel shortest-window definitions.

## 14.2 Proposed new R module

```text
application/R/rqr_tolerance_scan.R
```

Suggested functions:

```r
rqr_tcsp_scan_count()
rqr_tcsp_scan_probability()
rqr_tcsp_calibrate_count()
rqr_tcsp_shortest_window()
rqr_tcsp_tilt_from_window()
rqr_tcsp_fit_univariate()
rqr_tcsp_path()
rqr_tcsp_predict_next_start()
rqr_tcsp_local_correct()
rqr_tcsp_validate_action()
```

The first production version can wrap
`rqr_mt_tilt_empirical_shortest()` after validating that its exact action
matches the theorem.

## 14.3 Fit object metadata

Every TCSP fit should store:

```text
schema_version
method
sample_size
guaranteed_content
tolerance_confidence
target_content
content_buffer
retained_count
scan_critical_method
scan_critical_value
scan_numerical_error_control
formal_tolerance_action
interval_endpoint_convention
shortest_window_start
shortest_window_end
tie_count
tie_rule
boundary_status
lower_endpoint
upper_endpoint
width
retained_mean
delta_raw
delta_standardized
learning_rate
learning_rate_mode
prior_type
posterior_summary_action
global_shortest_verified
assumptions_passed
assumptions_failed
finite_sample_claim_available
asymptotic_claim_available
response_scale_description
root_label_contract
provenance_digest
```

## 14.4 Tests

Add:

```text
application/tests/testthat/test-rqr-tcsp-scan-calibration.R
application/tests/testthat/test-rqr-tcsp-shortest-window.R
application/tests/testthat/test-rqr-tcsp-path-continuation.R
application/tests/testthat/test-rqr-tcsp-fixed-target-mcmc.R
application/tests/testthat/test-rqr-tcsp-action-contract.R
application/tests/testthat/test-rqr-tcsp-feasibility.R
application/tests/testthat/test-rqr-tcsp-fail-closed.R
application/tests/testthat/test-rqr-tcsp-ties-atoms.R
```

Essential deterministic tests:

- scan count monotonicity in \(k\);
- shortest width monotonicity in retained count;
- exact agreement with brute-force window scanning;
- exact agreement between reported tilt and retained mean minus full mean;
- zero-tilt backward compatibility;
- fixed-rate and prior gates;
- whole-root swaps;
- action serialization round trip;
- trust-region expansion;
- local result versus global result;
- deterministic tie handling;
- infeasibility;
- response-scale metadata.

## 14.5 Simulation scripts

Add a reproducible repeated-DGP pipeline distinct from single-data oracle
illustrations. It should compare, at minimum:

- scan-calibrated TCSP;
- exact Wilks/order-statistic intervals;
- Young–Mathew interpolation;
- calibrated Gibbs tolerance intervals;
- correctly specified parametric intervals when available;
- equal-tailed intervals at the same calibrated total content.

Primary metrics:

\[
\widehat{\Pr}\{C_F(\widehat T)\ge c\},
\]

one-sided Monte Carlo lower confidence bound, mean/median width, width
quantiles, content distribution, lower and upper omitted mass, failure
severity, infeasibility, runtime, and optimization diagnostics.

The exploratory simulations already produced in the ChatGPT work should be
reimplemented in the repository before entering the article.

---

# 15. Complete Codex prompt

Copy the following prompt into a new Codex chat opened inside the current
`RQR-GIBBS` repository.

```text
# Codex task: integrate the authoritative scan-calibrated TCSP-MT-RQR
# method and prepare a tolerance-centered manuscript revision

You are working inside the current local clone of `AntonioAPDL/RQR-GIBBS`.
Treat this as a statistical-method, proof-action, manuscript-reframing, and
research-software integration task. Do not assume the local HEAD matches any
July 2026 handoff baseline. Do not merge to `main`.

The controlling design report is:

TCSP_MT_RQR_AUTHORITATIVE_METHOD_AND_MANUSCRIPT_INTEGRATION_REPORT_20260811.md

Locate that file at the path supplied in this chat, copy it into an appropriate
tracked documentation directory, and read it completely before editing.

========================================================================
1. AUDIT THE CURRENT REPOSITORY
========================================================================

Read all applicable `AGENTS.md` files and then read, at minimum:

AGENTS.md
STYLE_PROFILE.md
README.md
Makefile
main.tex
rqr-gibbs-supplement.tex
refs.bib
application/DESCRIPTION
application/NAMESPACE
application/R/rqr_mean_tilt_init.R
application/R/rqr_mcmc_fit.R
application/R/rqr_root_labels.R
application/R/rqr_utils.R
application/R/rqr_numerics.R
application/R/rqr_dlm_fit.R
application/tests/testthat/test-rqr-native-mean-tilt.R

Search the repository for all current mean-tilt, shortest-window, tolerance,
scan-statistic, DKW, Kuiper, Wilks, Young–Mathew, calibrated-Gibbs, content,
coverage, and action-contract code and documentation.

Record before any edit:

git status --short --branch
git rev-parse HEAD
git log -1 --oneline --decorate
git remote -v

Require a clean worktree. Do not stash, reset, clean, overwrite, or discard
unrelated work.

The public repository had 328 commits when the design report was prepared,
but that is not a commit identifier. The local hash is authoritative.

========================================================================
2. CREATE A DEDICATED BRANCH AND PROTECT THE CURRENT MANUSCRIPT
========================================================================

Create a branch such as:

feature/scan-tcsp-mt-rqr-manuscript-20260811

Before editing, store hashes of:

main.tex
rqr-gibbs-supplement.tex
refs.bib
README.md

under a local ignored audit directory.

Do not merge to main. Do not push if authentication is unavailable; report
that fact exactly.

========================================================================
3. AUTHORITATIVE METHOD CONTRACT
========================================================================

Use these distinct quantities:

c             = guaranteed population content
1-alpha       = repeated-sample tolerance confidence
k             = scan-calibrated retained observation count
q = k/n       = fitted target content
t = q-c       = content buffer
delta         = fixed MT-RQR placement/retained-mean tilt
omega         = fixed generalized-Bayes learning rate

The formal univariate tolerance action must be exactly one canonical closed
k-observation window:

[Y_(j), Y_(j+k-1)],  j=1,...,n-k+1.

Use the deterministic first global minimum under the repository numerical tie
rule.

Define the distribution-free scan count by the Uniform(0,1) scan statistic:

M_n(c) = maximum number of sample points in any probability interval
         having population content at most c,

k_(n,c,alpha) = smallest k such that
                Pr{M_n(c) < k} >= 1-alpha.

The formal action is the shortest empirical interval containing this k.

Do not mix this closed-window convention with the earlier half-open
(Y_(j),Y_(j+k)] convention. If existing theory or code uses the latter, write
an explicit reconciliation note and update all theorem, software, metadata,
and test contracts consistently before making a claim.

The empirical shortest-path tilt is:

delta_hat_SH(q)
  = mean{Y_(j),...,Y_(j+k-1)} - mean(Y).

The generalized posterior is fitted only after q and delta are frozen.

The tolerance certificate comes from scan calibration, not from omega, the
posterior credible level, the Cornish–Fisher approximation, local
optimization, or the Gibbs sampler.

========================================================================
4. CURRENT CODE TO REUSE
========================================================================

Audit and reuse:

rqr_mt_tilt_empirical_shortest()
rqr_mt_tilt_cf()
rqr_mt_tilt_screen()
rqr_mt_select_tilt_candidate()
rqr_mcmc_fit()
the complete-root label/swap functions

Do not duplicate the shortest-window engine if it can be safely generalized.

The current empirical-shortest function already scans closed windows with
ceiling(q*n), selects the first minimum, and returns retained-mean tilt
metadata. Preserve its behavior unless a documented theorem/action correction
requires a reviewed API change.

The current candidate selector's point guard and Bonferroni Normal lower bound
are not the authoritative finite-sample scan certificate. Keep them
exploratory, rename their status if needed, or add an explicit warning. Do not
silently promote them.

Nonzero tilt remains restricted to:

learning_rate_mode = "fixed_rate"
proper Gaussian/ridge roots
supported fixed-design/frozen-DESN paths
DLM fixed-W or pre-frozen discount template

Reject nonzero-tilt learned-rate, RHS-NS, adaptive/component-scale dynamic,
VB/CAVI, and unsupported replica-exchange modes.

========================================================================
5. IMPLEMENT THE NEW CALIBRATION AND PATH LAYER
========================================================================

Add a coherent module, preferably:

application/R/rqr_tolerance_scan.R

with public APIs equivalent to:

rqr_tcsp_scan_count()
rqr_tcsp_scan_probability()
rqr_tcsp_calibrate_count()
rqr_tcsp_shortest_window()
rqr_tcsp_tilt_from_window()
rqr_tcsp_fit_univariate()
rqr_tcsp_path()
rqr_tcsp_predict_next_start()
rqr_tcsp_local_correct()
rqr_tcsp_validate_action()

First search for current equivalents and avoid collisions.

Critical-value modes:

1. exact recursion, if a verified implementation is available;
2. rigorously conservative Uniform Monte Carlo with a declared one-sided
   confidence bound for numerical calibration error;
3. verified Kuiper or DKW fallback;
4. asymptotic q only as initialization/diagnostic.

Never label Monte Carlo calibration "exact."

Implement fail-closed infeasibility.

For path tracing across ordered contents:

- solve the first content globally;
- predict each next window by nested left/right expansion;
- optionally use derivative or constrained interpolation predictors;
- use adaptive local correction;
- expand the trust region when a local optimum lands at its boundary;
- globally verify every formal univariate tolerance action;
- store local regret and mode-switch diagnostics.

Use all path identities in the design report, but do not turn an unproved
formula into a manuscript theorem.

========================================================================
6. THEORY WORKSPACE AND CLAIM LEDGER
========================================================================

Create or reconcile a tracked theory workspace under an appropriate `docs/`
path. Do not blindly apply an old overlay to an advanced repository.

Add a proof ledger covering at least:

T-SCAN-1 finite-sample scan theorem
T-SCAN-2 critical-count numerical certification
T-ACTION action matching
T-SH-1 shortest-path existence/uniqueness/nesting
T-SH-2 endpoint/width/tilt derivatives
T-OPT-1 exact scan-class width optimality
T-MIXED shortest-location mixed rates
T-CONTENT-IF content influence function
T-ORTH shortest-selection content orthogonality
T-QASY asymptotic content target
T-WIDTH first-order width price
T-OPT-2 first-order MT-RQR optimality
T-LOCAL continuation correctness
T-GB-ACTION posterior action equivalence
T-SANDWICH endpoint uncertainty
T-REG finite-family marginal regression tolerance

Allowed statuses must distinguish:

PROVED-AND-AUDITED
DERIVED-PENDING
IMPLEMENTED-AUDIT-PENDING
BLOCKING
LATER
REJECTED

Do not mark a theorem proved because tests pass.

========================================================================
7. MANUSCRIPT REFRAMING
========================================================================

This is a branch-level candidate revision. Update the manuscript only after
the repository audit is complete.

Preferred title:

Calibrated Minimum-Width Tolerance Intervals with
Generalized-Bayesian Relaxed Quantile Regression

Update consistently:

- `\title{}`;
- PDF metadata `pdftitle`;
- supplement title and PDF metadata;
- README manuscript description;
- arXiv checklist or source-package description if title metadata is copied
  there.

Use the candidate abstract and introduction in the controlling report as the
starting text. Reconcile them with the current article rather than replacing
valid current theory mechanically.

Add a new main section titled approximately:

Scan-Calibrated Minimum-Width Tolerance Intervals

It must include:

1. the content-confidence definition;
2. explicit separation of c, alpha, q, delta, and omega;
3. the scan statistic and calibrated count;
4. the canonical shortest k-observation action;
5. the shortest-path tilt estimate;
6. exact action versus posterior-summary distinction;
7. the sequential/local path algorithm;
8. crucial relationships across contents:
   - equal endpoint density;
   - nestedness;
   - endpoint derivatives;
   - asymmetric left/right mass shares;
   - W_SH'(q)=1/lambda_q;
   - convexity of width;
   - tilt derivative;
9. failure modes and response-scale dependence;
10. current theorem statuses.

Do not state a finite-sample theorem as completed unless its proof ledger
dependencies and critical-value implementation are actually complete and
independently audited.

If Tier-1 theory is not complete, write the section as a clearly labeled
proposed authoritative extension, retain conditional wording in the abstract,
and add visible `% TODO-TCSP-PROOF-GATE` comments at every claim that requires
promotion.

Do not hide pending status in prose.

Reorganize the article so the story is coherent:

1. tolerance problem;
2. fixed-content and content–tilt geometry;
3. scan-calibrated shortest action;
4. generalized-Bayes fixed-target inference;
5. computation;
6. theory;
7. validation;
8. structured extensions.

If the revised article becomes too broad, move detailed DESN/DLM derivations
to the supplement while retaining one concise structured-extension section.

========================================================================
8. REFERENCES
========================================================================

Audit `refs.bib` and add only verified bibliographic entries needed for:

- Wilks tolerance intervals;
- Young–Mathew interpolation;
- smallest nonparametric tolerance regions;
- scan statistics;
- shorth/shortest empirical intervals;
- minimum-volume/highest-density sets;
- calibrated Bayesian nonparametric tolerance intervals;
- generalized Bayes;
- RQR;
- proper scoring and sharpness subject to calibration;
- AL/GIG augmentation.

Do not cite secondary web summaries when a primary source is available.

========================================================================
9. FIGURES AND TABLES
========================================================================

Add source-controlled figure/table generators for:

- the tolerance workflow;
- width versus tilt over several contents;
- shortest-path minima over content;
- local continuation versus global minima;
- scan-calibrated q versus c;
- tolerance success versus width for benchmark methods;
- asymmetric omitted-tail allocation.

Do not reuse ChatGPT-generated numerical outputs as manuscript evidence unless
they are reimplemented from a tracked script and a frozen configuration.

All main figures must be vector or publication-quality, grayscale accessible,
and use action-specific labels.

========================================================================
10. TESTS AND VALIDATION
========================================================================

Add tests equivalent to:

test-rqr-tcsp-scan-calibration.R
test-rqr-tcsp-shortest-window.R
test-rqr-tcsp-path-continuation.R
test-rqr-tcsp-fixed-target-mcmc.R
test-rqr-tcsp-action-contract.R
test-rqr-tcsp-feasibility.R
test-rqr-tcsp-fail-closed.R
test-rqr-tcsp-ties-atoms.R

Run all repository-required gates, including at minimum:

make smoke
make test-theory-figures
make test-theory-tables
make pdf
make supplement
make package-install
make test-native
make test-native-mean-tilt
make test-standalone-contracts

Run `R CMD build` and `R CMD check` if required by repository policy.

Run:

git diff --check
git status --short
git diff --stat
git diff --name-status

Search every new or modified file for prohibited wording:

posterior predictive tolerance interval
response likelihood
posterior credibility equals tolerance confidence
omega as tolerance factor
globally shortest
minimum expected width
pointwise conditional tolerance
posterior mean inherits exact scan validity
transformation-invariant shortest interval

Warnings and explicit negations are allowed; unsupported affirmative claims
are not.

========================================================================
11. REQUIRED FINAL CODEX RESPONSE
========================================================================

Return one complete implementation report containing:

- starting and ending commit hashes;
- branch name;
- push/PR status;
- exact files added and changed;
- the canonical interval action selected;
- critical-value method and its numerical error control;
- current proof-ledger status for every load-bearing claim;
- exact manuscript title used;
- summary of title/abstract/introduction changes;
- exact new section labels;
- code API and metadata contract;
- all test/build/check results;
- current unsupported branches and fail-closed behavior;
- any unresolved action mismatch;
- any theorem not promoted;
- a concise diff review suitable for a statistical coauthor.

Do not merge. Do not claim a theorem, validation result, or command was
completed unless it actually was.
```

---

# 16. Final recommendation

The best coherent framework is:

\[
\boxed{
\begin{aligned}
&\textbf{Validity:}
&&\text{distribution-free scan calibration of }k\text{ and }q=k/n,\\
&\textbf{Sharpness:}
&&\text{shortest contiguous empirical interval at calibrated }q,\\
&\textbf{Asymmetry:}
&&\text{shortest-path MT-RQR tilt }\delta_{\rm SH}(q),\\
&\textbf{Inference:}
&&\text{fixed-target generalized posterior with separate endpoint UQ},\\
&\textbf{Computation:}
&&\text{continuation and local correction with global action verification}.
\end{aligned}
}
\]

This is more integrated and more faithful to the RQR geometry than a symmetric
post hoc envelope. It also gives the cleanest path to a strong statistical
claim:

> Under continuous iid sampling and a valid scan critical count, the canonical
> shortest calibrated empirical interval has the required content-confidence;
> under regular strict unimodality, its population target follows a unique
> nested shortest path, and the generalized posterior supplies separate
> fixed-target uncertainty.

The title should be changed only on a dedicated branch and promoted only when
the finite-sample scan theorem, numerical critical engine, exact action
contract, shortest-path theory, and software validation have passed. Until
then, the report and manuscript should describe TCSP as the authoritative
planned extension rather than as a completed theorem-backed method.

---

# 17. Source basis

The report is based on:

- the live public `RQR-GIBBS` repository and its current `README.md`,
  `main.tex`, supplement, mean-tilt implementation, MCMC implementation,
  root-label implementation, tests, and bibliography as inspected on
  2026-08-11;
- the current project theory manuscript and final theory-audit materials;
- the TCSP-MT-RQR roadmap and claims registry;
- Pouplin et al., *Relaxed Quantile Regression: Prediction Intervals for
  Asymmetric Noise*;
- Bissiri, Holmes, and Walker, *A General Framework for Updating Belief
  Distributions*;
- Gneiting and Raftery, *Strictly Proper Scoring Rules, Prediction, and
  Estimation*;
- Kozumi and Kobayashi, *Gibbs Sampling Methods for Bayesian Quantile
  Regression*;
- Pourmohamad, Richardson, and Sansó, *Calibrated Bayesian Nonparametric
  Tolerance Intervals*.

The exploratory simulation results discussed in the prior ChatGPT
conversation are treated as design evidence only. They must be reproduced in
the tracked repository before being used as article evidence.
