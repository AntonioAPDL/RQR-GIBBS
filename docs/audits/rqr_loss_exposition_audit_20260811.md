# RQR Loss and Abstract Exposition Audit

Date: 2026-08-11

## Scope

This audit covers the abstract, the first presentation of the ordinary relaxed
quantile regression (RQR) loss in `main.tex`, its self-contained derivation in
`rqr-gibbs-supplement.tex`, and the transition from the loss to the static
generalized posterior. It changes exposition and the placement of an existing
figure only. It does not change the population target, Gibbs updates,
variational approximations, software API, figure content, tables, or empirical
results.

## Finding

The preceding manuscript defined the scalar product residual and its check
loss before displaying the observation-level interval loss. That order was
algebraically correct, but it made the central statistical object harder to
recognize. In particular, a reader could initially mistake the check-loss
index for an endpoint quantile level or conflate the loss index with the
generalized-Bayes learning rate.

The preceding abstract was accurate but read partly as an implementation
inventory. Its list of deferred extensions obscured the paper's central
sequence: interval nonidentification, population characterization, mean tilt,
generalized-posterior computation, structured predictors, and the inferential
boundary. It was therefore shortened and reorganized around that sequence.

## Abstract contract

The revised abstract:

- opens with the interval-placement problem rather than computation;
- defines RQR through a nonnegative endpoint-product loss;
- states the mean-preserving population target and the role of fixed mean tilt;
- summarizes the pseudo-AL augmentation and the resulting full conditionals;
- identifies regularized static, frozen-feature DESN, and dynamic root models
  as restrictions on a common computational framework;
- explains why the dynamic sampler alternates root-specific FFBS updates;
- describes the evidence as population diagnostics and single-data
  illustrations rather than comparative validation;
- states the current nonzero-tilt implementation boundary compactly; and
- closes by distinguishing interval-functional generalized posteriors from
  posteriors derived from response likelihoods and from posterior predictive
  response laws.

## Adopted presentation contract

The revised exposition uses the following order.

1. Fix the target interval content `c` and ordered endpoints `a < b`.
2. Define the nonnegative pointwise loss directly, distinguishing observations
   inside and outside the interval.
3. Explain that the loss is distance-sensitive rather than a binary coverage
   penalty, and that `c` determines the relative penalty assigned to misses.
4. Define the scalar check loss and product residual only afterward.
5. Establish the exact identity
   `ell_c(a,b;y) = rho_c((y-a)(y-b))`.
6. State explicitly that the check loss acts on the product residual, not on
   either response residual separately.
7. Distinguish the three roles of notation: `c` is target interval content,
   endpoint quantile levels are not assigned, and `omega_R` is the separate
   generalized-Bayes learning rate.
8. Introduce midpoint and half-width geometry before the population score
   equations.
9. Write the generalized posterior as a prior updated by cumulative interval
   loss, while retaining the explicit statement that it is not derived from a
   response likelihood.

The Figure 1 float is flushed before the loss section so that the piecewise
definition, check-loss representation, and midpoint geometry appear as one
uninterrupted argument in the rendered article.

## Algebraic verification

Let `e=(y-a)(y-b)`. For `a<y<b`, `e<0`, so

```text
rho_c(e) = e(c-1) = (1-c)(y-a)(b-y).
```

For `y<=a` or `y>=b`, `e>=0`, so

```text
rho_c(e) = c e = c(y-a)(y-b).
```

The two formulas agree at either root, where the loss is zero. The product
representation is invariant to exchanging the roots; ordering is required
only to name lower and upper endpoints. Differentiating away from the roots
gives the score multiplier `c-I(a<Y<b)`, from which the established population
content and retained-mean equations follow.

## Bayesian interpretation

The observation-level loss defines the interval functional through expected
risk. The generalized posterior subsequently updates the prior by the
exponential of minus the cumulative loss, scaled by `omega_R`. The pseudo-AL
identity is an augmentation of that generalized posterior. It is not an
ordinary response likelihood, and interval-root draws are not posterior
predictive response draws.

## Validation record

| Check | Result |
|---|---|
| Abstract length after TeX stripping | 219 words |
| Bayesian-language guard | Pass |
| Native mean-tilt tests | Pass; one declared working-mode warning and one unavailable-external-shell skip |
| Pinned exdqlm RQR smoke suite | Pass from isolated Git-archive runtime at `dffb71ee70b597d6a716ee74be1cbc99731cd453`; protected checkout unchanged |
| Deterministic theory-figure tests | Pass |
| Deterministic theory-table tests | Pass |
| Main article | Pass, 24 pages, no unresolved references or layout warnings |
| Supplement | Pass, 29 pages, no unresolved references or layout warnings |
| Visual inspection | Pass for abstract, Figure 1 boundary, loss definition, check-loss representation, and supplement derivation |
| arXiv source package | Pass; 14 files and clean ZIP integrity test |

The native warning states that adaptive discounting remains an experimental
working recursion rather than an exact fixed-joint Gibbs mode. The skipped
native DESN shell requires an external namespace; its pinned isolated-runtime
counterpart passed in the exdqlm smoke suite. Neither item is caused by the
expository changes.
