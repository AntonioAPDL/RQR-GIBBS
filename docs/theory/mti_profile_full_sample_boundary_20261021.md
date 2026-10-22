# A Full-Sample Boundary of the Penalized MTI Criterion

This note concerns an optional future construction. It does not change the
current MTI procedure, calibration, article, or reported comparison.

## Containment Result

Consider the intercept criterion in the supplement with fixed fitted content
`q`, fixed tilt, learning rate `omega > 0`, and equal positive Gaussian root
precision `lambda`. The prior means are common and fixed. Write
`eta = lambda/omega` and `s = n*q - eta`.

For a fixed midpoint, the profiled squared radius is an empirical `s/n`
quantile of squared distances. If

```text
n - 1 < s < n,
```

the minimizing radius is the largest observed distance. Every fitted interval
therefore contains the entire training sample. This conclusion holds at the
global minimizing midpoint and for every fixed finite tilt; it does not rely
on an empirical convergence flag.

When `0 < eta < 1`, the admissible fitted-content range is nonempty:

```text
(n - 1 + eta)/n < q < 1.
```

Its midpoint, `q = 1 - (1-eta)/(2*n)`, is one explicit interior choice. This
choice follows from the containment condition; it is not a guessed desired
population coverage. The strict inequality matters: at `s = n-1`, a smaller
radius can minimize the profiled criterion.

## Frequentist Consequence

For continuous iid observations from `F`, the content of the sample range
`[Y_(1),Y_(n)]` has distribution `Beta(n-1,2)`. Consequently every interval
containing that range satisfies

```text
Pr_F{F(upper)-F(lower) >= c}
  >= Pr{Beta(n-1,2) >= c}
   = 1 - n*c^(n-1) + (n-1)*c^n.
```

Thus the desired statement follows whenever the right-hand side is at least
`p`. This is an inherited sample-range result under its stated assumptions.
It does not assert a new distribution-free theorem for the current
multi-content MTI selection rule, whose candidates need not satisfy `s>n-1`.

If an additional direct-DP screen is retained, nonreturns must still be
addressed. With concentration `a`, an interval containing every observation
has posterior mass stochastically at least `Beta(n,a)`. A DP threshold no
larger than `Pr{Beta(n,a)>=c}` therefore cannot reject it. For `a=1`, this
lower probability is `1-c^n`. This is a separate model-based content
calculation; the frequentist conclusion above follows from sample-range
containment.

## Width and Interpretation

Containment implies width at least `Y_(n)-Y_(1)`. This route cannot yield a
width improvement over Wilks' sample-range interval in the same sample.
It can provide a genuine MTI minimization with a finite-sample bound in a
prespecified boundary regime, but offers no new width advantage there.
It should not be presented as a novel improvement over Wilks or silently
inserted as a replacement for the currently calibrated method.

The equality case can be constructed explicitly. Put `a=min(y)`, `b=max(y)`
and choose weights one on the interior observations, with total weight
`s-(n-2)` split between two extreme observations. For common prior mean `mu0`,
set

```text
delta = {sum(w_i*y_i) + eta*(a+b-mu0)}/(n*q) - mean(y).
```

The weights satisfy the radius condition and the midpoint subgradient
equation. Strong convexity of the midpoint profile then identifies the
sample-range endpoints as a global ordered solution. Tied observations
allow compatible fractional allocations at the same endpoint values.

This explains a limitation of interpreting target corrections as a width
improvement: a correctly optimized MTI target can recover an existing
order-statistic interval exactly.

## Reproducible Check

The existing API can check the equality construction directly:

```r
set.seed(20261021)
y <- rexp(50)
n <- length(y)
lambda <- 1e-4
omega <- 1
mu0 <- 0
eta <- lambda/omega
q <- 1-(1-eta)/(2*n)
s <- n*q-eta
a <- min(y)
b <- max(y)
w <- rep(1,n)
w[c(which.min(y),which.max(y))] <- 1-(n-s)/2
delta <- (sum(w*y)+eta*(a+b-mu0))/(n*q)-mean(y)
fit <- rqrgibbs::rqr_mti_intercept_fit(y,q,delta,
  learning_rate=omega,prior_precision=lambda,prior_mean=mu0)
stopifnot(fit$converged,
  max(abs(c(fit$lower,fit$upper)-c(a,b)))<1e-8)
```

The same check was made for 24 combinations of tied, constant, exponential,
and Gaussian samples, three precisions, and two common prior means. All
passed the direct objective-bound check. An additional 72 arbitrary-tilt
fits contained all observations, and direct-DP probabilities satisfied the
stated lower bound in the checked cases. No simulation policy was changed.
