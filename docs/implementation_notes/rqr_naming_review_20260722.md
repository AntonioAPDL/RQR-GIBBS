# Naming review for the RQR model family

Date: 2026-07-22

## Recommendation

Do not rename the software or manuscript family before the mathematical review
of the population target is complete. For the present article, retain **RQR**
for continuity with Pouplin et al. and add the factual descriptor
**coverage-targeted interval-root regression**.

If a full rename is later warranted, the strongest candidate is:

**Coverage-Targeted Interval Regression (CTIR)**

Suggested variants are **CTIR-DESN** and **CTIR-DLM**. The longer phrase
**coverage-targeted interval-root regression** should appear at first use so
readers know that the primitive objects are two exchangeable roots ordered
after fitting.

The word **targeted** is essential. It states the objective without promising
finite-sample, distribution-free, or pointwise conditional coverage.

## Why not the shorter alternatives?

### Coverage regression

This is catchy but too strong and too broad. Under an unrestricted pointwise
population minimizer, the RQR first-order conditions imply a coverage equation
under regularity assumptions. Under a restricted linear, DESN-readout, or DLM
class, the estimating equations are projections and need not provide
conditional coverage at every covariate value. The name also risks confusion
with conformal and calibration methods whose central selling point is a formal
coverage guarantee.

Search results also use “coverage regression” descriptively in unrelated
fields, which weakens discoverability.

### Direct coverage regression

“Direct” correctly distinguishes the method from pairing preassigned quantile
levels, but “coverage” still sounds guaranteed. It also hides the first-moment
balance component of the unrestricted target.

### Interval regression

Avoid this name. In established econometric and statistical software, interval
regression means regression for an interval-censored response. The official
Stata documentation places it under censored regression and describes observed
outcomes known only through intervals:
https://www.stata.com/features/overview/multilevel-interval-regression/

Using the same term for direct prediction-interval roots would create immediate
search and reviewer confusion.

### Direct interval regression

This is clearer than “interval regression,” but the collision remains, and
“direct interval prediction” is already a broad methodological category in the
prediction-interval literature. The review by Dewolf, De Baets, and Waegeman
explicitly distinguishes direct interval estimation from Bayesian, ensemble,
and conformal methods:
https://arxiv.org/abs/2107.00363

It is therefore a good descriptor, not a distinctive model-family name.

### Interval-root regression

This is the most mechanically faithful option. It emphasizes exchangeable
roots and does not claim calibration. Its weakness is that it does not explain
what selects the roots or why the method is scientifically interesting.
**Coverage-targeted interval-root regression** fixes that weakness.

### Coverage-balanced interval regression

This reflects the coverage and conditional first-moment balance equations and
has the attractive acronym CBIR. However, “balanced” is not standard, needs a
derivation before it is intelligible, and inherits the same unrestricted-versus-
restricted qualification. It is better used as a description of the
population characterization than as the primary name.

### Dynamic coverage linear model or coverage-DLM

Avoid these as primary names. They can be read as dynamic models that guarantee
coverage. They also obscure that the state evolution acts on interval roots and
that the response contribution remains a generalized-Bayes loss. **CTIR-DLM**
is safer because the target qualifier is retained.

### C-DESN

This is short but not self-explanatory; C could mean calibrated, censored,
conformal, classification, or coverage. **RQR-DESN** is more discoverable now,
and **CTIR-DESN** would be clearer after a deliberate rename.

## Candidate ranking

1. **Coverage-Targeted Interval Regression (CTIR)**: best balance of fidelity,
   appeal, and restraint.
2. **Interval-Root Regression (IRR)**: most conservative factual rename, but
   less distinctive.
3. **Coverage-Balanced Interval Regression (CBIR)**: memorable, but depends too
   heavily on a qualified population characterization.
4. **Direct Interval Regression (DIR)**: understandable but collides with
   interval-censoring and broad direct-interval literature.
5. **Coverage Regression (CR)**: catchy but most likely to overclaim and to
   collide with unrelated usage.

## Publication transition strategy

For the present development cycle:

1. keep repository and function names under RQR;
2. use the phrase “coverage-targeted interval-root learning under the RQR
   loss” in the abstract and introduction;
3. ask external reviewers to audit the unrestricted and restricted target
   distinction;
4. complete simulation and calibration evidence;
5. decide on a rename before public API stabilization, not after users depend
   on the names.

A possible future title is:

**Bayesian Coverage-Targeted Interval Regression via the Relaxed Quantile
Loss**

This preserves the citation trail to the original method while foregrounding
the actual interval target. Until the external audit is complete, the current
title remains the safer choice.
