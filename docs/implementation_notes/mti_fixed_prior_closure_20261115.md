# Numerical Completion of the Fixed-Prior ECM/Gibbs Study

## Statistical Specification

This continuation preserves the fixed-prior specification of
`mti_fixed_prior_20261114.json`. The two raw interval roots have independent
Normal(0,2^2) priors in declared reporting units. The learning rate is one.
The prior mean, variance, and learning rate are specified before fitting and
are not estimated from the analysis sample or held-out observations. The
DP base and design-dependent concentration also remain unchanged.

In simulations the reporting coordinates are the predeclared centered,
unit-variance study coordinates. The application uses the numerical units
of the public file. Its prior is a working regularization assumption, not
independently elicited manufacturing information. The preceding
[fixed-prior specification](mti_fixed_prior_validation_20261114.md) gives
the prior-width interpretation and response-unit transformations.

For fixed fitted content q and tilt delta, the Gaussian prior makes the
loss-based generalized posterior proper: a positive quadratic in the roots
dominates the tilted loss's possible negative linear directions. Both ECM
and Gibbs use this same target. ECM provides finite numerical mode estimates;
the marginal slice-within-Gibbs sampler estimates posterior uncertainty with
the latent augmentation variables integrated out. The conditionally
Gaussian/GIG derivation remains applicable to the same root posterior.

Fitted content and tilt are selected from training observations, then held
fixed for Gibbs sampling. This is selected-target generalized Bayes, not a
joint posterior integrating over q and delta. Ordered-root posterior means
are interval-functional summaries, not posterior predictive responses.
Proper priors and accurate MCMC do not establish frequentist tolerance
validity for every reported endpoint summary.

## Numerical Correction

The original independent validation identified an unresolved Gamma profile
candidate whose lower root was about 5.6e-14 from an observed value. Its
objective gap was zero, but the closed-interval count changed when the root
was represented by the exact observed knot. The conditional-mode refinement
could end without repeating the existing guarded knot-representation check.

The correction repeats that check only when final conditional-mode accuracy
is unresolved. The proposed knot must be the analytical conditional mode,
must be within the existing numerical proximity bound, and must pass the
stable objective-increment guard. Accuracy and closed-count stability are
then reassessed using unchanged tolerances. The correction neither clips
endpoints to sample extremes nor introduces a fallback interval.

Already accepted candidates take exactly their original computational path.
The targeted regression test reproduces the original sample and the original
binary q value; using only its rounded decimal label can miss the numerical
case. The genuine full-profile check verifies identical previously accurate
candidates, unchanged sample and prior hashes, and the unchanged target grid.
It also verifies that the original failed file remains untouched.
The check command additionally fits one original application partition for
each response and verifies its fixed prior and numerical accuracy.

## Preserving the Original Study

The original simulation source is
`9b53485fc9e4d6ab43a92d80a1b0931bf58c910e`. Its detached worktree, fits,
receipts, and failure-inclusive summaries are not modified. The new
controller waits for its completion marker; it does not restart validation.

Only failed numerical profiles are eligible for repair. A repaired result
must use the original task, observations, prior, learning rate, DP prior,
target menu, and sampling seeds. The program rejects replacement of an
accepted fit, changed observations or priors, a changed target grid, or
changes to previously accurate candidates. Repaired fits and their receipts
are saved separately. `repair_provenance.csv` records both source commits and
the original and repaired object hashes.

Combined validation tables preserve all observations and replace only the
failed numerical outputs with their verified repairs. They remain explicitly
attributable to the original and corrected numerical sources. Original
failures remain available for the failure-inclusive assessment. No fitted
objects are copied into version control.

## Application and Focused Diagnostics

After numerical completion, the continuation fits all 3,004 application
pairs using the original partitions and fixed prior. At most 40 workers fit
one pair each, with four serial Gibbs chains and one numerical-library
thread. Held-out responses are used only for evaluation. Summaries separate
responses and partition types, retain raw 2.5th--97.5th width percentiles,
and describe held-out empirical content. Overlapping partitions are not
treated as independent binomial tolerance-confidence observations.
Every attempted pair remains in the summary denominator, including failed
fits with missing endpoints; their reasons are saved separately.

The focused Student-t diagnostic concerns n=500,c=0.99. Up to four saved
samples are chosen reproducibly from each of three patterns: ECM alone
attains content, both attain content, and neither attains content. These
outcome-stratified cases are for numerical diagnosis, not unbiased
performance estimation or prior selection.

Each diagnostic repeats the same fixed target and prior with four fresh
chains, 500 burn-in cycles, and 8,000 retained draws per chain. Mean changes
are compared using the original and new Monte Carlo errors. Saved outputs
include ordered-root, width, and objective traces, interval nesting,
population content, and the interval's own DP probability. Population truth
is evaluated only after sampling. Neither priors nor target menus are tuned
using these results. Full draws are retained locally; the PDF thins displayed
iterations only to keep traces legible.

The ECM interval's DP probability requirement does not automatically hold
for posterior-mean endpoints. A persistent Gibbs-mean attainment deficit
must limit claims for that reported summary even when the posterior
calculation is accurate. ECM tolerance performance and Gibbs conditional
uncertainty remain separate reporting questions.

## Commands and Outputs

From the current implementation checkout:

```bash
make test-mti-fixed-prior
make test-mti-fixed-prior-closure
make prepare-mti-fixed-prior-closure
make check-mti-fixed-prior-closure
make launch-mti-fixed-prior-closure
```

The launcher creates an exact-commit detached continuation and a private
copy of the original study's installed packages. It verifies installed-file
hashes, R and dependency versions, fixed statistical settings, and generator
identity before proceeding. It records the existing exdqlm reference pin;
it does not compile, install, load, or modify an external source checkout.

Use `make health-mti-fixed-prior-closure` in the frozen continuation worktree
for an on-request check. Its local output directory is
`application/outputs/mti_fixed_prior_closure_20261115/`:

- `status.json`, `manifest.json`, and `parent_completion.json`: current
  phase and identities of both numerical sources.
- `repairs/` and `repair_provenance.csv`: immutable repair evidence.
- `validation/`: combined cell-level results and `study_review.pdf`.
- `application/`: all fits, descriptive summaries, and `application_review.pdf`.
- `diagnostics/`: selected-case identities, long-chain fits, mean/MCSE
  comparisons, case summaries, and `fixed_target_traces.pdf`.
- `assessment.md`: completed numerical and statistical assessment for review.

Saved jobs are reused only after receipt, hash, and task checks. An interrupted
save without a valid receipt is recomputed. A statistical or numerical
problem is reported with evidence retained; failures are not silently
resampled away. The application and diagnostic stages run sequentially so
their worker budgets do not compete. A 16-GiB free-space reserve is checked
before each batch, with an application storage estimate before launch.

No manuscript number, figure, prior specification, or conclusion is replaced
automatically. Full case-specific evidence, including the Student-t warning,
must be interpreted before promotion. Subsequent prose follows
`STYLE_PROFILE.md` and retains the distinction between generalized posterior
uncertainty and repeated-sampling tolerance performance.
