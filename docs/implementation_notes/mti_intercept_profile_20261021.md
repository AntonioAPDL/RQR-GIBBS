# Profiled MTI Intercept Estimates

## Statistical Purpose

This revision separates fixed-target minimization, empirical target
construction, and repeated-sampling tolerance assessment. The existing ECM
routine and all historical results remain available without numerical changes.
The article now describes those finite-iteration estimates without claiming
that they attain a generalized-posterior mode.

For fixed fitted content `q`, mean tilt `delta`, rate `omega`, and equal
positive Gaussian root precision `lambda`, the new intercept solver minimizes
the original penalized endpoint criterion. The common Gaussian mean is fixed;
its default is zero. Minimization over squared half-width leaves a strongly
convex midpoint criterion. The squared radius is an empirical quantile of
squared distances with mass `n*q - lambda/omega`. A finite bracket follows
from the extreme fractional retained sums. Monotone subgradient bisection
finds the minimizing midpoint. The supplement gives the zero-mean derivation.

`rqr_mti_intercept_fit()` has independent R and C++ implementations. It
evaluates the original loss and a global lower bound obtained from feasible
fractional weights. The default relative objective-gap tolerance is `1e-8`;
failure is explicit. Collapsed-radius cases, ties, and exterior roots are
retained when implied by the criterion. No Jacobian is added, and no clipping
or benchmark interval replaces a numerical failure.

## Fractional Targets

`rqr_mti_empirical_tilt_bounds()` computes the smallest and largest retained
means with weights in `[0,1]` summing to `n*q`. These are unpenalized empirical
bounds. Gaussian penalties alter the score identities.

`rqr_mti_fractional_window()` takes the first shortest support containing
`ceiling(n*q)` observations, up to floating-point integer recognition. For
fractional mass `k+r`, its balanced allocation gives each endpoint weight
`(1+r)/2` and each interior point weight one. Left and right allocations are
also explicit options. Integer mass gives the ordinary shortest contiguous
order-statistic window. The retained mean divides its weighted sum by mass;
interpolating neighboring window means can violate the admissible range.

Fractional mass is different from the closed-interval count. The direct DP
posterior uses the actual integer count, including all tied observations.

`rqr_mti_dp_profile()` searches the prescribed `q` menu and empirical tilt
positions. The revision study uses seven positions across the sharp range,
the balanced fractional-window tilt, and zero, with duplicate removal.
Every target must satisfy the numerical accuracy requirement before the
shortest DP-eligible candidate can be selected. This new rule inherits no
frequentist guarantee or historical ECM calibration.

## Controlled Assessment

Configuration: `application/config/mti_profile_revision_20261021.json`.
Runner: `application/scripts/92_run_mti_profile_revision.R`.

1. **Mechanism:** four prespecified observed samples and two synthetic
   samples isolate the solver and fractional-target changes in a 2-by-2
   comparison. Historical fits are replayed at matched initial values.
   All 2464 targets from 28 saved diagnostic menus are compared with their
   independent cell-minimum references; the revised menus are assessed too.
2. **Saved samples:** fit new menus to all 3004 application training samples.
   The historical DP threshold is diagnostic only. Held-out batches do not
   determine the revised menu or probability threshold.
3. **New calibration:** preserve the eight original standardized distributions
   and nine feasible `(n,c,p)` cells. For each family/cell, 32 independent
   development datasets determine a probability-threshold grid, followed by
   1000 fresh assessment datasets. Thresholds include probability quantiles,
   the requested confidence, and the old threshold as a reference. No guessed
   delivery target is imposed. Bonferroni-adjusted one-sided Clopper--Pearson
   bounds account for all family/cell/threshold assessments. Every assessed
   family must return intervals and satisfy the requested lower bound.
4. **Independent validation:** only after every calibration cell passes,
   freeze the new policy and use 1000 fresh datasets per family/cell.
   Fit the revised procedure, historical ECM procedure, Young--Mathew, and
   Wilks to the same samples. The historical procedure keeps its own frozen
   calibration. Report unconditional content attainment, binomial intervals,
   nonreturns, and within-family raw-width means and 2.5%/97.5% quantiles.
5. **Decision:** retain old article inputs. Scientific assessment of independent
   results is required before any proposed replacement. An unresolved
   calibration bound is inconclusive; the procedure must stop rather than
   weaken its assessment or reuse failed thresholds as validated settings.

The panel has 72 family/design cells, 74,304 calibration/development datasets,
and, conditionally, 72,000 independent validation datasets. These are iid
univariate data; no regression or dynamic experiment is added.

## Reproduction and Execution

Use a clean reviewed commit. `make mti-profile-runtime` archives this
repository's committed package into its ignored cache, builds a source
tarball, and installs it in a commit-specific local library. No external
exdqlm checkout is built, installed, or loaded. The manifest records the
configured exdqlm pin, package version, shared-library and R database hashes,
R and benchmark versions, Git commit, and calculation-source hashes.

```bash
make test-mti-profile
make mti-profile-runtime
make mti-profile-preflight
make launch-mti-profile-revision MTI_PROFILE_WORKERS=24
make health-mti-profile-revision
```

The default run directory is
`application/outputs/mti_profile_revision_20261021/revision_20261021`.
Override it with `MTI_PROFILE_RUN_DIR`. The launcher detaches the process,
records its PID and log, uses a lock to prevent duplicate execution, and
records its exit status. Workers use one BLAS/OpenMP thread each. The current
24-worker setting leaves capacity for unrelated calculations on the host.

Each completed batch is written atomically. Resumption requires the same
source, package, configuration, and task identities. Task failures are saved
and never replaced by new observations. A completed failed stage cannot be
treated as a passed stage. To resume after an external interruption, rerun
the launch command with the same source, runtime, and output directory; after
code or numerical changes, choose a new directory and repeat prerequisites.

The mechanism directory contains per-target traces, numerical comparisons,
and `mti_profile_mechanism.pdf` with endpoint and objective traces. A second
PDF shows the final 20 available iterations, so the wide initial midpoint
bracket does not obscure late numerical behavior. Saved-case,
calibration, and validation summaries remain local-only. Plans and audits
remain under ignored `.codex_work/`; this document records the implemented
method and reproduction instructions.

## Portable Package Tests

The package tarball intentionally excludes repository scripts and study
configurations. Its intercept tests therefore use 46 small fixed-target
references in `application/tests/testthat/fixtures/mti_intercept_profile/`.
These are numerical reference values, not fitted application objects. To
regenerate them, run `application/scripts/94_generate_mti_profile_test_reference.R`
with the profile package installed. The generator uses the independent
piecewise-quadratic cell calculation and checks its direct-loss evaluation.
Both R and C++ estimates are compared with these references during package
checks. Repository-script tests for checkpoint resumption, independent
threshold development, and failed candidate searches run separately through
`make test-mti-profile`.
