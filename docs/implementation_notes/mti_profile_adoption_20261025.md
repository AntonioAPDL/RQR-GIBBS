# Adopted Profiled MTI Tolerance Procedure

## Statistical Specification

The current procedure is `mti_profile_mass_cells_v1`, computed by the native
fixed-target MTI profile solver. It selects the shortest eligible candidate
on the declared finite content/tilt grid. It does not claim continuous global
width optimality. Generalized Bayes defines the penalized interval-root
criterion; a separate direct DP posterior assesses interval content.

The adopted policy is in
`application/config/mti_profile_adopted_20261025.json`; its nine cutoff rows
and small calibration/validation summaries are under
`figures/data/mti_profile_validation_20261025/`. The completed study source
is `5b329e597d12826b05019306f1ab0d19e439c97c`. Its combined policy hash is
`a4fdeafb5b9a1be267a64f765e340ac89cd8e0f4085f80a0a8fd1aba24a75d3d`.

The follow-up independently resolves n=50,c=.90 and n=100,c=.95 and preserves
the other seven cutoffs. A fixed 2.5% error allocation rechecks every original
comparison; the other 2.5% covers the two new settings, eight distributions,
and their independently developed finite threshold menus. Every selected
calibration lower bound exceeds .95. This is finite-panel Monte Carlo evidence,
not a distribution-free theorem or a guarantee that every future sample
produces an eligible interval.

Independent validation uses 72000 datasets, 1000 per distribution/design case,
paired across refined MTI, frozen MTI, Young--Mathew, and sample-range Wilks.
The article reports the three primary methods only: 216000 method evaluations.
All return intervals. MTI attainment ranges from .951 to .993; simultaneous
lower bounds exceed .95 in 38 cases and remain inconclusive in 34. None has
an upper bound below .95. Width superiority over Young--Mathew is not uniform.

## Application and Compatibility

`application/config/pharma_tolerance_application_profiled_20261025.json`
applies the fixed n=100,c=.90 rule to the original pharmaceutical data and
exactly the same random, chronological, month-grouped, and API-grouped splits.
No response, held-out outcome, or application comparison is used to tune the
rule. Split identities are compared to the historical membership record
before calculation. The historical application remains unchanged under
`figures/data/pharma_mti_application_20261020/`.

The old internal application row identifier `mti_ecm_adaptive_cell` is retained
solely for compatibility with existing split-summary and plotting functions.
New rows explicitly record `mti_computation=fixed_target_profile`, the profile
convergence result, iteration count, objective gap, and candidate diagnostics.
ECM diagnostic columns are absent in meaning and set to NA for these rows;
they must never be interpreted as ECM fits. The manifest records the actual
scientific method, policy hash, native library hash, repository/exdqlm pins,
and calculation source hashes. The printed label is MTI.

## Reproduction

Use the verified archived rqrgibbs 0.1.0.9038 numerical runtime from the completed
study. No external source checkout is compiled or loaded. Point
`MTI_ADOPTED_LIBRARY` and `PHARMA_MTI_LIBRARY` to that installed library:

```bash
make verify-mti-profile-adoption MTI_ADOPTED_LIBRARY=/absolute/path/to/library
make test-mti-profile-adoption MTI_ADOPTED_LIBRARY=/absolute/path/to/library
make evaluate-mti-profile-application \
  MTI_ADOPTED_LIBRARY=/absolute/path/to/library \
  PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_profile_application_20261025/confirmatory \
  PHARMA_MTI_WORKERS=32
```

Calculation source must be committed before evaluation. One thread is used
per worker, with a maximum of 40 workers. Application evaluation is a fixed
analysis, not a fresh calibration or a statistical pilot. Existing outputs
are not overwritten. `pma_verify_run()` checks the completed evidence before
publication into the new versioned application directory.

The adopted-study import is an explicit, one-time operation:

```bash
Rscript application/scripts/101_adopt_mti_profile.R --mode=import \
  --study-run=/absolute/path/to/threshold_followup_20261024
```

It verifies completion, policy/results hashes, CDF contents, all dataset
identities, selected calibration bounds, and then exports small evidence.
It never refits or changes thresholds. Historical calibration and fitted
simulation objects remain local and unchanged. The committed case summaries
support table/figure reproduction without heavy model objects.

Current `theory-tables` uses the adopted evidence, and current `theory-figures`
uses those generated tables. Historical tolerance table scripts remain
available through `historical-tolerance-tables`. Article figures retain raw
within-case empirical 95% width ranges, mean crosses, and population
shortest-content references; the attainment plot spans family-specific point
estimates rather than binomial confidence intervals.

## Adoption Checks

Focused tests cover all nine policy settings, cached/uncached fitting parity,
unknown/duplicate/unresolved policies, absence of eligible intervals,
unchanged application splits, explicit profile diagnostics, numerical-gap
rejection, historical application behavior, and three-method unpooled reports.
Package numerical source is unchanged from the completed study. Manuscript
changes distinguish ECM as an alternative calculation, the profiled method
actually evaluated, and the separate finite-panel tolerance assessment.
