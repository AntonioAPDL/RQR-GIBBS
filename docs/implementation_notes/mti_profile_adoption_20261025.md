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

The completed evaluation used the archived rqrgibbs 0.1.0.9038 numerical runtime.
A rebuilt installation must preserve the frozen numerical source, exported R
functions, and R/C++ objective parity. Its binary hash is recorded separately;
cross-machine byte identity is not required. No external source checkout is
compiled or loaded. Point
`MTI_ADOPTED_LIBRARY` and `PHARMA_MTI_LIBRARY` to that installed library:

```bash
make verify-mti-profile-adoption MTI_ADOPTED_LIBRARY=/absolute/path/to/library
make test-mti-profile-adoption MTI_ADOPTED_LIBRARY=/absolute/path/to/library
make evaluate-mti-profile-application \
  MTI_ADOPTED_LIBRARY=/absolute/path/to/library \
  PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_profile_application_20261025/confirmatory \
  PHARMA_MTI_WORKERS=32
make validate-mti-profile-application \
  MTI_ADOPTED_LIBRARY=/absolute/path/to/library \
  PHARMA_MTI_RUN_DIR=application/outputs/pharma_mti_profile_application_20261025/confirmatory
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

## Completed Application Evaluation

Calculation commit: `4dd8ae716b1539e3738bd94119627d96d39564fc`.
The fixed application completed 1502 partitions for each of two responses,
3004 MTI fits, and 9012 method evaluations with 32 single-threaded workers
in 245.7 seconds. Every interval was returned. All candidate objective gaps
passed; the largest absolute relative gap was 1.384227e-13.

`102_validate_mti_profile_application.R` independently recalculated all 3004
selected profiles with zero endpoint discrepancy, all DP posterior content
probabilities, and all 9012 held-out empirical contents. Membership matches
the historical partitions exactly; Young--Mathew and Wilks results are
unchanged. Publication requires this matching independent validation record.

Across random splits, tensile-strength median widths are .514392 for MTI,
.533259 for Young--Mathew, and .665456 for Wilks. Weight-RSD medians are
1.690000, 2.262237, and 3.400000, respectively. MTI and Young--Mathew both
have median held-out empirical content .954023 for each response. These
overlapping historical partitions support descriptive comparisons, not a
new tolerance-confidence claim. Absolute objective gaps are summarized in
the reporting tables; raw signed roundoff values remain in the result file.

The main and supplement now describe the profile solver and paired validation,
use gamma consistently as the probability cutoff, and retain ECM as an
alternative calculation only. They include independent uncertainty and
near-boundary limitations. Historical method records remain reproducible.
