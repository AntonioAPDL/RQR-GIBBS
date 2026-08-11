# TCSP Validation Next-Stage Audit and Launch Plan

Date: 2026-08-11
Schema: `rqrgibbs_tcsp_validation_next_stage_plan/1.0.0`
Status: post-pilot diagnosis and full-pilot launch plan

## Verdict

The current repository is ready for a larger iid univariate TCSP full pilot,
but it is not ready for confirmatory promotion. The compact pilot run validated
the mechanics that matter first: deterministic seeds, repeated-sample data
reuse across methods, TCSP retained-count calibration, true population-content
evaluation, manifest hashing, failure accounting, and local-only output
storage. It did not provide publication-precision evidence.

The next optimal step is therefore not to rewrite the manuscript around the
pilot and not to authorize a confirmatory study. The next step is to launch a
clean, committed-source full pilot after the audit plumbing in this branch is
merged. That full pilot should be audited before any article title, abstract,
simulation section, or bibliography changes are made from empirical claims.

## Repository Audit Findings

The TCSP method currently implemented in the package is the first global
minimum-width closed order-statistic window after a retained-count calibration.
The validation harness correctly evaluates the population content
`F(upper) - F(lower)` under known DGPs. It does not evaluate ordinary response
likelihood, posterior-predictive response draws, regression tolerance, dynamic
tolerance, or posterior-action equivalence.

The pilot config is deliberately fail-closed:

- confirmatory execution is disabled;
- the confirmatory stage requires a new config;
- generated validation outputs remain under ignored `application/outputs/`;
- unavailable competitors are disabled with explicit reasons;
- Monte Carlo scan calibration is recorded as conservative numerical
  calibration, not exact recursion;
- DKW calibration is allowed to fail closed when the retained count exceeds
  `n`.

The closed pilot bundle at
`application/outputs/tcsp_validation_v1/pilot_codex_20260811` passed artifact
manifest verification and contains 576 replication rows, 72 summary rows, and
72 method-row failures. Those failures are not runtime errors: they are the
expected DKW infeasibility cells at `n=80` and at `n=250,c=0.90` under the
pilot tolerance-confidence grid.

One promotion gap was diagnosed. The pilot's `source_state.json` records that
the run was launched while the harness files were still local changes. That is
acceptable for a rehearsal, because the committed code now contains those
files, but it is not acceptable for publication or confirmatory evidence. Any
promotion-eligible run must be relaunched after this audit layer is committed
and merged.

## Design Diagnosis

The current pilot grid has the right shape for a compact rehearsal:

- `n = 80` stresses fail-closed DKW infeasibility;
- `n = 250` shows the transition between infeasible and feasible DKW actions;
- `n = 600` exercises feasible high-content behavior in the pilot confidence
  grid;
- normal, lognormal, and mixture DGPs provide a quick correctly specified and
  misspecified contrast;
- MC scan calibration with 250 Uniform simulations is enough for plumbing, not
  for final efficiency ranking.

The full pilot should broaden this without changing the claim scope. The
sample-size grid should add `n=1200`. This is not cosmetic: at high content and
high confidence, DKW at `n=600` can still be close to a range-wide action, so
`n=1200` is needed to judge whether the DKW-calibrated shortest window has a
meaningful width advantage over range-like competitors.

The DGP grid should use all tracked iid univariate DGPs in the config:

- standard normal;
- centered standardized lognormal;
- variance-standardized Student `t_5`;
- standardized separated normal mixture;
- bounded right-skewed beta.

The method grid should keep:

- TCSP with DKW calibration;
- TCSP with conservative Uniform Monte Carlo scan calibration;
- symmetric Wilks/order-statistic interval;
- Wilks min-max range interval;
- equal-tailed empirical interval at the DKW retained count as a diagnostic
  only, not as a certified formal action;
- normal-theory Howe approximation as a correctly specified/misspecified
  sensitivity method;
- population shortest interval as an oracle width reference where useful.

Young-Mathew interpolation and calibrated Bayesian nonparametric Gibbs
tolerance intervals should remain disabled until they have tracked
implementations, tests, and dependency declarations.

## Full-Pilot Contract

The full pilot should be launched only after this audit branch is on `main`.
Minimum launch contract:

- source commit: exact `main` commit containing the audit script and tests;
- source status: no tracked code changes in the validation source;
- output root: ignored `application/outputs/tcsp_validation_v1`;
- DGPs: all five tracked iid univariate DGPs;
- contents: `0.80` and `0.90`;
- tolerance confidences: `0.80` and `0.90`;
- sample sizes: `80`, `250`, `600`, and `1200`;
- methods: active source-controlled methods only;
- Uniform scan calibration: at least 5000 simulations per
  `n/content/confidence` cell;
- replications: at least 250 per cell for the full pilot;
- generated output: local-only, then compact audit bundle under `docs/audits/`.

The full pilot is still not confirmatory. It is the stage that checks runtime,
failure modes, DKW feasibility, MC calibration stability, competitor behavior,
and whether the full design has enough precision for a final frozen
confirmatory config.

## Promotion Gates

Before any full-pilot result can be used in manuscript prose, all of these
must pass:

- artifact manifest verifies by byte count and SHA-256;
- closeout row accounting matches replication, summary, and failure artifacts;
- source state records a committed source and no validation-source drift;
- config preserves iid univariate claim scope;
- confirmatory execution remains disabled;
- DKW infeasible cells are retained as failures, not dropped from denominators;
- MC calibration certificates meet their lower-bound target;
- method summaries aggregate from replication-level rows;
- disabled competitors remain documented;
- audit bundle records promotion blockers explicitly.

Before confirmatory execution can be authorized, the full-pilot audit must also
show that the selected grid, calibration budget, and replication budget are
operationally stable. Confirmatory authorization should be a separate config
change whose diff only flips the intended execution gate and binds the frozen
source commit.

## Manuscript Boundary

No title, abstract, simulation section, or bibliography claim should be updated
from the compact pilot. The article can mention the implemented TCSP validation
protocol only after a full-pilot audit exists, and it can report empirical
performance only after promotion gates pass. The proof ledger remains
authoritative: empirical validation does not prove the exact scan theorem and
does not transfer posterior summaries into formal tolerance actions.
