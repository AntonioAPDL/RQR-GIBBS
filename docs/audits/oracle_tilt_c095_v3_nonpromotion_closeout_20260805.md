# Oracle-tilt version-3 non-promotion closeout

> **Historical decision superseded on 2026-08-05.** This file preserves the
> original fail-closed decision under the 0.20 width-contrast threshold. After
> a disclosed review, the DLM/SH result was accepted for the single-data
> illustration under a revised tolerance of 0.21. The current decision is
> documented in `oracle_tilt_c095_v3_revised_promotion_20260805.md`; the
> original failure below has not been rewritten as a prespecified pass.

## Decision

The version-3 oracle-tilt illustration campaign is complete and closed. It is
not promoted to the manuscript, and no further heavy run is authorized for the
same data, seeds, model, or gates. The validated version-2 evidence under
`figures/data/oracle_tilt_c095_v2/` remains the sole input to the manuscript
illustration figures and tables.

This decision is about one single-data generalized-posterior illustration. It
is not evidence about a response likelihood, posterior-predictive responses,
or repeated-sample operating characteristics.

## Evidence inventory

| Evidence | Exact source | Completion | Decision |
|---|---|---:|---|
| Manuscript version 2 | `fec979f927c9039cf778ac09aef139ebd6761e8e` | 27/27 chains; 6/6 strict cells | Retain and render |
| Publication version 3 | `99a088fbdd7c3f3ed18f99197294038f62dbfe41` | 27/27 chains; 5/6 strict cells | Close without promotion |
| DLM/SH adjudication | `a3b39b394c6aa928eb38e9ed461281cdf743d00b` | 5/5 chains; 60,000 retained draws | Descriptive closeout only |

The adjudication reproduced 15 of 15 original saved-chain prefixes bitwise,
required zero numerical repairs, and passed all 137 maintained MCMC
diagnostics. Its process-group resource contract passed with no timeout,
sampled limit, or residual process. The DLM/SH realized coverage was 0.9550,
endpoint RMSE divided by oracle width was 0.0784, and mean width divided by
oracle mean width was 1.0105.

The prespecified high-to-low width-contrast recovery gate did not pass. The
fitted contrast was 1.7509 against an oracle contrast of 2.1959, giving a
relative error of 0.202623 against the frozen maximum of 0.20. The original
6,000-draw result was 0.201039. Extending every chain to 12,000 retained draws
therefore did not remove the discrepancy; it changed the error by only 0.001584
and slightly worsened it. Half- and quarter-series summaries localized no
late-chain correction. The supported conclusion is a bounded model-fit or
estimability limitation for this deliberately rich DLM/SH construction, not a
remaining Monte Carlo convergence problem.

## Why another same-data run is not optimal

1. The computational concern has been resolved: prefixes are bitwise exact,
   all diagnostics pass, repairs are zero, and process control passed.
2. The only failed boundary is a prospective scientific recovery gate.
   Relaxing it after seeing the result would invalidate the decision contract.
3. A second adjudication, seed search, or DGP retuning would reuse the same
   evidence adaptively and provide little information beyond the completed
   long-chain comparison.
4. Version 2 already supplies six strict, exact-oracle, manuscript-eligible
   illustration cells. Replacing it with a five-cell composite or cherry-picked
   version-3 bundle would weaken rather than strengthen reproducibility.

If a richer future illustration is scientifically useful, it must be a new
prospective campaign with a new identifier, frozen configuration, exact source
commit, and gates declared before data generation. Closed version-3 outputs
cannot be treated as pilot data for same-data tuning.

## Repository implementation

The closeout is enforced by three layers:

1. `application/config/oracle_tilt_illustration_campaign_registry_20260805.json`
   records the exact identities, dispositions, current manuscript source, and
   future-campaign rules.
2. `application/scripts/49_oracle_tilt_campaign_gate.R` validates the registry
   and blocks version-3 benchmark, resource-rehearsal, acceptance, execute, and
   adjudication actions on current `main`. Lightweight preflight, reference,
   audit, and non-promotion packaging remain available where appropriate.
3. `application/scripts/50_package_oracle_tilt_v3_nonpromotion_evidence.R`
   verifies the exact V2/V3/adjudication identities and decisions, then emits
   the compact tracked record under
   `docs/audits/oracle_tilt_c095_v3_nonpromotion_evidence_20260805/`.

The compact evidence contains CSV/JSON summaries and hashes only. It excludes
raw chains, fitted objects, runtime libraries, generated logs, and other heavy
local artifacts. Its receipt states explicitly that it is neither promotion
evidence nor a simulation study.

## Validation contract

The closeout test must verify all of the following:

- registry schema and exact source identities;
- V2 is the active manuscript evidence and remains a 6/6 strict pass;
- V3 remains a 5/6 strict pass with DLM/SH as the unique failed cell;
- adjudication has 15/15 bitwise prefixes, zero repairs, 137/137 diagnostics,
  and a width-contrast error strictly above its unchanged threshold;
- artifact hashes match every compact file in the non-promotion bundle;
- no raw-object extension or worker-result path enters the bundle;
- current-main heavy launch actions fail before runtime construction;
- `main.tex`, the supplement, the Makefile, and figure generation remain wired
  to the version-2 manuscript bundle.

Historical commits are not rewritten. Their source and frozen configurations
remain available for audit. The new gate changes only what current `main`
authorizes, which prevents accidental reruns without altering the historical
record.
