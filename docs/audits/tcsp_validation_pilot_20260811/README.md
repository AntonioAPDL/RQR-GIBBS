# TCSP validation pilot audit

This compact bundle audits the ignored local TCSP validation pilot run.
It is a wiring and diagnosis record, not manuscript evidence and not a theorem proof.

## Verdict

- status: `audited_rehearsal_not_promoted`
- source run: `/data/muscat_data/jaguir26/RQR-GIBBS/application/outputs/tcsp_validation_v1/pilot_codex_20260811`
- rows: 576
- summary rows: 72
- failures: 72
- required gate failures: 0
- promotion blockers recorded: 2

The pilot validated the run plumbing, artifact manifest, DGP/method contracts,
failure accounting, and conservative TCSP scan-calibration behavior. It did
not meet promotion conditions because the source run was a compact
8-replication rehearsal and its recorded source state included local changes.

## Next Step

Launch a full iid univariate pilot only after this audit plumbing is merged.
The full pilot should use a committed source state, a larger Monte Carlo scan
calibration budget, all tracked DGPs, both tolerance-confidence levels, and
a sample-size grid that adds `n=1200` so high-content DKW intervals are not
only range-wide stress cases.

## Files

- `audit_summary.json`: machine-readable verdict and source-run accounting.
- `audit_gates.csv`: pass/fail gates and promotion blockers.
- `method_summary.csv`: method-level results from replication-level data.
- `cell_summary_compact.csv`: cell-level pilot summaries with failure cells retained.
- `critical_count_summary.csv`: calibration feasibility summary by method.
- `dkw_feasibility.csv`: DKW retained-count feasibility by sample size/content/confidence.
- `mc_calibration_health.csv`: Uniform Monte Carlo scan calibration certificates.
- `normal_howe_sensitivity.csv`: normal-theory competitor behavior by DGP.
- `next_stage_plan.csv`: source-controlled launch plan for the full pilot.
- `source_run_manifest.csv`: copied manifest for the ignored source run.
- `artifact_hashes.csv`: SHA-256 hashes for this audit bundle.
