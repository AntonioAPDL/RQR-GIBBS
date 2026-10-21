# Pharmaceutical ECM Numerical Findings

This diagnostic study does not replace or recalibrate the published procedure.

Completed 28 prespecified cases; 84 fixed-target budget fits; 3004 saved-interval diagnostics.
Baseline relative objective gaps above 1e-08: 26/28.
Persistent gaps at the longest budget: 26/28.
Endpoint changes above 1e-05 training SD: 1/28.
Posterior screen pass/fail changes: 0/28.

## Interpretation

A small reference objective gap supports numerical near-optimality only for the fixed q and tilt studied. A nonzero midpoint gradient is not a nonsmooth optimality test. Nearest-kink calculations are backward-error diagnostics and never replace saved intervals.

Candidate-menu reference comparisons preserve the original q/tilt menu and DP threshold. They are counterfactual numerical checks, not new validation evidence. Objective levels are compared only within the same q/tilt target.

Some prespecified comparisons detect numerical sensitivity. Investigate its size and cause before any algorithm change; a revised selection rule needs independent simulation validation. Do not tune calibration to these responses.

Trace plots show actual endpoint/width/criterion paths. Fitted content and tilt are fixed within each ECM run and therefore appear in headings rather than fictitious parameter traces.

Figure report: pharma_ecm_numerical_diagnostics.pdf
Detailed continuous values: case_decisions.csv, summary.csv, references.csv, candidates.csv, all_saved_diagnostics.csv, iteration_traces.csv.gz.
