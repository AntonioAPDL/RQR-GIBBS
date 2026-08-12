# TCSP/ECM Split-Exact Smoke Results

Date: 2026-08-12
Working-tree HEAD recorded in smoke manifests:
`4ca59085630ead755f5f09f8013dc287a993f71e`

## MT-RQR-ECM Smoke

Command:

```bash
make rqr-ecm-validation-smoke
```

Output:

```text
application/outputs/rqr_ecm_validation_v1/smoke_20260812T061214Z
```

Summary:

- rows: 12;
- convergence: 12 `converged`, 0 failures;
- maximum ECM/direct-optimizer objective gap: `1.6722e-7`;
- response likelihood: `false`;
- interpretation: fixed-target optimizer validation, not tolerance-confidence
  evidence.

## Split Exact-Spacing Smoke

Command:

```bash
make tcsp-split-exact-validation-smoke
```

Output:

```text
application/outputs/tcsp_split_exact_validation_v1/smoke_20260812T061359Z
```

Summary:

- rows: 80;
- methods: scan DKW, split empirical-shortest, split ECM fixed-tilt,
  split Cornish-Fisher, min-max comparator;
- infeasible method failures: 0;
- validity was summarized before width;
- response likelihood: `false`;
- interpretation: iid univariate pilot validation, not a full campaign.

## Follow-Up

The next prepared execution is the moderate smoke-extension stage in
`docs/implementation_notes/tcsp_ecm_split_exact_validation_plan_20260812.md`.
No full-pilot or confirmatory launch is authorized by these smoke results.
