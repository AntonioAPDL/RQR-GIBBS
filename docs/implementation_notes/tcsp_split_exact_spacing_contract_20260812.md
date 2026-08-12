# TCSP Split Exact-Spacing Contract

Date: 2026-08-12
Branch: `feature/mt-rqr-ecm-split-exact-tcsp-20260812`

## Purpose

The split exact-spacing method separates placement from certification. An
independent pilot sample selects the lower-tail placement. An independent main
sample supplies a fixed order-statistic spacing with an exact distribution-free
content law under iid continuity.

This construction is initially univariate and intercept-only for the formal
tolerance action. It does not establish conditional regression tolerance.

## Main-Sample Calibration

For ordered main-sample values `Z_(1) <= ... <= Z_(N)` and fixed indices
`r < s`, define `d = s - r`. Then

```text
F(Z_(s)) - F(Z_(r)) ~ Beta(d, N + 1 - d).
```

The indexing distinction is stored in every object:

- `spacing_gap = d` is the probability-spacing parameter;
- `closed_position_count = d + 1` is the number of main-sample order-statistic
  positions in `[Z_(r), Z_(s)]`.

The public helper `rqr_tcsp_exact_spacing_gap()` chooses the smallest `d` such
that

```text
P{Beta(d, N + 1 - d) >= c} >= 1 - alpha.
```

## Pilot Placement

The public helper `rqr_tcsp_split_exact_fit()` fixes the split by `split_seed`
before fitting. The main sample cannot influence the pilot placement except
through the predeclared main-sample size and spacing gap.

Supported pilot methods:

- `empirical_shortest`, the default intercept-only method;
- `ecm_fixed_tilt`, which uses ECM with a pilot-only fixed tilt;
- `ecm_profile`, which selects from a predeclared pilot-only tilt grid;
- `cornish_fisher`, a fast placement initializer/comparator.

For intercept-only pilot data, empirical sorting solves the pilot shortest
window directly. ECM is included for diagnostics, smoothing, future regression
extensions, and dense fixed-target path work; it is not claimed to beat exact
pilot sorting in the univariate problem.

## Formal Action

The returned formal action is

```text
[Z_(r_hat), Z_(r_hat + d)]
```

on the independent main sample, where `r_hat` is fixed by the pilot before the
main order statistics are used. Conditional on the pilot, the recorded Beta
survival probability supplies the distribution-free repeated-sample statement
for iid continuous univariate data.

## Stored Provenance

Each object records:

- split seed;
- pilot and main indices;
- pilot method;
- pilot fit digest;
- optional ECM fit digest;
- main spacing gap and closed count;
- exact Beta survival probability;
- lower and upper main indices;
- endpoint convention;
- response-scale statement;
- formal action source.

## Validation

The smoke validation script is:

```bash
make tcsp-split-exact-validation-smoke
```

The config is:

```text
application/config/tcsp_split_exact_validation_v1.json
```

The config authorizes only smoke and moderate pilot runs. Heavy validation and
confirmatory execution require a separate reviewed configuration and explicit
authorization.
