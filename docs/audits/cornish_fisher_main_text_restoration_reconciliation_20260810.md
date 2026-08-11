# Cornish--Fisher Main-Text Restoration Reconciliation

## Decision

The Cornish--Fisher (CF) approximation belongs in the main article as a
bounded population diagnostic, not as part of Figure 2 and not only in the
supplement. The exact cross-distribution geometry remains Figure 2. The
existing two-panel CF display is restored as Figure 3, followed by a compact
seven-law population table. The supplement retains the derivation and
implementation qualifications without duplicating those displays.

This change does not modify the generalized-Bayes target, Gibbs transitions,
exact population tilts used by the model illustrations, or any simulation
protocol.

## Audit conclusions

- The first-order formulas are algebraically consistent with the retained-
  mean population identity and use the conditional truncated moment, including
  division by interval content.
- The approximation is exact at symmetry and accurate for the near-Normal
  Gamma fixture, but materially less accurate for some strongly skewed,
  bounded-support, and support-boundary cases. It is therefore an
  initialization or screening anchor, not a target definition.
- The reflected Beta laws give the required sign reversal and equal absolute
  error, providing a useful implementation check.
- The exponential shortest-contiguous oracle is explicitly identified as a
  lower-support-boundary solution.
- Figure 3 uses true population skewness. It contains no sample plug-in
  estimate, MCMC output, or response-predictive interpretation.
- Cornish and Fisher (1937), DOI `10.2307/1400905`, is now cited as the
  foundational source.
- The main-text implementation-status sentence now matches the native API:
  fixed nonzero tilt is bounded to declared fixed-rate ridge and supported DLM
  evolution contracts, while repeated-sample evidence remains deepest for
  ordinary RQR.

## Reproducibility changes

1. The table generator now emits explicit machine-readable distribution
   parameter names in CSV and concise typeset names in TeX.
2. The generated table groups equal-tailed and shortest-contiguous quantities
   and reports exact tilt, CF tilt, and absolute standardized gap.
3. Table tests reproduce every numerical cell, reflection behavior, boundary
   status, column order, and caption claim boundary.
4. Figure tests enforce that Figure 2 is CF-free, Figure 3 and the table occur
   exactly once in the main article, neither display is duplicated in the
   supplement, and both are included by the arXiv packager.
5. Figure documentation now records the actual internal deterministic fixture:
   content `0.80` and asymmetric-Laplace quantile index `0.65`. The manuscript
   intentionally describes it only as a left-skewed illustration to avoid
   confusing that arbitrary display law with the interval content.

## Validation matrix

| Gate | Result |
|---|---|
| `make smoke` | PASS |
| `make test-theory-tables` | PASS |
| `make test-theory-figures` | PASS |
| `make pdf` | PASS, 23 pages |
| `make supplement` | PASS, 28 pages |
| Undefined citations or references | 0 |
| Overfull/underfull boxes or PDF warnings | 0 |
| Visual review of main page 9 | PASS |
| arXiv ZIP integrity test | PASS |
| arXiv ZIP extracted compile | PASS, 23 pages |

The ignored validation archive is
`application/cache/arxiv_preprint_20260810_cf_restoration/`
`rqr_gibbs_arxiv_source_20260810_cf_restoration.zip`, with SHA-256
`91de9d3ed0dca83f85c412f2f3f45bdc37d099285f13e39693a06374688a387c`.
It contains only the top-level source, bibliography inputs, five main-text
figure assets, the generated CF table, and compact source notes/manifest.

## Evidence boundary

The restored display and table establish only deterministic population
behavior of a first-order approximation. They do not validate finite-sample
tilt estimation, repeated-sample calibration, forecasting performance,
response simulation, or superiority over exact-oracle tilts. The planned
validation study continues to use exact externally supplied population tilts;
CF is not substituted into that protocol.
