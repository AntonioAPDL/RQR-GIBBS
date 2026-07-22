# RQR-GIBBS Standalone Bootstrap

Date: 2026-07-22

## Decision

RQR is moved into a standalone manuscript/reproducibility repo because it has a
different inferential target from Q-DESN. The Q-DESN article estimates
conditional quantile readouts. RQR estimates direct interval roots under a
loss-based generalized posterior.

## Source Material

The initial scaffold uses:

- Q-DESN manuscript style and academic writing profile;
- the RQR-DESN prose and preliminary table from the Q-DESN article/supplement;
- the pushed exdqlm RQR implementation branch at commit
  `dffb71ee70b597d6a716ee74be1cbc99731cd453`;
- local PDF copies kept under ignored `literature/pdfs/`.

## Immediate Gates

Before heavy simulation:

1. clone this repo on Jerez;
2. clone the exdqlm RQR branch on Jerez;
3. run `make smoke`;
4. run `make test-exdqlm-rqr`;
5. compile `main.tex` and `rqr-gibbs-supplement.tex`;
6. implement and smoke-test the RQR-DLM/FFBS path;
7. freeze a standalone simulation manifest including fixed-design RQR,
   RQR-DESN, RQR-DLM, quantile-derived intervals, and empirical baselines.

## Nonclaims

The migrated RQR-DESN table is preliminary. It should not be presented as the
final standalone evidence because it does not include the RQR-DLM comparison
and was originally generated as a Q-DESN-adjacent direct-interval check.

