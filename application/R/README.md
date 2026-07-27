# Native RQR-GIBBS implementation

This directory began from auditable seed copies of the RQR fixed-design and
DESN-readout code in:

```text
Muscat source path: /data/jaguir26/local/src/exdqlm__wt__qdesn_0p4p0_integration
Jerez reference path: /data/muscat_data/jaguir26/exdqlm__wt__qdesn_0p4p0_integration
branch: feature/rqr-desn-readout-20260716
commit: dffb71ee70b597d6a716ee74be1cbc99731cd453
```

The executable RQR and RQR-DLM implementation is now native to the standalone
`rqrgibbs` package in this repository. The pinned exdqlm commit is a read-only
compatibility and comparator reference; it is not an implementation target and
is never compiled, installed, or loaded directly from its checkout. Validation
materializes that exact commit with `git archive` and builds it only below the
ignored `application/cache/` tree.

Changes to the native sampler, state-space utilities, or public contracts must
be made here and covered by the package tests. They must not be propagated into
an exdqlm source checkout as a side effect of this project.
