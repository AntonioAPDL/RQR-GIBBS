# RQR-GIBBS Transition Final Audit

Date: 2026-07-22

## Conclusion

The transition is complete. RQR has been separated from the Q-DESN article into
a standalone `RQR-GIBBS` repository, the Jerez workspace is available for the
next phase, and the old Muscat RQR run has been stopped without deleting
traceability artifacts.

## Why The Split Was Made

Advisor feedback favored treating the RQR loss, Gibbs augmentation, fixed
nonlinear DESN readout, and planned linear dynamic/state-space extension as a
standalone article rather than as a secondary section of the Q-DESN paper. The
split keeps the Q-DESN article focused on Bayesian quantile forecasting with
Deep Echo State Networks, while giving RQR enough space for its own model,
posterior computation, simulation study, and applications.

## Authoritative Repositories

| Location | Repo | Branch | Minimum Expected Commit | Role |
|---|---|---|---:|---|
| Muscat | `/data/jaguir26/local/src/RQR-GIBBS` | `main` | `fa8a2242e3fea481322b9c727860f764a8bb6393` or newer | Standalone RQR manuscript/reproducibility repo |
| Jerez | `/data/muscat_data/jaguir26/RQR-GIBBS` | `main` | `fa8a2242e3fea481322b9c727860f764a8bb6393` or newer | New working repo for future RQR development |
| Jerez | `/data/muscat_data/jaguir26/exdqlm__wt__qdesn_0p4p0_integration` | `feature/rqr-desn-readout-20260716` | `dffb71ee70b597d6a716ee74be1cbc99731cd453` | Pinned RQR implementation source of truth |
| Jerez | `/data/muscat_data/jaguir26/Article-Q-DESN---Version-2` | `main` | `f9f22804eff3871bb5350c8add04b7c9f4d4957b` | Q-DESN article/style reference only |

## RQR-GIBBS Contents

The standalone repo contains:

- `main.tex` and `rqr-gibbs-supplement.tex`;
- `STYLE_PROFILE.md` and `AGENTS.md`;
- `refs.bib`;
- RQR implementation seed files under `application/R/`;
- simulation, audit, launch, and preflight scripts under `application/scripts/`;
- focused RQR tests under `application/tests/testthat/`;
- migrated RQR evidence table under `tables/`;
- transition and implementation documentation under `docs/`;
- local-only PDF and output workspaces governed by `.gitignore`.

The copied RQR implementation is intentionally a seed/reference layer. The
pinned exdqlm branch remains the implementation source of truth until native
RQR-GIBBS APIs are promoted and tested.

## Validation Status

Completed validation gates:

| Gate | Status |
|---|---|
| `make smoke` on Muscat | pass |
| `make smoke` on Jerez | pass |
| `make pdf` on Jerez | pass |
| `make supplement` on Jerez | pass |
| `make test-exdqlm-rqr` on Jerez | pass |
| Local literature PDFs copied | 18 PDFs |
| Literature manifest generated | pass |
| PDFs and manifests ignored | pass |
| Tracked PDF count in RQR-GIBBS | 0 |
| Tracked TeX artifact count in RQR-GIBBS | 0 |

Jerez lacks `latexmk` in `PATH`, but `pdflatex` and `bibtex` are available and
the `Makefile` fallback has already compiled both TeX entry points.

## Q-DESN Article Cleanup

RQR mentions were removed from the Q-DESN article, supplement, references, and
tables. The cleanup commit is:

```text
f9f22804eff3871bb5350c8add04b7c9f4d4957b
```

The direct Overleaf Git branch was verified at the same commit during handoff.
The Q-DESN article repo still has unrelated joint-QDESN dirty files on Muscat;
these were intentionally left untouched because they are outside the RQR split.

## Muscat Run Closeout

The old Muscat RQR-DESN article-congruent simulation was stopped cleanly. Its
partial state is documented in:

```text
docs/audits/muscat_rqr_run_closeout_20260722.md
```

No model payloads were deleted. The run was small and partial, with no aggregate
metric summaries and no heavy `.rds/.rda/.RData/.rdata` artifacts.

## Next Technical Direction

The next Jerez chat should verify the repo state, then plan before launching:

1. Promote/wrap the existing RQR implementation into a native standalone API.
2. Validate learned-scale RQR Gibbs updates and fixed-design contracts.
3. Implement or validate the linear dynamic RQR-DLM/state-space path.
4. Freeze a matched simulation design for fixed-design RQR, RQR-DESN, RQR-DLM,
   quantile-derived interval baselines, and empirical baselines.
5. Keep all heavy outputs ignored and reproducible through manifests.
6. Update manuscript prose only after model and simulation gates are clear.

## First Jerez Prompt

The self-contained prompt for the first Jerez Codex chat is:

```text
docs/implementation_notes/jerez_first_codex_prompt_20260722.md
```
