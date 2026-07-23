# Repository Instructions

This repository is for the standalone RQR-GIBBS manuscript and companion
simulation code. Before editing manuscript prose, read `STYLE_PROFILE.md`.

Core rules:

- Preserve the distinction between loss-based generalized Bayes updates and
  ordinary response likelihoods.
- Do not describe RQR interval draws as posterior predictive response draws.
- Keep RQR-DESN and RQR-DLM evidence scoped to the simulation or application
  protocol that produced it.
- Treat `application/data_local/`, `application/cache/`, `application/runs/`,
  `application/logs/`, `application/outputs/`, `literature/pdfs/`, and
  `.codex_work/` as local-only workspaces.
- Commit reproducible source files, configs, scripts, manifests, tables, and
  documentation; do not commit heavy fitted model objects.
- When running on Jerez, record the exact Git commits for this repo and for the
  pinned exdqlm RQR branch before launching heavy simulations.

Protected scope:

- This repo may use the Q-DESN article and exdqlm RQR branch as references, but
  it should not mutate those repositories as a side effect.
- Never compile, install, or load a package namespace directly from an exdqlm
  source checkout. Materialize the pinned commit with `git archive`, then build,
  install, and test it only under the ignored `application/cache/` tree.
- Run Git provenance reads against external repositories with optional locks
  disabled, and require the before/after checkout guard to include ignored
  files so compiler artifacts cannot change silently.
- If Q-DESN article cleanup is needed, do it in the Q-DESN article repo as a
  separate scoped commit.
