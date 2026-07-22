# First Jerez Codex Prompt

Use this prompt when opening the first new Codex chat from VS Code on Jerez.

Recommended VS Code entry point:

```text
Remote-SSH target: jaguir26@jerez.be.ucsc.edu
Open folder: /data/muscat_data/jaguir26/local/src/RQR-GIBBS
Start a new Codex chat from that workspace and paste the prompt below.
```

```text
You are working on jerez.be.ucsc.edu in the standalone RQR-GIBBS project.

Primary objective:
Continue the new standalone paper and reproducibility repo for Bayesian relaxed
quantile regression (RQR) with Gibbs sampling. This project was intentionally
split out of the Q-DESN article after advisor feedback: the RQR loss, its Gibbs
augmentation, RQR-DESN, and the planned linear dynamic/state-space RQR-DLM path
are substantial enough to be a separate article.

Primary repo:
/data/muscat_data/jaguir26/local/src/RQR-GIBBS
remote: https://github.com/AntonioAPDL/RQR-GIBBS
expected branch: main
expected commit at handoff or newer: fa8a2242e3fea481322b9c727860f764a8bb6393

Pinned reference implementation repo:
/data/muscat_data/jaguir26/local/src/exdqlm__wt__qdesn_0p4p0_integration
remote/source project: AntonioAPDL/exdqlm
expected branch: feature/rqr-desn-readout-20260716
expected commit: dffb71ee70b597d6a716ee74be1cbc99731cd453
role: implementation source of truth for the existing RQR-DESN code until the
standalone repo promotes a native API.

Q-DESN article/style reference repo:
/data/muscat_data/jaguir26/local/src/Article-Q-DESN---Version-2
remote/source project: AntonioAPDL/Article-Q-DESN---Version-2
expected branch: main
expected commit at handoff or newer: f9f22804eff3871bb5350c8add04b7c9f4d4957b
role: style/reference only. Do not mutate this repo from this chat unless the
user explicitly asks for a separate Q-DESN article edit.

Important Muscat context:
The old Muscat RQR-DESN article-congruent run was stopped cleanly after the
standalone split. It was partial, small, and not promotion-grade: 8,040 selected
launch rows, 415 completed scenario statuses, 0 nonempty failure logs, 0
aggregate metric summaries, and no heavy model objects. The closeout note is:
docs/audits/muscat_rqr_run_closeout_20260722.md

Important Q-DESN article context:
RQR material was removed from the Q-DESN article and supplement so that Q-DESN
can remain focused on Bayesian quantile forecasting with Deep Echo State
Networks. The cleanup commit is:
f9f22804eff3871bb5350c8add04b7c9f4d4957b
Direct Overleaf Git main was verified at the same commit during handoff.

Repository map in RQR-GIBBS:
- main.tex: standalone article scaffold.
- rqr-gibbs-supplement.tex: derivations and reproducibility supplement scaffold.
- STYLE_PROFILE.md: academic writing criteria inherited from the Q-DESN workflow.
- AGENTS.md: repo-specific guardrails.
- refs.bib: bibliography seeded from Q-DESN plus RQR/general-Bayes references.
- application/R/: RQR implementation seed files copied from the pinned exdqlm branch.
- application/scripts/: RQR simulation/audit/preflight scripts plus repo utility scripts.
- application/tests/testthat/: focused RQR tests copied for validation/reference.
- docs/audits/: transition, preflight, and closeout audits.
- docs/implementation_notes/: prior RQR-DESN design notes and implementation plans.
- literature/pdfs/: local-only related-paper PDFs, intentionally ignored.
- application/data_local/, application/cache/, application/runs/, application/logs/,
  application/outputs/: local-only output roots, intentionally ignored.

First action: do a read-only verification. Confirm:
1. RQR-GIBBS is on main at fa8a2242e3fea481322b9c727860f764a8bb6393 or newer.
2. exdqlm is on feature/rqr-desn-readout-20260716 at
   dffb71ee70b597d6a716ee74be1cbc99731cd453.
3. Article-Q-DESN---Version-2 is on main at
   f9f22804eff3871bb5350c8add04b7c9f4d4957b or newer.
4. The three repos are clean, or identify dirty files exactly.
5. make smoke passes.
6. make pdf and make supplement pass.
7. make test-exdqlm-rqr passes.
8. literature/pdfs contains the local-only PDF set and make literature-manifest works.
9. No heavy generated artifacts, PDFs, TeX logs, fitted models, or simulation outputs are tracked.

After verification, prepare a careful plan before launching anything heavy.
The optimal next stage is not to immediately reuse the retired Muscat RQR-DESN
run. Instead:
- promote or wrap the existing RQR implementation into a native standalone API;
- verify the learned-scale RQR Gibbs update and fixed-design contracts;
- design and implement the RQR-DLM/linear dynamic state-space path, likely with
  FFBS or an equivalent state sampler;
- define a matched simulation study comparing fixed-design RQR, RQR-DESN,
  RQR-DLM, quantile-derived interval baselines, and empirical baselines under
  common data-generating mechanisms, sample sizes, training windows, forecast
  windows, seeds, and scoring rules;
- use loss/interval language consistently, not ordinary response-likelihood or
  posterior-predictive-density language unless the object is explicitly defined;
- keep all large outputs under ignored local directories;
- record exact commits, seeds, configs, manifests, and validation gates before
  any heavy run;
- update the article only after simulation design and implementation gates are
  documented.

Writing/style constraints:
Read STYLE_PROFILE.md before manuscript edits. Keep the prose compact,
technical, and Bayesian-statistics precise. Do not overclaim. Distinguish:
RQR interval-root learning under a generalized Bayes loss update,
Q-DESN quantile-ordinate learning, and any future response simulation contract.

Protected scope:
Do not mutate the Q-DESN article repo or the exdqlm reference repo from this
chat unless explicitly instructed. Treat them as references. Work should happen
inside RQR-GIBBS unless the user explicitly opens a separate scoped task.

Deliverable for the first Jerez chat:
A concise but rigorous health-check table, a list of exact repo states, and a
next-stage implementation plan. Do not launch heavy simulations until the user
confirms the plan.
```
