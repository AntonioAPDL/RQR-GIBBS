# Wave-2 M03/M08 candidate-selection closeout

Date: 2026-07-31

Source commit: `c2d560d761aae35554cadfe417e11a65ef540043`

Authorization changed: no

Promotion evidence: no

The two clean development comparisons completed successfully and their
recursive artifact manifests verified. They used the reviewed maximum seed
ledger with SHA-256
`3dc8483f4a777ab766704b901997295bed1c89db0590429a70f3116b233e948f`.

## Decisions

- M03 selected `M03_REX4_B500_R1500`: all four exact replica-exchange
  ladders passed every unchanged hard/guard diagnostic, and REX4 had the
  smallest declared primitive transition cost.
- M08 selected `M08_uniform_B1000_R4000`: the original schedule reproduced
  the known marginal failure and the uniform extension passed the hard and two
  guard replications.

All selected fits used the exact declared target and reported zero numerical
repairs. Scientific performance metrics were not used. Raw chain evidence and
full local manifests remain under ignored `application/cache/` roots and are
not promotion or article evidence.

## Local evidence identities

The input hashes are recorded in `evidence_hashes.csv`. The selected case-level
diagnostics are in `selected_job_summary.csv`, and the deterministic least-cost
decision is in `candidate_decisions.csv`.

The RQR update remains a generalized-Bayes interval-root update. Replica
exchange tempers its loss contribution; it does not create an ordinary response
likelihood or posterior-predictive response draws.
