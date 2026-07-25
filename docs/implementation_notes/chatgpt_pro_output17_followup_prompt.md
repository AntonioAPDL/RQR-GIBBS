# ChatGPT Pro Output-17 independent launch review

Use ordinary web access or the connected read-only GitHub app. Do not require
an upload and do not attempt any GitHub write operation.

Repository:

```text
https://github.com/AntonioAPDL/RQR-GIBBS
```

The user will provide the complete reconciliation commit SHA with this prompt.
Download that immutable public snapshot, verify that it contains this file,
and treat the following scientific implementation as authoritative:

```text
aa1ded8c5b4db2257c6985c73626e1a0a252fc72
```

Read completely:

```text
AGENTS.md
external_reviews/chatgpt_pro_output16_20260725/
docs/audits/chatgpt_pro_output16_reconciliation_20260725.md
docs/audits/rqr_dlm_output16_reconciliation_evidence_20260725/
application/config/rqr_dlm/rqr_dlm_main_simulation_20260724.R
application/scripts/lib/rqr_dlm_confirmatory_simulation.R
application/scripts/15_run_rqr_dlm_confirmatory_simulation.R
application/scripts/17_launch_rqr_dlm_confirmatory_wave.R
application/scripts/18_orchestrate_rqr_dlm_confirmatory_simulation.R
application/scripts/19_prepare_rqr_dlm_confirmatory_authorization.R
application/scripts/20_launch_rqr_dlm_confirmatory_simulation.sh
application/scripts/21_healthcheck_rqr_dlm_confirmatory_simulation.R
application/tests/testthat/test-rqr-dlm-confirmatory-contract.R
```

Treat all repository reports and recorded checks as claims to audit. Do not
modify RQR-GIBBS, exdqlm, or the Q-DESN article repository. Preserve these
fixed interpretations:

- RQR is a loss-based generalized-Bayes interval-root update, not an ordinary
  response likelihood.
- RQR root/state draws are not posterior-predictive response draws.
- The main study assesses repeated-sampling operating characteristics only
  under the frozen design.
- The embedded sentinels are part of the main confirmatory study; do not
  request a disposable pilot.
- CAVI/ELBO and RQR-DESN remain deferred.

## Primary review questions

Independently determine whether Output-16 blockers WAV-001 and DIA-001 are
actually closed.

For WAV-001, try to construct concrete counterexamples involving:

1. launching a standard wave before its sentinel;
2. launching after a failed or incomplete wave;
3. skipping or replaying a canonical wave;
4. using another run, authorization commit, runtime, task plan, or artifact
   manifest;
5. using an add-batch decision for the wrong preceding or next target;
6. launching any future wave for a precision-stopped group;
7. altering an earlier completion, predecessor manifest, output manifest, or
   resumed task plan;
8. resuming at a batch, final-collection, or final-audit boundary;
9. bypassing the coordinator by invoking script 17 directly.

Confirm that every launched or skipped wave records the requested raw
identities, worker/task counts, decisions, and hashes, and that only the next
canonical wave can create an output root.

For DIA-001, verify method by method that:

1. the required diagnostic schema is explicit rather than inferred by column
   intersection;
2. selected training-time lower, upper, midpoint, and width functions cover
   first/last, break, missingness, and scale boundaries when present;
3. dynamic terminal functions are required;
4. future conditional-mean root functions are required at horizons 1, 5, 10,
   and 20;
5. future propagation uses retained terminal states with no process noise and
   no generated response;
6. learned `lambda` and every applicable component scale are required;
7. fixed-lambda constants remain outside stochastic diagnostics;
8. omitted required fields and compensating time paths fail deterministic
   tests.

Also audit:

- the new authorization materializer and detached/resume-safe launch path;
- the propagation of verified precision stops over every later batch;
- exact-runtime bindings and compact evidence hashes;
- the fact that the configuration flag is still `FALSE`;
- that no confirmatory fit or disposable pilot was run;
- that the protected exdqlm and Q-DESN repositories remain references only.

Do not reopen previously accepted design, DGP, RNG, oracle, comparator,
runtime-lineage, or ADEMP decisions unless you identify a concrete
source-level counterexample in the current implementation.

## Required decision

Return a rigorous but compact response directly in the chat; do not create a
ZIP and do not ask the user to transfer files. Include:

1. the reconciliation SHA actually read;
2. the authoritative implementation SHA above;
3. a finding table with exact source locations and reproducible
   counterexamples for any failure;
4. explicit `PASS`, `PARTIAL`, or `FAIL` decisions for WAV-001 and DIA-001;
5. one of these exact launch decisions:

```text
CREATE FLAG-ONLY AUTHORIZATION COMMIT: GO
CREATE FLAG-ONLY AUTHORIZATION COMMIT: NO-GO
```

6. whether the post-authorization sequence below is correct:

```text
one-line flag commit
-> isolated runtime rebuilt from that exact commit
-> 22-gate preflight rerun
-> 15-gate oracle/reference rerun
-> authorization bundle materialized
-> complete main confirmatory coordinator launched
-> read-only health checks until final audit
```

If the decision is GO, state clearly that no additional pilot or another Pro
review is required before Codex performs that exact post-authorization
sequence. If the decision is NO-GO, give only concrete, launch-blocking source
defects and their smallest justified corrections.
