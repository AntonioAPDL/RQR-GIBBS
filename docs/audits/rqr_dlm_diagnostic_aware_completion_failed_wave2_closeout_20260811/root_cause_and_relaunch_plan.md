# RQR-DLM diagnostic-aware run: root cause and relaunch plan

Date: 2026-08-11

Run root:

```text
/data/muscat_data/jaguir26/.rqr_gibbs_frozen_runs/rqr_dlm_diagnostic_aware_completion_20260810_ca4575e
```

Run commit:

```text
ca4575e0188f0cdf200e781b39e47cb8e72b53c5
```

## Executive diagnosis

The run stopped at canonical wave 2 because two long M01 local-level
replications failed the end-of-fit provenance promotion gate.  The recorded
message digest

```text
3e8f305d695e6ac5d4a60b5ebb207a0303d1a1475777e759b6a20cb45e86ec2d
```

is the SHA-256 digest of

```text
A fitted method failed runtime provenance eligibility.
```

The stopped wave had no diagnostic failures among the diagnostics that were
successfully constructed:

```text
6370 / 6370 diagnostics passed
```

The failed tasks were:

| DGP | replication | method | status |
| --- | ----------: | ------ | ------ |
| S03 | 117 | M01 | provenance gate failure |
| S03 | 121 | M01 | provenance gate failure |

This is an execution-isolation failure, not evidence that the RQR-DLM
generalized-Bayes target, C++ FFBS kernel, component-scale transition, or
MCMC diagnostic contract is statistically invalid.

## Root cause

The isolated package runtime was valid when preflight and reference artifacts
were created.  The attestation bound the runtime to the source worktree:

```text
/data/muscat_data/jaguir26/.rqr_gibbs_worktrees/oracle_tilt_v3_process_isolation_20260801
```

at commit

```text
ca4575e0188f0cdf200e781b39e47cb8e72b53c5
```

The multi-day simulation was then allowed to continue while the same source
worktree advanced to later commits.  Per-fit provenance recomputes the
source-worktree Git state at fit completion.  Shorter cells that finished
before the worktree advanced remained eligible; longer M01 cells that finished
after the worktree advanced lost runtime/source attestation eligibility and
triggered the fail-closed worker stop.

A short reproduction at the exact `ca4575e` source state with the archived
runtime recovered the failing subcondition:

```text
reproducibility_eligible        FALSE
provenance_complete             TRUE
git_dirty                       FALSE
expected_git_commit_match       TRUE
primary_runtime_source_match    FALSE
runtime_attestation_match       FALSE
```

In that reproduction the mismatch was expected because the runtime attestation
recorded the original source path, while the reproduction used a different
worktree at the same commit.  The same path-sensitive attestation mechanism
explains the actual failed run after the originally bound source worktree
advanced during execution.

## Implemented correction

The runtime-binding helpers now support an explicit detached launch-source
contract:

```text
RQR_ALLOW_DETACHED_LAUNCH_SOURCE=TRUE
```

When this opt-in is absent, promotion-grade scripts retain the prior strict
requirement: clean `main` at the expected SHA.  When the opt-in is present, the
source may instead be a clean detached worktree at the expected SHA.  This
preserves the existing guardrails for ordinary reference work while allowing
long runs to remain path-stable even if the editable main worktree advances.

The helper

```text
application/scripts/66_prepare_rqr_dlm_frozen_launch_source.sh
```

creates or verifies such a detached launch worktree, builds the exact isolated
runtime from the committed `application/` subtree, and writes an environment
file under ignored cache space with the required runtime variables.

## Relaunch contract

Do not reuse partial outputs from the failed run as confirmatory evidence.
They remain useful only as failure evidence and timing/resource context.

Before relaunch:

1. Commit the source-isolation patch and this closeout.
2. Create a dedicated detached launch worktree at the new committed SHA with
   `application/scripts/66_prepare_rqr_dlm_frozen_launch_source.sh`.
3. Build the primary isolated runtime from that detached worktree.
4. Run preflight and reference artifacts from the detached worktree.
5. Prepare a new commit-bound authorization bundle using those fresh
   artifacts.
6. Launch a fresh run under a new run ID and output root.
7. Do not edit, switch, clean, or remove the detached launch worktree until the
   run completes and its final audit is written.

The launch-source worktree is operational infrastructure.  It does not change
the statistical estimand: RQR-DLM remains a loss-based generalized-Bayes update
for interval-root state paths, not an ordinary response-likelihood model and
not a posterior-predictive response simulator.
