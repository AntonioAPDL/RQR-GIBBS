# RQR-DLM frozen launch-source runbook

Date: 2026-08-11

Purpose: long confirmatory RQR-DLM runs must not depend on an editable source
worktree that may advance while chains are still running.  Use a detached,
commit-exact launch worktree and build the isolated `rqrgibbs` runtime from
that same path.

## Required source state

Run from a clean committed RQR-GIBBS source state.  Record:

```bash
git rev-parse HEAD
git status --short --branch
```

Do not launch from an uncommitted worktree.

## Prepare the frozen launch source

From the ordinary clean repository root, after choosing the committed SHA:

```bash
expected_commit="$(git rev-parse HEAD)"
launch_root="/data/muscat_data/jaguir26/.rqr_gibbs_launch_sources/rqr_dlm_${expected_commit}"

application/scripts/66_prepare_rqr_dlm_frozen_launch_source.sh \
  "$expected_commit" \
  "$launch_root"
```

The script verifies that `launch_root` is a clean detached worktree at
`expected_commit`, builds an isolated package runtime from
`expected_commit:application`, and writes:

```text
<launch_root>/application/cache/rqr_dlm_launch_sources/launch_env_<commit>.sh
```

Source that file before preflight, reference, authorization, promotion checks,
or execution:

```bash
source "$launch_root/application/cache/rqr_dlm_launch_sources/launch_env_${expected_commit}.sh"
cd "$launch_root"
```

The environment file prepends the isolated `rqrgibbs` runtime through
`R_LIBS` and leaves `R_LIBS_USER` untouched so ordinary dependency libraries
remain visible.

## Guardrail

Do not edit, switch, clean, remove, or reuse the detached launch worktree while
the run is active.  The fit-level provenance gate intentionally rechecks the
attested source path at fit completion.

The ordinary editable `main` worktree may continue to receive manuscript,
audit, and planning commits.  Those edits must not happen in the detached
launch worktree.

## Relaunch sequence

Use a new run ID and a fresh ignored output root.  Do not reuse partial outputs
from any failed run as confirmatory evidence.

1. Prepare the detached launch source and isolated runtime.
2. Run the normal DLM preflight from the detached launch source.
3. Run the normal reference stage from the detached launch source.
4. Prepare a new commit-bound authorization bundle from those fresh artifacts.
5. Run promotion checks from the detached launch source.
6. Launch the fresh simulation from the detached launch source.
7. Monitor progress with the existing health/check scripts.
8. After completion, run the final audit and commit only compact source,
   manifests, tables, and documentation.

## Statistical scope

This runbook changes only execution isolation.  It does not change the
RQR-DLM target, MCMC kernels, diagnostics, response-generation design, or
interpretation.  The fitted quantities remain loss-based generalized-Bayes
interval-root summaries, not ordinary posterior-predictive response draws.
