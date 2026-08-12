#!/usr/bin/env bash
set -euo pipefail

# Prepare a dedicated, detached, clean source worktree for long RQR-DLM
# confirmatory runs.  The fitted package is still built from an exact Git
# archive and installed into an ignored, disjoint runtime library.  The purpose
# of the detached source worktree is to keep per-fit provenance checks stable
# while the editable main worktree continues to receive manuscript or audit
# commits.

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <expected-commit> <launch-worktree-path>" >&2
  exit 64
fi

expected_commit="${1,,}"
launch_worktree="$(realpath -m "$2")"
repo_root="$(pwd -P)"

if [[ ! "$expected_commit" =~ ^[0-9a-f]{40}$ ]]; then
  echo "The expected commit must be a complete lowercase SHA." >&2
  exit 64
fi
if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this script from the RQR-GIBBS repository root." >&2
  exit 64
fi
for command in git Rscript realpath; do
  if ! command -v "$command" >/dev/null 2>&1; then
    echo "Missing required command: $command" >&2
    exit 69
  fi
done
git -C "$repo_root" cat-file -e "${expected_commit}^{commit}"

if [[ -e "$launch_worktree" ]]; then
  if [[ ! -d "$launch_worktree/.git" && ! -f "$launch_worktree/.git" ]]; then
    echo "Existing launch path is not a Git worktree: $launch_worktree" >&2
    exit 65
  fi
else
  mkdir -p "$(dirname "$launch_worktree")"
  git -C "$repo_root" worktree add --detach "$launch_worktree" "$expected_commit"
fi

head_commit="$(git -C "$launch_worktree" -c core.fsmonitor=false rev-parse HEAD)"
branch="$(git -C "$launch_worktree" -c core.fsmonitor=false rev-parse --abbrev-ref HEAD)"
tracked_status="$(
  git -C "$launch_worktree" -c core.fsmonitor=false \
    status --porcelain=v2 --untracked-files=all
)"
if [[ "$head_commit" != "$expected_commit" ||
      "$branch" != "HEAD" ||
      -n "$tracked_status" ]]; then
  echo "The launch source must be a clean detached worktree at the expected commit." >&2
  exit 65
fi

runtime_root="${RQR_PRIMARY_RUNTIME_ROOT:-$(dirname "$launch_worktree")/.rqr_gibbs_primary_runtime}"
runtime_root="$(realpath -m "$runtime_root")"
attestation_path="$runtime_root/$expected_commit/attestations/rqrgibbs_${expected_commit}.rds"
runtime_library="$runtime_root/$expected_commit/library"

(
  cd "$launch_worktree"
  RQR_ALLOW_DETACHED_LAUNCH_SOURCE=TRUE \
  RQR_EXPECTED_PRIMARY_COMMIT="$expected_commit" \
  RQR_PRIMARY_RUNTIME_ROOT="$runtime_root" \
    Rscript application/scripts/04_prepare_primary_runtime.R
)

if [[ ! -f "$attestation_path" ]]; then
  echo "Primary runtime attestation was not produced: $attestation_path" >&2
  exit 70
fi
if [[ ! -d "$runtime_library/rqrgibbs" ]]; then
  echo "Primary runtime library was not produced: $runtime_library" >&2
  exit 70
fi

env_dir="$launch_worktree/application/cache/rqr_dlm_launch_sources"
mkdir -p "$env_dir"
env_file="$env_dir/launch_env_${expected_commit}.sh"
{
  printf 'export RQR_ALLOW_DETACHED_LAUNCH_SOURCE=TRUE\n'
  printf 'export RQR_EXPECTED_PRIMARY_COMMIT=%q\n' "$expected_commit"
  printf 'export RQR_PRIMARY_RUNTIME_ATTESTATION=%q\n' "$attestation_path"
  printf 'export RQR_PRIMARY_RUNTIME_ROOT=%q\n' "$runtime_root"
  printf 'export RQR_PRIMARY_RUNTIME_LIBRARY=%q\n' "$runtime_library"
  printf 'if [ -n "${R_LIBS:-}" ]; then\n'
  printf '  export R_LIBS=%q:"${R_LIBS}"\n' "$runtime_library"
  printf 'else\n'
  printf '  export R_LIBS=%q\n' "$runtime_library"
  printf 'fi\n'
} >"$env_file"

printf 'launch_worktree,%s\n' "$launch_worktree"
printf 'expected_commit,%s\n' "$expected_commit"
printf 'runtime_library,%s\n' "$runtime_library"
printf 'primary_attestation,%s\n' "$attestation_path"
printf 'env_file,%s\n' "$env_file"
