#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
commit=$(git rev-parse HEAD)
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit the source before preparing the runtime.\n' >&2; exit 65; }
cache="$PWD/application/cache"
runtime="$cache/mti_gibbs_runtime_$commit"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
if [[ -e "$runtime/commit.txt" ]]; then
  test "$(<"$runtime/commit.txt")" = "$commit"
  R_LIBS="$runtime/library" Rscript application/scripts/105_compare_mti_gibbs_ecm.R --mode=runtime
  exit 0
fi
common=$(realpath "$(git rev-parse --git-common-dir)")
launch="$(dirname "$common")/.codex_work/mti_gibbs_launch_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$cache" "$runtime"
export RQR_EXPECTED_PRIMARY_COMMIT="$commit"
export RQR_ALLOW_DETACHED_LAUNCH_SOURCE=TRUE
export RQR_PRIMARY_RUNTIME_ROOT="$cache/mti_gibbs_attested"
cd "$launch"
attested="$RQR_PRIMARY_RUNTIME_ROOT/$commit"
if [[ ! -f "$attested/attestations/rqrgibbs_$commit.rds" ]]; then
  if [[ -L "$launch/application/cache" ]]; then
    [[ "$(realpath "$launch/application/cache")" == "$(realpath "$cache")" ]] || exit 65
    rm "$launch/application/cache"
  fi
  Rscript application/scripts/04_prepare_primary_runtime.R
fi
test -f "$attested/attestations/rqrgibbs_$commit.rds"
if [[ ! -e "$runtime/library" ]]; then ln -s "$attested/library" "$runtime/library"; fi
if [[ ! -e "$launch/application/cache" ]]; then ln -s "$cache" "$launch/application/cache"; fi
printf '%s\n' "$attested/attestations/rqrgibbs_$commit.rds" > "$runtime/attestation_path.txt"
printf '%s\n' "$commit" > "$runtime/commit.txt"
if ! R_LIBS="$runtime/library" Rscript application/scripts/105_compare_mti_gibbs_ecm.R --mode=runtime; then
  rm "$runtime/commit.txt"
  exit 65
fi
printf 'Prepared attested runtime without sampling: %s/library\n' "$runtime"
