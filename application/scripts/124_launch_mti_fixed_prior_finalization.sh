#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
foreground=false
for argument in "$@"; do
  case "$argument" in
    --foreground) foreground=true ;;
    *) printf 'Unknown argument: %s\n' "$argument" >&2; exit 64 ;;
  esac
done
commit=$(git rev-parse HEAD)
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit finalization source before launch.\n' >&2; exit 65; }
output=application/outputs/mti_fixed_prior_finalization_20261115
if "$foreground"; then
  mkdir -p "$output"
  exec 9>"$output/controller.lock"
  flock -n 9 || { printf 'Finalization is already running.\n' >&2; exit 73; }
  rm -f "$output/controller_exit.json"
  printf '%s\n' "$$" > "$output/controller.pid"
  export MTI_PRIOR_R_LIBRARY="$PWD/application/cache/fixed_prior_library"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
  Rscript application/scripts/123_finalize_mti_fixed_prior.R run
  exit 0
fi
common=$(realpath "$(git rev-parse --git-common-dir)")
launch="$(dirname "$common")/.codex_work/mti_fixed_prior_finalization_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$launch/$output" "$launch/application/logs/mti_fixed_prior_finalization_20261115"
exec 8>"$launch/$output/launch.lock"
flock -n 8 || { printf 'Another finalization launch is being prepared.\n' >&2; exit 73; }
flock -n "$launch/$output/controller.lock" true || { printf 'Finalization is already running.\n' >&2; exit 73; }
parent=$(Rscript -e 'x <- jsonlite::read_json("application/config/mti_fixed_prior_finalization_20261115.json"); cat(x$continuation_worktree)')
library="$launch/application/cache/fixed_prior_library"
mkdir -p "$library"
for package in rqrgibbs tolerance; do
  installed="$(dirname "$common")/$parent/application/cache/fixed_prior_library/$package"
  [[ -d "$installed" ]] || exit 65
  if [[ ! -d "$library/$package" ]]; then cp -a "$installed" "$library/$package"; fi
done
setsid --fork bash "$launch/application/scripts/124_launch_mti_fixed_prior_finalization.sh" --foreground \
  >> "$launch/application/logs/mti_fixed_prior_finalization_20261115/run.log" 2>&1 < /dev/null 8>&-
printf 'Commit: %s\nFrozen finalization: %s\nOutputs: %s/%s\nOriginal computation is preserved.\n' "$commit" "$launch" "$launch" "$output"
