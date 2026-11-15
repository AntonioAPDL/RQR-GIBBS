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
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit the continuation source before launch.\n' >&2; exit 65; }
output=application/outputs/mti_fixed_prior_closure_20261115
if "$foreground"; then
  mkdir -p "$output"
  exec 9>"$output/controller.lock"
  flock -n 9 || { printf 'The continuation controller is already running.\n' >&2; exit 73; }
  rm -f "$output/controller_exit.json"
  printf '%s\n' "$$" > "$output/controller.pid"
  export MTI_PRIOR_R_LIBRARY="$PWD/application/cache/fixed_prior_library"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
  Rscript application/scripts/121_complete_mti_fixed_prior.R run
  exit 0
fi
common=$(realpath "$(git rev-parse --git-common-dir)")
launch="$(dirname "$common")/.codex_work/mti_fixed_prior_closure_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$launch/$output" "$launch/application/logs/mti_fixed_prior_closure_20261115"
exec 8>"$launch/$output/launch.lock"
flock -n 8 || { printf 'Another continuation launch is being prepared.\n' >&2; exit 73; }
flock -n "$launch/$output/controller.lock" true || { printf 'The continuation controller is already running.\n' >&2; exit 73; }
parent=$(Rscript -e 'x <- jsonlite::read_json("application/config/mti_fixed_prior_closure_20261115.json"); cat(x$parent_worktree)')
library="$launch/application/cache/fixed_prior_library"
mkdir -p "$library"
for package in rqrgibbs tolerance; do
  installed="$(dirname "$common")/$parent/application/cache/fixed_prior_library/$package"
  [[ -d "$installed" ]] || { printf 'The frozen parent package is missing: %s\n' "$package" >&2; exit 65; }
  if [[ ! -d "$library/$package" ]]; then cp -a "$installed" "$library/$package"; fi
done
setsid --fork bash "$launch/application/scripts/122_launch_mti_fixed_prior_closure.sh" --foreground \
  >> "$launch/application/logs/mti_fixed_prior_closure_20261115/run.log" 2>&1 < /dev/null 8>&-
printf 'Commit: %s\nFrozen continuation: %s\nOutputs: %s/%s\nParent simulation is not restarted.\nMaximum application workers: 40\n' \
  "$commit" "$launch" "$launch" "$output"
