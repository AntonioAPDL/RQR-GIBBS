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
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit the study source before launch.\n' >&2; exit 65; }
output=application/outputs/mti_fixed_prior_20261114
if "$foreground"; then
  mkdir -p "$output"
  exec 9>"$output/controller.lock"
  flock -n 9 || { printf 'The study controller is already running.\n' >&2; exit 73; }
  rm -f "$output/controller_exit.json"
  printf '%s\n' "$$" > "$output/controller.pid"
  export MTI_PRIOR_R_LIBRARY="$PWD/application/cache/fixed_prior_library"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
  Rscript application/scripts/119_validate_mti_fixed_prior.R pipeline
  exit 0
fi
common=$(realpath "$(git rev-parse --git-common-dir)")
launch="$(dirname "$common")/.codex_work/mti_fixed_prior_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$launch/$output" "$launch/application/logs/mti_fixed_prior_20261114"
exec 8>"$launch/$output/launch.lock"
flock -n 8 || { printf 'Another launch is being prepared.\n' >&2; exit 73; }
flock -n "$launch/$output/controller.lock" true || { printf 'The study controller is already running.\n' >&2; exit 73; }
library="$launch/application/cache/fixed_prior_library"
mkdir -p "$library"
for package in rqrgibbs tolerance; do
  if [[ ! -d "$library/$package" ]]; then
    installed=$(Rscript -e "cat(system.file(package='$package'))")
    [[ -d "$installed" ]] || exit 65
    cp -a "$installed" "$library/$package"
  fi
done
setsid --fork bash "$launch/application/scripts/120_launch_mti_fixed_prior.sh" --foreground \
  >> "$launch/application/logs/mti_fixed_prior_20261114/run.log" 2>&1 < /dev/null 8>&-
printf 'Commit: %s\nFrozen worktree: %s\nOutputs: %s/%s\nMaximum workers: 40\n' "$commit" "$launch" "$launch" "$output"
