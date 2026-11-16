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
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit study source before launch.\n' >&2; exit 65; }
output=application/outputs/mti_gibbs_boundary_calibration_20261115
if "$foreground"; then
  mkdir -p "$output"
  exec 9>"$output/controller.lock"
  flock -n 9 || { printf 'The study controller is already running.\n' >&2; exit 73; }
  export MTI_PRIOR_R_LIBRARY="$PWD/application/cache/fixed_prior_library"
  rm -f "$output/controller_exit.json"
  printf '%s\n' "$$" > "$output/controller.pid"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
  Rscript application/scripts/126_assess_mti_gibbs_boundary_calibration.R pipeline
  exit 0
fi
common=$(realpath "$(git rev-parse --git-common-dir)")
root=$(dirname "$common")
launch="$root/.codex_work/mti_gibbs_boundary_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$launch/$output" "$launch/application/cache/fixed_prior_library" "$launch/application/logs"
exec 8>"$launch/$output/launch.lock"
flock -n 8 || { printf 'Another launch is being prepared.\n' >&2; exit 73; }
flock -n "$launch/$output/controller.lock" true || { printf 'The controller is already running.\n' >&2; exit 73; }
parent="$root/.codex_work/mti_fixed_prior_9b53485fc9e4d6ab43a92d80a1b0931bf58c910e/application/cache/fixed_prior_library"
for package in rqrgibbs tolerance; do
  [[ -d "$parent/$package" ]] || { printf 'Pinned installed package missing: %s\n' "$package" >&2; exit 65; }
  if [[ ! -d "$launch/application/cache/fixed_prior_library/$package" ]]; then
    cp -a "$parent/$package" "$launch/application/cache/fixed_prior_library/$package"
  fi
done
Rscript -e 'a <- commandArgs(TRUE); p <- file.path(a[1], ".codex_work/mti_gibbs_boundary_latest.json"); t <- paste0(p, ".tmp"); jsonlite::write_json(list(study_id="mti_gibbs_boundary_calibration_20261115", checkout=a[2], output_root=file.path(a[2], a[3]), source_commit=a[4]), t, auto_unbox=TRUE, pretty=TRUE); stopifnot(file.rename(t,p))' \
  "$root" "$launch" "$output" "$commit"
setsid --fork bash "$launch/application/scripts/127_launch_mti_gibbs_boundary_calibration.sh" --foreground \
  >> "$launch/application/logs/mti_gibbs_boundary_calibration_20261115.log" 2>&1 < /dev/null 8>&-
printf 'Source: %s\nFrozen checkout: %s\nOutputs: %s/%s\nMaximum workers: 40\n' "$commit" "$launch" "$launch" "$output"
