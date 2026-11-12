#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
confirmation=""
workers=50
output=""
dry_run=false
foreground=false
allow_full=false
resume=false
for argument in "$@"; do
  case "$argument" in
    --confirm-article-commit=*) confirmation="${argument#*=}" ;;
    --workers=*) workers="${argument#*=}" ;;
    --output-dir=*) output="${argument#*=}" ;;
    --allow-full=true) allow_full=true ;;
    --dry-run) dry_run=true ;;
    --foreground) foreground=true ;;
    --resume) resume=true ;;
    *) printf 'Unknown argument: %s\n' "$argument" >&2; exit 64 ;;
  esac
done
commit=$(git rev-parse HEAD)
[[ "$confirmation" == "$commit" && "$allow_full" == true ]] || {
  printf 'Confirm the exact committed source and authorize the full study.\n' >&2; exit 65;
}
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit source before launch.\n' >&2; exit 65; }
[[ "$workers" =~ ^[0-9]+$ ]] && (( workers >= 1 && workers <= 50 )) || exit 64
root=$(dirname "$(realpath "$(git rev-parse --git-common-dir)")")
launch="$root/.codex_work/mti_gibbs_full_launch_$commit"
[[ -n "$output" ]] || output="$launch/application/outputs/mti_gibbs_full_comparison_20261111"
[[ "$output" == /* ]] || exit 64
if "$dry_run"; then
  printf 'Source: %s\nCommit: %s\nOutput: %s\nMaximum workers: %s\nResume: %s\n' \
    "$launch" "$commit" "$output" "$workers" "$resume"
  exit 0
fi
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
if "$foreground"; then
  mkdir -p "$output"
  exec 9>"$output/controller.lock"
  flock -n 9 || { printf 'Another controller owns this campaign.\n' >&2; exit 73; }
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
  mode=campaign
  if "$resume"; then mode=resume; fi
  Rscript application/scripts/111_run_mti_gibbs_full_comparison.R --mode="$mode" \
    --confirm-article-commit="$commit" --allow-full=true --workers="$workers" --output-dir="$output"
  exit 0
fi
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" && -z "$(git -C "$launch" status --porcelain)" ]] || exit 65
if [[ ! -e "$launch/application/cache" ]]; then ln -s "$PWD/application/cache" "$launch/application/cache"; fi
mkdir -p "$output"
options=()
if "$resume"; then options+=(--resume); fi
nohup setsid bash "$launch/application/scripts/112_launch_mti_gibbs_full_comparison.sh" \
  --foreground --confirm-article-commit="$commit" --allow-full=true --workers="$workers" \
  --output-dir="$output" "${options[@]}" >> "$output/controller.log" 2>&1 < /dev/null &
pid=$!
printf '%s\n' "$pid" > "$output/controller.pid"
printf 'Controller PID: %s\nFrozen commit: %s\nOutput: %s\n' "$pid" "$commit" "$output"
