#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
foreground=false
dry_run=false
allow_full=false
confirmation=""
workers=50
output=""
for argument in "$@"; do
  case "$argument" in
    --foreground) foreground=true ;;
    --dry-run) dry_run=true ;;
    --allow-full=true) allow_full=true ;;
    --confirm-article-commit=*) confirmation="${argument#*=}" ;;
    --workers=*) workers="${argument#*=}" ;;
    --output-dir=*) output="${argument#*=}" ;;
    *) printf 'Unknown argument: %s\n' "$argument" >&2; exit 64 ;;
  esac
done
commit=$(git rev-parse HEAD)
[[ "$confirmation" == "$commit" ]] || { printf 'Confirm the exact committed article.\n' >&2; exit 65; }
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit the source before launch.\n' >&2; exit 65; }
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
if "$foreground"; then
  [[ -n "$output" ]] || exit 64
  mkdir -p "$output"
  exec 9>"$output/controller.lock"
  flock -n 9 || { printf 'A campaign controller already holds the lock.\n' >&2; exit 73; }
  runtime="$PWD/application/cache/mti_gibbs_runtime_$commit/library"
  export R_LIBS="$runtime"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
  Rscript application/scripts/105_compare_mti_gibbs_ecm.R --mode=campaign \
    --confirm-article-commit="$commit" --workers="$workers" \
    --allow-full="$allow_full" --output-dir="$output"
  exit 0
fi
common=$(git rev-parse --git-common-dir)
common=$(realpath "$common")
launch="$(dirname "$common")/.codex_work/mti_gibbs_launch_$commit"
if [[ -z "$output" ]]; then
  output="$launch/application/outputs/mti_gibbs_ecm_comparison_20261111/campaign"
fi
[[ "$output" == /* ]] || { printf 'Use an absolute output directory.\n' >&2; exit 64; }
if "$dry_run"; then
  printf 'Frozen source: %s\nCommit: %s\nCampaign: %s\nMaximum workers: %s\nFull authorized: %s\n' \
    "$launch" "$commit" "$output" "$workers" "$allow_full"
  exit 0
fi
test -f "application/cache/mti_gibbs_runtime_$commit/commit.txt"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
[[ -z "$(git -C "$launch" status --porcelain)" ]] || exit 65
if [[ ! -e "$launch/application/cache" ]]; then
  ln -s "$PWD/application/cache" "$launch/application/cache"
fi
mkdir -p "$output"
nohup setsid bash "$launch/application/scripts/107_launch_mti_gibbs.sh" --foreground \
  --confirm-article-commit="$commit" --workers="$workers" --allow-full="$allow_full" \
  --output-dir="$output" > "$output/controller.log" 2>&1 < /dev/null &
pid=$!
printf '%s\n' "$pid" > "$output/controller.pid"
printf 'Controller PID: %s\nCampaign: %s\nLog: %s/controller.log\n' "$pid" "$output" "$output"
