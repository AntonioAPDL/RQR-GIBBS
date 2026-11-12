#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
confirmation=""
workers=50
foreground=false
dry_run=false
output=""
for argument in "$@"; do
  case "$argument" in
    --confirm-article-commit=*) confirmation="${argument#*=}" ;;
    --workers=*) workers="${argument#*=}" ;;
    --output-dir=*) output="${argument#*=}" ;;
    --foreground) foreground=true ;;
    --dry-run) dry_run=true ;;
    *) printf 'Unknown argument: %s\n' "$argument" >&2; exit 64 ;;
  esac
done
commit=$(git rev-parse HEAD)
[[ "$confirmation" == "$commit" && -z "$(git status --porcelain)" ]] || exit 65
common=$(realpath "$(git rev-parse --git-common-dir)")
base=$(dirname "$common")
launch="$base/.codex_work/mti_gibbs_practical_launch_$commit"
[[ -n "$output" ]] || output="$launch/application/outputs/mti_gibbs_practical_20261112"
[[ "$output" == /* ]] || exit 64
if "$dry_run"; then
  printf 'Frozen source: %s\nCommit: %s\nOutput: %s\nWorkers: %s\n' "$launch" "$commit" "$output" "$workers"
  exit 0
fi
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 BLIS_NUM_THREADS=1
if "$foreground"; then
  mkdir -p "$output"
  exec 9>"$output/controller.lock"
  flock -n 9 || exit 73
  parent_library="$base/.codex_work/mti_gibbs_launch_44afea30784540ae913b0a558a5c572790fd7493/application/cache/mti_gibbs_runtime_44afea30784540ae913b0a558a5c572790fd7493/library"
  test -d "$parent_library/rqrgibbs"
  export R_LIBS="$parent_library"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
  Rscript application/scripts/108_assess_mti_gibbs_practical.R --mode=campaign \
    --confirm-article-commit="$commit" --workers="$workers" --output-dir="$output"
  exit 0
fi
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
[[ -z "$(git -C "$launch" status --porcelain)" ]] || exit 65
if [[ ! -e "$launch/application/cache" ]]; then ln -s "$PWD/application/cache" "$launch/application/cache"; fi
mkdir -p "$output"
nohup setsid bash "$launch/application/scripts/109_launch_mti_gibbs_practical.sh" --foreground \
  --confirm-article-commit="$commit" --workers="$workers" --output-dir="$output" \
  >> "$output/controller.log" 2>&1 < /dev/null &
pid=$!
printf '%s\n' "$pid" > "$output/controller.pid"
printf 'Controller PID: %s\nOutput: %s\n' "$pid" "$output"
