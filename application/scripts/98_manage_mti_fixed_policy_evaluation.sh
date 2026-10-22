#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"
mode=${1:-health}
calibration=${2:-}
review=${3:-}
run=${4:-application/outputs/mti_fixed_policy_evaluation_20261021/independent_20261021}
workers=${5:-1}
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
runner=application/scripts/97_run_mti_fixed_policy_evaluation.R
if [[ "$mode" == health ]]; then
  Rscript "$runner" --mode=health --run-dir="$run"
  if [[ -f "$run/launcher.pid" ]]; then
    ps -p "$(<"$run/launcher.pid")" -o pid,etime,stat,args || true
  fi
  exit 0
fi
[[ "$mode" == launch || "$mode" == resume || "$mode" == preflight || "$mode" == report ]]
[[ -n "$calibration" && -n "$review" && -f "$calibration/identity.rds" ]]
[[ "$workers" =~ ^[0-9]+$ && "$workers" -ge 1 && "$workers" -le 40 ]]
if [[ -n "$(git status --porcelain)" ]]; then
  printf '%s\n' 'Commit reviewed source before preflight or launch.' >&2
  exit 1
fi
export R_LIBS
R_LIBS=$(Rscript -e 'x<-readRDS(commandArgs(TRUE)[1]);cat(dirname(dirname(dirname(x$package_R_hashes$path[1]))))' "$calibration/identity.rds")
Rscript "$runner" --mode=preflight --calibration-run="$calibration" \
  --calibration-review="$review" --run-dir="$run" --workers="$workers"
if [[ "$mode" == preflight ]]; then exit 0; fi
# Resolve the validated output path before creating any launcher files.
run=$(Rscript -e 'source("application/scripts/lib/mti_profile_review.R");cat(mpr_output_path(commandArgs(TRUE)[1]))' "$run")
allowed="$root/application/outputs/mti_fixed_policy_evaluation_20261021/"
[[ "$run" == "$allowed"* ]]
mkdir -p "$run"
exec 9>"$run/launch.lock"
if ! flock -n 9; then
  printf '%s\n' 'Another fit or report holds the evaluation lock.' >&2
  exit 1
fi
if [[ "$mode" == report ]]; then
  Rscript "$runner" --mode=report --calibration-run="$calibration" \
    --calibration-review="$review" --run-dir="$run" --workers="$workers"
  exit 0
fi
if [[ -f "$run/launcher.pid" ]] && kill -0 "$(<"$run/launcher.pid")" 2>/dev/null; then
  printf '%s\n' 'An evaluation launcher is already active.' >&2
  exit 1
fi
if [[ "$mode" == resume ]]; then
  reason=${6:-}
  [[ -n "$reason" && -f "$run/identity.rds" && -f "$run/launcher_exit.json" && ! -f "$run/evaluation.json" ]]
  Rscript -e 'stopifnot(jsonlite::read_json(commandArgs(TRUE)[1])$exit_status!=0)' "$run/launcher_exit.json"
  receipt="$run/attempts/$(date -u +%Y%m%dT%H%M%S%NZ)"
  mkdir -p "$receipt"
  mv "$run/launcher_exit.json" "$receipt/launcher_exit.json"
  printf '%s\n' "$reason" > "$receipt/resume_reason.txt"
elif [[ -f "$run/launcher_exit.json" ]]; then
  printf '%s\n' 'This attempt has a terminal receipt. Review it before an explicit resume.' >&2
  exit 1
fi
nohup setsid bash -c '
  set +e
  Rscript "$1" --mode=run --calibration-run="$2" --calibration-review="$3" --run-dir="$4" --workers="$5"
  status=$?
  temporary="$4/launcher_exit.json.tmp"
  printf "{\"exit_status\":%s,\"finished_utc\":\"%s\"}\n" "$status" "$(date -u +%FT%TZ)" > "$temporary"
  mv "$temporary" "$4/launcher_exit.json"
  exit "$status"
' independent-evaluation "$runner" "$calibration" "$review" "$run" "$workers" \
  >> "$run/launch.log" 2>&1 < /dev/null &
printf '%s\n' "$!" > "$run/launcher.pid"
printf 'Launched independent comparison PID %s, %s single-threaded workers; log: %s/launch.log\n' \
  "$!" "$workers" "$run"
