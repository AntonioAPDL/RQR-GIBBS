#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"
run=${1:?Provide the frozen study directory}
output=${2:-application/outputs/mti_profile_review_20261021/final}
run=$(cd "$run" && pwd)
[[ -f "$run/launch.lock" && -f "$run/manifest.json" ]]
[[ -z "$(git status --porcelain)" ]] || {
  printf '%s\n' 'Commit the reviewed reporting source before queuing a final assessment.' >&2
  exit 1
}
[[ "$output" != *".."* && "$output" == application/outputs/* ]]
mkdir -p "$output"
output=$(cd "$output" && pwd)
if [[ -f "$output/report.pid" ]] && kill -0 "$(<"$output/report.pid")" 2>/dev/null; then
  printf '%s\n' 'A completion-dependent report is already queued or running.' >&2
  exit 1
fi
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
nohup setsid flock -n "$output/report_job.lock" bash -c '
  set +e
  flock -s "$1/launch.lock" Rscript "$2/application/scripts/95_review_mti_profile_revision.R" --run-dir="$1" --output-dir="$3"
  status=$?
  printf "{\"exit_status\":%s,\"finished_utc\":\"%s\"}\n" "$status" "$(date -u +%FT%TZ)" > "$3/report_exit.json"
  exit "$status"
' report-after-study "$run" "$root" "$output" >> "$output/report.log" 2>&1 < /dev/null &
printf '%s\n' "$!" > "$output/report.pid"
printf 'Queued one report, PID %s. No refitting or polling. Log: %s/report.log\n' "$!" "$output"
