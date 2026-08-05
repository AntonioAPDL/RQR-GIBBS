#!/usr/bin/env bash
set -euo pipefail

unit=""
output_dir=""
for argument in "$@"; do
  case "$argument" in
    --unit=*) unit="${argument#--unit=}" ;;
    --output-dir=*) output_dir="${argument#--output-dir=}" ;;
    *) echo "Unknown health-check argument: $argument" >&2; exit 2 ;;
  esac
done
if [[ -z "$unit" || -z "$output_dir" ]]; then
  echo "--unit and --output-dir are required." >&2
  exit 2
fi

active="$(systemctl --user show "$unit.service" -p ActiveState --value 2>/dev/null || printf unknown)"
sub="$(systemctl --user show "$unit.service" -p SubState --value 2>/dev/null || printf unknown)"
result="$(systemctl --user show "$unit.service" -p Result --value 2>/dev/null || printf unavailable)"
workers=0
if [[ -d "$output_dir/worker_results" ]]; then
  workers="$(find "$output_dir/worker_results" -maxdepth 1 -type f -name 'dlm_sh_chain*.rds' | wc -l)"
fi
elapsed=0
rss=0
processes=0
threads=0
if [[ -f "$output_dir/process_group_monitor.csv" ]]; then
  read -r elapsed processes rss threads < <(
    awk -F, 'NR > 1 {e=$2;p=$3;r=$5;t=$6} END{print e+0,p+0,r+0,t+0}' \
      "$output_dir/process_group_monitor.csv"
  )
fi
runner_status="unavailable"
wrapper_pass="unavailable"
if [[ -f "$output_dir/wrapper_closeout.csv" ]]; then
  read -r runner_status wrapper_pass < <(
    awk -F, 'NR == 2 {gsub(/"/, "", $0); print $3, $4}' \
      "$output_dir/wrapper_closeout.csv"
  )
fi
prefix_checks=0
prefix_passes=0
if [[ -f "$output_dir/prefix_parity.csv" ]]; then
  read -r prefix_checks prefix_passes < <(
    awk -F, '
      NR == 1 {
        for (i = 1; i <= NF; i++) {
          gsub(/"/, "", $i)
          if ($i == "pass") pass_col = i
        }
        next
      }
      {
        value = $pass_col
        gsub(/"/, "", value)
        n += 1
        if (value == "TRUE") passed += 1
      }
      END {print n + 0, passed + 0}
    ' "$output_dir/prefix_parity.csv"
  )
fi
stage="not_started"
if [[ -f "$output_dir/stage_status.csv" ]]; then
  stage="$(awk -F, 'NR > 1 {gsub(/"/, "", $1); gsub(/"/, "", $3); value=$1 ":" $3} END {print value}' "$output_dir/stage_status.csv")"
fi
failure_records=0
if [[ -f "$output_dir/failure_log.csv" ]]; then
  failure_records="$(awk 'END {print (NR > 0 ? NR - 1 : 0)}' "$output_dir/failure_log.csv")"
fi
disposition="preparing"
if [[ -f "$output_dir/decision.csv" ]]; then
  disposition="$(awk -F, 'NR==2{gsub(/"/,"",$0);print $8}' "$output_dir/decision.csv")"
elif [[ "$wrapper_pass" == FALSE || "$failure_records" -gt 0 ]]; then
  disposition="failed"
elif [[ "$active" == active ]]; then
  disposition="running"
elif [[ "$workers" -gt 0 ]]; then
  disposition="stopped_incomplete"
elif [[ "$active" == inactive ]]; then
  disposition="stopped_before_worker_publication"
fi
printf '%-30s %s\n' \
  service "$active/$sub ($result)" \
  valid_saved_chains "$workers/5" \
  remaining_chains "$((5-workers))" \
  workflow_status "$disposition" \
  latest_stage "$stage" \
  prefix_checks "$prefix_passes/$prefix_checks" \
  failure_records "$failure_records" \
  runner_status "$runner_status" \
  wrapper_pass "$wrapper_pass" \
  elapsed_seconds "$elapsed" \
  current_sampled_rss_kib "$rss" \
  current_processes "$processes" \
  current_threads "$threads" \
  output_bytes "$(du -sb "$output_dir" 2>/dev/null | awk '{print $1}' || printf 0)"
