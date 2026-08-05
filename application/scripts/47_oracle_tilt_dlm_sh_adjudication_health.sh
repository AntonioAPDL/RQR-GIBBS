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
workers="$(find "$output_dir/worker_results" -maxdepth 1 -type f -name 'dlm_sh_chain*.rds' 2>/dev/null | wc -l)"
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
disposition="running"
if [[ -f "$output_dir/decision.csv" ]]; then
  disposition="$(awk -F, 'NR==2{gsub(/"/,"",$0);print $8}' "$output_dir/decision.csv")"
fi
printf '%-30s %s\n' \
  service "$active/$sub ($result)" \
  completed_chains "$workers/5" \
  remaining_chains "$((5-workers))" \
  disposition "$disposition" \
  elapsed_seconds "$elapsed" \
  current_sampled_rss_kib "$rss" \
  current_processes "$processes" \
  current_threads "$threads" \
  output_bytes "$(du -sb "$output_dir" 2>/dev/null | awk '{print $1}' || printf 0)"
