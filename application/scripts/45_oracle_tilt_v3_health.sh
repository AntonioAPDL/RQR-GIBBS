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

active_state="$(systemctl --user show "$unit.service" -p ActiveState --value 2>/dev/null || printf unknown)"
sub_state="$(systemctl --user show "$unit.service" -p SubState --value 2>/dev/null || printf unknown)"
result="$(systemctl --user show "$unit.service" -p Result --value 2>/dev/null || printf unavailable)"

completed_cells=0
completed_chains=0
failed_cells=0
if [[ -f "$output_dir/run_status.csv" ]]; then
  read -r completed_cells completed_chains failed_cells < <(
    awk -F, 'NR > 1 {
      gsub(/"/, "", $0)
      if ($3 == "completed") { cells += 1; chains += $4 }
      if ($8 == "fail") failed += 1
    } END { print cells + 0, chains + 0, failed + 0 }' \
      "$output_dir/run_status.csv"
  )
fi

current="unavailable"
if [[ -f "$output_dir/current_stage.csv" ]]; then
  current="$(awk -F, 'NR == 2 {
    gsub(/"/, "", $0)
    printf "%s/%s/%s (%s)", $2, $3, $4, $6
  }' "$output_dir/current_stage.csv")"
fi

elapsed=0
rss=0
threads=0
processes=0
if [[ -f "$output_dir/process_group_monitor.csv" ]]; then
  read -r elapsed processes rss threads < <(
    awk -F, 'NR > 1 { elapsed=$2; processes=$3; rss=$5; threads=$6 }
      END { print elapsed + 0, processes + 0, rss + 0, threads + 0 }' \
      "$output_dir/process_group_monitor.csv"
  )
fi

output_bytes="$(du -sb "$output_dir" 2>/dev/null | awk '{print $1}' || printf 0)"
free_bytes="$(df -Pk "${output_dir%/*}" | awk 'NR == 2 {print $4 * 1024}')"
printf '%-28s %s\n' \
  "service" "$active_state/$sub_state ($result)" \
  "current_stage" "$current" \
  "completed_cells" "$completed_cells/6" \
  "completed_chains" "$completed_chains/27" \
  "remaining_chains" "$((27 - completed_chains))" \
  "failed_cells" "$failed_cells" \
  "elapsed_seconds" "$elapsed" \
  "current_sampled_rss_kib" "$rss" \
  "current_processes" "$processes" \
  "current_threads" "$threads" \
  "output_bytes" "$output_bytes" \
  "filesystem_free_bytes" "$free_bytes"
