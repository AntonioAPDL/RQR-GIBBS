#!/usr/bin/env bash
set -euo pipefail

# Fault-test the process-group monitor without executing a simulation fit.
# A zero-thread ceiling must terminate preflight, publish telemetry, leave no
# final output directory, and sweep the complete process group.

repo_root="$(pwd -P)"
wrapper="$repo_root/application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh"
if [[ ! -x "$wrapper" || ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this test from the RQR-GIBBS repository root." >&2
  exit 64
fi

work_parent="$repo_root/.codex_work"
mkdir -p "$work_parent"
test_root="$(mktemp -d "$work_parent/confirm-monitor-XXXXXX")"
output_dir="$test_root/forbidden-output"
cleanup() {
  local status=$?
  trap - EXIT INT TERM HUP
  rm -rf -- "$test_root"
  exit "$status"
}
trap cleanup EXIT INT TERM HUP

monitor_root="$repo_root/application/logs/rqr_dlm_confirmatory_monitor"
before_epoch="$(date +%s)"
set +e
RQR_MAX_PROCESS_WAVE_SECONDS=60 \
RQR_MAX_PROCESS_GROUP_RSS_KIB=52428800 \
RQR_MAX_PROCESS_GROUP_THREADS=0 \
RQR_MONITOR_POLL_SECONDS=0.05 \
  "$wrapper" preflight "$output_dir"
status=$?
set -e
if [[ "$status" -eq 0 ]]; then
  echo "The injected monitor ceiling unexpectedly passed." >&2
  exit 1
fi
if [[ -e "$output_dir" ]]; then
  echo "A failed monitored stage published its final output." >&2
  exit 1
fi

summary="$(
  find "$monitor_root" -maxdepth 1 -type f \
    -name 'preflight-*-summary.csv' -newermt "@$before_epoch" \
    -printf '%T@ %p\n' |
    sort -nr |
    awk 'NR == 1 {sub(/^[^ ]+ /, ""); print; exit}'
)"
if [[ -z "$summary" || ! -f "$summary" ]]; then
  echo "The process monitor did not publish a resource summary." >&2
  exit 1
fi

IFS=, read -r mode runner_status elapsed peak_processes peak_rss \
  peak_threads ceiling_reason telemetry_role numerical_threads \
  sampled_thread_ceiling max_rss max_seconds < <(tail -n 1 "$summary")
if [[ "$mode" != "preflight" ||
      "$runner_status" -eq 0 ||
      "$peak_processes" -lt 1 ||
      "$peak_threads" -lt 1 ||
      "$ceiling_reason" != "process_group_threads" ||
      "$telemetry_role" != "sampled_process_group_telemetry" ||
      "$numerical_threads" -ne 1 ||
      "$sampled_thread_ceiling" -ne 0 ]]; then
  echo "The process-monitor fault summary failed its contract." >&2
  exit 1
fi

echo "Confirmatory process-monitor fault test passed."
