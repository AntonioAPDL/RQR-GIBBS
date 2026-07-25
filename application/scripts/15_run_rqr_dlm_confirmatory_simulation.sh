#!/usr/bin/env bash
set -euo pipefail

# Process-group wrapper for the fail-closed confirmatory runner.  Sampled RSS
# and thread counts are telemetry; timeout and configured RSS/thread ceilings
# are enforced at the complete process-group level.

if [[ $# -lt 2 || $# -gt 2 ]]; then
  echo "Usage: $0 <mode> <fresh-output-directory>" >&2
  exit 64
fi

mode="$1"
output_dir="$2"
case "$mode" in
  preflight|oracle-reference|sentinel-core|execute-confirmatory|collect|audit)
    ;;
  *)
    echo "Unknown mode: $mode" >&2
    exit 64
    ;;
esac

repo_root="$(pwd -P)"
runner="$repo_root/application/scripts/15_run_rqr_dlm_confirmatory_simulation.R"
if [[ ! -f "$runner" || ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this wrapper from the RQR-GIBBS repository root." >&2
  exit 64
fi
if [[ -e "$output_dir" ]]; then
  echo "Output directory must be fresh: $output_dir" >&2
  exit 64
fi

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export RCPP_PARALLEL_NUM_THREADS=1

monitor_root="${RQR_CONFIRMATORY_MONITOR_ROOT:-$repo_root/application/logs/rqr_dlm_confirmatory_monitor}"
mkdir -p "$monitor_root"
stamp="$(date -u +%Y%m%dT%H%M%SZ)"
monitor_file="$monitor_root/${mode}-${stamp}-$$.csv"
summary_file="$monitor_root/${mode}-${stamp}-$$-summary.csv"
temporary_summary="${summary_file}.part"
stdout_file="$monitor_root/${mode}-${stamp}-$$.stdout.log"
stderr_file="$monitor_root/${mode}-${stamp}-$$.stderr.log"

max_seconds="${RQR_MAX_PROCESS_WAVE_SECONDS:-1209600}"
max_rss_kib="${RQR_MAX_PROCESS_GROUP_RSS_KIB:-1572864}"
max_threads="${RQR_MAX_PROCESS_GROUP_THREADS:-1}"
poll_seconds="${RQR_MONITOR_POLL_SECONDS:-0.2}"

if ! [[ "$max_seconds" =~ ^[0-9]+$ ]] ||
   ! [[ "$max_rss_kib" =~ ^[0-9]+$ ]] ||
   ! [[ "$max_threads" =~ ^[0-9]+$ ]]; then
  echo "Resource ceilings must be nonnegative integers." >&2
  exit 64
fi

echo "timestamp_utc,processes,rss_kib,threads,ceiling_exceeded" >"$monitor_file"

child_pid=""
child_pgid=""
cleanup_required=1
cleanup_group() {
  local status=$?
  trap - EXIT INT TERM HUP
  if [[ -n "$child_pgid" ]]; then
    kill -TERM -- "-$child_pgid" 2>/dev/null || true
    for _ in $(seq 1 25); do
      if ! ps -eo pgid= | awk -v g="$child_pgid" '$1 == g {found=1} END {exit !found}'; then
        break
      fi
      sleep 0.2
    done
    kill -KILL -- "-$child_pgid" 2>/dev/null || true
  fi
  if [[ "$cleanup_required" -eq 1 && -n "$child_pgid" ]]; then
    while ps -eo pgid= | awk -v g="$child_pgid" '$1 == g {found=1} END {exit !found}'; do
      kill -KILL -- "-$child_pgid" 2>/dev/null || true
      sleep 0.1
    done
  fi
  rm -f "$temporary_summary"
  exit "$status"
}
trap cleanup_group EXIT INT TERM HUP

setsid Rscript "$runner" "$mode" "$output_dir" \
  >"$stdout_file" 2>"$stderr_file" &
child_pid=$!
child_pgid=""
for _ in $(seq 1 100); do
  observed_pgid="$(
    ps -o pgid= -p "$child_pid" 2>/dev/null |
      awk '{print $1; exit}'
  )"
  if [[ -n "$observed_pgid" ]]; then
    child_pgid="$observed_pgid"
    if [[ "$child_pgid" == "$child_pid" ]]; then
      break
    fi
  elif ! kill -0 "$child_pid" 2>/dev/null; then
    break
  fi
  sleep 0.05
done
if [[ -z "$child_pgid" ]]; then
  set +e
  wait "$child_pid"
  runner_status=$?
  set -e
  echo "Could not acquire the runner process group." >&2
  exit "$runner_status"
fi
started_epoch="$(date +%s)"
peak_rss=0
peak_threads=0
peak_processes=0
ceiling_reason=""

while true; do
  snapshot="$(
    ps -eo pgid=,rss=,nlwp= 2>/dev/null |
      awk -v g="$child_pgid" '
        $1 == g {
          processes += 1
          rss += $2
          threads += $3
        }
        END {
          printf "%d %d %d\n", processes + 0, rss + 0, threads + 0
        }
      '
  )"
  read -r processes rss threads <<<"$snapshot"
  if (( processes == 0 )); then
    if kill -0 "$child_pid" 2>/dev/null; then
      sleep "$poll_seconds"
      continue
    fi
    break
  fi
  (( rss > peak_rss )) && peak_rss=$rss
  (( threads > peak_threads )) && peak_threads=$threads
  (( processes > peak_processes )) && peak_processes=$processes
  now_epoch="$(date +%s)"
  elapsed=$((now_epoch - started_epoch))
  exceeded=0
  if (( elapsed > max_seconds )); then
    exceeded=1
    ceiling_reason="wall_time"
  elif (( rss > max_rss_kib )); then
    exceeded=1
    ceiling_reason="process_group_RSS"
  elif (( threads > max_threads )); then
    exceeded=1
    ceiling_reason="process_group_threads"
  fi
  printf '%s,%d,%d,%d,%d\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
    "$processes" "$rss" "$threads" "$exceeded" >>"$monitor_file"
  if (( exceeded == 1 )); then
    kill -TERM -- "-$child_pgid" 2>/dev/null || true
    sleep 1
    kill -KILL -- "-$child_pgid" 2>/dev/null || true
    break
  fi
  sleep "$poll_seconds"
done

set +e
wait "$child_pid"
runner_status=$?
set -e

# The group leader may exit before a descendant.  Sweep the complete PGID
# before declaring the wrapper complete.
remaining="$(
  ps -eo pgid= | awk -v g="$child_pgid" '$1 == g {count += 1} END {print count + 0}'
)"
if (( remaining > 0 )); then
  kill -TERM -- "-$child_pgid" 2>/dev/null || true
  sleep 1
  kill -KILL -- "-$child_pgid" 2>/dev/null || true
  ceiling_reason="${ceiling_reason:-orphaned_process_group}"
  runner_status=1
fi

ended_epoch="$(date +%s)"
elapsed=$((ended_epoch - started_epoch))
{
  echo "mode,runner_status,elapsed_seconds,peak_processes,peak_rss_kib,peak_threads,ceiling_reason,telemetry_role"
  printf '%s,%d,%d,%d,%d,%d,%s,%s\n' \
    "$mode" "$runner_status" "$elapsed" "$peak_processes" \
    "$peak_rss" "$peak_threads" "${ceiling_reason:-none}" \
    "sampled_process_group_telemetry"
} >"$temporary_summary"
mv "$temporary_summary" "$summary_file"

cleanup_required=0
trap - EXIT INT TERM HUP
if (( runner_status != 0 )); then
  echo "Runner failed; see $stderr_file and $summary_file" >&2
  exit "$runner_status"
fi
echo "Runner and process-group monitor passed."
echo "  output: $output_dir"
echo "  resource summary: $summary_file"
