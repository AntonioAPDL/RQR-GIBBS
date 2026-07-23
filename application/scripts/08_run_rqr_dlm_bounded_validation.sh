#!/usr/bin/env bash
set -euo pipefail

mode="${1:-preflight}"
case "$mode" in
  preflight|reference-only|execute-bounded) ;;
  *)
    echo "Mode must be preflight, reference-only, or execute-bounded." >&2
    exit 2
    ;;
esac

repo_root="$(pwd -P)"
if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this wrapper from the RQR-GIBBS repository root." >&2
  exit 2
fi
if [[ ! "${RQR_EXPECTED_PRIMARY_COMMIT:-}" =~ ^[0-9a-fA-F]{40}$ ]]; then
  echo "RQR_EXPECTED_PRIMARY_COMMIT must be a complete reviewed SHA." >&2
  exit 2
fi
if ! command -v setsid >/dev/null 2>&1; then
  echo "setsid is required for process-group isolation." >&2
  exit 2
fi

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export RQR_RESOURCE_MONITOR_ACTIVE=TRUE

timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
short_sha="${RQR_EXPECTED_PRIMARY_COMMIT:0:12}"
output_dir="${RQR_DLM_OUTPUT_DIR:-$repo_root/application/outputs/rqr_dlm_bounded_${mode//-/_}_${timestamp}_${short_sha}}"
mkdir -p "$output_dir"
export RQR_DLM_OUTPUT_DIR="$output_dir"

# These values mirror the frozen config and are intentionally not overridable.
timeout_seconds=2700
max_rss_kib=4194304
max_threads=4
max_processes=3
monitor_interval=0.2
export RQR_MONITOR_TIMEOUT_SECONDS="$timeout_seconds"
export RQR_MONITOR_MAX_RSS_KIB="$max_rss_kib"
export RQR_MONITOR_MAX_THREADS="$max_threads"
export RQR_MONITOR_MAX_PROCESSES="$max_processes"
export RQR_MONITOR_INTERVAL_SECONDS="$monitor_interval"
monitor_csv="$output_dir/process_tree_monitor.csv"
resource_csv="$output_dir/resource_summary.csv"
stdout_log="$output_dir/runner.stdout.log"
stderr_log="$output_dir/runner.stderr.log"

echo "elapsed_seconds,processes,threads,rss_kib" >"$monitor_csv"
start_epoch="$(date +%s)"
setsid Rscript \
  "$repo_root/application/scripts/08_run_rqr_dlm_bounded_validation.R" \
  "$mode" >"$stdout_log" 2>"$stderr_log" &
root_pid=$!
timed_out=FALSE
resource_limit_triggered=FALSE

terminate_process_group() {
  local signal="$1"
  kill "-$signal" -- "-$root_pid" 2>/dev/null || true
}

terminate_with_escalation() {
  terminate_process_group TERM
  for _ in {1..25}; do
    if ! kill -0 "$root_pid" 2>/dev/null; then
      return
    fi
    sleep "$monitor_interval"
  done
  terminate_process_group KILL
}

while kill -0 "$root_pid" 2>/dev/null; do
  now_epoch="$(date +%s)"
  elapsed=$((now_epoch - start_epoch))
  if (( elapsed > timeout_seconds )); then
    timed_out=TRUE
    terminate_with_escalation
    break
  fi
  metrics="$(
    ps -eo pid=,ppid=,rss=,nlwp= | awk -v root="$root_pid" '
      {
        pid[NR]=$1; ppid[NR]=$2; rss[NR]=$3; threads[NR]=$4
      }
      END {
        included[root]=1
        changed=1
        while (changed) {
          changed=0
          for (i=1; i<=NR; i++) {
            if (included[ppid[i]] && !included[pid[i]]) {
              included[pid[i]]=1
              changed=1
            }
          }
        }
        process_count=0
        thread_count=0
        rss_total=0
        for (i=1; i<=NR; i++) {
          if (included[pid[i]]) {
            process_count++
            thread_count+=threads[i]
            rss_total+=rss[i]
          }
        }
        printf "%d,%d,%d", process_count, thread_count, rss_total
      }
    '
  )"
  echo "$elapsed,$metrics" >>"$monitor_csv"
  IFS=, read -r current_processes current_threads current_rss_kib <<<"$metrics"
  if (( current_processes > max_processes ||
        current_threads > max_threads ||
        current_rss_kib > max_rss_kib )); then
    resource_limit_triggered=TRUE
    terminate_with_escalation
    break
  fi
  sleep "$monitor_interval"
done

set +e
wait "$root_pid"
runner_status=$?
set -e

maxima="$(
  awk -F, '
    NR > 1 {
      if ($2 > p) p=$2
      if ($3 > t) t=$3
      if ($4 > r) r=$4
    }
    END { printf "%d,%d,%d", p, t, r }
  ' "$monitor_csv"
)"
IFS=, read -r peak_processes peak_threads peak_rss_kib <<<"$maxima"
resource_pass=TRUE
if (( peak_processes > max_processes ||
      peak_threads > max_threads ||
      peak_rss_kib > max_rss_kib )); then
  resource_pass=FALSE
fi
if [[ "$timed_out" == TRUE ]]; then
  resource_pass=FALSE
fi
if [[ "$resource_limit_triggered" == TRUE ]]; then
  resource_pass=FALSE
fi

{
  echo "metric,value,limit,pass"
  echo "process_tree_peak_processes,$peak_processes,$max_processes,$([[ $peak_processes -le $max_processes ]] && echo TRUE || echo FALSE)"
  echo "process_tree_peak_threads,$peak_threads,$max_threads,$([[ $peak_threads -le $max_threads ]] && echo TRUE || echo FALSE)"
  echo "process_tree_peak_rss_kib,$peak_rss_kib,$max_rss_kib,$([[ $peak_rss_kib -le $max_rss_kib ]] && echo TRUE || echo FALSE)"
  echo "hard_timeout_triggered,$timed_out,FALSE,$([[ $timed_out == FALSE ]] && echo TRUE || echo FALSE)"
  echo "resource_limit_triggered,$resource_limit_triggered,FALSE,$([[ $resource_limit_triggered == FALSE ]] && echo TRUE || echo FALSE)"
  echo "runner_exit_status,$runner_status,0,$([[ $runner_status -eq 0 ]] && echo TRUE || echo FALSE)"
} >"$resource_csv"

if [[ $runner_status -ne 0 || "$resource_pass" != TRUE ]]; then
  echo "Bounded runner failed; inspect $output_dir" >&2
  exit 1
fi
echo "Bounded runner completed: $output_dir"
