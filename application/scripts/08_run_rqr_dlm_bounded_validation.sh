#!/usr/bin/env bash
set -euo pipefail

mode="${1:-preflight}"
case "$mode" in
  preflight|reference-only|benchmark-one-cell|execute-bounded) ;;
  *)
    echo "Mode must be preflight, reference-only, benchmark-one-cell, or execute-bounded." >&2
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
for command_name in setsid ps awk sha256sum stat Rscript; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "$command_name is required by the monitored runner." >&2
    exit 2
  fi
done

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export RQR_RESOURCE_MONITOR_ACTIVE=TRUE
export RQR_PROCESS_MONITOR_KIND=pgid_sampled_fallback
export RQR_MONITOR_KERNEL_HARD_MEMORY=FALSE

timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
short_sha="${RQR_EXPECTED_PRIMARY_COMMIT:0:12}"
output_dir="${RQR_DLM_OUTPUT_DIR:-$repo_root/application/outputs/rqr_dlm_bounded_${mode//-/_}_${timestamp}_${short_sha}}"
mkdir -p "$output_dir"
output_dir="$(cd "$output_dir" && pwd -P)"
export RQR_DLM_OUTPUT_DIR="$output_dir"

# These values mirror the frozen config. RSS is sampled telemetry with
# best-effort termination; it is not a cgroup/kernel-hard memory ceiling.
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

monitor_csv="$output_dir/process_group_monitor.csv"
resource_csv="$output_dir/resource_summary.csv"
stdout_log="$output_dir/runner.stdout.log"
stderr_log="$output_dir/runner.stderr.log"
fault_csv="$output_dir/monitor_fault_test.csv"
closeout_csv="$output_dir/wrapper_closeout.csv"
artifact_csv="$output_dir/artifact_hashes.csv"

group_exists() {
  local group_id="$1"
  kill -0 -- "-$group_id" 2>/dev/null
}

terminate_group() {
  local group_id="$1"
  local signal_name="$2"
  kill "-$signal_name" -- "-$group_id" 2>/dev/null || true
}

drain_group() {
  local group_id="$1"
  terminate_group "$group_id" TERM
  for _ in {1..25}; do
    if ! group_exists "$group_id"; then
      return 0
    fi
    sleep "$monitor_interval"
  done
  terminate_group "$group_id" KILL
  for _ in {1..25}; do
    if ! group_exists "$group_id"; then
      return 0
    fi
    sleep "$monitor_interval"
  done
  return 1
}

# Fault-injection check: a group leader exits nonzero while an ignored-HUP
# descendant remains. The PGID sweep must find and terminate the descendant.
setsid bash -c 'trap "" HUP; sleep 60 & exit 17' &
fault_root=$!
fault_pgid=$fault_root
sleep 0.2
fault_descendant_seen=FALSE
if group_exists "$fault_pgid"; then
  fault_descendant_seen=TRUE
fi
set +e
wait "$fault_root"
fault_status=$?
set -e
fault_drained=TRUE
if ! drain_group "$fault_pgid"; then
  fault_drained=FALSE
fi
fault_final_empty=TRUE
if group_exists "$fault_pgid"; then
  fault_final_empty=FALSE
fi
fault_pass=FALSE
if [[ "$fault_status" -eq 17 &&
      "$fault_descendant_seen" == TRUE &&
      "$fault_drained" == TRUE &&
      "$fault_final_empty" == TRUE ]]; then
  fault_pass=TRUE
fi
fault_tmp="${fault_csv}.tmp.$$"
{
  echo "metric,value,expected,pass"
  echo "leader_exit_status,$fault_status,17,$([[ $fault_status -eq 17 ]] && echo TRUE || echo FALSE)"
  echo "descendant_seen_after_leader_exit,$fault_descendant_seen,TRUE,$([[ $fault_descendant_seen == TRUE ]] && echo TRUE || echo FALSE)"
  echo "pgid_drain_completed,$fault_drained,TRUE,$([[ $fault_drained == TRUE ]] && echo TRUE || echo FALSE)"
  echo "final_pgid_empty,$fault_final_empty,TRUE,$([[ $fault_final_empty == TRUE ]] && echo TRUE || echo FALSE)"
} >"$fault_tmp"
mv "$fault_tmp" "$fault_csv"
if [[ "$fault_pass" != TRUE ]]; then
  echo "The process-group fault test failed; inspect $fault_csv." >&2
  exit 1
fi

echo "elapsed_seconds,processes,threads,rss_kib" >"$monitor_csv"
start_epoch="$(date +%s)"
root_pid=""
pgid=""
runner_waited=FALSE
runner_status=125
timed_out=FALSE
sampled_limit_triggered=FALSE
monitor_error=FALSE
signal_received=NONE

cleanup_group() {
  local saved_status=$?
  trap - EXIT INT TERM HUP
  if [[ -n "$pgid" ]] && group_exists "$pgid"; then
    drain_group "$pgid" || true
  fi
  exit "$saved_status"
}
handle_signal() {
  local signal_name="$1"
  local exit_status="$2"
  signal_received="$signal_name"
  if [[ -n "$pgid" ]] && group_exists "$pgid"; then
    drain_group "$pgid" || true
  fi
  exit "$exit_status"
}
trap cleanup_group EXIT
trap 'handle_signal INT 130' INT
trap 'handle_signal TERM 143' TERM
trap 'handle_signal HUP 129' HUP

setsid Rscript \
  "$repo_root/application/scripts/08_run_rqr_dlm_bounded_validation.R" \
  "$mode" >"$stdout_log" 2>"$stderr_log" &
root_pid=$!
pgid=$root_pid

# The background child can exist briefly before `setsid` has created its new
# process group. Entering the monitor loop before that transition would record
# no samples and then merely wait for the unmonitored child.
group_ready=FALSE
for _ in {1..50}; do
  if group_exists "$pgid"; then
    group_ready=TRUE
    break
  fi
  if ! kill -0 "$root_pid" 2>/dev/null; then
    break
  fi
  sleep 0.1
done
if [[ "$group_ready" != TRUE ]]; then
  monitor_error=TRUE
fi

while group_exists "$pgid"; do
  now_epoch="$(date +%s)"
  elapsed=$((now_epoch - start_epoch))
  if (( elapsed > timeout_seconds )); then
    timed_out=TRUE
    drain_group "$pgid" || true
    break
  fi
  if ! metrics="$(
    ps -eo pid=,pgid=,rss=,nlwp= | awk -v group="$pgid" '
      $2 == group {
        processes += 1
        threads += $4
        rss += $3
      }
      END {
        printf "%d,%d,%d", processes + 0, threads + 0, rss + 0
      }
    '
  )"; then
    monitor_error=TRUE
    drain_group "$pgid" || true
    break
  fi
  echo "$elapsed,$metrics" >>"$monitor_csv"
  IFS=, read -r current_processes current_threads current_rss_kib <<<"$metrics"
  if (( current_processes > max_processes ||
        current_threads > max_threads ||
        current_rss_kib > max_rss_kib )); then
    sampled_limit_triggered=TRUE
    drain_group "$pgid" || true
    break
  fi
  if ! kill -0 "$root_pid" 2>/dev/null; then
    set +e
    wait "$root_pid"
    runner_status=$?
    set -e
    runner_waited=TRUE
    if group_exists "$pgid"; then
      drain_group "$pgid" || true
    fi
    break
  fi
  sleep "$monitor_interval"
done

if [[ "$runner_waited" != TRUE ]]; then
  set +e
  wait "$root_pid"
  runner_status=$?
  set -e
fi
final_pgid_empty=TRUE
if group_exists "$pgid"; then
  drain_group "$pgid" || true
fi
if group_exists "$pgid"; then
  final_pgid_empty=FALSE
fi

maxima="$(
  awk -F, '
    NR > 1 {
      if ($2 > p) p=$2
      if ($3 > t) t=$3
      if ($4 > r) r=$4
    }
    END { printf "%d,%d,%d", p + 0, t + 0, r + 0 }
  ' "$monitor_csv"
)"
IFS=, read -r peak_processes peak_threads peak_rss_kib <<<"$maxima"

resource_pass=TRUE
if (( peak_processes > max_processes ||
      peak_threads > max_threads ||
      peak_rss_kib > max_rss_kib )); then
  resource_pass=FALSE
fi
if [[ "$timed_out" == TRUE ||
      "$sampled_limit_triggered" == TRUE ||
      "$monitor_error" == TRUE ||
      "$final_pgid_empty" != TRUE ||
      "$fault_pass" != TRUE ]]; then
  resource_pass=FALSE
fi

resource_tmp="${resource_csv}.tmp.$$"
{
  echo "metric,value,limit,pass"
  echo "sampled_process_group_peak_processes,$peak_processes,$max_processes,$([[ $peak_processes -le $max_processes ]] && echo TRUE || echo FALSE)"
  echo "sampled_process_group_peak_threads,$peak_threads,$max_threads,$([[ $peak_threads -le $max_threads ]] && echo TRUE || echo FALSE)"
  echo "sampled_process_group_peak_rss_kib,$peak_rss_kib,$max_rss_kib,$([[ $peak_rss_kib -le $max_rss_kib ]] && echo TRUE || echo FALSE)"
  echo "hard_timeout_triggered,$timed_out,FALSE,$([[ $timed_out == FALSE ]] && echo TRUE || echo FALSE)"
  echo "sampled_limit_triggered,$sampled_limit_triggered,FALSE,$([[ $sampled_limit_triggered == FALSE ]] && echo TRUE || echo FALSE)"
  echo "monitor_error,$monitor_error,FALSE,$([[ $monitor_error == FALSE ]] && echo TRUE || echo FALSE)"
  echo "final_pgid_empty,$final_pgid_empty,TRUE,$([[ $final_pgid_empty == TRUE ]] && echo TRUE || echo FALSE)"
  echo "runner_exit_status,$runner_status,0,$([[ $runner_status -eq 0 ]] && echo TRUE || echo FALSE)"
  echo "monitor_fault_test_pass,$fault_pass,TRUE,$([[ $fault_pass == TRUE ]] && echo TRUE || echo FALSE)"
  echo "kernel_hard_memory_ceiling,FALSE,FALSE,TRUE"
} >"$resource_tmp"
mv "$resource_tmp" "$resource_csv"

closeout_tmp="${closeout_csv}.tmp.$$"
{
  echo "field,value"
  echo "mode,$mode"
  echo "expected_primary_commit,${RQR_EXPECTED_PRIMARY_COMMIT,,}"
  echo "runner_exit_status,$runner_status"
  echo "resource_pass,$resource_pass"
  echo "monitor_kind,pgid_sampled_fallback"
  echo "kernel_hard_memory_ceiling,FALSE"
  echo "signal_received,$signal_received"
  echo "completed_at,$(date -u +%Y-%m-%dT%H:%M:%SZ)"
} >"$closeout_tmp"
mv "$closeout_tmp" "$closeout_csv"

artifact_tmp="${artifact_csv}.tmp.$$"
echo "sha256,bytes,path" >"$artifact_tmp"
while IFS= read -r -d '' path; do
  relative="${path#"$output_dir"/}"
  digest_value="$(sha256sum "$path" | awk '{print $1}')"
  bytes="$(stat -c '%s' "$path")"
  escaped_relative="${relative//\"/\"\"}"
  printf '%s,%s,"%s"\n' \
    "$digest_value" "$bytes" "$escaped_relative" >>"$artifact_tmp"
done < <(
  find "$output_dir" -type f \
    ! -name 'artifact_hashes.csv' \
    ! -name 'artifact_hashes.csv.tmp.*' -print0 | sort -z
)
mv "$artifact_tmp" "$artifact_csv"

trap - EXIT INT TERM HUP
if [[ $runner_status -ne 0 || "$resource_pass" != TRUE ]]; then
  echo "Bounded runner failed; inspect $output_dir" >&2
  exit 1
fi
echo "Bounded runner completed: $output_dir"
