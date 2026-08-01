#!/usr/bin/env bash
set -euo pipefail

mode="${1:-preflight}"
case "$mode" in
  preflight|reference-only|benchmark|execute) ;;
  *)
    echo "Mode must be preflight, reference-only, benchmark, or execute." >&2
    exit 2
    ;;
esac

repo_root="$(pwd -P)"
if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this wrapper from the RQR-GIBBS repository root." >&2
  exit 2
fi
for command_name in setsid ps awk sha256sum stat find sort date df Rscript; do
  command -v "$command_name" >/dev/null 2>&1 || {
    echo "$command_name is required by the monitored runner." >&2
    exit 2
  }
done

if [[ "$mode" == benchmark || "$mode" == execute ]]; then
  if [[ ! "${RQR_EXPECTED_PRIMARY_COMMIT:-}" =~ ^[0-9a-fA-F]{40}$ ]]; then
    echo "Promotion modes require RQR_EXPECTED_PRIMARY_COMMIT as a full SHA." >&2
    exit 2
  fi
  if [[ -z "${RQR_PRIMARY_RUNTIME_ATTESTATION:-}" ]]; then
    echo "Promotion modes require RQR_PRIMARY_RUNTIME_ATTESTATION." >&2
    exit 2
  fi
fi

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export TZ=UTC
export RQR_RESOURCE_MONITOR_ACTIVE=TRUE
export RQR_PROCESS_MONITOR_KIND=pgid_sampled_fallback
export RQR_MONITOR_KERNEL_HARD_MEMORY=FALSE

case "$mode" in
  preflight|reference-only) timeout_seconds=1800 ;;
  benchmark) timeout_seconds=7200 ;;
  execute) timeout_seconds=28800 ;;
esac
max_rss_kib=12582912
max_threads=8
max_processes=7
max_r_processes=3
monitor_interval=0.2
minimum_free_bytes=21474836480

available_kib="$(df -Pk "$repo_root" | awk 'NR == 2 {print $4}')"
if [[ ! "$available_kib" =~ ^[0-9]+$ ]]; then
  echo "Could not determine free space for the monitored output root." >&2
  exit 2
fi
available_bytes=$((available_kib * 1024))
if (( available_bytes < minimum_free_bytes )); then
  echo "The monitored run requires at least $minimum_free_bytes free bytes." >&2
  exit 2
fi

timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
short_sha="${RQR_EXPECTED_PRIMARY_COMMIT:-exploratory}"
short_sha="${short_sha:0:12}"
requested_output="${RQR_ORACLE_TILT_V3_OUTPUT_DIR:-$repo_root/application/outputs/oracle_tilt_c095_publication_v3/${mode}_${timestamp}_${short_sha}}"
if [[ -L "$requested_output" ]]; then
  echo "The output directory must not be a symbolic link." >&2
  exit 2
fi
mkdir -p "$requested_output"
output_dir="$(cd "$requested_output" && pwd -P)"
first_entry="$(find "$output_dir" -mindepth 1 -maxdepth 1 -print -quit)"
if [[ -n "$first_entry" && ! ( "$mode" == execute && "${RQR_ORACLE_TILT_V3_RESUME:-}" == YES ) ]]; then
  echo "The output directory must be fresh (or explicitly resumed for execute)." >&2
  exit 2
fi

monitor_csv="$output_dir/process_group_monitor.csv"
resource_csv="$output_dir/resource_summary.csv"
wrapper_closeout="$output_dir/wrapper_closeout.csv"
wrapper_manifest="$output_dir/wrapper_artifact_manifest.csv"
stdout_log="$output_dir/runner.stdout.log"
stderr_log="$output_dir/runner.stderr.log"
failure_log="$output_dir/wrapper_failure_log.csv"

root_pid=""
pgid=""
runner_status=125
timed_out=FALSE
limit_triggered=FALSE
signal_received=NONE
final_pgid_empty=FALSE
max_sampled_rss=0
max_sampled_threads=0
max_sampled_processes=0
max_sampled_r_processes=0
start_epoch="$(date +%s)"

pgid_snapshot() {
  local group_id="$1"
  ps -eo pgid=,rss=,nlwp=,stat=,comm= | awk -v group="$group_id" '
    $1 == group && $4 !~ /^Z/ {
      processes += 1; rss += $2; threads += $3
      if ($5 == "R") r_processes += 1
    }
    END {
      printf "%d,%d,%d,%d\n", processes + 0, r_processes + 0,
        rss + 0, threads + 0
    }
  '
}

terminate_group() {
  local group_id="$1"
  kill -TERM -- "-$group_id" 2>/dev/null || true
  local round
  for round in $(seq 1 25); do
    [[ "$(pgid_snapshot "$group_id" | cut -d, -f1)" == 0 ]] && return 0
    sleep "$monitor_interval"
  done
  kill -KILL -- "-$group_id" 2>/dev/null || true
}

on_signal() {
  signal_received="$1"
  if [[ -n "$pgid" ]]; then terminate_group "$pgid"; fi
}
trap 'on_signal INT' INT
trap 'on_signal TERM' TERM
trap 'on_signal HUP' HUP
trap 'if [[ -n "$pgid" ]]; then terminate_group "$pgid"; fi' EXIT

printf '%s\n' \
  "sample_utc,elapsed_seconds,processes,r_processes,rss_kib,threads" \
  >"$monitor_csv"
runner=(
  Rscript application/scripts/42_run_oracle_tilt_publication_v3.R
  "--mode=$mode"
  "--config=application/config/oracle_tilt_c095_publication_v3_20260801.json"
  "--output-dir=$output_dir"
)
setsid "${runner[@]}" >"$stdout_log" 2>"$stderr_log" &
root_pid=$!
pgid=$root_pid

while true; do
  snapshot="$(pgid_snapshot "$pgid")"
  IFS=, read -r processes r_processes rss_kib threads <<<"$snapshot"
  elapsed=$(( $(date +%s) - start_epoch ))
  printf '%s,%s,%s,%s,%s,%s\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$elapsed" \
    "$processes" "$r_processes" "$rss_kib" "$threads" >>"$monitor_csv"
  (( rss_kib > max_sampled_rss )) && max_sampled_rss=$rss_kib
  (( threads > max_sampled_threads )) && max_sampled_threads=$threads
  (( processes > max_sampled_processes )) && max_sampled_processes=$processes
  (( r_processes > max_sampled_r_processes )) && \
    max_sampled_r_processes=$r_processes
  if (( elapsed > timeout_seconds )); then
    timed_out=TRUE
    terminate_group "$pgid"
    break
  fi
  if (( rss_kib > max_rss_kib || threads > max_threads ||
        processes > max_processes || r_processes > max_r_processes )); then
    limit_triggered=TRUE
    terminate_group "$pgid"
    break
  fi
  if (( processes == 0 )); then break; fi
  sleep "$monitor_interval"
done

set +e
wait "$root_pid"
runner_status=$?
set -e
terminate_group "$pgid"
if [[ "$(pgid_snapshot "$pgid" | cut -d, -f1)" == 0 ]]; then
  final_pgid_empty=TRUE
fi
trap - EXIT

elapsed_total=$(( $(date +%s) - start_epoch ))
wrapper_pass=TRUE
if (( runner_status != 0 )) || [[ "$timed_out" != FALSE ]] ||
   [[ "$limit_triggered" != FALSE ]] || [[ "$final_pgid_empty" != TRUE ]] ||
   [[ "$signal_received" != NONE ]]; then
  wrapper_pass=FALSE
fi
printf '%s\n' \
  "mode,runner_status,elapsed_seconds,available_bytes_at_start,minimum_free_bytes,max_sampled_rss_kib,max_sampled_threads,max_sampled_processes,max_sampled_r_processes,timeout_seconds,rss_limit_kib,thread_limit,process_limit,r_process_limit,timezone,timed_out,sampled_limit_triggered,signal_received,final_pgid_empty,telemetry_kind,pass" \
  >"$resource_csv"
printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
  "$mode" "$runner_status" "$elapsed_total" "$available_bytes" \
  "$minimum_free_bytes" "$max_sampled_rss" \
  "$max_sampled_threads" "$max_sampled_processes" \
  "$max_sampled_r_processes" "$timeout_seconds" "$max_rss_kib" \
  "$max_threads" "$max_processes" "$max_r_processes" "$TZ" "$timed_out" \
  "$limit_triggered" "$signal_received" "$final_pgid_empty" \
  "sampled_pgid_not_kernel_hard" "$wrapper_pass" >>"$resource_csv"

printf '%s\n' "mode,finished_at_utc,runner_status,wrapper_pass" >"$wrapper_closeout"
printf '%s,%s,%s,%s\n' "$mode" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  "$runner_status" "$wrapper_pass" >>"$wrapper_closeout"
if [[ "$wrapper_pass" != TRUE ]]; then
  printf '%s\n' "mode,runner_status,timed_out,limit_triggered,signal_received" \
    >"$failure_log"
  printf '%s,%s,%s,%s,%s\n' "$mode" "$runner_status" "$timed_out" \
    "$limit_triggered" "$signal_received" >>"$failure_log"
fi

temporary_manifest="${wrapper_manifest}.tmp.$$"
printf '%s\n' "sha256,bytes,path" >"$temporary_manifest"
while IFS= read -r -d '' path; do
  relative="${path#"$output_dir"/}"
  digest_value="$(sha256sum "$path" | awk '{print $1}')"
  bytes="$(stat -c '%s' "$path")"
  escaped="${relative//\"/\"\"}"
  printf '%s,%s,"%s"\n' "$digest_value" "$bytes" "$escaped" \
    >>"$temporary_manifest"
done < <(find "$output_dir" -type f ! -name 'wrapper_artifact_manifest.csv*' -print0 | sort -z)
mv -T "$temporary_manifest" "$wrapper_manifest"

if [[ "$wrapper_pass" != TRUE ]]; then
  echo "The monitored $mode run failed; inspect $output_dir." >&2
  exit 1
fi
echo "[oracle-tilt-v3-monitor] $mode passed: $output_dir"
