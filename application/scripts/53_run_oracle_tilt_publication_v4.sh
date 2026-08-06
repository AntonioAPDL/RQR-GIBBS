#!/usr/bin/env bash
set -euo pipefail

mode="${1:-preflight}"
case "$mode" in
  preflight|reference-only|benchmark|resource-rehearsal|execute) ;;
  *) echo "Unsupported V4 mode: $mode" >&2; exit 2 ;;
esac

repo_root="$(pwd -P)"
config="${RQR_ORACLE_TILT_V4_CONFIG:-application/config/oracle_tilt_c095_publication_v4_seed_screen_20260805.json}"
if [[ ! -f "$repo_root/application/DESCRIPTION" || ! -f "$config" ]]; then
  echo "Run the V4 wrapper from the RQR-GIBBS repository root." >&2
  exit 2
fi
for command_name in setsid ps awk sha256sum stat find sort date df getconf Rscript; do
  command -v "$command_name" >/dev/null 2>&1 || {
    echo "$command_name is required by the V4 monitored runner." >&2
    exit 2
  }
done

if [[ -n "${RQR_PRIMARY_RUNTIME_ATTESTATION:-}" ]]; then
  default_r_library_path="$(Rscript -e 'cat(paste(.libPaths(), collapse=.Platform$path.sep))')"
  attested_runtime_library="$(Rscript -e '
    args <- commandArgs(trailingOnly = TRUE)
    attestation <- readRDS(args[[1L]])
    path <- attestation$runtime_package_path
    if (!is.character(path) || length(path) != 1L || !dir.exists(path)) {
      stop("Invalid attested runtime package path.", call. = FALSE)
    }
    cat(dirname(normalizePath(path, winslash = "/", mustWork = TRUE)))
  ' "$RQR_PRIMARY_RUNTIME_ATTESTATION")"
  export R_LIBS_USER="${attested_runtime_library}:${default_r_library_path}"
fi

if [[ "$mode" =~ ^(benchmark|resource-rehearsal|execute)$ ]]; then
  if [[ ! "${RQR_EXPECTED_PRIMARY_COMMIT:-}" =~ ^[0-9a-fA-F]{40}$ ||
        -z "${RQR_PRIMARY_RUNTIME_ATTESTATION:-}" ]]; then
    echo "$mode requires an exact full SHA and isolated runtime attestation." >&2
    exit 2
  fi
fi
if [[ "$mode" == benchmark &&
      "${RQR_ORACLE_TILT_V4_BENCHMARK_CONFIRM:-}" != YES ]]; then
  echo "V4 benchmark is fail-closed." >&2; exit 2
fi
if [[ "$mode" == resource-rehearsal &&
      "${RQR_ORACLE_TILT_V4_REHEARSAL_CONFIRM:-}" != YES ]]; then
  echo "V4 resource rehearsal is fail-closed." >&2; exit 2
fi
if [[ "$mode" == execute &&
      "${RQR_ORACLE_TILT_V4_CONFIRM:-}" != YES ]]; then
  echo "V4 production execution is fail-closed." >&2; exit 2
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
  benchmark) timeout_seconds=10800 ;;
  resource-rehearsal) timeout_seconds=3600 ;;
  execute) timeout_seconds=43200 ;;
esac
minimum_free_bytes=53687091200
minimum_available_memory_bytes=107374182400
minimum_idle_cpus=24
max_rss_kib=100663296
max_threads=64
max_processes=64
max_r_processes=19
monitor_interval=0.2

available_kib="$(df -Pk "$repo_root" | awk 'NR == 2 {print $4}')"
available_bytes=$((available_kib * 1024))
memory_available_kib="$(awk '/^MemAvailable:/ {print $2}' /proc/meminfo)"
memory_available_bytes=$((memory_available_kib * 1024))
logical_cpus="$(getconf _NPROCESSORS_ONLN)"
load_one="$(awk '{print $1}' /proc/loadavg)"
estimated_idle="$(awk -v cpus="$logical_cpus" -v current_load="$load_one" \
  'BEGIN { value = cpus - int(current_load + 0.999999); if (value < 0) value = 0; print value }')"
if (( available_bytes < minimum_free_bytes )); then
  echo "V4 requires at least 50 GiB free under the repository filesystem." >&2
  exit 2
fi
if [[ "$mode" =~ ^(resource-rehearsal|execute)$ ]]; then
  if (( memory_available_bytes < minimum_available_memory_bytes ||
        estimated_idle < minimum_idle_cpus )); then
    echo "The shared host lacks the frozen V4 memory/idle-CPU headroom." >&2
    exit 2
  fi
  other_heavy="$(ps -eo pid=,comm=,args= | awk -v self="$$" '
    $1 != self && $2 == "R" &&
      ($0 ~ /\.rqr_gibbs_launch_checkouts/ ||
       $0 ~ /RQR-GIBBS\/application\/scripts\/(3[0-9]|4[0-9]|5[0-1])_/) {
        print $1
      }
  ')"
  if [[ -n "$other_heavy" ]]; then
    echo "Another RQR-GIBBS heavy validation process is active." >&2
    exit 2
  fi
fi

timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
short_sha="${RQR_EXPECTED_PRIMARY_COMMIT:-exploratory}"
short_sha="${short_sha:0:12}"
requested_output="${RQR_ORACLE_TILT_V4_OUTPUT_DIR:-$repo_root/application/outputs/oracle_tilt_c095_publication_v4_seed_screen/${mode}_${timestamp}_${short_sha}}"
if [[ -L "$requested_output" ]]; then
  echo "The V4 output directory must not be a symbolic link." >&2
  exit 2
fi
mkdir -p "$requested_output"
output_dir="$(cd "$requested_output" && pwd -P)"
first_entry="$(find "$output_dir" -mindepth 1 -maxdepth 1 -print -quit)"
if [[ -n "$first_entry" &&
      ! ( "$mode" == execute && "${RQR_ORACLE_TILT_V4_RESUME:-}" == YES ) ]]; then
  echo "The output must be fresh unless V4 execute resume is explicit." >&2
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
    END { printf "%d,%d,%d,%d\n", processes+0, r_processes+0, rss+0, threads+0 }
  '
}

terminate_group() {
  local group_id="$1"
  kill -TERM -- "-$group_id" 2>/dev/null || true
  for _ in $(seq 1 25); do
    [[ "$(pgid_snapshot "$group_id" | cut -d, -f1)" == 0 ]] && return 0
    sleep "$monitor_interval"
  done
  kill -KILL -- "-$group_id" 2>/dev/null || true
}
on_signal() {
  signal_received="$1"
  [[ -n "$pgid" ]] && terminate_group "$pgid"
}
trap 'on_signal INT' INT
trap 'on_signal TERM' TERM
trap 'on_signal HUP' HUP
trap 'if [[ -n "$pgid" ]]; then terminate_group "$pgid"; fi' EXIT

printf '%s\n' \
  "sample_utc,elapsed_seconds,processes,r_processes,rss_kib,threads" \
  >"$monitor_csv"
if [[ "$mode" == execute ]]; then
  runner=(
    bash application/scripts/53_orchestrate_oracle_tilt_v4_execute.sh
    "--config=$config" "--output-dir=$output_dir"
  )
elif [[ "$mode" == resource-rehearsal ]]; then
  runner=(
    bash application/scripts/54_run_oracle_tilt_v4_resource_rehearsal.sh
    "--config=$config" "--output-dir=$output_dir"
  )
else
  runner=(
    Rscript application/scripts/52_run_oracle_tilt_publication_v4.R
    "--mode=$mode" "--config=$config" "--output-dir=$output_dir"
  )
fi
setsid "${runner[@]}" >"$stdout_log" 2>"$stderr_log" &
root_pid=$!
pgid=$root_pid

while true; do
  snapshot="$(pgid_snapshot "$pgid")"
  IFS=, read -r processes r_processes rss_kib threads <<<"$snapshot"
  elapsed=$(( $(date +%s) - start_epoch ))
  printf '%s,%s,%s,%s,%s,%s\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$elapsed" "$processes" \
    "$r_processes" "$rss_kib" "$threads" >>"$monitor_csv"
  (( rss_kib > max_sampled_rss )) && max_sampled_rss=$rss_kib
  (( threads > max_sampled_threads )) && max_sampled_threads=$threads
  (( processes > max_sampled_processes )) && max_sampled_processes=$processes
  (( r_processes > max_sampled_r_processes )) && max_sampled_r_processes=$r_processes
  if (( elapsed > timeout_seconds )); then
    timed_out=TRUE; terminate_group "$pgid"; break
  fi
  if (( rss_kib > max_rss_kib || threads > max_threads ||
        processes > max_processes || r_processes > max_r_processes )); then
    limit_triggered=TRUE; terminate_group "$pgid"; break
  fi
  (( processes == 0 )) && break
  sleep "$monitor_interval"
done

set +e
wait "$root_pid"
runner_status=$?
set -e
terminate_group "$pgid"
[[ "$(pgid_snapshot "$pgid" | cut -d, -f1)" == 0 ]] && final_pgid_empty=TRUE
trap - EXIT

elapsed_total=$(( $(date +%s) - start_epoch ))
wrapper_pass=TRUE
if (( runner_status != 0 )) || [[ "$timed_out" != FALSE ]] ||
   [[ "$limit_triggered" != FALSE ]] || [[ "$final_pgid_empty" != TRUE ]] ||
   [[ "$signal_received" != NONE ]]; then
  wrapper_pass=FALSE
fi
printf '%s\n' \
  "mode,runner_status,elapsed_seconds,available_bytes_at_start,available_memory_bytes_at_start,logical_cpus,load_one,estimated_idle_cpus,minimum_free_bytes,minimum_available_memory_bytes,minimum_idle_cpus,max_sampled_rss_kib,max_sampled_threads,max_sampled_processes,max_sampled_r_processes,timeout_seconds,rss_limit_kib,thread_limit,process_limit,r_process_limit,timezone,timed_out,sampled_limit_triggered,signal_received,final_pgid_empty,telemetry_kind,pass" \
  >"$resource_csv"
printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
  "$mode" "$runner_status" "$elapsed_total" "$available_bytes" \
  "$memory_available_bytes" "$logical_cpus" "$load_one" "$estimated_idle" \
  "$minimum_free_bytes" "$minimum_available_memory_bytes" "$minimum_idle_cpus" \
  "$max_sampled_rss" "$max_sampled_threads" "$max_sampled_processes" \
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
  echo "The monitored V4 $mode run failed; inspect $output_dir." >&2
  exit 1
fi
echo "[oracle-tilt-v4-monitor] $mode passed: $output_dir"
