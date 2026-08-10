#!/usr/bin/env bash
set -euo pipefail

mode="${1:-preflight}"
case "$mode" in
  preflight|reference-only|benchmark|sentinel|execute-wave) ;;
  *)
    echo "The monitored wrapper supports preflight, reference-only, benchmark, sentinel, or execute-wave." >&2
    exit 2
    ;;
esac

repo_root="$(pwd -P)"
if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this wrapper from the RQR-GIBBS repository root." >&2
  exit 2
fi
for command_name in setsid ps awk sha256sum stat find sort date df du Rscript; do
  command -v "$command_name" >/dev/null 2>&1 || {
    echo "$command_name is required by the monitored runner." >&2
    exit 2
  }
done

promotion=FALSE
if [[ "$mode" == benchmark || "$mode" == sentinel || "$mode" == execute-wave ]]; then
  promotion=TRUE
  if [[ ! "${RQR_EXPECTED_PRIMARY_COMMIT:-}" =~ ^[0-9a-fA-F]{40}$ ]]; then
    echo "Promotion modes require RQR_EXPECTED_PRIMARY_COMMIT as a full SHA." >&2
    exit 2
  fi
  if [[ -z "${RQR_PRIMARY_RUNTIME_ATTESTATION:-}" ]]; then
    echo "Promotion modes require RQR_PRIMARY_RUNTIME_ATTESTATION." >&2
    exit 2
  fi
fi

if [[ -n "${RQR_PRIMARY_RUNTIME_ATTESTATION:-}" ]]; then
  ordinary_library_path="$(Rscript -e 'cat(paste(.libPaths(), collapse=.Platform$path.sep))')"
  attested_library="$(Rscript -e '
    args <- commandArgs(trailingOnly = TRUE)
    attestation <- readRDS(args[[1L]])
    path <- attestation$runtime_package_path
    if (!is.character(path) || length(path) != 1L || !dir.exists(path)) {
      stop("Invalid runtime package path in attestation.", call. = FALSE)
    }
    cat(dirname(normalizePath(path, winslash = "/", mustWork = TRUE)))
  ' "$RQR_PRIMARY_RUNTIME_ATTESTATION")"
  export R_LIBS_USER="${attested_library}:${ordinary_library_path}"
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
  benchmark) timeout_seconds=43200 ;;
  sentinel) timeout_seconds=86400 ;;
  execute-wave) timeout_seconds=86400 ;;
esac
max_rss_kib=25165824
max_threads=32
max_processes=24
max_r_processes=16
monitor_interval=0.2
minimum_free_bytes=53687091200
max_run_bytes=0

config_path="${RQR_OMTV_CONFIG:-application/config/oracle_mean_tilt_validation_v1.json}"
resource_line="$(Rscript -e '
  args <- commandArgs(trailingOnly = TRUE)
  source("application/scripts/60_oracle_mean_tilt_validation_utils.R")
  config <- omtv_read_config(args[[1L]])
  omtv_validate_config(config)
  if (isTRUE(config$replication_schedule_frozen)) {
    cat(paste(
      format(config$resources$maximum_wave_seconds, scientific = FALSE, trim = TRUE),
      format(config$resources$maximum_process_tree_rss_kib, scientific = FALSE, trim = TRUE),
      format(config$resources$maximum_run_bytes, scientific = FALSE, trim = TRUE),
      format(config$resources$maximum_fit_workers, scientific = FALSE, trim = TRUE),
      format(config$resources$minimum_free_bytes, scientific = FALSE, trim = TRUE),
      sep = ","
    ))
  }
' "$config_path")"
if [[ -n "$resource_line" ]]; then
  IFS=, read -r timeout_seconds max_rss_kib max_run_bytes fit_workers \
    minimum_free_bytes \
    <<<"$resource_line"
  for numeric_value in \
      "$timeout_seconds" "$max_rss_kib" "$max_run_bytes" "$fit_workers" \
      "$minimum_free_bytes"; do
    [[ "$numeric_value" =~ ^[0-9]+$ ]] || {
      echo "The frozen resource contract did not yield whole-number ceilings." >&2
      exit 2
    }
  done
  max_r_processes=$((fit_workers + 2))
  max_processes=$((fit_workers + 8))
  max_threads=$((fit_workers + 8))
fi

available_kib="$(df -Pk "$repo_root" | awk 'NR == 2 {print $4}')"
if [[ ! "$available_kib" =~ ^[0-9]+$ ]] ||
   (( available_kib * 1024 < minimum_free_bytes )); then
  echo "At least $minimum_free_bytes free bytes are required." >&2
  exit 2
fi

timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
short_sha="${RQR_EXPECTED_PRIMARY_COMMIT:-exploratory}"
short_sha="${short_sha:0:12}"
requested_output="${RQR_OMTV_OUTPUT_DIR:-$repo_root/application/outputs/oracle_mean_tilt_validation_v1/${mode}_${timestamp}_${short_sha}}"
if [[ -L "$requested_output" ]]; then
  echo "The output directory must not be a symbolic link." >&2
  exit 2
fi
mkdir -p "$requested_output"
output_dir="$(cd "$requested_output" && pwd -P)"
if [[ -n "$(find "$output_dir" -mindepth 1 -maxdepth 1 -print -quit)" &&
      ! ( "$mode" == execute-wave && "${RQR_OMTV_RESUME:-}" == YES ) ]]; then
  echo "The output directory must be fresh unless execute-wave resume is explicit." >&2
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
max_sampled_output_bytes=0
last_output_check=-5
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

printf '%s\n' "sample_utc,elapsed_seconds,processes,r_processes,rss_kib,threads" >"$monitor_csv"
runner=(
  Rscript application/scripts/60_run_oracle_mean_tilt_validation.R
  "--mode=$mode"
  "--config=${RQR_OMTV_CONFIG:-application/config/oracle_mean_tilt_validation_v1.json}"
  "--output-dir=$output_dir"
)
if [[ "$mode" == execute-wave ]]; then
  runner+=("--wave=${RQR_OMTV_WAVE:-0}")
fi

setsid "${runner[@]}" >"$stdout_log" 2>"$stderr_log" &
root_pid=$!
pgid=$root_pid

while true; do
  snapshot="$(pgid_snapshot "$pgid")"
  IFS=, read -r processes r_processes rss threads <<<"$snapshot"
  elapsed=$(( $(date +%s) - start_epoch ))
  printf '%s,%d,%d,%d,%d,%d\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$elapsed" "$processes" \
    "$r_processes" "$rss" "$threads" >>"$monitor_csv"
  (( rss > max_sampled_rss )) && max_sampled_rss=$rss
  (( threads > max_sampled_threads )) && max_sampled_threads=$threads
  (( processes > max_sampled_processes )) && max_sampled_processes=$processes
  (( r_processes > max_sampled_r_processes )) && max_sampled_r_processes=$r_processes
  if (( elapsed - last_output_check >= 5 )); then
    sampled_output_bytes=$(( $(du -sk "$output_dir" | awk '{print $1}') * 1024 ))
    (( sampled_output_bytes > max_sampled_output_bytes )) &&
      max_sampled_output_bytes=$sampled_output_bytes
    last_output_check=$elapsed
  fi
  if (( rss > max_rss_kib || threads > max_threads ||
        processes > max_processes || r_processes > max_r_processes ||
        (max_run_bytes > 0 && sampled_output_bytes > max_run_bytes) )); then
    limit_triggered=TRUE
    terminate_group "$pgid"
    break
  fi
  if (( elapsed > timeout_seconds )); then
    timed_out=TRUE
    terminate_group "$pgid"
    break
  fi
  (( processes == 0 )) && break
  sleep "$monitor_interval"
done

set +e
wait "$root_pid"
runner_status=$?
set -e
[[ "$(pgid_snapshot "$pgid" | cut -d, -f1)" == 0 ]] && final_pgid_empty=TRUE
trap - EXIT
final_output_bytes=$(( $(du -sk "$output_dir" | awk '{print $1}') * 1024 ))
(( final_output_bytes > max_sampled_output_bytes )) &&
  max_sampled_output_bytes=$final_output_bytes
if (( max_run_bytes > 0 && final_output_bytes > max_run_bytes )); then
  limit_triggered=TRUE
fi

wrapper_pass=FALSE
if (( runner_status == 0 )) && [[ "$timed_out" == FALSE &&
    "$limit_triggered" == FALSE && "$final_pgid_empty" == TRUE &&
    "$signal_received" == NONE ]]; then
  wrapper_pass=TRUE
fi
printf '%s\n' \
  "mode,runner_status,pass,max_sampled_rss_kib,max_sampled_threads,max_sampled_processes,max_sampled_r_processes,max_sampled_output_bytes,timed_out,limit_triggered,final_pgid_empty,monitor_kind" \
  "$mode,$runner_status,$wrapper_pass,$max_sampled_rss,$max_sampled_threads,$max_sampled_processes,$max_sampled_r_processes,$max_sampled_output_bytes,$timed_out,$limit_triggered,$final_pgid_empty,pgid_sampled_fallback" >"$resource_csv"
printf '%s\n' \
  "mode,runner_status,wrapper_pass,signal_received,final_pgid_empty,promotion_mode" \
  "$mode,$runner_status,$wrapper_pass,$signal_received,$final_pgid_empty,$promotion" >"$wrapper_closeout"
if [[ "$wrapper_pass" != TRUE ]]; then
  printf '%s\n' "mode,runner_status,timed_out,limit_triggered,signal_received" \
    "$mode,$runner_status,$timed_out,$limit_triggered,$signal_received" >"$failure_log"
fi

printf '%s\n' "path,bytes,sha256" >"$wrapper_manifest"
for path in "$monitor_csv" "$resource_csv" "$wrapper_closeout" "$stdout_log" "$stderr_log"; do
  [[ -f "$path" ]] || continue
  printf '%s,%s,%s\n' "$(basename "$path")" "$(stat -c %s "$path")" \
    "$(sha256sum "$path" | awk '{print $1}')" >>"$wrapper_manifest"
done
if [[ -f "$failure_log" ]]; then
  printf '%s,%s,%s\n' "$(basename "$failure_log")" "$(stat -c %s "$failure_log")" \
    "$(sha256sum "$failure_log" | awk '{print $1}')" >>"$wrapper_manifest"
fi

if [[ "$wrapper_pass" != TRUE ]]; then
  echo "The monitored $mode run failed; see $output_dir." >&2
  exit "$runner_status"
fi
echo "[oracle-mean-tilt-validation] monitored $mode passed: $output_dir"
