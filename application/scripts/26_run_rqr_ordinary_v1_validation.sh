#!/usr/bin/env bash
set -euo pipefail

# Process-group wrapper for the ordinary-RQR version-1 validator. Resource
# observations are sampled telemetry, not cgroup/kernel-hard measurements.
# The wrapper nevertheless fails closed when a configured sample exceeds the
# process, thread, or cumulative output/run byte contract.

mode="${1:-preflight}"
case "$mode" in
  preflight|reference-only|benchmark-one-cell|execute-bounded) ;;
  *)
    echo "Mode must be preflight, reference-only, benchmark-one-cell, or execute-bounded." >&2
    exit 2
    ;;
esac

repo_root="$(pwd -P)"
if [[ ! -f "$repo_root/application/DESCRIPTION" ||
      ! -f "$repo_root/application/scripts/25_validate_rqr_ordinary_v1.R" ]]; then
  echo "Run this wrapper from the RQR-GIBBS repository root." >&2
  exit 2
fi
if [[ ! "${RQR_EXPECTED_PRIMARY_COMMIT:-}" =~ ^[0-9a-fA-F]{40}$ ]]; then
  echo "RQR_EXPECTED_PRIMARY_COMMIT must be a complete reviewed SHA." >&2
  exit 2
fi
if [[ "$mode" == "benchmark-one-cell" &&
      "${RQR_ORDINARY_V1_BENCHMARK_CONFIRM:-}" != \
        "I_CONFIRM_ORDINARY_V1_ONE_CELL_BENCHMARK" ]]; then
  echo "benchmark-one-cell requires the exact benchmark confirmation." >&2
  exit 2
fi
if [[ "$mode" == "execute-bounded" ]]; then
  if [[ "${RQR_ORDINARY_V1_CONFIRM:-}" != "YES" ]]; then
    echo "execute-bounded requires RQR_ORDINARY_V1_CONFIRM=YES." >&2
    exit 2
  fi
  for required_directory in \
      RQR_ORDINARY_V1_REFERENCE_DIR \
      RQR_ORDINARY_V1_BENCHMARK_DIR \
      RQR_ORDINARY_V1_BENCHMARK_MONITOR_DIR \
      RQR_ORDINARY_V1_DLM_COMPANION_DIR; do
    if [[ -z "${!required_directory:-}" ]]; then
      echo "execute-bounded requires $required_directory." >&2
      exit 2
    fi
  done
fi
for command_name in \
    bash chmod date dd dirname find mkdir mktemp mv ps rm Rscript setsid \
    sha256sum sleep sort stat awk; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "$command_name is required by the ordinary-v1 monitor." >&2
    exit 2
  fi
done

# These limits are in the process environment before R starts.
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export RQR_ORDINARY_V1_MONITOR_ACTIVE=YES
export RQR_ORDINARY_V1_MONITOR_CLEANUP_TRAPS=YES
export RQR_ORDINARY_V1_MONITOR_FINAL_PGID_SWEEP=YES
export RQR_ORDINARY_V1_MONITOR_KIND=pgid_sampled_fail_closed
export RQR_ORDINARY_V1_MONITOR_KERNEL_HARD_RESOURCE_LIMITS=FALSE

timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
short_sha="${RQR_EXPECTED_PRIMARY_COMMIT:0:12}"
default_primary_runtime_root="$(
  dirname "$repo_root"
)/.rqr_gibbs_primary_runtime"
export RQR_PRIMARY_RUNTIME_ROOT="${RQR_PRIMARY_RUNTIME_ROOT:-$default_primary_runtime_root}"

if [[ "$mode" != "preflight" &&
      -z "${RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS:-}" ]]; then
  materialization_root="$repo_root/application/cache/rqr_ordinary_v1_desn_materialization/$RQR_EXPECTED_PRIMARY_COMMIT"
  design_name="rqr_ordinary_v1_attested_desn_design_${RQR_EXPECTED_PRIMARY_COMMIT}.rds"
  mapfile -t design_candidates < <(
    find "$materialization_root" -type f -name "$design_name" -print 2>/dev/null |
      sort
  )
  if [[ "${#design_candidates[@]}" -ne 1 ]]; then
    echo "Expected exactly one attested DESN design for this source SHA; found ${#design_candidates[@]}." >&2
    echo "Run make materialize-ordinary-v1-desn or set RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS explicitly." >&2
    exit 2
  fi
  export RQR_ORDINARY_V1_ATTESTED_DESN_DESIGN_RDS="${design_candidates[0]}"
fi

prepare_fresh_directory() {
  local label="$1"
  local requested="$2"
  local canonical first_entry
  if [[ -L "$requested" ]]; then
    echo "$label must not be a symbolic link." >&2
    return 1
  fi
  mkdir -p -- "$requested"
  canonical="$(cd "$requested" && pwd -P)"
  if ! first_entry="$(
    find "$canonical" -mindepth 1 -maxdepth 1 -print -quit
  )"; then
    echo "Could not inspect $label." >&2
    return 1
  fi
  if [[ -n "$first_entry" ]]; then
    echo "$label must be a fresh empty directory." >&2
    return 1
  fi
  printf '%s\n' "$canonical"
}

requested_output="${RQR_ORDINARY_V1_OUTPUT_DIR:-$repo_root/application/outputs/ordinary_v1_${mode//-/_}_${timestamp}_${short_sha}}"
requested_run="${RQR_ORDINARY_V1_RUN_DIR:-$repo_root/application/runs/ordinary_v1_${mode//-/_}_${timestamp}_${short_sha}}"
requested_monitor="${RQR_ORDINARY_V1_MONITOR_DIR:-$repo_root/application/logs/ordinary_v1_${mode//-/_}_${timestamp}_${short_sha}}"
output_dir="$(prepare_fresh_directory RQR_ORDINARY_V1_OUTPUT_DIR "$requested_output")"
run_dir="$(prepare_fresh_directory RQR_ORDINARY_V1_RUN_DIR "$requested_run")"
monitor_dir="$(prepare_fresh_directory RQR_ORDINARY_V1_MONITOR_DIR "$requested_monitor")"

case "$run_dir/" in
  "$output_dir/"*|"$monitor_dir/"*)
    echo "The run directory must be disjoint from output and monitor directories." >&2
    exit 2
    ;;
esac
case "$output_dir/" in
  "$run_dir/"*|"$monitor_dir/"*)
    echo "The output directory must be disjoint from run and monitor directories." >&2
    exit 2
    ;;
esac
case "$monitor_dir/" in
  "$output_dir/"*|"$run_dir/"*)
    echo "The monitor directory must be disjoint from output and run directories." >&2
    exit 2
    ;;
esac

export RQR_ORDINARY_V1_OUTPUT_DIR="$output_dir"
export RQR_ORDINARY_V1_RUN_DIR="$run_dir"
export RQR_ORDINARY_V1_MONITOR_DIR="$monitor_dir"

test_scenario="${RQR_ORDINARY_V1_MONITOR_TEST_SCENARIO:-}"
if [[ -n "$test_scenario" &&
      "${RQR_ORDINARY_V1_MONITOR_TEST_CONFIRM:-}" != "I_CONFIRM_ORDINARY_V1_MONITOR_TEST" ]]; then
  echo "Dummy monitor scenarios require explicit test confirmation." >&2
  exit 2
fi

timeout_seconds=14400
max_processes=3
max_threads=4
max_artifact_bytes=1073741824
monitor_interval=0.20
term_grace_samples=10
final_sweep_samples=50
if [[ -n "$test_scenario" ]]; then
  timeout_seconds="${RQR_ORDINARY_V1_TEST_TIMEOUT_SECONDS:-2}"
  max_processes="${RQR_ORDINARY_V1_TEST_MAX_PROCESSES:-4}"
  max_threads="${RQR_ORDINARY_V1_TEST_MAX_THREADS:-8}"
  max_artifact_bytes="${RQR_ORDINARY_V1_TEST_MAX_ARTIFACT_BYTES:-1048576}"
  monitor_interval="${RQR_ORDINARY_V1_TEST_MONITOR_INTERVAL:-0.05}"
  term_grace_samples=4
  final_sweep_samples=40
fi
for integer_value in \
    "$timeout_seconds" "$max_processes" "$max_threads" \
    "$max_artifact_bytes" "$term_grace_samples" "$final_sweep_samples"; do
  if [[ ! "$integer_value" =~ ^[1-9][0-9]*$ ]]; then
    echo "Monitor integer limits must be positive integers." >&2
    exit 2
  fi
done
if [[ ! "$monitor_interval" =~ ^0?\.[0-9]+$ &&
      ! "$monitor_interval" =~ ^[1-9][0-9]*(\.[0-9]+)?$ ]]; then
  echo "Monitor interval must be a positive decimal number." >&2
  exit 2
fi
if ! awk -v value="$monitor_interval" \
    'BEGIN { exit !(value > 0) }'; then
  echo "Monitor interval must be greater than zero." >&2
  exit 2
fi

export RQR_ORDINARY_V1_MONITOR_TIMEOUT_SECONDS="$timeout_seconds"
export RQR_ORDINARY_V1_MONITOR_MAX_PROCESSES="$max_processes"
export RQR_ORDINARY_V1_MONITOR_MAX_THREADS="$max_threads"
export RQR_ORDINARY_V1_MONITOR_MAX_ARTIFACT_BYTES="$max_artifact_bytes"

monitor_csv="$monitor_dir/process_group_monitor.csv"
resource_csv="$monitor_dir/process_group_resource_summary.csv"
closeout_csv="$monitor_dir/wrapper_closeout.csv"
manifest_csv="$monitor_dir/wrapper_artifact_hashes.csv"
stdout_log="$monitor_dir/runner.stdout.log"
stderr_log="$monitor_dir/runner.stderr.log"

root_pid=""
pgid=""
runner_waited=FALSE
runner_status=125
timed_out=FALSE
process_limit_triggered=FALSE
thread_limit_triggered=FALSE
artifact_limit_triggered=FALSE
monitor_error=FALSE
signal_received=NONE
kill_escalated=FALSE
final_pgid_empty=TRUE
residual_group_cleanup=FALSE
finalized=FALSE
final_exit_status=1
wrapper_incoming_status=1
peak_processes=0
peak_threads=0
peak_rss_kib=0
peak_artifact_bytes=0

pgid_process_count() {
  local group_id="$1"
  ps -eo pgid=,stat= | awk -v group="$group_id" '
    $1 == group && $2 !~ /^Z/ { count += 1 }
    END { print count + 0 }
  '
}

group_exists() {
  local group_id="$1"
  local count
  count="$(pgid_process_count "$group_id")" || return 2
  [[ "$count" -gt 0 ]]
}

signal_group() {
  local group_id="$1"
  local signal_name="$2"
  kill "-$signal_name" -- "-$group_id" 2>/dev/null || true
}

drain_group() {
  local group_id="$1"
  local index status
  signal_group "$group_id" TERM
  for ((index=0; index<term_grace_samples; index++)); do
    if group_exists "$group_id"; then
      sleep "$monitor_interval"
    else
      status=$?
      [[ "$status" -eq 1 ]] && return 0
      return 1
    fi
  done
  kill_escalated=TRUE
  signal_group "$group_id" KILL
  for ((index=0; index<final_sweep_samples; index++)); do
    if group_exists "$group_id"; then
      sleep "$monitor_interval"
    else
      status=$?
      [[ "$status" -eq 1 ]] && return 0
      return 1
    fi
  done
  return 1
}

sample_group_metrics() {
  local group_id="$1"
  ps -eo pgid=,stat=,rss=,nlwp= | awk -v group="$group_id" '
    $1 == group && $2 !~ /^Z/ {
      processes += 1
      rss += $3
      threads += $4
    }
    END {
      printf "%d,%d,%d", processes + 0, threads + 0, rss + 0
    }
  '
}

sample_artifact_bytes() {
  local attempt value
  for attempt in 1 2 3; do
    if value="$(
      find "$output_dir" "$run_dir" -type f -printf '%s\n' 2>/dev/null |
        awk '{ total += $1 } END { printf "%.0f", total + 0 }'
    )"; then
      printf '%s\n' "$value"
      return 0
    fi
    sleep 0.02
  done
  return 1
}

atomic_publish() {
  local temporary="$1"
  local target="$2"
  mv -T -- "$temporary" "$target"
}

append_wrapper_manifest_row() {
  local manifest_path="$1"
  local role="$2"
  local display_path="$3"
  local actual_path="$4"
  local bytes digest_value
  if [[ ! -f "$actual_path" || -L "$actual_path" ]]; then
    return 1
  fi
  bytes="$(stat -c '%s' "$actual_path")" || return 1
  digest_value="$(sha256sum "$actual_path" | awk '{ print $1 }')" ||
    return 1
  if [[ ! "$bytes" =~ ^[0-9]+$ ||
        ! "$digest_value" =~ ^[0-9a-f]{64}$ ]]; then
    return 1
  fi
  printf '%s,%s,%s,%s\n' \
    "$role" "$display_path" "$bytes" "$digest_value" >>"$manifest_path"
}

write_wrapper_manifest() {
  local temporary path_list path name relative
  local monitor_scan_failed=FALSE
  local output_scan_failed=FALSE
  local -A expected_monitor=(
    [process_group_monitor.csv]=TRUE
    [process_group_resource_summary.csv]=TRUE
    [runner.stderr.log]=TRUE
    [runner.stdout.log]=TRUE
    [wrapper_closeout.csv]=TRUE
  )
  temporary="$(mktemp "$monitor_dir/.wrapper_artifact_hashes.XXXXXX")" ||
    return 1
  path_list="$(mktemp "$monitor_dir/.wrapper_manifest_paths.XXXXXX")" || {
    rm -f -- "$temporary"
    return 1
  }
  if ! find "$monitor_dir" -mindepth 1 -maxdepth 1 \
      ! -name 'wrapper_artifact_hashes.csv' -print0 |
      LC_ALL=C sort -z >"$path_list"; then
    rm -f -- "$temporary" "$path_list"
    return 1
  fi
  printf '%s\n' "role,path,bytes,sha256" >"$temporary" || {
    rm -f -- "$temporary" "$path_list"
    return 1
  }
  for name in \
      process_group_monitor.csv \
      process_group_resource_summary.csv \
      runner.stderr.log \
      runner.stdout.log \
      wrapper_closeout.csv; do
    append_wrapper_manifest_row \
      "$temporary" monitor_evidence "monitor/$name" \
      "$monitor_dir/$name" || {
      rm -f -- "$temporary" "$path_list"
      return 1
    }
  done
  while IFS= read -r -d '' path; do
    if [[ "$path" == "$temporary" || "$path" == "$path_list" ]]; then
      continue
    fi
    name="${path##*/}"
    if [[ -z "${expected_monitor[$name]+present}" ]]; then
      monitor_scan_failed=TRUE
    fi
  done <"$path_list"
  rm -f -- "$path_list"
  path_list="$(mktemp "$monitor_dir/.wrapper_manifest_paths.XXXXXX")" || {
    rm -f -- "$temporary"
    return 1
  }
  if ! find "$output_dir" -mindepth 1 -print0 |
      LC_ALL=C sort -z >"$path_list"; then
    : >"$path_list"
    output_scan_failed=TRUE
  fi
  while IFS= read -r -d '' path; do
    relative="${path#"$output_dir"/}"
    if [[ -d "$path" && ! -L "$path" ]]; then
      if [[ "$mode" == execute-bounded &&
            "$relative" == protected_dlm_companion ]]; then
        continue
      fi
      rm -f -- "$temporary" "$path_list"
      return 1
    fi
    name="$relative"
    append_wrapper_manifest_row \
      "$temporary" r_evidence_binding "output/$name" "$path" || {
      rm -f -- "$temporary" "$path_list"
      return 1
    }
  done <"$path_list"
  rm -f -- "$path_list"
  atomic_publish "$temporary" "$manifest_csv" || return 1
  [[ "$monitor_scan_failed" == FALSE &&
     "$output_scan_failed" == FALSE ]]
}

validate_wrapper_manifest() {
  local header role relative bytes digest_value actual_path
  local actual_bytes actual_digest key path name output_name
  local -a expected_monitor=(
    process_group_monitor.csv
    process_group_resource_summary.csv
    runner.stderr.log
    runner.stdout.log
    wrapper_closeout.csv
  )
  local -A seen=()
  if [[ ! -f "$manifest_csv" || -L "$manifest_csv" ]]; then
    return 1
  fi
  {
    IFS= read -r header || return 1
    [[ "$header" == "role,path,bytes,sha256" ]] || return 1
    while IFS=, read -r role relative bytes digest_value; do
      [[ -n "$role" && -n "$relative" && "$bytes" =~ ^[0-9]+$ &&
         "$digest_value" =~ ^[0-9a-f]{64}$ ]] || return 1
      key="$role|$relative"
      [[ -z "${seen[$key]+present}" ]] || return 1
      seen["$key"]=TRUE
      case "$role:$relative" in
        monitor_evidence:monitor/process_group_monitor.csv|\
        monitor_evidence:monitor/process_group_resource_summary.csv|\
        monitor_evidence:monitor/runner.stderr.log|\
        monitor_evidence:monitor/runner.stdout.log|\
        monitor_evidence:monitor/wrapper_closeout.csv)
          actual_path="$monitor_dir/${relative#monitor/}"
          ;;
        r_evidence_binding:output/*)
          output_name="${relative#output/}"
          case "$output_name" in
            ""|/*|.|..|./*|../*|*/./*|*/../*|*/.|*/..|*//*)
              return 1
              ;;
          esac
          actual_path="$output_dir/$output_name"
          ;;
        *)
          return 1
          ;;
      esac
      [[ -f "$actual_path" && ! -L "$actual_path" ]] || return 1
      actual_bytes="$(stat -c '%s' "$actual_path")" || return 1
      actual_digest="$(sha256sum "$actual_path" | awk '{ print $1 }')" ||
        return 1
      [[ "$bytes" == "$actual_bytes" &&
         "$digest_value" == "$actual_digest" ]] || return 1
    done
  } <"$manifest_csv"
  for name in "${expected_monitor[@]}"; do
    key="monitor_evidence|monitor/$name"
    [[ -n "${seen[$key]+present}" ]] || return 1
  done
  while IFS= read -r -d '' path; do
    name="${path##*/}"
    case "$name" in
      process_group_monitor.csv|\
      process_group_resource_summary.csv|\
      runner.stderr.log|\
      runner.stdout.log|\
      wrapper_closeout.csv)
        key="monitor_evidence|monitor/$name"
        [[ -f "$path" && ! -L "$path" &&
           -n "${seen[$key]+present}" ]] || return 1
        ;;
      *)
        return 1
        ;;
    esac
  done < <(
    find "$monitor_dir" -mindepth 1 -maxdepth 1 \
      ! -name 'wrapper_artifact_hashes.csv' -print0 |
      LC_ALL=C sort -z
  )
  while IFS= read -r -d '' path; do
    output_name="${path#"$output_dir"/}"
    if [[ -d "$path" && ! -L "$path" ]]; then
      [[ "$mode" == execute-bounded &&
         "$output_name" == protected_dlm_companion ]] || return 1
      continue
    fi
    key="r_evidence_binding|output/$output_name"
    [[ -f "$path" && ! -L "$path" &&
       -n "${seen[$key]+present}" ]] || return 1
  done < <(
    find "$output_dir" -mindepth 1 -print0 |
      LC_ALL=C sort -z
  )
}

write_resource_summary() {
  local temporary
  temporary="$(mktemp "$monitor_dir/.resource_summary.XXXXXX")"
  {
    echo "metric,value,limit,pass,enforcement"
    echo "sampled_peak_processes,$peak_processes,$max_processes,$([[ $peak_processes -le $max_processes ]] && echo TRUE || echo FALSE),sampled_fail_closed_not_kernel_hard"
    echo "sampled_peak_threads,$peak_threads,$max_threads,$([[ $peak_threads -le $max_threads ]] && echo TRUE || echo FALSE),sampled_fail_closed_not_kernel_hard"
    echo "sampled_peak_rss_kib,$peak_rss_kib,NA,TRUE,telemetry_only_not_kernel_hard"
    echo "sampled_peak_output_run_bytes,$peak_artifact_bytes,$max_artifact_bytes,$([[ $peak_artifact_bytes -le $max_artifact_bytes ]] && echo TRUE || echo FALSE),sampled_cumulative_fail_closed"
    echo "hard_wall_timeout_triggered,$timed_out,FALSE,$([[ $timed_out == FALSE ]] && echo TRUE || echo FALSE),fail_closed"
    echo "process_limit_triggered,$process_limit_triggered,FALSE,$([[ $process_limit_triggered == FALSE ]] && echo TRUE || echo FALSE),sampled_fail_closed_not_kernel_hard"
    echo "thread_limit_triggered,$thread_limit_triggered,FALSE,$([[ $thread_limit_triggered == FALSE ]] && echo TRUE || echo FALSE),sampled_fail_closed_not_kernel_hard"
    echo "artifact_limit_triggered,$artifact_limit_triggered,FALSE,$([[ $artifact_limit_triggered == FALSE ]] && echo TRUE || echo FALSE),sampled_cumulative_fail_closed"
    echo "monitor_error,$monitor_error,FALSE,$([[ $monitor_error == FALSE ]] && echo TRUE || echo FALSE),fail_closed"
    echo "final_pgid_empty,$final_pgid_empty,TRUE,$([[ $final_pgid_empty == TRUE ]] && echo TRUE || echo FALSE),final_sweep"
    echo "residual_group_cleanup,$residual_group_cleanup,FALSE,$([[ $residual_group_cleanup == FALSE ]] && echo TRUE || echo FALSE),fail_closed"
    echo "kill_escalation_used,$kill_escalated,FALSE,$([[ $kill_escalated == FALSE ]] && echo TRUE || echo FALSE),term_then_kill"
    echo "runner_exit_status,$runner_status,0,$([[ $runner_status -eq 0 ]] && echo TRUE || echo FALSE),fail_closed"
    echo "wrapper_incoming_status,$wrapper_incoming_status,0,$([[ $wrapper_incoming_status -eq 0 ]] && echo TRUE || echo FALSE),fail_closed"
  } >"$temporary"
  atomic_publish "$temporary" "$resource_csv"
}

write_closeout() {
  local temporary resource_pass
  resource_pass=TRUE
  if [[ "$runner_status" -ne 0 || "$timed_out" == TRUE ||
        "$process_limit_triggered" == TRUE ||
        "$thread_limit_triggered" == TRUE ||
        "$artifact_limit_triggered" == TRUE ||
        "$monitor_error" == TRUE || "$final_pgid_empty" != TRUE ||
        "$residual_group_cleanup" == TRUE ||
        "$signal_received" != NONE ||
        "$wrapper_incoming_status" -ne 0 ]]; then
    resource_pass=FALSE
  fi
  temporary="$(mktemp "$monitor_dir/.wrapper_closeout.XXXXXX")"
  {
    echo "field,value"
    echo "schema_version,rqrgibbs_ordinary_v1_wrapper/1.0.0"
    echo "mode,$mode"
    echo "expected_primary_commit,${RQR_EXPECTED_PRIMARY_COMMIT,,}"
    echo "process_group_id,${pgid:-NA}"
    echo "runner_exit_status,$runner_status"
    echo "resource_pass,$resource_pass"
    echo "monitor_kind,pgid_sampled_fail_closed"
    echo "sampled_resource_maxima_are_kernel_hard,FALSE"
    echo "signal_received,$signal_received"
    echo "final_pgid_empty,$final_pgid_empty"
    echo "residual_group_cleanup,$residual_group_cleanup"
    echo "completed_at,$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  } >"$temporary"
  atomic_publish "$temporary" "$closeout_csv"
}

validate_r_output_bundle() {
  if [[ -n "$test_scenario" || "$runner_status" -ne 0 ]]; then
    return 0
  fi
  RQR_ORDINARY_V1_SOURCE_ONLY=YES \
  RQR_ORDINARY_V1_POSTRUN_SOURCE="$repo_root/application/scripts/25_validate_rqr_ordinary_v1.R" \
  RQR_ORDINARY_V1_POSTRUN_MODE="$mode" \
  RQR_ORDINARY_V1_POSTRUN_OUTPUT="$output_dir" \
    Rscript --vanilla -e '
      environment <- new.env(parent = globalenv())
      sys.source(
        Sys.getenv("RQR_ORDINARY_V1_POSTRUN_SOURCE"),
        envir = environment
      )
      output <- Sys.getenv("RQR_ORDINARY_V1_POSTRUN_OUTPUT")
      mode <- Sys.getenv("RQR_ORDINARY_V1_POSTRUN_MODE")
      manifest <- utils::read.csv(
        file.path(output, "artifact_hashes.csv"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      expected <- environment$rqr_ordinary_v1_expected_output_files(mode)
      if (!isTRUE(environment$rqr_ordinary_v1_validate_artifact_manifest(
        output, manifest, expected_files = expected
      ))) {
        stop("The post-R compact output validation failed.", call. = FALSE)
      }
    ' >/dev/null 2>&1
}

finalize_wrapper() {
  local incoming_status="${1:-1}"
  local status final_bytes
  if [[ "$finalized" == TRUE ]]; then
    return
  fi
  finalized=TRUE
  wrapper_incoming_status="$incoming_status"
  set +e
  if [[ -n "$pgid" ]]; then
    if group_exists "$pgid"; then
      residual_group_cleanup=TRUE
      drain_group "$pgid" || monitor_error=TRUE
    else
      status=$?
      [[ "$status" -eq 2 ]] && monitor_error=TRUE
    fi
  fi
  if [[ -n "$root_pid" && "$runner_waited" != TRUE ]]; then
    wait "$root_pid"
    runner_status=$?
    runner_waited=TRUE
  fi
  final_pgid_empty=TRUE
  if [[ -n "$pgid" ]]; then
    if group_exists "$pgid"; then
      final_pgid_empty=FALSE
      monitor_error=TRUE
    else
      status=$?
      if [[ "$status" -eq 2 ]]; then
        final_pgid_empty=FALSE
        monitor_error=TRUE
      fi
    fi
  fi
  if final_bytes="$(sample_artifact_bytes)"; then
    (( final_bytes > peak_artifact_bytes )) &&
      peak_artifact_bytes="$final_bytes"
    if (( final_bytes > max_artifact_bytes )); then
      artifact_limit_triggered=TRUE
    fi
  else
    monitor_error=TRUE
  fi
  write_resource_summary || monitor_error=TRUE
  write_closeout || monitor_error=TRUE
  if ! write_wrapper_manifest ||
      ! validate_r_output_bundle ||
      ! validate_wrapper_manifest; then
    monitor_error=TRUE
    # Re-publish summaries so the failure is visible, then make one final
    # atomic manifest/readback attempt over those final bytes.
    write_resource_summary || true
    write_closeout || true
    write_wrapper_manifest || true
    validate_r_output_bundle || true
    validate_wrapper_manifest || true
  fi

  if [[ "$signal_received" == INT ]]; then
    final_exit_status=130
  elif [[ "$signal_received" == TERM ]]; then
    final_exit_status=143
  elif [[ "$signal_received" == HUP ]]; then
    final_exit_status=129
  elif [[ "$timed_out" == TRUE ]]; then
    final_exit_status=124
  elif [[ "$runner_status" -ne 0 && "$runner_status" -lt 126 ]]; then
    final_exit_status="$runner_status"
  elif [[ "$incoming_status" -ne 0 ||
          "$process_limit_triggered" == TRUE ||
          "$thread_limit_triggered" == TRUE ||
          "$artifact_limit_triggered" == TRUE ||
          "$monitor_error" == TRUE ||
          "$residual_group_cleanup" == TRUE ||
          "$final_pgid_empty" != TRUE ]]; then
    final_exit_status=1
  else
    final_exit_status=0
  fi
  set -e
}

on_exit() {
  local incoming_status=$?
  trap - EXIT INT TERM HUP
  finalize_wrapper "$incoming_status"
  if [[ "$final_exit_status" -eq 0 ]]; then
    echo "ordinary-v1 wrapper PASS: $monitor_dir"
  else
    echo "ordinary-v1 wrapper FAIL: $monitor_dir" >&2
  fi
  exit "$final_exit_status"
}

handle_signal() {
  signal_received="$1"
  exit "$2"
}

trap on_exit EXIT
trap 'handle_signal INT 130' INT
trap 'handle_signal TERM 143' TERM
trap 'handle_signal HUP 129' HUP

printf '%s\n' \
  "elapsed_seconds,processes,threads,rss_kib,output_run_bytes" >"$monitor_csv"

case "$test_scenario" in
  "")
    runner_command=(
      Rscript
      "$repo_root/application/scripts/25_validate_rqr_ordinary_v1.R"
      "$mode"
    )
    ;;
  normal-exit)
    runner_command=(bash -c 'sleep 0.10; exit 0')
    ;;
  child-failure)
    runner_command=(
      bash -c '(sleep 0.10; exit 17) & child=$!; wait "$child"'
    )
    ;;
  timeout-cleanup)
    runner_command=(bash -c 'trap "" HUP TERM; sleep 60 & wait')
    ;;
  artifact-limit)
    runner_command=(
      bash -c
      'dd if=/dev/zero of="$RQR_ORDINARY_V1_OUTPUT_DIR/artifact-limit.bin" bs=4096 count=1 status=none; sleep 60'
    )
    ;;
  process-limit)
    runner_command=(
      bash -c
      'sleep 60 & sleep 60 & sleep 60 & sleep 60 & wait'
    )
    ;;
  thread-limit)
    # The contract limits the aggregate NLWP count for the process group.
    # Two single-threaded group members therefore exercise the same gate
    # deterministically without depending on a threaded language runtime.
    runner_command=(bash -c 'sleep 60 & wait')
    ;;
  signal-cleanup)
    runner_command=(bash -c 'sleep 60 & wait')
    ;;
  monitor-error)
    runner_command=(
      bash -c
      'chmod 000 "$RQR_ORDINARY_V1_OUTPUT_DIR"; sleep 60'
    )
    ;;
  monitor-hidden)
    runner_command=(
      bash -c
      'printf "%s\n" unexpected >"$RQR_ORDINARY_V1_MONITOR_DIR/.unexpected-monitor"; sleep 0.10; exit 0'
    )
    ;;
  monitor-temporary-lookalike)
    runner_command=(
      bash -c
      'printf "%s\n" unexpected >"$RQR_ORDINARY_V1_MONITOR_DIR/.wrapper_artifact_hashes.injected"; sleep 0.10; exit 0'
    )
    ;;
  *)
    echo "Unknown RQR_ORDINARY_V1_MONITOR_TEST_SCENARIO." >&2
    exit 2
    ;;
esac

start_epoch="$(date +%s)"
setsid "${runner_command[@]}" >"$stdout_log" 2>"$stderr_log" &
root_pid=$!
pgid=$root_pid

for _ in {1..50}; do
  if group_exists "$pgid"; then
    break
  fi
  status=$?
  if [[ "$status" -eq 2 ]]; then
    monitor_error=TRUE
    break
  fi
  if ! kill -0 "$root_pid" 2>/dev/null; then
    break
  fi
  sleep 0.02
done

while true; do
  if group_exists "$pgid"; then
    :
  else
    status=$?
    [[ "$status" -eq 2 ]] && monitor_error=TRUE
    break
  fi

  now_epoch="$(date +%s)"
  elapsed=$((now_epoch - start_epoch))
  if (( elapsed >= timeout_seconds )); then
    timed_out=TRUE
    drain_group "$pgid" || monitor_error=TRUE
    break
  fi
  if ! metrics="$(sample_group_metrics "$pgid")"; then
    monitor_error=TRUE
    drain_group "$pgid" || true
    break
  fi
  if ! artifact_bytes="$(sample_artifact_bytes)"; then
    monitor_error=TRUE
    drain_group "$pgid" || true
    break
  fi
  IFS=, read -r current_processes current_threads current_rss_kib \
    <<<"$metrics"
  (( current_processes > peak_processes )) &&
    peak_processes="$current_processes"
  (( current_threads > peak_threads )) && peak_threads="$current_threads"
  (( current_rss_kib > peak_rss_kib )) && peak_rss_kib="$current_rss_kib"
  (( artifact_bytes > peak_artifact_bytes )) &&
    peak_artifact_bytes="$artifact_bytes"
  printf '%s,%s,%s\n' "$elapsed" "$metrics" "$artifact_bytes" >>"$monitor_csv"

  if (( current_processes > max_processes )); then
    process_limit_triggered=TRUE
  fi
  if (( current_threads > max_threads )); then
    thread_limit_triggered=TRUE
  fi
  if (( artifact_bytes > max_artifact_bytes )); then
    artifact_limit_triggered=TRUE
  fi
  if [[ "$process_limit_triggered" == TRUE ||
        "$thread_limit_triggered" == TRUE ||
        "$artifact_limit_triggered" == TRUE ]]; then
    drain_group "$pgid" || monitor_error=TRUE
    break
  fi
  if ! kill -0 "$root_pid" 2>/dev/null; then
    set +e
    wait "$root_pid"
    runner_status=$?
    set -e
    runner_waited=TRUE
    if group_exists "$pgid"; then
      residual_group_cleanup=TRUE
      drain_group "$pgid" || monitor_error=TRUE
    else
      status=$?
      [[ "$status" -eq 2 ]] && monitor_error=TRUE
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
  runner_waited=TRUE
fi
if group_exists "$pgid"; then
  residual_group_cleanup=TRUE
  drain_group "$pgid" || monitor_error=TRUE
else
  status=$?
  [[ "$status" -eq 2 ]] && monitor_error=TRUE
fi

if [[ "$runner_status" -ne 0 || "$timed_out" == TRUE ||
      "$process_limit_triggered" == TRUE ||
      "$thread_limit_triggered" == TRUE ||
      "$artifact_limit_triggered" == TRUE ||
      "$residual_group_cleanup" == TRUE ||
      "$monitor_error" == TRUE ]]; then
  exit 1
fi
exit 0
