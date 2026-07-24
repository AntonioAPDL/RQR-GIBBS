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
for command_name in setsid ps awk sha256sum stat Rscript find sort date \
    mv mktemp; do
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
requested_output_dir="${RQR_DLM_OUTPUT_DIR:-$repo_root/application/outputs/rqr_dlm_bounded_${mode//-/_}_${timestamp}_${short_sha}}"
if [[ -L "$requested_output_dir" ]]; then
  echo "RQR_DLM_OUTPUT_DIR must not be a symbolic link." >&2
  exit 2
fi
mkdir -p "$requested_output_dir"
output_dir="$(cd "$requested_output_dir" && pwd -P)"
if ! first_output_entry="$(
  find "$output_dir" -mindepth 1 -maxdepth 1 -print -quit
)"; then
  echo "Could not verify that RQR_DLM_OUTPUT_DIR is empty." >&2
  exit 2
fi
if [[ -n "$first_output_entry" ]]; then
  echo "RQR_DLM_OUTPUT_DIR must be a fresh empty directory." >&2
  exit 2
fi
export RQR_DLM_OUTPUT_DIR="$output_dir"

# These values mirror the frozen config. RSS is sampled telemetry with
# best-effort termination; it is not a cgroup/kernel-hard memory ceiling.
timeout_seconds=14400
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
failure_csv="$output_dir/failure_log.csv"

root_pid=""
pgid=""
runner_waited=FALSE
runner_status=125
timed_out=FALSE
sampled_limit_triggered=FALSE
monitor_error=FALSE
pgid_query_error=FALSE
signal_received=NONE
fault_pass=FALSE
final_pgid_empty=TRUE
main_kill_escalated=FALSE
finalizer_error=FALSE
finalized=FALSE
final_exit_status=1
artifact_manifest_error=NONE
final_evidence_error=NONE
artifact_test_fault="${RQR_MONITOR_TEST_ARTIFACT_FAULT:-}"
case "$artifact_test_fault" in
  ""|enumeration-failure|hash-failure-after-first|canonical-target-directory) ;;
  *)
    echo "Unknown RQR_MONITOR_TEST_ARTIFACT_FAULT." >&2
    exit 2
    ;;
esac
if [[ -n "$artifact_test_fault" &&
      "${RQR_MONITOR_TEST_CONFIRM:-}" != "I_CONFIRM_RQR_MONITOR_FAULT_TEST" ]]; then
  echo "Artifact fault injection requires its explicit confirmation." >&2
  exit 2
fi

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
  if ! count="$(pgid_process_count "$group_id")"; then
    pgid_query_error=TRUE
    kill -0 -- "-$group_id" 2>/dev/null
    return
  fi
  [[ "$count" -gt 0 ]]
}

terminate_group() {
  local group_id="$1"
  local signal_name="$2"
  kill "-$signal_name" -- "-$group_id" 2>/dev/null || true
}

drain_group() {
  local group_id="$1"
  local term_rounds="${2:-25}"
  local drain_index
  terminate_group "$group_id" TERM
  for ((drain_index=0; drain_index<term_rounds; drain_index++)); do
    if ! group_exists "$group_id"; then
      return 0
    fi
    sleep "$monitor_interval"
  done
  main_kill_escalated=TRUE
  terminate_group "$group_id" KILL
  for ((drain_index=0; drain_index<25; drain_index++)); do
    if ! group_exists "$group_id"; then
      return 0
    fi
    sleep "$monitor_interval"
  done
  return 1
}

csv_quote() {
  local value="${1//\"/\"\"}"
  printf '"%s"' "$value"
}

append_wrapper_failure() {
  local stage="$1"
  local message="$2"
  if [[ -e "$failure_csv" || -L "$failure_csv" ]]; then
    if [[ ! -f "$failure_csv" || -L "$failure_csv" ]]; then
      return 1
    fi
  elif ! printf '%s\n' \
      "recorded_at,mode,stage,fixture_id,learning_rate_mode,chain,message" \
      >"$failure_csv"; then
    return 1
  fi
  if ! {
    csv_quote "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    printf ","
    csv_quote "$mode"
    printf ","
    csv_quote "$stage"
    printf ",,,,"
    csv_quote "$message"
    printf "\n"
  } >>"$failure_csv"; then
    return 1
  fi
}

publish_regular_file() {
  local source="$1"
  local target="$2"
  if [[ ! -f "$source" || -L "$source" ]]; then
    return 1
  fi
  if [[ -e "$target" || -L "$target" ]]; then
    if [[ ! -f "$target" || -L "$target" ]]; then
      return 1
    fi
  fi
  mv -T -- "$source" "$target" || return 1
  [[ -f "$target" && ! -L "$target" ]]
}

enumerate_artifact_paths() {
  find "$output_dir" -type f \
    ! -name 'artifact_hashes.csv' \
    ! -name 'artifact_hashes.csv.tmp.*' \
    ! -name '.artifact_paths.*' -print0 || return 1
  if [[ "$artifact_test_fault" == enumeration-failure ]]; then
    return 42
  fi
}

write_artifact_manifest() {
  local artifact_tmp path_list
  local path relative digest_value bytes escaped_relative row_count
  artifact_manifest_error=NONE
  artifact_tmp="$(mktemp "${artifact_csv}.tmp.XXXXXX")" || {
    artifact_manifest_error=artifact_manifest_temp_failed
    return 1
  }
  path_list="$(mktemp "$output_dir/.artifact_paths.XXXXXX")" || {
    artifact_manifest_error=artifact_path_list_temp_failed
    rm -f -- "$artifact_tmp"
    return 1
  }
  if ! enumerate_artifact_paths | sort -z >"$path_list"; then
    artifact_manifest_error=artifact_enumeration_or_sort_failed
    rm -f -- "$artifact_tmp" "$path_list"
    return 1
  fi
  if ! printf '%s\n' "sha256,bytes,path" >"$artifact_tmp"; then
    artifact_manifest_error=artifact_manifest_header_failed
    rm -f -- "$artifact_tmp" "$path_list"
    return 1
  fi
  row_count=0
  while IFS= read -r -d '' path; do
    relative="${path#"$output_dir"/}"
    if ! digest_value="$(sha256sum "$path" | awk '{print $1}')"; then
      artifact_manifest_error=artifact_sha256_failed
      rm -f -- "$artifact_tmp" "$path_list"
      return 1
    fi
    if ! bytes="$(stat -c '%s' "$path")"; then
      artifact_manifest_error=artifact_stat_failed
      rm -f -- "$artifact_tmp" "$path_list"
      return 1
    fi
    escaped_relative="${relative//\"/\"\"}"
    if ! printf '%s,%s,"%s"\n' \
        "$digest_value" "$bytes" "$escaped_relative" >>"$artifact_tmp"; then
      artifact_manifest_error=artifact_manifest_row_failed
      rm -f -- "$artifact_tmp" "$path_list"
      return 1
    fi
    row_count=$((row_count + 1))
    if [[ "$artifact_test_fault" == hash-failure-after-first &&
          "$row_count" -ge 1 ]]; then
      artifact_manifest_error=artifact_injected_hash_failure
      rm -f -- "$artifact_tmp" "$path_list"
      return 1
    fi
  done <"$path_list"
  rm -f -- "$path_list"
  if ! publish_regular_file "$artifact_tmp" "$artifact_csv"; then
    artifact_manifest_error=artifact_manifest_publish_failed
    rm -f -- "$artifact_tmp"
    return 1
  fi
}

write_resource_summary() {
  local resource_tmp
  resource_tmp="$(mktemp "${resource_csv}.tmp.XXXXXX")" || return 1
  if ! {
    echo "metric,value,limit,pass"
    echo "sampled_process_group_peak_processes,$peak_processes,$max_processes,$([[ $peak_processes -le $max_processes ]] && echo TRUE || echo FALSE)"
    echo "sampled_process_group_peak_threads,$peak_threads,$max_threads,$([[ $peak_threads -le $max_threads ]] && echo TRUE || echo FALSE)"
    echo "sampled_process_group_peak_rss_kib,$peak_rss_kib,$max_rss_kib,$([[ $peak_rss_kib -le $max_rss_kib ]] && echo TRUE || echo FALSE)"
    echo "hard_timeout_triggered,$timed_out,FALSE,$([[ $timed_out == FALSE ]] && echo TRUE || echo FALSE)"
    echo "sampled_limit_triggered,$sampled_limit_triggered,FALSE,$([[ $sampled_limit_triggered == FALSE ]] && echo TRUE || echo FALSE)"
    echo "monitor_error,$monitor_error,FALSE,$([[ $monitor_error == FALSE ]] && echo TRUE || echo FALSE)"
    echo "pgid_query_error,$pgid_query_error,FALSE,$([[ $pgid_query_error == FALSE ]] && echo TRUE || echo FALSE)"
    echo "finalizer_error,$finalizer_error,FALSE,$([[ $finalizer_error == FALSE ]] && echo TRUE || echo FALSE)"
    echo "signal_received,$signal_received,NONE,$([[ $signal_received == NONE ]] && echo TRUE || echo FALSE)"
    echo "final_pgid_empty,$final_pgid_empty,TRUE,$([[ $final_pgid_empty == TRUE ]] && echo TRUE || echo FALSE)"
    echo "runner_exit_status,$runner_status,0,$([[ $runner_status -eq 0 ]] && echo TRUE || echo FALSE)"
    echo "monitor_fault_test_pass,$fault_pass,TRUE,$([[ $fault_pass == TRUE ]] && echo TRUE || echo FALSE)"
    echo "pgid_kill_escalation_used,$main_kill_escalated,FALSE,$([[ $main_kill_escalated == FALSE ]] && echo TRUE || echo FALSE)"
    echo "kernel_hard_memory_ceiling,FALSE,FALSE,TRUE"
  } >"$resource_tmp"; then
    rm -f -- "$resource_tmp"
    return 1
  fi
  if ! publish_regular_file "$resource_tmp" "$resource_csv"; then
    rm -f -- "$resource_tmp"
    return 1
  fi
}

write_closeout_summary() {
  local closeout_tmp
  closeout_tmp="$(mktemp "${closeout_csv}.tmp.XXXXXX")" || return 1
  if ! {
    echo "field,value"
    echo "schema_version,rqrgibbs_dlm_wrapper_closeout/2.1.0"
    echo "mode,$mode"
    echo "expected_primary_commit,${RQR_EXPECTED_PRIMARY_COMMIT,,}"
    echo "process_group_id,${pgid:-NA}"
    echo "runner_exit_status,$runner_status"
    echo "resource_pass,$resource_pass"
    echo "monitor_kind,pgid_sampled_fallback"
    echo "kernel_hard_memory_ceiling,FALSE"
    echo "signal_received,$signal_received"
    echo "final_pgid_empty,$final_pgid_empty"
    echo "finalizer_error,$finalizer_error"
    echo "completed_at,$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  } >"$closeout_tmp"; then
    rm -f -- "$closeout_tmp"
    return 1
  fi
  if ! publish_regular_file "$closeout_tmp" "$closeout_csv"; then
    rm -f -- "$closeout_tmp"
    return 1
  fi
}

write_final_evidence() {
  local evidence_failed=FALSE
  local -a evidence_errors=()
  if ! write_resource_summary; then
    evidence_failed=TRUE
    evidence_errors+=(resource_summary_publish_failed)
  fi
  if ! write_closeout_summary; then
    evidence_failed=TRUE
    evidence_errors+=(wrapper_closeout_publish_failed)
  fi
  if ! write_artifact_manifest; then
    evidence_failed=TRUE
    evidence_errors+=("artifact_manifest:$artifact_manifest_error")
  fi
  if [[ "$evidence_failed" == TRUE ]]; then
    final_evidence_error="$(
      IFS=';'
      echo "${evidence_errors[*]}"
    )"
  else
    final_evidence_error=NONE
  fi
  [[ "$evidence_failed" == FALSE ]]
}

finalize_wrapper() {
  local incoming_status="${1:-1}"
  local maxima peak_processes peak_threads peak_rss_kib
  local resource_pass
  if [[ "$finalized" == TRUE ]]; then
    return 0
  fi
  finalized=TRUE
  set +e

  if [[ -n "$pgid" ]] && group_exists "$pgid"; then
    if ! drain_group "$pgid"; then
      finalizer_error=TRUE
    fi
  fi
  if [[ -n "$root_pid" && "$runner_waited" != TRUE ]]; then
    wait "$root_pid"
    runner_status=$?
    runner_waited=TRUE
  fi
  final_pgid_empty=TRUE
  if [[ -n "$pgid" ]] && group_exists "$pgid"; then
    final_pgid_empty=FALSE
    finalizer_error=TRUE
  fi

  if [[ -s "$monitor_csv" ]]; then
    if ! maxima="$(
      awk -F, '
        NR > 1 {
          if ($2 > p) p=$2
          if ($3 > t) t=$3
          if ($4 > r) r=$4
        }
        END { printf "%d,%d,%d", p + 0, t + 0, r + 0 }
      ' "$monitor_csv"
    )"; then
      maxima="0,0,0"
      finalizer_error=TRUE
    fi
  else
    maxima="0,0,0"
  fi
  if [[ ! "$maxima" =~ ^[0-9]+,[0-9]+,[0-9]+$ ]]; then
    maxima="0,0,0"
    finalizer_error=TRUE
  fi
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
        "$pgid_query_error" == TRUE ||
        "$final_pgid_empty" != TRUE ||
        "$fault_pass" != TRUE ||
        "$finalizer_error" == TRUE ||
        "$main_kill_escalated" == TRUE ||
        "$signal_received" != NONE ||
        "$runner_status" -ne 0 ||
        "$incoming_status" -ne 0 ]]; then
    resource_pass=FALSE
  fi

  if [[ "$resource_pass" != TRUE ]]; then
    if ! append_wrapper_failure \
      "wrapper_finalization" \
      "signal=$signal_received runner_status=$runner_status incoming_status=$incoming_status timed_out=$timed_out sampled_limit=$sampled_limit_triggered monitor_error=$monitor_error pgid_query_error=$pgid_query_error final_pgid_empty=$final_pgid_empty"; then
      finalizer_error=TRUE
      resource_pass=FALSE
    fi
  elif [[ ! -e "$failure_csv" && ! -L "$failure_csv" ]]; then
    if ! printf '%s\n' \
        "recorded_at,mode,stage,fixture_id,learning_rate_mode,chain,message" \
        >"$failure_csv"; then
      finalizer_error=TRUE
      resource_pass=FALSE
    fi
  elif [[ ! -f "$failure_csv" || -L "$failure_csv" ]]; then
    finalizer_error=TRUE
    resource_pass=FALSE
  fi

  if ! write_final_evidence; then
    finalizer_error=TRUE
    resource_pass=FALSE
    append_wrapper_failure \
      "final_evidence" \
      "Final evidence publication failed: $final_evidence_error." ||
      true
    # Rewrite all evidence after recording the failure. The manifest is always
    # attempted last so a successful manifest cannot be made stale by a later
    # resource, closeout, or failure-ledger write.
    write_resource_summary || true
    write_closeout_summary || true
    write_artifact_manifest || true
  fi

  if [[ "$resource_pass" == TRUE ]]; then
    final_exit_status=0
  elif [[ "$signal_received" == INT ]]; then
    final_exit_status=130
  elif [[ "$signal_received" == TERM ]]; then
    final_exit_status=143
  elif [[ "$signal_received" == HUP ]]; then
    final_exit_status=129
  else
    final_exit_status=1
  fi
  set -e
}

on_exit() {
  local incoming_status=$?
  trap - EXIT INT TERM HUP
  finalize_wrapper "$incoming_status"
  if [[ "$final_exit_status" -eq 0 ]]; then
    echo "Bounded runner completed: $output_dir"
  else
    echo "Bounded runner failed; inspect $output_dir" >&2
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

if [[ "$artifact_test_fault" == canonical-target-directory ]]; then
  mkdir "$resource_csv"
fi

printf '%s\n' "elapsed_seconds,processes,threads,rss_kib" >"$monitor_csv"

# Fault injection: the leader exits nonzero while a HUP/TERM-resistant
# descendant remains. The final PGID sweep must require KILL and empty it.
main_kill_escalated=FALSE
setsid bash -c 'trap "" HUP TERM; sleep 60 & exit 17' &
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
if ! drain_group "$fault_pgid" 5; then
  fault_drained=FALSE
fi
fault_kill_escalated="$main_kill_escalated"
main_kill_escalated=FALSE
fault_final_empty=TRUE
if group_exists "$fault_pgid"; then
  fault_final_empty=FALSE
fi
fault_pass=FALSE
if [[ "$fault_status" -eq 17 &&
      "$fault_descendant_seen" == TRUE &&
      "$fault_drained" == TRUE &&
      "$fault_kill_escalated" == TRUE &&
      "$fault_final_empty" == TRUE ]]; then
  fault_pass=TRUE
fi
fault_tmp="${fault_csv}.tmp.$$"
{
  echo "metric,value,expected,pass"
  echo "leader_exit_status,$fault_status,17,$([[ $fault_status -eq 17 ]] && echo TRUE || echo FALSE)"
  echo "descendant_seen_after_leader_exit,$fault_descendant_seen,TRUE,$([[ $fault_descendant_seen == TRUE ]] && echo TRUE || echo FALSE)"
  echo "pgid_drain_completed,$fault_drained,TRUE,$([[ $fault_drained == TRUE ]] && echo TRUE || echo FALSE)"
  echo "kill_escalation_used,$fault_kill_escalated,TRUE,$([[ $fault_kill_escalated == TRUE ]] && echo TRUE || echo FALSE)"
  echo "final_pgid_empty,$fault_final_empty,TRUE,$([[ $fault_final_empty == TRUE ]] && echo TRUE || echo FALSE)"
} >"$fault_tmp"
publish_regular_file "$fault_tmp" "$fault_csv"
if [[ "$fault_pass" != TRUE ]]; then
  echo "The process-group fault test failed; inspect $fault_csv." >&2
  exit 1
fi

test_scenario="${RQR_MONITOR_TEST_SCENARIO:-}"
if [[ -n "$test_scenario" &&
      "${RQR_MONITOR_TEST_CONFIRM:-}" != "I_CONFIRM_RQR_MONITOR_FAULT_TEST" ]]; then
  echo "Monitor fault injection requires its explicit confirmation." >&2
  exit 2
fi

case "$test_scenario" in
  "")
    runner_command=(
      Rscript
      "$repo_root/application/scripts/08_run_rqr_dlm_bounded_validation.R"
      "$mode"
    )
    ;;
  long-running)
    runner_command=(bash -c 'sleep 60')
    ;;
  monitor-error)
    runner_command=(bash -c 'sleep 60')
    ;;
  leader-exits-first)
    runner_command=(bash -c 'trap "" HUP; sleep 60 & exit 17')
    ;;
  term-resistant-child)
    runner_command=(
      bash -c 'trap "" HUP TERM; sleep 60 & wait'
    )
    ;;
  zero-sample-startup)
    runner_command=(bash -c 'exit 19')
    ;;
  *)
    echo "Unknown RQR_MONITOR_TEST_SCENARIO." >&2
    exit 2
    ;;
esac

start_epoch="$(date +%s)"
setsid "${runner_command[@]}" >"$stdout_log" 2>"$stderr_log" &
root_pid=$!
pgid=$root_pid

# The background child can exist briefly before setsid creates its group.
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

sample_count=0
while group_exists "$pgid"; do
  now_epoch="$(date +%s)"
  elapsed=$((now_epoch - start_epoch))
  if (( elapsed > timeout_seconds )); then
    timed_out=TRUE
    drain_group "$pgid" || finalizer_error=TRUE
    break
  fi
  if [[ "$test_scenario" == monitor-error && "$sample_count" -ge 1 ]]; then
    monitor_error=TRUE
    drain_group "$pgid" || finalizer_error=TRUE
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
    drain_group "$pgid" || finalizer_error=TRUE
    break
  fi
  echo "$elapsed,$metrics" >>"$monitor_csv"
  sample_count=$((sample_count + 1))
  IFS=, read -r current_processes current_threads current_rss_kib <<<"$metrics"
  if (( current_processes > max_processes ||
        current_threads > max_threads ||
        current_rss_kib > max_rss_kib )); then
    sampled_limit_triggered=TRUE
    drain_group "$pgid" || finalizer_error=TRUE
    break
  fi
  if ! kill -0 "$root_pid" 2>/dev/null; then
    set +e
    wait "$root_pid"
    runner_status=$?
    set -e
    runner_waited=TRUE
    if group_exists "$pgid"; then
      drain_group "$pgid" || finalizer_error=TRUE
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
  drain_group "$pgid" || finalizer_error=TRUE
fi
if group_exists "$pgid"; then
  final_pgid_empty=FALSE
  finalizer_error=TRUE
fi

if [[ "$runner_status" -ne 0 ||
      "$timed_out" == TRUE ||
      "$sampled_limit_triggered" == TRUE ||
      "$monitor_error" == TRUE ||
      "$finalizer_error" == TRUE ||
      "$final_pgid_empty" != TRUE ]]; then
  exit 1
fi
exit 0
