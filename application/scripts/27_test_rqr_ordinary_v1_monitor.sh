#!/usr/bin/env bash
set -euo pipefail

# Bounded shell-only fault fixtures for the ordinary-v1 process-group wrapper.
# No R validator, package build, or MCMC process is started by this script.

repo_root="$(pwd -P)"
wrapper="$repo_root/application/scripts/26_run_rqr_ordinary_v1_validation.sh"
if [[ ! -x "$wrapper" || ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this test from the RQR-GIBBS repository root." >&2
  exit 2
fi
for command_name in \
    awk cmp find git grep kill mktemp ps rm sed sha256sum sleep sort stat; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "$command_name is required by the monitor fault test." >&2
    exit 2
  fi
done

expected_commit="$(git -C "$repo_root" rev-parse HEAD)"
test_root="$(mktemp -d \
  "$repo_root/application/cache/ordinary_v1_monitor_faults.XXXXXX")"
sleep 60 &
sentinel_pid=$!

cleanup() {
  local saved_status=$?
  trap - EXIT INT TERM HUP
  if kill -0 "$sentinel_pid" 2>/dev/null; then
    kill -TERM "$sentinel_pid" 2>/dev/null || true
  fi
  wait "$sentinel_pid" 2>/dev/null || true
  if [[ -d "$test_root" &&
        "$test_root" == "$repo_root/application/cache/ordinary_v1_monitor_faults."* ]]; then
    rm -rf -- "$test_root"
  fi
  exit "$saved_status"
}
trap cleanup EXIT INT TERM HUP

pgid_live_count() {
  local group_id="$1"
  ps -eo pgid=,stat= | awk -v group="$group_id" '
    $1 == group && $2 !~ /^Z/ { count += 1 }
    END { print count + 0 }
  '
}

csv_field() {
  local path="$1"
  local field="$2"
  awk -F, -v key="$field" '$1 == key { print $2; exit }' "$path"
}

verify_wrapper_manifest() {
  local scenario="$1"
  local output_dir="$2"
  local monitor_dir="$3"
  local manifest="$monitor_dir/wrapper_artifact_hashes.csv"
  local expected_set actual_set header
  local role relative bytes digest_value actual_path actual_bytes actual_digest
  if [[ ! -f "$manifest" || -L "$manifest" ]]; then
    echo "$scenario omitted wrapper_artifact_hashes.csv." >&2
    return 1
  fi
  expected_set="$(mktemp "$test_root/${scenario}.expected_manifest.XXXXXX")"
  actual_set="$(mktemp "$test_root/${scenario}.actual_manifest.XXXXXX")"
  printf '%s\n' \
    monitor_evidence,monitor/process_group_monitor.csv \
    monitor_evidence,monitor/process_group_resource_summary.csv \
    monitor_evidence,monitor/runner.stderr.log \
    monitor_evidence,monitor/runner.stdout.log \
    monitor_evidence,monitor/wrapper_closeout.csv \
    >"$expected_set"
  { find "$output_dir" -mindepth 1 -type f \
      -printf 'r_evidence_binding,output/%P\n' 2>/dev/null || true; } |
    LC_ALL=C sort >>"$expected_set"
  LC_ALL=C sort -o "$expected_set" "$expected_set"
  awk -F, 'NR > 1 { print $1 "," $2 }' "$manifest" |
    LC_ALL=C sort >"$actual_set"
  if ! cmp -s "$expected_set" "$actual_set"; then
    echo "$scenario wrapper manifest does not match the exact bound file set." >&2
    return 1
  fi
  {
    IFS= read -r header || return 1
    if [[ "$header" != "role,path,bytes,sha256" ]]; then
      echo "$scenario wrapper manifest has the wrong header." >&2
      return 1
    fi
    while IFS=, read -r role relative bytes digest_value; do
      case "$role:$relative" in
        monitor_evidence:monitor/*)
          actual_path="$monitor_dir/${relative#monitor/}"
          ;;
        r_evidence_binding:output/*)
          actual_path="$output_dir/${relative#output/}"
          ;;
        *)
          echo "$scenario wrapper manifest has an invalid role/path." >&2
          return 1
          ;;
      esac
      if [[ ! -f "$actual_path" || -L "$actual_path" ||
            ! "$bytes" =~ ^[0-9]+$ ||
            ! "$digest_value" =~ ^[0-9a-f]{64}$ ]]; then
        echo "$scenario wrapper manifest has an invalid row." >&2
        return 1
      fi
      actual_bytes="$(stat -c '%s' "$actual_path")"
      actual_digest="$(sha256sum "$actual_path" | awk '{ print $1 }')"
      if [[ "$bytes" != "$actual_bytes" ||
            "$digest_value" != "$actual_digest" ]]; then
        echo "$scenario wrapper manifest failed byte/hash readback." >&2
        return 1
      fi
    done
  } <"$manifest"
}

verify_common_evidence() {
  local scenario="$1"
  local output_dir="$2"
  local monitor_dir="$3"
  local expected_pass="$4"
  local closeout="$monitor_dir/wrapper_closeout.csv"
  local resource="$monitor_dir/process_group_resource_summary.csv"
  local telemetry="$monitor_dir/process_group_monitor.csv"
  local pgid
  for required in \
      "$closeout" "$resource" "$telemetry" \
      "$monitor_dir/runner.stdout.log" "$monitor_dir/runner.stderr.log" \
      "$monitor_dir/wrapper_artifact_hashes.csv"; do
    if [[ ! -f "$required" || -L "$required" ]]; then
      echo "$scenario omitted regular monitor evidence: $required" >&2
      return 1
    fi
  done
  if [[ "$(csv_field "$closeout" resource_pass)" != "$expected_pass" ]]; then
    echo "$scenario recorded the wrong resource_pass." >&2
    return 1
  fi
  if [[ "$(csv_field "$closeout" final_pgid_empty)" != TRUE ]]; then
    echo "$scenario did not complete its final PGID sweep." >&2
    return 1
  fi
  pgid="$(csv_field "$closeout" process_group_id)"
  if [[ ! "$pgid" =~ ^[1-9][0-9]*$ ||
        "$(pgid_live_count "$pgid")" -ne 0 ]]; then
    echo "$scenario retained a live process-group member." >&2
    return 1
  fi
  if [[ "$scenario" != monitor-temporary-lookalike ]] &&
      find "$monitor_dir" -maxdepth 1 -type f \
      \( -name '.resource_summary.*' -o -name '.wrapper_closeout.*' \
         -o -name '.wrapper_artifact_hashes.*' \
         -o -name '.wrapper_manifest_paths.*' \) |
      grep -q .; then
    echo "$scenario left an unpublished atomic-summary temporary file." >&2
    return 1
  fi
  if ! kill -0 "$sentinel_pid" 2>/dev/null; then
    echo "$scenario affected an unrelated process group." >&2
    return 1
  fi
  verify_wrapper_manifest "$scenario" "$output_dir" "$monitor_dir"
}

run_scenario() {
  local scenario="$1"
  local expected_zero="$2"
  local output_dir="$test_root/$scenario/output"
  local run_dir="$test_root/$scenario/run"
  local monitor_dir="$test_root/$scenario/monitor"
  local invocation_log="$test_root/$scenario.invocation.log"
  local artifact_limit=1048576
  local max_processes=4
  local max_threads=8
  local timeout=2
  local status
  if [[ "$scenario" == timeout-cleanup ]]; then
    timeout=1
  fi
  if [[ "$scenario" == artifact-limit ]]; then
    artifact_limit=1024
  fi
  if [[ "$scenario" == process-limit ]]; then
    max_processes=2
    max_threads=64
  fi
  if [[ "$scenario" == thread-limit ]]; then
    max_processes=64
    max_threads=1
  fi
  set +e
  env \
    RQR_EXPECTED_PRIMARY_COMMIT="$expected_commit" \
    RQR_ORDINARY_V1_OUTPUT_DIR="$output_dir" \
    RQR_ORDINARY_V1_RUN_DIR="$run_dir" \
    RQR_ORDINARY_V1_MONITOR_DIR="$monitor_dir" \
    RQR_ORDINARY_V1_MONITOR_TEST_SCENARIO="$scenario" \
    RQR_ORDINARY_V1_MONITOR_TEST_CONFIRM=I_CONFIRM_ORDINARY_V1_MONITOR_TEST \
    RQR_ORDINARY_V1_TEST_TIMEOUT_SECONDS="$timeout" \
    RQR_ORDINARY_V1_TEST_MAX_PROCESSES="$max_processes" \
    RQR_ORDINARY_V1_TEST_MAX_THREADS="$max_threads" \
    RQR_ORDINARY_V1_TEST_MAX_ARTIFACT_BYTES="$artifact_limit" \
    "$wrapper" preflight >"$invocation_log" 2>&1
  status=$?
  set -e

  if [[ "$expected_zero" == TRUE && "$status" -ne 0 ]]; then
    echo "$scenario failed with status $status." >&2
    sed -n '1,120p' "$invocation_log" >&2
    return 1
  fi
  if [[ "$expected_zero" == FALSE && "$status" -eq 0 ]]; then
    echo "$scenario unexpectedly succeeded." >&2
    return 1
  fi
  verify_common_evidence \
    "$scenario" "$output_dir" "$monitor_dir" \
    "$([[ "$expected_zero" == TRUE ]] && echo TRUE || echo FALSE)"

  case "$scenario" in
    normal-exit)
      if [[ "$(csv_field "$monitor_dir/wrapper_closeout.csv" \
          runner_exit_status)" -ne 0 ]]; then
        echo "normal-exit did not preserve a zero runner status." >&2
        return 1
      fi
      ;;
    child-failure)
      if [[ "$(csv_field "$monitor_dir/wrapper_closeout.csv" \
          runner_exit_status)" -ne 17 ]]; then
        echo "child-failure did not preserve status 17." >&2
        return 1
      fi
      ;;
    timeout-cleanup)
      if [[ "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          hard_wall_timeout_triggered)" != TRUE ]]; then
        echo "timeout-cleanup did not trip the wall timeout." >&2
        return 1
      fi
      if [[ "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          kill_escalation_used)" != TRUE ]]; then
        echo "timeout-cleanup did not exercise TERM-then-KILL cleanup." >&2
        return 1
      fi
      ;;
    artifact-limit)
      if [[ "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          artifact_limit_triggered)" != TRUE ]]; then
        echo "artifact-limit did not trip the cumulative byte gate." >&2
        return 1
      fi
      ;;
    process-limit)
      if [[ "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          process_limit_triggered)" != TRUE ||
            "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          thread_limit_triggered)" != FALSE ]]; then
        echo "process-limit did not isolate the process-count gate." >&2
        return 1
      fi
      ;;
    thread-limit)
      if [[ "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          thread_limit_triggered)" != TRUE ||
            "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          process_limit_triggered)" != FALSE ]]; then
        echo "thread-limit did not isolate the aggregate-NLWP gate." >&2
        return 1
      fi
      ;;
    monitor-error)
      if [[ "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          monitor_error)" != TRUE ]]; then
        echo "monitor-error did not fail closed on telemetry loss." >&2
        return 1
      fi
      ;;
    monitor-hidden)
      if [[ "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          monitor_error)" != TRUE ||
            ! -f "$monitor_dir/.unexpected-monitor" ]]; then
        echo "monitor-hidden did not reject and preserve the unexpected entry." >&2
        return 1
      fi
      if grep -q 'monitor/\\.unexpected-monitor' \
          "$monitor_dir/wrapper_artifact_hashes.csv"; then
        echo "monitor-hidden incorrectly bound an undeclared monitor artifact." >&2
        return 1
      fi
      ;;
    monitor-temporary-lookalike)
      if [[ "$(csv_field "$monitor_dir/process_group_resource_summary.csv" \
          monitor_error)" != TRUE ||
            ! -f "$monitor_dir/.wrapper_artifact_hashes.injected" ]]; then
        echo "monitor-temporary-lookalike did not reject the injected entry." >&2
        return 1
      fi
      if grep -q 'monitor/\\.wrapper_artifact_hashes\\.injected' \
          "$monitor_dir/wrapper_artifact_hashes.csv"; then
        echo "monitor-temporary-lookalike bound an undeclared entry." >&2
        return 1
      fi
      ;;
  esac
}

run_signal_scenario() {
  local scenario=signal-cleanup
  local output_dir="$test_root/$scenario/output"
  local run_dir="$test_root/$scenario/run"
  local monitor_dir="$test_root/$scenario/monitor"
  local invocation_log="$test_root/$scenario.invocation.log"
  local wrapper_pid status observed=FALSE
  local telemetry="$monitor_dir/process_group_monitor.csv"

  env \
    RQR_EXPECTED_PRIMARY_COMMIT="$expected_commit" \
    RQR_ORDINARY_V1_OUTPUT_DIR="$output_dir" \
    RQR_ORDINARY_V1_RUN_DIR="$run_dir" \
    RQR_ORDINARY_V1_MONITOR_DIR="$monitor_dir" \
    RQR_ORDINARY_V1_MONITOR_TEST_SCENARIO="$scenario" \
    RQR_ORDINARY_V1_MONITOR_TEST_CONFIRM=I_CONFIRM_ORDINARY_V1_MONITOR_TEST \
    RQR_ORDINARY_V1_TEST_TIMEOUT_SECONDS=10 \
    RQR_ORDINARY_V1_TEST_MAX_PROCESSES=4 \
    RQR_ORDINARY_V1_TEST_MAX_THREADS=8 \
    RQR_ORDINARY_V1_TEST_MAX_ARTIFACT_BYTES=1048576 \
    "$wrapper" preflight >"$invocation_log" 2>&1 &
  wrapper_pid=$!

  for _ in {1..200}; do
    if [[ -f "$telemetry" &&
          "$(awk 'END { print NR + 0 }' "$telemetry")" -ge 2 ]]; then
      observed=TRUE
      break
    fi
    if ! kill -0 "$wrapper_pid" 2>/dev/null; then
      break
    fi
    sleep 0.02
  done
  if [[ "$observed" != TRUE ]]; then
    kill -TERM "$wrapper_pid" 2>/dev/null || true
    wait "$wrapper_pid" 2>/dev/null || true
    echo "signal-cleanup never reached an observed live process group." >&2
    sed -n '1,120p' "$invocation_log" >&2
    return 1
  fi

  kill -TERM "$wrapper_pid"
  set +e
  wait "$wrapper_pid"
  status=$?
  set -e
  if [[ "$status" -ne 143 ]]; then
    echo "signal-cleanup returned $status instead of 143." >&2
    sed -n '1,120p' "$invocation_log" >&2
    return 1
  fi
  verify_common_evidence "$scenario" "$output_dir" "$monitor_dir" FALSE
  if [[ "$(csv_field "$monitor_dir/wrapper_closeout.csv" \
      signal_received)" != TERM ]]; then
    echo "signal-cleanup did not record the TERM signal." >&2
    return 1
  fi
}

run_scenario normal-exit TRUE
run_scenario child-failure FALSE
run_scenario timeout-cleanup FALSE
run_scenario artifact-limit FALSE
run_scenario process-limit FALSE
run_scenario thread-limit FALSE
run_scenario monitor-error FALSE
run_scenario monitor-hidden FALSE
run_scenario monitor-temporary-lookalike FALSE
run_signal_scenario

echo "ordinary-v1 monitor fault suite passed: 10/10 scenarios."
