#!/usr/bin/env bash
set -euo pipefail

repo_root="$(pwd -P)"
wrapper="$repo_root/application/scripts/08_run_rqr_dlm_bounded_validation.sh"
if [[ ! -x "$wrapper" || ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this monitor test from the RQR-GIBBS repository root." >&2
  exit 2
fi
expected_commit="$(git -C "$repo_root" rev-parse HEAD)"
test_root="$(mktemp -d "$repo_root/application/outputs/rqr_monitor_faults_XXXXXX")"

cleanup() {
  local saved_status=$?
  trap - EXIT INT TERM HUP
  if [[ -d "$test_root" &&
        "$test_root" == "$repo_root/application/outputs/rqr_monitor_faults_"* ]]; then
    rm -rf -- "$test_root"
  fi
  exit "$saved_status"
}
trap cleanup EXIT INT TERM HUP

pgid_live_count() {
  local pgid="$1"
  ps -eo pgid=,stat= | awk -v group="$pgid" '
    $1 == group && $2 !~ /^Z/ { count += 1 }
    END { print count + 0 }
  '
}

verify_artifacts() {
  local scenario="$1"
  local output_dir="$2"
  local expected_signal="$3"
  local required path sha bytes relative actual_sha actual_bytes pgid
  for required in \
    resource_summary.csv wrapper_closeout.csv artifact_hashes.csv \
    failure_log.csv monitor_fault_test.csv process_group_monitor.csv; do
    if [[ ! -f "$output_dir/$required" ]]; then
      echo "$scenario omitted $required." >&2
      return 1
    fi
  done
  if [[ "$(awk -F, 'NR > 1 { n += 1 } END { print n + 0 }' \
      "$output_dir/failure_log.csv")" -lt 1 ]]; then
    echo "$scenario did not record a structured failure." >&2
    return 1
  fi
  if ! awk -F, -v expected="$expected_signal" '
      $1 == "signal_received" {
        found = 1
        if ($2 != expected) exit 1
      }
      END { if (!found) exit 1 }
    ' "$output_dir/wrapper_closeout.csv"; then
    echo "$scenario recorded the wrong signal." >&2
    return 1
  fi
  if ! awk -F, '
      $1 == "final_pgid_empty" {
        found = 1
        if ($2 != "TRUE") exit 1
      }
      END { if (!found) exit 1 }
    ' "$output_dir/wrapper_closeout.csv"; then
    echo "$scenario left its process group nonempty." >&2
    return 1
  fi
  pgid="$(awk -F, '$1 == "process_group_id" { print $2 }' \
    "$output_dir/wrapper_closeout.csv")"
  if [[ ! "$pgid" =~ ^[0-9]+$ || "$(pgid_live_count "$pgid")" -ne 0 ]]; then
    echo "$scenario retained live PGID members." >&2
    return 1
  fi
  while IFS=, read -r sha bytes relative; do
    [[ "$sha" == sha256 ]] && continue
    relative="${relative#\"}"
    relative="${relative%\"}"
    path="$output_dir/$relative"
    if [[ ! -f "$path" ]]; then
      echo "$scenario artifact manifest names a missing file." >&2
      return 1
    fi
    actual_sha="$(sha256sum "$path" | awk '{print $1}')"
    actual_bytes="$(stat -c '%s' "$path")"
    if [[ "$sha" != "$actual_sha" || "$bytes" != "$actual_bytes" ]]; then
      echo "$scenario artifact manifest failed rehashing." >&2
      return 1
    fi
  done <"$output_dir/artifact_hashes.csv"
}

run_failure_scenario() {
  local scenario="$1"
  local output_dir="$test_root/$scenario"
  local invocation_log="$test_root/${scenario}.invocation.log"
  mkdir -p "$output_dir"
  set +e
  env \
    RQR_EXPECTED_PRIMARY_COMMIT="$expected_commit" \
    RQR_DLM_OUTPUT_DIR="$output_dir" \
    RQR_MONITOR_TEST_SCENARIO="$scenario" \
    RQR_MONITOR_TEST_CONFIRM=I_CONFIRM_RQR_MONITOR_FAULT_TEST \
    "$wrapper" preflight >"$invocation_log" 2>&1
  local status=$?
  set -e
  if [[ "$status" -eq 0 ]]; then
    echo "$scenario unexpectedly succeeded." >&2
    return 1
  fi
  verify_artifacts "$scenario" "$output_dir" NONE
}

run_signal_scenario() {
  local scenario="$1"
  local output_dir="$test_root/$scenario"
  local invocation_log="$test_root/${scenario}.invocation.log"
  local wrapper_pid status ready
  mkdir -p "$output_dir"
  env \
    RQR_EXPECTED_PRIMARY_COMMIT="$expected_commit" \
    RQR_DLM_OUTPUT_DIR="$output_dir" \
    RQR_MONITOR_TEST_SCENARIO="$scenario" \
    RQR_MONITOR_TEST_CONFIRM=I_CONFIRM_RQR_MONITOR_FAULT_TEST \
    "$wrapper" preflight >"$invocation_log" 2>&1 &
  wrapper_pid=$!
  ready=FALSE
  for _ in {1..200}; do
    if [[ -f "$output_dir/process_group_monitor.csv" &&
          "$(wc -l <"$output_dir/process_group_monitor.csv")" -ge 2 ]]; then
      ready=TRUE
      break
    fi
    if ! kill -0 "$wrapper_pid" 2>/dev/null; then
      break
    fi
    sleep 0.1
  done
  if [[ "$ready" != TRUE ]]; then
    echo "$scenario did not reach monitor readiness." >&2
    return 1
  fi
  kill -TERM "$wrapper_pid"
  set +e
  wait "$wrapper_pid"
  status=$?
  set -e
  if [[ "$status" -ne 143 ]]; then
    echo "$scenario returned $status instead of 143." >&2
    return 1
  fi
  verify_artifacts "$scenario" "$output_dir" TERM
}

run_signal_scenario long-running
run_failure_scenario monitor-error
run_failure_scenario leader-exits-first
run_signal_scenario term-resistant-child
run_failure_scenario zero-sample-startup

echo "RQR-DLM monitor fault suite passed: 5/5 scenarios."
