#!/usr/bin/env bash
set -euo pipefail

# Blocking coordinator for the final fail-closed S10 guard and S05/S06
# affected-wave validation. Use the detached launcher below for background
# operation. No scientific execution flag or authorization bundle is used.

if [[ $# -ne 4 ]]; then
  cat >&2 <<'EOF'
Usage: 52_orchestrate_rqr_dlm_affected_wave_validation.sh \
  <primary-attestation.rds> <exdqlm-attestation.json> \
  <quantreg-attestation.json> <fresh-output-root>
EOF
  exit 64
fi

repo_root="$(git rev-parse --show-toplevel)"
primary_attestation="$(realpath "$1")"
exdqlm_attestation="$(realpath "$2")"
quantreg_attestation="$(realpath "$3")"
output_root="$4"
if [[ "$output_root" != /* ]]; then
  output_root="$repo_root/$output_root"
fi
output_root="$(realpath -m "$output_root")"
case "$output_root" in
  "$repo_root/application/cache/"*) ;;
  *)
    echo "Output root must be below application/cache/." >&2
    exit 64
    ;;
esac
if [[ -e "$output_root" ]]; then
  echo "Output root must be fresh: $output_root" >&2
  exit 64
fi

source_commit="$(git -C "$repo_root" rev-parse HEAD)"
if [[ -n "$(git -C "$repo_root" status --porcelain=v2 --untracked-files=all)" ]]; then
  echo "The affected-wave coordinator requires a clean source checkout." >&2
  exit 65
fi
if [[ "$(git -C "$repo_root" rev-parse --abbrev-ref HEAD)" != "main" ]]; then
  echo "The launch checkout must expose the reviewed commit as local main." >&2
  exit 65
fi

runtime_package_path="$({
  Rscript -e 'x <- readRDS(commandArgs(TRUE)[1]); cat(x$runtime_package_path)' \
    "$primary_attestation"
})"
runtime_library="$(dirname "$runtime_package_path")"
if [[ ! -d "$runtime_package_path" ]]; then
  echo "The attested primary runtime package is absent." >&2
  exit 65
fi

mkdir -p "$output_root/control"
control_root="$output_root/control"
coordinator_pid="$$"
coordinator_pgid="$(ps -o pgid= -p "$$" | tr -d '[:space:]')"
current_stage="initialization"
terminal_status_written=0

thread_env=(
  OMP_NUM_THREADS=1
  OPENBLAS_NUM_THREADS=1
  MKL_NUM_THREADS=1
  VECLIB_MAXIMUM_THREADS=1
  NUMEXPR_NUM_THREADS=1
  RCPP_PARALLEL_NUM_THREADS=1
)
existing_user_library="${R_LIBS_USER:-/home/jaguir26/R/x86_64-redhat-linux-gnu-library/4.5}"
common_env=(
  "${thread_env[@]}"
  "R_LIBS_USER=$runtime_library:$existing_user_library"
  "RQR_EXPECTED_PRIMARY_COMMIT=$source_commit"
  "RQR_PRIMARY_RUNTIME_ATTESTATION=$primary_attestation"
  "RQR_EXDQLM_CRAN_ATTESTATION=$exdqlm_attestation"
  "RQR_QUANTREG_CRAN_ATTESTATION=$quantreg_attestation"
  "RQR_CONFIRMATORY_MONITOR_ROOT=$control_root/monitor"
)

update_status() {
  local status="$1"
  local stage="$2"
  local exit_status="${3:-}"
  local temporary
  temporary="$(mktemp "$control_root/.coordinator_status.XXXXXX")"
  cat >"$temporary" <<EOF
field	value
schema_version	rqrgibbs_dlm_affected_wave_coordinator/1.1.0
source_commit	$source_commit
coordinator_pid	$coordinator_pid
coordinator_pgid	$coordinator_pgid
status	$status
stage	$stage
scientific_promotion	FALSE
confirmatory_launch_authorized	FALSE
updated_at_utc	$(date -u +%Y-%m-%dT%H:%M:%SZ)
EOF
  if [[ -n "$exit_status" ]]; then
    printf 'exit_status\t%s\n' "$exit_status" >>"$temporary"
  fi
  mv "$temporary" "$control_root/coordinator_status.tsv"
}

record_terminal_status() {
  local exit_status="$?"
  if (( terminal_status_written == 0 && exit_status != 0 )); then
    update_status failed "$current_stage" "$exit_status"
  fi
}
trap record_terminal_status EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP

update_status starting initialization
current_stage="s10_guard"
update_status running "$current_stage"
env "${common_env[@]}" Rscript \
  "$repo_root/application/scripts/51_validate_rqr_dlm_s10_transition_guard.R" \
  --output-root="$output_root/s10_guard" --workers=8 \
  >"$control_root/s10_guard.stdout.log" \
  2>"$control_root/s10_guard.stderr.log"

wrapper="$repo_root/application/scripts/15_run_rqr_dlm_confirmatory_simulation.sh"
current_stage="preflight"
update_status running "$current_stage"
env "${common_env[@]}" bash "$wrapper" preflight \
  "$output_root/preflight" \
  >"$control_root/preflight.launch.stdout.log" \
  2>"$control_root/preflight.launch.stderr.log"

current_stage="oracle_reference"
update_status running "$current_stage"
env "${common_env[@]}" bash "$wrapper" oracle-reference \
  "$output_root/oracle_reference" \
  >"$control_root/oracle_reference.launch.stdout.log" \
  2>"$control_root/oracle_reference.launch.stderr.log"

current_stage="task_preparation"
update_status running "$current_stage"
env "${common_env[@]}" Rscript \
  "$repo_root/application/scripts/51_prepare_rqr_dlm_affected_wave_validation.R" \
  --preflight-root="$output_root/preflight" \
  --output-root="$output_root/task_preparation" \
  >"$control_root/task_preparation.stdout.log" \
  2>"$control_root/task_preparation.stderr.log"

preflight="$output_root/preflight"
reference="$output_root/oracle_reference"
worker_common_env=(
  "${common_env[@]}"
  "RQR_DLM_DEVELOPMENT_GATE_CONFIRM=TRUE"
  "RQR_CONFIRMATORY_CANONICAL_TASK_PLAN=$preflight/replication_plan_maximum.csv"
  "RQR_CONFIRM_SEED_LEDGER=$preflight/seed_ledger_maximum.csv"
  "RQR_CONFIRMATORY_PREFLIGHT_ARTIFACT_HASHES=$preflight/artifact_hashes.csv"
  "RQR_CONFIRMATORY_REFERENCE_ARTIFACT_HASHES=$reference/artifact_hashes.csv"
  "RQR_CONFIRMATORY_REFERENCE_RUNTIME_BUNDLE=$reference/runtime_bundle.json"
  "RQR_CONFIRMATORY_REFERENCE_DEPENDENCY_MANIFEST=$reference/dependency_manifest.csv"
  "RQR_CONFIRMATORY_REFERENCE_TOOLCHAIN_MANIFEST=$reference/toolchain_manifest.csv"
  "RQR_MAX_PROCESS_WAVE_SECONDS=259200"
  "RQR_MAX_PROCESS_GROUP_RSS_KIB=1572864"
  "RQR_MAX_PROCESS_GROUP_THREADS=4"
)

current_stage="affected_wave_workers"
update_status running "$current_stage"
pids=()
for slot in $(seq 1 8); do
  task_file="$output_root/task_preparation/worker_$(printf '%02d' "$slot")_tasks.csv"
  worker_root="$output_root/worker_$(printf '%02d' "$slot")"
  (
    env "${worker_common_env[@]}" \
      "RQR_DLM_DEVELOPMENT_WORKER_SLOT=$slot" \
      "RQR_CONFIRM_TASK_FILE=$task_file" \
      bash "$wrapper" development-affected-wave "$worker_root"
  ) >"$control_root/worker_$(printf '%02d' "$slot").launch.stdout.log" \
    2>"$control_root/worker_$(printf '%02d' "$slot").launch.stderr.log" &
  pids+=("$!")
done

worker_failure=0
for pid in "${pids[@]}"; do
  if ! wait "$pid"; then
    worker_failure=1
  fi
done
if (( worker_failure != 0 )); then
  terminal_status_written=1
  update_status failed "$current_stage" 1
  exit 1
fi

current_stage="compact_closeout"
update_status running "$current_stage"
env "${common_env[@]}" Rscript \
  "$repo_root/application/scripts/52_closeout_rqr_dlm_affected_wave_validation.R" \
  "$output_root" \
  >"$control_root/closeout.stdout.log" \
  2>"$control_root/closeout.stderr.log"

current_stage="complete"
update_status passed "$current_stage" 0
terminal_status_written=1
printf 'Affected-wave validation completed: %s\n' "$output_root"
