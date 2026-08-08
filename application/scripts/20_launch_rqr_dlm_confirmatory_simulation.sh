#!/usr/bin/env bash
set -euo pipefail

# Start or resume the complete confirmatory coordinator as a detached,
# inspectable process. The JSON input is produced by script 19 after the
# flag-only authorization commit and exact-runtime reference gates pass.

if [[ $# -ne 4 ]]; then
  echo "Usage: $0 <launch-inputs.json> <run-id> <run-root> <fresh-log-root>" >&2
  exit 64
fi

launch_inputs="$(realpath "$1")"
run_id="$2"
run_root="$(realpath -m "$3")"
log_root="$(realpath -m "$4")"
repo_root="$(pwd -P)"

if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this launcher from the RQR-GIBBS root." >&2
  exit 64
fi
if [[ ! "$run_id" =~ ^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$ ]]; then
  echo "The run ID is invalid." >&2
  exit 64
fi
if [[ -e "$log_root" ]]; then
  echo "The supervisor log root must be fresh." >&2
  exit 64
fi
for command in jq Rscript setsid sha256sum; do
  if ! command -v "$command" >/dev/null 2>&1; then
    echo "Missing required command: $command" >&2
    exit 69
  fi
done

read_json_path() {
  local field="$1"
  local value
  value="$(jq -er --arg field "$field" '.[$field] | strings | select(length > 0)' "$launch_inputs")"
  realpath "$value"
}

reviewed_commit="$(jq -er '.reviewed_implementation_commit' "$launch_inputs")"
authorization_commit="$(jq -er '.authorization_commit' "$launch_inputs")"
primary_attestation="$(read_json_path primary_runtime_attestation)"
preflight_directory="$(read_json_path preflight_directory)"
reference_directory="$(read_json_path reference_directory)"
exdqlm_attestation="$(read_json_path exdqlm_attestation)"
quantreg_attestation="$(read_json_path quantreg_attestation)"
authorization_bundle="$(read_json_path authorization_bundle)"
seed_ledger="$(read_json_path seed_ledger)"
canonical_task_plan="$(read_json_path canonical_task_plan)"
canonical_wave_plan="$(read_json_path canonical_wave_plan)"
authorization_schema="$(jq -er '.schema_version' "$authorization_bundle")"
diagnostic_aware_completion="FALSE"
if [[ "$authorization_schema" == \
      "rqrgibbs_dlm_diagnostic_aware_authorization/1.0.0" ]]; then
  diagnostic_aware_completion="TRUE"
  export RQR_DLM_EXECUTION_POLICY="diagnostic-aware-completion"
elif [[ "$authorization_schema" != \
        "rqrgibbs_dlm_confirmatory_authorization/1.0.0" ]]; then
  echo "The authorization bundle schema is unsupported." >&2
  exit 65
fi

if [[ ! "$reviewed_commit" =~ ^[0-9a-f]{40}$ ||
      ! "$authorization_commit" =~ ^[0-9a-f]{40}$ ]]; then
  echo "The launch-input commit identities are invalid." >&2
  exit 65
fi
head_commit="$(git -c core.fsmonitor=false rev-parse HEAD)"
if [[ "$head_commit" != "$authorization_commit" ||
      -n "$(git -c core.fsmonitor=false status --porcelain=v2 --untracked-files=all)" ]]; then
  echo "The launch checkout is not the exact clean authorization commit." >&2
  exit 65
fi

primary_runtime_path="$(
  Rscript -e '
    arguments <- commandArgs(trailingOnly = TRUE)
    attestation <- readRDS(arguments[[1L]])
    runtime_path <- attestation$runtime_package_path
    if (!is.character(runtime_path) ||
        length(runtime_path) != 1L ||
        is.na(runtime_path) ||
        !dir.exists(runtime_path)) {
      stop("The primary RDS attestation has no valid runtime path.",
           call. = FALSE)
    }
    cat(normalizePath(runtime_path, winslash = "/", mustWork = TRUE))
  ' "$primary_attestation"
)"
exdqlm_runtime_path="$(jq -er '.runtime_path' "$exdqlm_attestation")"
quantreg_runtime_path="$(jq -er '.runtime_path' "$quantreg_attestation")"
for runtime_path in \
  "$primary_runtime_path" "$exdqlm_runtime_path" "$quantreg_runtime_path"; do
  if [[ ! -d "$runtime_path" ]]; then
    echo "An attested runtime directory is missing: $runtime_path" >&2
    exit 66
  fi
done

mkdir -p "$run_root"
mkdir -p "$log_root"
environment_csv="$log_root/launch_environment.csv"
pid_path="$log_root/coordinator.pid"
stdout_path="$log_root/coordinator.stdout.log"
stderr_path="$log_root/coordinator.stderr.log"

export RQR_EXPECTED_PRIMARY_COMMIT="$authorization_commit"
export RQR_PRIMARY_RUNTIME_ATTESTATION="$primary_attestation"
export RQR_CONFIRMATORY_AUTHORIZATION_BUNDLE="$authorization_bundle"
export RQR_CONFIRMATORY_CANONICAL_TASK_PLAN="$canonical_task_plan"
export RQR_CONFIRM_SEED_LEDGER="$seed_ledger"
export RQR_EXDQLM_CRAN_ATTESTATION="$exdqlm_attestation"
export RQR_QUANTREG_CRAN_ATTESTATION="$quantreg_attestation"
export RQR_CONFIRMATORY_PREFLIGHT_ARTIFACT_HASHES="$preflight_directory/artifact_hashes.csv"
export RQR_CONFIRMATORY_REFERENCE_ARTIFACT_HASHES="$reference_directory/artifact_hashes.csv"
export RQR_CONFIRMATORY_REFERENCE_RUNTIME_BUNDLE="$reference_directory/runtime_bundle.json"
export RQR_CONFIRMATORY_REFERENCE_DEPENDENCY_MANIFEST="$reference_directory/comparator_dependency_manifest.csv"
export RQR_CONFIRMATORY_REFERENCE_TOOLCHAIN_MANIFEST="$reference_directory/toolchain_manifest.csv"
export RQR_CONFIRMATORY_RUN_ID="$run_id"
export R_LIBS="$(dirname "$primary_runtime_path"):$(dirname "$exdqlm_runtime_path"):$(dirname "$quantreg_runtime_path")${R_LIBS:+:$R_LIBS}"

for required in \
  "$RQR_CONFIRMATORY_PREFLIGHT_ARTIFACT_HASHES" \
  "$RQR_CONFIRMATORY_REFERENCE_ARTIFACT_HASHES" \
  "$RQR_CONFIRMATORY_REFERENCE_RUNTIME_BUNDLE" \
  "$RQR_CONFIRMATORY_REFERENCE_DEPENDENCY_MANIFEST" \
  "$RQR_CONFIRMATORY_REFERENCE_TOOLCHAIN_MANIFEST"; do
  if [[ ! -f "$required" ]]; then
    echo "A required launch evidence artifact is missing: $required" >&2
    exit 66
  fi
done

{
  echo "field,value"
  printf 'run_id,%s\n' "$run_id"
  printf 'reviewed_implementation_commit,%s\n' "$reviewed_commit"
  printf 'authorization_commit,%s\n' "$authorization_commit"
  printf 'launch_inputs_sha256,%s\n' "$(sha256sum "$launch_inputs" | awk '{print $1}')"
  printf 'authorization_bundle_sha256,%s\n' "$(sha256sum "$authorization_bundle" | awk '{print $1}')"
  printf 'canonical_wave_plan_sha256,%s\n' "$(sha256sum "$canonical_wave_plan" | awk '{print $1}')"
  printf 'diagnostic_aware_completion,%s\n' "$diagnostic_aware_completion"
  printf 'authorization_schema,%s\n' "$authorization_schema"
  printf 'run_root,%s\n' "$run_root"
} >"$environment_csv"

coordinator="$repo_root/application/scripts/18_orchestrate_rqr_dlm_confirmatory_simulation.R"
nohup setsid Rscript "$coordinator" "$canonical_wave_plan" "$run_root" \
  >"$stdout_path" 2>"$stderr_path" </dev/null &
coordinator_pid=$!
printf '%s\n' "$coordinator_pid" >"$pid_path"
sleep 1
if ! kill -0 "$coordinator_pid" 2>/dev/null; then
  echo "The coordinator terminated during startup." >&2
  tail -40 "$stderr_path" >&2 || true
  exit 70
fi

if [[ "$diagnostic_aware_completion" == "TRUE" ]]; then
  echo "Diagnostic-aware completion coordinator started."
else
  echo "Confirmatory coordinator started."
fi
echo "  PID: $coordinator_pid"
echo "  run root: $run_root"
echo "  stdout: $stdout_path"
echo "  stderr: $stderr_path"
