#!/usr/bin/env bash
set -euo pipefail

repo_root="$(pwd -P)"
config="$repo_root/application/config/oracle_tilt_c095_publication_v4_seed_screen_20260805.json"
if [[ ! -f "$repo_root/application/DESCRIPTION" || ! -f "$config" ]]; then
  echo "Run the V4 launcher from the RQR-GIBBS repository root." >&2
  exit 2
fi
for command_name in git systemd-run systemctl Rscript sha256sum; do
  command -v "$command_name" >/dev/null 2>&1 || {
    echo "$command_name is required." >&2; exit 2;
  }
done

# The tracked source is intentionally false until an independent review
# authorizes a flag-only commit.  This check makes a production launch from the
# implementation commit impossible even when environment confirmations leak.
Rscript --vanilla - "$config" <<'RSCRIPT'
config <- jsonlite::read_json(commandArgs(TRUE)[1L], simplifyVector = FALSE)
if (!isTRUE(config$execution_authorized)) {
  message("The tracked V4 config is not execution-authorized.")
  quit(status = 65L)
}
RSCRIPT

source_commit="$(git rev-parse HEAD)"
if [[ ! "$source_commit" =~ ^[0-9a-f]{40}$ ]] ||
   [[ -n "$(git status --porcelain --untracked-files=all)" ]] ||
   [[ "${RQR_EXPECTED_PRIMARY_COMMIT:-}" != "$source_commit" ]]; then
  echo "V4 launch requires a clean checkout at the reviewed full SHA." >&2
  exit 2
fi
required_variables=(
  RQR_PRIMARY_RUNTIME_ATTESTATION
  RQR_ORACLE_TILT_V4_PREFLIGHT_DIR
  RQR_ORACLE_TILT_V4_REFERENCE_DIR
  RQR_ORACLE_TILT_V4_BENCHMARK_DIR
  RQR_ORACLE_TILT_V4_RESOURCE_DIR
)
for variable in "${required_variables[@]}"; do
  [[ -n "${!variable:-}" ]] || {
    echo "$variable is required." >&2; exit 2;
  }
done
if [[ "${RQR_ORACLE_TILT_V4_CONFIRM:-}" != YES ]]; then
  echo "Set RQR_ORACLE_TILT_V4_CONFIRM=YES after independent source review." >&2
  exit 2
fi

short_sha="${source_commit:0:12}"
unit="rqrgibbs-oracle-v4-${short_sha}"
output_dir="${RQR_ORACLE_TILT_V4_OUTPUT_DIR:-$repo_root/application/outputs/oracle_tilt_c095_publication_v4_seed_screen/exact_${short_sha}/execute}"
if systemctl --user is-active --quiet "$unit.service"; then
  echo "The V4 service is already active: $unit" >&2; exit 2
fi

log_root="$repo_root/application/logs/oracle_tilt_c095_publication_v4_seed_screen"
mkdir -p "$log_root"
receipt="$log_root/${unit}_launch.csv"
printf '%s\n' \
  "launched_at_utc,unit,source_commit,config_sha256,output_dir,runtime_attestation,preflight_dir,reference_dir,benchmark_dir,resource_dir" \
  >"$receipt"
printf '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' \
  "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$unit" "$source_commit" \
  "$(sha256sum "$config" | awk '{print $1}')" "$output_dir" \
  "$RQR_PRIMARY_RUNTIME_ATTESTATION" "$RQR_ORACLE_TILT_V4_PREFLIGHT_DIR" \
  "$RQR_ORACLE_TILT_V4_REFERENCE_DIR" "$RQR_ORACLE_TILT_V4_BENCHMARK_DIR" \
  "$RQR_ORACLE_TILT_V4_RESOURCE_DIR" >>"$receipt"

systemd-run --user --unit="$unit" --collect \
  --property="WorkingDirectory=$repo_root" \
  --property=RuntimeMaxSec=12h \
  --property=MemoryHigh=92G \
  --property=MemoryMax=96G \
  --property=TasksMax=72 \
  --setenv="RQR_EXPECTED_PRIMARY_COMMIT=$source_commit" \
  --setenv="RQR_PRIMARY_RUNTIME_ATTESTATION=$RQR_PRIMARY_RUNTIME_ATTESTATION" \
  --setenv="RQR_ORACLE_TILT_V4_PREFLIGHT_DIR=$RQR_ORACLE_TILT_V4_PREFLIGHT_DIR" \
  --setenv="RQR_ORACLE_TILT_V4_REFERENCE_DIR=$RQR_ORACLE_TILT_V4_REFERENCE_DIR" \
  --setenv="RQR_ORACLE_TILT_V4_BENCHMARK_DIR=$RQR_ORACLE_TILT_V4_BENCHMARK_DIR" \
  --setenv="RQR_ORACLE_TILT_V4_RESOURCE_DIR=$RQR_ORACLE_TILT_V4_RESOURCE_DIR" \
  --setenv="RQR_ORACLE_TILT_V4_CONFIRM=YES" \
  --setenv="RQR_ORACLE_TILT_V4_RESUME=${RQR_ORACLE_TILT_V4_RESUME:-NO}" \
  --setenv="RQR_ORACLE_TILT_V4_OUTPUT_DIR=$output_dir" \
  "$repo_root/application/scripts/53_run_oracle_tilt_publication_v4.sh" execute

echo "[oracle-tilt-v4-launch] unit=$unit"
echo "[oracle-tilt-v4-launch] output=$output_dir"
echo "[oracle-tilt-v4-launch] health: application/scripts/55_oracle_tilt_v4_health.sh --unit=$unit --output-dir=$output_dir"
