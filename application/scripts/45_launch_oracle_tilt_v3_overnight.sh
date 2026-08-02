#!/usr/bin/env bash
set -euo pipefail

repo_root="$(pwd -P)"
if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this launcher from the RQR-GIBBS repository root." >&2
  exit 2
fi
for command_name in git systemd-run systemctl sha256sum; do
  command -v "$command_name" >/dev/null 2>&1 || {
    echo "$command_name is required." >&2
    exit 2
  }
done

source_commit="$(git rev-parse HEAD)"
if [[ ! "$source_commit" =~ ^[0-9a-f]{40}$ ]]; then
  echo "Could not resolve the full source commit." >&2
  exit 2
fi
if [[ -n "$(git status --porcelain --untracked-files=all)" ]]; then
  echo "The overnight launcher requires a clean worktree." >&2
  exit 2
fi
if [[ "${RQR_EXPECTED_PRIMARY_COMMIT:-}" != "$source_commit" ]]; then
  echo "RQR_EXPECTED_PRIMARY_COMMIT must equal the executing HEAD." >&2
  exit 2
fi

required_variables=(
  RQR_PRIMARY_RUNTIME_ATTESTATION
  RQR_ORACLE_TILT_V3_PREFLIGHT_DIR
  RQR_ORACLE_TILT_V3_REFERENCE_DIR
  RQR_ORACLE_TILT_V3_BENCHMARK_DIR
)
for variable in "${required_variables[@]}"; do
  if [[ -z "${!variable:-}" ]]; then
    echo "$variable is required." >&2
    exit 2
  fi
done
if [[ "${RQR_ORACLE_TILT_V3_CONFIRM:-}" != YES ]]; then
  echo "Set RQR_ORACLE_TILT_V3_CONFIRM=YES after exact-source review." >&2
  exit 2
fi

short_sha="${source_commit:0:12}"
unit="rqrgibbs-oracle-v3-${short_sha}"
output_dir="${RQR_ORACLE_TILT_V3_OUTPUT_DIR:-$repo_root/application/outputs/oracle_tilt_c095_publication_v3/exact_${short_sha}/execute}"
if systemctl --user is-active --quiet "$unit.service"; then
  echo "The overnight service is already active: $unit" >&2
  exit 2
fi

log_root="$repo_root/application/logs/oracle_tilt_c095_publication_v3"
mkdir -p "$log_root"
receipt="$log_root/${unit}_launch.csv"
printf '%s\n' \
  "launched_at_utc,unit,source_commit,output_dir,runtime_attestation,preflight_dir,reference_dir,benchmark_dir" \
  >"$receipt"
printf '%s,%s,%s,%s,%s,%s,%s,%s\n' \
  "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$unit" "$source_commit" \
  "$output_dir" "$RQR_PRIMARY_RUNTIME_ATTESTATION" \
  "$RQR_ORACLE_TILT_V3_PREFLIGHT_DIR" "$RQR_ORACLE_TILT_V3_REFERENCE_DIR" \
  "$RQR_ORACLE_TILT_V3_BENCHMARK_DIR" >>"$receipt"

systemd-run --user --unit="$unit" --collect \
  --property="WorkingDirectory=$repo_root" \
  --property=RuntimeMaxSec=9h \
  --property=MemoryHigh=12G \
  --property=MemoryMax=14G \
  --property=TasksMax=16 \
  --setenv="RQR_EXPECTED_PRIMARY_COMMIT=$source_commit" \
  --setenv="RQR_PRIMARY_RUNTIME_ATTESTATION=$RQR_PRIMARY_RUNTIME_ATTESTATION" \
  --setenv="RQR_ORACLE_TILT_V3_PREFLIGHT_DIR=$RQR_ORACLE_TILT_V3_PREFLIGHT_DIR" \
  --setenv="RQR_ORACLE_TILT_V3_REFERENCE_DIR=$RQR_ORACLE_TILT_V3_REFERENCE_DIR" \
  --setenv="RQR_ORACLE_TILT_V3_BENCHMARK_DIR=$RQR_ORACLE_TILT_V3_BENCHMARK_DIR" \
  --setenv="RQR_ORACLE_TILT_V3_CONFIRM=YES" \
  --setenv="RQR_ORACLE_TILT_V3_OUTPUT_DIR=$output_dir" \
  "$repo_root/application/scripts/43_run_oracle_tilt_publication_v3.sh" execute

echo "[oracle-tilt-v3-launch] unit=$unit"
echo "[oracle-tilt-v3-launch] output=$output_dir"
echo "[oracle-tilt-v3-launch] health: application/scripts/45_oracle_tilt_v3_health.sh --unit=$unit --output-dir=$output_dir"
