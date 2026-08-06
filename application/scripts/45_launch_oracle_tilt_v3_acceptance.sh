#!/usr/bin/env bash
set -euo pipefail

repo_root="$(pwd -P)"
if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this launcher from the RQR-GIBBS repository root." >&2
  exit 2
fi
for command_name in git systemd-run systemctl Rscript; do
  command -v "$command_name" >/dev/null 2>&1 || {
    echo "$command_name is required." >&2
    exit 2
  }
done
Rscript application/scripts/49_oracle_tilt_campaign_gate.R \
  "--repo-root=$repo_root" \
  --campaign=publication_v3 \
  --action=acceptance

source_commit="$(git rev-parse HEAD)"
if [[ ! "$source_commit" =~ ^[0-9a-f]{40}$ ]] ||
   [[ -n "$(git status --porcelain --untracked-files=all)" ]] ||
   [[ "${RQR_EXPECTED_PRIMARY_COMMIT:-}" != "$source_commit" ]]; then
  echo "Acceptance requires a clean expected source commit." >&2
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
if [[ "${RQR_ORACLE_TILT_V3_ACCEPTANCE_CONFIRM:-}" != YES ]]; then
  echo "Set RQR_ORACLE_TILT_V3_ACCEPTANCE_CONFIRM=YES after review." >&2
  exit 2
fi

short_sha="${source_commit:0:12}"
unit="rqrgibbs-oracle-v3-acceptance-${short_sha}"
output_dir="${RQR_ORACLE_TILT_V3_OUTPUT_DIR:-$repo_root/application/outputs/oracle_tilt_c095_publication_v3/exact_${short_sha}/acceptance}"
if systemctl --user is-active --quiet "$unit.service"; then
  echo "The acceptance service is already active: $unit" >&2
  exit 2
fi

systemd-run --user --unit="$unit" --collect \
  --property="WorkingDirectory=$repo_root" \
  --property=RuntimeMaxSec=90m \
  --property=MemoryHigh=12G \
  --property=MemoryMax=14G \
  --property=TasksMax=16 \
  --setenv="RQR_EXPECTED_PRIMARY_COMMIT=$source_commit" \
  --setenv="RQR_PRIMARY_RUNTIME_ATTESTATION=$RQR_PRIMARY_RUNTIME_ATTESTATION" \
  --setenv="RQR_ORACLE_TILT_V3_PREFLIGHT_DIR=$RQR_ORACLE_TILT_V3_PREFLIGHT_DIR" \
  --setenv="RQR_ORACLE_TILT_V3_REFERENCE_DIR=$RQR_ORACLE_TILT_V3_REFERENCE_DIR" \
  --setenv="RQR_ORACLE_TILT_V3_BENCHMARK_DIR=$RQR_ORACLE_TILT_V3_BENCHMARK_DIR" \
  --setenv="RQR_ORACLE_TILT_V3_ACCEPTANCE_CONFIRM=YES" \
  --setenv="RQR_ORACLE_TILT_V3_OUTPUT_DIR=$output_dir" \
  "$repo_root/application/scripts/43_run_oracle_tilt_publication_v3.sh" acceptance

echo "[oracle-tilt-v3-acceptance-launch] unit=$unit"
echo "[oracle-tilt-v3-acceptance-launch] output=$output_dir"
