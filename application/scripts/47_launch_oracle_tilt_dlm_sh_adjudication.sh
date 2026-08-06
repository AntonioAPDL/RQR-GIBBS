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
if [[ ! "$source_commit" =~ ^[0-9a-f]{40}$ ]] ||
   [[ -n "$(git status --porcelain --untracked-files=all)" ]]; then
  echo "The adjudication launcher requires a clean committed worktree." >&2
  exit 2
fi
if [[ "${RQR_EXPECTED_PRIMARY_COMMIT:-}" != "$source_commit" ]]; then
  echo "RQR_EXPECTED_PRIMARY_COMMIT must equal the executing HEAD." >&2
  exit 2
fi
if [[ -z "${RQR_PRIMARY_RUNTIME_ATTESTATION:-}" ||
      -z "${RQR_ORACLE_TILT_V3_BASELINE_DIR:-}" ]]; then
  echo "The runtime attestation and immutable baseline directory are required." >&2
  exit 2
fi
if [[ "${RQR_ORACLE_TILT_DLM_SH_ADJUDICATION_CONFIRM:-}" != YES ]]; then
  echo "Set RQR_ORACLE_TILT_DLM_SH_ADJUDICATION_CONFIRM=YES after review." >&2
  exit 2
fi

short_sha="${source_commit:0:12}"
unit="rqrgibbs-dlm-sh-adjudication-${short_sha}"
output_dir="${RQR_ORACLE_TILT_DLM_SH_ADJUDICATION_OUTPUT_DIR:-$repo_root/application/outputs/oracle_tilt_dlm_sh_adjudication/exact_${short_sha}/execute}"
if systemctl --user is-active --quiet "$unit.service"; then
  echo "The adjudication service is already active: $unit" >&2
  exit 2
fi

log_root="$repo_root/application/logs/oracle_tilt_dlm_sh_adjudication"
mkdir -p "$log_root"
receipt="$log_root/${unit}_launch.csv"
printf '%s\n' \
  "launched_at_utc,unit,source_commit,output_dir,baseline_dir,runtime_attestation" \
  >"$receipt"
printf '%s,%s,%s,%s,%s,%s\n' \
  "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$unit" "$source_commit" \
  "$output_dir" "$RQR_ORACLE_TILT_V3_BASELINE_DIR" \
  "$RQR_PRIMARY_RUNTIME_ATTESTATION" >>"$receipt"

systemd-run --user --unit="$unit" --collect \
  --property="WorkingDirectory=$repo_root" \
  --property=RuntimeMaxSec=7h \
  --property=MemoryHigh=12G \
  --property=MemoryMax=14G \
  --property=TasksMax=16 \
  --setenv="RQR_EXPECTED_PRIMARY_COMMIT=$source_commit" \
  --setenv="RQR_PRIMARY_RUNTIME_ATTESTATION=$RQR_PRIMARY_RUNTIME_ATTESTATION" \
  --setenv="RQR_ORACLE_TILT_V3_BASELINE_DIR=$RQR_ORACLE_TILT_V3_BASELINE_DIR" \
  --setenv="RQR_ORACLE_TILT_DLM_SH_ADJUDICATION_CONFIRM=YES" \
  --setenv="RQR_ORACLE_TILT_DLM_SH_ADJUDICATION_OUTPUT_DIR=$output_dir" \
  "$repo_root/application/scripts/43_run_oracle_tilt_publication_v3.sh" adjudication

echo "[dlm-sh-adjudication-launch] unit=$unit"
echo "[dlm-sh-adjudication-launch] output=$output_dir"
echo "[dlm-sh-adjudication-launch] health: application/scripts/47_oracle_tilt_dlm_sh_adjudication_health.sh --unit=$unit --output-dir=$output_dir"
