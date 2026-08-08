#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 3 ]]; then
  cat >&2 <<'EOF'
Usage: 56_launch_rqr_dlm_multicomponent_recovery.sh \
  <primary-attestation.rds> <fresh-output-root> <workers>
EOF
  exit 64
fi

repo_root="$(git rev-parse --show-toplevel)"
primary_attestation="$(realpath "$1")"
output_root="$2"
workers="$3"
if [[ "$output_root" != /* ]]; then
  output_root="$repo_root/$output_root"
fi
output_root="$(realpath -m "$output_root")"
if [[ -e "$output_root" || -e "${output_root}_preflight" ]]; then
  echo "Recovery output roots must be fresh." >&2
  exit 64
fi
control_root="$repo_root/application/logs/$(basename "$output_root")_control"
if [[ -e "$control_root" ]]; then
  echo "Recovery control root must be fresh." >&2
  exit 64
fi
mkdir -p "$control_root"

orchestrator="$repo_root/application/scripts/56_orchestrate_rqr_dlm_multicomponent_recovery.sh"
nohup setsid bash "$orchestrator" "$primary_attestation" \
  "$output_root" "$workers" "$control_root" \
  >"$control_root/coordinator.stdout.log" \
  2>"$control_root/coordinator.stderr.log" </dev/null &
pid=$!
printf '%s\n' "$pid" >"$control_root/coordinator.pid"
printf '%s\n' "$pid" >"$control_root/coordinator.pgid"
printf '%s\n' "$output_root" >"$control_root/output_root.txt"
printf '%s\n' "$(git -C "$repo_root" rev-parse HEAD)" \
  >"$control_root/source_commit.txt"

sleep 1
if ! kill -0 "$pid" 2>/dev/null; then
  echo "The recovery coordinator exited during startup." >&2
  tail -40 "$control_root/coordinator.stderr.log" >&2 || true
  exit 1
fi
printf 'Multicomponent recovery launched in the background.\n'
printf '  PID/PGID:    %s\n' "$pid"
printf '  output root: %s\n' "$output_root"
printf '  control:     %s\n' "$control_root"
