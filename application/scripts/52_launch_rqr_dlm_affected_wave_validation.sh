#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 4 ]]; then
  cat >&2 <<'EOF'
Usage: 52_launch_rqr_dlm_affected_wave_validation.sh \
  <primary-attestation.rds> <exdqlm-attestation.json> \
  <quantreg-attestation.json> <fresh-output-root>
EOF
  exit 64
fi

repo_root="$(git rev-parse --show-toplevel)"
output_root="$4"
if [[ "$output_root" != /* ]]; then
  output_root="$repo_root/$output_root"
fi
output_root="$(realpath -m "$output_root")"
if [[ -e "$output_root" ]]; then
  echo "Output root must be fresh: $output_root" >&2
  exit 64
fi

control_root="$repo_root/application/logs/$(basename "$output_root")_launch"
if [[ -e "$control_root" ]]; then
  echo "Launch control root must be fresh: $control_root" >&2
  exit 64
fi
mkdir -p "$control_root"

orchestrator="$repo_root/application/scripts/52_orchestrate_rqr_dlm_affected_wave_validation.sh"
nohup setsid bash "$orchestrator" "$1" "$2" "$3" "$output_root" \
  >"$control_root/coordinator.stdout.log" \
  2>"$control_root/coordinator.stderr.log" </dev/null &
pid=$!
printf '%s\n' "$pid" >"$control_root/coordinator.pid"
printf '%s\n' "$output_root" >"$control_root/output_root.txt"
printf '%s\n' "$(git -C "$repo_root" rev-parse HEAD)" \
  >"$control_root/source_commit.txt"

sleep 1
if ! kill -0 "$pid" 2>/dev/null; then
  echo "The affected-wave coordinator exited during startup." >&2
  tail -40 "$control_root/coordinator.stderr.log" >&2 || true
  exit 1
fi
printf 'Affected-wave validation launched in the background.\n'
printf '  PID/PGID:    %s\n' "$pid"
printf '  output root: %s\n' "$output_root"
printf '  control:     %s\n' "$control_root"
