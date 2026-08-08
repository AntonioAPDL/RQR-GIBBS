#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 4 || $# -gt 5 ]]; then
  cat >&2 <<'EOF'
Usage: 48_launch_rqr_dlm_skewed_candidates.sh \
  <seed-ledger.csv> <exdqlm-attestation.json> <output-root> <workers> \
  [whole_scan|joint_elliptical]

The output root and its sibling control directory must not already exist.
The script runs the authenticated preflight synchronously, then starts the
development-only comparison in a detached process group.
EOF
  exit 64
fi

repo_root="$(git rev-parse --show-toplevel)"
seed_ledger="$(realpath "$1")"
exdqlm_attestation="$(realpath "$2")"
output_root="$3"
workers="$4"
candidate_family="${5:-whole_scan}"

case "$candidate_family" in
  whole_scan|joint_elliptical) ;;
  *)
    echo "candidate family must be whole_scan or joint_elliptical." >&2
    exit 64
    ;;
esac

case "$workers" in
  ''|*[!0-9]*)
    echo "workers must be an integer in [1, 12]." >&2
    exit 64
    ;;
esac
if (( workers < 1 || workers > 12 )); then
  echo "workers must be an integer in [1, 12]." >&2
  exit 64
fi

if [[ "$output_root" != /* ]]; then
  output_root="$repo_root/$output_root"
fi
output_root="$(realpath -m "$output_root")"
case "$output_root" in
  "$repo_root/application/cache/"*) ;;
  *)
    echo "output-root must be below application/cache/." >&2
    exit 64
    ;;
esac

run_label="$(basename "$output_root")"
control_root="$repo_root/application/logs/${run_label}_control"
if [[ -e "$output_root" || -e "$control_root" ]]; then
  echo "The output and control roots must be new." >&2
  exit 64
fi
mkdir -p "$control_root"

runner="$repo_root/application/scripts/47_compare_rqr_dlm_skewed_failure_candidates.R"
common=(
  "--seed-ledger=$seed_ledger"
  "--exdqlm-attestation=$exdqlm_attestation"
  "--output-root=$output_root"
  "--workers=$workers"
  "--candidate-family=$candidate_family"
)

thread_env=(
  OMP_NUM_THREADS=1
  OPENBLAS_NUM_THREADS=1
  MKL_NUM_THREADS=1
  BLIS_NUM_THREADS=1
  VECLIB_MAXIMUM_THREADS=1
  NUMEXPR_NUM_THREADS=1
)

(
  cd "$repo_root"
  env "${thread_env[@]}" Rscript "$runner" \
    --mode=preflight "${common[@]}"
) >"$control_root/preflight.stdout.log" \
  2>"$control_root/preflight.stderr.log"

source_commit="$(git -C "$repo_root" rev-parse HEAD)"
started_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
(
  cd "$repo_root"
  nohup setsid env "${thread_env[@]}" Rscript "$runner" \
    --mode=execute "${common[@]}" \
    >"$control_root/runner.stdout.log" \
    2>"$control_root/runner.stderr.log" </dev/null &
  runner_pid=$!
  printf '%s\n' "$runner_pid" >"$control_root/runner.pid"
  printf '%s\n' "$runner_pid" >"$control_root/runner.pgid"
)

runner_pid="$(cat "$control_root/runner.pid")"
cat >"$control_root/launch.tsv" <<EOF
field	value
schema_version	rqrgibbs_dlm_skewed_candidate_launch/1.1.0
source_commit	$source_commit
source_clean	TRUE
candidate_family	$candidate_family
seed_ledger	$seed_ledger
exdqlm_attestation	$exdqlm_attestation
output_root	$output_root
workers	$workers
runner_pid	$runner_pid
runner_pgid	$runner_pid
started_at_utc	$started_at
confirmatory_authorization	FALSE
scientific_promotion	FALSE
EOF

sleep 1
if ! kill -0 "$runner_pid" 2>/dev/null; then
  echo "The detached comparison exited during startup." >&2
  tail -40 "$control_root/runner.stderr.log" >&2 || true
  exit 1
fi

printf 'Candidate comparison started in the background.\n'
printf '  source commit: %s\n' "$source_commit"
printf '  output root:   %s\n' "$output_root"
printf '  control root:  %s\n' "$control_root"
printf '  PID/PGID:      %s\n' "$runner_pid"
