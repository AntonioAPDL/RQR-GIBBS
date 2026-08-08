#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 4 ]]; then
  cat >&2 <<'EOF'
Usage: 56_orchestrate_rqr_dlm_multicomponent_recovery.sh \
  <primary-attestation.rds> <fresh-output-root> <workers> <control-root>
EOF
  exit 64
fi

repo_root="$(git rev-parse --show-toplevel)"
primary_attestation="$(realpath "$1")"
output_root="$2"
workers="$3"
control_root="$4"
if [[ "$output_root" != /* ]]; then
  output_root="$repo_root/$output_root"
fi
if [[ "$control_root" != /* ]]; then
  control_root="$repo_root/$control_root"
fi
output_root="$(realpath -m "$output_root")"
control_root="$(realpath -m "$control_root")"
preflight_root="${output_root}_preflight"
case "$output_root" in
  "$repo_root/application/cache/"*) ;;
  *) echo "Output root must be below application/cache/." >&2; exit 64 ;;
esac
case "$control_root" in
  "$repo_root/application/logs/"*) ;;
  *) echo "Control root must be below application/logs/." >&2; exit 64 ;;
esac
case "$workers" in
  ''|*[!0-9]*) echo "workers must be an integer in [1, 8]." >&2; exit 64 ;;
esac
if (( workers < 1 || workers > 8 )); then
  echo "workers must be an integer in [1, 8]." >&2
  exit 64
fi
if [[ -e "$output_root" || -e "$preflight_root" || ! -d "$control_root" ]]; then
  echo "Output roots must be fresh and control root must exist." >&2
  exit 64
fi

source_commit="$(git -C "$repo_root" rev-parse HEAD)"
if [[ -n "$(git -C "$repo_root" status --porcelain=v2 --untracked-files=all)" ||
      "$(git -C "$repo_root" rev-parse --abbrev-ref HEAD)" != "main" ]]; then
  echo "The recovery coordinator requires a clean exact launch checkout on local main." >&2
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

coordinator_pid="$$"
coordinator_pgid="$(ps -o pgid= -p "$$" | tr -d '[:space:]')"
current_stage="initialization"
terminal_status_written=0
update_status() {
  local status="$1"
  local stage="$2"
  local exit_status="${3:-}"
  local temporary
  temporary="$(mktemp "$control_root/.coordinator_status.XXXXXX")"
  cat >"$temporary" <<EOF
field	value
schema_version	rqrgibbs_dlm_multicomponent_recovery_coordinator/1.0.0
source_commit	$source_commit
coordinator_pid	$coordinator_pid
coordinator_pgid	$coordinator_pgid
status	$status
stage	$stage
output_root	$output_root
preflight_root	$preflight_root
workers	$workers
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

existing_user_library="${R_LIBS_USER:-/home/jaguir26/R/x86_64-redhat-linux-gnu-library/4.5}"
common_env=(
  OMP_NUM_THREADS=1
  OPENBLAS_NUM_THREADS=1
  MKL_NUM_THREADS=1
  BLIS_NUM_THREADS=1
  VECLIB_MAXIMUM_THREADS=1
  NUMEXPR_NUM_THREADS=1
  RCPP_PARALLEL_NUM_THREADS=1
  "R_LIBS_USER=$runtime_library:$existing_user_library"
  "RQR_EXPECTED_PRIMARY_COMMIT=$source_commit"
  "RQR_PRIMARY_RUNTIME_ATTESTATION=$primary_attestation"
)
runner="$repo_root/application/scripts/55_compare_rqr_dlm_multicomponent_scale_candidates.R"

update_status starting initialization
current_stage="preflight"
update_status running "$current_stage"
(
  cd "$repo_root"
  env "${common_env[@]}" Rscript "$runner" \
    --mode=preflight --output-root="$preflight_root" --workers="$workers"
) >"$control_root/preflight.stdout.log" \
  2>"$control_root/preflight.stderr.log"

current_stage="candidate_comparison"
update_status running "$current_stage"
(
  cd "$repo_root"
  env "${common_env[@]}" Rscript "$runner" \
    --mode=execute --output-root="$output_root" --workers="$workers"
) >"$control_root/runner.stdout.log" \
  2>"$control_root/runner.stderr.log"

current_stage="complete"
update_status passed "$current_stage" 0
terminal_status_written=1
printf 'Multicomponent recovery comparison completed: %s\n' "$output_root"
