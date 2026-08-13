#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

mode="${RQR_BAYES_UQ_OVERNIGHT_MODE:-moderate}"
config="${RQR_BAYES_UQ_VALIDATION_CONFIG:-application/config/rqr_bayes_uq_validation_v1.json}"
run_id="${RQR_BAYES_UQ_RUN_ID:-overnight_$(date -u +%Y%m%dT%H%M%SZ)}"
run_root="${RQR_BAYES_UQ_VALIDATION_RUN_ROOT:-application/runs/rqr_bayes_uq_validation_v1}"
log_root="${RQR_BAYES_UQ_VALIDATION_LOG_ROOT:-application/logs/rqr_bayes_uq_validation_v1}"
output_dir="${run_root}/${run_id}"
log_file="${log_root}/${run_id}.log"
pid_file="${log_root}/${run_id}.pid"

mkdir -p "${run_root}" "${log_root}"

git_commit="$(git rev-parse HEAD)"
git_status="$(git status --short)"
if [[ -n "${git_status}" ]]; then
  echo "Refusing to launch overnight Bayesian UQ validation run from a dirty source tree." >&2
  echo "${git_status}" >&2
  exit 65
fi

launch_cmd=(
  Rscript application/scripts/69_validate_rqr_bayes_uq.R
  "--mode=${mode}"
  "--config=${config}"
  "--output-dir=${output_dir}"
)

if command -v setsid >/dev/null 2>&1; then
  nohup setsid "${launch_cmd[@]}" >"${log_file}" 2>&1 &
else
  nohup "${launch_cmd[@]}" >"${log_file}" 2>&1 &
fi
pid="$!"
echo "${pid}" > "${pid_file}"
sleep 2
if ! kill -0 "${pid}" >/dev/null 2>&1; then
  printf 'Bayesian UQ validation run failed to remain active.\n' >&2
  printf '  pid: %s\n' "${pid}" >&2
  printf '  log_file: %s\n' "${log_file}" >&2
  exit 66
fi

printf 'Launched Bayesian UQ validation run\n'
printf '  mode: %s\n' "${mode}"
printf '  git_commit: %s\n' "${git_commit}"
printf '  pid: %s\n' "${pid}"
printf '  output_dir: %s\n' "${output_dir}"
printf '  log_file: %s\n' "${log_file}"
printf '  pid_file: %s\n' "${pid_file}"
