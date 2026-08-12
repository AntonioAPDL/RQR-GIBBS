#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

mode="${RQR_BAYES_UQ_OVERNIGHT_MODE:-moderate}"
config="${RQR_BAYES_UQ_VALIDATION_CONFIG:-application/config/rqr_bayes_uq_validation_v1.json}"
run_id="overnight_$(date -u +%Y%m%dT%H%M%SZ)"
run_root="${RQR_BAYES_UQ_VALIDATION_RUN_ROOT:-application/runs/rqr_bayes_uq_validation_v1}"
log_root="${RQR_BAYES_UQ_VALIDATION_LOG_ROOT:-application/logs/rqr_bayes_uq_validation_v1}"
output_dir="${run_root}/${run_id}"
log_file="${log_root}/${run_id}.log"
pid_file="${log_root}/${run_id}.pid"

mkdir -p "${run_root}" "${log_root}"

git_commit="$(git rev-parse HEAD)"
git_status="$(git status --short)"
if [[ -n "${git_status}" ]]; then
  echo "Refusing to launch overnight Bayesian UQ pilot from a dirty source tree." >&2
  echo "${git_status}" >&2
  exit 65
fi

nohup Rscript application/scripts/69_validate_rqr_bayes_uq.R \
  --mode="${mode}" \
  --config="${config}" \
  --output-dir="${output_dir}" \
  >"${log_file}" 2>&1 &
pid="$!"
echo "${pid}" > "${pid_file}"

cat "Launched Bayesian UQ validation pilot\n"
cat "  mode: ${mode}\n"
cat "  git_commit: ${git_commit}\n"
cat "  pid: ${pid}\n"
cat "  output_dir: ${output_dir}\n"
cat "  log_file: ${log_file}\n"
cat "  pid_file: ${pid_file}\n"
