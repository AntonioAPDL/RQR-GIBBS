#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

config="${RQR_BAYES_UQ_MAIN_CONFIG:-application/config/rqr_bayes_uq_validation_main_20260813.json}"
run_root="${RQR_BAYES_UQ_MAIN_WAVE_RUN_ROOT:-application/runs/rqr_bayes_uq_validation_main_20260813}"
mode="${RQR_BAYES_UQ_MAIN_WAVE_MODE:-confirmatory}"
run_id="${RQR_BAYES_UQ_MAIN_WAVE_RUN_ID:-wave_${mode}_$(date -u +%Y%m%dT%H%M%SZ)}"
max_concurrent="${RQR_BAYES_UQ_MAIN_WAVE_MAX_CONCURRENT:-6}"
poll_seconds="${RQR_BAYES_UQ_MAIN_WAVE_POLL_SECONDS:-60}"
run_dir="${RQR_BAYES_UQ_MAIN_RUN_DIR:-}"

git_status="$(git status --short)"
if [[ -n "${git_status}" ]]; then
  echo "Refusing to launch Bayesian uncertainty waves from a dirty source tree." >&2
  echo "${git_status}" >&2
  exit 65
fi

if [[ -z "${run_dir}" ]]; then
  prepare_output="$(
    Rscript application/scripts/71_manage_rqr_bayes_uq_main_waves.R \
      --action=prepare \
      "--mode=${mode}" \
      "--config=${config}" \
      "--run-root=${run_root}" \
      "--run-id=${run_id}"
  )"
  printf '%s\n' "${prepare_output}"
  run_dir="$(printf '%s\n' "${prepare_output}" | awk -F= '/^RUN_DIR=/{print $2}' | tail -1)"
fi

if [[ -z "${run_dir}" || ! -d "${run_dir}" ]]; then
  echo "Could not resolve prepared Bayesian uncertainty wave run directory." >&2
  exit 66
fi

mkdir -p "${run_dir}/logs" "${run_dir}/pids"
scheduler_log="${run_dir}/logs/scheduler.log"
scheduler_pid_file="${run_dir}/pids/scheduler.pid"
scheduler_config="${run_dir}/config_frozen.json"
if [[ ! -f "${scheduler_config}" ]]; then
  scheduler_config="${config}"
fi

if command -v setsid >/dev/null 2>&1; then
  nohup setsid Rscript application/scripts/71_manage_rqr_bayes_uq_main_waves.R \
    --action=launch \
    "--mode=${mode}" \
    "--config=${scheduler_config}" \
    "--run-dir=${run_dir}" \
    "--max-concurrent=${max_concurrent}" \
    "--poll-seconds=${poll_seconds}" \
    >"${scheduler_log}" 2>&1 &
else
  nohup Rscript application/scripts/71_manage_rqr_bayes_uq_main_waves.R \
    --action=launch \
    "--mode=${mode}" \
    "--config=${scheduler_config}" \
    "--run-dir=${run_dir}" \
    "--max-concurrent=${max_concurrent}" \
    "--poll-seconds=${poll_seconds}" \
    >"${scheduler_log}" 2>&1 &
fi
scheduler_pid="$!"
echo "${scheduler_pid}" > "${scheduler_pid_file}"
sleep 2
if ! kill -0 "${scheduler_pid}" >/dev/null 2>&1; then
  echo "Bayesian uncertainty wave scheduler failed to remain active." >&2
  echo "  log_file: ${scheduler_log}" >&2
  exit 67
fi

printf 'Launched Bayesian uncertainty wave scheduler\n'
printf '  mode: %s\n' "${mode}"
printf '  run_dir: %s\n' "${run_dir}"
printf '  scheduler_pid: %s\n' "${scheduler_pid}"
printf '  scheduler_log: %s\n' "${scheduler_log}"
printf '  max_concurrent: %s\n' "${max_concurrent}"
