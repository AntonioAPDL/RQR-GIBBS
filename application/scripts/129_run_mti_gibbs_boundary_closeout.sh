#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
root=$(dirname "$(realpath "$(git rev-parse --git-common-dir)")")
export MTI_PRIOR_R_LIBRARY="${MTI_PRIOR_R_LIBRARY:-$root/.codex_work/mti_fixed_prior_9b53485fc9e4d6ab43a92d80a1b0931bf58c910e/application/cache/fixed_prior_library}"
output=application/outputs/mti_gibbs_boundary_closeout_20261116
mkdir -p "$output"
exec 9>"$output/controller.lock"
flock -n 9 || { printf 'Closeout calculations are already running.\n' >&2; exit 73; }
printf '%s\n' "$$" > "$output/controller.pid"
trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
Rscript application/scripts/128_close_mti_gibbs_boundary_study.R run
