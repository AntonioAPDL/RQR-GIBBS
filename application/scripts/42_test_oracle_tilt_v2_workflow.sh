#!/usr/bin/env bash
set -euo pipefail

repo_root="$(pwd -P)"
if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this test from the RQR-GIBBS repository root." >&2
  exit 2
fi
test_root="$(mktemp -d "$repo_root/application/cache/otv2-workflow-test-XXXXXX")"
cleanup() {
  rm -rf -- "$test_root"
}
trap cleanup EXIT

preflight_dir="$test_root/preflight"
RQR_ORACLE_TILT_V2_OUTPUT_DIR="$preflight_dir" \
  application/scripts/41_run_oracle_tilt_publication_v2.sh preflight

Rscript - "$preflight_dir" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
root <- args[1L]
resource <- read.csv(file.path(root, "resource_summary.csv"))
wrapper <- read.csv(file.path(root, "wrapper_closeout.csv"))
stopifnot(
  nrow(resource) == 1L, isTRUE(resource$pass),
  isTRUE(resource$final_pgid_empty), resource$runner_status == 0L,
  resource$process_limit == 7L, resource$r_process_limit == 3L,
  resource$thread_limit == 8L, identical(resource$timezone, "UTC"),
  resource$max_sampled_r_processes <= resource$r_process_limit,
  nrow(wrapper) == 1L, isTRUE(wrapper$wrapper_pass),
  file.exists(file.path(root, "wrapper_artifact_manifest.csv"))
)
RSCRIPT

if Rscript application/scripts/40_run_oracle_tilt_publication_v2.R \
    --mode=benchmark \
    --config=application/config/oracle_tilt_c095_publication_v2_20260731.json \
    --output-dir="$test_root/benchmark" >/dev/null 2>&1; then
  echo "Benchmark unexpectedly bypassed isolated-runtime fail-closure." >&2
  exit 1
fi

if Rscript application/scripts/40_run_oracle_tilt_publication_v2.R \
    --mode=execute \
    --config=application/config/oracle_tilt_c095_publication_v2_20260731.json \
    --output-dir="$test_root/execute" >/dev/null 2>&1; then
  echo "Execute unexpectedly bypassed authorization fail-closure." >&2
  exit 1
fi

if Rscript application/scripts/41_package_oracle_tilt_v2_evidence.R \
    --run-dir="$preflight_dir" \
    --output-dir="$test_root/evidence" >/dev/null 2>&1; then
  echo "The packager unexpectedly accepted preflight-only evidence." >&2
  exit 1
fi

echo "[oracle-tilt-v2-workflow-test] PASS"
