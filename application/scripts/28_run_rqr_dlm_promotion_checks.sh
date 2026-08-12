#!/usr/bin/env bash
set -euo pipefail

# Run the post-computation RQR-DLM promotion checks without installing into or
# otherwise mutating the exact runtime whose attestation supports the heavy
# validation gates. The checked package is installed from the attested source
# package into a fresh, disjoint library under the ignored output root.

if [[ $# -ne 5 ]]; then
  echo "Usage: $0 <expected-commit> <primary-attestation.rds> <exdqlm-cran-attestation.json> <quantreg-cran-attestation.json> <fresh-output-root>" >&2
  exit 64
fi

expected_commit="${1,,}"
primary_attestation="$(realpath "$2")"
exdqlm_attestation="$(realpath "$3")"
quantreg_attestation="$(realpath "$4")"
output_root="$(realpath -m "$5")"
repo_root="$(pwd -P)"
git_common_dir="$(
  git -C "$repo_root" -c core.fsmonitor=false \
    rev-parse --path-format=absolute --git-common-dir
)"
canonical_repo_root="$(dirname "$git_common_dir")"
canonical_repo_parent="$(dirname "$canonical_repo_root")"
exdqlm_repo="${EXDQLM_RQR_REPO:-$canonical_repo_parent/exdqlm__wt__qdesn_0p4p0_integration}"

if [[ ! "$expected_commit" =~ ^[0-9a-f]{40}$ ]]; then
  echo "The expected commit must be a complete lowercase SHA." >&2
  exit 64
fi
if [[ ! -f "$repo_root/application/DESCRIPTION" ]]; then
  echo "Run this script from the RQR-GIBBS repository root." >&2
  exit 64
fi
if [[ ! -d "$exdqlm_repo/.git" && ! -f "$exdqlm_repo/.git" ]]; then
  echo "Could not locate the protected exdqlm checkout: $exdqlm_repo" >&2
  echo "Set EXDQLM_RQR_REPO explicitly when it is not beside the canonical RQR-GIBBS checkout." >&2
  exit 66
fi
exdqlm_repo="$(realpath "$exdqlm_repo")"
if [[ -e "$output_root" ]]; then
  echo "The promotion-check output root must be fresh: $output_root" >&2
  exit 64
fi
for command in git R Rscript jq sha256sum realpath tar; do
  if ! command -v "$command" >/dev/null 2>&1; then
    echo "Missing required command: $command" >&2
    exit 69
  fi
done

head_commit="$(git -c core.fsmonitor=false rev-parse HEAD)"
branch="$(git -c core.fsmonitor=false rev-parse --abbrev-ref HEAD)"
tracked_status="$(
  git -c core.fsmonitor=false status --porcelain=v2 --untracked-files=all
)"
allow_detached_launch_source="${RQR_ALLOW_DETACHED_LAUNCH_SOURCE:-FALSE}"
branch_allowed="FALSE"
if [[ "$branch" == "main" ]]; then
  branch_allowed="TRUE"
elif [[ "$allow_detached_launch_source" == "TRUE" && "$branch" == "HEAD" ]]; then
  branch_allowed="TRUE"
fi
if [[ "$head_commit" != "$expected_commit" ||
      "$branch_allowed" != "TRUE" ||
      -n "$tracked_status" ]]; then
  echo "Promotion checks require clean main at the expected commit, or an explicitly authorized clean detached launch-source worktree at that commit." >&2
  exit 65
fi

compiled_before="$(
  find application/src -maxdepth 1 -type f \
    \( -name '*.o' -o -name '*.so' -o -name '*.dll' \) -print
)"
if [[ -n "$compiled_before" ]]; then
  echo "The source checkout already contains ignored compiler artifacts:" >&2
  echo "$compiled_before" >&2
  exit 65
fi

mkdir -p "$output_root/logs" "$output_root/check-library"
gate_status="$output_root/gate_status.csv"
echo "stage,status,started_at_utc,ended_at_utc,elapsed_seconds,stdout,stderr" \
  >"$gate_status"

run_logged() {
  local stage="$1"
  shift
  local stdout="$output_root/logs/${stage}.stdout.log"
  local stderr="$output_root/logs/${stage}.stderr.log"
  local started ended start_epoch end_epoch status
  started="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  start_epoch="$(date +%s)"
  echo "[RUN] $stage"
  set +e
  "$@" >"$stdout" 2>"$stderr"
  status=$?
  set -e
  ended="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  end_epoch="$(date +%s)"
  printf '%s,%d,%s,%s,%d,%s,%s\n' \
    "$stage" "$status" "$started" "$ended" \
    "$((end_epoch - start_epoch))" "$stdout" "$stderr" >>"$gate_status"
  if (( status != 0 )); then
    echo "[FAIL] $stage returned $status" >&2
    tail -80 "$stdout" >&2 || true
    tail -80 "$stderr" >&2 || true
    exit "$status"
  fi
  echo "[PASS] $stage"
}

primary_fields="$(
  Rscript - "$primary_attestation" "$repo_root" "$expected_commit" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
attestation <- readRDS(args[[1L]])
repo_root <- args[[2L]]
expected_commit <- args[[3L]]
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "isolated_runtime_lineage.R"
  ),
  envir = environment()
)
required <- c(
  "source_commit", "source_package_path", "source_package_sha256",
  "runtime_package_path", "runtime_package_tree_digest"
)
if (!all(required %in% names(attestation)) ||
    !identical(attestation$source_commit, expected_commit) ||
    !file.exists(attestation$source_package_path) ||
    !dir.exists(attestation$runtime_package_path) ||
    !identical(
      rqr_file_sha256(attestation$source_package_path),
      attestation$source_package_sha256
    ) ||
    !identical(
      rqr_directory_digest(attestation$runtime_package_path),
      attestation$runtime_package_tree_digest
    )) {
  stop("The primary runtime attestation does not verify.", call. = FALSE)
}
cat(
  attestation$source_package_path,
  attestation$runtime_package_path,
  attestation$runtime_package_tree_digest,
  sep = "\t"
)
RS
)"
IFS=$'\t' read -r primary_source_package primary_runtime_path \
  primary_runtime_digest <<<"$primary_fields"

exdqlm_runtime_path="$(jq -er '.runtime_path' "$exdqlm_attestation")"
quantreg_runtime_path="$(jq -er '.runtime_path' "$quantreg_attestation")"
for runtime_path in "$exdqlm_runtime_path" "$quantreg_runtime_path"; do
  if [[ ! -d "$runtime_path" ]]; then
    echo "A comparator runtime is missing: $runtime_path" >&2
    exit 66
  fi
done

original_r_libs="${R_LIBS:-}"
check_library="$output_root/check-library"
dependency_libraries="$(
  dirname "$exdqlm_runtime_path"
):$(dirname "$quantreg_runtime_path")"
export R_LIBS="$check_library:$dependency_libraries${original_r_libs:+:$original_r_libs}"
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export RCPP_PARALLEL_NUM_THREADS=1
export EXDQLM_RQR_REPO="$exdqlm_repo"

# Generic package tests exercise their own fail-closed runtime branches. They
# must not inherit the exact-runtime binding used by the heavy promotion gates.
unset RQR_EXPECTED_PRIMARY_COMMIT
unset RQR_PRIMARY_RUNTIME_ATTESTATION

run_logged install_check_runtime \
  R CMD INSTALL --preclean --clean --library="$check_library" \
  "$primary_source_package"
run_logged make_smoke \
  Rscript application/scripts/00_validate_environment.R
run_logged test_standalone_contracts \
  Rscript -e \
  'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "dlm-bounded|dlm-main|dlm-confirmatory", reporter = "summary")'
run_logged test_native \
  Rscript -e \
  'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "native", reporter = "summary")'
run_logged test_native_mean_tilt \
  Rscript -e \
  'library(rqrgibbs); testthat::test_dir("application/tests/testthat", filter = "native-mean-tilt", reporter = "summary")'
run_logged test_oracle_tilt_illustrations \
  Rscript -e \
  'library(rqrgibbs); testthat::test_file("application/tests/testthat/test-rqr-oracle-tilt-illustrations.R", reporter = "summary")'

mkdir -p "$output_root/package-check"
run_logged package_check \
  bash -c 'cd "$1" && R CMD check --no-manual "$2"' \
  _ "$output_root/package-check" "$primary_source_package"

run_logged prepare_pinned_exdqlm_runtime \
  Rscript application/scripts/04_prepare_pinned_exdqlm_runtime.R
run_logged test_exdqlm_rqr \
  Rscript application/scripts/02_smoke_rqr_exdqlm_branch.R

# The ordinary PDF targets regenerate a tracked provenance receipt whose
# repository-commit field legitimately changes at every source commit. Build
# the exact committed document tree in an isolated archive checkout so that
# publication checks remain complete without mutating the promotion source.
document_source="$output_root/document-source"
mkdir -p "$document_source"
run_logged prepare_document_source \
  bash -c \
  'git -c core.fsmonitor=false archive "$1" | tar -x -C "$2"' \
  _ "$expected_commit" "$document_source"
run_logged make_pdf make -C "$document_source" pdf
run_logged make_supplement make -C "$document_source" supplement
run_logged make_literature_manifest make literature-manifest

runtime_digest_after="$(
  Rscript - "$primary_attestation" "$repo_root" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
attestation <- readRDS(args[[1L]])
repo_root <- args[[2L]]
sys.source(
  file.path(
    repo_root, "application", "scripts", "lib",
    "isolated_runtime_lineage.R"
  ),
  envir = environment()
)
cat(rqr_directory_digest(attestation$runtime_package_path))
RS
)"
if [[ "$runtime_digest_after" != "$primary_runtime_digest" ]]; then
  echo "The exact attested runtime changed during promotion checks." >&2
  exit 65
fi

compiled_after="$(
  find application/src -maxdepth 1 -type f \
    \( -name '*.o' -o -name '*.so' -o -name '*.dll' \) -print
)"
if [[ -n "$compiled_after" ]]; then
  echo "Promotion checks left compiler artifacts in the source checkout." >&2
  echo "$compiled_after" >&2
  exit 65
fi
if [[ -n "$(
  git -c core.fsmonitor=false status --porcelain=v2 --untracked-files=all
)" ]]; then
  echo "Promotion checks changed tracked or untracked source files." >&2
  git -c core.fsmonitor=false status --short >&2
  exit 65
fi

{
  echo "field,value"
  printf 'source_commit,%s\n' "$expected_commit"
  printf 'primary_source_package,%s\n' "$primary_source_package"
  printf 'primary_runtime_path,%s\n' "$primary_runtime_path"
  printf 'primary_runtime_digest_before,%s\n' "$primary_runtime_digest"
  printf 'primary_runtime_digest_after,%s\n' "$runtime_digest_after"
  printf 'check_library,%s\n' "$check_library"
  printf 'exdqlm_runtime_path,%s\n' "$exdqlm_runtime_path"
  printf 'exdqlm_source_checkout,%s\n' "$exdqlm_repo"
  printf 'quantreg_runtime_path,%s\n' "$quantreg_runtime_path"
  printf 'source_worktree_clean_after,TRUE\n'
  printf 'source_compiler_artifacts_after,0\n'
} >"$output_root/runtime_binding.csv"

Rscript - "$output_root" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
files <- list.files(
  root, recursive = TRUE, full.names = TRUE, all.files = TRUE,
  no.. = TRUE
)
files <- files[file.info(files)$isdir %in% FALSE]
files <- files[basename(files) != "artifact_hashes.csv"]
relative <- substring(files, nchar(root) + 2L)
sha <- vapply(
  files,
  function(path) digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  ),
  character(1L)
)
value <- data.frame(
  relative_path = relative,
  bytes = unname(file.info(files)$size),
  sha256 = unname(sha),
  stringsAsFactors = FALSE
)
utils::write.csv(
  value[order(value$relative_path), , drop = FALSE],
  file.path(root, "artifact_hashes.csv"),
  row.names = FALSE, quote = TRUE
)
RS

echo "[SUCCESS] Hermetic RQR-DLM promotion checks passed."
echo "  evidence: $output_root"
