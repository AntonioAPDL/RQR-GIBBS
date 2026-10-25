#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"
mode=${1:-health}
baseline=${2:-}
run=${3:-application/outputs/mti_q_grid_refinement_20261022/prepared_20261022}
workers=${4:-4}
extra=("${@:5}")
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export GIT_OPTIONAL_LOCKS=0
runner=application/scripts/99_run_mti_q_grid_study.R
if [[ "$mode" == health ]]; then
  Rscript "$runner" --mode=health --run-dir="$run" "${extra[@]}"
  if [[ -f "$run/launcher.pid" ]]; then ps -p "$(<"$run/launcher.pid")" -o pid,etime,stat,args || true; fi
  exit 0
fi
if [[ -n "$(git status --porcelain)" ]]; then
  printf '%s\n' 'Commit reviewed source before preparing or launching the grid study.' >&2
  exit 1
fi
commit=$(git rev-parse HEAD)
runtime="$root/application/cache/mti_q_grid_runtime_$commit"
export R_LIBS="$runtime/library"
if [[ "$mode" == prepare ]]; then
  if [[ -f "$runtime/ready" ]]; then sha256sum --check "$runtime/SHA256SUMS"; exit 0; fi
  mkdir -p "$runtime/source" "$runtime/build" "$runtime/library"
  git archive "$commit" application | tar -x -C "$runtime/source"
  (cd "$runtime/build" && R CMD build --no-build-vignettes "$runtime/source/application")
  tarballs=("$runtime/build"/rqrgibbs_*.tar.gz)
  [[ ${#tarballs[@]} -eq 1 && -f "${tarballs[0]}" ]]
  R CMD INSTALL --library="$runtime/library" "${tarballs[0]}"
  Rscript -e 'library(rqrgibbs);stopifnot(packageVersion("rqrgibbs")=="0.1.0.9038",
    length(rqr_mti_refine_content_grid(c(.93,.995),50,.9)$added_q)==2,
    !"exdqlm" %in% loadedNamespaces())'
  sha256sum "${tarballs[0]}" "$runtime/library/rqrgibbs/libs/rqrgibbs.so" \
    "$runtime/library/rqrgibbs/R/"* > "$runtime/SHA256SUMS"
  printf '%s\n' "$commit" > "$runtime/ready"
  exit 0
fi
[[ -n "$baseline" && -f "$baseline/identity.rds" ]]
[[ -f "$runtime/ready" && "$(<"$runtime/ready")" == "$commit" ]]
[[ "$workers" =~ ^[0-9]+$ && "$workers" -ge 1 && "$workers" -le 40 ]]
sha256sum --check "$runtime/SHA256SUMS"
Rscript "$runner" --mode=preflight --baseline-run="$baseline" --run-dir="$run" --workers="$workers" "${extra[@]}"
if [[ "$mode" == preflight ]]; then exit 0; fi
[[ "$mode" == queue-screen || "$mode" == screen || "$mode" == calibration || "$mode" == validation || "$mode" == precision || "$mode" == final || "$mode" == followup ]]
run=$(Rscript -e 'source("application/scripts/lib/mti_profile_review.R");cat(mpr_output_path(commandArgs(TRUE)[1]))' "$run")
[[ "$run" == "$root/application/outputs/mti_q_grid_refinement_20261022/"* ]]
mkdir -p "$run"
exec 8>"$run/launch.lock"
if ! flock -n 8; then printf '%s\n' 'Another grid stage holds the study lock.' >&2; exit 1; fi
if [[ "$mode" == precision ]]; then
  Rscript "$runner" --mode=precision --baseline-run="$baseline" --run-dir="$run" --workers="$workers" "${extra[@]}"
  exit 0
fi
stage=$mode
if [[ "$mode" == queue-screen ]]; then stage=screen; fi
if [[ -f "$run/${stage}_exit.json" ]]; then
  printf '%s\n' 'This stage already has a terminal receipt; review it before an explicit manual resume.' >&2
  exit 1
fi
nohup setsid bash -c '
  set +e
  runner=$1; baseline=$2; run=$3; workers=$4; stage=$5; mode=$6; commit=$7
  shift 7
  status=0
  if [[ "$mode" == queue-screen ]]; then
    if [[ ! -f "$baseline/launch.lock" ]]; then status=1; else
      exec 9<"$baseline/launch.lock"
      flock -s 9 || status=$?
    fi
  fi
  if [[ "$(git rev-parse HEAD)" != "$commit" || -n "$(git status --porcelain)" ]]; then status=1; fi
  if [[ "$status" == 0 ]]; then
    Rscript "$runner" --mode="$stage" --baseline-run="$baseline" --run-dir="$run" --workers="$workers" "$@"
    status=$?
  fi
  printf "{\"stage\":\"%s\",\"exit_status\":%s,\"completed_utc\":\"%s\"}\n" \
    "$stage" "$status" "$(date -u +%FT%TZ)" > "$run/${stage}_exit.json.tmp"
  mv "$run/${stage}_exit.json.tmp" "$run/${stage}_exit.json"
  exit "$status"
' mti-grid-stage "$runner" "$baseline" "$run" "$workers" "$stage" "$mode" "$commit" "${extra[@]}" \
  >> "$run/launch.log" 2>&1 < /dev/null &
printf '%s\n' "$!" > "$run/launcher.pid"
printf 'Prepared %s PID %s with %s workers; log: %s/launch.log\n' "$mode" "$!" "$workers" "$run"
