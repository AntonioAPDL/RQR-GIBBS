#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"
mode=${1:-health}
workers=${2:-24}
run=${3:-application/outputs/mti_profile_revision_20261021/revision_20261021}
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
commit=$(git rev-parse HEAD)
runtime="$root/application/cache/mti_profile_runtime_$commit"
export R_LIBS="$runtime/library"
if [[ "$mode" == health ]]; then
  Rscript application/scripts/92_run_mti_profile_revision.R --stage=health --run-dir="$run"
  if [[ -f "$run/launcher.pid" ]]; then
    ps -p "$(<"$run/launcher.pid")" -o pid,etime,stat,args || true
  fi
  exit 0
fi
if [[ -n "$(git status --porcelain)" ]]; then
  printf '%s\n' 'Commit the reviewed source before runtime preparation or launch.' >&2
  exit 1
fi
if [[ "$mode" == prepare ]]; then
  if [[ -f "$runtime/ready" ]]; then
    printf 'Committed runtime already prepared: %s\n' "$runtime"
    exit 0
  fi
  mkdir -p "$runtime/source" "$runtime/library" "$runtime/build"
  git archive "$commit" application | tar -x -C "$runtime/source"
  (cd "$runtime/build" && R CMD build --no-build-vignettes "$runtime/source/application")
  tarballs=("$runtime/build"/rqrgibbs_*.tar.gz)
  [[ ${#tarballs[@]} -eq 1 && -f "${tarballs[0]}" ]]
  R CMD INSTALL --library="$runtime/library" "${tarballs[0]}"
  Rscript -e 'library(rqrgibbs);stopifnot(rqr_mti_intercept_fit(1:4,.9)$converged,
    !"exdqlm" %in% loadedNamespaces())'
  sha256sum "${tarballs[0]}" "$runtime/library/rqrgibbs/libs/rqrgibbs.so" > "$runtime/SHA256SUMS"
  printf '%s\n' "$commit" > "$runtime/ready"
elif [[ "$mode" == launch ]]; then
  [[ "$workers" =~ ^[0-9]+$ && "$workers" -ge 1 && "$workers" -le 40 ]]
  [[ -f "$runtime/ready" && "$(<"$runtime/ready")" == "$commit" ]]
  sha256sum --check "$runtime/SHA256SUMS"
  Rscript application/scripts/92_run_mti_profile_revision.R --stage=preflight
  mkdir -p "$run"
  run=$(cd "$run" && pwd)
  if [[ -f "$run/launcher.pid" ]] && kill -0 "$(<"$run/launcher.pid")" 2>/dev/null; then
    printf 'A launcher is already active: %s\n' "$(<"$run/launcher.pid")" >&2
    exit 1
  fi
  nohup setsid flock -n "$run/launch.lock" bash -c '
    set +e
    Rscript application/scripts/92_run_mti_profile_revision.R --stage=pipeline --workers="$1" --run-dir="$2"
    status=$?
    printf "{\"exit_status\":%s,\"finished_utc\":\"%s\"}\n" "$status" "$(date -u +%FT%TZ)" > "$2/launcher_exit.json"
    exit "$status"
  ' profile-study "$workers" "$run" >> "$run/launch.log" 2>&1 < /dev/null &
  printf '%s\n' "$!" > "$run/launcher.pid"
  printf 'Launched PID %s with %s workers; log: %s/launch.log\n' "$!" "$workers" "$run"
else
  printf 'Usage: bash %s prepare|launch|health [workers] [run-directory]\n' "$0" >&2
  exit 1
fi
