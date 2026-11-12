#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
commit=$(git rev-parse HEAD)
runtime="application/cache/mti_gibbs_runtime_$commit"
if [[ -e "$runtime/commit.txt" ]]; then
  test "$(<"$runtime/commit.txt")" = "$commit"
  printf 'Existing installed runtime: %s/library\n' "$runtime"
  exit 0
fi
mkdir -p "$runtime/source" "$runtime/library"
git archive "$commit" application | tar -x -C "$runtime/source"
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
  R CMD INSTALL --library="$runtime/library" "$runtime/source/application"
printf '%s\n' "$commit" > "$runtime/commit.txt"
printf 'Prepared installed runtime without sampling: %s/library\n' "$runtime"
