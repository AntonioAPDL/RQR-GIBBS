#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 BLIS_NUM_THREADS=1
commit=$(git rev-parse HEAD)
root=$(dirname "$(realpath "$(git rev-parse --git-common-dir)")")
if [[ "${1:-}" == --foreground ]]; then
  output="${MTI_SKEWNESS_OUTPUT_ROOT:?Missing output root}"
  exec 9>"$output/controller.lock"
  flock -n 9 || exit 73
  if [[ -f "$output/controller_exit.json" ]]; then
    mv "$output/controller_exit.json" "$output/controller_exit_$(date -u +%Y%m%dT%H%M%SZ)_$$.json"
  fi
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json.tmp"; mv "$output/controller_exit.json.tmp" "$output/controller_exit.json"; exit "$code"' EXIT
  printf '%s\n' "$$" > "$output/controller.pid"
  Rscript application/scripts/144_run_mti_skewness_extension.R run
  exit 0
fi
[[ $# == 0 ]] || { printf 'Usage: %s [--foreground]\n' "$0" >&2; exit 64; }
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit source before launch.\n' >&2; exit 65; }
exec 7>"$root/.codex_work/mti_skewness_extension_launch.lock"
flock -n 7 || exit 73
pointer="$root/.codex_work/mti_skewness_extension_latest.json"
if [[ -f "$pointer" ]]; then
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$pointer")
  flock -n "$previous/controller.lock" true || { printf 'The skewness study is already running.\n' >&2; exit 73; }
fi
launch="$root/.codex_work/mti_skewness_extension_frozen_$commit"
output="${MTI_SKEWNESS_STORAGE_ROOT:-/var/tmp/jaguir26-mti-studies}/mti_skewness_extension_20261121_$commit"
[[ -d "$launch" ]] || git worktree add --detach "$launch" "$commit"
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$output" "$launch/application/cache"
chmod 700 "$output"
library_source="${MTI_SKEWNESS_R_LIBRARY:-$(pwd)/application/cache/skewness_library}"
[[ -d "$library_source/statmod" && -d "$library_source/actuar" && -d "$library_source/tolerance" ]] || {
  printf 'Prepare the isolated library first.\n' >&2; exit 65;
}
if [[ ! -d "$launch/application/cache/skewness_library" ]]; then
  cp -a "$library_source" "$launch/application/cache/skewness_library"
fi
cp -p "$(dirname "$library_source")/skewness_runtime.json" "$launch/application/cache/skewness_runtime.json"
export MTI_SKEWNESS_R_LIBRARY="$launch/application/cache/skewness_library"
export MTI_SKEWNESS_OUTPUT_ROOT="$output"
flock -n "$output/controller.lock" true || exit 73
Rscript -e 'a<-commandArgs(TRUE);tmp<-paste0(a[1],".tmp");jsonlite::write_json(list(study_id="mti_skewness_extension_20261121",output_root=a[2],source_commit=a[3],checkout=a[4]),tmp,auto_unbox=TRUE,pretty=TRUE);stopifnot(file.rename(tmp,a[1]))' "$pointer" "$output" "$commit" "$launch"
setsid --fork bash "$launch/application/scripts/145_launch_mti_skewness_extension.sh" --foreground \
  >> "$output/controller.log" 2>&1 < /dev/null 7>&-
printf 'Skewness extension launched.\nSource: %s\nOutput: %s\nWorker ceiling: 40\n' "$commit" "$output"
