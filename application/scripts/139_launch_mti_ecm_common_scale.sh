#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
commit=$(git rev-parse HEAD)
common=$(realpath "$(git rev-parse --git-common-dir)")
root=$(dirname "$common")
if [[ "${1:-}" == --foreground ]]; then
  output="${MTI_COMMON_SCALE_OUTPUT_ROOT:?Missing isolated output root}"
  exec 9>"$output/controller.lock"
  flock -n 9 || exit 73
  if [[ -f "$output/controller_exit.json" ]]; then
    mv "$output/controller_exit.json" "$output/controller_exit_$(date -u +%Y%m%dT%H%M%SZ)_$$.json"
  fi
  printf '%s\n' "$$" > "$output/controller.pid"
  printf '{"stage":"preparation","pid":%d}\n' "$$" > "$output/startup_status.json"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json.tmp"; mv "$output/controller_exit.json.tmp" "$output/controller_exit.json"; exit "$code"' EXIT
  Rscript application/scripts/138_assess_mti_ecm_common_scale.R prepare
  printf '{"stage":"rehearsal","pid":%d}\n' "$$" > "$output/startup_status.json"
  Rscript application/scripts/138_assess_mti_ecm_common_scale.R check
  printf '{"stage":"screening","pid":%d}\n' "$$" > "$output/startup_status.json"
  Rscript application/scripts/138_assess_mti_ecm_common_scale.R screen
  exit 0
fi
[[ $# == 0 ]] || exit 64
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit source before launch.\n' >&2; exit 65; }
exec 7>"$root/.codex_work/mti_ecm_common_scale_launch.lock"
flock -n 7 || exit 73
pointer="$root/.codex_work/mti_ecm_common_scale_latest.json"
if [[ -f "$pointer" ]]; then
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$pointer")
  flock -n "$previous/controller.lock" true || { printf 'A common-scale study is already running.\n' >&2; exit 73; }
fi
launch="$root/.codex_work/mti_ecm_common_scale_$commit"
output="/var/tmp/jaguir26-mti-studies/mti_ecm_common_scale_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$output"
export MTI_COMMON_SCALE_OUTPUT_ROOT="$output"
library_source="${MTI_COMMON_SCALE_R_LIBRARY:-$(pwd)/application/cache/common_scale_library}"
mkdir -p "$launch/application/cache"
if [[ ! -d "$launch/application/cache/common_scale_library" ]]; then
  cp -a "$library_source" "$launch/application/cache/common_scale_library"
fi
export MTI_COMMON_SCALE_R_LIBRARY="$launch/application/cache/common_scale_library"
flock -n "$output/controller.lock" true || exit 73
Rscript -e 'a<-commandArgs(TRUE);tmp<-paste0(a[1],".tmp");jsonlite::write_json(list(
  output_root=a[2],source_commit=a[3],checkout=a[4]),tmp,auto_unbox=TRUE,pretty=TRUE);
  stopifnot(file.rename(tmp,a[1]))' "$pointer" "$output" "$commit" "$launch"
setsid --fork bash "$launch/application/scripts/139_launch_mti_ecm_common_scale.sh" --foreground \
  >> "$output/controller.log" 2>&1 < /dev/null 7>&-
printf 'Common-scale ECM screening launched.\nSource: %s\nOutput: %s\nWorker ceiling: 40\n' "$commit" "$output"
