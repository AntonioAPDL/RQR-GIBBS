#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
commit=$(git rev-parse HEAD)
root=$(dirname "$(realpath "$(git rev-parse --git-common-dir)")")
if [[ "${1:-}" == --foreground ]]; then
  output="${MTI_GAMMA_SCALE_OUTPUT_ROOT:?Missing output root}"
  exec 9>"$output/controller.lock"
  flock -n 9 || exit 73
  if [[ -f "$output/controller_exit.json" ]]; then
    mv "$output/controller_exit.json" "$output/controller_exit_$(date -u +%Y%m%dT%H%M%SZ)_$$.json"
  fi
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json.tmp"; mv "$output/controller_exit.json.tmp" "$output/controller_exit.json"; exit "$code"' EXIT
  printf '%s\n' "$$" > "$output/controller.pid"
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$root/.codex_work/mti_ecm_common_scale_latest.json")
  printf '{"stage":"waiting_for_previous_study","pid":%d}\n' "$$" > "$output/startup_status.json"
  flock -s "$previous/controller.lock" true
  Rscript application/scripts/140_assess_mti_gamma_scale.R prepare
  Rscript application/scripts/140_assess_mti_gamma_scale.R pipeline
  Rscript application/scripts/140_assess_mti_gamma_scale.R verify
  exit 0
fi
[[ $# == 0 ]] || exit 64
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit source before launch.\n' >&2; exit 65; }
exec 7>"$root/.codex_work/mti_gamma_scale_launch.lock"
flock -n 7 || exit 73
pointer="$root/.codex_work/mti_gamma_scale_latest.json"
if [[ -f "$pointer" ]]; then
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$pointer")
  flock -n "$previous/controller.lock" true || { printf 'A Gamma study is already running.\n' >&2; exit 73; }
fi
launch="$root/.codex_work/mti_gamma_scale_$commit"
output="/var/tmp/jaguir26-mti-studies/mti_gamma_scale_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$output" "$launch/application/cache"
library_source="${MTI_GAMMA_SCALE_R_LIBRARY:-$(pwd)/application/cache/gamma_scale_library}"
if [[ ! -d "$launch/application/cache/gamma_scale_library" ]]; then
  cp -a "$library_source" "$launch/application/cache/gamma_scale_library"
fi
export MTI_GAMMA_SCALE_OUTPUT_ROOT="$output"
export MTI_GAMMA_SCALE_R_LIBRARY="$launch/application/cache/gamma_scale_library"
flock -n "$output/controller.lock" true || exit 73
Rscript -e 'a<-commandArgs(TRUE);tmp<-paste0(a[1],".tmp");jsonlite::write_json(list(output_root=a[2],source_commit=a[3],checkout=a[4]),tmp,auto_unbox=TRUE,pretty=TRUE);stopifnot(file.rename(tmp,a[1]))' "$pointer" "$output" "$commit" "$launch"
setsid --fork bash "$launch/application/scripts/141_launch_mti_gamma_scale.sh" --foreground \
  >> "$output/controller.log" 2>&1 < /dev/null 7>&-
printf 'Gamma study launched.\nSource: %s\nOutput: %s\nWorker ceiling: 40\n' "$commit" "$output"
