#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
commit=$(git rev-parse HEAD)
root=$(dirname "$(realpath "$(git rev-parse --git-common-dir)")")
if [[ "${1:-}" == --foreground ]]; then
  output="${MTI_ECM_CALIBRATION_OUTPUT_ROOT:?Missing output root}"
  exec 9>"$output/controller.lock"
  flock -n 9 || exit 73
  if [[ -f "$output/controller_exit.json" ]]; then
    mv "$output/controller_exit.json" "$output/controller_exit_$(date -u +%Y%m%dT%H%M%SZ)_$$.json"
  fi
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json.tmp"; mv "$output/controller_exit.json.tmp" "$output/controller_exit.json"; exit "$code"' EXIT
  printf '%s\n' "$$" > "$output/controller.pid"
  Rscript application/scripts/142_assess_mti_ecm_adaptive_calibration.R prepare
  Rscript application/scripts/142_assess_mti_ecm_adaptive_calibration.R pipeline
  exit 0
fi
[[ $# == 0 ]] || { printf 'Usage: %s [--foreground]\n' "$0" >&2; exit 64; }
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit source before launch.\n' >&2; exit 65; }
exec 7>"$root/.codex_work/mti_ecm_adaptive_calibration_launch.lock"
flock -n 7 || exit 73
pointer="$root/.codex_work/mti_ecm_adaptive_calibration_latest.json"
if [[ -f "$pointer" ]]; then
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$pointer")
  flock -n "$previous/controller.lock" true || { printf 'An adaptive ECM study is already running.\n' >&2; exit 73; }
fi
launch="$root/.codex_work/mti_ecm_adaptive_calibration_$commit"
output="${MTI_ECM_CALIBRATION_STORAGE_ROOT:-/var/tmp/jaguir26-mti-studies}/mti_ecm_adaptive_calibration_20261120_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$output" "$launch/application/cache"
chmod 700 "$output"
library_source="${MTI_ECM_CALIBRATION_R_LIBRARY:-$(pwd)/application/cache/gamma_scale_library}"
[[ -d "$library_source/rqrgibbs" ]] || { printf 'The installed numerical library is missing.\n' >&2; exit 65; }
if [[ ! -d "$launch/application/cache/adaptive_calibration_library" ]]; then
  cp -a "$library_source" "$launch/application/cache/adaptive_calibration_library"
fi
export MTI_ECM_CALIBRATION_OUTPUT_ROOT="$output"
export MTI_ECM_CALIBRATION_R_LIBRARY="$launch/application/cache/adaptive_calibration_library"
flock -n "$output/controller.lock" true || exit 73
Rscript -e 'a<-commandArgs(TRUE);tmp<-paste0(a[1],".tmp");jsonlite::write_json(list(study_id="mti_ecm_adaptive_calibration_20261120",output_root=a[2],source_commit=a[3],checkout=a[4]),tmp,auto_unbox=TRUE,pretty=TRUE);stopifnot(file.rename(tmp,a[1]))' "$pointer" "$output" "$commit" "$launch"
setsid --fork bash "$launch/application/scripts/143_launch_mti_ecm_adaptive_calibration.sh" --foreground \
  >> "$output/controller.log" 2>&1 < /dev/null 7>&-
printf 'Adaptive ECM study launched.\nSource: %s\nOutput: %s\nWorker ceiling: 40\n' "$commit" "$output"
