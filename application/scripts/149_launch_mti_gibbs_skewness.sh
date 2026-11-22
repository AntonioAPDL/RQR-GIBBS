#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 BLIS_NUM_THREADS=1
if [[ "${1:-}" == --foreground ]]; then
  output="${MTI_GIBBS_SKEWNESS_OUTPUT_ROOT:?Missing Gibbs output root}"
  exec 9>"$output/controller.lock"
  flock -n 9 || exit 73
  if [[ -f "$output/controller_exit.json" ]]; then
    mv "$output/controller_exit.json" "$output/controller_exit_$(date -u +%Y%m%dT%H%M%SZ)_$$.json"
  fi
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json.tmp"; mv "$output/controller_exit.json.tmp" "$output/controller_exit.json"; exit "$code"' EXIT
  printf '%s\n' "$$" > "$output/controller.pid"
  Rscript application/scripts/148_run_mti_gibbs_skewness.R run
  exit 0
fi
[[ $# == 0 || ( $# == 1 && "$1" == --resume ) ]] || { printf 'Usage: %s [--resume]\n' "$0" >&2; exit 64; }
root=$(dirname "$(realpath "$(git rev-parse --git-common-dir)")")
mkdir -p "$root/.codex_work"
exec 7>"$root/.codex_work/mti_gibbs_skewness_launch.lock"
flock -n 7 || exit 73
pointer="$root/.codex_work/mti_gibbs_skewness_latest.json"
if [[ -f "$pointer" ]]; then
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$pointer")
  flock -n "$previous/controller.lock" true || { printf 'The Gibbs extension is already running.\n' >&2; exit 73; }
fi
if [[ "${1:-}" == --resume ]]; then
  [[ -f "$pointer" ]] || { printf 'No Gibbs extension exists to resume.\n' >&2; exit 65; }
  mapfile -t settings < <(Rscript -e 'p<-jsonlite::read_json(commandArgs(TRUE)[1]);cat(p$checkout,p$output_root,p$source_commit,p$parent,sep="\n")' "$pointer")
  launch="${settings[0]}"; output="${settings[1]}"; commit="${settings[2]}"; parent="${settings[3]}"
else
  [[ -z "$(git status --porcelain)" ]] || { printf 'Commit source before launch.\n' >&2; exit 65; }
  commit=$(git rev-parse HEAD)
  launch="$root/.codex_work/mti_gibbs_skewness_frozen_$commit"
  output="${MTI_GIBBS_SKEWNESS_STORAGE_ROOT:-/var/tmp/jaguir26-mti-studies}/mti_gibbs_skewness_20261122_$commit"
  parent=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$root/.codex_work/mti_skewness_extension_latest.json")
  [[ -d "$launch" ]] || git worktree add --detach "$launch" "$commit"
  [[ "$output" != "$parent" && "$output" != "$parent/"* ]] || exit 65
  mkdir -p "$output" "$launch/application/cache"
  chmod 700 "$output"
  source_library="${MTI_GIBBS_SKEWNESS_LIBRARY:-$(pwd)/application/cache/gibbs_skewness_library}"
  [[ -f "$(dirname "$source_library")/gibbs_skewness_runtime.json" ]] || { printf 'Prepare the isolated Gibbs library first.\n' >&2; exit 65; }
  if [[ ! -d "$launch/application/cache/gibbs_skewness_library" ]]; then
    cp -a "$source_library" "$launch/application/cache/gibbs_skewness_library"
    cp -p "$(dirname "$source_library")/gibbs_skewness_runtime.json" "$launch/application/cache/gibbs_skewness_runtime.json"
  fi
  # Reuse content-tested numerical checks only when all recorded source hashes match.
  checks="$(pwd)/application/outputs/mti_gibbs_skewness_checks_20261122/prelaunch_checks.json"
  [[ ! -f "$checks" || -f "$output/prelaunch_checks.json" ]] || cp -p "$checks" "$output/prelaunch_checks.json"
fi
[[ "$(git --no-optional-locks -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
[[ -z "$(git --no-optional-locks -C "$launch" status --porcelain)" ]] || exit 65
flock -n "$output/controller.lock" true || exit 73
export MTI_GIBBS_SKEWNESS_LIBRARY="$launch/application/cache/gibbs_skewness_library"
export MTI_GIBBS_SKEWNESS_OUTPUT_ROOT="$output" MTI_GIBBS_SKEWNESS_PARENT="$parent"
Rscript -e 'a<-commandArgs(TRUE);tmp<-paste0(a[1],".tmp");jsonlite::write_json(list(study_id="mti_gibbs_skewness_20261122",checkout=a[2],output_root=a[3],source_commit=a[4],parent=a[5]),tmp,pretty=TRUE,auto_unbox=TRUE);stopifnot(file.rename(tmp,a[1]))' "$pointer" "$launch" "$output" "$commit" "$parent"
setsid --fork bash "$launch/application/scripts/149_launch_mti_gibbs_skewness.sh" --foreground \
  >> "$output/controller.log" 2>&1 < /dev/null 7>&-
printf 'Gibbs skewness extension launched.\nSource: %s\nOutput: %s\nWorker ceiling: 40\n' "$commit" "$output"
