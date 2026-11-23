#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
unset MTI_ECM_NOMINAL_OUTPUT_ROOT
commit=$(git rev-parse HEAD)
common=$(realpath "$(git rev-parse --git-common-dir)")
root=$(dirname "$common")
output=application/outputs/mti_ecm_nominal_20261119
export MTI_PRIOR_R_LIBRARY="$root/.codex_work/mti_fixed_prior_9b53485fc9e4d6ab43a92d80a1b0931bf58c910e/application/cache/fixed_prior_library"
if [[ "${1:-}" == --foreground ]]; then
  exec 9>"$output/controller.lock"
  flock -n 9 || exit 73
  rm -f "$output/controller_exit.json"
  printf '%s\n' "$$" > "$output/controller.pid"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json.tmp"; mv "$output/controller_exit.json.tmp" "$output/controller_exit.json"; exit "$code"' EXIT
  free=$(awk -v cores="$(getconf _NPROCESSORS_ONLN)" '{n=int(cores-$1); print (n>0?n:1)}' /proc/loadavg)
  export MTI_ECM_NOMINAL_WORKERS=$(( free < 40 ? free : 40 ))
  Rscript application/scripts/136_assess_mti_ecm_nominal.R prepare
  Rscript application/scripts/136_assess_mti_ecm_nominal.R check
  Rscript application/scripts/136_assess_mti_ecm_nominal.R pipeline
  exit 0
fi
[[ $# == 0 ]] || { printf 'Usage: %s [--foreground]\n' "$0" >&2; exit 64; }
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit source before launching.\n' >&2; exit 65; }
exec 7>"$root/.codex_work/mti_ecm_nominal_launch.lock"
flock -n 7 || exit 73
pointer="$root/.codex_work/mti_ecm_nominal_latest.json"
if [[ -f "$pointer" ]]; then
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$pointer")
  flock -n "$previous/controller.lock" true || { printf 'The exploration is already running.\n' >&2; exit 73; }
fi
launch="$root/.codex_work/mti_ecm_nominal_$commit"
storage="${MTI_ECM_NOMINAL_STORAGE_ROOT:-/var/tmp/jaguir26-mti-studies}/mti_ecm_nominal_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$storage" "$launch/application/outputs"
chmod 700 "$storage"
if [[ ! -e "$launch/$output" ]]; then ln -s "$storage" "$launch/$output"; fi
[[ "$(realpath "$launch/$output")" == "$(realpath "$storage")" ]] || exit 65
exec 8>"$storage/launch.lock"
flock -n 8 || exit 73
flock -n "$storage/controller.lock" true || exit 73
Rscript -e '
 a<-commandArgs(TRUE); p<-jsonlite::read_json(file.path(a[1],".codex_work/mti_gibbs_joint_latest.json"));
 p$manifest_sha256<-digest::digest(file=file.path(p$output_root,"manifest.json"),algo="sha256",serialize=FALSE);
 stopifnot(p$source_commit=="6a67d41f507b7fe234c208c0d718ab3b76146615");
 file<-file.path(a[3],"dependency.json");
 if(file.exists(file))stopifnot(identical(jsonlite::read_json(file),p)) else jsonlite::write_json(p,file,auto_unbox=TRUE,pretty=TRUE);
 target<-file.path(a[1],".codex_work/mti_ecm_nominal_latest.json");tmp<-paste0(target,".tmp");
 jsonlite::write_json(list(study_id="mti_ecm_nominal_20261119",checkout=a[2],output_root=a[3],source_commit=a[4]),tmp,auto_unbox=TRUE,pretty=TRUE);
 stopifnot(file.rename(tmp,target))' "$root" "$launch" "$storage" "$commit"
setsid --fork bash "$launch/application/scripts/137_launch_mti_ecm_nominal.sh" --foreground \
  >> "$storage/controller.log" 2>&1 < /dev/null 8>&- 7>&-
printf 'ECM-only exploration launched.\nSource: %s\nOutputs: %s\nWorker ceiling: 40\n' "$commit" "$storage"
