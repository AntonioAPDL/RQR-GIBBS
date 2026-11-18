#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
unset MTI_JOINT_OUTPUT_ROOT
foreground=false
case "${1:-}" in
  --foreground) foreground=true ;;
  "") ;;
  *) printf 'Usage: %s [--foreground]\n' "$0" >&2; exit 64 ;;
esac
commit=$(git rev-parse HEAD)
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit study source before scheduling.\n' >&2; exit 65; }
common=$(realpath "$(git rev-parse --git-common-dir)")
root=$(dirname "$common")
output=application/outputs/mti_gibbs_joint_20261117
export MTI_PRIOR_R_LIBRARY="$root/.codex_work/mti_fixed_prior_9b53485fc9e4d6ab43a92d80a1b0931bf58c910e/application/cache/fixed_prior_library"
if "$foreground"; then
  exec 9>"$output/controller.lock"
  flock -n 9 || { printf 'The joint-study controller is already running.\n' >&2; exit 73; }
  rm -f "$output/controller_exit.json"
  printf '%s\n' "$$" > "$output/controller.pid"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json.tmp"; mv "$output/controller_exit.json.tmp" "$output/controller_exit.json"; exit "$code"' EXIT
  predecessor=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$output/dependency.json")
  while true; do
    state=$(Rscript application/scripts/133_assess_mti_gibbs_joint.R dependency-state \
      "$predecessor" "$output/scheduler_status.json")
    case "$state" in
      ready)
        if flock -n "$predecessor/controller.lock" true; then break; fi ;;
      dependency_failed) printf 'Predecessor did not complete successfully; no study started.\n' >&2; exit 75 ;;
    esac
    sleep 300
  done
  Rscript application/scripts/131_assess_mti_gibbs_decisions.R verify "$predecessor"
  # Respect other users' activity as well as the forty-worker ceiling.
  free=$(awk -v cores="$(getconf _NPROCESSORS_ONLN)" '{n=int(cores-$1); print (n>0?n:1)}' /proc/loadavg)
  export MTI_JOINT_WORKERS=$(( free < 40 ? free : 40 ))
  Rscript application/scripts/133_assess_mti_gibbs_joint.R prepare
  Rscript application/scripts/133_assess_mti_gibbs_joint.R check
  Rscript application/scripts/133_assess_mti_gibbs_joint.R pipeline
  exit 0
fi
exec 7>"$root/.codex_work/mti_gibbs_joint_launch.lock"
flock -n 7 || { printf 'Another joint study is being scheduled.\n' >&2; exit 73; }
pointer="$root/.codex_work/mti_gibbs_joint_latest.json"
if [[ -f "$pointer" ]]; then
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$pointer")
  if [[ -f "$previous/controller.lock" ]]; then
    flock -n "$previous/controller.lock" true || { printf 'A joint study is already scheduled or running.\n' >&2; exit 73; }
  fi
fi
launch="$root/.codex_work/mti_gibbs_joint_$commit"
storage="${MTI_JOINT_STORAGE_ROOT:-/var/tmp/jaguir26-mti-studies}/mti_gibbs_joint_$commit"
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$storage" "$launch/application/outputs"
chmod 700 "$(dirname "$storage")" "$storage"
if [[ ! -e "$launch/$output" ]]; then ln -s "$storage" "$launch/$output"; fi
[[ "$(realpath "$launch/$output")" == "$(realpath "$storage")" ]] || exit 65
exec 8>"$storage/launch.lock"
flock -n 8 || exit 73
flock -n "$storage/controller.lock" true || { printf 'The controller is already running.\n' >&2; exit 73; }
Rscript -e '
  a<-commandArgs(TRUE); p<-jsonlite::read_json(file.path(a[1],".codex_work/mti_gibbs_decision_latest.json"));
  path<-file.path(p$output_root,"frozen_selection.rds"); stopifnot(file.exists(path));
  p$selection_sha256<-digest::digest(file=path,algo="sha256",serialize=FALSE);
  dependency<-file.path(a[3],"dependency.json");
  if(file.exists(dependency)) stopifnot(identical(jsonlite::read_json(dependency),p)) else jsonlite::write_json(p,dependency,auto_unbox=TRUE,pretty=TRUE);
  target<-file.path(a[1],".codex_work/mti_gibbs_joint_latest.json"); tmp<-paste0(target,".tmp");
  jsonlite::write_json(list(study_id="mti_gibbs_joint_20261117",checkout=a[2],output_root=a[3],source_commit=a[4]),tmp,auto_unbox=TRUE,pretty=TRUE); stopifnot(file.rename(tmp,target))' \
  "$root" "$launch" "$storage" "$commit"
setsid --fork bash "$launch/application/scripts/134_launch_mti_gibbs_joint.sh" --foreground \
  >> "$storage/controller.log" 2>&1 < /dev/null 8>&- 7>&-
printf 'Scheduled after verified predecessor completion.\nSource: %s\nCheckout: %s\nOutputs: %s\nWorker ceiling: 40\n' "$commit" "$launch" "$storage"
