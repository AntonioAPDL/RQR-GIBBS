#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
foreground=false
for argument in "$@"; do
  case "$argument" in
    --foreground) foreground=true ;;
    *) printf 'Unknown argument: %s\n' "$argument" >&2; exit 64 ;;
  esac
done
commit=$(git rev-parse HEAD)
[[ -z "$(git status --porcelain)" ]] || { printf 'Commit study source before launch.\n' >&2; exit 65; }
common=$(realpath "$(git rev-parse --git-common-dir)")
root=$(dirname "$common")
output=application/outputs/mti_gibbs_decision_calibration_20261116
export MTI_PRIOR_R_LIBRARY="$root/.codex_work/mti_fixed_prior_9b53485fc9e4d6ab43a92d80a1b0931bf58c910e/application/cache/fixed_prior_library"
if "$foreground"; then
  exec 9>"$output/controller.lock"
  flock -n 9 || { printf 'The study controller is already running.\n' >&2; exit 73; }
  rm -f "$output/controller_exit.json"
  printf '%s\n' "$$" > "$output/controller.pid"
  trap 'code=$?; trap - EXIT; printf "{\"exit_code\":%d,\"finished_utc\":\"%s\"}\n" "$code" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > "$output/controller_exit.json"; exit "$code"' EXIT
  Rscript application/scripts/131_assess_mti_gibbs_decisions.R prepare
  Rscript application/scripts/131_assess_mti_gibbs_decisions.R check
  Rscript application/scripts/131_assess_mti_gibbs_decisions.R pipeline
  exit 0
fi
launch="$root/.codex_work/mti_gibbs_decision_$commit"
storage="${MTI_DECISION_STORAGE_ROOT:-/var/tmp/jaguir26-mti-studies}/mti_gibbs_decision_$commit"
exec 7>"$root/.codex_work/mti_gibbs_decision_launch.lock"
flock -n 7 || { printf 'Another decision-study launch is being prepared.\n' >&2; exit 73; }
pointer="$root/.codex_work/mti_gibbs_decision_latest.json"
if [[ -f "$pointer" ]]; then
  previous=$(Rscript -e 'cat(jsonlite::read_json(commandArgs(TRUE)[1])$output_root)' "$pointer")
  if [[ -f "$previous/controller.lock" ]]; then
    flock -n "$previous/controller.lock" true || { printf 'A previous decision study is still running.\n' >&2; exit 73; }
  fi
fi
if [[ ! -d "$launch" ]]; then git worktree add --detach "$launch" "$commit"; fi
[[ "$(git -C "$launch" rev-parse HEAD)" == "$commit" ]] || exit 65
mkdir -p "$storage" "$launch/application/outputs"
chmod 700 "$(dirname "$storage")" "$storage"
if [[ ! -e "$launch/$output" ]]; then ln -s "$storage" "$launch/$output"; fi
[[ "$(realpath "$launch/$output")" == "$(realpath "$storage")" ]] || { printf 'Output location differs.\n' >&2; exit 65; }
exec 8>"$storage/launch.lock"
flock -n 8 || { printf 'Another launch is being prepared.\n' >&2; exit 73; }
flock -n "$storage/controller.lock" true || { printf 'The controller is already running.\n' >&2; exit 73; }
Rscript -e 'a<-commandArgs(TRUE); p<-file.path(a[1],".codex_work/mti_gibbs_decision_latest.json"); t<-paste0(p,".tmp"); jsonlite::write_json(list(study_id="mti_gibbs_decision_calibration_20261116",checkout=a[2],output_root=a[3],source_commit=a[4]),t,auto_unbox=TRUE,pretty=TRUE); stopifnot(file.rename(t,p))' \
  "$root" "$launch" "$storage" "$commit"
setsid --fork bash "$launch/application/scripts/132_launch_mti_gibbs_decisions.sh" --foreground \
  >> "$storage/controller.log" 2>&1 < /dev/null 8>&- 7>&-
printf 'Source: %s\nFrozen checkout: %s\nOutputs: %s\nMaximum workers: 40\n' "$commit" "$launch" "$storage"
