#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
repo_root="$(cd "$script_dir/../.." && pwd -P)"
cd "$repo_root"

config=""
output_dir=""
acceptance_only=FALSE
for argument in "$@"; do
  case "$argument" in
    --config=*) config="${argument#--config=}" ;;
    --output-dir=*) output_dir="${argument#--output-dir=}" ;;
    --acceptance-only) acceptance_only=TRUE ;;
    *)
      echo "Unknown process-isolated orchestrator argument: $argument" >&2
      exit 2
      ;;
  esac
done
if [[ "$acceptance_only" == TRUE &&
      "${RQR_ORACLE_TILT_V3_ACCEPTANCE_CONFIRM:-}" != YES ]]; then
  echo "Acceptance execution is fail-closed." >&2
  exit 2
fi
if [[ -z "$config" || -z "$output_dir" ]]; then
  echo "--config and --output-dir are required." >&2
  exit 2
fi
if [[ ! -f "$config" || ! -d "$output_dir" ]]; then
  echo "The configuration or output directory is unavailable." >&2
  exit 2
fi

runner="application/scripts/42_run_oracle_tilt_publication_v3.R"
if [[ ! -f "$runner" ]]; then
  echo "Run the orchestrator from the RQR-GIBBS repository root." >&2
  exit 2
fi

lifecycle="$output_dir/process_lifecycle.csv"
failure="$output_dir/orchestrator_failure_log.csv"
current_stage="$output_dir/current_stage.csv"
if [[ ! -f "$lifecycle" ]]; then
  printf '%s\n' \
    "stage,family,target,child_pid,started_at_utc,finished_at_utc,elapsed_seconds,exit_status,post_stage_r_processes,pass" \
    >"$lifecycle"
fi

group_id="$(ps -o pgid= -p $$ | awk '{print $1}')"
if [[ ! "$group_id" =~ ^[0-9]+$ ]]; then
  echo "Could not determine the orchestrator process group." >&2
  exit 2
fi

post_stage_r_processes() {
  ps -eo pgid=,stat=,comm= | awk -v group="$group_id" '
    $1 == group && $2 !~ /^Z/ && $3 == "R" { count += 1 }
    END { print count + 0 }
  '
}

record_row() {
  local row="$1"
  local temporary="${lifecycle}.tmp.$$"
  cp "$lifecycle" "$temporary"
  printf '%s\n' "$row" >>"$temporary"
  mv -T "$temporary" "$lifecycle"
}

record_failure() {
  local stage="$1"
  local family="$2"
  local target="$3"
  local status="$4"
  printf '%s\n' \
    "recorded_at_utc,stage,family,target,exit_status,message" \
    >"$failure"
  printf '%s,%s,%s,%s,%s,%s\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$stage" "$family" "$target" \
    "$status" "process-isolated stage failed; later cells were not launched" \
    >>"$failure"
}

write_current_stage() {
  local stage="$1"
  local family="$2"
  local target="$3"
  local child_pid="$4"
  local status="$5"
  local temporary="${current_stage}.tmp.$$"
  printf '%s\n' "updated_at_utc,stage,family,target,child_pid,status" \
    >"$temporary"
  printf '%s,%s,%s,%s,%s,%s\n' \
    "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$stage" "$family" "$target" \
    "$child_pid" "$status" >>"$temporary"
  mv -T "$temporary" "$current_stage"
}

run_stage() {
  local stage="$1"
  local family="${2:-}"
  local target="${3:-}"
  local started_epoch started_utc child_pid status finished_epoch finished_utc
  local elapsed remaining_r pass args
  started_epoch="$(date +%s)"
  started_utc="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  args=(
    "$runner" "--mode=execute" "--execute-stage=$stage"
    "--config=$config" "--output-dir=$output_dir"
  )
  if [[ "$stage" == cell ]]; then
    args+=("--family=$family" "--target=$target")
  fi
  set +e
  Rscript "${args[@]}" &
  child_pid=$!
  write_current_stage "$stage" "$family" "$target" "$child_pid" "running"
  wait "$child_pid"
  status=$?
  set -e
  finished_epoch="$(date +%s)"
  finished_utc="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  elapsed=$((finished_epoch - started_epoch))
  remaining_r="$(post_stage_r_processes)"
  pass=TRUE
  if (( status != 0 || remaining_r != 0 )); then pass=FALSE; fi
  record_row \
    "$stage,$family,$target,$child_pid,$started_utc,$finished_utc,$elapsed,$status,$remaining_r,$pass"
  write_current_stage "$stage" "$family" "$target" "$child_pid" \
    "$([[ "$pass" == TRUE ]] && printf completed || printf failed)"
  if [[ "$pass" != TRUE ]]; then
    record_failure "$stage" "$family" "$target" "$status"
    return 1
  fi
}

run_stage prepare
cell_plan="$output_dir/cell_plan.csv"
if [[ ! -f "$cell_plan" ]]; then
  echo "The prepare stage did not publish cell_plan.csv." >&2
  exit 1
fi
mapfile -t planned_cells < <(
  Rscript --vanilla - "$cell_plan" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
plan <- utils::read.csv(args[[1L]], stringsAsFactors = FALSE)
required <- c(
  "family", "target", "order", "cell_key", "process_isolation",
  "maximum_chain_workers"
)
valid <- nrow(plan) == 6L && all(required %in% names(plan)) &&
  identical(as.integer(plan$order), 1:6) &&
  !anyDuplicated(paste(plan$family, plan$target, sep = "/")) &&
  all(plan$process_isolation) && all(plan$maximum_chain_workers == 2L)
if (!valid) quit(save = "no", status = 1L)
for (index in seq_len(nrow(plan))) {
  cat(plan$family[index], plan$target[index], sep = "\t")
  cat("\n")
}
RSCRIPT
)
if [[ "${#planned_cells[@]}" -ne 6 ]]; then
  echo "The authoritative cell plan is invalid." >&2
  exit 1
fi
completed_plan_cells=0
for planned_cell in "${planned_cells[@]}"; do
  IFS=$'\t' read -r family target <<<"$planned_cell"
  run_stage cell "$family" "$target"
  completed_plan_cells=$((completed_plan_cells + 1))
  if [[ "$acceptance_only" == TRUE && "$completed_plan_cells" -eq 1 ]]; then
    break
  fi
done
if [[ "$acceptance_only" == TRUE ]]; then
  Rscript --vanilla - "$output_dir" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1L]], mustWork = TRUE)
status <- utils::read.csv(
  file.path(root, "run_status.csv"), stringsAsFactors = FALSE
)
receipt_path <- file.path(root, "cells", "fixed_design_rqr", "cell_receipt.json")
if (!requireNamespace("jsonlite", quietly = TRUE) ||
    !file.exists(receipt_path)) quit(save = "no", status = 1L)
receipt <- jsonlite::read_json(receipt_path, simplifyVector = TRUE)
pass <- nrow(status) == 6L && status$status[1L] == "completed" &&
  all(status$status[-1L] == "pending") && isTRUE(receipt$eligible) &&
  identical(receipt$family, "fixed_design") && identical(receipt$target, "RQR")
out <- data.frame(
  schema_version = "rqrgibbs_oracle_tilt_publication_acceptance/1.0.0",
  completed_cells = sum(status$status == "completed"),
  completed_chains = sum(status$chains_completed),
  accepted_family = receipt$family, accepted_target = receipt$target,
  eligible = isTRUE(receipt$eligible), pass = pass,
  finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  stringsAsFactors = FALSE
)
write.csv(out, file.path(root, "acceptance_closeout.csv"), row.names = FALSE)
if (!pass) quit(save = "no", status = 1L)
RSCRIPT
  write_current_stage acceptance "fixed_design" "RQR" "0" "completed"
  echo "[oracle-tilt-v3-orchestrator] monitored acceptance cell passed"
  exit 0
fi
run_stage finalize
write_current_stage complete "" "" "0" "completed"

echo "[oracle-tilt-v3-orchestrator] all six isolated cells passed"
