#!/usr/bin/env bash
set -euo pipefail

unit=""
output_dir=""
for argument in "$@"; do
  case "$argument" in
    --unit=*) unit="${argument#--unit=}" ;;
    --output-dir=*) output_dir="${argument#--output-dir=}" ;;
    *) echo "Unknown V4 health argument: $argument" >&2; exit 2 ;;
  esac
done
if [[ -z "$output_dir" || ! -d "$output_dir" ]]; then
  echo "--output-dir must identify an existing V4 output directory." >&2
  exit 2
fi

active_state=not_supplied
sub_state=not_supplied
result=not_supplied
if [[ -n "$unit" ]] && command -v systemctl >/dev/null 2>&1; then
  active_state="$(systemctl --user show "$unit.service" -p ActiveState --value 2>/dev/null || printf unknown)"
  sub_state="$(systemctl --user show "$unit.service" -p SubState --value 2>/dev/null || printf unknown)"
  result="$(systemctl --user show "$unit.service" -p Result --value 2>/dev/null || printf unavailable)"
fi

read -r completed_cells completed_chains failed_cells eligible_cells < <(
  Rscript --vanilla - "$output_dir" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
root <- args[[1L]]
status_path <- file.path(root, "run_status.csv")
cell_root <- file.path(root, "cells")
completed_cells <- 0L; completed_chains <- 0L; failed_cells <- 0L
eligible_cells <- 0L
if (file.exists(status_path)) {
  x <- tryCatch(utils::read.csv(status_path, stringsAsFactors = FALSE),
                error = function(error) data.frame())
  if (nrow(x)) {
    completed_cells <- sum(x$status == "completed", na.rm = TRUE)
    completed_chains <- sum(x$chains_completed, na.rm = TRUE)
    if ("selection_eligible" %in% names(x))
      eligible_cells <- sum(x$selection_eligible, na.rm = TRUE)
  }
}
if (dir.exists(cell_root)) {
  receipts <- list.files(cell_root, pattern = "cell_receipt\\.json$",
                         recursive = TRUE, full.names = TRUE)
  completed_cells <- max(completed_cells, length(receipts))
  if (length(receipts)) {
    values <- lapply(receipts, function(path) tryCatch(
      jsonlite::read_json(path, simplifyVector = TRUE),
      error = function(error) NULL
    ))
    values <- Filter(Negate(is.null), values)
    if (length(values)) {
      completed_chains <- max(completed_chains, sum(vapply(
        values, function(x) {
          value <- x$completed_chains
          if (is.null(value) || length(value) != 1L) value <- 0L
          as.integer(value)
        }, integer(1L)
      )))
      eligible_cells <- max(eligible_cells, sum(vapply(
        values, function(x) isTRUE(x$selection_eligible), logical(1L)
      )))
    }
  }
}
failed_cells <- length(list.files(
  root, pattern = "^(failure_|orchestrator_failure_log)", full.names = TRUE
))
cat(completed_cells, completed_chains, failed_cells, eligible_cells, "\n")
RSCRIPT
)

stage=unavailable
if [[ -f "$output_dir/current_stage.csv" ]]; then
  stage="$(awk -F, 'NR == 2 {gsub(/"/, "", $0); printf "%s (%s)", $3, $4}' \
    "$output_dir/current_stage.csv")"
fi
read -r elapsed processes r_processes rss threads < <(
  if [[ -f "$output_dir/process_group_monitor.csv" ]]; then
    awk -F, 'NR > 1 {e=$2;p=$3;r=$4;m=$5;t=$6}
      END {print e+0,p+0,r+0,m+0,t+0}' \
      "$output_dir/process_group_monitor.csv"
  else
    printf '0 0 0 0 0\n'
  fi
)

selected=0
if [[ -f "$output_dir/selected_candidates.csv" ]]; then
  selected="$(( $(wc -l <"$output_dir/selected_candidates.csv") - 1 ))"
fi
output_bytes="$(du -sb "$output_dir" 2>/dev/null | awk '{print $1}' || printf 0)"
free_bytes="$(df -Pk "$output_dir" | awk 'NR == 2 {printf "%.0f", $4 * 1024}')"
printf '%-30s %s\n' \
  service "$active_state/$sub_state ($result)" \
  stage "$stage" \
  completed_cells "$completed_cells/18" \
  completed_chains "$completed_chains/81" \
  remaining_cells "$((18 - completed_cells))" \
  remaining_chains "$((81 - completed_chains))" \
  selection_eligible_cells "$eligible_cells/18" \
  selected_family_winners "$selected/2" \
  failure_artifacts "$failed_cells" \
  elapsed_seconds "$elapsed" \
  sampled_processes "$processes" \
  sampled_R_processes "$r_processes" \
  sampled_rss_kib "$rss" \
  sampled_threads "$threads" \
  output_bytes "$output_bytes" \
  filesystem_free_bytes "$free_bytes"
