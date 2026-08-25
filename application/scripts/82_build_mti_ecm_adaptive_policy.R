#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/82_build_mti_ecm_adaptive_policy.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

for (package in c("rqrgibbs", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}

default_results <- file.path(
  "application", "runs",
  "rqr_bayes_uq_validation_mti_ecm_dp_profile_stage2_tuning_20260823",
  "wave_confirmatory_mti_ecm_dp_profile_stage2_20260823T230135Z",
  "bayes_uq_validation_results.csv"
)
results_path <- normalizePath(arg_value("--results=", default_results),
                              winslash = "/", mustWork = TRUE)
output_path <- normalizePath(arg_value(
  "--output=",
  file.path("application", "config",
            "mti_ecm_adaptive_cell_policy_20260824.csv")
), winslash = "/", mustWork = FALSE)
policy_id <- arg_value("--policy-id=", "mti_ecm_adaptive_cell")
selection <- tolower(arg_value("--selection=", "cell"))
bound_method <- arg_value("--bound-method=", "clopper_pearson")
bound_confidence <- as.numeric(arg_value("--bound-confidence=", "0.95"))[1L]
margin <- as.numeric(arg_value("--margin=", "0"))[1L]
fallback_method_id <- arg_value("--fallback-method-id=", "")

if (!selection %in% c("cell", "global", "oracle")) {
  stopf("--selection must be one of cell, global, or oracle.")
}
if (!is.finite(bound_confidence) ||
    bound_confidence <= 0 || bound_confidence >= 1) {
  stopf("--bound-confidence must be in (0, 1).")
}
if (!is.finite(margin) || margin < 0) {
  stopf("--margin must be nonnegative.")
}

truthy <- function(x) {
  if (is.logical(x)) return(x %in% TRUE)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes")
}
num <- function(x) suppressWarnings(as.numeric(x))
median_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}
mean_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}
quantile_or_na <- function(x, p) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) as.numeric(stats::quantile(x, p, names = FALSE, type = 8))
  else NA_real_
}
cell_key <- function(n, content, tolerance_confidence) {
  sprintf(
    "n%04d_c%s_t%s",
    as.integer(n),
    gsub("\\.", "", sprintf("%.3f", as.numeric(content))),
    gsub("\\.", "", sprintf("%.3f", as.numeric(tolerance_confidence)))
  )
}

results <- utils::read.csv(results_path, stringsAsFactors = FALSE)
required <- c("method_id", "dgp_id", "n", "guaranteed_content",
              "tolerance_confidence", "replication", "success", "infeasible",
              "width")
missing <- setdiff(required, names(results))
if (length(missing)) {
  stopf("Results input is missing required column(s): ",
        paste(missing, collapse = ", "))
}
results <- results[grepl("^mti_ecm_dp_profile_tune_", results$method_id), ,
                   drop = FALSE]
if (!nrow(results)) stopf("No MTI-ECM tuning rows found in results input.")

results$success_bool <- truthy(results$success)
results$infeasible_bool <- truthy(results$infeasible)
results$delivered <- !results$infeasible_bool & results$success_bool
results$width_num <- num(results$width)
results$screen <- if ("effective_posterior_confidence" %in% names(results)) {
  num(results$effective_posterior_confidence)
} else {
  NA_real_
}
results$cell_key <- cell_key(
  results$n, results$guaranteed_content, results$tolerance_confidence
)

group_key <- paste(results$method_id, results$dgp_id, results$cell_key, sep = "\r")
cell_rows <- lapply(split(results, group_key), function(df) {
  successes <- sum(df$delivered, na.rm = TRUE)
  reps <- nrow(df)
  target <- df$tolerance_confidence[[1L]]
  lower <- rqrgibbs::rqr_delivery_lower_bound(
    successes = successes,
    replications = reps,
    confidence = bound_confidence,
    method = bound_method
  )
  data.frame(
    method_id = df$method_id[[1L]],
    dgp_id = df$dgp_id[[1L]],
    n = as.integer(df$n[[1L]]),
    content = as.numeric(df$guaranteed_content[[1L]]),
    tolerance_confidence = as.numeric(target),
    cell_key = df$cell_key[[1L]],
    replications = as.integer(reps),
    successes = as.integer(successes),
    observed_delivery = successes / reps,
    delivery_lower_bound = lower,
    admissible = is.finite(lower) && lower >= target + margin,
    median_width = median_or_na(df$width_num),
    mean_width = mean_or_na(df$width_num),
    width_q025 = quantile_or_na(df$width_num, 0.025),
    width_q975 = quantile_or_na(df$width_num, 0.975),
    screen = mean_or_na(df$screen),
    stringsAsFactors = FALSE
  )
})
cell_summary <- do.call(rbind, cell_rows)

choose_rule <- function(df, scope_label) {
  method_rows <- lapply(split(df, df$method_id), function(mm) {
    data.frame(
      source_method_id = mm$method_id[[1L]],
      scope = scope_label,
      dgp_cells = nrow(mm),
      calibration_replications = sum(mm$replications),
      calibration_successes = sum(mm$successes),
      min_observed_delivery = min(mm$observed_delivery, na.rm = TRUE),
      min_delivery_lower_bound = min(mm$delivery_lower_bound, na.rm = TRUE),
      all_cells_admissible = all(mm$admissible),
      median_cell_width = median_or_na(mm$median_width),
      mean_cell_width = mean_or_na(mm$mean_width),
      width_q025_min = min(mm$width_q025, na.rm = TRUE),
      width_q975_max = max(mm$width_q975, na.rm = TRUE),
      screen = mean_or_na(mm$screen),
      stringsAsFactors = FALSE
    )
  })
  candidates <- do.call(rbind, method_rows)
  admissible <- candidates[candidates$all_cells_admissible, , drop = FALSE]
  if (nrow(admissible)) {
    admissible <- admissible[order(admissible$median_cell_width,
                                   admissible$mean_cell_width,
                                   admissible$source_method_id), ,
                             drop = FALSE]
    out <- admissible[1L, , drop = FALSE]
    out$decision <- "selected_by_configured_bound"
    return(out)
  }
  if (nzchar(fallback_method_id) &&
      fallback_method_id %in% candidates$source_method_id) {
    out <- candidates[candidates$source_method_id == fallback_method_id,
                      , drop = FALSE][1L, , drop = FALSE]
    out$decision <- "fallback_unresolved_by_configured_bound"
    return(out)
  }
  candidates <- candidates[order(
    -candidates$min_delivery_lower_bound,
    candidates$median_cell_width,
    candidates$source_method_id
  ), , drop = FALSE]
  out <- candidates[1L, , drop = FALSE]
  out$decision <- "best_effort_unresolved_by_configured_bound"
  out
}

if (identical(selection, "global")) {
  selected <- choose_rule(cell_summary, "global")
  keys <- unique(cell_summary[, c("n", "content", "tolerance_confidence",
                                  "cell_key"), drop = FALSE])
  out <- cbind(
    data.frame(policy_id = policy_id, method_id = "mti_ecm_adaptive_global",
               keys, stringsAsFactors = FALSE),
    selected[rep(1L, nrow(keys)), , drop = FALSE]
  )
} else if (identical(selection, "oracle")) {
  split_key <- paste(cell_summary$dgp_id, cell_summary$cell_key, sep = "\r")
  out <- do.call(rbind, lapply(split(cell_summary, split_key), function(df) {
    selected <- choose_rule(df, "oracle_dgp_cell")
    cbind(
      data.frame(policy_id = policy_id,
                 method_id = "mti_ecm_adaptive_oracle_diagnostic",
                 dgp_id = df$dgp_id[[1L]],
                 n = df$n[[1L]],
                 content = df$content[[1L]],
                 tolerance_confidence = df$tolerance_confidence[[1L]],
                 cell_key = df$cell_key[[1L]],
                 stringsAsFactors = FALSE),
      selected
    )
  }))
} else {
  out <- do.call(rbind, lapply(split(cell_summary, cell_summary$cell_key),
                               function(df) {
    selected <- choose_rule(df, "cell")
    cbind(
      data.frame(policy_id = policy_id,
                 method_id = "mti_ecm_adaptive_cell",
                 n = df$n[[1L]],
                 content = df$content[[1L]],
                 tolerance_confidence = df$tolerance_confidence[[1L]],
                 cell_key = df$cell_key[[1L]],
                 stringsAsFactors = FALSE),
      selected
    )
  }))
}
out$bound_method <- bound_method
out$bound_confidence <- bound_confidence
out$delivery_margin <- margin
out$input_results_path <- results_path
out$input_results_digest <- digest::digest(results_path, algo = "sha256",
                                           file = TRUE)
out$created_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
out <- out[order(out$n, out$content, out$tolerance_confidence,
                 out$source_method_id), , drop = FALSE]
rownames(out) <- NULL

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, output_path, row.names = FALSE)
cat("Wrote adaptive MTI-ECM policy table\n")
cat("  output=", output_path, "\n", sep = "")
cat("  rows=", nrow(out), "\n", sep = "")
cat("  bound_method=", bound_method, "\n", sep = "")
cat("  bound_confidence=", bound_confidence, "\n", sep = "")
cat("  margin=", margin, "\n", sep = "")
