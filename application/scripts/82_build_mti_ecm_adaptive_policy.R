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

for (package in c("rqrgibbs", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}

default_results <- file.path(
  "application", "runs",
  "rqr_bayes_uq_validation_mti_ecm_adaptive_calibration_20260824",
  "wave_confirmatory_20260825T005037Z",
  "bayes_uq_validation_results.csv"
)
default_output <- file.path(
  "application", "config",
  "mti_ecm_adaptive_cell_policy_20260825.csv"
)
results_path <- normalizePath(arg_value("--results=", default_results),
                              winslash = "/", mustWork = TRUE)
output_path <- normalizePath(arg_value("--output=", default_output),
                             winslash = "/", mustWork = FALSE)
diagnostics_output <- arg_value("--diagnostics-output=", "")
if (!nzchar(diagnostics_output)) {
  diagnostics_output <- sub("\\.csv$", "_diagnostics.csv", output_path)
}
diagnostics_output <- normalizePath(diagnostics_output, winslash = "/",
                                    mustWork = FALSE)
policy_id <- arg_value("--policy-id=", "mti_ecm_adaptive_cell_pooled_20260825")
selection <- tolower(gsub("_", "-", arg_value("--selection=", "pooled-cell")))
if (identical(selection, "cell-pooled")) selection <- "pooled-cell"
if (identical(selection, "pooled")) selection <- "pooled-cell"
bound_method <- arg_value("--bound-method=", "clopper_pearson")
bound_confidence <- as.numeric(arg_value("--bound-confidence=", "0.95"))[1L]
margin <- as.numeric(arg_value("--margin=", "0"))[1L]
fallback_method_id <- arg_value("--fallback-method-id=", "")
method_pattern <- arg_value(
  "--method-pattern=",
  "^(mti_ecm_dp_profile_tune_|mti_ecm_adaptive_screen_)"
)

if (!selection %in% c("pooled-cell", "cell", "global", "oracle")) {
  stopf("--selection must be one of pooled-cell, cell, global, or oracle.")
}
if (!is.finite(bound_confidence) ||
    bound_confidence <= 0 || bound_confidence >= 1) {
  stopf("--bound-confidence must be in (0, 1).")
}
if (!is.finite(margin) || margin < 0) {
  stopf("--margin must be nonnegative.")
}
if (!nzchar(method_pattern)) {
  stopf("--method-pattern must be a nonempty regular expression.")
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
min_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) min(x) else NA_real_
}
integer_min_or_na <- function(x) {
  x <- as.integer(x)
  x <- x[!is.na(x)]
  if (length(x)) min(x) else NA_integer_
}
cell_key <- function(n, content, tolerance_confidence) {
  sprintf(
    "n%04d_c%s_t%s",
    as.integer(n),
    gsub("\\.", "", sprintf("%.3f", as.numeric(content))),
    gsub("\\.", "", sprintf("%.3f", as.numeric(tolerance_confidence)))
  )
}
bind_fill <- function(frames) {
  frames <- Filter(function(x) is.data.frame(x) && nrow(x), frames)
  if (!length(frames)) return(data.frame())
  cols <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(df) {
    missing <- setdiff(cols, names(df))
    for (col in missing) df[[col]] <- NA
    df[, cols, drop = FALSE]
  })
  do.call(rbind, frames)
}
repo_relative_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(repo_root, "/")
  if (startsWith(path, prefix)) {
    substring(path, nchar(prefix) + 1L)
  } else {
    path
  }
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
results <- results[grepl(method_pattern, results$method_id), , drop = FALSE]
if (!nrow(results)) {
  stopf("No adaptive MTI-ECM rows matched --method-pattern: ", method_pattern)
}

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

summarise_raw <- function(df, scope, dgp_id = NA_character_) {
  successes <- sum(df$delivered, na.rm = TRUE)
  reps <- nrow(df)
  target <- as.numeric(df$tolerance_confidence[[1L]])
  lower <- rqrgibbs::rqr_delivery_lower_bound(
    successes = successes,
    replications = reps,
    confidence = bound_confidence,
    method = bound_method
  )
  required_successes <- rqrgibbs::rqr_delivery_min_successes(
    replications = reps,
    target = target,
    confidence = bound_confidence,
    method = bound_method,
    margin = margin
  )
  success_margin <- if (is.na(required_successes)) {
    NA_integer_
  } else {
    as.integer(successes - required_successes)
  }
  width <- df$width_num[!df$infeasible_bool & is.finite(df$width_num)]
  data.frame(
    source_method_id = df$method_id[[1L]],
    calibration_scope = scope,
    dgp_id = dgp_id,
    n = as.integer(df$n[[1L]]),
    content = as.numeric(df$guaranteed_content[[1L]]),
    tolerance_confidence = as.numeric(target),
    delivery_target = as.numeric(target),
    cell_key = df$cell_key[[1L]],
    dgp_cells = length(unique(df$dgp_id)),
    passing_dgp_cells = NA_integer_,
    calibration_replications = as.integer(reps),
    calibration_successes = as.integer(successes),
    minimum_successes_required = as.integer(required_successes),
    success_margin_to_requirement = success_margin,
    observed_delivery = successes / reps,
    delivery_lower_bound = lower,
    min_observed_delivery = NA_real_,
    min_delivery_lower_bound = NA_real_,
    min_success_margin_to_requirement = NA_integer_,
    all_dgp_cells_admissible = NA,
    admissible = is.finite(lower) && lower >= target + margin,
    mean_width = mean_or_na(width),
    median_width = median_or_na(width),
    width_q025 = quantile_or_na(width, 0.025),
    width_q975 = quantile_or_na(width, 0.975),
    screen = mean_or_na(df$screen),
    posterior_confidence = mean_or_na(df$screen),
    stringsAsFactors = FALSE
  )
}

split_summary <- function(df, keys, scope, dgp = FALSE) {
  split_key <- do.call(paste, c(df[keys], sep = "\r"))
  rows <- lapply(split(df, split_key), function(dd) {
    summarise_raw(
      dd,
      scope = scope,
      dgp_id = if (isTRUE(dgp)) dd$dgp_id[[1L]] else NA_character_
    )
  })
  do.call(rbind, rows)
}

dgp_cell_summary <- split_summary(
  results, c("method_id", "dgp_id", "cell_key"),
  scope = "distribution_cell",
  dgp = TRUE
)
pooled_cell_summary <- split_summary(
  results, c("method_id", "cell_key"),
  scope = "pooled_cell",
  dgp = FALSE
)
global_summary <- split_summary(
  results, "method_id",
  scope = "global",
  dgp = FALSE
)
global_summary$n <- NA_integer_
global_summary$content <- NA_real_
global_summary$tolerance_confidence <- NA_real_
global_summary$cell_key <- NA_character_

all_dgp_cell_candidates <- do.call(rbind, lapply(
  split(dgp_cell_summary, paste(dgp_cell_summary$source_method_id,
                                dgp_cell_summary$cell_key, sep = "\r")),
  function(mm) {
    raw <- results[
      results$method_id == mm$source_method_id[[1L]] &
        results$cell_key == mm$cell_key[[1L]],
      ,
      drop = FALSE
    ]
    pooled <- summarise_raw(raw, scope = "all_distribution_cell")
    pooled$passing_dgp_cells <- sum(mm$admissible, na.rm = TRUE)
    pooled$min_observed_delivery <- min_or_na(mm$observed_delivery)
    pooled$min_delivery_lower_bound <- min_or_na(mm$delivery_lower_bound)
    pooled$min_success_margin_to_requirement <-
      integer_min_or_na(mm$success_margin_to_requirement)
    pooled$all_dgp_cells_admissible <- all(mm$admissible)
    pooled$admissible <- pooled$all_dgp_cells_admissible
    pooled
  }
))

choose_candidate <- function(candidates) {
  candidates <- candidates[is.finite(candidates$screen), , drop = FALSE]
  if (!nrow(candidates)) {
    stopf("No candidate rows with finite screen values were available.")
  }
  mean_width_order <- function(df) {
    ifelse(is.finite(df$mean_width), df$mean_width, Inf)
  }
  median_width_order <- function(df) {
    ifelse(is.finite(df$median_width), df$median_width, Inf)
  }
  admissible <- candidates[candidates$admissible, , drop = FALSE]
  if (nrow(admissible)) {
    admissible <- admissible[order(
      mean_width_order(admissible),
      median_width_order(admissible),
      admissible$source_method_id
    ), , drop = FALSE]
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
  lower_score <- candidates$delivery_lower_bound
  if ("min_delivery_lower_bound" %in% names(candidates)) {
    lower_score <- ifelse(is.finite(candidates$min_delivery_lower_bound),
                          candidates$min_delivery_lower_bound,
                          candidates$delivery_lower_bound)
  }
  candidates <- candidates[order(
    -lower_score,
    mean_width_order(candidates),
    median_width_order(candidates),
    candidates$source_method_id
  ), , drop = FALSE]
  out <- candidates[1L, , drop = FALSE]
  out$decision <- "best_effort_unresolved_by_configured_bound"
  out
}

policy_method_id <- switch(
  selection,
  "pooled-cell" = "mti_ecm_adaptive_cell",
  "cell" = "mti_ecm_adaptive_cell",
  "global" = "mti_ecm_adaptive_global",
  "oracle" = "mti_ecm_adaptive_oracle_diagnostic"
)

if (identical(selection, "global")) {
  selected <- choose_candidate(global_summary)
  keys <- unique(results[, c("n", "guaranteed_content",
                             "tolerance_confidence", "cell_key"), drop = FALSE])
  names(keys)[names(keys) == "guaranteed_content"] <- "content"
  out <- selected[rep(1L, nrow(keys)), , drop = FALSE]
  out$n <- as.integer(keys$n)
  out$content <- as.numeric(keys$content)
  out$tolerance_confidence <- as.numeric(keys$tolerance_confidence)
  out$cell_key <- keys$cell_key
} else if (identical(selection, "oracle")) {
  split_key <- paste(dgp_cell_summary$dgp_id, dgp_cell_summary$cell_key,
                     sep = "\r")
  out <- do.call(rbind, lapply(split(dgp_cell_summary, split_key),
                               choose_candidate))
} else if (identical(selection, "cell")) {
  out <- do.call(rbind, lapply(split(all_dgp_cell_candidates,
                                     all_dgp_cell_candidates$cell_key),
                               choose_candidate))
} else {
  out <- do.call(rbind, lapply(split(pooled_cell_summary,
                                     pooled_cell_summary$cell_key),
                               choose_candidate))
}

out$policy_id <- policy_id
out$method_id <- policy_method_id
out$selection_rule <- selection
out$bound_method <- bound_method
out$bound_confidence <- bound_confidence
out$delivery_margin <- margin
out$input_results_path <- repo_relative_path(results_path)
out$input_results_digest <- digest::digest(results_path, algo = "sha256",
                                           file = TRUE)
out$created_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

front_cols <- c(
  "policy_id", "method_id", "source_method_id", "selection_rule",
  "calibration_scope", "decision", "n", "content", "tolerance_confidence",
  "delivery_target", "cell_key", "screen", "posterior_confidence",
  "calibration_replications", "calibration_successes",
  "minimum_successes_required", "success_margin_to_requirement",
  "observed_delivery", "delivery_lower_bound",
  "admissible", "mean_width", "median_width", "width_q025", "width_q975",
  "dgp_cells", "passing_dgp_cells", "min_observed_delivery",
  "min_delivery_lower_bound", "min_success_margin_to_requirement",
  "all_dgp_cells_admissible"
)
front_cols <- front_cols[front_cols %in% names(out)]
out <- out[, c(front_cols, setdiff(names(out), front_cols)), drop = FALSE]
out <- out[order(out$n, out$content, out$tolerance_confidence,
                 out$source_method_id), , drop = FALSE]
rownames(out) <- NULL

diagnostics <- bind_fill(list(
  pooled_cell_summary,
  all_dgp_cell_candidates,
  dgp_cell_summary,
  global_summary
))
diagnostics$selection_rule <- selection
diagnostics$bound_method <- bound_method
diagnostics$bound_confidence <- bound_confidence
diagnostics$delivery_margin <- margin
diagnostics$input_results_digest <- out$input_results_digest[[1L]]
diagnostics <- diagnostics[order(
  diagnostics$calibration_scope,
  diagnostics$n,
  diagnostics$content,
  diagnostics$tolerance_confidence,
  diagnostics$dgp_id,
  diagnostics$source_method_id
), , drop = FALSE]
rownames(diagnostics) <- NULL

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(diagnostics_output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, output_path, row.names = FALSE)
utils::write.csv(diagnostics, diagnostics_output, row.names = FALSE)

cat("Wrote adaptive MTI-ECM policy table\n")
cat("  output=", output_path, "\n", sep = "")
cat("  diagnostics_output=", diagnostics_output, "\n", sep = "")
cat("  rows=", nrow(out), "\n", sep = "")
cat("  selection=", selection, "\n", sep = "")
cat("  method_pattern=", method_pattern, "\n", sep = "")
cat("  bound_method=", bound_method, "\n", sep = "")
cat("  bound_confidence=", bound_confidence, "\n", sep = "")
cat("  margin=", margin, "\n", sep = "")
