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
optional_numeric_arg <- function(prefix, default) {
  value <- arg_value(prefix, NULL)
  if (is.null(value) || !nzchar(value)) return(default)
  value_lower <- tolower(value)
  if (value_lower %in% c("inf", "+inf", "infinity", "+infinity")) {
    return(Inf)
  }
  if (value_lower %in% c("-inf", "-infinity")) return(-Inf)
  out <- suppressWarnings(as.numeric(value)[1L])
  if (is.na(out)) {
    stopf("Argument ", prefix, " must be numeric or Inf; got: ", value)
  }
  out
}

for (package in c("rqrgibbs", "digest", "data.table")) {
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
reference_results_arg <- arg_value("--reference-results=", "")
reference_method_id <- arg_value("--reference-method-id=", "tcsp_mc")
width_objective <- tolower(gsub(
  "_", "-", arg_value("--width-objective=", "mean-median")
))
max_infeasible_rate <- optional_numeric_arg("--max-infeasible-rate=", Inf)
max_width_q975_to_median <- optional_numeric_arg(
  "--max-width-q975-to-median=", Inf
)
max_mean_width_ratio_to_median <- optional_numeric_arg(
  "--max-mean-width-ratio-to-median=", Inf
)
max_width_q975_to_reference_q975 <- optional_numeric_arg(
  "--max-width-q975-to-reference-q975=", Inf
)
max_median_width_to_reference_median <- optional_numeric_arg(
  "--max-median-width-to-reference-median=", Inf
)
min_mean_candidate_feasible_count <- optional_numeric_arg(
  "--min-mean-candidate-feasible-count=", -Inf
)
min_min_candidate_feasible_count <- optional_numeric_arg(
  "--min-min-candidate-feasible-count=", -Inf
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
if (!width_objective %in% c("mean-median", "median-q975")) {
  stopf("--width-objective must be one of mean-median or median-q975.")
}
if (!is.infinite(max_infeasible_rate) &&
    (!is.finite(max_infeasible_rate) ||
     max_infeasible_rate < 0 || max_infeasible_rate > 1)) {
  stopf("--max-infeasible-rate must be in [0, 1] or Inf.")
}
if (!is.infinite(max_width_q975_to_median) &&
    (!is.finite(max_width_q975_to_median) ||
     max_width_q975_to_median < 1)) {
  stopf("--max-width-q975-to-median must be at least 1 or Inf.")
}
if (!is.infinite(max_mean_width_ratio_to_median) &&
    (!is.finite(max_mean_width_ratio_to_median) ||
     max_mean_width_ratio_to_median <= 0)) {
  stopf("--max-mean-width-ratio-to-median must be positive or Inf.")
}
if (!is.infinite(max_width_q975_to_reference_q975) &&
    (!is.finite(max_width_q975_to_reference_q975) ||
     max_width_q975_to_reference_q975 < 1)) {
  stopf("--max-width-q975-to-reference-q975 must be at least 1 or Inf.")
}
if (!is.infinite(max_median_width_to_reference_median) &&
    (!is.finite(max_median_width_to_reference_median) ||
     max_median_width_to_reference_median < 1)) {
  stopf("--max-median-width-to-reference-median must be at least 1 or Inf.")
}
if (!is.infinite(min_mean_candidate_feasible_count) &&
    (!is.finite(min_mean_candidate_feasible_count) ||
     min_mean_candidate_feasible_count < 0)) {
  stopf("--min-mean-candidate-feasible-count must be nonnegative or -Inf.")
}
if (!is.infinite(min_min_candidate_feasible_count) &&
    (!is.finite(min_min_candidate_feasible_count) ||
     min_min_candidate_feasible_count < 0)) {
  stopf("--min-min-candidate-feasible-count must be nonnegative or -Inf.")
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
max_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) max(x) else NA_real_
}
integer_min_or_na <- function(x) {
  x <- as.integer(x)
  x <- x[!is.na(x)]
  if (length(x)) min(x) else NA_integer_
}
safe_ratio <- function(numerator, denominator) {
  if (is.finite(numerator) && is.finite(denominator) && denominator > 0) {
    numerator / denominator
  } else {
    NA_real_
  }
}
pass_upper <- function(value, limit) {
  is.infinite(limit) || (is.finite(value) && value <= limit)
}
pass_lower <- function(value, limit) {
  is.infinite(limit) && limit < 0 || (is.finite(value) && value >= limit)
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

required <- c("method_id", "dgp_id", "n", "guaranteed_content",
              "tolerance_confidence", "replication", "success", "infeasible",
              "width")
results_header <- names(data.table::fread(
  results_path, nrows = 0L, showProgress = FALSE
))
results_columns <- intersect(
  unique(c(required, "effective_posterior_confidence",
           "candidate_feasible_count")),
  results_header
)
results <- as.data.frame(data.table::fread(
  results_path, select = results_columns, showProgress = FALSE
))
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

pass_upper_vec <- function(value, limit) {
  if (is.infinite(limit)) rep(TRUE, length(value))
  else is.finite(value) & value <= limit
}
pass_lower_vec <- function(value, limit) {
  if (is.infinite(limit) && limit < 0) rep(TRUE, length(value))
  else is.finite(value) & value >= limit
}
safe_ratio_vec <- function(numerator, denominator) {
  out <- rep(NA_real_, length(numerator))
  ok <- is.finite(numerator) & is.finite(denominator) & denominator > 0
  out[ok] <- numerator[ok] / denominator[ok]
  out
}
max_finite <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) max(x) else NA_real_
}
max_median_width_reference_limit <- max_median_width_to_reference_median
max_width_q975_reference_limit <- max_width_q975_to_reference_q975
delivery_min_successes_fast <- function(replications, target) {
  replications <- as.integer(replications)
  target <- as.numeric(target)
  threshold <- target + margin
  if (!is.finite(threshold) || threshold >= 1) return(NA_integer_)
  if (rqrgibbs::rqr_delivery_lower_bound(
    replications, replications,
    confidence = bound_confidence,
    method = bound_method
  ) < threshold) {
    return(NA_integer_)
  }
  lo <- 0L
  hi <- replications
  while (lo < hi) {
    mid <- as.integer(floor((lo + hi) / 2L))
    lower <- rqrgibbs::rqr_delivery_lower_bound(
      mid, replications,
      confidence = bound_confidence,
      method = bound_method
    )
    if (lower >= threshold) hi <- mid else lo <- mid + 1L
  }
  as.integer(lo)
}

results_dt <- data.table::as.data.table(results)
if ("candidate_feasible_count" %in% names(results_dt)) {
  results_dt[, candidate_feasible_count_num := num(candidate_feasible_count)]
} else {
  results_dt[, candidate_feasible_count_num := NA_real_]
}

reference_results_path <- ""
reference_results_digest <- NA_character_
reference_cell_summary <- data.table::data.table()
reference_constraints_active <- FALSE
reference_method_label <- reference_method_id
if (nzchar(reference_results_arg)) {
  reference_results_path <- normalizePath(
    reference_results_arg, winslash = "/", mustWork = TRUE
  )
  reference_results_digest <- digest::digest(
    reference_results_path, algo = "sha256", file = TRUE
  )
  reference_required <- c(
    "method_id", "dgp_id", "n", "guaranteed_content",
    "tolerance_confidence", "replication", "infeasible", "width"
  )
  reference_header <- names(data.table::fread(
    reference_results_path, nrows = 0L, showProgress = FALSE
  ))
  reference_columns <- intersect(reference_required, reference_header)
  reference <- data.table::fread(
    reference_results_path, select = reference_columns, showProgress = FALSE
  )
  reference_missing <- setdiff(reference_required, names(reference))
  if (length(reference_missing)) {
    stopf("Reference results input is missing required column(s): ",
          paste(reference_missing, collapse = ", "))
  }
  reference <- reference[method_id == reference_method_label]
  if (!nrow(reference)) {
    stopf("No reference rows matched --reference-method-id: ",
          reference_method_label)
  }
  reference[, `:=`(
    n = as.integer(n),
    content = num(guaranteed_content),
    tolerance_confidence = num(tolerance_confidence),
    width_num = num(width),
    infeasible_bool = truthy(infeasible)
  )]
  reference_cell_summary <- reference[, {
    width <- width_num[!infeasible_bool & is.finite(width_num)]
    .(
      reference_method_id = reference_method_label,
      reference_replications = as.integer(.N),
      reference_width_count = as.integer(length(width)),
      reference_median_width = median_or_na(width),
      reference_width_q975 = quantile_or_na(width, 0.975)
    )
  }, by = .(dgp_id, n, content, tolerance_confidence)]
  reference_constraints_active <-
    !is.infinite(max_median_width_to_reference_median) ||
    !is.infinite(max_width_q975_to_reference_q975)
}

summarise_grouped <- function(dt, by, scope) {
  out <- dt[, {
    width <- width_num[!infeasible_bool & is.finite(width_num)]
    feasible_counts <- candidate_feasible_count_num
    .(
      calibration_scope = scope,
      n = as.integer(n[[1L]]),
      content = as.numeric(guaranteed_content[[1L]]),
      tolerance_confidence = as.numeric(tolerance_confidence[[1L]]),
      delivery_target = as.numeric(tolerance_confidence[[1L]]),
      cell_key_value = cell_key[[1L]],
      dgp_cells = as.integer(data.table::uniqueN(dgp_id)),
      passing_dgp_cells = NA_integer_,
      calibration_replications = as.integer(.N),
      calibration_successes = as.integer(sum(delivered, na.rm = TRUE)),
      observed_delivery = mean(delivered, na.rm = TRUE),
      min_observed_delivery = NA_real_,
      min_delivery_lower_bound = NA_real_,
      min_success_margin_to_requirement = NA_integer_,
      all_dgp_cells_admissible = NA,
      infeasible_rate = mean(infeasible_bool, na.rm = TRUE),
      max_infeasible_rate = max_infeasible_rate,
      mean_candidate_feasible_count = mean_or_na(feasible_counts),
      min_candidate_feasible_count = min_or_na(feasible_counts),
      max_candidate_feasible_count = max_or_na(feasible_counts),
      min_mean_candidate_feasible_count =
        min_mean_candidate_feasible_count,
      min_min_candidate_feasible_count =
        min_min_candidate_feasible_count,
      mean_width = mean_or_na(width),
      median_width = median_or_na(width),
      width_q025 = quantile_or_na(width, 0.025),
      width_q975 = quantile_or_na(width, 0.975),
      screen = mean_or_na(screen),
      posterior_confidence = mean_or_na(screen)
    )
  }, by = by]
  if ("method_id" %in% names(out)) {
    data.table::setnames(out, "method_id", "source_method_id")
  }
  if ("cell_key" %in% names(out)) {
    out[, cell_key_value := NULL]
  } else {
    data.table::setnames(out, "cell_key_value", "cell_key")
  }
  if (!"dgp_id" %in% names(out)) out[, dgp_id := NA_character_]
  out
}

add_reference_defaults <- function(out) {
  out[, `:=`(
    reference_method_id = NA_character_,
    reference_cells = 0L,
    reference_missing_cells = 0L,
    reference_width_cells = 0L,
    observed_max_median_width_to_reference_median = NA_real_,
    max_median_width_to_reference_median =
      max_median_width_to_reference_median,
    median_width_to_reference_median_pass = TRUE,
    observed_max_width_q975_to_reference_q975 = NA_real_,
    max_width_q975_to_reference_q975 =
      max_width_q975_to_reference_q975,
    width_q975_to_reference_q975_pass = TRUE,
    reference_missing_pass = TRUE
  )]
  out
}

add_reference_to_distribution <- function(out) {
  if (!nrow(reference_cell_summary)) return(add_reference_defaults(out))
  out <- merge(
    out,
    reference_cell_summary,
    by = c("dgp_id", "n", "content", "tolerance_confidence"),
    all.x = TRUE,
    sort = FALSE
  )
  out[, reference_missing := is.na(reference_width_count)]
  out[, `:=`(
    reference_method_id = ifelse(reference_missing, NA_character_,
                                 reference_method_id),
    reference_cells = 1L,
    reference_missing_cells = as.integer(reference_missing),
    reference_width_cells = as.integer(
      !reference_missing &
        is.finite(reference_median_width) &
        is.finite(reference_width_q975)
    ),
    observed_max_median_width_to_reference_median = safe_ratio_vec(
      median_width, reference_median_width
    ),
    max_median_width_to_reference_median =
      max_median_width_to_reference_median,
    observed_max_width_q975_to_reference_q975 = safe_ratio_vec(
      width_q975, reference_width_q975
    ),
    max_width_q975_to_reference_q975 =
      max_width_q975_to_reference_q975
  )]
  out
}

reference_rollup <- function(distribution_summary, by) {
  if (!nrow(reference_cell_summary)) return(data.table::data.table())
  distribution_summary[, .(
    reference_method_id = reference_method_id[[1L]],
    reference_cells = as.integer(.N),
    reference_missing_cells = as.integer(sum(reference_missing_cells > 0L)),
    reference_width_cells = as.integer(sum(reference_width_cells > 0L)),
    observed_max_median_width_to_reference_median =
      max_finite(observed_max_median_width_to_reference_median),
    max_median_width_to_reference_median =
      max_median_width_reference_limit,
    observed_max_width_q975_to_reference_q975 =
      max_finite(observed_max_width_q975_to_reference_q975),
    max_width_q975_to_reference_q975 =
      max_width_q975_reference_limit
  ), by = by]
}

add_reference_from_rollup <- function(out, rollup, by) {
  if (!nrow(reference_cell_summary)) return(add_reference_defaults(out))
  out <- merge(out, rollup, by = by, all.x = TRUE, sort = FALSE)
  missing_reference_row <- is.na(out$reference_cells)
  out[missing_reference_row, `:=`(
    reference_method_id = NA_character_,
    reference_cells = 0L,
    reference_missing_cells = 1L,
    reference_width_cells = 0L,
    observed_max_median_width_to_reference_median = NA_real_,
    observed_max_width_q975_to_reference_q975 = NA_real_
  )]
  out
}

finish_summary <- function(out) {
  out$minimum_successes_required <- as.integer(mapply(
    delivery_min_successes_fast,
    replications = out$calibration_replications,
    target = out$tolerance_confidence
  ))
  out$delivery_lower_bound <- as.numeric(mapply(
    rqrgibbs::rqr_delivery_lower_bound,
    successes = out$calibration_successes,
    replications = out$calibration_replications,
    MoreArgs = list(confidence = bound_confidence, method = bound_method)
  ))
  out$success_margin_to_requirement <-
    as.integer(out$calibration_successes - out$minimum_successes_required)
  out$admissible_before_stability <-
    is.finite(out$delivery_lower_bound) &
    out$delivery_lower_bound >= out$tolerance_confidence + margin
  out$infeasible_rate[!is.finite(out$infeasible_rate)] <- NA_real_
  out$infeasible_rate_pass <- pass_upper_vec(
    out$infeasible_rate, max_infeasible_rate
  )
  out$width_q975_to_median <- safe_ratio_vec(out$width_q975,
                                             out$median_width)
  out$width_q975_to_median_pass <- pass_upper_vec(
    out$width_q975_to_median, max_width_q975_to_median
  )
  out$max_width_q975_to_median <- max_width_q975_to_median
  out$mean_width_to_median_width <- safe_ratio_vec(out$mean_width,
                                                   out$median_width)
  out$mean_width_ratio_pass <- pass_upper_vec(
    out$mean_width_to_median_width, max_mean_width_ratio_to_median
  )
  out$max_mean_width_ratio_to_median <- max_mean_width_ratio_to_median
  out$reference_missing_pass <-
    !reference_constraints_active | out$reference_missing_cells == 0L
  out$median_width_to_reference_median_pass <-
    !reference_constraints_active |
    pass_upper_vec(
      out$observed_max_median_width_to_reference_median,
      max_median_width_to_reference_median
    )
  out$width_q975_to_reference_q975_pass <-
    !reference_constraints_active |
    pass_upper_vec(
      out$observed_max_width_q975_to_reference_q975,
      max_width_q975_to_reference_q975
    )
  out$mean_candidate_feasible_count_pass <- pass_lower_vec(
    out$mean_candidate_feasible_count, min_mean_candidate_feasible_count
  )
  out$min_candidate_feasible_count_pass <- pass_lower_vec(
    out$min_candidate_feasible_count, min_min_candidate_feasible_count
  )
  out$candidate_feasible_count_pass <-
    out$mean_candidate_feasible_count_pass &
    out$min_candidate_feasible_count_pass
  out$width_stability_pass <-
    out$width_q975_to_median_pass &
    out$mean_width_ratio_pass &
    out$reference_missing_pass &
    out$median_width_to_reference_median_pass &
    out$width_q975_to_reference_q975_pass
  out$admissible <-
    out$admissible_before_stability &
    out$infeasible_rate_pass &
    out$candidate_feasible_count_pass &
    out$width_stability_pass
  as.data.frame(out)
}

dgp_cell_summary <- summarise_grouped(
  results_dt,
  by = c("method_id", "dgp_id", "cell_key"),
  scope = "distribution_cell"
)
dgp_cell_summary <- add_reference_to_distribution(dgp_cell_summary)
dgp_cell_summary <- finish_summary(dgp_cell_summary)

pooled_cell_summary <- summarise_grouped(
  results_dt,
  by = c("method_id", "cell_key"),
  scope = "pooled_cell"
)
pooled_reference <- reference_rollup(
  data.table::as.data.table(dgp_cell_summary),
  by = c("source_method_id", "cell_key")
)
pooled_cell_summary <- add_reference_from_rollup(
  pooled_cell_summary, pooled_reference,
  by = c("source_method_id", "cell_key")
)
pooled_cell_summary <- finish_summary(pooled_cell_summary)

global_summary <- summarise_grouped(
  results_dt,
  by = "method_id",
  scope = "global"
)
global_reference <- reference_rollup(
  data.table::as.data.table(dgp_cell_summary),
  by = "source_method_id"
)
global_summary <- add_reference_from_rollup(
  global_summary, global_reference,
  by = "source_method_id"
)
global_summary <- finish_summary(global_summary)
global_summary$n <- NA_integer_
global_summary$content <- NA_real_
global_summary$tolerance_confidence <- NA_real_
global_summary$cell_key <- NA_character_

all_dgp_cell_candidates <- as.data.frame(pooled_cell_summary)
all_dgp_cell_candidates$calibration_scope <- "all_distribution_cell"
distribution_status <- data.table::as.data.table(dgp_cell_summary)[, .(
  passing_dgp_cells = as.integer(sum(admissible, na.rm = TRUE)),
  min_observed_delivery = min_or_na(observed_delivery),
  min_delivery_lower_bound = min_or_na(delivery_lower_bound),
  min_success_margin_to_requirement =
    integer_min_or_na(success_margin_to_requirement),
  all_dgp_cells_admissible = all(admissible)
), by = .(source_method_id, cell_key)]
all_dgp_cell_candidates <- merge(
  all_dgp_cell_candidates,
  as.data.frame(distribution_status),
  by = c("source_method_id", "cell_key"),
  all.x = TRUE,
  suffixes = c("", ".distribution"),
  sort = FALSE
)
for (col in c("passing_dgp_cells", "min_observed_delivery",
              "min_delivery_lower_bound", "min_success_margin_to_requirement",
              "all_dgp_cells_admissible")) {
  replacement <- paste0(col, ".distribution")
  if (replacement %in% names(all_dgp_cell_candidates)) {
    all_dgp_cell_candidates[[col]] <- all_dgp_cell_candidates[[replacement]]
    all_dgp_cell_candidates[[replacement]] <- NULL
  }
}
all_dgp_cell_candidates$admissible <-
  all_dgp_cell_candidates$all_dgp_cells_admissible &
  all_dgp_cell_candidates$admissible_before_stability &
  all_dgp_cell_candidates$infeasible_rate_pass &
  all_dgp_cell_candidates$candidate_feasible_count_pass &
  all_dgp_cell_candidates$width_stability_pass

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
  width_q975_order <- function(df) {
    ifelse(is.finite(df$width_q975), df$width_q975, Inf)
  }
  admissible <- candidates[candidates$admissible, , drop = FALSE]
  if (nrow(admissible)) {
    if (identical(width_objective, "median-q975")) {
      admissible <- admissible[order(
        median_width_order(admissible),
        width_q975_order(admissible),
        mean_width_order(admissible),
        admissible$source_method_id
      ), , drop = FALSE]
    } else {
      admissible <- admissible[order(
        mean_width_order(admissible),
        median_width_order(admissible),
        width_q975_order(admissible),
        admissible$source_method_id
      ), , drop = FALSE]
    }
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
    width_q975_order(candidates),
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
out$width_objective <- width_objective
out$max_infeasible_rate_configured <- max_infeasible_rate
out$max_width_q975_to_median_configured <- max_width_q975_to_median
out$max_mean_width_ratio_to_median_configured <-
  max_mean_width_ratio_to_median
out$max_median_width_to_reference_median_configured <-
  max_median_width_to_reference_median
out$max_width_q975_to_reference_q975_configured <-
  max_width_q975_to_reference_q975
out$min_mean_candidate_feasible_count_configured <-
  min_mean_candidate_feasible_count
out$min_min_candidate_feasible_count_configured <-
  min_min_candidate_feasible_count
out$input_results_path <- repo_relative_path(results_path)
out$input_results_digest <- digest::digest(results_path, algo = "sha256",
                                           file = TRUE)
out$reference_results_path <- if (nzchar(reference_results_path)) {
  repo_relative_path(reference_results_path)
} else {
  NA_character_
}
out$reference_results_digest <- reference_results_digest
out$created_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

front_cols <- c(
  "policy_id", "method_id", "source_method_id", "selection_rule",
  "calibration_scope", "decision", "n", "content", "tolerance_confidence",
  "delivery_target", "cell_key", "screen", "posterior_confidence",
  "calibration_replications", "calibration_successes",
  "minimum_successes_required", "success_margin_to_requirement",
  "observed_delivery", "delivery_lower_bound",
  "admissible_before_stability", "admissible",
  "infeasible_rate", "infeasible_rate_pass",
  "candidate_feasible_count_pass", "width_stability_pass",
  "mean_width", "median_width", "width_q025", "width_q975",
  "width_q975_to_median", "mean_width_to_median_width",
  "reference_method_id", "reference_cells", "reference_missing_cells",
  "observed_max_median_width_to_reference_median",
  "observed_max_width_q975_to_reference_q975",
  "median_width_to_reference_median_pass",
  "width_q975_to_reference_q975_pass",
  "dgp_cells", "passing_dgp_cells", "min_observed_delivery",
  "min_delivery_lower_bound", "min_success_margin_to_requirement",
  "all_dgp_cells_admissible", "width_objective"
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
diagnostics$width_objective <- width_objective
diagnostics$max_infeasible_rate_configured <- max_infeasible_rate
diagnostics$max_width_q975_to_median_configured <- max_width_q975_to_median
diagnostics$max_mean_width_ratio_to_median_configured <-
  max_mean_width_ratio_to_median
diagnostics$max_median_width_to_reference_median_configured <-
  max_median_width_to_reference_median
diagnostics$max_width_q975_to_reference_q975_configured <-
  max_width_q975_to_reference_q975
diagnostics$min_mean_candidate_feasible_count_configured <-
  min_mean_candidate_feasible_count
diagnostics$min_min_candidate_feasible_count_configured <-
  min_min_candidate_feasible_count
diagnostics$input_results_digest <- out$input_results_digest[[1L]]
diagnostics$reference_results_path <- out$reference_results_path[[1L]]
diagnostics$reference_results_digest <- out$reference_results_digest[[1L]]
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
cat("  width_objective=", width_objective, "\n", sep = "")
cat("  max_infeasible_rate=", max_infeasible_rate, "\n", sep = "")
cat("  max_width_q975_to_median=", max_width_q975_to_median, "\n", sep = "")
cat("  max_mean_width_ratio_to_median=", max_mean_width_ratio_to_median,
    "\n", sep = "")
cat("  reference_method_id=", reference_method_id, "\n", sep = "")
cat("  reference_results=",
    if (nzchar(reference_results_path)) reference_results_path else "",
    "\n", sep = "")
cat("  max_median_width_to_reference_median=",
    max_median_width_to_reference_median, "\n", sep = "")
cat("  max_width_q975_to_reference_q975=",
    max_width_q975_to_reference_q975, "\n", sep = "")
cat("  min_mean_candidate_feasible_count=",
    min_mean_candidate_feasible_count, "\n", sep = "")
cat("  min_min_candidate_feasible_count=",
    min_min_candidate_feasible_count, "\n", sep = "")
