#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "tables/generate_tolerance_validation_scenario_tables.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

default_primary_dir <- file.path(
  "application", "runs", "rqr_bayes_uq_validation_main_20260813",
  "wave_main_20260813T103232Z"
)
default_primary_results <- file.path(default_primary_dir,
                                     "bayes_uq_validation_results.csv")
default_young_mathew_results <- file.path(
  default_primary_dir, "young_mathew_addon_20260815T064224Z",
  "bayes_uq_validation_results.csv"
)
default_small_results <- file.path(
  "application", "runs", "rqr_bayes_uq_followup_20260816",
  "wave_small_sample_95_20260817T005145Z",
  "bayes_uq_validation_results.csv"
)

primary_path <- arg_value(
  "--primary-results=",
  Sys.getenv("RQR_BAYES_UQ_PRIMARY_RESULTS", unset = default_primary_results)
)
young_mathew_path <- arg_value(
  "--young-mathew-results=",
  Sys.getenv("RQR_BAYES_UQ_PRIMARY_YM_RESULTS",
             unset = default_young_mathew_results)
)
small_path <- arg_value(
  "--small95-results=",
  Sys.getenv("RQR_BAYES_UQ_SMALL95_RESULTS", unset = default_small_results)
)
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

outputs <- c(
  "tolerance_validation_primary_scenario_ranges.csv",
  "tolerance_validation_primary_scenario_ranges.tex",
  "tolerance_validation_primary_scenario_ranges_n500.tex",
  "tolerance_validation_primary_scenario_ranges_n1000.tex",
  "tolerance_validation_small_sample_boundary.csv",
  "tolerance_validation_small_sample_boundary.tex",
  "tolerance_validation_primary_dgp_delivery.csv",
  "tolerance_validation_primary_dgp_delivery.tex",
  "tolerance_validation_small_sample_dgp_delivery.csv",
  "tolerance_validation_small_sample_dgp_delivery.tex",
  "tolerance_validation_primary_scenario_details.csv",
  "tolerance_validation_small_sample_scenario_details.csv"
)
committed_outputs <- file.path(repo_root, "tables", outputs)
target_outputs <- file.path(output_dir, outputs)

if (!file.exists(primary_path) || !file.exists(small_path)) {
  if (all(file.exists(committed_outputs))) {
    if (!identical(normalizePath(output_dir, winslash = "/", mustWork = FALSE),
                   normalizePath(file.path(repo_root, "tables"),
                                 winslash = "/", mustWork = TRUE))) {
      file.copy(committed_outputs, target_outputs, overwrite = TRUE)
    }
    cat("Using committed scenario-aware tolerance validation tables;",
        "provide raw result CSVs to regenerate.\n")
    quit(status = 0)
  }
  missing <- c(
    primary_results = primary_path,
    small95_results = small_path
  )
  missing <- missing[!file.exists(missing)]
  stopf("Missing scenario table input(s): ",
        paste(names(missing), missing, sep = "=", collapse = "; "))
}

method_order <- c(
  "tcsp_mc",
  "hdp_s_mc",
  "tcsp_mti_ecm_map_mc",
  "young_mathew",
  "wilks_minmax",
  "tcsp_dkw"
)
small_method_order <- c(
  "tcsp_mc",
  "tcsp_mti_ecm_map_mc",
  "young_mathew",
  "wilks_minmax",
  "tcsp_dkw"
)
method_labels <- c(
  tcsp_mc = "TCSP",
  hdp_s_mc = "Hybrid DP--scan",
  tcsp_mti_ecm_map_mc = "MTI ECM",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks",
  tcsp_dkw = "DKW"
)
dgp_labels <- c(
  normal = "Gaussian",
  lognormal = "log-normal",
  lognormal_hard = "hard log-normal",
  mixture = "mixture",
  sharp_mixture = "sharp mixture",
  contaminated_normal = "contaminated normal",
  student_t3 = "Student t3",
  beta_left = "left-skewed Beta"
)

required_columns <- c(
  "dgp_id", "n", "guaranteed_content", "tolerance_confidence",
  "replication", "method_id", "success", "infeasible", "width",
  "elapsed_sec"
)
read_results <- function(path, label) {
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  missing <- setdiff(required_columns, names(out))
  if (length(missing)) {
    stopf(label, " is missing column(s): ", paste(missing, collapse = ", "))
  }
  out
}
bind_fill <- function(...) {
  frames <- list(...)
  frames <- frames[vapply(frames, nrow, integer(1L)) > 0L]
  if (!length(frames)) return(data.frame())
  cols <- Reduce(union, lapply(frames, names))
  frames <- lapply(frames, function(x) {
    missing <- setdiff(cols, names(x))
    for (col in missing) x[[col]] <- NA
    x[, cols, drop = FALSE]
  })
  do.call(rbind, frames)
}
truthy <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(!is.na(x) & x != 0)
  x <- tolower(trimws(as.character(x)))
  !is.na(x) & x %in% c("true", "t", "1", "yes", "y")
}
num <- function(x) suppressWarnings(as.numeric(x))
median_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
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

add_tcsp_width_ratio <- function(results) {
  results$n <- as.integer(results$n)
  results$guaranteed_content <- num(results$guaranteed_content)
  results$tolerance_confidence <- num(results$tolerance_confidence)
  results$replication <- as.integer(results$replication)
  results$width <- num(results$width)
  key <- paste(results$dgp_id, results$n,
               sprintf("%.4f", results$guaranteed_content),
               sprintf("%.4f", results$tolerance_confidence),
               results$replication, sep = "||")
  tcsp <- results[results$method_id == "tcsp_mc", , drop = FALSE]
  tcsp_key <- paste(tcsp$dgp_id, tcsp$n,
                    sprintf("%.4f", tcsp$guaranteed_content),
                    sprintf("%.4f", tcsp$tolerance_confidence),
                    tcsp$replication, sep = "||")
  tcsp_width <- tcsp$width
  names(tcsp_width) <- tcsp_key
  results$tcsp_reference_width <- unname(tcsp_width[key])
  results$width_ratio_to_tcsp <- results$width / results$tcsp_reference_width
  results$width_ratio_to_tcsp[
    !is.finite(results$width_ratio_to_tcsp)
  ] <- NA_real_
  results
}

scenario_detail <- function(results, methods) {
  results <- results[results$method_id %in% methods, , drop = FALSE]
  results$infeasible_bool <- truthy(results$infeasible)
  results$success_bool <- truthy(results$success)
  results$elapsed_sec <- num(results$elapsed_sec)
  key <- paste(results$dgp_id, results$n,
               sprintf("%.4f", results$guaranteed_content),
               results$method_id, sep = "||")
  rows <- lapply(split(results, key), function(df) {
    returned <- !df$infeasible_bool & !is.na(df$success_bool)
    finite_width <- is.finite(df$width_ratio_to_tcsp)
    data.frame(
      dgp_id = df$dgp_id[[1L]],
      dgp = dgp_labels[df$dgp_id[[1L]]],
      n = as.integer(df$n[[1L]]),
      content = num(df$guaranteed_content[[1L]]),
      method_id = df$method_id[[1L]],
      method = method_labels[df$method_id[[1L]]],
      replications = nrow(df),
      infeasible_rate = mean(df$infeasible_bool),
      delivery_success = mean(!df$infeasible_bool & df$success_bool),
      returned_success = if (any(returned)) {
        mean(df$success_bool[returned])
      } else {
        NA_real_
      },
      median_width_ratio_to_tcsp = if (any(finite_width)) {
        stats::median(df$width_ratio_to_tcsp[finite_width])
      } else {
        NA_real_
      },
      median_elapsed_sec = median_or_na(df$elapsed_sec),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$dgp[is.na(out$dgp)] <- out$dgp_id[is.na(out$dgp)]
  out$method[is.na(out$method)] <- out$method_id[is.na(out$method)]
  out <- out[order(
    out$n, out$content, out$dgp,
    match(out$method_id, methods)
  ), ]
  rownames(out) <- NULL
  out
}

range_summary <- function(detail, methods) {
  key <- paste(detail$n, sprintf("%.4f", detail$content),
               detail$method_id, sep = "||")
  rows <- lapply(split(detail, key), function(df) {
    data.frame(
      n = as.integer(df$n[[1L]]),
      content = num(df$content[[1L]]),
      method_id = df$method_id[[1L]],
      method = df$method[[1L]],
      dgp_cells = nrow(df),
      fail_closed_dgp_cells = sum(df$infeasible_rate >= 1 - 1e-12),
      partial_infeasible_dgp_cells = sum(df$infeasible_rate > 1e-12 &
                                           df$infeasible_rate < 1 - 1e-12),
      delivery_min = min_or_na(df$delivery_success),
      delivery_max = max_or_na(df$delivery_success),
      returned_success_min = min_or_na(df$returned_success),
      returned_success_max = max_or_na(df$returned_success),
      median_width_ratio_to_tcsp =
        median_or_na(df$median_width_ratio_to_tcsp),
      median_elapsed_sec = median_or_na(df$median_elapsed_sec),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$n, out$content, match(out$method_id, methods)), ]
  rownames(out) <- NULL
  out
}

format_pct <- function(x) {
  x <- num(x)
  ifelse(is.finite(x), sprintf("%.1f", 100 * x), "--")
}
format_range <- function(a, b) {
  a <- num(a)
  b <- num(b)
  out <- rep("--", length(a))
  ok <- is.finite(a) & is.finite(b)
  same <- ok & abs(a - b) < 5e-4
  out[same] <- format_pct(a[same])
  out[ok & !same] <- paste0(format_pct(a[ok & !same]), "--",
                            format_pct(b[ok & !same]))
  out
}
format_num <- function(x, digits = 3) {
  x <- num(x)
  out <- ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), "--")
  sub("\\.?0+$", "", out)
}
format_sec <- function(x) {
  x <- num(x)
  ifelse(!is.finite(x), "--", ifelse(x < 0.01, sprintf("%.3f", x),
                                     format_num(x, 3)))
}
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}
cell_label <- function(n, content) {
  sprintf("\\(n=%s,c=%.2f\\)", as.integer(n), num(content))
}

write_range_tex <- function(summary, path) {
  lines <- c(
    "\\begin{tabularx}{\\textwidth}{@{}l>{\\raggedright\\arraybackslash}Xrrrrr@{}}",
    "\\toprule",
    "Cell & Method & DGPs & Fail-closed & Delivery range (\\%) & Returned range (\\%) & Width/TCSP\\\\",
    "\\midrule"
  )
  for (cell in unique(paste(summary$n, summary$content, sep = "||"))) {
    block <- summary[paste(summary$n, summary$content, sep = "||") == cell,
                     , drop = FALSE]
    for (ii in seq_len(nrow(block))) {
      lines <- c(lines, sprintf(
        "%s & %s & %s & %s & %s & %s & %s \\\\",
        if (ii == 1L) cell_label(block$n[[ii]], block$content[[ii]]) else "",
        escape_latex(block$method[[ii]]),
        block$dgp_cells[[ii]],
        block$fail_closed_dgp_cells[[ii]],
        format_range(block$delivery_min[[ii]], block$delivery_max[[ii]]),
        format_range(block$returned_success_min[[ii]],
                     block$returned_success_max[[ii]]),
        format_num(block$median_width_ratio_to_tcsp[[ii]], 3)
      ))
    }
    lines <- c(lines, "\\addlinespace[0.25em]")
  }
  lines <- lines[-length(lines)]
  writeLines(c(lines, "\\bottomrule", "\\end{tabularx}"), path)
}

wide_delivery <- function(detail, methods) {
  detail$delivery_pct <- format_pct(detail$delivery_success)
  scenarios <- unique(detail[, c("dgp", "n", "content"), drop = FALSE])
  scenarios <- scenarios[order(scenarios$n, scenarios$content, scenarios$dgp),
                         , drop = FALSE]
  out <- scenarios
  for (method in methods) {
    values <- rep("--", nrow(scenarios))
    label <- method_labels[method]
    for (ii in seq_len(nrow(scenarios))) {
      hit <- detail[
        detail$dgp == scenarios$dgp[[ii]] &
          detail$n == scenarios$n[[ii]] &
          abs(detail$content - scenarios$content[[ii]]) < 1e-12 &
          detail$method_id == method,
        ,
        drop = FALSE
      ]
      if (nrow(hit)) values[[ii]] <- hit$delivery_pct[[1L]]
    }
    out[[unname(label)]] <- values
  }
  out
}

write_wide_delivery_tex <- function(wide, path, methods, caption, label) {
  method_headers <- escape_latex(unname(method_labels[methods]))
  align <- paste0("@{}p{0.22\\textwidth}rr",
                  paste(rep("r", length(methods)), collapse = ""), "@{}")
  header <- c(
    paste(c("DGP", "\\(n\\)", "\\(c\\)", method_headers), collapse = " & "),
    "\\\\",
    "\\midrule"
  )
  lines <- c(
    sprintf("\\begin{longtable}{%s}", align),
    sprintf("\\caption{%s}\\label{%s}\\\\", caption, label),
    "\\toprule",
    header,
    "\\endfirsthead",
    "\\toprule",
    header,
    "\\endhead"
  )
  for (ii in seq_len(nrow(wide))) {
    vals <- unname(as.character(wide[ii, unname(method_labels[methods]),
                                       drop = TRUE]))
    lines <- c(lines, paste(c(
      escape_latex(wide$dgp[[ii]]),
      as.integer(wide$n[[ii]]),
      sprintf("%.2f", num(wide$content[[ii]])),
      vals
    ), collapse = " & "), "\\\\")
  }
  writeLines(c(lines, "\\bottomrule", "\\end{longtable}"), path)
}

primary <- read_results(primary_path, "Primary validation results")
if (file.exists(young_mathew_path)) {
  primary <- bind_fill(
    primary,
    read_results(young_mathew_path, "Young--Mathew add-on results")
  )
}
small <- read_results(small_path, "Small-sample validation results")

primary <- add_tcsp_width_ratio(primary)
small <- add_tcsp_width_ratio(small)

primary_detail <- scenario_detail(primary, method_order)
primary_range <- range_summary(primary_detail, method_order)
small_detail <- scenario_detail(small, small_method_order)
small_boundary_detail <- small_detail[
  small_detail$n %in% c(50L, 100L),
  ,
  drop = FALSE
]
small_boundary <- range_summary(small_boundary_detail, small_method_order)

primary_delivery <- wide_delivery(primary_detail, method_order)
small_delivery <- wide_delivery(small_boundary_detail, small_method_order)

utils::write.csv(primary_range,
                 file.path(output_dir,
                           "tolerance_validation_primary_scenario_ranges.csv"),
                 row.names = FALSE)
utils::write.csv(small_boundary,
                 file.path(output_dir,
                           "tolerance_validation_small_sample_boundary.csv"),
                 row.names = FALSE)
utils::write.csv(primary_delivery,
                 file.path(output_dir,
                           "tolerance_validation_primary_dgp_delivery.csv"),
                 row.names = FALSE)
utils::write.csv(small_delivery,
                 file.path(output_dir,
                           "tolerance_validation_small_sample_dgp_delivery.csv"),
                 row.names = FALSE)
utils::write.csv(primary_detail,
                 file.path(output_dir,
                           "tolerance_validation_primary_scenario_details.csv"),
                 row.names = FALSE)
utils::write.csv(small_detail,
                 file.path(output_dir,
                           "tolerance_validation_small_sample_scenario_details.csv"),
                 row.names = FALSE)

write_range_tex(
  primary_range,
  file.path(output_dir, "tolerance_validation_primary_scenario_ranges.tex")
)
for (nn in sort(unique(primary_range$n))) {
  write_range_tex(
    primary_range[primary_range$n == nn, , drop = FALSE],
    file.path(
      output_dir,
      sprintf("tolerance_validation_primary_scenario_ranges_n%s.tex", nn)
    )
  )
}
write_range_tex(
  small_boundary,
  file.path(output_dir, "tolerance_validation_small_sample_boundary.tex")
)
write_wide_delivery_tex(
  primary_delivery,
  file.path(output_dir, "tolerance_validation_primary_dgp_delivery.tex"),
  method_order,
  "\\textbf{Primary iid tolerance-validation delivery by DGP.} Entries are delivery percentages for the primary grid at tolerance confidence \\(0.95\\). Delivery counts fail-closed cells as failures.",
  "tab:supp-primary-dgp-delivery"
)
write_wide_delivery_tex(
  small_delivery,
  file.path(output_dir, "tolerance_validation_small_sample_dgp_delivery.tex"),
  small_method_order,
  "\\textbf{Small-sample tolerance-validation delivery by DGP.} Entries are delivery percentages for the practical \\(n=50\\) and \\(n=100\\) follow-up cells at tolerance confidence \\(0.95\\).",
  "tab:supp-small-sample-dgp-delivery"
)

cat("Wrote scenario-aware tolerance validation tables to: ",
    output_dir, "\n", sep = "")
