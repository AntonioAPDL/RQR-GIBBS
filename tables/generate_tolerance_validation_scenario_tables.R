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
  "application", "runs",
  "rqr_bayes_uq_validation_main_3method_refined_dgps_20260820",
  "wave_confirmatory_refined_dgps_20260820T221539Z"
)
default_primary_results <- file.path(default_primary_dir,
                                     "bayes_uq_validation_results.csv")
default_young_mathew_results <- ""
default_small_results <- ""
default_scan_calibration <- file.path(default_primary_dir,
                                      "scan_calibration_summary.csv")

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
scan_calibration_path <- arg_value(
  "--scan-calibration-csv=",
  Sys.getenv("RQR_BAYES_UQ_SCAN_CALIBRATION",
             unset = default_scan_calibration)
)
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

primary_outputs <- c(
  "tolerance_validation_article_scenario_ranges.csv",
  "tolerance_validation_article_scenario_ranges.tex",
  "tolerance_validation_article_scenario_ranges_n50.tex",
  "tolerance_validation_article_scenario_ranges_n100.tex",
  "tolerance_validation_article_scenario_ranges_n500.tex",
  "tolerance_validation_article_scenario_ranges_n1000.tex",
  "tolerance_validation_article_dgp_delivery.csv",
  "tolerance_validation_article_dgp_delivery.tex",
  "tolerance_validation_article_dgp_width_ranges.csv",
  "tolerance_validation_article_dgp_width_ranges.tex",
  "tolerance_validation_article_scan_calibration.csv",
  "tolerance_validation_article_scan_calibration.tex",
  "tolerance_validation_article_scenario_details.csv"
)
small_outputs <- c(
  "tolerance_validation_article_small_sample_boundary.csv",
  "tolerance_validation_article_small_sample_boundary.tex",
  "tolerance_validation_article_small_sample_dgp_delivery.csv",
  "tolerance_validation_article_small_sample_dgp_delivery.tex",
  "tolerance_validation_article_small_sample_dgp_width_ranges.csv",
  "tolerance_validation_article_small_sample_dgp_width_ranges.tex",
  "tolerance_validation_article_small_sample_scenario_details.csv"
)
outputs <- c(primary_outputs, small_outputs)
committed_outputs <- file.path(repo_root, "tables", outputs)
target_outputs <- file.path(output_dir, outputs)

if (!file.exists(primary_path)) {
  committed_primary_outputs <- file.path(repo_root, "tables", primary_outputs)
  if (all(file.exists(committed_primary_outputs))) {
    if (!identical(normalizePath(output_dir, winslash = "/", mustWork = FALSE),
                   normalizePath(file.path(repo_root, "tables"),
                                 winslash = "/", mustWork = TRUE))) {
      copy_outputs <- c(
        primary_outputs,
        small_outputs[file.exists(file.path(repo_root, "tables", small_outputs))]
      )
      file.copy(file.path(repo_root, "tables", copy_outputs),
                file.path(output_dir, copy_outputs), overwrite = TRUE)
    }
    cat("Using committed scenario-aware tolerance validation tables;",
        "provide raw result CSVs to regenerate.\n")
    quit(status = 0)
  }
  missing <- c(
    primary_results = primary_path
  )
  missing <- missing[!file.exists(missing)]
  stopf("Missing scenario table input(s): ",
        paste(names(missing), missing, sep = "=", collapse = "; "))
}

primary_supp_method_order <- c(
  "tcsp_mc",
  "young_mathew",
  "wilks_minmax"
)
primary_main_method_order <- c(
  "tcsp_mc",
  "young_mathew",
  "wilks_minmax"
)
small_supp_method_order <- c(
  "tcsp_mc",
  "young_mathew",
  "wilks_minmax"
)
small_main_method_order <- c(
  "tcsp_mc",
  "young_mathew",
  "wilks_minmax"
)
method_labels <- c(
  tcsp_mc = "TCSP",
  young_mathew = "Young--Mathew",
  wilks_minmax = "Wilks"
)
dgp_labels <- c(
  normal = "Gaussian",
  laplace = "Laplace",
  lognormal = "log-normal",
  lognormal_hard = "hard log-normal",
  mixture = "mixture",
  sharp_mixture = "sharp mixture",
  contaminated_normal = "contaminated normal",
  student_t3 = "Student t3",
  gamma2 = "Gamma(2,1)",
  exponential = "exponential",
  beta52 = "Beta(5,2)",
  beta_left = "left-skewed Beta"
)

required_columns <- c(
  "dgp_id", "n", "guaranteed_content", "tolerance_confidence",
  "replication", "method_id", "success", "infeasible", "width"
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
quantile_or_na <- function(x, prob) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) {
    unname(stats::quantile(x, prob, names = FALSE, type = 8))
  } else {
    NA_real_
  }
}
first_ordered_replicate_rows <- function(results) {
  results$n <- as.integer(results$n)
  results$guaranteed_content <- num(results$guaranteed_content)
  results$tolerance_confidence <- num(results$tolerance_confidence)
  results$replication <- as.integer(results$replication)
  results$width <- num(results$width)
  if ("posterior_confidence" %in% names(results)) {
    results$posterior_confidence <- num(results$posterior_confidence)
    post <- results$posterior_confidence
  } else {
    post <- rep(NA_real_, nrow(results))
  }
  ord <- order(
    results$dgp_id, results$n, results$guaranteed_content,
    results$tolerance_confidence, results$method_id, results$replication,
    post, na.last = TRUE
  )
  results <- results[ord, , drop = FALSE]
  key <- paste(
    results$dgp_id, results$n, sprintf("%.4f", results$guaranteed_content),
    sprintf("%.4f", results$tolerance_confidence), results$method_id,
    results$replication,
    sep = "||"
  )
  results[!duplicated(key), , drop = FALSE]
}

scenario_detail <- function(results, methods) {
  results <- results[results$method_id %in% methods, , drop = FALSE]
  if (!nrow(results)) {
    return(data.frame(
      dgp_id = character(), dgp = character(), n = integer(),
      content = numeric(), method_id = character(), method = character(),
      replications = integer(), infeasible_rate = numeric(),
      delivery_success = numeric(), returned_success = numeric(),
      median_width = numeric(), mean_width = numeric(), width_q025 = numeric(),
      width_q975 = numeric(), stringsAsFactors = FALSE
    ))
  }
  results$infeasible_bool <- truthy(results$infeasible)
  results$success_bool <- truthy(results$success)
  key <- paste(results$dgp_id, results$n,
               sprintf("%.4f", results$guaranteed_content),
               results$method_id, sep = "||")
  rows <- lapply(split(results, key), function(df) {
    returned <- !df$infeasible_bool & !is.na(df$success_bool)
    finite_width <- is.finite(df$width)
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
      median_width = if (any(finite_width)) {
        stats::median(df$width[finite_width])
      } else {
        NA_real_
      },
      mean_width = if (any(finite_width)) {
        mean(df$width[finite_width])
      } else {
        NA_real_
      },
      width_q025 = quantile_or_na(df$width, 0.025),
      width_q975 = quantile_or_na(df$width, 0.975),
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
  if (!nrow(detail)) {
    return(data.frame(
      n = integer(), content = numeric(), method_id = character(),
      method = character(), delivery_min = numeric(), delivery_max = numeric(),
      returned_success_min = numeric(), returned_success_max = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  key <- paste(detail$n, sprintf("%.4f", detail$content),
               detail$method_id, sep = "||")
  rows <- lapply(split(detail, key), function(df) {
    data.frame(
      n = as.integer(df$n[[1L]]),
      content = num(df$content[[1L]]),
      method_id = df$method_id[[1L]],
      method = df$method[[1L]],
      delivery_min = min_or_na(df$delivery_success),
      delivery_max = max_or_na(df$delivery_success),
      returned_success_min = min_or_na(df$returned_success),
      returned_success_max = max_or_na(df$returned_success),
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
format_width_range <- function(a, b) {
  a <- num(a)
  b <- num(b)
  out <- rep("--", length(a))
  ok <- is.finite(a) & is.finite(b)
  same <- ok & abs(a - b) < 5e-4
  out[same] <- format_num(a[same], 2)
  out[ok & !same] <- paste0(format_num(a[ok & !same], 2), "--",
                            format_num(b[ok & !same], 2))
  out
}
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}
cell_label <- function(n, content) {
  sprintf("\\(n=%s,c=%.2f\\)", as.integer(n), num(content))
}

dgp_width_ranges <- function(detail, methods) {
  if (!nrow(detail)) {
    return(data.frame(
      dgp_id = character(), dgp = character(), n = integer(),
      content = numeric(), method_id = character(), method = character(),
      replications = integer(), delivery_success = numeric(),
      returned_success = numeric(), mean_width = numeric(), width_q025 = numeric(),
      width_q975 = numeric(), stringsAsFactors = FALSE
    ))
  }
  out <- detail[detail$method_id %in% methods, c(
    "dgp_id", "dgp", "n", "content", "method_id", "method",
    "replications", "delivery_success", "returned_success",
    "mean_width", "width_q025", "width_q975"
  ), drop = FALSE]
  out <- out[order(out$n, out$content, out$dgp,
                   match(out$method_id, methods)), ]
  rownames(out) <- NULL
  out
}

write_range_tex <- function(summary, path) {
  lines <- c(
    "\\begin{tabular}{@{}l@{\\hspace{0.75em}}l@{\\hspace{0.75em}}r@{}}",
    "\\toprule",
    "Cell & Method & Delivery success range (\\%)\\\\",
    "\\midrule"
  )
  if (!nrow(summary)) {
    writeLines(c(
      lines,
      "\\multicolumn{3}{@{}l@{}}{No selected validation rows.}\\\\",
      "\\bottomrule",
      "\\end{tabular}"
    ), path)
    return(invisible(path))
  }
  for (cell in unique(paste(summary$n, summary$content, sep = "||"))) {
    block <- summary[paste(summary$n, summary$content, sep = "||") == cell,
                     , drop = FALSE]
    for (ii in seq_len(nrow(block))) {
      lines <- c(lines, sprintf(
        "%s & %s & %s \\\\",
        if (ii == 1L) cell_label(block$n[[ii]], block$content[[ii]]) else "",
        escape_latex(block$method[[ii]]),
        format_range(block$delivery_min[[ii]], block$delivery_max[[ii]])
      ))
    }
    lines <- c(lines, "\\addlinespace[0.25em]")
  }
  lines <- lines[-length(lines)]
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), path)
}

write_dgp_width_tex <- function(widths, path, caption, label) {
  lines <- c(
    "\\begingroup",
    "\\scriptsize",
    "\\begin{longtable}{@{}p{0.24\\textwidth}rrp{0.20\\textwidth}rr@{}}",
    sprintf("\\caption{%s}\\label{%s}\\\\", caption, label),
    "\\toprule",
    "Distribution & \\(n\\) & \\(c\\) & Method & Delivery (\\%) & Width 95\\% range\\\\",
    "\\midrule",
    "\\endfirsthead",
    "\\toprule",
    "Distribution & \\(n\\) & \\(c\\) & Method & Delivery (\\%) & Width 95\\% range\\\\",
    "\\midrule",
    "\\endhead"
  )
  body <- vapply(seq_len(nrow(widths)), function(ii) {
    sprintf(
      "%s & %s & %.2f & %s & %s & %s \\\\",
      escape_latex(widths$dgp[[ii]]),
      as.integer(widths$n[[ii]]),
      num(widths$content[[ii]]),
      escape_latex(widths$method[[ii]]),
      format_pct(widths$delivery_success[[ii]]),
      format_width_range(widths$width_q025[[ii]], widths$width_q975[[ii]])
    )
  }, character(1L))
  writeLines(c(lines, body, "\\bottomrule", "\\end{longtable}", "\\endgroup"),
             path)
}

wide_delivery <- function(detail, methods) {
  if (!nrow(detail)) {
    out <- data.frame(dgp = character(), n = integer(), content = numeric(),
                      stringsAsFactors = FALSE)
    for (method in methods) out[[unname(method_labels[method])]] <- character()
    return(out)
  }
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
    paste(c("Distribution", "\\(n\\)", "\\(c\\)", method_headers),
          collapse = " & "),
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

scan_calibration_table <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(data.frame(
      n = integer(), content = numeric(), tolerance_confidence = numeric(),
      retained_count = integer(), retained_fraction = numeric(),
      content_buffer = numeric(), certified_lower_probability = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  scan <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "n", "guaranteed_content", "tolerance_confidence", "retained_count",
    "content_buffer", "certified_lower_probability"
  )
  missing <- setdiff(required, names(scan))
  if (length(missing)) {
    stopf("Scan calibration CSV is missing column(s): ",
          paste(missing, collapse = ", "))
  }
  scan$n <- as.integer(scan$n)
  scan$content <- num(scan$guaranteed_content)
  scan$tolerance_confidence <- num(scan$tolerance_confidence)
  scan$retained_count <- as.integer(scan$retained_count)
  scan$content_buffer <- num(scan$content_buffer)
  scan$certified_lower_probability <- num(scan$certified_lower_probability)
  if ("infeasible" %in% names(scan)) {
    scan$infeasible_bool <- truthy(scan$infeasible)
  } else {
    scan$infeasible_bool <- FALSE
  }
  scan <- scan[!scan$infeasible_bool, , drop = FALSE]
  scan$retained_fraction <- scan$retained_count / scan$n
  out <- unique(scan[, c(
    "n", "content", "tolerance_confidence", "retained_count",
    "retained_fraction", "content_buffer", "certified_lower_probability"
  ), drop = FALSE])
  out <- out[order(out$n, out$content, out$tolerance_confidence), ]
  rownames(out) <- NULL
  out
}

format_prob3 <- function(x) {
  x <- num(x)
  ifelse(is.finite(x), sprintf("%.3f", x), "--")
}

write_scan_calibration_tex <- function(scan, path) {
  lines <- c(
    "\\begin{tabular}{@{}rrrrrr@{}}",
    "\\toprule",
    "\\(n\\) & \\(c\\) & Retained count \\(k\\) & \\(k/n\\) & Buffer \\(k/n-c\\) & Certified lower probability\\\\",
    "\\midrule"
  )
  if (!nrow(scan)) {
    writeLines(c(
      lines,
      "\\multicolumn{6}{@{}l@{}}{No scan-calibration input was provided.}\\\\",
      "\\bottomrule",
      "\\end{tabular}"
    ), path)
    return(invisible(path))
  }
  body <- vapply(seq_len(nrow(scan)), function(ii) {
    sprintf(
      "%s & %.2f & %s & %s & %s & %s \\\\",
      as.integer(scan$n[[ii]]),
      num(scan$content[[ii]]),
      as.integer(scan$retained_count[[ii]]),
      format_prob3(scan$retained_fraction[[ii]]),
      format_prob3(scan$content_buffer[[ii]]),
      format_prob3(scan$certified_lower_probability[[ii]])
    )
  }, character(1L))
  writeLines(c(lines, body, "\\bottomrule", "\\end{tabular}"), path)
}

primary <- read_results(primary_path, "Primary validation results")
if (file.exists(young_mathew_path) &&
    !any(primary$method_id == "young_mathew")) {
  primary <- bind_fill(
    primary,
    read_results(young_mathew_path, "Young--Mathew add-on results")
  )
}
primary <- first_ordered_replicate_rows(primary)
small <- if (nzchar(small_path) && file.exists(small_path)) {
  first_ordered_replicate_rows(
    read_results(small_path, "Small-sample validation results")
  )
} else {
  primary[0L, , drop = FALSE]
}

primary_detail <- scenario_detail(primary, primary_supp_method_order)
primary_range <- range_summary(
  primary_detail[primary_detail$method_id %in% primary_main_method_order,
                 , drop = FALSE],
  primary_main_method_order
)
small_detail <- scenario_detail(small, small_supp_method_order)
small_boundary_detail <- small_detail[
  small_detail$n %in% c(50L, 100L),
  ,
  drop = FALSE
]
small_boundary <- range_summary(
  small_boundary_detail[
    small_boundary_detail$method_id %in% small_main_method_order,
    ,
    drop = FALSE
  ],
  small_main_method_order
)
has_small_boundary <- nrow(small_boundary_detail) > 0L
primary_dgp_widths <- dgp_width_ranges(primary_detail,
                                       primary_main_method_order)
small_dgp_widths <- dgp_width_ranges(small_boundary_detail,
                                     small_main_method_order)
scan_calibration <- scan_calibration_table(scan_calibration_path)

primary_delivery <- wide_delivery(primary_detail, primary_supp_method_order)
small_delivery <- wide_delivery(small_boundary_detail, small_supp_method_order)

utils::write.csv(primary_range,
                 file.path(output_dir,
                           "tolerance_validation_article_scenario_ranges.csv"),
                 row.names = FALSE)
utils::write.csv(primary_delivery,
                 file.path(output_dir,
                           "tolerance_validation_article_dgp_delivery.csv"),
                 row.names = FALSE)
utils::write.csv(primary_dgp_widths,
                 file.path(output_dir,
                           "tolerance_validation_article_dgp_width_ranges.csv"),
                 row.names = FALSE)
utils::write.csv(scan_calibration,
                 file.path(output_dir,
                           "tolerance_validation_article_scan_calibration.csv"),
                 row.names = FALSE)
utils::write.csv(primary_detail,
                 file.path(output_dir,
                           "tolerance_validation_article_scenario_details.csv"),
                 row.names = FALSE)
if (has_small_boundary) {
  utils::write.csv(small_boundary,
                   file.path(output_dir,
                             "tolerance_validation_article_small_sample_boundary.csv"),
                   row.names = FALSE)
  utils::write.csv(small_delivery,
                   file.path(output_dir,
                             "tolerance_validation_article_small_sample_dgp_delivery.csv"),
                   row.names = FALSE)
  utils::write.csv(small_dgp_widths,
                   file.path(output_dir,
                             "tolerance_validation_article_small_sample_dgp_width_ranges.csv"),
                   row.names = FALSE)
  utils::write.csv(small_detail,
                   file.path(output_dir,
                             "tolerance_validation_article_small_sample_scenario_details.csv"),
                   row.names = FALSE)
} else {
  unlink(file.path(output_dir, small_outputs), force = TRUE)
}

write_range_tex(
  primary_range,
  file.path(output_dir, "tolerance_validation_article_scenario_ranges.tex")
)
for (nn in sort(unique(primary_range$n))) {
  write_range_tex(
    primary_range[primary_range$n == nn, , drop = FALSE],
    file.path(
      output_dir,
      sprintf("tolerance_validation_article_scenario_ranges_n%s.tex", nn)
    )
  )
}
write_wide_delivery_tex(
  primary_delivery,
  file.path(output_dir, "tolerance_validation_article_dgp_delivery.tex"),
  primary_supp_method_order,
  "\\textbf{Primary iid tolerance-validation delivery by distribution.} Entries are percentages of replications in which the returned interval attained the requested population content for the three reported methods at tolerance confidence \\(0.95\\). Replications with no returned interval count as failures.",
  "tab:supp-primary-dgp-delivery"
)
write_dgp_width_tex(
  primary_dgp_widths,
  file.path(output_dir, "tolerance_validation_article_dgp_width_ranges.tex"),
  "\\textbf{Primary iid tolerance-validation width ranges by distribution.} Width intervals are empirical 2.5\\%--97.5\\% ranges over the paired resamplings within each distribution, sample size, content, and method. Widths are summarized within, rather than pooled across, distributions.",
  "tab:supp-primary-dgp-width-ranges"
)
write_scan_calibration_tex(
  scan_calibration,
  file.path(output_dir, "tolerance_validation_article_scan_calibration.tex")
)
if (has_small_boundary) {
  write_range_tex(
    small_boundary,
    file.path(output_dir, "tolerance_validation_article_small_sample_boundary.tex")
  )
  write_wide_delivery_tex(
    small_delivery,
    file.path(output_dir,
              "tolerance_validation_article_small_sample_dgp_delivery.tex"),
    small_supp_method_order,
    "\\textbf{Small-sample tolerance-validation delivery by distribution.} Entries are delivery percentages for the practical \\(n=50\\) and \\(n=100\\) follow-up cells at tolerance confidence \\(0.95\\).",
    "tab:supp-small-sample-dgp-delivery"
  )
  write_dgp_width_tex(
    small_dgp_widths,
    file.path(output_dir,
              "tolerance_validation_article_small_sample_dgp_width_ranges.tex"),
    "\\textbf{Small-sample tolerance-validation width ranges by distribution.} Width intervals are empirical 2.5\\%--97.5\\% ranges over the paired resamplings within each distribution, sample size, content, and method. Widths are summarized within, rather than pooled across, distributions.",
    "tab:supp-small-sample-dgp-width-ranges"
  )
}

cat("Wrote scenario-aware tolerance validation tables to: ",
    output_dir, "\n", sep = "")
