#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[startsWith(args, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/68_validate_tcsp_split_exact.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

cli <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- cli[startsWith(cli, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)

for (package in c("rqrgibbs", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}
library(rqrgibbs)
`%||%` <- function(a, b) if (is.null(a)) b else a

mode <- tolower(arg_value("--mode=", "smoke"))
config_path <- normalizePath(
  arg_value("--config=", file.path("application", "config",
                                   "tcsp_split_exact_validation_v1.json")),
  winslash = "/", mustWork = TRUE
)
config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
if (!mode %in% names(config$modes)) {
  stopf("Unsupported split exact TCSP validation mode: ", mode)
}
if (!isTRUE(config$execution[[paste0(mode, "_authorized")]])) {
  stopf("Split exact TCSP validation mode is not authorized: ", mode)
}
output_dir <- normalizePath(
  arg_value("--output-dir=", file.path(
    "application", "outputs", "tcsp_split_exact_validation_v1",
    paste0(mode, "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  )),
  winslash = "/", mustWork = FALSE
)
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stopf("The output directory must be fresh: ", output_dir)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

git_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(e) NA_character_
)
mode_cfg <- config$modes[[mode]]
dgp_by_id <- setNames(config$dgps, vapply(config$dgps, `[[`, character(1L), "dgp_id"))

dgp_meta <- function(dgp) {
  if (identical(dgp$family, "normal")) {
    return(list(
      r = function(n) stats::rnorm(n),
      p = stats::pnorm
    ))
  }
  if (identical(dgp$family, "standardized_lognormal")) {
    logsd <- dgp$logsd %||% 0.75
    mean_raw <- exp(logsd^2 / 2)
    sd_raw <- sqrt((exp(logsd^2) - 1) * exp(logsd^2))
    return(list(
      r = function(n) (stats::rlnorm(n, 0, logsd) - mean_raw) / sd_raw,
      p = function(x) stats::plnorm(x * sd_raw + mean_raw, 0, logsd)
    ))
  }
  if (identical(dgp$family, "standardized_normal_mixture")) {
    weights <- as.numeric(dgp$weights)
    means <- as.numeric(dgp$means)
    sds <- as.numeric(dgp$sds)
    mean_mix <- sum(weights * means)
    second <- sum(weights * (sds^2 + means^2))
    sd_mix <- sqrt(second - mean_mix^2)
    return(list(
      r = function(n) {
        comp <- sample.int(length(weights), n, replace = TRUE, prob = weights)
        (stats::rnorm(n, means[comp], sds[comp]) - mean_mix) / sd_mix
      },
      p = function(x) {
        raw <- x * sd_mix + mean_mix
        rowSums(vapply(seq_along(weights), function(j) {
          weights[[j]] * stats::pnorm(raw, means[[j]], sds[[j]])
        }, numeric(length(raw))))
      }
    ))
  }
  stopf("Unsupported DGP family: ", dgp$family)
}

content_of <- function(fit, cdf) {
  lower <- fit$contract$lower_endpoint
  upper <- fit$contract$upper_endpoint
  as.numeric(cdf(upper) - cdf(lower))
}

fit_method <- function(method_id, y, c_target, conf, pilot_fraction, seed) {
  if (identical(method_id, "tcsp_dkw")) {
    return(rqr_tcsp_fit_univariate(
      y, c_target, conf, scan_method = "dkw_conservative"
    ))
  }
  if (identical(method_id, "split_empirical_shortest")) {
    return(rqr_tcsp_split_exact_fit(
      y, c_target, conf,
      pilot_fraction = pilot_fraction,
      pilot_method = "empirical_shortest",
      split_seed = seed
    ))
  }
  if (identical(method_id, "split_ecm_fixed_tilt")) {
    return(rqr_tcsp_split_exact_fit(
      y, c_target, conf,
      pilot_fraction = pilot_fraction,
      pilot_method = "ecm_fixed_tilt",
      split_seed = seed,
      ecm_args = list(ecm_control = as.list(config$ecm_control))
    ))
  }
  if (identical(method_id, "split_cornish_fisher")) {
    return(rqr_tcsp_split_exact_fit(
      y, c_target, conf,
      pilot_fraction = pilot_fraction,
      pilot_method = "cornish_fisher",
      split_seed = seed
    ))
  }
  if (identical(method_id, "wilks_minmax")) {
    contract <- list(
      formal_tolerance_action = "[Y_(1),Y_(n)]",
      lower_endpoint = min(y),
      upper_endpoint = max(y),
      width = diff(range(y)),
      formal_action_source = "full_sample_minmax_comparator",
      posterior_summary_action = "not_formal_tolerance_action",
      finite_sample_claim_available = TRUE
    )
    return(structure(list(contract = contract),
                     class = c("rqr_tcsp_minmax_comparator", "list")))
  }
  stopf("Unsupported method_id: ", method_id)
}

rows <- list()
counter <- 0L
base_seed <- as.integer(config$base_seed %||% 812700)
for (dgp_id in as.character(mode_cfg$dgp_ids)) {
  dgp <- dgp_by_id[[dgp_id]]
  meta <- dgp_meta(dgp)
  for (n in as.integer(mode_cfg$sample_sizes)) {
    for (c_target in as.numeric(mode_cfg$guaranteed_contents)) {
      for (conf in as.numeric(mode_cfg$tolerance_confidences)) {
        for (pilot_fraction in as.numeric(mode_cfg$pilot_fractions)) {
          for (rep in seq_len(as.integer(mode_cfg$replications))) {
            counter <- counter + 1L
            seed <- base_seed + counter
            set.seed(seed)
            y <- meta$r(n)
            method_ids <- as.character(mode_cfg$method_ids)
            for (method_id in method_ids) {
              method_seed <- seed + match(method_id, method_ids) * 10000L
              timing <- system.time({
                fit <- tryCatch(
                  fit_method(
                    method_id, y, c_target, conf, pilot_fraction, method_seed
                  ),
                  error = function(e) e
                )
              })
              if (inherits(fit, "error")) {
                rows[[length(rows) + 1L]] <- data.frame(
                  mode = mode,
                  dgp_id = dgp_id,
                  n = n,
                  guaranteed_content = c_target,
                  tolerance_confidence = conf,
                  pilot_fraction = pilot_fraction,
                  replication = rep,
                  seed = seed,
                  method_id = method_id,
                  success = NA,
                  content = NA_real_,
                  width = NA_real_,
                  infeasible = TRUE,
                  message = conditionMessage(fit),
                  elapsed_sec = unname(timing[["elapsed"]])
                )
              } else {
                content <- content_of(fit, meta$p)
                rows[[length(rows) + 1L]] <- data.frame(
                  mode = mode,
                  dgp_id = dgp_id,
                  n = n,
                  guaranteed_content = c_target,
                  tolerance_confidence = conf,
                  pilot_fraction = pilot_fraction,
                  replication = rep,
                  seed = seed,
                  method_id = method_id,
                  success = content >= c_target - 1e-12,
                  content = content,
                  width = fit$contract$width,
                  infeasible = FALSE,
                  message = "",
                  elapsed_sec = unname(timing[["elapsed"]])
                )
              }
            }
          }
        }
      }
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output_dir, "split_exact_validation_results.csv"),
          row.names = FALSE)
summary <- aggregate(
  cbind(success = as.numeric(success), width, infeasible = as.numeric(infeasible)) ~
    mode + dgp_id + n + guaranteed_content + tolerance_confidence +
    pilot_fraction + method_id,
  data = results,
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = stats::median(x, na.rm = TRUE))
)
write.csv(summary, file.path(output_dir, "split_exact_validation_summary.csv"),
          row.names = FALSE)
manifest <- list(
  schema_version = config$schema_version,
  study_id = config$study_id,
  mode = mode,
  git_commit = git_commit,
  config_path = config_path,
  output_dir = output_dir,
  rows = nrow(results),
  generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  response_likelihood = FALSE,
  interpretation = config$interpretation
)
jsonlite::write_json(
  manifest, file.path(output_dir, "manifest.json"),
  pretty = TRUE, auto_unbox = TRUE
)
writeLines(c(
  "# Split Exact-Spacing TCSP Validation Smoke",
  "",
  paste("Mode:", mode),
  paste("Git commit:", git_commit),
  paste("Rows:", nrow(results)),
  "",
  "This run compares iid univariate tolerance actions. It is a pilot validation artifact, not a heavy campaign."
), file.path(output_dir, "README.md"))
cat("TCSP split exact validation", mode, "completed:", output_dir, "\n")
