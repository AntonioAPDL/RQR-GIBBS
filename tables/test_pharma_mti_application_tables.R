#!/usr/bin/env Rscript

for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R", "pharma_mti_reporting.R")) {
  source(file.path("application/scripts/lib", name))
}
config <- pma_read_config("application/config/pharma_tolerance_application_profiled_20261025.json")
stopifnot(pma_number(-1e-12) == "0.000", pma_number(-.001) == "-0.001")
results_path <- file.path(config$paths$canonical_dir,"results.csv")
inputs <- pma_read_report_inputs(config,results_path)
expected_splits <- pma_make_splits(inputs$observed, config, "confirmatory")
expected_membership <- pma_membership(expected_splits, inputs$observed)
stopifnot(isTRUE(all.equal(expected_membership, inputs$membership, check.attributes = FALSE)))
concentration <- 1
base <- list(mean=0,sd=4)
# Recompute every empirical evaluation and DP probability from the stored endpoints.
for (s in expected_splits) {
  if (s$scheme %in% names(config$sensitivity$grouped_variables)) {
    variable <- config$sensitivity$grouped_variables[[s$scheme]]
    stopifnot(length(intersect(inputs$observed[[variable]][s$train],
                               inputs$observed[[variable]][s$heldout])) == 0L)
  }
  z <- inputs$results[inputs$results$split_scheme == s$scheme & inputs$results$split_id == s$split_id, ]
  stopifnot(nrow(z) == 6L, all(z$split_seed == s$seed))
  for (i in seq_len(nrow(z))) {
    r <- z[i, ]
    if (!r$interval_returned) {
      stopifnot(!r$heldout_attains_content, nzchar(r$failure_reason))
      next
    }
    test_y <- inputs$observed[[r$response_id]][s$heldout]
    empirical <- mean(test_y >= r$lower & test_y <= r$upper)
    stopifnot(abs(empirical - r$heldout_empirical_content) < 1e-12,
      abs(mean(test_y < r$lower) - r$heldout_lower_omitted) < 1e-12,
      abs(mean(test_y > r$upper) - r$heldout_upper_omitted) < 1e-12,
      r$heldout_attains_content == (empirical >= config$analysis$content))
    if (r$method_id == pma_methods[1]) {
      y <- inputs$observed[[r$response_id]][s$train]
      k <- sum(y >= r$lower & y <= r$upper)
      mass <- pnorm(r$upper, base$mean, base$sd) - pnorm(r$lower, base$mean, base$sd)
      probability <- pbeta(config$analysis$content, k + concentration*mass,
                           length(y) - k + concentration*(1-mass), lower.tail = FALSE)
      stopifnot(abs(probability - r$mti_posterior_content_probability) < 1e-10)
    }
  }
}
output <- tempfile("pharma-mti-table-test-")
dir.create(output)
on.exit(unlink(output, recursive = TRUE), add = TRUE)
run <- function(extra) {
  status <- suppressWarnings(system2("Rscript", c("tables/generate_pharma_mti_application_tables.R",
    paste0("--config=",attr(config,"config_path")),extra),
                                    stdout = TRUE, stderr = TRUE))
  list(status = attr(status, "status") %||% 0L, output = status)
}
generated <- run(c(paste0("--results-csv=",results_path),paste0("--output-dir=",output)))
if (generated$status != 0L) stop(paste(generated$output, collapse = "\n"))
summary <- read.csv(file.path(output, "pharma_application_summary.csv"))
timing <- read.csv(file.path(output, "pharma_application_runtime_summary.csv"))
for (i in seq_len(nrow(timing))) {
  r <- timing[i, ]
  z <- inputs$results[inputs$results$response_id == r$response_id &
    inputs$results$split_scheme == r$split_scheme & inputs$results$method_id == r$method_id, ]
  stopifnot(r$splits == nrow(z), abs(r$total_worker_sec-sum(z$elapsed_sec)) < 1e-8,
    abs(r$median_sec-median(z$elapsed_sec)) < 1e-8)
}
stopifnot(setequal(summary$method_id, pma_methods), all(summary$train_n == 100),
          all(summary$heldout_n == 87))
for (i in seq_len(nrow(summary))) {
  s <- summary[i, ]
  z <- inputs$results[inputs$results$response_id == s$response_id &
    inputs$results$split_scheme == s$split_scheme & inputs$results$method_id == s$method_id, ]
  returned <- z[z$interval_returned, ]
  stopifnot(s$splits == nrow(z), s$interval_return_rate == mean(z$interval_returned),
    s$heldout_attainment_rate == mean(z$heldout_attains_content),
    abs(s$width_median - median(returned$width)) < 1e-12,
    abs(s$width_q025 - quantile(returned$width, .025, type = 8)) < 1e-12,
    abs(s$width_q975 - quantile(returned$width, .975, type = 8)) < 1e-12,
    abs(s$lower_omitted_median - median(returned$heldout_lower_omitted)) < 1e-12)
}
paired <- read.csv(file.path(output, "pharma_application_paired_summary.csv"))
for (i in seq_len(nrow(paired))) {
  s <- paired[i, ]
  z <- inputs$results[inputs$results$response_id == s$response_id &
                        inputs$results$split_scheme == s$split_scheme, ]
  a <- z[z$method_id == pma_methods[1] & z$interval_returned, ]
  b <- z[z$method_id == s$comparator_id & z$interval_returned, ]
  b <- b[match(a$split_id, b$split_id), ]
  keep <- !is.na(b$split_id)
  a <- a[keep, ]; b <- b[keep, ]
  variable <- sub("_difference$", "", s$metric)
  value <- if (s$metric == "width_ratio") a$width / b$width else a[[variable]] - b[[variable]]
  q <- quantile(value, c(.025,.25,.5,.75,.975), type = 8, names = FALSE)
  stopifnot(s$finite_pairs == length(value), max(abs(q - unlist(s[c("q025","q25","median","q75","q975")]))) < 1e-12)
}
commands <- read.csv(file.path(output, "pharma_application_numbers.csv"), colClasses = "character")
for (response in c("fct_tensile", "tbl_rsd_weight")) {
  prefix <- if (response == "fct_tensile") "PharmaTensile" else "PharmaRsd"
  a <- inputs$results[inputs$results$response_id == response & inputs$results$split_scheme == "random" &
                       inputs$results$method_id == pma_methods[1], ]
  expect <- sprintf("%.3f", median(a$width[a$interval_returned]))
  got <- commands$value[commands$command == paste0(prefix,"MtiWidth")]
  stopifnot(identical(got, expect))
  got <- commands$value[commands$command == paste0(prefix,"Converged")]
  stopifnot(as.integer(got) == sum(a$interval_returned & pma_fit_converged(a), na.rm = TRUE))
}
tex_paths <- list.files(output, "\\.tex$", full.names = TRUE)
tex <- paste(unlist(lapply(tex_paths, readLines, warn = FALSE)), collapse = "\n")
stopifnot(!grepl("TCSP|tcsp_mc|TSCP", tex),
  grepl("(proposed)", tex, fixed = TRUE), grepl("(benchmark)", tex, fixed = TRUE),
  grepl("paired random splits of one finite dataset", tex, fixed = TRUE),
  grepl("MTI minus the benchmark", tex, fixed = TRUE))
missing <- run(c("--results-csv=/definitely/absent/results.csv", "--use-committed-inputs=true", paste0("--output-dir=",output)))
stopifnot(missing$status != 0L,
          any(grepl("Expected canonical application results are absent", missing$output)))
committed <- list.files("tables", "^pharma_application_.*\\.(tex|csv)$", full.names = TRUE)
for (path in committed) {
  regenerated <- file.path(output, basename(path))
  if (file.exists(regenerated)) stopifnot(identical(readLines(path), readLines(regenerated)))
}
cat("Pharmaceutical MTI table values, pairing, prose numbers, and missing-input checks passed.\n")
