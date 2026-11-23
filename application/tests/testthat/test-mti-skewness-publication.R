root <- normalizePath(if (file.exists("application/scripts/lib/mti_skewness_publication.R")) "." else "../../..")
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "pharma_tolerance_application", "pharma_mti_application", "mti_fixed_prior_publication",
  "mti_fixed_prior_reporting", "mti_skewness_reporting", "mti_skewness_publication"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))

testthat::test_that("17-digit exports preserve near-boundary endpoints and CDF scores", {
  path <- tempfile(); on.exit(unlink(path))
  lower <- -.4472135954998593954
  x <- data.frame(lower = c(lower, NA_real_), upper = c(1.6666666666666667, 2),
    q = c(.99900000000000011, .99), delta = c(-.10345000000000001, 0))
  msu_write(x, path)
  y <- data.table::fread(path)
  for (field in names(x)) testthat::expect_identical(y[[field]], x[[field]])
  mass <- function(endpoint) pweibull(5, shape = .5) -
    pweibull(endpoint * sqrt(20) + 2, shape = .5)
  testthat::expect_identical(mass(y$lower[1]), mass(lower))
  data.table::fwrite(x, path)
  rounded <- data.table::fread(path)$lower[1]
  testthat::expect_gt(abs(mass(rounded) - mass(lower)), 1e-12)
})

testthat::test_that("expanded evidence preserves methods, priors, historical summaries and Gibbs scope", {
  oldwd <- getwd(); setwd(root); on.exit(setwd(oldwd))
  cfg <- msu_config()
  testthat::expect_silent(msu_verify(cfg, rendered = TRUE))
  p <- mwd_json(file.path(cfg$evidence_dir, "provenance.json"))
  testthat::expect_false(p$prior_uses_analysis_data)
  testthat::expect_false(p$new_gibbs_fits)
  testthat::expect_false(p$fits_rerun)
  d <- msu_read(cfg, "primary_cells")
  testthat::expect_equal(nrow(d), 324L)
  testthat::expect_equal(data.table::uniqueN(d$law_id), 12L)
  testthat::expect_equal(sum(d$samples), 324000L)
  testthat::expect_setequal(d$method, c("ecm", "young_mathew", "wilks"))
  testthat::expect_equal(unique(d$family_size), 108L)
  old <- data.table::fread(file.path(cfg$historical_evidence, "simulation_cells.csv"))
  testthat::expect_equal(nrow(old), 288L)
  testthat::expect_equal(nrow(old[method == "gibbs"]), 72L)
  diagnostics <- msu_read(cfg, "numerical_diagnostics")
  testthat::expect_equal(sum(diagnostics$datasets), 36000L)
  testthat::expect_equal(sum(diagnostics$strict_initial_flags_true), 0L)
  testthat::expect_true(all(diagnostics$max_objective_gap <= 1e-6))
  testthat::expect_true(all(diagnostics$max_scaled_endpoint_gap <= 1e-5))
})

testthat::test_that("missing designs, duplication and incorrect uncertainty families are rejected", {
  oldwd <- getwd(); setwd(root); on.exit(setwd(oldwd))
  cfg <- msu_config(); d <- msu_read(cfg, "primary_cells")
  testthat::expect_error(msu_validate_cells(d[-1], cfg), "Incomplete")
  testthat::expect_error(msu_validate_cells(data.table::rbindlist(list(d, d[1])), cfg), "Incomplete")
  changed <- data.table::copy(d); changed[1, family_size := 72L]
  testthat::expect_error(msu_validate_cells(changed, cfg), "Monte Carlo family")
  changed <- data.table::copy(d); changed[n == 50, n := 51L]
  testthat::expect_error(msu_validate_cells(changed, cfg), "summaries differ")
  changed <- data.table::copy(d); changed[1, simultaneous_lower := lower_95]
  testthat::expect_error(msu_validate_cells(changed, cfg), "summaries differ")
  changed <- data.table::copy(d); changed[1, returned := 999L]
  testthat::expect_error(msu_validate_cells(changed, cfg), "Incomplete")
})

testthat::test_that("portable verification detects altered compact evidence and method descriptions", {
  oldwd <- getwd(); setwd(root); on.exit(setwd(oldwd))
  cfg <- msu_config(); tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  testthat::expect_true(all(file.copy(list.files(cfg$evidence_dir, full.names = TRUE), tmp)))
  cfg$evidence_dir <- tmp
  cfg$study_pointer <- file.path(tmp, "no-local-fits.json")
  testthat::expect_silent(msu_verify(cfg, rendered = TRUE))
  p <- mwd_json(file.path(tmp, "provenance.json")); p$prior_uses_analysis_data <- TRUE
  mgfp_json_write(p, file.path(tmp, "provenance.json"))
  testthat::expect_error(msu_verify(cfg), "method or provenance")
  p$prior_uses_analysis_data <- FALSE; mgfp_json_write(p, file.path(tmp, "provenance.json"))
  d <- data.table::fread(file.path(tmp, "primary_cells.csv")); d[1, attained := 0L]
  msu_write(d, file.path(tmp, "primary_cells.csv"))
  testthat::expect_error(msu_verify(cfg), "Publication data changed")
})

testthat::test_that("displayed comparisons and uncertainty statements follow the case evidence", {
  oldwd <- getwd(); setwd(root); on.exit(setwd(oldwd))
  cfg <- msu_config(); numbers <- msu_numbers(cfg)
  testthat::expect_equal(numbers[["MTIExpandedECMShorter"]], "63")
  testthat::expect_equal(numbers[["MTIExpandedECMLower"]], "73")
  testthat::expect_equal(numbers[["MTIExpandedECMInclude"]], "35")
  testthat::expect_equal(numbers[["MTIExpandedYMMin"]], "93.1")
  added <- msu_read(cfg, "added_cells")
  testthat::expect_true(all(added[method == "ecm", attainment] >= .95))
  pairs <- msu_read(cfg, "paired_comparison")
  new <- pairs[law_id %in% added$law_id & comparator == "young_mathew"]
  testthat::expect_equal(sum(new$mean_width_difference < 0), 24L)
  testthat::expect_equal(sum(new$mean_width_difference > 0), 12L)
  warning <- added[law_id == "invgauss_shape025" & n == 500 & content == .95 & method == "young_mathew"]
  testthat::expect_lt(warning$upper_95, .95)
  testthat::expect_gt(warning$simultaneous_upper, .95)
  widths <- msu_read(cfg, "primary_widths")
  testthat::expect_equal(nrow(widths), 324L)
  testthat::expect_true(all(widths$width_q975 >= widths$width_q025))
})

testthat::test_that("article tables, figures and build checks use the expanded comparison", {
  oldwd <- getwd(); setwd(root); on.exit(setwd(oldwd))
  cfg <- msu_config()
  table <- paste(readLines(file.path(cfg$table_dir, "primary_ranges.tex")), collapse = "\n")
  granular <- paste(readLines(file.path(cfg$table_dir, "added_comparison.tex")), collapse = "\n")
  testthat::expect_false(grepl("DGPs|Fail-closed|Median|Width/|Time|seconds", table))
  testthat::expect_true(grepl("95\\% width range", granular, fixed = TRUE))
  testthat::expect_true(grepl("tab:supp-mti-added-simulation", granular, fixed = TRUE))
  for (part in c("small", "n500", "n1000")) {
    file <- file.path(cfg$figure_dir, paste0("width_ranges_", part, ".pdf"))
    testthat::expect_gt(file.info(file)$size, 10000)
    testthat::expect_equal(rawToChar(readBin(file, "raw", 4)), "%PDF")
  }
  main <- paste(readLines("main.tex"), collapse = "\n")
  testthat::expect_true(grepl("same twelve distributions", main, fixed = TRUE))
  testthat::expect_true(grepl(paste0(cfg$table_dir, "/primary_ranges.tex"), main, fixed = TRUE))
  testthat::expect_equal(length(gregexpr("\\ContinuedFloat", main, fixed = TRUE)[[1]]), 2L)
  make <- readLines("Makefile")
  for (target in c("pdf supplement", "arxiv-source", "article-handoff")) {
    rule <- grep(paste0("^", target, ":"), make, value = TRUE)
    testthat::expect_true(any(grepl("verify-mti-skewness-publication", rule, fixed = TRUE)))
  }
})
