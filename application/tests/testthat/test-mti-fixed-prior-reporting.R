root <- normalizePath(if (file.exists("application/scripts/lib/mti_fixed_prior_reporting.R")) "." else "../../..")
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "mti_gibbs_full_publication_figures", "pharma_tolerance_application", "pharma_mti_application",
  "mti_fixed_prior_publication", "mti_fixed_prior_reporting"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))

testthat::test_that("held-out summaries preserve the partition and closed-interval convention", {
  methods <- names(mff_methods)
  rows <- data.table::data.table(job_id = 1L, method = methods, training_key = "random:1",
    response_id = "fct_tensile", returned = TRUE, lower = 1, upper = 2, width = 1,
    content = .9, heldout_n = 3L, heldout_empirical_content = 2/3)
  context <- list(training_ids = list("random:1" = 1:2),
    observed = data.frame(.analysis_row = 1:5, fct_tensile = c(99, 99, 1, 2, 3)))
  values <- mff_application_values(rows, context)
  testthat::expect_equal(values$heldout_content, rep(2/3, 4))
  testthat::expect_equal(values$lower_omitted, rep(0, 4))
  testthat::expect_equal(values$upper_omitted, rep(1/3, 4))
  testthat::expect_equal(sum(names(values) == "heldout_n"), 1L)
  testthat::expect_equal(mff_application_cells(values)$attempts, rep(1L, 4))
  primary <- mfr_application_input(values)
  testthat::expect_equal(nrow(primary), 3L)
  testthat::expect_setequal(primary$method_id, c("mti_ecm_adaptive_cell", "young_mathew", "wilks_minmax"))
  testthat::expect_equal(primary$heldout_upper_omitted, rep(1/3, 3))
  path <- tempfile(); on.exit(unlink(path))
  boundary <- values; boundary$lower <- 0.57432014310823753; boundary$upper <- 1.6666666666666667
  pma_write_csv(as.data.frame(boundary), path)
  testthat::expect_identical(data.table::fread(path)$lower, boundary$lower)
  testthat::expect_identical(data.table::fread(path)$upper, boundary$upper)
  rows$heldout_empirical_content[1] <- 0
  testthat::expect_error(mff_application_values(rows, context), "differs from the original")
})

testthat::test_that("current manuscript evidence is complete and independently specified", {
  cfg <- mwd_json(file.path(root, "application/config/mti_fixed_prior_reporting_20261115.json"))
  if (!file.exists(file.path(root, cfg$evidence_dir, "reporting_provenance.json")))
    testthat::skip("Current numerical summaries have not been collected in this source snapshot.")
  original <- getwd(); setwd(root); on.exit(setwd(original))
  testthat::expect_silent(mfr_verify(cfg))
  cells <- mfr_read(cfg$evidence_dir, "simulation_cells")
  testthat::expect_equal(cells[response_id == "student_t3" & n == 500 & content == .99 & method == "gibbs", successes], 931L)
  testthat::expect_equal(cells[response_id == "student_t3" & n == 500 & content == .99 & method == "ecm", successes], 952L)
  primary <- mfr_read(cfg$evidence_dir, "primary_widths")
  testthat::expect_equal(nrow(primary), 216L)
  testthat::expect_false("mti_gibbs_mean" %in% primary$method_id)
  testthat::expect_equal(sort(unique(cells$attempts)), 1000L)
  app <- mfr_read(cfg$evidence_dir, "application_values")
  testthat::expect_equal(nrow(app), 12016L)
  testthat::expect_false(any(app$data_adaptive_prior, na.rm = TRUE))
  testthat::expect_true(all(mfr_read(cfg$evidence_dir, "numerical_assessment")$accurate == c(72000L, 3004L)))
  example <- mfr_read(cfg$evidence_dir, "examples")[case_id == 1L]
  testthat::expect_equal(example$response_id, "normal")
  testthat::expect_equal(example$n, 1000L)
  trace <- mfr_read(cfg$evidence_dir, "trace")
  testthat::expect_true(all(trace$source_job_id == example$source_job_id & trace$response_id == "normal"))
  ranks <- mfr_read(cfg$evidence_dir, "ranks")
  testthat::expect_true(all(ranks$source_job_id == example$source_job_id & ranks$response_id == "normal"))
  patterns <- mfr_read(cfg$evidence_dir, "width_patterns")
  testthat::expect_equal(sum(patterns$ecm_shorter_young_mathew), 39L)
  testthat::expect_equal(patterns[response_id == "normal", ecm_shorter_young_mathew], 1L)
  testthat::expect_equal(patterns[response_id == "student_t3", ecm_shorter_young_mathew], 2L)
  testthat::expect_true(all(patterns[!response_id %in% c("normal", "student_t3"), ecm_shorter_young_mathew] == 6L))
  agreement <- mfr_read(cfg$evidence_dir, "wilks_interval_agreement")
  boundary <- agreement[(n == 50 & content == .9) | (n == 100 & content == .95) | (n == 500 & content == .99)]
  testthat::expect_equal(nrow(boundary), 24L)
  testthat::expect_true(all(boundary$samples == 1000L & boundary$same_intervals == 1000L))
  support <- mfr_read(cfg$evidence_dir, "application_support")
  rsd <- support[response_id == "tbl_rsd_weight" & split_scheme == "random" & method == "gibbs"]
  testthat::expect_equal(rsd$partitions, 1000L)
  testthat::expect_equal(rsd$negative_lower, 772L)
  testthat::expect_equal(rsd$median_lower, -.9018160113, tolerance = 1e-9)
})

testthat::test_that("endpoint agreement uses matched samples, designs, and both endpoints", {
  e <- data.table::data.table(job_id = 1:3, response_id = "normal", n = 50L, content = .9,
    method = "ecm", lower = c(-2, -2, 0), upper = c(2, 3, 2))
  w <- data.table::copy(e)[, method := "wilks"]
  w$upper[2] <- 4
  w$lower[3] <- 5e-13
  result <- mfr_wilks_agreement(e, w[3:1])
  testthat::expect_equal(result$samples, 3L)
  testthat::expect_equal(result$same_intervals, 2L)
  testthat::expect_equal(mfr_wilks_agreement(e, w, tolerance = 0)$same_intervals, 1L)
  testthat::expect_error(mfr_wilks_agreement(e, w[-1]), "samples differ")
  testthat::expect_error(mfr_wilks_agreement(e, data.table::rbindlist(list(w, w[1]))), "unique samples")
  different <- data.table::copy(w); different$content[1] <- .95
  testthat::expect_error(mfr_wilks_agreement(e, different), "designs differ")
  invalid <- data.table::copy(w); invalid$upper[1] <- Inf
  testthat::expect_error(mfr_wilks_agreement(e, invalid), "valid finite intervals")
  invalid$upper[1] <- -3
  testthat::expect_error(mfr_wilks_agreement(e, invalid), "valid finite intervals")
  testthat::expect_error(mfr_wilks_agreement(e, w, tolerance = -1), "nonnegative")
})

testthat::test_that("width and support comparisons retain distribution and partition groups", {
  cells <- data.table::CJ(response_id = c("normal", "exponential"), n = 50L,
    content = c(.9, .95), method = c("ecm", "gibbs", "young_mathew"))
  cells[, mean_width := c(ecm = 1, gibbs = 2, young_mathew = 3)[method]]
  cells[response_id == "normal" & content == .95 & method == "ecm", mean_width := 4]
  patterns <- mfr_width_patterns(cells)
  testthat::expect_equal(patterns[response_id == "normal", ecm_shorter_young_mathew], 1L)
  testthat::expect_equal(patterns[response_id == "exponential", ecm_shorter_young_mathew], 2L)
  testthat::expect_error(mfr_width_patterns(cells[method != "gibbs"]), "widths are missing")
  values <- data.table::data.table(response_id = "tbl_rsd_weight", method = "gibbs",
    split_scheme = c("random", "random", "chronological_forward"), lower = c(-1, 0, -2), upper = 3)
  support <- mfr_application_support(values)
  testthat::expect_equal(support[split_scheme == "random", partitions], 2L)
  testthat::expect_equal(support[split_scheme == "random", negative_lower], 1L)
  testthat::expect_equal(support[split_scheme == "random", median_lower], -.5)
  testthat::expect_equal(support[split_scheme == "chronological_forward", negative_lower], 1L)
})

testthat::test_that("posterior display examples have explicit order independent of task storage", {
  d <- data.table::data.table(response_id = rep(c("student_t3", "exponential", "normal"), each = 2),
    n = rep(c(500L, 1000L, 1000L), each = 2), content = rep(c(.99, .9, .9), each = 2),
    job_id = c(41002L, 41001L, 50002L, 50001L, 48002L, 48001L))
  selected <- mfr_example_tasks(d)
  testthat::expect_equal(selected$response_id, c("normal", "student_t3", "exponential"))
  testthat::expect_equal(selected$job_id, c(48001L, 41001L, 50001L))
  testthat::expect_identical(mfr_example_tasks(d[6:1]), selected)
  testthat::expect_error(mfr_example_tasks(d[response_id != "normal"]), "example is missing")
})

testthat::test_that("primary tables contain only the specified methods and metric", {
  d <- data.table::data.table(n = 50, content = .9, method_id = unname(mff_ids[c("ecm", "young_mathew", "wilks")]),
    method = unname(mfr_labels[c("ecm", "young_mathew", "wilks")]), delivery_min = c(.95, .94, .96),
    delivery_max = c(.99, .98, 1))
  path <- tempfile(); on.exit(unlink(path))
  mfr_primary_table(d, path)
  text <- paste(readLines(path), collapse = "\n")
  testthat::expect_match(text, "Content-attainment range")
  testthat::expect_match(text, "95.0--99.0", fixed = TRUE)
  testthat::expect_match(text, "Wilks", fixed = TRUE)
  testthat::expect_false(grepl("Gibbs|Runtime|DGPs|Fail-closed|Median sec|Width/", text))
})
