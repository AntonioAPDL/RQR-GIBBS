root <- normalizePath(if (file.exists("application/scripts/lib/mti_fixed_prior_reporting.R")) "." else "../../..")
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "pharma_tolerance_application", "pharma_mti_application", "mti_fixed_prior_publication", "mti_fixed_prior_reporting"))
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
  rows$heldout_empirical_content[1] <- 0
  testthat::expect_error(mff_application_values(rows, context), "differs from the original")
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
