root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_full_publication.R")) "." else "../../..")
source(file.path(root, "application/scripts/lib/mti_gibbs_full_report_review.R"))
source(file.path(root, "application/scripts/lib/mti_gibbs_full_publication.R"))
source(file.path(root, "application/scripts/lib/mti_gibbs_full_publication_figures.R"))

publication_fixture <- function() {
  d <- data.frame(dataset_id = 1:4, source = rep(c("simulation", "application"), each = 2),
    response_id = rep(c("normal", "response"), each = 2), n = 50L, content = .9,
    tolerance_confidence = .95, replication = rep(1:2, 2), split_scheme = rep(c("", "random"), each = 2),
    ecm_lower = 0, ecm_upper = 2, mean_lower = rep(c(-1, -2), 2), mean_upper = rep(c(3, 4), 2),
    mean_width = rep(c(4, 6), 2), mean_content = .96, mean_attained = TRUE,
    median_content = rep(c(.94, .89), 2), median_interval_width = rep(c(3.6, 5.6), 2),
    target_class = rep(c("interior", "boundary"), 2), preferred_pass = TRUE)
  intervals <- do.call(rbind, lapply(1:4, function(i) {
    z <- d[rep(i, 4), c("dataset_id", "source", "response_id", "n", "content", "tolerance_confidence",
      "replication", "split_scheme")]
    z$method <- c("mti_gibbs_mean", "mti_ecm_adaptive_cell", "young_mathew",
      if (d$source[i] == "simulation") "wilks" else "wilks_minmax")
    z$width <- c(d$mean_width[i], 2, 2.2, 2.5); z$attained <- c(TRUE, TRUE, FALSE, TRUE)
    z$available <- TRUE; z
  }))
  p <- data.frame(source = rep(d$source, each = 4), variable = rep(c("lower", "upper", "width", "midpoint"), 4),
    preferred_pass = TRUE, mean_precision = TRUE, median_precision = TRUE, interval_precision = TRUE,
    mcse_sd_ratio = .02)
  cells <- mgfa_cells(intervals)
  list(d = d, p = p, intervals = intervals, cells = cells)
}

testthat::test_that("simultaneous bounds use the actual per-method family size and omit applications", {
  x <- data.frame(source = c("simulation", "simulation", "simulation", "application"),
    method = c("a", "a", "b", "a"), datasets = 1000L, attained = c(0L, 1000L, 950L, 900L))
  z <- mgfp_bounds(x)
  testthat::expect_equal(nrow(z), 3L)
  testthat::expect_equal(z$family_size, c(2L, 2L, 1L))
  testthat::expect_equal(z$simultaneous_lower[2], binom.test(1000, 1000, conf.level = .975)$conf.int[1])
  testthat::expect_equal(z$simultaneous_upper[1], binom.test(0, 1000, conf.level = .975)$conf.int[2])
  testthat::expect_equal(z$simultaneous_lower[3], binom.test(950, 1000)$conf.int[1])
  testthat::expect_error(mgfp_bounds(transform(x, attained = 1001L)), "Invalid")
})

testthat::test_that("paired comparison selects each comparator and preserves separate uncertainty definitions", {
  x <- publication_fixture(); z <- do.call(mgfp_aggregate, x)
  testthat::expect_equal(z$paired_cells$mean_width_difference, c(3, 2.8, 2.5))
  testthat::expect_equal(z$paired_cells$gibbs_shorter_datasets, rep(0L, 3))
  testthat::expect_equal(z$paired_cells$attainment_difference, c(0, 1, 0))
  testthat::expect_equal(z$median_sensitivity$median_endpoint_width_mean, 4.6)
  testthat::expect_equal(z$median_sensitivity$median_endpoint_attainment, .5)
  testthat::expect_equal(z$median_sensitivity$mean_endpoint_attainment, 1)
  testthat::expect_true(all(is.na(z$application_partitions$lower_95)))
  testthat::expect_equal(sum(z$geometry$datasets), 4L)
})

testthat::test_that("hash checks validate both historical ordered arrays and named current records", {
  paths <- c(tempfile(), tempfile()); on.exit(unlink(paths))
  writeLines("one", paths[1]); writeLines("two", paths[2])
  values <- vapply(paths, mgfa_hash, "")
  testthat::expect_silent(mgfp_check_hashes(unname(values), paths))
  testthat::expect_silent(mgfp_check_hashes(as.list(setNames(values, paths))))
  testthat::expect_error(mgfp_check_hashes(unname(values)), "Missing or unnamed")
  testthat::expect_error(mgfp_check_hashes(unname(rev(values)), paths), "hash differs")
  writeLines("changed", paths[1])
  testthat::expect_error(mgfp_check_hashes(values, paths), "hash differs")
})

testthat::test_that("illustrative chains remain bound to their data and fixed target", {
  directory <- tempfile(); dir.create(file.path(directory, "chains/0000"), recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE))
  task <- data.frame(case_id = "fixture", job_id = 1L, dataset_sha256 = "sample", q = .9)
  manifest <- list(identity = list(commit = "fixture"), base_configuration = list(n_mcmc = 4L))
  path <- file.path(directory, "chains/0000/chain_000001.rds")
  saveRDS(list(sample_sha256 = "sample", draws = cbind(lower = 1:4, upper = 2:5)), path)
  jsonlite::write_json(list(sha256 = mgfa_hash(path), task_sha256 = digest::digest(task, algo = "sha256"),
    analysis_identity = digest::digest(manifest$identity, algo = "sha256")), paste0(path, ".json"), auto_unbox = TRUE)
  testthat::expect_equal(nrow(mgfp_read_chain(directory, task, manifest)$draws), 4L)
  task$q <- .95
  testthat::expect_error(mgfp_read_chain(directory, task, manifest), "receipt or observations differ")
})

testthat::test_that("simulation tables group labels and report raw percentile ranges without extra metrics", {
  designs <- data.frame(n = c(50, 100, 100, 500, 500, 500, 1000, 1000, 1000),
    content = c(.9, .9, .95, .9, .95, .99, .9, .95, .99))
  cells <- do.call(rbind, lapply(names(mgfp_families), function(family)
    do.call(rbind, lapply(1:9, function(i) data.frame(response_id = family, n = designs$n[i],
      content = designs$content[i], method = names(mgfp_methods), datasets = 1000L,
      attainment_rate = .97, width_q025 = 1, width_q975 = 5)))))
  path <- tempfile(); on.exit(unlink(path))
  mgfp_simulation_table(cells, path); text <- readLines(path)
  testthat::expect_equal(sum(grepl("^1000 & 0.99 & MTI-ECM", text)), 8L)
  testthat::expect_equal(sum(grepl("^ &  & ", text)), 216L)
  testthat::expect_equal(sum(grepl("1.000--5.000", text, fixed = TRUE)), 288L)
  testthat::expect_false(any(grepl("Fail-closed|Median sec|Width/|DGPs|article-facing", text)))
  testthat::expect_equal(sum(grepl("\\endhead", text, fixed = TRUE)), 8L)
})

testthat::test_that("design groups have larger gaps than within-group method spacing and Wilks stays solid black", {
  x <- expand.grid(n = c(50, 100), content = .9, method = names(mgfp_methods), stringsAsFactors = FALSE)
  a <- mgfp_layout(x)
  first <- sort(a$y[x$n == 100]); second <- sort(a$y[x$n == 50])
  testthat::expect_gt(min(second) - max(first), max(diff(first)))
  testthat::expect_identical(unname(mgfp_colors["wilks"]), "black")
  testthat::expect_identical(mgfp_range(1, 5), "1.000--5.000")
})

testthat::test_that("versioned output filenames prevent old generators from overwriting full-study article inputs", {
  code <- readLines(file.path(root, "application/scripts/110_review_mti_gibbs.R"))
  testthat::expect_false(any(grepl("mti_gibbs_full_assessment|figS_mti_full_", code)))
  cfg <- mgfa_json(file.path(root, "application/config/mti_gibbs_full_reporting_20261113.json"))
  testthat::expect_false(cfg$new_tolerance_guarantee)
  testthat::expect_false(cfg$replace_ecm_results)
  testthat::expect_equal(length(cfg$example_case_ids), 4L)
  testthat::expect_silent(withr::with_dir(root, mgfp_prior_verify(cfg)))
})

testthat::test_that("completed evidence preserves the full design and distinct posterior summaries", {
  withr::with_dir(root, {
    cfg <- mgfa_json("application/config/mti_gibbs_full_reporting_20261113.json")
    s <- mgfp_verify(cfg)
    read <- function(name) read.csv(file.path(cfg$evidence_dir, paste0(name, ".csv")))
    cells <- read("simulation_cells"); app <- read("application_partitions")
    testthat::expect_equal(nrow(cells), 8L * 9L * 4L)
    testthat::expect_equal(nrow(app), 2L * 5L * 4L)
    testthat::expect_true(all(cells$datasets == 1000L))
    testthat::expect_true(all(cells$family_size == 72L))
    testthat::expect_equal(s$completion$datasets, 75004L)
    testthat::expect_equal(s$chains, 4L * s$completion$datasets)
    testthat::expect_equal(sum(read("geometry")$datasets), s$completion$datasets)
    testthat::expect_true(all(cells$width_q975 >= cells$width_q025))
    testthat::expect_true(all(is.na(app$lower_95)))
    testthat::expect_equal(nrow(read("median_sensitivity")), 72L)
    ranks <- read("rank_example")
    testthat::expect_equal(as.vector(tapply(ranks$count, ranks$variable, sum)), rep(16000L, 4L))
    receipt <- mgfa_json(file.path(cfg$evidence_dir, "render_provenance.json"))
    pdfs <- names(receipt$outputs)[grepl("\\.pdf$", names(receipt$outputs))]
    testthat::expect_length(pdfs, 8L)
    testthat::expect_true(all(file.info(pdfs)$size > 1000L))
  })
})

testthat::test_that("both manuscripts use the complete assessment without overwriting the ECM comparison", {
  main <- readLines(file.path(root, "main.tex"))
  supplement <- readLines(file.path(root, "rqr-gibbs-supplement.tex"))
  includes <- function(lines, filename) any(grepl(paste0("\\input{tables/", filename, "}"), lines, fixed = TRUE))
  testthat::expect_true(includes(main, "mti_gibbs_full_numbers.tex"))
  testthat::expect_true(includes(supplement, "mti_gibbs_full_numbers.tex"))
  testthat::expect_true(includes(main, "mti_gibbs_full_assessment_main.tex"))
  testthat::expect_true(includes(supplement, "mti_gibbs_full_assessment_supplement.tex"))
  testthat::expect_true(includes(supplement, "mti_gibbs_full_validation_supplement.tex"))
  testthat::expect_true(includes(supplement, "mti_gibbs_full_application_comparison.tex"))
  testthat::expect_true(includes(main, "tolerance_validation_article_scenario_ranges.tex"))
  testthat::expect_false(includes(main, "mti_gibbs_comparison_main.tex"))
  testthat::expect_false(includes(supplement, "mti_gibbs_comparison_supplement.tex"))
  testthat::expect_false(includes(supplement, "tolerance_validation_article_dgp_delivery.tex"))
  testthat::expect_false(includes(supplement, "tolerance_validation_article_dgp_width_ranges.tex"))
})
