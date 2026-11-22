root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_skewness.R")) "." else "../../..")
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis", "pharma_tolerance_application", "pharma_mti_application",
  "mti_profile_revision", "mti_profile_review", "mti_gibbs_full_comparison",
  "mti_skewness_distributions", "mti_skewness_extension", "mti_skewness_reporting",
  "mti_gibbs_skewness", "mti_gibbs_skewness_checks", "mti_gibbs_skewness_reporting"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))

mgs_fixture <- function() {
  cfg <- mwd_json(file.path(root, "application/config/mti_gibbs_skewness_20261122.json"))
  cfg$sampling$n_burn <- 10L; cfg$sampling$budgets <- c(100L, 200L)
  cfg$expected_datasets <- 2L; cfg$replications <- 2L
  task <- data.frame(job_id = 1L, case_id = "fixture", law_id = "normal", n = 6L, content = .9,
    tolerance_confidence = .95, replication = 1L, q = .95, delta = -.02,
    ecm_lower = -1, ecm_upper = 2, sample_sha256 = mwd_hash(c(-1, -.2, 0, .5, 1, 2)), parent_sha256 = "parent")
  y <- c(-1, -.2, 0, .5, 1, 2)
  old <- data.table::data.table(method = c("ecm", "young_mathew", "wilks"),
    law_id = "normal", n = 6L, content = .9, tolerance_confidence = .95, returned = TRUE,
    lower = -1, upper = 2, width = 3, population_content = pnorm(2) - pnorm(-1), attained = FALSE)
  input <- list(y = y, sample_sha256 = task$sample_sha256, parent_sha256 = "parent", rows = old, law = list(p = pnorm))
  policy <- mwd_json(file.path(root, cfg$diagnostic_config))$policy
  # Small persistence fixtures do not replace the production diagnostic policy.
  policy$preferred_rhat <- 5; policy$preferred_ess <- 1; policy$preferred_mcse_ratio <- 1
  policy$relative_width_resolution <- 1
  list(cfg = cfg, task = task, input = input, context = list(policy = policy, laws = list(normal = list(p = pnorm))))
}

testthat::test_that("production specification preserves the fixed prior and original sampler budgets", {
  withr::local_dir(root)
  cfg <- mgs_config()
  testthat::expect_false(cfg$refit_ecm)
  testthat::expect_false(cfg$automatic_prior_tuning)
  testthat::expect_false(cfg$automatic_article_replacement)
  testthat::expect_equal(cfg$sampling$prior_variance, 4)
  testthat::expect_equal(cfg$sampling$budgets, c(1000, 2000, 4000))
  testthat::expect_equal(cfg$expected_datasets, 36000)
  testthat::expect_true(all(file.exists(mgs_sources)))
  launcher <- paste(readLines("application/scripts/149_launch_mti_gibbs_skewness.sh"), collapse = "\n")
  for (term in c("flock -n", "--resume", "git worktree add --detach", "controller_exit.json", "setsid --fork"))
    testthat::expect_true(grepl(term, launcher, fixed = TRUE))
  testthat::expect_equal(system2("bash", c("-n", "application/scripts/149_launch_mti_gibbs_skewness.sh")), 0L)
})

testthat::test_that("continuation preserves the uninterrupted chain and its MTI objective", {
  x <- mgs_fixture()
  init <- mwd_initial(x$input$y, -1, 2, 4, 0)
  settings <- c(x$cfg$sampling, list(initial_midpoint_offsets_sd = init$offsets, initial_halfwidth_multipliers = init$radii))
  t <- data.frame(chain = 1L, seed = 1740000011L, q = .95, delta = -.02, ecm_lower = -1, ecm_upper = 2)
  first <- mgp_slice_fit(x$input$y, t, settings, n_keep = 100)
  continued <- mgs_extend(x$input$y, t, settings, first, 200)
  whole <- mgp_slice_fit(x$input$y, t, settings, n_keep = 200)
  testthat::expect_identical(continued$draws, whole$draws)
  testthat::expect_identical(continued$last, whole$last)
  testthat::expect_identical(continued$retained_objective, whole$retained_objective)
  testthat::expect_equal(continued$target_loss_trace, whole$target_loss_trace, tolerance = 1e-14)
  record <- list(task = t, fit = continued, sample_sha256 = x$input$sample_sha256)
  testthat::expect_silent(mgs_validate_chain(record, x$input$y, x$cfg, 200))
  record$fit$retained_objective[1] <- record$fit$retained_objective[1] + 1
  testthat::expect_error(mgs_validate_chain(record, x$input$y, x$cfg, 200), "objective differs")
  testthat::expect_error(mgs_extend(x$input$y, t, settings, first, 100), "must add")
})

testthat::test_that("saved stages are immutable, recover valid atomic writes and reject corruption", {
  x <- mgs_fixture(); directory <- withr::local_tempdir()
  value <- list(task = x$task, identity = "fixture", sample_sha256 = x$input$sample_sha256, marker = 1L)
  path <- file.path(directory, "record.rds")
  mgs_write(value, path, "fixture")
  validate <- function(z) mwd_require(z$marker == 1L, "Invalid fixture marker.")
  testthat::expect_identical(mgs_read(path, x$task, "fixture", validate), value)
  testthat::expect_error(mgs_write(value, path, "fixture"), "overwrite")
  before <- mwd_file_hash(path); unlink(paste0(path, ".json"))
  testthat::expect_identical(mgs_read(path, x$task, "fixture", validate), value)
  testthat::expect_equal(mwd_file_hash(path), before)
  testthat::expect_true(mwd_json(paste0(path, ".json"))$recovered_atomic_write)
  testthat::expect_error(mgs_read(path, x$task, "wrong", validate), "identity")
  value$marker <- 2L; saveRDS(value, path)
  testthat::expect_error(mgs_read(path, x$task, "fixture", validate), "checksum")
  unlink(paste0(path, ".json"))
  testthat::expect_error(mgs_read(path, x$task, "fixture", validate), "Invalid fixture")
  testthat::expect_false(file.exists(paste0(path, ".json")))
})

testthat::test_that("sampling reuses fixed targets and resumes without rerunning chains", {
  x <- mgs_fixture(); directory <- withr::local_tempdir()
  first <- mgs_sample(x$task, x$input, x$cfg, x$context, directory, "fixture")
  paths <- list.files(directory, full.names = TRUE, recursive = TRUE)
  before <- mgfp_hashes(paths)
  second <- mgs_sample(x$task, x$input, x$cfg, x$context, directory, "fixture")
  testthat::expect_identical(first, second)
  testthat::expect_identical(before, mgfp_hashes(paths))
  testthat::expect_equal(first$row$q, x$task$q)
  testthat::expect_equal(first$row$delta, x$task$delta)
  testthat::expect_equal(first$row$population_content, pnorm(first$row$upper) - pnorm(first$row$lower))
  testthat::expect_equal(first$row$attained, first$row$population_content >= .9)
  testthat::expect_silent(mgs_validate_result(first, x$input, x$task, x$cfg))
  first$row$lower <- first$row$lower - 1
  testthat::expect_error(mgs_validate_result(first, x$input, x$task, x$cfg), "summaries differ")
})

testthat::test_that("dataset dispatch and resume preserve draws without ECM fitting", {
  x <- mgs_fixture(); a <- withr::local_tempdir(); b <- withr::local_tempdir()
  tasks <- x$task[rep(1, 2), ]; tasks$job_id <- 1:2; tasks$replication <- 1:2
  env <- new.env(parent = environment(mgs_run_jobs))
  env$mgs_input <- function(task, context, cfg) x$input
  env$mgs_resources <- function(...) list(workers = 2L)
  env$mfp_fit_data <- function(...) stop("ECM must never be fitted by this extension.")
  queue <- mgs_run_jobs; environment(queue) <- env
  queue(tasks, x$cfg, x$context, a, "fixture", 2L)
  paths <- list.files(a, full.names = TRUE, recursive = TRUE, pattern = "[.]rds$")
  before <- mgfp_hashes(paths)
  queue(tasks[2:1, ], x$cfg, x$context, a, "fixture", 1L)
  testthat::expect_identical(before, mgfp_hashes(paths))
  queue(tasks[2:1, ], x$cfg, x$context, b, "fixture", 2L)
  for (id in 1:2) for (chain in 1:4) {
    p <- mgs_chain_path(a, id, chain, 100L); q <- mgs_chain_path(b, id, chain, 100L)
    testthat::expect_identical(readRDS(p)$fit$draws, readRDS(q)$fit$draws)
  }
  blocked <- withr::local_tempdir(); file.create(file.path(blocked, "STOP_REQUESTED"))
  testthat::expect_error(queue(tasks, x$cfg, x$context, blocked, "fixture", 1L), "Graceful stop")
  prepared <- list(tasks = tasks, identity = "fixture")
  collector <- mgs_collect; environment(collector) <- env
  result <- collector(prepared, x$cfg, x$context, a, 2L)
  testthat::expect_equal(nrow(result$rows), 8L)
  testthat::expect_equal(nrow(result$parameters), 8L)
  testthat::expect_equal(nrow(result$diagnostics), 2L)
  testthat::expect_false(any(result$diagnostics$sampling_error))
  trace <- file.path(a, "trace.pdf")
  mgs_trace_pdf(prepared, x$cfg, a, result$diagnostics, trace)
  testthat::expect_gt(file.info(trace)$size, 5000)
  failed <- withr::local_tempdir()
  env$mgs_sample <- function(...) {warning("recorded fixture warning"); stop("deliberate sampler failure")}
  queue(tasks, x$cfg, x$context, failed, "fixture", 2L)
  bad <- collector(prepared, x$cfg, x$context, failed, 2L)
  testthat::expect_true(all(bad$diagnostics$sampling_error))
  testthat::expect_true(all(bad$diagnostics$warnings == 1L))
  testthat::expect_true(all(!bad$rows[method == "gibbs", returned]))
  testthat::expect_true(all(!bad$rows[method == "gibbs", attained]))
})

testthat::test_that("expanded bounds preserve old estimates and use 108 cases per method", {
  withr::local_dir(root)
  cfg <- mgs_config(); old <- mgs_historical_cells(cfg)
  testthat::expect_equal(nrow(old), 288L)
  testthat::expect_true(all(is.finite(old$mcse)))
  primary <- data.table::fread(file.path(cfg$primary_evidence, "primary_cells.csv"))
  added <- primary[law_id %in% cfg$law_ids]
  gibbs <- data.table::copy(added[method == "ecm"])[, method := "gibbs"]
  added <- data.table::rbindlist(list(added, gibbs))
  fields <- names(old)
  combined <- mgs_bounds(data.table::rbindlist(list(old, added[, ..fields])))
  testthat::expect_equal(nrow(combined), 432L)
  testthat::expect_equal(unique(combined$family_size), 108L)
  testthat::expect_equal(combined[method == "gibbs" & law_id == "student_t3" & n == 500 & content == .99, attained], 931L)
  testthat::expect_equal(combined[1, simultaneous_lower], qbeta(.05 / 216, combined$attained[1], 1001 - combined$attained[1]))
  path <- file.path(withr::local_tempdir(), "comparison.pdf")
  mgs_comparison_pdf(combined, cfg, path)
  testthat::expect_gt(file.info(path)$size, 5000)
  testthat::expect_equal(rawToChar(readBin(path, "raw", 4)), "%PDF")
})

testthat::test_that("paired comparisons retain sample-specific differences and numerical failures", {
  d <- data.table::CJ(job_id = 1:2, method = c("ecm", "gibbs", "young_mathew", "wilks"))
  d[, `:=`(law_id = "fixture", n = 50L, content = .9, tolerance_confidence = .95,
    returned = TRUE, width = c(ecm = 2, gibbs = 3, young_mathew = 4, wilks = 5)[method], attained = method != "gibbs")]
  paired <- mgs_paired(d)
  testthat::expect_equal(paired[comparator == "ecm", mean_width_difference], 1)
  testthat::expect_equal(paired[comparator == "young_mathew", mean_width_difference], -1)
  testthat::expect_true(all(paired$attainment_difference == -1))
  testthat::expect_equal(paired[comparator == "wilks", width_difference_mcse], 0)
  d[method == "gibbs" & job_id == 1, `:=`(returned = FALSE, width = NA_real_, attained = FALSE)]
  testthat::expect_true(all(mgs_paired(d)$paired_widths == 1L))
})

testthat::test_that("completion reporting preserves the original study and produces all four-method summaries", {
  withr::local_dir(root)
  cfg <- mgs_config(); output <- withr::local_tempdir(); primary_dir <- withr::local_tempdir()
  historical <- mgs_historical_cells(cfg)
  design <- data.table::fread("tables/tolerance_validation_article_scan_calibration.csv")
  rows <- data.table::CJ(law_id = cfg$law_ids, design_id = 1:9, replication = 1:1000,
    method = c("ecm", "gibbs", "young_mathew", "wilks"))
  rows[, `:=`(n = design$n[design_id], content = design$content[design_id], tolerance_confidence = .95,
    returned = TRUE, attained = replication <= 950L, width = 1 + replication / 1000 + (method == "gibbs"), study_id = "fixture")]
  rows[, job_id := .GRP, by = .(law_id, design_id, replication)]
  rows[, task_key := paste0("fixture:", job_id)]
  summary <- msx_summary(rows)
  fields <- names(historical)
  primary <- data.table::rbindlist(list(historical[method != "gibbs"], summary[method != "gibbs", ..fields]))
  pma_write_csv(as.data.frame(primary), file.path(primary_dir, "primary_cells.csv"))
  file.copy(file.path(cfg$primary_evidence, "population_content_references.csv"), primary_dir)
  cfg$primary_evidence <- primary_dir
  fixture <- list(rows = rows, parameters = data.table::data.table(variable = "fixture"),
    diagnostics = data.table::data.table(job_id = 1:36000, adequate = TRUE, sampling_error = FALSE))
  env <- new.env(parent = environment(mgs_report))
  env$mgs_collect <- function(...) fixture
  env$mgs_trace_pdf <- function(prepared, cfg, directory, diagnostics, path) {
    grDevices::pdf(path); graphics::plot.new(); grDevices::dev.off()
  }
  report <- mgs_report; environment(report) <- env
  result <- report(list(identity = "fixture"), cfg, list(), output)
  testthat::expect_true(result$numerically_adequate)
  combined <- data.table::fread(file.path(output, "report/combined_cells.csv"))
  testthat::expect_equal(nrow(combined), 432L)
  testthat::expect_equal(unique(combined$family_size), 108L)
  verification <- mwd_json(file.path(output, "verification.json"))
  testthat::expect_true(verification$original_primary_unchanged)
  testthat::expect_false(verification$article_replaced)
  testthat::expect_equal(verification$adequate_datasets, 36000L)
  testthat::expect_true(file.exists(file.path(output, "report/four_method_cases.tex")))
  testthat::expect_true(file.exists(file.path(output, "report/scientific_review.md")))
})
