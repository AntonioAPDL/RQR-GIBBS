root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_full_comparison.R")) "." else "../../..")
for (file in c("pharma_tolerance_application.R", "pharma_mti_application.R",
    "mti_profile_revision.R", "mti_profile_review.R", "mti_final_reporting.R",
    "mti_ecm_recovery.R", "mti_gibbs_comparison.R", "mti_gibbs_practical.R",
    "mti_gibbs_review.R", "pinned_exdqlm_runtime.R", "mti_gibbs_full_comparison.R"))
  source(file.path(root, "application/scripts/lib", file))

fixture <- function() {
  y <- c(-1, -.2, .3, 1.5)
  base <- jsonlite::read_json(file.path(root, "application/config/mti_gibbs_ecm_comparison_20261111.json"),
    simplifyVector = TRUE)
  base$n_burn <- 10L; base$n_mcmc <- 100L; base$prior_variance <- .7
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_gibbs_full_comparison_20261111.json"),
    simplifyVector = TRUE)
  cfg$batch_chains <- 4L; cfg$disk_reserve_gib <- 0; cfg$report_reserve_gib <- 0
  cfg$inode_reserve <- 0L
  task <- data.frame(dataset_id = 1L, case_id = "fixture", source = "simulation",
    response_id = "fixture", n = 4L, content = .8, tolerance_confidence = .95,
    replication = 1L, data_seed = 42L, dataset_sha256 = mgc_hash(y), q = .9, delta = -.01,
    ecm_lower = -1, ecm_upper = 1.5, split_scheme = "", split_id = NA_integer_,
    diagnostic_role = "full", chain = 1L, seed = 731L, job_id = 1L)
  jobs <- task[rep(1L, 4L), ]; jobs$chain <- 1:4; jobs$seed <- 731:734; jobs$job_id <- 1:4
  rownames(jobs) <- NULL
  list(y = y, base = base, cfg = cfg, task = jobs[1, , drop = FALSE], jobs = jobs,
    identity = list(commit = "fixture", settings = "fixture"),
    context = list(laws = list(fixture = list(r = function(n) y[seq_len(n)], p = pnorm))))
}

testthat::test_that("marginal continuation exactly reproduces uninterrupted draws and RNG", {
  x <- fixture()
  for (delta in c(0, -.03, .03)) {
    x$task$delta <- delta
    first <- mgp_slice_fit(x$y, x$task, x$base, n_keep = 30L)
    extra <- mgf_continue(x$y, x$task, x$base, first, 70L)
    whole <- mgp_slice_fit(x$y, x$task, x$base, n_keep = 100L)
    testthat::expect_identical(rbind(first$draws, extra$draws), whole$draws)
    testthat::expect_identical(c(first$retained_objective, extra$retained_objective), whole$retained_objective)
    testthat::expect_identical(extra$last, whole$last)
  }
})

testthat::test_that("compact records preserve all draws and reject corruption or wrong identities", {
  x <- fixture(); directory <- tempfile("full-gibbs-")
  on.exit(unlink(directory, recursive = TRUE))
  fit <- mgp_slice_fit(x$y, x$task, x$base)
  record <- mgf_compact(fit, x$task, x$y, x$identity, 0, 24L)
  path <- mgf_path(directory, x$task$job_id)
  testthat::expect_silent(mgf_write(record, path, x$task, x$identity, 100L, x$y, x$base))
  read <- mgf_read(path, x$task, x$identity, 100L, x$y, x$base)
  testthat::expect_identical(read$draws, fit$draws)
  testthat::expect_identical(read$last, fit$last)
  testthat::expect_null(read$warmup_root1)
  testthat::expect_error(mgf_read(path, x$task, list(commit = "wrong"), 100L), "Corrupt or incompatible")
  testthat::expect_error(mgf_read(path, x$task, x$identity, 99L), "Corrupt or incompatible")
  bad <- record; bad$objective_checks$values[1] <- bad$objective_checks$values[1] + 1
  testthat::expect_error(mgf_validate(bad, x$task, x$identity, 100L, x$y, x$base), "objective check failed")
  bad <- record; bad$parent_sha256 <- "wrong"
  saveRDS(bad, path)
  testthat::expect_error(mgf_read(path, x$task, x$identity, 100L), "Corrupt or incompatible")
})

testthat::test_that("resuming a queue reuses complete records and is scheduling-independent", {
  x <- fixture(); directory <- tempfile("full-queue-")
  dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  testthat::expect_equal(mgf_queue(x$jobs, x$cfg, x$base, x$context, x$identity, directory, 2L), 4)
  paths <- vapply(x$jobs$job_id, function(id) mgf_path(directory, id), "")
  hashes <- vapply(paths, mgc_file_hash, "")
  testthat::expect_equal(mgf_queue(x$jobs[4:1, ], x$cfg, x$base, x$context, x$identity, directory, 1L), 4)
  testthat::expect_identical(hashes, vapply(paths, mgc_file_hash, ""))
  fit <- mgp_slice_fit(x$y, x$task, x$base)
  testthat::expect_identical(mgf_read(paths[1], x$task, x$identity, 100L)$draws, fit$draws)
  blocked <- file.path(directory, "stopped"); dir.create(blocked)
  file.create(file.path(blocked, "STOP_REQUESTED"))
  testthat::expect_error(mgf_queue(x$jobs, x$cfg, x$base, x$context, x$identity, blocked, 1L),
    "Graceful stop requested")
})

testthat::test_that("worker failures remain recorded and prevent silent scheduling progress", {
  x <- fixture(); directory <- tempfile("full-error-")
  dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  x$jobs$dataset_sha256[1] <- "changed"
  testthat::expect_error(mgf_queue(x$jobs, x$cfg, x$base, x$context, x$identity, directory, 2L), "worker failed")
  testthat::expect_true(file.exists(paste0(mgf_path(directory, 1), ".error.json")))
  testthat::expect_false(file.exists(paste0(mgf_path(directory, 1), ".json")))
  testthat::expect_true(file.exists(paste0(mgf_path(directory, 2), ".json")))
})

testthat::test_that("posterior mean and median endpoint summaries retain their distinct definitions", {
  x <- fixture(); directory <- tempfile("full-assess-")
  dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  mgf_queue(x$jobs, x$cfg, x$base, x$context, x$identity, directory, 2L)
  practical <- jsonlite::read_json(file.path(root, "application/config/mti_gibbs_practical_20261112.json"),
    simplifyVector = TRUE)
  result <- mgf_assess_one(x$jobs, x$cfg, x$base, practical, x$context, x$identity, directory, FALSE)
  d <- result$parameters
  testthat::expect_equal(result$dataset$mean_width, d$mean[d$variable == "width"], tolerance = 1e-12)
  testthat::expect_equal(result$dataset$median_interval_width,
    d$median[d$variable == "upper"] - d$median[d$variable == "lower"])
  testthat::expect_equal(result$dataset$mean_content,
    pnorm(result$dataset$mean_upper) - pnorm(result$dataset$mean_lower))
})

testthat::test_that("extensions bind to their exact parent and reassess all four combined chains", {
  x <- fixture(); x$cfg$extension_draws <- 100L
  directory <- tempfile("full-extension-")
  dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  mgf_queue(x$jobs, x$cfg, x$base, x$context, x$identity, directory, 2L)
  before <- mgc_file_hash(mgf_path(directory, 1L))
  mgf_queue(x$jobs, x$cfg, x$base, x$context, x$identity, directory, 2L, TRUE)
  practical <- jsonlite::read_json(file.path(root, "application/config/mti_gibbs_practical_20261112.json"),
    simplifyVector = TRUE)
  d <- mgf_assess_one(x$jobs, x$cfg, x$base, practical, x$context, x$identity, directory, TRUE)
  testthat::expect_true(d$dataset$extended)
  testthat::expect_equal(d$dataset$retained_per_chain, 200)
  testthat::expect_identical(before, mgc_file_hash(mgf_path(directory, 1L)))
  path <- mgf_path(directory, 1L, TRUE)
  extra <- mgf_read(path, x$task, x$identity, 100L)
  extra$parent_sha256 <- "changed"
  mgf_write(extra, path, x$task, x$identity, 100L, x$y, x$base)
  testthat::expect_error(mgf_assess_one(x$jobs, x$cfg, x$base, practical, x$context,
    x$identity, directory, TRUE), "parent digest changed")
})

testthat::test_that("resource checks and qualification prevent an unsafe automatic full launch", {
  x <- fixture()
  testthat::expect_silent(mgf_space(x$cfg, 300016L, tempdir(), available = 1e12, free_inodes = 1e9))
  testthat::expect_error(mgf_space(x$cfg, 1000, tempdir(), available = 0, free_inodes = 1e9), "Storage limit")
  testthat::expect_error(mgf_space(x$cfg, 1000, tempdir(), available = 1e12, free_inodes = 0), "inodes")
  testthat::expect_error(mgf_qualification_gate(list(passed = FALSE), x$identity), "has not passed")
  q <- list(passed = TRUE, analysis_identity = mgc_hash(x$identity))
  testthat::expect_silent(mgf_qualification_gate(q, x$identity))
  testthat::expect_error(mgf_qualification_gate(q, list(commit = "changed")), "has not passed")
  code <- deparse(body(mgf_campaign))
  testthat::expect_lt(grep("mgf_qualify", code)[1], grep("mgf_queue", code)[1])
})

testthat::test_that("controller locks distinguish live owners from recoverable dead owners", {
  directory <- tempfile("full-lock-")
  lock <- mgc_lock(directory)
  on.exit(unlink(c(directory, lock), recursive = TRUE))
  testthat::expect_error(mgc_lock(directory), "locked")
  testthat::expect_error(mgc_lock(directory, recover = TRUE), "live or unidentified")
  mgf_json_write(list(pid = .Machine$integer.max, token = "missing",
    host = Sys.info()[["nodename"]]), file.path(lock, "owner.json"))
  testthat::expect_equal(mgc_lock(directory, recover = TRUE), lock)
})

testthat::test_that("application summaries avoid independent-binomial claims for overlapping partitions", {
  d <- data.frame(source = c("application", "application", "simulation", "simulation"),
    response_id = "fixture", n = 50, content = .9, tolerance_confidence = .95,
    method = "mti_gibbs_mean", split_scheme = c("random", "random", "", ""),
    attained = c(TRUE, FALSE, TRUE, TRUE), width = c(1, 2, 3, 4), available = TRUE)
  cells <- mgf_cell_summary(d)
  testthat::expect_true(is.na(cells$lower_95[cells$source == "application"]))
  testthat::expect_true(is.finite(cells$lower_95[cells$source == "simulation"]))
  testthat::expect_equal(cells$attainment_rate[cells$source == "application"], .5)
})

testthat::test_that("campaign writes are restricted to RQR-owned output directories", {
  testthat::expect_error(mgf_output_guard(tempdir()), "RQR-owned")
  testthat::expect_silent(mgf_output_guard(file.path(mgf_root(), ".codex_work",
    "guard-fixture", "application/outputs/full-study")))
})
