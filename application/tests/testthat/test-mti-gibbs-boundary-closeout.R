root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_boundary_closeout.R")) "." else "../../..")
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "mti_gibbs_boundary_closeout", "mti_gibbs_boundary_closeout_figures"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))

testthat::test_that("replay compares every candidate and eligibility without changing tolerances", {
  d <- data.frame(candidate_index = 1:2, target_content = c(.97, .98), mean_tilt = c(0,.1),
    lower = c(-2,-2.1), upper = c(2,2.1), observed_count = c(97L,98L), ecm_objective = c(20,21),
    posterior_content_probability = c(.995,.996), numerically_accurate = TRUE,
    posterior_constraint_satisfied = TRUE)
  testthat::expect_true(all(mbx_candidate_comparison(d, d[2:1, ], 1e-10)$maximum_difference == 0))
  b <- d; b$lower[1] <- b$lower[1]+1e-6
  testthat::expect_error(mbx_candidate_comparison(d,b,1e-10), "calculations differ")
  b <- d; b$posterior_constraint_satisfied[1] <- FALSE
  testthat::expect_error(mbx_candidate_comparison(d,b,1e-10), "calculations differ")
  testthat::expect_error(mbx_candidate_comparison(d,d[1, ],1e-10), "definitions differ")
})

testthat::test_that("confirmation is preassigned and does not branch on outcomes", {
  cfg <- jsonlite::read_json(file.path(root,"application/config/mti_gibbs_boundary_closeout_20261116.json"),
    simplifyVector = TRUE)
  context <- list(scan = data.frame(n = 500L, content = .95, retained_fraction = .99))
  tasks <- mbx_confirmation_tasks(cfg, context)
  testthat::expect_equal(nrow(tasks), 2000L)
  testthat::expect_false(anyDuplicated(tasks$data_seed) > 0)
  testthat::expect_true(all(tasks$n == 500 & tasks$content == .95 & tasks$tolerance_confidence == .95))
  testthat::expect_true(all(tasks$data_seed == 1920000000 + seq_len(2000)))
  testthat::expect_false(cfg$prior_tuning)
  testthat::expect_false(cfg$replace_original_results)
})

testthat::test_that("confirmation uses all trials and retains raw sampling width quantiles", {
  d <- data.frame(response_id = "normal", n = 500L, content = .95, tolerance_confidence = .95,
    method = "ecm", attained = c(TRUE,TRUE,FALSE,FALSE), returned = c(TRUE,TRUE,TRUE,FALSE), width = 1:4)
  z <- mbx_bounds(d)
  testthat::expect_equal(z$attempts, 4L)
  testthat::expect_equal(z$successes, 2L)
  testthat::expect_equal(z$returned, 3L)
  testthat::expect_equal(z$attainment, .5)
  testthat::expect_equal(z$width_q025, unname(quantile(1:4,.025)))
  testthat::expect_equal(z$width_q975, unname(quantile(1:4,.975)))
  testthat::expect_equal(z$lower_95, binom.test(2,4)$conf.int[1])
  testthat::expect_equal(z$upper_95, binom.test(2,4)$conf.int[2])
})

testthat::test_that("diagnostic rendering separates the legend and preserves all supplied cases", {
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory, recursive = TRUE))
  cfg <- list(evidence_root = file.path(directory,"data"), figure_root = file.path(directory,"figures"))
  dir.create(cfg$evidence_root)
  d <- data.table::CJ(response_id = c("normal","student_t3"), n = c(100L,500L), content = .95,
    method = c("gibbs_calibrated","gibbs_unscaled","ecm","young_mathew","wilks"))
  d[, `:=`(tolerance_confidence = .95, width_q025 = 2, width_q975 = 5,
    mean_width = 3, lower_95 = .91, upper_95 = 1, attainment = .98)]
  data.table::fwrite(d,file.path(cfg$evidence_root,"comparison_summary.csv"))
  jsonlite::write_json(list(test = TRUE),file.path(cfg$evidence_root,"summary.json"))
  render <- mbx_render; environment(render) <- new.env(parent = environment(mbx_render))
  environment(render)$mbx_verify <- function(...) list(confirmation = data.frame(method = "ecm",successes = 1900,
    attempts = 2000, lower_95 = .939, upper_95 = .959))
  before <- getwd(); setwd(directory); on.exit(setwd(before), add = TRUE)
  dir.create("application/scripts/lib", recursive = TRUE)
  file.copy(file.path(root,"application/scripts/lib/mti_gibbs_boundary_closeout_figures.R"),
    "application/scripts/lib/mti_gibbs_boundary_closeout_figures.R")
  render(cfg)
  testthat::expect_true(file.info(file.path(cfg$figure_root,"gibbs_boundary_pilot_review.pdf"))$size > 0)
  testthat::expect_true(file.exists(file.path(cfg$evidence_root,"rendered.json")))
  testthat::expect_true(any(grepl("1900",readLines("tables/mti_gibbs_boundary_20261116/numbers.tex"))))
})
