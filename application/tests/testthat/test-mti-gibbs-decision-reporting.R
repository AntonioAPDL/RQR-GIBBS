root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_decision_reporting.R")) "." else "../../..")
for (name in c("mti_gibbs_full_report_review","mti_gibbs_full_publication","mti_gibbs_width_diagnosis",
  "mti_gibbs_decision_study","mti_gibbs_decision_reporting"))
  source(file.path(root,"application/scripts/lib",paste0(name,".R")))

testthat::test_that("completed reporting has one checked cohort with all cases and no averaged width ranges", {
  withr::local_dir(root)
  cfg <- mwd_json("application/config/mti_gibbs_decision_reporting_20261118.json")
  testthat::expect_true(mdr_verify(cfg))
  d <- data.table::fread(file.path(cfg$evidence_root,"comparison_summary.csv"))
  testthat::expect_error(mdr_validate(d[-1],cfg),"missing cases")
  testthat::expect_error(mdr_validate(rbind(d,d[1]),cfg),"missing cases")
  broken <- data.table::copy(d);broken[1,returned:=999L]
  testthat::expect_error(mdr_validate(broken,cfg),"missing cases")
  testthat::expect_equal(nrow(d[method=="gibbs_proposed"]),72L)
  testthat::expect_true(all(d$width_q975>d$width_q025))
  testthat::expect_true(all(d$attainment_lower<=d$attainment & d$attainment_upper>=d$attainment))
})

testthat::test_that("reported numbers reproduce the checked independent summaries", {
  withr::local_dir(root)
  cfg <- mwd_json("application/config/mti_gibbs_decision_reporting_20261118.json")
  p <- data.table::fread(file.path(cfg$evidence_root,"proposed_summary.csv"))
  s <- mwd_json(file.path(cfg$evidence_root,"numerical_summary.json"))
  numbers <- readLines(file.path(cfg$table_root,"numbers.tex"))
  testthat::expect_true(any(grepl(sprintf("{%.1f}",100*min(p$attainment)),numbers,fixed=TRUE)))
  testthat::expect_equal(s$pointwise_lower_above_p,sum(p$attainment_lower>.95))
  testthat::expect_equal(s$simultaneous_lower_above_p,sum(p$simultaneous_lower>.95))
  testthat::expect_equal(s$datasets,72000L)
  testthat::expect_equal(s$posterior_records,144000L)
  table <- readLines(file.path(cfg$table_root,"comparison.tex"))
  testthat::expect_equal(sum(grepl("n=",table,fixed=TRUE)),9L)
  testthat::expect_false(any(grepl("median|seconds|Fail-closed|DGPs",table)))
  testthat::expect_equal(length(list.files(cfg$figure_root,pattern="[.]pdf$")),18L)
  testthat::expect_equal(qbinom(1-.05/144,1000,.95)+1,973)
})

testthat::test_that("altered reporting evidence is rejected", {
  withr::local_dir(root)
  cfg <- mwd_json("application/config/mti_gibbs_decision_reporting_20261118.json")
  cfg$completed_source <- "different"
  testthat::expect_error(mdr_verify(cfg),"cohort")
  cfg <- mwd_json("application/config/mti_gibbs_decision_reporting_20261118.json")
  cfg$completed_evidence_sha256 <- "different"
  testthat::expect_error(mdr_verify(cfg),"cohort")
})
