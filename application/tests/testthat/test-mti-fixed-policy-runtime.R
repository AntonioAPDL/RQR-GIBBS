test_that("installed runtime completes a paired all-design integration test and report", {
  e <- new.env(parent=globalenv())
  root <- normalizePath(testthat::test_path("..","..",".."))
  for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R"))
    sys.source(file.path(root,"application/scripts/lib",file),e)
  old <- getwd(); setwd(root); on.exit(setwd(old),add=TRUE)
  expect_identical(as.character(packageVersion("rqrgibbs")),"0.1.0.9037")
  expect_false("exdqlm" %in% loadedNamespaces())
  cfg <- jsonlite::read_json("application/config/mti_profile_revision_20261021.json",simplifyVector=FALSE)
  spec <- jsonlite::read_json("application/config/mti_profile_fixed_policy_evaluation_20261021.json",simplifyVector=FALSE)
  frozen <- jsonlite::read_json(cfg$validation_config,simplifyVector=FALSE)
  frozen$modes$confirmatory$dgp_ids <- list("normal","exponential")
  directory <- tempfile("fixed_runtime_",tmpdir="application/outputs")
  dir.create(directory,recursive=TRUE)
  e$pta_atomic_write_json(frozen,file.path(directory,"design.json"))
  cfg$validation_config <- file.path(directory,"design.json")
  cfg$validation$replications <- 1L
  cfg$validation$seed_base <- 4321000L
  design <- e$mpr_design(frozen)
  expect_silent(e$mpf_check_seeds(design,cfg))
  output <- file.path(directory,"validation"); dir.create(output)
  fits <- e$mp_simulation_stage(cfg,"validation",output,directory,2L,
    validation_policy_path=spec$policy_csv)
  expect_true(fits$passed)
  expect_equal(fits$completed_datasets,18L)
  expect_equal(fits$method_fits,72L)
  z <- read.csv(file.path(output,"results.csv"))
  hashes <- vapply(list.files(file.path(output,"checkpoints"),"[.]rds$",full.names=TRUE),
    e$mp_sha,character(1))
  e$mp_simulation_stage(cfg,"validation",output,directory,1L,
    validation_policy_path=spec$policy_csv)
  expect_identical(z,read.csv(file.path(output,"results.csv")))
  expect_identical(hashes,vapply(list.files(file.path(output,"checkpoints"),"[.]rds$",full.names=TRUE),
    e$mp_sha,character(1)))
  expect_equal(sum(!is.na(z$ecm_converged[z$method=="mti_ecm_historical"])),
    sum(z$returned[z$method=="mti_ecm_historical"]))
  expect_true(all(is.na(z$ecm_converged[z$method=="mti_profile_fractional_admissible_v1"])))
  identity <- list(commit="integration-test-only",policy_sha256=e$mp_sha(spec$policy_csv))
  assessment <- e$mpf_report(directory,design,cfg,spec,identity)
  expect_equal(assessment$expected_datasets,18L)
  expect_equal(assessment$method_evaluations,72L)
  expect_false(assessment$original_calibration_passed)
  expect_false(assessment$article_inputs_replaced)
  expect_true(file.exists(file.path(directory,"mti_profile_assessment.pdf")))
  expect_true(all(file.info(file.path(directory,c("REPORT.md","validation_paired_comparisons.csv")))$size>0))
  expect_equal(nrow(read.csv(file.path(directory,"validation_paired_comparisons.csv"))),54L)
  cat("\nIntegration diagnostics retained:",normalizePath(directory),"\n")
})
