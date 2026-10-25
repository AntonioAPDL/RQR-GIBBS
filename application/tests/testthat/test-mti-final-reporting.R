final_reporting_env <- function() {
  e <- new.env(parent=globalenv())
  e$repo <- normalizePath(testthat::test_path("..","..",".."))
  for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R",
    "mti_q_grid_study.R","mti_final_study.R","mti_final_reporting.R"))
    sys.source(file.path(e$repo,"application/scripts/lib",file),e)
  e
}

test_that("independent review retains each method and exact within-family widths", {
  e <- final_reporting_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  spec <- jsonlite::read_json("application/config/mti_q_grid_refinement_final_20261022.json",simplifyVector=FALSE)
  cfg <- e$mpq_config(spec)
  cfg$validation$replications <- 2L
  cfg$validation$seed_base <- 1812200L
  design <- e$mpr_design(e$mp_frozen_profile(cfg)$validation)
  tasks <- e$mpq_tasks(design,cfg,"validation")
  policy <- unique(design[c("n","content","tolerance_confidence")])
  policy$threshold <- .95
  policy$method_id <- spec$primary_method
  policy$decision <- "passed_new_calibration"
  # These are test-only policy values; the fixture makes no calibration claim.
  retain <- Sys.getenv("MTI_FINAL_TEST_REVIEW_DIR","")
  if(nzchar(retain))dir.create(retain,recursive=TRUE,showWarnings=FALSE)
  directory <- tempfile("final_review_",tmpdir=if(nzchar(retain))retain else tempdir())
  dir.create(directory)
  if(!nzchar(retain))on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  dir.create(file.path(directory,"calibration"))
  e$pma_write_csv(policy,file.path(directory,"calibration/policy.csv"))
  hash <- e$mp_sha(file.path(directory,"calibration/policy.csv"))
  e$pta_atomic_write_json(list(passed=TRUE,policy_sha256=hash),
    file.path(directory,"calibration/completion.json"))
  validation <- e$mpq_stage("validation",directory,cfg,spec,design,1L)
  expect_true(validation$passed)
  expect_equal(validation$completed_datasets,nrow(tasks))
  expect_identical(validation$frozen_policy_sha256,hash)
  results <- read.csv(file.path(directory,"validation/results.csv"))
  expect_identical(validation$results_sha256,e$mp_sha(file.path(directory,"validation/results.csv")))
  checkpoints <- e$pma_file_record(list.files(file.path(directory,"validation/checkpoints"),"[.]rds$",full.names=TRUE))
  expect_equal(e$mpq_stage("validation",directory,cfg,spec,design,1L),validation)
  expect_identical(checkpoints,e$pma_file_record(checkpoints$path))
  output <- file.path(directory,"review")
  expect_silent(e$mpq_export_review(directory,output,cfg,spec,design))
  widths <- read.csv(file.path(output,"dgp_width_ranges.csv"))
  expect_setequal(unique(widths$method_id),c(spec$primary_method,"young_mathew","wilks_minmax"))
  expect_equal(nrow(widths),nrow(design)*3L)
  for(i in seq_len(nrow(widths))) {
    s <- widths[i,]
    x <- results[results$cell_index==s$cell_index & results$method==s$source_method_id & results$returned,"width"]
    expect_equal(c(s$width_q025,s$width_q975),e$mpr_quantiles(x),tolerance=1e-12)
    if(length(x))expect_equal(s$mean_width,mean(x),tolerance=1e-12)
  }
  ranges <- read.csv(file.path(output,"scenario_ranges.csv"))
  expect_equal(nrow(ranges),27L)
  expect_true(all(ranges$delivery_min<=ranges$delivery_max))
  for(file in c("fig04_tolerance_validation_primary.png","fig05_tolerance_validation_width_ranges.png")) {
    expect_true(file.exists(file.path(output,file)))
    expect_gt(file.info(file.path(output,file))$size,10000)
  }
  report <- e$mpr_read_json(file.path(output,"review_manifest.json"))
  expect_false(report$publication_replaced)
  expect_true(report$adoption_requires_scientific_review)
  before <- e$pma_file_record(list.files(output,full.names=TRUE))
  expect_silent(e$mpq_export_review(directory,output,cfg,spec,design))
  expect_identical(before,e$pma_file_record(before$path))
  e$pma_write_csv(results[-1,],file.path(directory,"validation/results.csv"))
  expect_error(e$mpq_export_review(directory,output,cfg,spec,design),"results changed")
  e$pma_write_csv(results,file.path(directory,"validation/results.csv"))
  if(nzchar(retain))cat("\nTest-only review figures retained:",normalizePath(output),"\n")
})

test_that("review cannot replace canonical tables or proceed without calibration", {
  e <- final_reporting_env()
  directory <- tempfile("final_review_guard_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  expect_error(e$mpq_export_review(directory,file.path(directory,"tables"),list(),list(),data.frame()),
    "isolated run/review")
  expect_error(e$mpq_export_review(directory,file.path(directory,"review"),list(),list(),data.frame()),
    "Complete calibration")
})
