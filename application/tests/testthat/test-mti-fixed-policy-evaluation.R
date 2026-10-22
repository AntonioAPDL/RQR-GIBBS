fixed_policy_env <- function() {
  e <- new.env(parent=globalenv())
  root <- normalizePath(testthat::test_path("..","..",".."))
  for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R"))
    sys.source(file.path(root,"application/scripts/lib",file),e)
  e$repo <- root
  e
}

fixed_policy_fixture <- function() {
  cells <- data.frame(n=c(50,100),content=c(.9,.95),tolerance_confidence=.95,
    threshold=c(NA,.97),interpretation=c("simultaneous_evidence_inconclusive","passed"))
  design <- cells[rep(1:2,each=2),c("n","content","tolerance_confidence")]
  design$cell_index <- 1:4
  design$dgp_id <- rep(c("normal","exponential"),2)
  assessment <- do.call(rbind,lapply(1:2,function(i) {
    z <- cells[rep(i,6),c("n","content","tolerance_confidence")]
    z$dgp_id <- rep(c("normal","exponential"),3)
    z$threshold <- rep(c(.95,.97,.99),each=2)
    z$replications <- 100L; z$returned <- 100L; z$successes <- 99L
    z$lower_bound <- rep(if(i==1)c(.90,.93,.93) else c(.94,.96,.97),each=2)
    z
  }))
  list(cells=cells,design=design,assessment=assessment)
}

test_that("fixed policy preserves passes and makes a deterministic exploratory choice", {
  e <- fixed_policy_env(); f <- fixed_policy_fixture()
  x <- e$mpf_policy(f$cells,f$assessment,f$design)
  expect_equal(x$policy$threshold,c(.97,.97))
  expect_identical(x$policy$original_calibration_passed,c(FALSE,TRUE))
  expect_identical(x$policy$selection,c("exploratory_calibration_maximin","preserved_original_pass"))
  expect_equal(sum(x$evidence$selected),2L)
  expect_equal(x,e$mpf_policy(f$cells[2:1,],f$assessment[12:1,],f$design),ignore_attr=TRUE)
  f$assessment$returned[f$assessment$n==50 & f$assessment$threshold==.97] <- 99L
  expect_equal(e$mpf_policy(f$cells,f$assessment,f$design)$policy$threshold,c(.99,.97))
})

test_that("policy rejects incomplete assessments, invalid passes, and lack of returns", {
  e <- fixed_policy_env(); f <- fixed_policy_fixture()
  expect_error(e$mpf_policy(f$cells,f$assessment[-1,],f$design),"missing family")
  expect_error(e$mpf_policy(f$cells,rbind(f$assessment,f$assessment[1,]),f$design),"duplicate")
  bad <- f$assessment; bad$returned[bad$n==50] <- 99L
  expect_error(e$mpf_policy(f$cells,bad,f$design),"No threshold")
  bad <- f$cells; bad$threshold[2] <- .95
  expect_error(e$mpf_policy(bad,f$assessment,f$design),"previously passed")
  bad <- f$cells; bad$threshold[1] <- .95
  expect_error(e$mpf_policy(bad,f$assessment,f$design),"unresolved")
  expect_error(e$mpf_policy(f$cells[1,,drop=FALSE],f$assessment,f$design),"Policy cells")
})

test_that("policy freeze is immutable and detects tampering", {
  e <- fixed_policy_env(); f <- fixed_policy_fixture()
  directory <- tempfile("frozen_policy_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  spec <- list(study_id="test",policy_rule="fixed",policy_csv=file.path(directory,"policy.csv"),
    threshold_evidence_csv=file.path(directory,"thresholds.csv"),
    policy_receipt=file.path(directory,"receipt.json"),upstream_inputs=list(hash="example"))
  v <- list(selection=e$mpf_policy(f$cells,f$assessment,f$design))
  e$mpf_freeze(spec,v)
  expect_silent(e$mpf_check_policy(spec,v))
  expect_error(e$mpf_freeze(spec,v),"already exist")
  changed <- read.csv(spec$policy_csv); changed$threshold[1] <- .96
  e$pma_write_csv(changed,spec$policy_csv)
  expect_error(e$mpf_check_policy(spec,v),"receipt mismatch")
})

test_that("independent seeds are disjoint and preserve unique tasks", {
  e <- fixed_policy_env(); f <- fixed_policy_fixture()
  cfg <- list(calibration=list(seed_base=1000,cell_seed_stride=100,
    replications=10,threshold_development_replications=2),
    validation=list(seed_base=10000,cell_seed_stride=100,replications=10))
  expect_silent(e$mpf_check_seeds(f$design,cfg))
  cfg$validation$seed_base <- 1000
  expect_error(e$mpf_check_seeds(f$design,cfg),"overlap")
  cfg$validation$cell_seed_stride <- 5
  expect_error(e$mpf_check_seeds(f$design,cfg),"stride")
})

test_that("old calibration stop is retained and selection ignores true content", {
  e <- fixed_policy_env()
  old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  cfg <- jsonlite::read_json("application/config/mti_profile_revision_20261021.json",simplifyVector=FALSE)
  directory <- tempfile("old_unresolved_"); dir.create(file.path(directory,"calibration"),recursive=TRUE)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  e$pma_write_csv(data.frame(threshold=NA_real_),file.path(directory,"calibration","policy.csv"))
  expect_error(e$mp_simulation_stage(cfg,"validation",directory,directory,1),"Calibration is unresolved")
  runner <- readLines("application/scripts/92_run_mti_profile_revision.R")
  expect_true(any(grepl('require_stage("calibration")',runner,fixed=TRUE)))
  z <- data.frame(profile_converged=TRUE,posterior_content_probability=c(.99,.999),
    width=c(1,2),target_content=.98,mean_tilt=0,central_tilt=0,lower=0,
    candidate_index=1:2,true_content=c(.1,1))
  selected <- e$mp_select(z,.95)$candidate_index
  z$true_content <- rev(z$true_content)
  expect_identical(e$mp_select(z,.95)$candidate_index,selected)
})

test_that("health reports datasets separately from four method evaluations", {
  e <- fixed_policy_env(); f <- fixed_policy_fixture()
  cfg <- list(validation=list(replications=1000,methods=letters[1:4]))
  directory <- tempfile("independent_health_")
  expect_equal(e$mpf_health(directory,f$design,cfg)$expected_datasets,4000)
  expect_equal(e$mpf_health(directory,f$design,cfg)$expected_method_evaluations,16000)
  expect_equal(e$mpf_health(directory,f$design,cfg)$state,"not_started")
  dir.create(file.path(directory,"validation","checkpoints"),recursive=TRUE)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  e$pta_atomic_write_json(list(completed_tasks=100),
    file.path(directory,"validation","checkpoints","progress.json"))
  expect_equal(e$mpf_health(directory,f$design,cfg)$remaining_datasets,3900)
  expect_equal(e$mpf_health(directory,f$design,cfg)$checkpointed_method_evaluations,400)
})

test_that("frozen source policy contains all nine cases and only three exploratory choices", {
  e <- fixed_policy_env()
  spec <- jsonlite::read_json(file.path(e$repo,
    "application/config/mti_profile_fixed_policy_evaluation_20261021.json"),simplifyVector=FALSE)
  p <- read.csv(file.path(e$repo,spec$policy_csv))
  receipt <- jsonlite::read_json(file.path(e$repo,spec$policy_receipt),simplifyVector=TRUE)
  expect_equal(nrow(p),9L)
  expect_equal(sum(p$original_calibration_passed),6L)
  expect_true(all(is.finite(p$threshold)))
  expect_identical(receipt$policy_sha256,e$mp_sha(file.path(e$repo,spec$policy_csv)))
  expect_false(receipt$original_calibration_passed)
  expect_false(spec$automatic_application_rerun)
  expect_false(spec$automatic_article_replacement)
})

test_that("new output isolation rejects nesting and existing symlink aliases", {
  e <- fixed_policy_env()
  directory <- tempfile("fixed_output_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  upstream <- file.path(directory,"calibration"); dir.create(upstream)
  review <- file.path(directory,"review"); dir.create(review)
  root <- file.path(directory,"new")
  spec <- list(output_root=root)
  expect_equal(e$mpf_check_output(file.path(root,"run"),spec,upstream,review),
    file.path(normalizePath(directory),"new","run"))
  expect_error(e$mpf_check_output(file.path(upstream,"run"),spec,upstream,review),"output root")
  dir.create(root)
  expect_true(file.symlink(upstream,file.path(root,"alias")))
  expect_error(e$mpf_check_output(file.path(root,"alias","run"),spec,upstream,review),"output root|disjoint")
})

test_that("upstream evidence hashes are checked before any fits or policy substitution", {
  e <- fixed_policy_env()
  directory <- tempfile("upstream_hash_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  revision <- file.path(directory,"revision.json")
  jsonlite::write_json(list(test=TRUE),revision)
  spec <- list(purpose="independent_empirical_comparison_not_original_calibration_pass",
    revision_config=revision,revision_config_sha256=e$mp_sha(revision),
    upstream_inputs=list(assessment.json="wrong-hash"))
  jsonlite::write_json(list(provenance=list(verified=TRUE)),file.path(directory,"assessment.json"))
  expect_error(e$mpf_verify_upstream(spec,list(),directory,directory,directory),"review hash mismatch")
  spec$revision_config_sha256 <- "wrong-hash"
  expect_error(e$mpf_verify_upstream(spec,list(),directory,directory,directory),"configuration changed")
})
