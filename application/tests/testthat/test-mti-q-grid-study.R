q_study_env <- function() {
  e <- new.env(parent=globalenv())
  e$repo <- normalizePath(testthat::test_path("..","..",".."))
  for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R","mti_q_grid_study.R",
    "mti_final_study.R","mti_final_reporting.R"))
    sys.source(file.path(e$repo,"application/scripts/lib",file),e)
  e
}

test_that("new study roles and seeds are separate from the frozen experiments", {
  e <- q_study_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  spec <- jsonlite::read_json("application/config/mti_q_grid_refinement_20261022.json",simplifyVector=FALSE)
  cfg <- e$mpq_config(spec)
  design <- e$mpr_design(e$mp_frozen_profile(cfg)$validation)
  original <- jsonlite::read_json(spec$revision_config,simplifyVector=FALSE)
  expect_silent(e$mpq_seed_check(design,cfg,original))
  expect_equal(nrow(e$mpq_tasks(design,cfg,"screen")),72L)
  expect_equal(nrow(e$mpq_tasks(design,cfg,"calibration")),74304L)
  expect_equal(nrow(e$mpq_tasks(design,cfg,"validation")),72000L)
  expect_identical(unique(e$mpq_tasks(design,cfg,"screen")$role),"numerical_screen")
  retired <- cfg; retired$validation$seed_base <- cfg$retired_validation_seed_base
  expect_error(e$mpq_seed_check(design,retired,original),"overlap")
  cfg$validation$seed_base <- cfg$calibration$seed_base
  expect_error(e$mpq_seed_check(design,cfg,original),"overlap")
  bad <- spec; bad$calibration$replications <- 0
  expect_error(e$mpq_config(bad),"stage size")
  bad <- spec; bad$threshold_rule <- "reuse_previous"
  expect_error(e$mpq_config(bad),"Unsupported")
  bad <- spec; bad$validation$methods <- "refined_only"
  expect_error(e$mpq_config(bad),"comparison methods")
})

test_that("an unfinished baseline cannot authorize a new fitting stage", {
  e <- q_study_env(); old <- getwd(); on.exit(setwd(old),add=TRUE)
  root <- tempfile("grid_baseline_")
  run <- file.path(root,"application","outputs","baseline","independent")
  dir.create(run,recursive=TRUE); setwd(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  paths <- c("application/R/rqr_mti_intercept_profile.R",
    "application/R/rqr_mti_profile_selection.R","application/src/rqr_mti_intercept_profile.cpp")
  for(path in paths) {
    dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
    writeLines("unchanged",path)
  }
  writeLines("frozen", "policy.csv")
  spec <- list(baseline_commit="baseline",baseline_policy="policy.csv",
    baseline_policy_sha256=e$mp_sha("policy.csv"))
  empty <- data.frame(path=character(),sha256=character())
  identity <- list(commit="baseline",policy_sha256=spec$baseline_policy_sha256,
    sources=empty,protected_inputs=empty,expected_datasets=72,methods=c("a","b"))
  saveRDS(identity,file.path(run,"identity.rds"))
  expect_identical(e$mpq_baseline_check(run,spec),identity)
  expect_error(e$mpq_baseline_check(run,spec,complete=TRUE),"complete baseline")
  e$pta_atomic_write_json(list(expected_datasets=72,method_evaluations=144),file.path(run,"evaluation.json"))
  e$pta_atomic_write_json(list(exit_status=1),file.path(run,"launcher_exit.json"))
  expect_error(e$mpq_baseline_check(run,spec,complete=TRUE),"complete baseline")
  e$pta_atomic_write_json(list(exit_status=0),file.path(run,"launcher_exit.json"))
  expect_identical(e$mpq_baseline_check(run,spec,complete=TRUE),identity)
})

test_that("calibration develops its own grid and does not inherit the old threshold", {
  e <- q_study_env()
  cfg <- list(calibration=list(familywise_confidence=.95,replications=200L))
  spec <- list(threshold_quantile_count=3L,primary_method="refined")
  make <- function(i)list(identity=data.frame(n=50,content=.9,tolerance_confidence=.95,
    dgp_id="normal",role=if(i<=2)"threshold_development" else "calibration_assessment"),
    candidates=data.frame(profile_converged=TRUE,posterior_content_probability=if(i<=2)c(.6,.97)[i] else .99,
      width=1,target_content=.97,mean_tilt=0,central_tilt=0,lower=0,
      candidate_index=1L,true_content=.99))
  values <- lapply(1:202,make)
  directory <- tempfile("new_grid_calibration_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  result <- e$mpq_calibrate(values,cfg,spec,directory)
  expect_true(result$passed)
  first <- read.csv(file.path(directory,"threshold_assessment.csv"))
  expect_false(any(first$threshold==.995))
  expect_equal(read.csv(file.path(directory,"policy.csv"))$threshold,.6)
  expect_equal(first$lower_bound[1],stats::binom.test(200,200,
    conf.level=1-first$alpha[1],alternative="greater")$conf.int[1])
  for(i in 3:202)values[[i]]$candidates$true_content <- .8
  result <- e$mpq_calibrate(values,cfg,spec,directory)
  expect_false(result$passed)
  expect_true(is.na(read.csv(file.path(directory,"policy.csv"))$threshold))
  expect_identical(first$threshold,read.csv(file.path(directory,"threshold_assessment.csv"))$threshold)
})

test_that("a nested screen is a numerical check rather than calibration evidence", {
  e <- q_study_env(); spec <- list(primary_method="refined")
  z <- data.frame(cell_index=c(1,1),replication=1,seed=123,
    method=c("refined","mti_profile_fractional_admissible_v1"),returned=TRUE,
    lower=0,upper=c(1,2),width=c(1,2),numerical_failure_candidates=0L,added_q_count=2L)
  x <- e$mpq_screen_assessment(z,spec)
  expect_true(x$passed)
  expect_equal(x$strictly_shorter_test_intervals,1)
  expect_false(x$tolerance_validity_assessed)
  z$width[1] <- 3
  expect_false(e$mpq_screen_assessment(z,spec)$passed)
  z$width[1] <- 1; z$numerical_failure_candidates[1] <- 1L
  expect_false(e$mpq_screen_assessment(z,spec)$passed)
})

test_that("unresolved or mismatched policies cannot start validation", {
  e <- q_study_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  spec <- jsonlite::read_json("application/config/mti_q_grid_refinement_20261022.json",simplifyVector=FALSE)
  cfg <- e$mpq_config(spec); design <- e$mpr_design(e$mp_frozen_profile(cfg)$validation)[1,,drop=FALSE]
  cfg$validation$replications <- 1L
  cfg$validation$seed_base <- 2012200
  directory <- tempfile("unresolved_grid_"); dir.create(file.path(directory,"calibration"),recursive=TRUE)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  expect_error(e$mpq_stage("validation",directory,cfg,spec,design,1),"unresolved or absent")
  e$pta_atomic_write_json(list(passed=TRUE,policy_sha256="wrong"),
    file.path(directory,"calibration","completion.json"))
  e$pma_write_csv(data.frame(n=50,content=.9,tolerance_confidence=.95,threshold=.95),
    file.path(directory,"calibration","policy.csv"))
  expect_error(e$mpq_stage("validation",directory,cfg,spec,design,1),"hash changed")
  e$pta_atomic_write_json(list(passed=TRUE,
    policy_sha256=e$mp_sha(file.path(directory,"calibration","policy.csv"))),
    file.path(directory,"calibration","completion.json"))
  expect_error(e$mpq_stage("validation",directory,cfg,spec,design,1),"identity or design")
})

test_that("precision curves use hypothetical attainment, without a selected budget", {
  e <- q_study_env()
  design <- data.frame(n=50,content=.9,tolerance_confidence=.95)
  cfg <- list(calibration=list(familywise_confidence=.95,replications=1000))
  spec <- list(threshold_quantile_count=5)
  x <- e$mpq_precision_reference(design,cfg,spec)
  expect_equal(nrow(x),25L)
  expect_true(all(x$reference_only))
  expect_true(all(x$hypothetical_attainment>=.95 & x$hypothetical_attainment<=1))
  expect_true(all(x$probability_of_passing[x$hypothetical_attainment==1]==1))
  expect_true(all(x$probability_of_passing[x$hypothetical_attainment==.95]<=x$alpha[1]+1e-12))
})

test_that("union fitting preserves baseline candidates and supports paired reporting", {
  e <- q_study_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  expect_true("rqr_mti_refine_content_grid" %in% getNamespaceExports("rqrgibbs"))
  spec <- jsonlite::read_json("application/config/mti_q_grid_refinement_20261022.json",simplifyVector=FALSE)
  cfg <- e$mpq_config(spec)
  design <- e$mpr_design(e$mp_frozen_profile(cfg)$validation)[1,,drop=FALSE]
  cfg$screen$seed_base <- 9912200
  cfg$validation$seed_base <- 2012200
  cfg$validation$replications <- 1L
  task <- e$mpq_tasks(design,cfg,"screen")[1,,drop=FALSE]
  set.seed(task$seed); y <- rnorm(task$n)
  fit <- e$mpq_fit(y,task,cfg)
  profile <- e$mp_frozen_profile(cfg)
  menu <- e$mp_menu(y,task$content,task$tolerance_confidence,profile,
    e$mp_scan_fraction(task$n,task$content,task$tolerance_confidence))
  reference <- e$mp_profile(y,menu$q_grid,task$tolerance_confidence,cfg,content=task$content)
  fields <- setdiff(names(reference$candidates),"candidate_index")
  expect_equal(fit$baseline[fields],reference$candidates[fields],ignore_attr=TRUE,tolerance=0)
  expect_true(length(fit$grid$added_q)>0)
  screen <- e$mpq_one(task,"screen",cfg,spec,read.csv(spec$baseline_policy))
  expect_true(e$mpq_screen_assessment(screen,spec)$passed)
  directory <- tempfile("grid_integration_"); dir.create(file.path(directory,"calibration"),recursive=TRUE)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  policy <- data.frame(n=50,content=.9,tolerance_confidence=.95,threshold=.95,
    method_id=spec$primary_method,decision="passed_new_calibration")
  e$pma_write_csv(policy,file.path(directory,"calibration","policy.csv"))
  e$pta_atomic_write_json(list(passed=TRUE,
    policy_sha256=e$mp_sha(file.path(directory,"calibration","policy.csv"))),
    file.path(directory,"calibration","completion.json"))
  result <- e$mpq_stage("validation",directory,cfg,spec,design,1L)
  expect_true(result$passed)
  expect_equal(result$method_evaluations,4L)
  expect_true(file.exists(file.path(directory,"validation","mti_profile_assessment.pdf")))
  expect_true(file.exists(file.path(directory,"validation","REPORT.md")))
  expect_equal(nrow(read.csv(file.path(directory,"validation","paired_comparisons.csv"))),3L)
  a <- readRDS(file.path(directory,"validation","checkpoints","batch_00001.rds"))
  again <- e$mpq_stage("validation",directory,cfg,spec,design,2L)
  expect_identical(again,result)
  expect_identical(a,readRDS(file.path(directory,"validation","checkpoints","batch_00001.rds")))
})
