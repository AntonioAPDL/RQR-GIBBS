final_study_env <- function() {
  e <- new.env(parent=globalenv())
  e$repo <- normalizePath(testthat::test_path("..","..",".."))
  for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R",
    "mti_q_grid_study.R","mti_final_study.R","mti_final_reporting.R"))
    sys.source(file.path(e$repo,"application/scripts/lib",file),e)
  e
}

final_calibration_fixture <- function(e,content=.3) {
  design <- data.frame(cell_index=1L,n=5L,content=content,tolerance_confidence=.8,dgp_id="normal")
  cfg <- list(calibration=list(replications=200L,threshold_development_replications=3L,
    familywise_confidence=.95,seed_base=1712200,cell_seed_stride=1000),
    execution=list(checkpoint_batch=40L),profile=list(objective_tolerance=1e-8))
  tasks <- e$mpq_tasks(design,cfg,"calibration")
  law <- list(r=function(n)stats::rnorm(n),p=stats::pnorm)
  values <- lapply(seq_len(nrow(tasks)),function(i) {
    set.seed(tasks$seed[i]); y <- law$r(5); l <- min(y); u <- max(y)
    h <- stats::pnorm(u,sd=4)-stats::pnorm(l,sd=4)
    z <- data.frame(candidate_index=1L,profile_converged=TRUE,relative_objective_gap=0,
      lower=l,upper=u,width=u-l,posterior_content_probability=stats::pbeta(content,5+h,1-h,lower.tail=FALSE),
      true_content=stats::pnorm(u)-stats::pnorm(l),target_content=(1+content)/2,
      mean_tilt=0,central_tilt=0,observed_count=5L)
    list(identity=tasks[i,,drop=FALSE],candidates=z)
  })
  list(design=design,cfg=cfg,tasks=tasks,values=values,context=list(laws=list(normal=law)),
    spec=list(threshold_quantile_count=5L,primary_method="mti_profile_mass_cells_v1"))
}

test_that("the final specification fixes the agreed budget and supports all forty workers", {
  e <- final_study_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  spec <- jsonlite::read_json("application/config/mti_q_grid_refinement_final_20261022.json",simplifyVector=FALSE)
  cfg <- e$mpq_config(spec); design <- e$mpr_design(e$mp_frozen_profile(cfg)$validation)
  expect_equal(nrow(e$mpq_tasks(design,cfg,"calibration")),1730304L)
  expect_equal(nrow(e$mpq_tasks(design,cfg,"validation")),72000L)
  expect_gte(cfg$execution$checkpoint_batch,40L)
  expect_true(spec$final_study$validation_after_passing_calibration)
  expect_false(spec$final_study$automatic_adoption)
  expect_false(spec$automatic_promotion)
  original <- jsonlite::read_json(spec$revision_config,simplifyVector=FALSE)
  expect_silent(e$mpq_seed_check(design,cfg,original))
  args <- c("application/scripts/100_manage_mti_q_grid_study.sh","health","",tempfile(),"40",
    "--config=application/config/mti_q_grid_refinement_final_20261022.json")
  lines <- system2("bash",shQuote(args),stdout=TRUE,stderr=TRUE)
  expect_true(any(grepl("1730304",lines,fixed=TRUE)))
  expect_false(any(grepl("74304",lines,fixed=TRUE)))
  alpha <- .05/(9*8*26)
  required <- stats::qbinom(1-alpha,24000,.95)+1
  expect_equal(required,22935)
  expect_gte(e$mp_binom_lower(required,24000,alpha),.95)
  expect_lt(e$mp_binom_lower(required-1,24000,alpha),.95)
})

test_that("cached immutable inputs reproduce uncached candidate calculations exactly", {
  e <- final_study_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  spec <- jsonlite::read_json("application/config/mti_q_grid_refinement_final_20261022.json",simplifyVector=FALSE)
  cfg <- e$mpq_config(spec); design <- e$mpr_design(e$mp_frozen_profile(cfg)$validation)[1:8,]
  set.seed(8112200); before <- .Random.seed; context <- e$mpq_context(cfg)
  expect_identical(before,.Random.seed)
  for(i in seq_len(nrow(design))) {
    set.seed(8112200+i); task <- design[i,,drop=FALSE]
    y <- context$laws[[task$dgp_id]]$r(task$n)
    a <- e$mpq_fit(y,task,cfg)
    b <- e$mpq_fit(y,task,cfg,context)
    expect_identical(a,b)
  }
})

test_that("batchwise calibration reproduces in-memory decisions and preserves checkpoints", {
  e <- final_study_env(); x <- final_calibration_fixture(e)
  directory <- tempfile("calibration_batch_parity_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  a <- file.path(directory,"memory"); b <- file.path(directory,"bounded")
  dir.create(a); dir.create(b)
  reference <- e$mpq_calibrate(x$values,x$cfg,x$spec,a)
  result <- e$mp_checkpoint_jobs(x$tasks,function(task)x$values[[task$replication]],
    file.path(b,"checkpoints"),1L,x$cfg$execution$checkpoint_batch,collect=FALSE)
  expect_null(result)
  records <- e$pma_file_record(list.files(file.path(b,"checkpoints"),"[.]rds$",full.names=TRUE))
  bounded <- e$mpq_calibrate_checkpoints(x$tasks,x$cfg,x$spec,b,x$context)
  expect_identical(bounded$passed,reference$passed)
  expect_identical(read.csv(file.path(a,"policy.csv")),read.csv(file.path(b,"policy.csv")))
  expect_equal(read.csv(file.path(a,"threshold_assessment.csv")),
    read.csv(file.path(b,"threshold_assessment.csv")),ignore_attr=TRUE,tolerance=0)
  expect_identical(records,e$pma_file_record(records$path))
  expect_equal(bounded$numerical_failure_datasets,0L)
  expect_true(file.exists(file.path(b,"checkpoint_manifest.csv")))
  saved <- readRDS(records$path[1]); saved$values[[1]]$candidates$observed_count <- 4L
  saveRDS(saved,records$path[1])
  expect_error(e$mpq_calibrate_checkpoints(x$tasks,x$cfg,x$spec,b,x$context),"closed-interval counts")
})

test_that("an unresolved bounded calibration remains unresolved", {
  e <- final_study_env(); x <- final_calibration_fixture(e,.99)
  directory <- tempfile("calibration_unresolved_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  e$mp_checkpoint_jobs(x$tasks,function(task)x$values[[task$replication]],
    file.path(directory,"checkpoints"),1L,x$cfg$execution$checkpoint_batch,collect=FALSE)
  result <- e$mpq_calibrate_checkpoints(x$tasks,x$cfg,x$spec,directory,x$context)
  expect_false(result$passed)
  expect_equal(result$calibrated_cells,0L)
  expect_true(all(is.na(read.csv(file.path(directory,"policy.csv"))$threshold)))
})

test_that("task errors stop a batch launch and cannot be silently retried on resume", {
  e <- final_study_env()
  directory <- tempfile("final_task_error_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  tasks <- data.frame(index=1:4)
  calls <- 0L
  one <- function(task) {calls <<- calls+1L; if(task$index==1L)stop("test error"); task}
  expect_error(e$mp_checkpoint_jobs(tasks,one,directory,1L,2L,collect=FALSE,
    stop_on_task_error=TRUE),"[Tt]ask error")
  expect_equal(calls,2L)
  expect_equal(length(list.files(directory,"[.]rds$")),1L)
  expect_error(e$mp_checkpoint_jobs(tasks,one,directory,1L,2L,collect=FALSE,
    stop_on_task_error=TRUE),"[Tt]ask error")
  expect_equal(calls,2L)
})

test_that("the final controller never starts validation after unresolved calibration", {
  e <- final_study_env(); seen <- character(); checks <- 0L
  e$mpq_stage <- function(stage,...) {seen <<- c(seen,stage);list(passed=FALSE)}
  e$mpq_export_review <- function(...)stop("Review must not run")
  spec <- list(final_study=list(validation_after_passing_calibration=TRUE,automatic_adoption=FALSE))
  directory <- tempfile("final_controller_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  status <- e$mpq_final_run(directory,list(),spec,data.frame(),1L,function()checks <<- checks+1L)
  expect_identical(seen,"calibration")
  expect_equal(checks,1L)
  expect_identical(status$decision,"stopped_unresolved_calibration")
  expect_false(status$validation_started)
  expect_false(status$article_inputs_replaced)
})

test_that("passing calibration authorizes frozen validation and review, not adoption", {
  e <- final_study_env(); seen <- character(); checks <- 0L
  e$mpq_stage <- function(stage,...) {seen <<- c(seen,stage);list(passed=TRUE)}
  e$mpq_export_review <- function(...)"review"
  spec <- list(final_study=list(validation_after_passing_calibration=TRUE,automatic_adoption=FALSE))
  directory <- tempfile("final_controller_pass_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  status <- e$mpq_final_run(directory,list(),spec,data.frame(),1L,function()checks <<- checks+1L)
  expect_identical(seen,c("calibration","validation"))
  expect_equal(checks,3L)
  expect_true(status$validation_started)
  expect_false(status$article_inputs_replaced)
  expect_identical(status$decision,"complete_requires_scientific_adoption_review")
})

test_that("the application action uses a supported frozen policy without ECM relabeling", {
  e <- final_study_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  spec <- jsonlite::read_json("application/config/mti_q_grid_refinement_final_20261022.json",simplifyVector=FALSE)
  cfg <- e$mpq_config(spec)
  policy <- data.frame(n=100L,content=.9,tolerance_confidence=.95,threshold=.95,
    method_id=spec$primary_method,decision="passed_new_calibration")
  set.seed(9112200); y <- rexp(100)
  fit <- e$mpq_frozen_action(y,.9,.95,policy,cfg)
  expect_true(fit$returned)
  expect_identical(fit$computation,"fixed_target_profile")
  expect_false(fit$universal_frequentist_guarantee)
  expect_true(all(fit$selected$posterior_content_probability>=.95))
  expect_error(e$mpq_frozen_action(y[1:50],.9,.95,policy,cfg),"No calibrated policy")
  policy$decision <- "unresolved"
  expect_error(e$mpq_frozen_action(y,.9,.95,policy,cfg),"No calibrated policy")
})
