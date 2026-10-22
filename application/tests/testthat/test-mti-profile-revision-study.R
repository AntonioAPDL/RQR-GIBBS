profile_study_env <- function() {
  e <- new.env(parent=globalenv())
  for (file in c("pharma_tolerance_application.R","pharma_mti_application.R",
                 "mti_profile_revision.R")) {
    roots <- c(testthat::test_path("..",".."),
      testthat::test_path("..","..","00_pkg_src","rqrgibbs"))
    paths <- file.path(roots,"scripts","lib",file)
    if(!any(file.exists(paths))) stop("Missing study assessment source: ",file)
    sys.source(paths[file.exists(paths)][1L],e)
  }
  e
}

test_that("profile study checkpoint resumption preserves tasks and failures", {
  e <- profile_study_env()
  directory <- tempfile("profile_checkpoints_")
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  tasks <- data.frame(index=1:5)
  calls <- 0L
  one <- function(task) {
    calls <<- calls+1L
    if(task$index==3) stop("Prespecified task failure")
    data.frame(index=task$index,value=task$index^2)
  }
  first <- e$mp_checkpoint_jobs(tasks,one,directory,1,2)
  second <- e$mp_checkpoint_jobs(tasks,one,directory,1,2)
  expect_equal(calls,5L)
  expect_identical(first,second)
  expect_match(first[[3]]$error,"Prespecified")
  expect_error(e$mp_checkpoint_jobs(transform(tasks,index=index+1),one,directory,1,2),
    "task mismatch")
})

test_that("threshold grid is developed independently of binomial assessment", {
  e <- profile_study_env()
  cfg <- list(calibration=list(threshold_quantile_count=3L,
    familywise_confidence=.95,threshold_development_replications=2L,replications=100L))
  candidate <- function(probability,content=.99) data.frame(
    posterior_content_probability=probability,profile_converged=TRUE,
    width=1,target_content=.98,mean_tilt=0,central_tilt=0,
    lower=0,candidate_index=1L,true_content=content)
  values <- lapply(1:102,function(i) list(identity=data.frame(n=100,content=.9,
    tolerance_confidence=.95,dgp_id="test",role=if(i<=2)
      "threshold_development" else "calibration_assessment"),
    candidates=candidate(if(i<=2) .97 else .999)))
  directory <- tempfile("profile_calibration_")
  dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  policy <- e$mp_calibration_policy(values,cfg,directory)
  expect_true(is.finite(policy$threshold))
  first <- read.csv(file.path(directory,"threshold_assessment.csv"))
  for(i in 3:102) values[[i]]$candidates <- candidate(.96,content=.8)
  unresolved <- e$mp_calibration_policy(values,cfg,directory)
  second <- read.csv(file.path(directory,"threshold_assessment.csv"))
  expect_identical(sort(unique(first$threshold)),sort(unique(second$threshold)))
  expect_true(is.na(unresolved$threshold))
  expect_true(all(second$successes==0))
  expect_equal(e$mp_binom_lower(0,100,.05),0)
})

test_that("profile study rejects partial numerically failed candidate searches", {
  e <- profile_study_env()
  z <- data.frame(posterior_content_probability=c(.99,.999),
    profile_converged=c(TRUE,FALSE),width=c(2,1),target_content=.98,
    mean_tilt=0,central_tilt=0,lower=0,candidate_index=1:2)
  expect_equal(nrow(e$mp_select(z,.95)),0L)
  z$profile_converged <- TRUE
  expect_equal(e$mp_select(z,.95)$candidate_index,2L)
  expect_equal(nrow(e$mp_select(z,.9999)),0L)
})
