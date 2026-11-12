root <- normalizePath(if(file.exists("application/scripts/lib/mti_ecm_recovery.R"))"." else "../../..")
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
  "mti_profile_revision.R","mti_profile_review.R","mti_final_reporting.R","mti_ecm_recovery.R"))
  source(file.path(root,"application/scripts/lib",file))

testthat::test_that("ECM evidence cannot be relabeled from another numerical method", {
  cfg <- list(primary_method="mti_ecm_adaptive_cell",expected_datasets=1L,replications=1L)
  z <- data.frame(method_id=cfg$primary_method,ecm_backend="cpp",posterior_draws=NA_real_,
    uq_engine="mti_ecm_direct_dp",dgp_id="normal",n=50,guaranteed_content=.9,
    tolerance_confidence=.95,replication=1,infeasible=FALSE,lower=-2,upper=2,
    success=TRUE,content=.96)
  testthat::expect_silent(mre_check_identity(z,cfg))
  z$uq_engine <- "fixed_target_profile"
  testthat::expect_error(mre_check_identity(z,cfg),"not the specified ECM")
  z$uq_engine <- "mti_ecm_direct_dp"; z$posterior_draws <- 500
  testthat::expect_error(mre_check_identity(z,cfg),"not the specified ECM")
})

testthat::test_that("sample reconstruction checks distribution and sample diagnostics", {
  diagnostics <- function(y) {
    q <- quantile(y,c(.1,.25,.75,.9),type=8,names=FALSE); med <- median(y)
    data.frame(bowley_skewness=(q[3]+q[2]-2*med)/(q[3]-q[2]),
      robust_tail_ratio=(q[4]-med)/(med-q[1]))
  }
  set.seed(100);y <- rnorm(50); d <- diagnostics(y)
  row <- data.frame(seed=100,n=50,lower=-2,upper=2,content=pnorm(2)-pnorm(-2),
    sample_bowley_skewness=d$bowley_skewness,sample_tail_ratio=d$robust_tail_ratio)
  testthat::expect_identical(mre_replay(row,list(r=rnorm,p=pnorm),diagnostics),y)
  row$seed <- 101
  testthat::expect_error(mre_replay(row,list(r=rnorm,p=pnorm),diagnostics),"sample does not match")
  row$seed <- 100;row$content <- .5
  testthat::expect_error(mre_replay(row,list(r=rnorm,p=pnorm),diagnostics),"population content")
})

testthat::test_that("paired comparisons reject different sample seeds", {
  z <- data.frame(n=50,content=.9,tolerance_confidence=.95,dgp_id="normal",
    replication=1,seed=c(1,2),method=c("ecm","wilks"),cell_index=1,
    content_attained=TRUE,returned=TRUE,width=4)
  testthat::expect_error(mpr_paired_summary(z,list(primary_method="ecm")),"seeds do not match")
})

testthat::test_that("CDF checks account only for endpoint storage rounding", {
  cdf <- function(x) pgamma((x+sqrt(.5))*sqrt(.5),shape=.5)
  lower <- -sqrt(.5)+1e-10; upper <- 4
  content <- cdf(upper)-cdf(lower)
  testthat::expect_true(mre_content_check(lower+1e-15,upper,content,cdf))
  testthat::expect_false(mre_content_check(lower,upper,content-.001,cdf))
})
