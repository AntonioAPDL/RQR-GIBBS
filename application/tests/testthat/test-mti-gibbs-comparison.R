root <- normalizePath(if(file.exists("application/scripts/lib/mti_gibbs_comparison.R"))"." else "../../..")
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
  "mti_profile_revision.R","mti_profile_review.R","mti_final_reporting.R",
  "mti_ecm_recovery.R","mti_gibbs_comparison.R"))source(file.path(root,"application/scripts/lib",file))

testthat::test_that("a Gibbs study requires explicit article-commit confirmation", {
  commit <- paste(rep("a",40),collapse="")
  testthat::expect_error(mgc_authorized("",commit),"not authorized")
  testthat::expect_error(mgc_authorized(paste0(commit,"b"),commit),"not authorized")
  testthat::expect_silent(mgc_authorized(commit,commit))
})

testthat::test_that("the displayed Gaussian conditional has the augmented-kernel quadratic", {
  y <- c(-1,.2,1,3); b <- 2; v <- c(.4,.5,.3,.8)
  q <- .93; delta <- -.2; omega <- 1.3; lambda <- .01
  sigma <- 1/omega; xi <- (1-2*q)/(q*(1-q)); phi <- 2/(q*(1-q))
  d <- y-b; r <- y*d-xi*v; w <- 1/(phi*sigma*v)
  Q <- lambda+sum(w*d^2); H <- sum(w*d*r)+omega*length(y)*q*delta
  kernel <- function(a)sum(dnorm((y-a)*(y-b),xi*v,sqrt(phi*sigma*v),log=TRUE))+
    omega*length(y)*q*delta*(a+b)-lambda*(a^2+b^2)/2
  for(a in c(-2,-.1,.8))testthat::expect_equal(kernel(a)-kernel(0),H*a-Q*a^2/2,
    tolerance=1e-12)
})

testthat::test_that("the pseudo-AL mixture recovers the check-loss factor", {
  for(q in c(.5,.9,.99))for(e in c(-1,.2,2)) {
    sigma <- .7;xi <- (1-2*q)/(q*(1-q));phi <- 2/(q*(1-q))
    actual <- integrate(function(v)dnorm(e,xi*v,sqrt(phi*sigma*v))*
      dexp(v,rate=1/sigma),0,Inf,rel.tol=1e-8)$value
    expected <- q*(1-q)/sigma*exp(-e*(q-(e<0))/sigma)
    testthat::expect_equal(actual,expected,tolerance=1e-7)
  }
})

testthat::test_that("application sample recovery preserves training membership", {
  context <- list(observed=data.frame(.analysis_row=1:4,x=c(3,6,9,12)),
    membership=data.frame(split_scheme="random",split_id=1,
      training=c(TRUE,FALSE,TRUE,FALSE),analysis_row_id=1:4))
  task <- data.frame(source="application",response_id="x",split_scheme="random",split_id=1,n=2)
  testthat::expect_equal(mgc_sample(task,context),c(3,9))
  task$n <- 3
  testthat::expect_error(mgc_sample(task,context),"incomplete")
})

testthat::test_that("assessment and health use completed independent-chain files", {
  testthat::skip_if_not_installed("posterior")
  out <- tempfile("gibbs-assessment-");dir.create(file.path(out,"chains"),recursive=TRUE)
  on.exit(unlink(out,recursive=TRUE),add=TRUE)
  cfg <- list(n_mcmc=128L,chains=4L,rhat_limit=1.2,minimum_bulk_ess=10,minimum_tail_ess=10)
  jobs <- data.frame(job_id=1:4,dataset_id=1L,case_id="fixture",source="simulation",
    q=.98,delta=.1,ecm_lower=-2,ecm_upper=2,chain=1:4)
  pma_write_csv(jobs,file.path(out,"jobs.csv"))
  pta_atomic_write_json(list(commit=paste(rep("a",40),collapse=""),workers=2,settings=cfg),
    file.path(out,"manifest.json"))
  for(i in 1:4) {
    set.seed(i);lo <- rnorm(128,-2,.1);hi <- rnorm(128,2,.1)
    saveRDS(list(task=jobs[i,],draws=cbind(lower=lo,upper=hi,width=hi-lo),elapsed_seconds=2),
      file.path(out,"chains",sprintf("chain_%06d.rds",i)))
  }
  result <- mgc_assess(cfg,out)
  testthat::expect_setequal(result$variable,c("lower","upper","width"))
  testthat::expect_equal(result$ecm_value,c(-2,2,4))
  health <- mgc_health(out)
  testthat::expect_equal(health$completed_chains,4)
  testthat::expect_equal(health$remaining_chains,0)
  testthat::expect_equal(health$completed_datasets,1)
  testthat::expect_true(health$assessment_complete)
  assessment <- jsonlite::read_json(file.path(out,"assessment.json"))
  testthat::expect_false(assessment$new_tolerance_guarantee)
  testthat::expect_false(assessment$article_replaced)
})
