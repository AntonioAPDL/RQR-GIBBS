threshold_followup_env <- function() {
  e <- new.env(parent=globalenv())
  e$repo <- normalizePath(testthat::test_path("..","..",".."))
  for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R",
    "mti_q_grid_study.R","mti_final_study.R","mti_final_reporting.R","mti_threshold_followup.R"))
    sys.source(file.path(e$repo,"application/scripts/lib",file),e)
  e
}

threshold_followup_fixture <- function(e) {
  design <- data.frame(cell_index=1L,n=5L,content=.3,tolerance_confidence=.8,dgp_id="normal")
  cfg <- list(calibration=list(replications=200L,threshold_development_replications=3L,
    familywise_confidence=.95,seed_base=1712200,cell_seed_stride=1000),
    execution=list(checkpoint_batch=40L),profile=list(objective_tolerance=1e-8))
  tasks <- e$mpq_tasks(design,cfg,"calibration")
  law <- list(r=stats::rnorm,p=stats::pnorm)
  values <- lapply(seq_len(nrow(tasks)),function(i) {
    set.seed(tasks$seed[i]); y <- law$r(5); l <- min(y); u <- max(y)
    z <- data.frame(candidate_index=1L,profile_converged=TRUE,relative_objective_gap=0,
      lower=l,upper=u,width=u-l,posterior_content_probability=stats::pbeta(.3,5.2,.8,lower.tail=FALSE),
      true_content=stats::pnorm(u)-stats::pnorm(l),target_content=.8,
      mean_tilt=0,central_tilt=0,observed_count=5L)
    list(identity=tasks[i,,drop=FALSE],candidates=z)
  })
  spec <- list(threshold_rule="independent_development_logit_gap_refinement",
    threshold_quantile_count=5L,threshold_gap_levels=2L,
    followup=list(original_error_fraction=.5),primary_method="mti_profile_mass_cells_v1")
  list(design=design,cfg=cfg,tasks=tasks,values=values,
    context=list(laws=list(normal=law)),spec=spec)
}

test_that("development-only gap refinement is nested, bounded, and deterministic", {
  e <- threshold_followup_env()
  spec <- list(threshold_rule="independent_development_quantiles_and_requested_confidence",
    threshold_quantile_count=25L)
  p <- stats::plogis(seq(-2,7,length.out=80))
  base <- e$mpq_threshold_menu(p,.95,spec)
  spec$threshold_rule <- "independent_development_logit_gap_refinement"
  spec$threshold_gap_levels <- 2L
  set.seed(17); rng <- .Random.seed
  refined <- e$mpq_threshold_menu(p,.95,spec,c(.999,.998))
  expect_identical(rng,.Random.seed)
  expect_true(all(base %in% refined))
  expect_true(.998 %in% refined)
  expect_lte(length(refined),105L)
  expect_true(all(diff(refined)>0 & refined[-1]<1))
  expect_identical(refined,e$mpq_threshold_menu(p,.95,spec,c(.999,.998)))
  expect_false(identical(refined,e$mpq_threshold_menu(p/2,.95,spec,c(.9,.8))))
  expect_true(all(e$mpq_threshold_menu(p,.95,spec,c(1,1))<1))
  expect_true(all(e$mpq_threshold_menu(p,.95,spec,c(0,.8))>0))
  expect_error(e$mpq_threshold_menu(p,.95,spec),"eligibility")
  expect_error(e$mpq_threshold_menu(p,.95,spec,NA_real_),"eligibility")
  spec$threshold_gap_levels <- 9L
  expect_error(e$mpq_threshold_menu(p,.95,spec,.99),"resolution")
})

test_that("retained and new comparisons share one simultaneous error allowance", {
  e <- threshold_followup_env(); x <- threshold_followup_fixture(e)
  alpha <- e$mpq_calibration_alpha(x$cfg,x$spec,2L,8L,105L)
  expect_equal(alpha,.025/(2*8*105))
  expect_equal(.025+2*8*105*alpha,.05)
  original <- x$spec; original$threshold_rule <- "independent_development_quantiles_and_requested_confidence"
  expect_equal(e$mpq_calibration_alpha(x$cfg,original,9L,8L,26L),.05/(9*8*6))
  x$spec$followup$original_error_fraction <- .2
  expect_error(e$mpq_calibration_alpha(x$cfg,x$spec,2,8,105),"equal error split")
})

test_that("the new design and seeds leave the seven other settings untouched", {
  e <- threshold_followup_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old),add=TRUE)
  spec <- jsonlite::read_json("application/config/mti_threshold_followup_20261024.json",simplifyVector=FALSE)
  cfg <- e$mpq_config(spec); design <- e$mpr_design(e$mp_frozen_profile(cfg)$validation)
  sub <- e$mpq_followup_design(design,spec)
  expect_equal(sub$cell_index,c(1:8,17:24))
  expect_equal(nrow(e$mpq_tasks(sub,cfg,"calibration")),384512L)
  expect_equal(nrow(e$mpq_tasks(design,cfg,"validation")),72000L)
  parent <- jsonlite::read_json(spec$followup$parent_config,simplifyVector=FALSE)
  expect_silent(e$mpq_followup_seed_check(design,cfg,parent))
  expect_silent(e$mpq_seed_check(design,cfg,jsonlite::read_json(spec$revision_config,simplifyVector=FALSE)))
  cfg$calibration$seed_base <- parent$calibration$seed_base
  expect_error(e$mpq_followup_seed_check(design,cfg,parent),"overlap")
  spec$followup$reassess_cells[[2]] <- spec$followup$reassess_cells[[1]]
  expect_error(e$mpq_followup_design(design,spec),"distinct")
})

test_that("tightening retained policies preserves their thresholds and rejects insufficient evidence", {
  e <- threshold_followup_env(); x <- threshold_followup_fixture(e)
  p <- data.frame(n=c(50L,100L,500L),content=c(.9,.95,.9),tolerance_confidence=.95,
    threshold=c(.97,NA,.98),method_id="mti_profile_mass_cells_v1",
    decision=c("passed_new_calibration","unresolved","passed_new_calibration"))
  a <- p; a$threshold <- c(.97,.97,.98); a$dgp_id <- "normal"
  a$replications <- a$returned <- 24000L; a$successes <- 23500L
  a$alpha <- .05/(3*1*6); a$lower_bound <- 0
  x$spec$followup$retained_settings <- 2L
  result <- e$mpq_followup_retained(p,a,x$cfg,x$spec)
  expect_identical(result$policy$threshold,c(.97,.98))
  expect_equal(result$assessment$alpha,rep(a$alpha[1]/2,2L))
  expect_true(all(result$assessment$lower_bound>=.95))
  a$successes[1] <- 22500L
  expect_error(e$mpq_followup_retained(p,a,x$cfg,x$spec),"tightened")
  a$alpha <- a$alpha*2
  expect_error(e$mpq_followup_retained(p,a,x$cfg,x$spec),"allocation")
})

test_that("refined bounded calibration matches memory and reads only development for its menu", {
  e <- threshold_followup_env(); x <- threshold_followup_fixture(e)
  directory <- tempfile("threshold_parity_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  memory <- file.path(directory,"memory"); bounded <- file.path(directory,"bounded")
  dir.create(memory); dir.create(bounded)
  expected <- e$mpq_calibrate(x$values,x$cfg,x$spec,memory)
  e$mp_checkpoint_jobs(x$tasks,function(task)x$values[[task$replication]],
    file.path(bounded,"checkpoints"),1L,x$cfg$execution$checkpoint_batch,collect=FALSE)
  actual <- e$mpq_calibrate_checkpoints(x$tasks,x$cfg,x$spec,bounded,x$context)
  expect_identical(actual$passed,expected$passed)
  expect_equal(read.csv(file.path(memory,"threshold_assessment.csv")),
    read.csv(file.path(bounded,"threshold_assessment.csv")),tolerance=0)
  original <- read.csv(file.path(memory,"threshold_assessment.csv"))
  for(i in which(x$tasks$role=="calibration_assessment"))
    x$values[[i]]$candidates$posterior_content_probability <- .1
  tainted <- file.path(directory,"assessment_changed"); dir.create(tainted)
  e$mpq_calibrate(x$values,x$cfg,x$spec,tainted)
  expect_identical(unique(read.csv(file.path(tainted,"threshold_assessment.csv"))$threshold),
    unique(original$threshold))
  z <- x$values[[1]]$candidates
  expect_equal(e$mpr_candidate_decisions(z,z$posterior_content_probability,.3)$returned,1L)
  expect_equal(e$mpr_candidate_decisions(z,1,.3)$returned,0L)
})

test_that("an unresolved follow-up cannot start validation or overwrite old results", {
  e <- threshold_followup_env(); seen <- character()
  e$mpq_stage <- function(stage,...) {seen <<- c(seen,stage);list(passed=FALSE)}
  e$mpq_followup_combined <- function(...)stop("Combined policy must not be created")
  directory <- tempfile("threshold_stopped_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  spec <- list(final_study=list(validation_after_passing_calibration=TRUE,automatic_adoption=FALSE))
  ref <- list(reassess_design=data.frame(),retained=list(policy=data.frame(n=1:7)))
  result <- e$mpq_followup_run(directory,list(),spec,data.frame(),1L,function()NULL,ref)
  expect_identical(seen,"calibration")
  expect_identical(result$decision,"stopped_unresolved_followup")
  expect_false(result$validation_started)
  expect_false(result$article_inputs_replaced)
})

test_that("a passing follow-up validates the full combined policy and requires scientific review", {
  e <- threshold_followup_env(); sizes <- integer(); seen <- character()
  e$mpq_stage <- function(stage,output,cfg,spec,design,workers) {
    seen <<- c(seen,stage); sizes <<- c(sizes,nrow(design)); list(passed=TRUE)
  }
  e$mpq_followup_combined <- function(output,...)file.path(output,"combined")
  e$mpq_export_review <- function(...)"review"
  directory <- tempfile("threshold_passed_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  spec <- list(final_study=list(validation_after_passing_calibration=TRUE,automatic_adoption=FALSE))
  ref <- list(reassess_design=data.frame(i=1:16),retained=list(policy=data.frame(n=1:7)))
  result <- e$mpq_followup_run(directory,list(),spec,data.frame(i=1:72),1L,function()NULL,ref)
  expect_identical(seen,c("calibration","validation"))
  expect_equal(sizes,c(16L,72L))
  expect_true(result$validation_started)
  expect_false(result$article_inputs_replaced)
  expect_identical(result$decision,"complete_requires_scientific_adoption_review")
})

test_that("combined evidence preserves retained cutoffs and verifies completed files on resume", {
  e <- threshold_followup_env()
  directory <- tempfile("threshold_combined_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  path <- file.path(directory,"calibration"); dir.create(path)
  policy <- data.frame(n=c(50L,100L,100L,500L),content=c(.9,.9,.95,.9),
    tolerance_confidence=.95,threshold=c(NA,.98,NA,.99),
    method_id="mti_profile_mass_cells_v1",
    decision=c("unresolved","passed_new_calibration","unresolved","passed_new_calibration"))
  retained <- policy[is.finite(policy$threshold),]
  fresh <- policy[is.na(policy$threshold),]; fresh$threshold <- c(.98,.99)
  fresh$decision <- "passed_new_calibration"
  assessment <- function(p,alpha) {
    p$dgp_id <- "normal"; p$replications <- p$returned <- 24000L
    p$successes <- 23800L; p$content_attainment <- p$successes/p$replications
    p$alpha <- alpha; p$lower_bound <- mapply(e$mp_binom_lower,p$successes,p$replications,p$alpha)
    p$mean_width <- 2
    p[setdiff(names(p),c("method_id","decision"))]
  }
  low <- fresh; low$threshold <- .8
  fa <- assessment(rbind(low,fresh),.025/(2*1*2))
  fa$successes[fa$threshold==.8] <- 21000L
  fa$content_attainment <- fa$successes/fa$replications
  fa$lower_bound <- mapply(e$mp_binom_lower,fa$successes,fa$replications,fa$alpha)
  receipt <- e$mpq_write_calibration(fresh,fa,path); receipt$numerical_failure_datasets <- 0L
  e$pta_atomic_write_json(receipt,file.path(path,"completion.json"))
  spec <- list(threshold_rule="independent_development_logit_gap_refinement",
    threshold_quantile_count=5L,followup=list(original_error_fraction=.5,parent_policy_sha256="test-parent"))
  cfg <- list(calibration=list(familywise_confidence=.95))
  ref <- list(parent_policy=policy,reassess_design=fresh[c("n","content","tolerance_confidence")],
    retained=list(policy=retained,assessment=assessment(retained,.025/(4*1*6))),total_familywise_error=.05)
  design <- policy[c("n","content","tolerance_confidence")]
  combined <- e$mpq_followup_combined(directory,cfg,spec,ref,design)
  result <- read.csv(file.path(combined,"calibration/policy.csv"))
  expect_equal(result$threshold,c(.98,.98,.99,.99))
  expect_equal(nrow(result),4L)
  expect_true(all(result$decision=="passed_new_calibration"))
  before <- e$pma_file_record(list.files(file.path(combined,"calibration"),full.names=TRUE))
  expect_identical(e$mpq_followup_combined(directory,cfg,spec,ref,design),combined)
  expect_identical(before,e$pma_file_record(before$path))
  wrong <- fresh; wrong$threshold[1] <- .8
  e$pma_write_csv(wrong,file.path(path,"policy.csv"))
  expect_error(e$mpq_followup_combined(directory,cfg,spec,ref,design),"evidence changed")
  e$pma_write_csv(fresh,file.path(path,"policy.csv"))
  result$threshold[1] <- .7
  e$pma_write_csv(result,file.path(combined,"calibration/policy.csv"))
  expect_error(e$mpq_followup_combined(directory,cfg,spec,ref,design),"combined policy or evidence changed")
})
