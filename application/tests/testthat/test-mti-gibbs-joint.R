root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_joint.R")) "." else "../../..")
for (name in c("mti_gibbs_practical", "mti_gibbs_full_report_review", "mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis", "mti_prior_screen", "mti_fixed_prior", "mti_gibbs_boundary_calibration",
  "mti_gibbs_decision_calibration", "mti_fixed_prior_finalization", "mti_gibbs_joint", "mti_gibbs_joint_precision",
  "mti_gibbs_joint_study", "mti_gibbs_joint_reuse"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))
joint_config <- function() mwd_json(file.path(root, "application/config/mti_gibbs_joint_20261117.json"))
joint_radius <- function(s = 1) data.frame(n=100L,content=.9,tolerance_confidence=.95,rule="proposed",multiplier=s)
joint_candidates <- function() data.frame(candidate_index = 1:4, target_content = c(.92,.94,.96,.98),
  mean_tilt = 0, central_tilt = 0, lower = -c(1,2,3,4), upper = c(1,2,3,4), width = c(2,4,6,8),
  finite_interval = TRUE, numerically_accurate = TRUE, posterior_content_probability = c(.6,.8,.95,1))

testthat::test_that("the actual launcher allocates workers for idle and busy hosts", {
  testthat::skip_if(!nzchar(Sys.which("bash")) || !nzchar(Sys.which("awk")))
  launcher <- readLines(file.path(root, "application/scripts/134_launch_mti_gibbs_joint.sh"))
  allocation <- launcher[grepl("^  (free=|export MTI_JOINT_WORKERS=)", launcher)]
  testthat::expect_length(allocation, 2L)
  fixture <- tempfile(); on.exit(unlink(fixture))
  allocation <- gsub("$(getconf _NPROCESSORS_ONLN)", "64", allocation, fixed = TRUE)
  allocation <- gsub("/proc/loadavg", shQuote(fixture), allocation, fixed = TRUE)
  command <- paste(c("set -euo pipefail", allocation, 'printf "%s\\n" "$MTI_JOINT_WORKERS"'), collapse = "\n")
  loads <- c(0, 30.4, 63.5, 64, 80)
  expected <- c(40L, 33L, 1L, 1L, 1L)
  for (i in seq_along(loads)) {
    writeLines(sprintf("%g 0 0 1/1 1", loads[i]), fixture)
    value <- system2("bash", c("-c", shQuote(command)), stdout = TRUE, stderr = TRUE)
    testthat::expect_null(attr(value, "status"))
    testthat::expect_identical(value, as.character(expected[i]))
  }
})

testthat::test_that("the fixed independent coordinates and unscaled definition are enforced", {
  cfg <- joint_config(); testthat::expect_true(mjc_validate(cfg))
  testthat::expect_equal(nrow(mjc_models(cfg)), 18)
  cfg$radius_multiplier <- .99
  testthat::expect_error(mjc_validate(cfg), "specification")
  cfg <- joint_config(); cfg$independent_coordinates$estimated_from_analysis_sample <- TRUE
  testthat::expect_error(mjc_validate(cfg), "specification")
  y <- c(-100,2,3,1000)
  a <- mfp_spec(y, 1, list(mode = "fixed", center = 0, scale = 1,
    estimated_from_analysis_sample = FALSE, source = "fixed external coordinates"))
  testthat::expect_equal(a$prior_sd, 1); testthat::expect_equal(a$z, y)
})

testthat::test_that("analytic DP probabilities match the package, including closed endpoints", {
  testthat::skip_if_not_installed("rqrgibbs")
  y <- c(-1,-1,0,.5,2,2)
  for (a in c(.5,1,2)) for (h in c(2,4,8)) {
    lower <- c(-1,0,-10,100); upper <- c(2,.5,10,101)
    f <- rqrgibbs::rqr_dp_fit(y, a, rqrgibbs::rqr_dp_base_normal(0,h), strict_bayes = TRUE)
    expected <- rqrgibbs::rqr_dp_content_probability(f,lower,upper,.9)$posterior_content_probability
    testthat::expect_equal(mjc_probabilities(y,lower,upper,.9,a,h),expected,tolerance = 1e-12)
  }
  testthat::expect_equal(mjc_probabilities(y,-1e100,1e100,.9,1,4),1)
  testthat::expect_error(mjc_probabilities(y,2,1,.9,1,4),"Invalid")
})

testthat::test_that("threshold events cover both directions and agree with brute-force replay", {
  a <- joint_candidates(); a$response_id <- "a"; a$population_content <- c(.91,.85,.96,.99)
  b <- a; b$response_id <- "b"; b$width <- b$width*2
  b$posterior_content_probability <- c(.5,.7,.9,.99)
  paths <- list(a,a,b,b); reference <- c(a=4,b=8)
  curve <- mjc_curve(paths, reference, .8, .9, .95)
  testthat::expect_true(any(curve$gamma > .8) && any(curve$gamma < .8))
  testthat::expect_true(any(curve$gamma == .8))
  for (i in seq_len(nrow(curve))) {
    z <- lapply(paths, function(p) mdc_select(p,curve$gamma[i]))
    complete <- all(vapply(z,nrow,1L)==1)
    testthat::expect_equal(curve$complete[i],complete)
    if (!complete) { testthat::expect_equal(curve$worst_ratio[i],Inf); next }
    ratios <- c(mean(vapply(z[1:2],function(p) p$width,0))/reference[1],
      mean(vapply(z[3:4],function(p) p$width,0))/reference[2])
    testthat::expect_equal(curve$worst_ratio[i],unname(max(ratios)))
    testthat::expect_equal(curve$minimum_attainment[i],min(vapply(z,function(p) as.numeric(p$population_content>=.9),0)))
  }
  testthat::expect_equal(mdc_select(a,.8)$candidate_index,2L)
  testthat::expect_equal(mdc_select(a,.8+1e-12)$candidate_index,3L)
  testthat::expect_true(any(diff(curve$minimum_attainment)<0))
})

testthat::test_that("nominations include both priors and have a bounded one-time refinement", {
  d <- expand.grid(prior_sd=c(1,2), gamma=c(.6,.8,.95))
  d$model <- paste0("sd",d$prior_sd); d$rule <- paste(d$model,d$gamma)
  d$worst_ratio <- rep(c(.7,.9),3); d$mean_log_ratio <- log(d$worst_ratio)
  d$attainment_pass <- d$gamma >= .8; d$complete <- TRUE
  d$reference_dp <- TRUE; d$region_length <- .1
  s <- mjc_nominate(d)
  testthat::expect_setequal(s$prior_sd,c(1,2)); testthat::expect_lte(nrow(s),4L)
  r <- mjc_nominate(d,s$rule,refinement=TRUE)
  testthat::expect_lte(nrow(r),2L); testthat::expect_false(any(r$rule %in% s$rule))
  d$complete <- FALSE; testthat::expect_equal(nrow(mjc_nominate(d)),0L)
})

testthat::test_that("prior-specific ECM tables cannot be relabeled", {
  a <- list(prior=list(prior_sd=2),candidates=joint_candidates())
  rule <- list(prior_sd=1,concentration=1,base_sd=4,gamma=.9)
  testthat::expect_error(mjc_select(seq(-1,1,length.out=50),a,rule,.9,"original"),"different prior")
})

testthat::test_that("target reuse excludes DP choices but includes all root-posterior settings", {
  cfg <- joint_config(); y <- seq(-1,1,length.out=50)
  target <- list(q=.95,geometry=list(delta=0))
  key <- mjc_target_key(y,target,2,cfg,list(policy="unchanged"))
  cfg$dp_concentration <- 123
  testthat::expect_identical(key,mjc_target_key(y,target,2,cfg,list(policy="unchanged")))
  testthat::expect_false(identical(key,mjc_target_key(y,target,1,cfg,list(policy="unchanged"))))
  target$geometry$delta <- .1
  testthat::expect_false(identical(key,mjc_target_key(y,target,2,cfg,list(policy="unchanged"))))
})

testthat::test_that("sampling uses the requested prior and reaches the declared continuation limit", {
  sample <- mjc_sample; environment(sample) <- new.env(parent=environment(mjc_sample))
  seen <- numeric()
  environment(sample)$mgp_slice_fit <- function(y,task,cfg,n_keep) {
    seen <<- c(seen,cfg$prior_variance)
    list(draws=cbind(rep(-1,n_keep),rep(1,n_keep)))
  }
  environment(sample)$mgp_summaries <- function(fits,task,policy) data.frame(
    variable=c("lower","upper","width","midpoint"),mean=c(-1,1,2,0),
    preferred_pass=TRUE,mean_precision=nrow(fits[[1]]$draws)>=8000,rhat=1,ess_bulk=1000,ess_tail=1000,mcse_mean=.001)
  cfg <- joint_config()
  target <- list(q=.95,geometry=list(delta=0),selected=data.frame(lower=-1,upper=1))
  value <- sample(seq(-1,1,length.out=50),target,1,cfg,list(),100)
  testthat::expect_true(value$adequate); testthat::expect_true(all(seen==1))
  testthat::expect_equal(value$history$retained,c(1000,2000,4000,8000))
  testthat::expect_equal(mjc_interval(value),c(-1,1))
  value$parameters$mean[3] <- 3
  testthat::expect_error(mjc_interval(value),"inconsistent")
})

testthat::test_that("developmental advancement is law-specific, paired, and not an attainment floor", {
  cfg <- joint_config()
  d <- data.table::CJ(job_id=1:100,response_id=paste0("law",1:8),rule=c("R0","new"))
  d[, `:=`(n=100L,content=.9,tolerance_confidence=.95,replication=job_id,
    returned=TRUE,adequate=TRUE,attained=TRUE,width=ifelse(rule=="R0",10,8))]
  s <- mjc_scores(d,cfg,joint_radius()); testthat::expect_true(s$scores$advance)
  d[rule=="new" & response_id=="law1" & replication>94,attained:=FALSE]
  testthat::expect_false(mjc_scores(d,cfg,joint_radius())$scores$advance)
  d[rule=="new",attained:=TRUE]; d[rule=="new" & response_id=="law1",width:=10.01]
  testthat::expect_false(mjc_scores(d,cfg,joint_radius())$scores$advance)
  d[rule=="new",width:=8]; d[rule=="new" & job_id==1 & response_id=="law1",adequate:=FALSE]
  testthat::expect_false(mjc_scores(d,cfg,joint_radius())$scores$advance)
})

testthat::test_that("nonreturns are attainment failures and width ranges condition on returns", {
  cfg <- joint_config()
  d <- data.frame(n=50,content=.9,tolerance_confidence=.95,response_id="normal",method="new",
    returned=c(TRUE,TRUE,FALSE),attained=c(TRUE,FALSE,FALSE),width=c(2,4,NA),population_content=c(.95,.8,NA))
  s <- mjc_summary(d,cfg)
  testthat::expect_equal(s$attainment,1/3); testthat::expect_equal(s$width_samples,2L)
  testthat::expect_equal(s$mean_width,3); testthat::expect_equal(s$width_q025,2.05)
  d$returned <- d$attained <- FALSE
  testthat::expect_true(is.na(mjc_summary(d,cfg)$mean_width))
})

testthat::test_that("resumption verifies receipts and never overwrites a saved failure", {
  directory <- tempfile(); dir.create(directory); on.exit(unlink(directory,recursive=TRUE))
  queue <- mjs_queue; environment(queue) <- new.env(parent=environment(mjs_queue))
  environment(queue)$mdc_capacity <- function(...) invisible(TRUE)
  cfg <- list(output_root=directory); tasks <- data.frame(job_id=1:2,seed=10:11)
  calls <- 0L
  fit <- function(task) {calls <<- calls+1L;list(ok=TRUE,task=task,rows=data.frame(x=1))}
  invisible(capture.output(first <- queue(tasks,"test",cfg,"identity",1,fit)))
  invisible(capture.output(queue(tasks,"test",cfg,"identity",1,fit)))
  testthat::expect_equal(calls,2L)
  testthat::expect_error(queue(tasks,"test",cfg,"changed",1,fit),"inputs changed")
  saveRDS(list(ok=TRUE),first$paths[1])
  testthat::expect_error(queue(tasks,"test",cfg,"identity",1,fit),"checksum")
})

testthat::test_that("dependency success requires both a successful exit and completion evidence", {
  d <- tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  testthat::expect_equal(mjs_dependency_status(d),"waiting_for_predecessor")
  mgfp_json_write(list(status="complete_requires_scientific_review"),file.path(d,"study_summary.json"))
  testthat::expect_equal(mjs_dependency_status(d),"waiting_for_predecessor")
  mgfp_json_write(list(exit_code=1L),file.path(d,"controller_exit.json"))
  testthat::expect_equal(mjs_dependency_status(d),"dependency_failed")
  mgfp_json_write(list(exit_code=0L),file.path(d,"controller_exit.json"))
  testthat::expect_equal(mjs_dependency_status(d),"ready")
  mgfp_json_write(list(status="stopped_requires_review"),file.path(d,"study_summary.json"))
  testthat::expect_equal(mjs_dependency_status(d),"dependency_failed")
})

testthat::test_that("historical seed collisions are moved deterministically without changing other streams", {
  x <- mjs_reserved_blocks(1000,1:4,200,c(1411,1814),10:20)
  testthat::expect_equal(x,c(1200,3000,1600,3200))
  testthat::expect_identical(x,mjs_reserved_blocks(1000,1:4,200,c(1411,1814),10:20))
  testthat::expect_error(mjs_reserved_blocks(1000,1:4,200,c(1411,1814),10),"exhausted")
})

testthat::test_that("local evidence remains verifiable after moving its directory", {
  d <- tempfile();dir.create(d); on.exit(unlink(d,recursive=TRUE))
  mgfp_json_write(list(status="complete_no_developmental_gain"),file.path(d,"study_summary.json"))
  cfg <- joint_config();cfg$output_root <- d
  mjs_preserve(cfg,list(source_commit="test"))
  invisible(capture.output(testthat::expect_true(mjs_verify(d,cfg))))
  mgfp_json_write(list(status="changed"),file.path(d,"study_summary.json"))
  testthat::expect_error(mjs_verify(d,cfg),"Evidence changed")
})

testthat::test_that("equal DP-selected targets share draws, while changed priors do not", {
  fit <- mjs_gibbs; environment(fit) <- new.env(parent=environment(mjs_gibbs))
  y <- seq(-1,1,length.out=100); cfg <- joint_config(); calls <- numeric()
  target <- list(q=.96,delta_reference=0,geometry=list(delta=0,original_margin=.2),
    selected=data.frame(candidate_index=1L,lower=-1,upper=1,posterior_content_probability=.999))
  environment(fit)$mjc_select <- function(...) target
  environment(fit)$mjr_sample_checked <- function(y,target,sd,cfg,policy,seed) {
    calls <<- c(calls,sd)
    list(adequate=TRUE,fits=list(list(draws=cbind(rep(-1,10),rep(1,10)))),
      parameters=data.frame(variable=c("lower","upper","width","midpoint"),mean=c(-1,1,2,0),
        q=target$q,delta=0,rhat=1,ess_bulk=1000,ess_tail=1000,mcse_mean=.001))
  }
  context <- list(laws=list(normal=list(r=function(n) y,p=pnorm)),policy_diagnostics=list(),
    old_tilts=data.frame(n=100L,content=.9,variant="sqrt_rank"))
  task <- data.frame(job_id=1L,n=100L,content=.9,tolerance_confidence=.95,response_id="normal",
    replication=1L,data_seed=17L,chain_seed=1000L)
  a <- list(sample_sha256=mwd_hash(y)); anchors <- list("2"=a,"1"=a)
  rules <- data.frame(n=100L,content=.9,rule=c("R0","other_dp","sd1"),prior_sd=c(2,2,1),
    concentration=c(1,2,1),base_sd=4,gamma=.99,slot=1:3)
  value <- fit(task,anchors,rules,cfg,context)
  testthat::expect_true(value$ok);testthat::expect_equal(calls,c(2,1))
  testthat::expect_length(value$records,2L);testthat::expect_equal(nrow(value$rows),3L)
  testthat::expect_true(all(value$rows$radius_multiplier==1))
  value2 <- fit(task,anchors,rules,cfg,context,previous=value)
  testthat::expect_equal(value2$new_targets,0L);testthat::expect_equal(calls,c(2,1))
})

testthat::test_that("nonadvancing designs retain R0 explicitly rather than inventing a new rule", {
  selections <- data.table::data.table(n=c(100,100),content=.9,rule=c("R0","new"),slot=1:2)
  scores <- data.table::data.table(n=100,content=.9,rule="new",advance=FALSE,worst_ratio=.8,mean_log_ratio=log(.8))
  frozen <- mjs_freeze(scores,selections,joint_config())
  testthat::expect_true(all(frozen$rule=="R0"));testthat::expect_false(any(frozen$advanced))
  testthat::expect_equal(nrow(frozen),1L)
  testthat::expect_false("gibbs_new_unscaled" %in% frozen$method)
  scores$advance <- TRUE
  frozen <- mjs_freeze(scores,selections,joint_config())
  testthat::expect_equal(frozen[method=="gibbs_new_unscaled",rule],"new")
  testthat::expect_equal(sum(frozen$advanced),1L)
})

testthat::test_that("paired figures and summaries keep distributions separate and raw widths", {
  d <- tempfile();dir.create(d);dir.create(file.path(d,"validation"));on.exit(unlink(d,recursive=TRUE))
  cfg <- joint_config();cfg$output_root <- d
  rows <- data.table::CJ(n=100L,content=.9,response_id=c("normal","exponential"),
    method=c("gibbs_new_unscaled","gibbs_R0_unscaled","gibbs_R0_scaled","ecm_primary","young_mathew","wilks"),
    replication=1:3)
  rows[, `:=`(job_id=replication+ifelse(response_id=="normal",0L,3L),tolerance_confidence=.95,
    returned=TRUE,attained=TRUE,width=ifelse(method=="gibbs_new_unscaled",2,3),population_content=.98)]
  rows[method=="gibbs_R0_unscaled",width:=4]
  invisible(mjs_report(rows,cfg))
  testthat::expect_true(file.info(file.path(d,"validation/width_and_attainment.pdf"))$size>1000)
  paired <- data.table::fread(file.path(d,"validation/paired_comparison.csv"))
  testthat::expect_equal(nrow(paired),2L);testthat::expect_true(all(paired$mean_paired_width_change == -1))
  testthat::expect_true(all(paired$reference_method == "gibbs_R0_scaled"))
  summary <- mjc_summary(rows,cfg)
  testthat::expect_true(all(is.na(summary[method!="gibbs_new_unscaled",simultaneous_lower])))
})

testthat::test_that("independent assessment shares the R0 draws and preserves primary ECM", {
  job <- mjs_final_job; environment(job) <- new.env(parent=environment(mjs_final_job))
  priors <- numeric(); y <- seq(-1,1,length.out=100)
  environment(job)$mjc_fit_anchor <- function(y,task,sd,cfg,context) {
    priors <<- c(priors,sd);list(candidates=joint_candidates())
  }
  environment(job)$mjs_gibbs <- function(task,anchors,rules,cfg,context) {
    half <- ifelse(rules$method=="gibbs_new_unscaled",.8,1)
    list(ok=TRUE,rows=data.table::data.table(task[rep(1,nrow(rules)),],rule=rules$rule,method=rules$method,
      returned=TRUE,adequate=TRUE,lower=-half,upper=half,width=2*half,
      population_content=pnorm(half)-pnorm(-half),attained=FALSE,radius_multiplier=1))
  }
  environment(job)$mfp_comparators <- function(...) data.frame(method=c("young_mathew","wilks"),
    returned=TRUE,lower=c(-1.5,-2),upper=c(1.5,2),width=c(3,4),failure_reason="")
  cfg <- joint_config()
  task <- data.frame(job_id=1L,n=100L,content=.9,tolerance_confidence=.95,response_id="normal",data_seed=17L)
  rules <- data.frame(n=100L,content=.9,prior_sd=c(2,1),rule=c("R0","new"),
    method=c("gibbs_R0_unscaled","gibbs_new_unscaled"),advanced=c(FALSE,TRUE))
  context <- list(laws=list(normal=list(r=function(n) y,p=pnorm)),
    policy=data.frame(n=100L,content=.9,screen=.9),
    radius=data.table::data.table(n=100L,content=.9,rule="proposed",multiplier=1.1))
  value <- job(task,rules,cfg,context)
  testthat::expect_equal(priors,c(2,1));testthat::expect_equal(nrow(value$rows),6L)
  testthat::expect_equal(value$rows[method=="gibbs_R0_scaled",width],2.2)
  testthat::expect_equal(value$rows[method=="gibbs_new_unscaled",width],1.6)
  testthat::expect_equal(value$rows[method=="ecm_primary",width],6)
  testthat::expect_false(anyDuplicated(value$rows$method)>0)
})

testthat::test_that("calibrated scoring changes only the reference and keeps exact scale relations", {
  cfg <- joint_config()
  d <- data.table::CJ(job_id=1:100,response_id=paste0("law",1:8),rule=c("R0","new"))
  d[, `:=`(n=100L,content=.9,tolerance_confidence=.95,replication=job_id,
    returned=TRUE,adequate=TRUE,attained=TRUE,width=ifelse(rule=="R0",10,8+job_id/1000))]
  before <- data.table::copy(d); baseline <- mjc_scores(d,cfg,joint_radius())
  for (s in c(.8,1,1.2)) {
    out <- mjc_scores(d,cfg,joint_radius(s))
    testthat::expect_equal(out$cells$mean_ratio,baseline$cells$mean_ratio/s)
    testthat::expect_equal(out$cells$paired_log_upper,baseline$cells$paired_log_upper-log(s))
    testthat::expect_identical(d,before)
  }
  testthat::expect_error(mjc_scores(d,cfg,rbind(joint_radius(),joint_radius())),"Duplicate")
  testthat::expect_error(mjc_scores(d,cfg,joint_radius(-1)),"invalid")
  r <- joint_radius(); r$tolerance_confidence <- .9
  testthat::expect_error(mjc_scores(d,cfg,r),"Missing")
  testthat::expect_error(mjc_scores(d,cfg,r[0,]),"Missing")
  testthat::expect_error(mjc_scores(d[!(rule=="R0" & job_id==1)],cfg,joint_radius()),"Missing R0")
})

testthat::test_that("only advancing designs enter the fixed independent domain", {
  tasks <- data.frame(job_id=1:6,n=rep(c(100L,500L),each=3),content=.9,tolerance_confidence=.95,data_seed=101:106)
  frozen <- data.frame(n=c(100L,500L),content=.9,tolerance_confidence=.95,advanced=c(FALSE,TRUE))
  d <- mjs_validation_domain(tasks,frozen)
  testthat::expect_identical(d,tasks[4:6,,drop=FALSE],ignore_attr=TRUE)
  testthat::expect_identical(d$job_id,4:6)
  frozen$advanced <- FALSE
  testthat::expect_equal(nrow(mjs_validation_domain(tasks,frozen)),0L)
  testthat::expect_equal(joint_config()$assurance$comparisons,72L)
})

testthat::test_that("function fingerprints ignore prose and score edits, and detect computation changes", {
  path <- tempfile(fileext=".R"); on.exit(unlink(path))
  writeLines(c("fit <- function(x) x + 1", "score <- function(x) x / 2"),path)
  a <- mjr_function(path,"fit")
  writeLines(c("# A reporting revision", "fit <- function(x) x + 1", "score <- function(x) x / 4"),path)
  testthat::expect_identical(a,mjr_function(path,"fit"))
  writeLines("fit <- function(x) x + 2",path)
  testthat::expect_false(identical(a,mjr_function(path,"fit")))
})

testthat::test_that("posterior target identities do not depend on developmental scoring", {
  cfg <- joint_config(); target <- list(q=.95,geometry=list(delta=0)); y <- c(-1,0,1)
  before <- mjc_target_key(y,target,2,cfg,list())
  cfg$selection$minimum_worst_law_width_reduction <- .02
  testthat::expect_identical(before,mjc_target_key(y,target,2,cfg,list()))
  cfg$sampling$n_mcmc <- 2000
  testthat::expect_false(identical(before,mjc_target_key(y,target,2,cfg,list())))
})

testthat::test_that("mode reuse checks original bytes, task, computation and observations", {
  source <- tempfile(); target <- tempfile()
  dir.create(file.path(source,"modes/fits"),recursive=TRUE);dir.create(target)
  on.exit(unlink(c(source,target),recursive=TRUE))
  cfg <- joint_config(); cfg$output_root <- target
  tasks <- data.frame(job_id=1L,n=3L,response_id="normal",data_seed=10L)
  old <- list(source_commit=mgfp_git("rev-parse","HEAD"),configuration=cfg,programs=list())
  id <- mwd_hash(list(old,"modes",tasks))
  mgfp_json_write(list(identity=old),file.path(source,"manifest.json"))
  mgfp_json_write(list(analysis_id=id),file.path(source,"modes/manifest.json"))
  # JSON is the canonical identity representation used by the actual driver.
  old <- mwd_json(file.path(source,"manifest.json"))$identity
  id <- mwd_hash(list(old,"modes",tasks))
  mgfp_json_write(list(analysis_id=id),file.path(source,"modes/manifest.json"))
  set.seed(10); y <- rnorm(3)
  anchors <- lapply(c(2,1),function(sd) list(prior=list(prior_sd=sd),sample_sha256=mwd_hash(y),
    candidates=joint_candidates()));names(anchors)<-c("2","1")
  path <- file.path(source,"modes/fits/fit_000001.rds")
  mbc_atomic_fit(list(ok=TRUE,task=tasks[1,],anchors=anchors),path,id,tasks[1,])
  current <- old
  current$mode_reuse <- list(output_root=source,checkout=root,source_commit=old$source_commit,
    manifest_sha256=mwd_file_hash(file.path(source,"manifest.json")))
  context <- list(laws=list(normal=list(r=rnorm)))
  invisible(capture.output(testthat::expect_equal(mjs_import_modes(tasks,cfg,current,context),1L)))
  newpath <- file.path(target,"modes/fits/fit_000001.rds")
  testthat::expect_identical(mwd_file_hash(path),mwd_file_hash(newpath))
  testthat::expect_identical(mbc_read_fit(newpath,mwd_hash(list(current,"modes",tasks)),tasks[1,])$anchors,anchors)
  invisible(capture.output(testthat::expect_equal(mjs_import_modes(tasks,cfg,current,context),1L)))
  changed <- current;changed$configuration$root_sd<-c(2,.5)
  testthat::expect_error(mjs_import_modes(tasks,cfg,changed,context),"settings differ")
  saveRDS(list(ok=TRUE),path)
  testthat::expect_error(mjs_import_modes(tasks,cfg,current,context),"checksum")
})

joint_precision_record <- function(keep = 8000L) {
  draws <- cbind(lower=rep(-1,keep),upper=rep(1,keep))
  fit <- list(draws=draws,warmup_root1=matrix(1:4,2),warmup_root2=matrix(5:8,2),
    retained_objective=rep(0,keep))
  list(fits=rep(list(fit),4), parameters=data.frame(variable=c("lower","upper","width","midpoint"),
    mean=c(-1,1,2,0),q=.96,delta=0,preferred_pass=TRUE,mean_precision=c(TRUE,TRUE,FALSE,TRUE),
    rhat=1,ess_bulk=1000,ess_tail=1000,mcse_mean=.02),
    history=data.frame(retained=keep,adequate=FALSE,max_rhat=1,min_bulk_ess=1000,
      min_tail_ess=1000,max_mcse_mean=.02), adequate=FALSE,prior_sd=1,q=.96,delta=0,seed=100L)
}

testthat::test_that("precision extension is separate from unchanged posterior target identities", {
  cfg <- joint_config(); y <- c(-1,0,1); target <- list(q=.96,geometry=list(delta=0))
  key <- mjc_target_key(y,target,1,cfg,list())
  cfg$precision_extension$retained_per_chain <- 64000
  testthat::expect_identical(key,mjc_target_key(y,target,1,cfg,list()))
  testthat::expect_error(mjc_validate(cfg),"specification")
  r <- joint_precision_record(); testthat::expect_true(mjp_precision_only(r))
  r$adequate <- TRUE; testthat::expect_false(mjp_precision_only(r))
  r <- joint_precision_record();r$parameters$preferred_pass[1]<-FALSE
  testthat::expect_false(mjp_precision_only(r))
  r$parameters$preferred_pass[1]<-NA;testthat::expect_false(mjp_precision_only(r))
})

testthat::test_that("same-target extension preserves prefixes and stops at its finite precision limit", {
  extend <- mjp_extend; environment(extend) <- new.env(parent=environment(mjp_extend))
  original <- joint_precision_record(); seen <- list()
  environment(extend)$mgp_slice_fit <- function(y,task,cfg,n_keep) {
    seen[[length(seen)+1L]] <<- list(task=task,settings=cfg,keep=n_keep,y=y)
    f <- original$fits[[task$chain]]
    f$draws <- f$draws[rep(1L,n_keep),,drop=FALSE];rownames(f$draws)<-NULL
    f$retained_objective <- rep(0,n_keep);f
  }
  environment(extend)$mgp_summaries <- function(fits,task,policy) {
    p <- original$parameters;p$mean_precision <- nrow(fits[[1]]$draws)>=64000;p
  }
  cfg <- joint_config(); target <- list(q=.96,content=.9,geometry=list(delta=0),selected=data.frame(lower=-1,upper=1))
  y <- c(-1,0,1)
  result <- extend(original,y,target,1,cfg,list(),100L)
  testthat::expect_true(result$adequate)
  testthat::expect_equal(result$history$retained,c(8000,32000,64000))
  testthat::expect_true(result$precision_extension$exact_prefix_preserved)
  testthat::expect_identical(result$precision_extension$original_record_sha256,mwd_hash(original))
  testthat::expect_equal(vapply(seen,function(x) x$task$seed,0L),rep(101:104,2))
  testthat::expect_true(all(vapply(seen,function(x) identical(x$y,y) && x$settings$prior_variance==1 &&
    x$task$q==.96 && x$task$delta==0 && x$settings$n_burn==500, FALSE)))
  testthat::expect_identical(original,joint_precision_record())
  bad <- target;bad$geometry$delta<-.1
  testthat::expect_error(extend(original,y,bad,1,cfg,list(),100L),"target or seed")
  testthat::expect_error(extend(original,y,target,2,cfg,list(),100L),"target or seed")
  testthat::expect_error(extend(original,y,target,1,cfg,list(),200L),"target or seed")
  adequate <- original;adequate$adequate<-TRUE
  testthat::expect_error(extend(adequate,y,target,1,cfg,list(),100L),"Only mean-precision")
  cfg$precision_extension$retained_per_chain <- 128000
  testthat::expect_error(extend(original,y,target,1,cfg,list(),100L),"bounded")
  cfg <- joint_config()
  environment(extend)$mgp_slice_fit <- function(y,task,cfg,n_keep) {
    f <- original$fits[[task$chain]]; f$draws <- f$draws[rep(1L,n_keep),,drop=FALSE]
    f$draws[1,1]<--2;f$retained_objective<-rep(0,n_keep);f
  }
  testthat::expect_error(extend(original,y,target,1,cfg,list(),100L),"prefix changed")
})

testthat::test_that("longer draws do not authorize another attempt after mixing degrades", {
  extend <- mjp_extend;environment(extend)<-new.env(parent=environment(mjp_extend))
  original <- joint_precision_record();calls<-0L
  environment(extend)$mgp_slice_fit <- function(y,task,cfg,n_keep) {
    calls <<- calls+1L; f<-original$fits[[task$chain]]
    f$draws<-f$draws[rep(1L,n_keep),,drop=FALSE];f$retained_objective<-rep(0,n_keep);f
  }
  environment(extend)$mgp_summaries <- function(...) {
    p<-original$parameters;p$preferred_pass<-FALSE;p
  }
  target<-list(q=.96,content=.9,geometry=list(delta=0),selected=data.frame(lower=-1,upper=1))
  value<-extend(original,c(-1,0,1),target,1,joint_config(),list(),100L)
  testthat::expect_false(value$adequate);testthat::expect_equal(calls,4L)
  testthat::expect_equal(tail(value$history$retained,1),32000)
})

testthat::test_that("recovery preserves adequate records and rejects relabeled targets", {
  before<-list(task=data.frame(job_id=1304L),records=list(good=list(adequate=TRUE,x=1),bad=joint_precision_record()))
  before$rows<-data.table::data.table(rule="x",prior_sd=1,concentration=1,base_sd=4,gamma=.97,q=.96,delta=0,
    original_delta=0,tilt_variant="original",original_margin=.1,selected_candidate=1L,ecm_lower=-1,ecm_upper=1,
    ecm_dp_probability=.99,sample_sha256="sample",target_key="target",radius_multiplier=1)
  after<-before;after$ok<-TRUE
  after$records$bad$adequate<-TRUE
  after$records$bad$parameters$mean_precision<-TRUE
  after$records$bad$precision_extension<-list(original_record_sha256=mwd_hash(before$records$bad))
  after$records$bad$fits<-lapply(before$records$bad$fits,function(f) {
    f$draws<-rbind(f$draws,f$draws);rownames(f$draws)<-NULL
    f$retained_objective<-rep(f$retained_objective,2);f
  })
  testthat::expect_true(mjr_check_recovered(before,after))
  changed<-after;changed$records$good$x<-2
  testthat::expect_error(mjr_check_recovered(before,changed),"adequate target")
  changed<-after;changed$rows<-data.table::copy(after$rows);changed$rows$q<-.99
  testthat::expect_error(mjr_check_recovered(before,changed),"selected targets")
  changed<-after;changed$records$bad$fits[[1]]$retained_objective[1]<-1
  testthat::expect_error(mjr_check_recovered(before,changed),"prefix changed")
})

testthat::test_that("development imports retain original bytes, receipts, and deterministic restart", {
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  from<-file.path(d,"original.rds");to<-file.path(d,"new/fit.rds")
  task<-data.frame(job_id=1L);value<-list(ok=TRUE,task=task,x=1)
  mbc_atomic_fit(value,from,"old",task)
  origin<-list(file=from,sha256=mwd_file_hash(from),receipt_sha256=mwd_file_hash(paste0(from,".json")))
  a<-mjr_copy(from,to,"new",task,origin);b<-mjr_copy(from,to,"new",task,origin)
  testthat::expect_identical(a,b);testthat::expect_identical(mwd_file_hash(from),mwd_file_hash(to))
  changed<-origin;changed$sha256<-"bad"
  testthat::expect_error(mjr_copy(from,to,"new",task,changed),"origin changed")
  saveRDS(list(ok=TRUE),to)
  testthat::expect_error(mjr_copy(from,to,"new",task,origin),"checksum")
})

testthat::test_that("only the precision wrapper is interchangeable in recovery computation checks", {
  path<-tempfile(fileext=".R");on.exit(unlink(path))
  writeLines("fit <- function(x) mjc_sample(x, q=1)",path)
  original<-mjr_function(path,"fit",TRUE)
  writeLines("fit <- function(x) mjr_sample_checked(x, q=1)",path)
  testthat::expect_identical(original,mjr_function(path,"fit",TRUE))
  testthat::expect_false(identical(original,mjr_function(path,"fit")))
  writeLines("fit <- function(x) mjr_sample_checked(x, q=2)",path)
  testthat::expect_false(identical(original,mjr_function(path,"fit",TRUE)))
})

testthat::test_that("development reuse rejects changed scientific settings and unreviewed sources", {
  cfg<-joint_config()
  old<-list(source_commit=cfg$development_recovery$source_commit,configuration=cfg)
  current<-list(configuration=cfg,mode_reuse=list(stage="development"))
  changed<-current;changed$configuration$selection$minimum_worst_law_width_reduction<-.02
  testthat::expect_error(mjr_check_development(old,changed,data.frame()),"settings differ")
  changed<-current;changed$configuration$dp_concentration<-c(1,2,3)
  testthat::expect_error(mjr_check_development(old,changed,data.frame()),"settings differ")
  changed<-old;changed$source_commit<-"unreviewed"
  testthat::expect_error(mjr_check_development(changed,current,data.frame()),"not been reviewed")
  changed<-current;changed$mode_reuse$stage<-"modes"
  testthat::expect_error(mjr_check_development(old,changed,data.frame()),"not been reviewed")
})

testthat::test_that("imported data.table rule columns do not mask the selected rule object", {
  check<-mjr_check_result;environment(check)<-new.env(parent=environment(mjr_check_result))
  y<-c(-1,0,1);task<-data.frame(job_id=1L,n=3L,content=.9,data_seed=10L,response_id="normal")
  target<-list(q=.96,geometry=list(delta=0),selected=data.frame(candidate_index=1L,lower=-1,upper=1))
  environment(check)$mjc_select<-function(...) target
  environment(check)$mjc_target_key<-function(...) "target"
  r<-joint_precision_record();r$adequate<-TRUE;r$parameters$mean_precision<-TRUE
  rows<-data.table::data.table(rule="R0",returned=TRUE,sample_sha256=mwd_hash(y),target_key="target",
    q=.96,delta=0,prior_sd=1,gamma=.97,concentration=1,base_sd=4,selected_candidate=1L,
    ecm_lower=-1,ecm_upper=1,adequate=TRUE,lower=-1,upper=1)
  value<-list(ok=TRUE,task=task,rows=rows,records=list(target=r))
  rules<-data.frame(n=3L,content=.9,rule="R0",prior_sd=1,gamma=.97,concentration=1,base_sd=4)
  context<-list(laws=list(normal=list(r=function(n)y)),policy_diagnostics=list(),
    old_tilts=data.frame(n=3L,content=.9,variant="original"))
  testthat::expect_identical(check(value,task,list("1"=list()),rules,joint_config(),context),y)
  value$ok<-FALSE
  testthat::expect_error(check(value,task,list("1"=list()),rules,joint_config(),context),"status")
})

testthat::test_that("the automatic wrapper leaves adequate and mixing-limited records unchanged", {
  sample<-mjr_sample_checked;environment(sample)<-new.env(parent=environment(mjr_sample_checked))
  record<-joint_precision_record();called<-FALSE
  environment(sample)$mjc_sample<-function(...) record
  environment(sample)$mjp_extend<-function(...) {called<<-TRUE;stop("Unexpected extension")}
  record$adequate<-TRUE
  testthat::expect_identical(sample(NULL,NULL,NULL,NULL,NULL,NULL),record)
  record$adequate<-FALSE;record$parameters$preferred_pass[1]<-FALSE
  testthat::expect_identical(sample(NULL,NULL,NULL,NULL,NULL,NULL),record)
  testthat::expect_false(called)
})
