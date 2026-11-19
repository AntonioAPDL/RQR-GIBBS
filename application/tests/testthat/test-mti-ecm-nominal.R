root <- normalizePath(if(file.exists("application/scripts/lib/mti_ecm_nominal.R"))"." else "../../..")
for(name in c("mti_gibbs_practical","mti_gibbs_full_report_review","mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis","mti_fixed_prior","mti_gibbs_boundary_calibration",
  "mti_gibbs_decision_calibration","mti_gibbs_joint","mti_ecm_nominal","mti_ecm_nominal_finalization"))
  source(file.path(root,"application/scripts/lib",paste0(name,".R")))
nominal_config <- function()mwd_json(file.path(root,"application/config/mti_ecm_nominal_20261119.json"))
nominal_candidates <- function()data.frame(candidate_index=1:3,target_content=c(.92,.96,.99),
  mean_tilt=0,central_tilt=0,lower=-c(1,2,3),upper=c(1,2,3),width=c(2,4,6),
  finite_interval=TRUE,numerically_accurate=TRUE,posterior_content_probability=c(.7,.9,.999))

testthat::test_that("the experiment fixes the model and excludes Gibbs and automatic publication", {
  cfg <- nominal_config();testthat::expect_true(mec_validate(cfg))
  for(field in c("sample_gibbs","automatic_article_replacement","application_transfer")) {
    bad <- cfg;bad[[field]] <- TRUE;testthat::expect_error(mec_validate(bad),"specification")
  }
  cfg$prior_sd <- 1;testthat::expect_error(mec_validate(cfg),"specification")
})

testthat::test_that("the primary ECM cutoff never comes from a Gibbs reference", {
  context <- list(policy=data.frame(n=500,content=.9,posterior_confidence=.995),
    reference=data.frame(n=500,content=.9,gamma=.8))
  testthat::expect_equal(mec_primary(context,500,.9),.995)
  testthat::expect_error(mec_primary(context,100,.9),"missing")
})

testthat::test_that("canonical RDS task reuse preserves identity and rejects changed caches", {
  prototype <- data.frame(n=c(50L,100L),content=c(.9,.95),kappa=c(2,2),response_id=c("a","b"))
  attr(prototype,"row.names") <- c(1L,3L)
  file <- tempfile(fileext=".rds");on.exit(unlink(file))
  testthat::expect_identical(mec_development_tasks(file,mwd_hash(prototype),function()prototype),prototype)
  testthat::expect_identical(mec_development_tasks(file,mwd_hash(prototype),function()stop("not reused")),prototype)
  testthat::expect_error(mec_development_tasks(file,"changed",function()prototype),"hash differs")
})

testthat::test_that("upper compatibility is computed from counts, not a guessed attainment floor", {
  value <- mec_upper(c(0,94,100),100,.05)
  testthat::expect_equal(value[2],qbeta(.95,95,6))
  testthat::expect_equal(value[3],1)
  testthat::expect_gt(value[2],.95)
  testthat::expect_lt(qbeta(.05,94,7),.95)
  testthat::expect_error(mec_upper(101,100,.05),"counts")
})

testthat::test_that("event curves reproduce direct selection and both directions of attainment changes", {
  a <- nominal_candidates();a$response_id <- "a";a$population_content <- c(.93,.88,.99)
  b <- a;b$response_id <- "b";b$population_content <- c(.85,.96,.99)
  b$posterior_content_probability <- c(.6,.85,.998)
  paths <- list(a,a,b,b)
  curve <- mec_curve(paths,c(a=4,b=4),.9,.9,.95,.05)
  for(i in seq_len(nrow(curve))) {
    z <- lapply(paths,function(x)mdc_select(x,curve$gamma[i]))
    complete <- all(vapply(z,nrow,1L)==1)
    testthat::expect_identical(curve$complete[i],complete)
    if(!complete)next
    rates <- c(mean(vapply(z[1:2],function(x)as.numeric(x$population_content>=.9),0)),
      mean(vapply(z[3:4],function(x)as.numeric(x$population_content>=.9),0)))
    widths <- vapply(z,function(x)x$width,0)
    testthat::expect_equal(curve$minimum_attainment[i],min(rates))
    testthat::expect_equal(curve$maximum_attainment[i],max(rates))
    testthat::expect_equal(curve$maximum_absolute_deviation[i],max(abs(rates-.95)))
    testthat::expect_equal(curve$worst_ratio[i],max(mean(widths[1:2]),mean(widths[3:4]))/4)
  }
  testthat::expect_equal(mdc_select(a,.9)$candidate_index,2L)
  testthat::expect_equal(mdc_select(a,.9+1e-12)$candidate_index,3L)
  testthat::expect_true(any(diff(curve$minimum_attainment)<0))
})

testthat::test_that("the three criteria differ and unavailable rules are never relabeled references", {
  d <- data.frame(gamma=c(.7,.8,.995),region_lower=c(0,.7,.9),region_upper=c(.7,.9,1),
    region_length=c(.7,.2,.1),complete=TRUE,laws=8,samples_per_law=100,
    worst_ratio=c(.7,.9,1),mean_log_ratio=log(c(.7,.9,1)),
    minimum_attainment=c(.90,.94,.99),maximum_attainment=c(.98,.98,1),
    maximum_absolute_deviation=c(.05,.03,.05),mean_absolute_deviation=c(.04,.02,.045),
    minimum_upper_bound=mec_upper(c(90,94,99),100,.05),reference_threshold=c(FALSE,FALSE,TRUE))
  policy <- mec_select_rules(d,.95,nominal_config())
  testthat::expect_equal(policy[method=="ecm_point_target",gamma],.995)
  testthat::expect_equal(policy[method=="ecm_compatible_target",gamma],.8)
  testthat::expect_equal(policy[method=="ecm_closest_nominal",gamma],.8)
  d$minimum_attainment <- .94
  policy <- mec_select_rules(d,.95,nominal_config())
  testthat::expect_false(policy[method=="ecm_point_target",available])
  testthat::expect_true(is.na(policy[method=="ecm_point_target",gamma]))
})

testthat::test_that("the fitter explicitly disables sampling and fixes a sample-independent prior", {
  fit <- mec_fit;environment(fit)<-new.env(parent=environment(mec_fit))
  seen <- NULL
  environment(fit)$mfp_fit_data <- function(...,sample_gibbs) {
    args <- list(...);seen <<- list(args=args,sample_gibbs=sample_gibbs)
    list(returned=TRUE,unresolved_candidates=0,prior=list(prior_sd=2,data_adaptive_prior=FALSE),
      sample_sha256=mwd_hash(args[[1]]),candidates=data.frame(numerically_accurate=TRUE))
  }
  context <- list(policy=data.frame(n=100,content=.9),prior=list(simulation_reference=list(),numerical=list()),
    profile=list(),dp=list(),policy_diagnostics=list())
  task <- data.frame(n=100,content=.9,tolerance_confidence=.95,retained_fraction=.98,ecm_seed=7)
  value <- fit(1:100,task,nominal_config(),context)
  testthat::expect_false(seen$sample_gibbs)
  testthat::expect_equal(seen$args[[5]],2)
  testthat::expect_null(value$fits)
})

testthat::test_that("interval selection preserves roots and does not substitute missing candidates", {
  intervals <- mec_intervals;environment(intervals)<-new.env(parent=environment(mec_intervals))
  environment(intervals)$mfp_accuracy <- function(...)list(pass=TRUE)
  task <- data.frame(n=100,content=.9)
  policy <- data.frame(n=100,content=.9,method=c("ecm_primary","other"),gamma=c(.9,.7),available=TRUE)
  rows <- intervals(1:100,task,list(candidates=nominal_candidates()),policy,nominal_config(),list())
  testthat::expect_equal(rows$width,c(4,2));testthat::expect_equal(rows$q,c(.96,.92))
  policy$gamma <- c(.9999,.9999)
  rows <- intervals(1:100,task,list(candidates=nominal_candidates()),policy,nominal_config(),list())
  testthat::expect_true(all(!rows$returned));testthat::expect_true(all(is.na(rows$width)))
})

testthat::test_that("nominal attainment can straddle its confidence interval while widths remain raw", {
  d <- data.table::data.table(n=100,content=.9,tolerance_confidence=.95,response_id="fixture",method="ecm_primary",
    returned=TRUE,attained=c(rep(TRUE,950),rep(FALSE,50)),width=1:1000)
  summary <- mec_summary(d,nominal_config())
  testthat::expect_equal(summary$attainment,.95)
  testthat::expect_lt(summary$lower,.95);testthat::expect_gt(summary$upper,.95)
  testthat::expect_equal(summary$width_q025,unname(quantile(1:1000,.025)))
  testthat::expect_equal(summary$width_q975,unname(quantile(1:1000,.975)))
})

testthat::test_that("the reporting and verification path reproduces results and detects corruption", {
  directory <- tempfile();dir.create(file.path(directory,"validation"),recursive=TRUE)
  on.exit(unlink(directory,recursive=TRUE))
  cfg <- nominal_config();cfg$output_root <- directory;cfg$expected_cells <- 1;cfg$validation_replications <- 2
  tasks <- data.frame(job_id=1:2,n=100,content=.9,response_id="normal")
  policy <- data.table::data.table(n=100,content=.9,method="ecm_primary",available=TRUE,gamma=.995,reason="")
  source <- file.path(root,"application/config/mti_ecm_nominal_20261119.json")
  mgfp_json_write(list(identity=list(programs=mgfp_hashes(source),article_files=mgfp_hashes(source))),
    file.path(directory,"manifest.json"))
  freeze <- mec_freeze(policy,tasks,directory,"fixture")
  testthat::expect_identical(mec_freeze(policy,tasks,directory,"fixture"),freeze)
  rows <- data.table::rbindlist(lapply(c("ecm_primary","young_mathew","wilks"),function(m)
    data.frame(job_id=1:2,n=100,content=.9,tolerance_confidence=.95,response_id="normal",method=m,
      lower=0,upper=c(.92,.98),width=c(.92,.98),population_content=c(.92,.98),
      returned=TRUE,attained=TRUE,numerically_accurate=TRUE)))
  mwd_write(rows,file.path(directory,"validation/interval_results.csv"))
  mec_report(rows,cfg)
  context <- list(laws=list(normal=list(p=punif)))
  testthat::expect_true(mec_verify(cfg,context))
  testthat::expect_gt(file.info(file.path(directory,"validation/exploratory_comparisons.pdf"))$size,1000)
  p <- data.table::fread(file.path(directory,"validation/paired_comparison.csv"));p$mean_width_change[1] <- 1
  mwd_write(p,file.path(directory,"validation/paired_comparison.csv"))
  testthat::expect_error(mec_verify(cfg,context),"paired comparison")
  rows$lower[1] <- .1
  mec_write_intervals(rows,file.path(directory,"validation/interval_results.csv"))
  testthat::expect_error(mec_verify(cfg,context),"Population contents or widths")
})

testthat::test_that("freezing records unavailable rules and refuses later alteration", {
  directory <- tempfile();dir.create(directory);on.exit(unlink(directory,recursive=TRUE))
  p <- data.table::data.table(n=100,content=.9,method=c("ecm_primary","unavailable"),
    available=c(TRUE,FALSE),gamma=c(.995,NA_real_),reason=c("","not_nominated"))
  tasks <- data.frame(job_id=1L)
  frozen <- mec_freeze(p,tasks,directory,"fixture")
  testthat::expect_equal(frozen$unavailable_rules$method,"unavailable")
  testthat::expect_identical(mec_freeze(p,tasks,directory,"fixture"),frozen)
  changed <- data.table::copy(p);changed$gamma[1] <- .9
  testthat::expect_error(mec_freeze(changed,tasks,directory,"fixture"),"rules changed")
  unlink(file.path(directory,"frozen_policy.json"));dir.create(file.path(directory,"validation/fits"),recursive=TRUE)
  saveRDS(list(),file.path(directory,"validation/fits/fit_000001.rds"))
  testthat::expect_error(mec_freeze(p,tasks,directory,"fixture"),"after fresh fitting")
})

testthat::test_that("interval export preserves doubles at a Gamma support boundary", {
  directory <- tempfile();dir.create(directory);on.exit(unlink(directory,recursive=TRUE))
  cdf <- function(y)pgamma(y*sqrt(.5)+.5,shape=.5)
  d <- data.frame(method=c("gamma","normal"),lower=c(-sqrt(.5)+1e-12,-1.2345678901234567),
    upper=c(6.03907312052499,2.3456789012345678),q=c(.99999999999999,NA_real_))
  d$width <- d$upper-d$lower;d$population_content <- cdf(d$upper)-cdf(d$lower)
  old <- file.path(directory,"old.csv");mwd_write(d,old)
  rounded <- data.table::fread(old)
  testthat::expect_gt(abs(cdf(rounded$upper[1])-cdf(rounded$lower[1])-d$population_content[1]),1e-12)
  path <- file.path(directory,"precise.csv");testthat::expect_true(mec_write_intervals(d,path))
  restored <- data.table::fread(path)
  for(name in names(d)[vapply(d,is.double,FALSE)])testthat::expect_identical(restored[[name]],d[[name]])
  testthat::expect_identical(cdf(restored$upper)-cdf(restored$lower),d$population_content)
  testthat::expect_identical(d$method,c("gamma","normal"))
})

nominal_finalization_fixture <- function() {
  parent <- tempfile();directory <- file.path(parent,"original")
  dir.create(file.path(directory,"validation/fits"),recursive=TRUE)
  cfg <- nominal_config();cfg$output_root <- directory;cfg$expected_cells <- 1;cfg$validation_replications <- 2
  commit <- mgfp_git("rev-parse","HEAD")
  tasks <- data.frame(job_id=1:2,n=100,content=.9,response_id="normal",data_seed=c(71L,72L))
  policy <- data.table::data.table(n=100,content=.9,method="ecm_primary",available=TRUE,gamma=.995,reason="")
  programs <- mgfp_hashes("application/config/mti_ecm_nominal_20261119.json")
  mgfp_json_write(list(identity=list(source_commit=commit,configuration=cfg,programs=programs,article_files=programs)),
    file.path(directory,"manifest.json"))
  mec_freeze(policy,tasks,directory,commit)
  mwd_write(policy,file.path(directory,"frozen_selection.csv"))
  mgfp_json_write(list(analysis_id="fixture"),file.path(directory,"validation/manifest.json"))
  mgfp_json_write(list(completed_datasets=2,expected_datasets=2,remaining_datasets=0,numerical_pass=TRUE),
    file.path(directory,"validation/progress.json"))
  mgfp_json_write(list(exit_code=1),file.path(directory,"controller_exit.json"))
  mgfp_json_write(list(status="stopped_requires_review"),file.path(directory,"study_summary.json"))
  rows <- lapply(seq_len(nrow(tasks)),function(i) {
    task <- tasks[i,,drop=FALSE];set.seed(task$data_seed);y <- runif(task$n)
    d <- data.table::data.table(job_id=task$job_id,n=100,content=.9,tolerance_confidence=.95,
      response_id="normal",method=c("ecm_primary","young_mathew","wilks"),lower=0,
      upper=if(i==1).92 else .98,width=if(i==1).92 else .98,
      population_content=if(i==1).92 else .98,returned=TRUE,attained=TRUE,numerically_accurate=TRUE)
    mbc_atomic_fit(list(ok=TRUE,unresolved_candidates=0L,sample_sha256=mwd_hash(y),
      prior=list(prior_sd=2,data_adaptive_prior=FALSE),candidates=data.frame(numerically_accurate=TRUE),rows=d),
      file.path(directory,"validation/fits",sprintf("fit_%06d.rds",task$job_id)),"fixture",task)
    d
  })
  rows <- data.table::rbindlist(rows);mwd_write(rows,file.path(directory,"validation/interval_results.csv"))
  mec_report(rows,cfg)
  list(parent=parent,study=list(checkout=normalizePath("."),output_root=directory,source_commit=commit),
    programs=programs,context=list(laws=list(normal=list(r=runif,p=punif))))
}

testthat::test_that("separate finalization preserves sampling history and resumes without refitting", {
  withr::local_dir(root)
  f <- nominal_finalization_fixture();on.exit(unlink(f$parent,recursive=TRUE))
  exit_path <- file.path(f$study$output_root,"controller_exit.json");before <- mwd_file_hash(exit_path)
  result <- mec_finalize(f$study,f$context,f$programs)
  record <- mwd_json(file.path(result$output_root,"finalization.json"))
  testthat::expect_equal(record$datasets,2L);testthat::expect_equal(record$interval_rows,6L)
  testthat::expect_equal(record$refitted_datasets,0L)
  testthat::expect_equal(record$changed_attainment_events,0L)
  testthat::expect_identical(mwd_file_hash(exit_path),before)
  testthat::expect_identical(mwd_json(exit_path)$exit_code,1L)
  testthat::expect_true(mec_verify_finalization(result$output_root,f$context))
  testthat::expect_identical(mec_finalize(f$study,f$context,f$programs),result)
  record_path <- file.path(result$output_root,"finalization.json")
  damaged <- record;damaged$identity$article_replaced <- TRUE
  mgfp_json_write(damaged,record_path)
  testthat::expect_error(mec_verify_finalization(result$output_root,f$context),"identity changed")
  mgfp_json_write(record,record_path)
  hash_path <- file.path(f$study$output_root,"frozen_selection.rds")
  changed <- readRDS(hash_path);changed$gamma <- .9;saveRDS(changed,hash_path)
  testthat::expect_error(mec_verify_finalization(result$output_root,f$context),"hash differs")
})

testthat::test_that("finalization refuses missing work, wrong samples, damaged receipts and changed outcomes", {
  withr::local_dir(root)
  f <- nominal_finalization_fixture();on.exit(unlink(f$parent,recursive=TRUE))
  wrong <- f$study;wrong$source_commit <- "changed"
  testthat::expect_error(mec_sampling_source(wrong),"commit changed")
  progress <- file.path(f$study$output_root,"validation/progress.json")
  mgfp_json_write(list(completed_datasets=1,expected_datasets=2,remaining_datasets=1,numerical_pass=TRUE),progress)
  testthat::expect_error(mec_sampling_source(f$study),"Sampling is incomplete")
  mgfp_json_write(list(completed_datasets=2,expected_datasets=2,remaining_datasets=0,numerical_pass=TRUE),progress)
  source <- mec_sampling_source(f$study)
  path <- file.path(f$study$output_root,"validation/fits/fit_000001.rds")
  old <- readRDS(path);bad <- old;bad$sample_sha256 <- "changed"
  task <- readRDS(file.path(f$study$output_root,"validation_tasks.rds"))[1,,drop=FALSE]
  mbc_atomic_fit(bad,path,"fixture",task)
  testthat::expect_error(mec_finalization_rows(source,f$context,1L),"saved sample")
  saveRDS(old,path)
  testthat::expect_error(mec_finalization_rows(source,f$context,1L),"checksum differs")
  rows <- data.table::fread(file.path(f$study$output_root,"validation/interval_results.csv"))
  rows$attained[1] <- FALSE
  testthat::expect_error(mec_same_outcomes(rows,file.path(f$study$output_root,"validation/interval_results.csv")),
    "change a recorded")
})

testthat::test_that("generator loading uses disposable scratch instead of original output", {
  parent <- tempfile();dir.create(parent);on.exit(unlink(parent,recursive=TRUE))
  original <- file.path(parent,"original");previous <- file.path(parent,"previous")
  dir.create(original);dir.create(previous)
  mgfp_json_write(list(identity=list(source=list(output_root=previous,dependency=list()))),
    file.path(original,"manifest.json"))
  mgfp_json_write(list(identity=list(configuration=list(output_root=original))),file.path(previous,"manifest.json"))
  context <- mec_finalization_context;environment(context) <- new.env(parent=environment(context))
  environment(context)$mjs_context <- function(cfg,dependency) {
    stopifnot(cfg$output_root!=original)
    saveRDS(list(generated=TRUE),file.path(cfg$output_root,"frozen_data_generators.R"))
    list(scratch=cfg$output_root)
  }
  result <- context(list(output_root=original))
  testthat::expect_false(dir.exists(result$scratch))
  testthat::expect_false(file.exists(file.path(original,"frozen_data_generators.R")))
})
