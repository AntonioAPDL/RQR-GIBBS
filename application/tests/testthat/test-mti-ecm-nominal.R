root <- normalizePath(if(file.exists("application/scripts/lib/mti_ecm_nominal.R"))"." else "../../..")
for(name in c("mti_gibbs_practical","mti_gibbs_full_report_review","mti_gibbs_full_publication",
  "mti_gibbs_width_diagnosis","mti_fixed_prior","mti_gibbs_boundary_calibration",
  "mti_gibbs_decision_calibration","mti_gibbs_joint","mti_ecm_nominal"))
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
  policy <- data.table::data.table(n=100,content=.9,method="ecm_primary",available=TRUE,gamma=.995)
  saveRDS(tasks,file.path(directory,"validation_tasks.rds"));saveRDS(policy,file.path(directory,"frozen_selection.rds"))
  source <- file.path(root,"application/config/mti_ecm_nominal_20261119.json")
  mgfp_json_write(list(identity=list(programs=mgfp_hashes(source),article_files=mgfp_hashes(source))),
    file.path(directory,"manifest.json"))
  mgfp_json_write(list(frozen_before_fresh_data=TRUE,development_only_selection=TRUE,
    automatic_article_replacement=FALSE,selection_sha256=mwd_file_hash(file.path(directory,"frozen_selection.rds")),
    tasks_sha256=mwd_file_hash(file.path(directory,"validation_tasks.rds"))),file.path(directory,"frozen_policy.json"))
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
})
