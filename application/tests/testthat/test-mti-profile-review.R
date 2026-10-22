review_env <- function() {
  e <- new.env(parent=globalenv())
  for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R")) {
    sys.source(testthat::test_path("..","..","scripts","lib",file),e)
  }
  e
}

review_fixture <- function(replications=3L) {
  cfg <- list(calibration=list(replications=1000L,threshold_development_replications=2L,
    threshold_quantile_count=3L,familywise_confidence=.95,seed_base=100L,cell_seed_stride=10000L),
    validation=list(replications=replications,seed_base=100000L,cell_seed_stride=10000L,
      methods=list("mti_profile_fractional_admissible_v1","wilks")),
    profile=list(objective_tolerance=1e-8))
  design <- data.frame(cell_index=1L,n=50,content=.9,tolerance_confidence=.95,dgp_id="test")
  tasks <- design[rep(1,replications),]
  tasks$replication <- seq_len(replications)
  tasks$role <- "independent_validation"
  tasks$seed <- cfg$validation$seed_base+cfg$validation$cell_seed_stride+tasks$replication
  rows <- do.call(rbind,lapply(unlist(cfg$validation$methods),function(method)
    data.frame(tasks,method=method,returned=TRUE,status="returned",lower=0,upper=1,
      width=1,true_content=1,content_attained=TRUE,numerical_failure_candidates=0L,
      relative_objective_gap=if(method=="wilks")NA_real_ else 0)))
  list(cfg=cfg,design=design,rows=rows,review=list(primary_method=
    "mti_profile_fractional_admissible_v1",historical_reference_threshold=.995))
}

test_that("review binomial bounds agree with the exact binomial test", {
  e <- review_env()
  for(k in c(0,3,10)) {
    b <- e$mpr_binomial_bounds(k,10,.025)
    ref <- stats::binom.test(k,10,conf.level=.95)$conf.int
    expect_equal(as.numeric(b),as.numeric(ref),tolerance=1e-12)
  }
})

test_that("review separates fitted datasets from completed statistical assessment", {
  e <- review_env(); path <- tempfile("review_states_")
  dir.create(file.path(path,"calibration"),recursive=TRUE)
  on.exit(unlink(path,recursive=TRUE),add=TRUE)
  expect_equal(e$mpr_stage_state(path,"calibration",10,10,TRUE)$status,"assessment_running")
  expect_equal(e$mpr_stage_state(path,"calibration",10,10,FALSE)$status,"assessment_incomplete")
  expect_equal(e$mpr_stage_state(path,"calibration",2,10,FALSE)$remaining_fits,8)
  jsonlite::write_json(list(passed=FALSE),file.path(path,"calibration/completion.json"),auto_unbox=TRUE)
  expect_equal(e$mpr_stage_state(path,"calibration",10,10,FALSE)$status,"calibration_unresolved")
  expect_error(e$mpr_stage_state(path,"calibration",2,10,FALSE),"incomplete")
})

test_that("independent selection reproduces every threshold and rejects partial failures", {
  e <- review_env();set.seed(71)
  z <- data.frame(candidate_index=1:30,target_content=rep(c(.97,.98,.99),10),
    mean_tilt=rep(c(-.1,0,.1),10),central_tilt=0,width=sample(1:10,30,replace=TRUE),
    lower=0,profile_converged=TRUE,posterior_content_probability=runif(30),true_content=runif(30))
  thresholds <- c(.1,.5,.95,.9999)
  answer <- e$mpr_candidate_decisions(z,thresholds,.9)
  for(i in seq_along(thresholds)) {
    selected <- e$mp_select(z,thresholds[i])
    expect_equal(answer$returned[i],as.integer(nrow(selected)>0))
    if(nrow(selected)) {
      expect_equal(answer$width_sum[i],selected$width)
      expect_equal(answer$successes[i],as.integer(selected$true_content>=.9))
    }
  }
  z$profile_converged[2] <- FALSE
  expect_equal(e$mpr_candidate_decisions(z,thresholds,.9)$returned,rep(0L,4))
})

test_that("validation review requires complete paired samples and correct scores", {
  e <- review_env();f <- review_fixture()
  expect_silent(e$mpr_validate_results(f$rows,f$design,f$cfg))
  expect_error(e$mpr_validate_results(rbind(f$rows,f$rows[1,]),f$design,f$cfg),"Duplicate")
  expect_error(e$mpr_validate_results(f$rows[-1,],f$design,f$cfg),"incomplete")
  bad <- f$rows;bad$seed[1] <- bad$seed[1]+1
  expect_error(e$mpr_validate_results(bad,f$design,f$cfg),"seed")
  bad <- f$rows;bad$width[1] <- .9
  expect_error(e$mpr_validate_results(bad,f$design,f$cfg),"widths disagree")
  bad <- f$rows;bad$content_attained[1] <- FALSE
  expect_error(e$mpr_validate_results(bad,f$design,f$cfg),"wrong event")
  bad <- f$rows;bad$relative_objective_gap[1] <- .001
  expect_error(e$mpr_validate_results(bad,f$design,f$cfg),"numerical")
  expect_silent(e$mpr_validate_results(f$rows,f$design,f$cfg,list(test=stats::punif)))
  bad <- f$rows;bad$true_content[1] <- .99
  expect_error(e$mpr_validate_results(bad,f$design,f$cfg,list(test=stats::punif)),"frozen DGP")
})

test_that("nonreturns stay in attainment while width pairing is explicit", {
  e <- review_env();f <- review_fixture();z <- f$rows
  z$true_content <- c(.9,.8,NA,.9,.9,.8)
  z$returned[3] <- FALSE;z$status[3] <- "no_eligible_interval"
  z$upper <- z$width <- c(1,2,NA,2,2,2)
  z$content_attained <- c(TRUE,FALSE,FALSE,TRUE,TRUE,FALSE)
  expect_silent(e$mpr_validate_results(z,f$design,f$cfg))
  summary <- e$mpr_validation_summary(z,f$design,f$cfg,f$review)
  primary <- summary[summary$method==f$review$primary_method,]
  expect_equal(primary$attainment,1/3)
  expect_equal(primary$returned,2)
  expect_equal(primary$width_mean,1.5)
  expect_equal(primary$evidence,"nonreturns_observed")
  paired <- e$mpr_paired_summary(z,f$review)
  expect_equal(paired$jointly_returned,2)
  expect_equal(paired$attainment_change,-1/3)
  expect_equal(paired$width_change_mean,-.5)
  expect_equal(paired$comparator_only_successes,1)
  shuffled <- e$mpr_paired_summary(z[c(3,1,5,2,6,4),],f$review)
  expect_equal(shuffled$width_change_mean,paired$width_change_mean)
  expect_equal(shuffled$attainment_change,paired$attainment_change)
})

test_that("simultaneous evidence is distinct from inconclusive or shortfall evidence", {
  e <- review_env();f <- review_fixture(1000L)
  summary <- e$mpr_validation_summary(f$rows,f$design,f$cfg,f$review)
  expect_equal(summary$evidence[summary$method==f$review$primary_method],"strong_panel_evidence")
  f$rows$content_attained[1:50] <- FALSE
  summary <- e$mpr_validation_summary(f$rows,f$design,f$cfg,f$review)
  expect_equal(summary$evidence[summary$method==f$review$primary_method],"inconclusive")
  f$rows$content_attained[1:200] <- FALSE
  summary <- e$mpr_validation_summary(f$rows,f$design,f$cfg,f$review)
  expect_equal(summary$evidence[summary$method==f$review$primary_method],"evidence_of_shortfall")
})

test_that("threshold reconstruction and binomial assessments remain independently checked", {
  e <- review_env();f <- review_fixture();directory <- tempfile("review_calibration_")
  dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  candidate <- function(probability) data.frame(posterior_content_probability=probability,
    profile_converged=TRUE,width=1,target_content=.98,mean_tilt=0,central_tilt=0,
    lower=0,candidate_index=1L,true_content=.99)
  values <- lapply(1:1002,function(i)list(identity=data.frame(n=50,content=.9,
    tolerance_confidence=.95,dgp_id="test",role=if(i<=2)"threshold_development" else
      "calibration_assessment"),candidates=candidate(if(i<=2).97 else .999)))
  policy <- e$mp_calibration_policy(values,f$cfg,directory)
  assessment <- read.csv(file.path(directory,"threshold_assessment.csv"))
  development <- setNames(list(c(.97,.97)),e$mpr_cell_key(f$design))
  result <- e$mpr_calibration_check(assessment,policy,f$design,f$cfg,f$review,development)
  expect_equal(result$interpretation,"passed")
  counts <- assessment[c("n","content","tolerance_confidence","dgp_id","threshold",
    "replications","returned","successes")]
  counts$width_sum <- assessment$mean_width*counts$returned
  expect_silent(e$mpr_calibration_check(assessment,policy,f$design,f$cfg,f$review,development,counts))
  counts$successes[1] <- counts$successes[1]-1
  expect_error(e$mpr_calibration_check(assessment,policy,f$design,f$cfg,f$review,development,counts),
    "independently selected")
  bad <- assessment;bad$lower_bound[1] <- .1
  expect_error(e$mpr_calibration_check(bad,policy,f$design,f$cfg,f$review,development),"bounds")
  expect_error(e$mpr_calibration_check(assessment,policy,f$design,f$cfg,f$review,
    setNames(list(.91),names(development))),"grid")
  policy$threshold <- .995
  expect_error(e$mpr_calibration_check(assessment,policy,f$design,f$cfg,f$review,development),"least passing")
})

test_that("checkpoint audit detects duplicates and validates stored candidate mass diagnostics", {
  e <- review_env();f <- review_fixture();directory <- tempfile("review_checkpoint_")
  dir.create(file.path(directory,"calibration/checkpoints"),recursive=TRUE)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  tasks <- f$design[rep(1,3),]
  tasks$replication <- 1:3;tasks$role <- c(rep("threshold_development",2),"calibration_assessment")
  tasks$seed <- f$cfg$calibration$seed_base+f$cfg$calibration$cell_seed_stride+1:3
  candidate <- data.frame(profile_converged=TRUE,relative_objective_gap=0,
    lower=0,upper=1,width=1,posterior_content_probability=.99,true_content=1,
    target_content=.98,mean_tilt=0,central_tilt=0,candidate_index=1L)
  saved <- list(tasks=tasks,values=lapply(1:3,function(i)
    list(identity=tasks[i,,drop=FALSE],candidates=candidate)))
  saveRDS(saved,file.path(directory,"calibration/checkpoints/batch_00001.rds"))
  assessment <- data.frame(f$design[rep(1,2),],threshold=c(.95,.99))
  expect_warning(out <- e$mpr_checkpoint_audit(directory,"calibration",f$design,f$cfg,assessment),NA)
  expect_equal(nrow(out),3)
  expect_equal(sum(out$numerical_failed_candidates),0)
  expect_equal(attr(out,"independent_threshold_counts")$successes,c(1,1))
  saveRDS(saved,file.path(directory,"calibration/checkpoints/batch_00002.rds"))
  expect_error(e$mpr_checkpoint_audit(directory,"calibration",f$design,f$cfg),"repeated")
})

test_that("precision reference uses exact sample-range content and binomial power", {
  e <- review_env();f <- review_fixture()
  z <- e$mpr_precision_reference(data.frame(f$design,interpretation="inconclusive"),f$design,f$cfg)
  expect_equal(unique(z$sample_range_reference_attainment),1-50*.9^49+49*.9^50,tolerance=1e-12)
  for(i in seq_len(nrow(z))) {
    s <- z[i,]
    if(s$required_successes<=s$replications) {
      expect_gte(e$mpr_binomial_bounds(s$required_successes,s$replications,s$alpha)["lower"],.95)
      expect_lt(e$mpr_binomial_bounds(s$required_successes-1,s$replications,s$alpha)["lower"],.95)
    }
  }
  expect_true(all(z$reference_only))
})

test_that("diagnostic figures render without warnings and output paths resolve symlinks", {
  e <- review_env();f <- review_fixture();directory <- tempfile("review_pdf_")
  dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  summary <- e$mpr_validation_summary(f$rows,f$design,f$cfg,f$review)
  expect_warning(e$mpr_report_pdf(directory,list(decision="fixture"),summary=summary),NA)
  expect_gt(file.info(file.path(directory,"mti_profile_assessment.pdf"))$size,1000)
  link <- file.path(directory,"linked")
  expect_true(file.symlink(tempdir(),link))
  expect_equal(e$mpr_output_path(file.path(link,"new")),file.path(normalizePath(tempdir()),"new"))
})
