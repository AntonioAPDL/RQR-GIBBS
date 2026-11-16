root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_decision_calibration.R")) "." else "../../..")
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "mti_gibbs_practical", "mti_prior_screen", "pharma_tolerance_application", "mti_gibbs_boundary_calibration",
  "mti_gibbs_boundary_study", "mti_gibbs_decision_calibration", "mti_gibbs_decision_study"))
  source(file.path(root, "application/scripts/lib", paste0(name, ".R")))

mdc_fixture <- function() data.frame(candidate_index = 1:6, target_content = c(.92,.93,.94,.95,.96,.97),
  mean_tilt = 0, central_tilt = 0, lower = -c(1,1,1.5,2,3,4), upper = c(1,1,1.5,2,3,4),
  width = c(2,2,3,4,6,8), finite_interval = TRUE, numerically_accurate = TRUE,
  posterior_content_probability = c(.7,.7,.6,.8,.95,.99))

testthat::test_that("the selection path reproduces brute-force choices including exact ties", {
  z <- mdc_fixture()
  testthat::expect_equal(mdc_path(z)$candidate_index, c(1L,4L,5L,6L))
  for (p in c(.1,.7,.7+1e-12,.8,.9,.95,.96,.99,.999)) {
    d <- z[z$posterior_content_probability >= p, ]
    d <- d[order(d$width,d$target_content,abs(d$mean_tilt-d$central_tilt),d$lower,d$candidate_index), ]
    testthat::expect_equal(mdc_select(z,p)$candidate_index, head(d$candidate_index,1))
  }
  z$numerically_accurate[1:2] <- FALSE
  testthat::expect_equal(mdc_select(z,.7)$candidate_index,4L)
  z$finite_interval <- FALSE
  testthat::expect_equal(nrow(mdc_select(z,.7)),0L)
  testthat::expect_error(mdc_select(z,1),"strictly between")
  z$candidate_index[2] <- 1L
  testthat::expect_error(mdc_path(z),"Invalid candidate")
})

testthat::test_that("event accumulation agrees with direct selection at every breakpoint", {
  path <- mdc_path(mdc_fixture())
  a <- path; a$response_id <- "a"; a$population_content <- c(.8,.91,.94,.99)
  b <- path; b$response_id <- "b"; b$population_content <- c(.85,.88,.98,.999)
  b$width <- b$width*2
  curve <- mdc_curve(list(a,b),.99,.9,.95)
  for (i in seq_len(nrow(curve))) {
    chosen <- lapply(list(a,b), function(z) z[which(z$posterior_content_probability >= curve$gamma[i])[1], ])
    ratios <- vapply(seq_along(chosen), function(j) chosen[[j]]$width / list(a,b)[[j]]$width[4], 0)
    testthat::expect_equal(curve$worst_log_ratio[i],max(log(ratios)))
    testthat::expect_equal(curve$mean_log_ratio[i],mean(log(ratios)))
    testthat::expect_equal(curve$minimum_attainment[i],min(vapply(chosen,function(z) as.numeric(z$population_content >= .9),0)))
  }
  choice <- mdc_shortlist(curve,.99)
  testthat::expect_lte(nrow(choice),3L)
  testthat::expect_true(all(choice$gamma %in% curve$gamma))
  testthat::expect_equal(mdc_shortlist(curve[1, ],curve$gamma[1])$rule,"baseline")
  a$population_content<-c(.95,.85,.99,.9)
  other<-mdc_curve(list(a),.99,.9,.95)
  testthat::expect_equal(other$minimum_attainment,c(1,0,1,1))
})

testthat::test_that("rescaled decisions recompute content and posterior probabilities", {
  report<-mdc_final_rows; environment(report)<-new.env(parent=environment(mdc_final_rows))
  seen<-list()
  environment(report)$mdc_dp<-function(y,anchor,lower,upper,content) {
    seen[[length(seen)+1L]]<<-data.frame(lower,upper)
    pnorm(upper)-pnorm(lower)
  }
  task<-data.frame(job_id=1L,n=100L,content=.9,response_id="normal",data_seed=17L)
  base<-data.table::data.table(rule=c("baseline","proposed"),center=0,halfwidth=1,
    width=2,lower=-1,upper=1,returned=TRUE,population_content=pnorm(1)-pnorm(-1))
  value<-list(rows=base,anchor=list(selected=data.frame(lower=-1.5,upper=1.5,width=3)),
    comparators=data.frame(method="wilks",returned=TRUE,lower=-2,upper=2,width=4))
  context<-list(laws=list(normal=list(r=rnorm,p=pnorm)),
    old_radius=data.table::data.table(n=100L,content=.9,multiplier=1.2))
  calibration<-data.table::data.table(n=100L,content=.9,rule=c("baseline","proposed"),multiplier=c(2,1.8))
  frozen<-data.frame(n=100L,content=.9,rule=c("baseline","proposed"),advance=c(TRUE,TRUE))
  result<-report(value,task,calibration,frozen,list(),context)
  testthat::expect_equal(result[method=="gibbs_proposed",width],3.6)
  testthat::expect_equal(result[method=="gibbs_proposed",dp_probability],pnorm(1.8)-pnorm(-1.8))
  testthat::expect_equal(unname(unlist(seen[[1]][2, ])),c(-1.8,1.8))
  frozen$advance[2]<-FALSE
  result<-report(value,task,calibration,frozen,list(),context)
  testthat::expect_equal(result[method=="gibbs_proposed",width],3.6)
  testthat::expect_equal(result[method=="gibbs_original_radius",width],2.4)
  testthat::expect_false(result[method=="gibbs_original_radius",attained])
  testthat::expect_false(result[method=="gibbs_proposed",exploratory_advancement])
})

testthat::test_that("calibration uses the declared simultaneous allocation and retains failures", {
  testthat::expect_equal(mbc_rank(1000,.95,.05,72)$rank,972L)
  testthat::expect_equal(mbc_rank(1000,.95,.05,144)$rank,973L)
  cfg <- list(expected_cells=1L, stages=list(calibration=list(replications=1000L)),
    calibration_error=.05, protected_comparisons=144L)
  rows <- data.table::CJ(n=100L,content=.9,response_id=paste0("law",1:8),
    rule=c("baseline","proposed"), replication=1:1000)
  rows[, `:=`(adequate=TRUE,tolerance_confidence=.95,required_scale=replication/1000)]
  cfg$expected_cells <- 8L
  z <- mdc_calibrate(rows,cfg)
  testthat::expect_true(all(z$policy$rank==973L & z$policy$multiplier==.973))
  rows[replication >= 973 & response_id=="law1", required_scale:=Inf]
  z <- mdc_calibrate(rows,cfg)
  testthat::expect_true(all(is.infinite(z$policy$multiplier)))
  testthat::expect_error(mdc_calibrate(rows[-1],cfg),"incomplete")
  rows$adequate[1] <- FALSE
  testthat::expect_error(mdc_calibrate(rows,cfg),"unresolved")
})

testthat::test_that("the materiality gate is design-specific and prefers unchanged targets", {
  cfg <- mwd_json(file.path(root,"application/config/mti_gibbs_decision_calibration_20261116.json"))
  rows <- data.table::CJ(n=100L,content=.9,response_id=c("normal","student_t3",cfg$asymmetric_laws),
    rule=c("baseline","alternative1"),replication=1:100)
  rows[, `:=`(tolerance_confidence=.95,adequate=TRUE,returned=TRUE,required_scale=.9,
    width=ifelse(rule=="baseline",2,1.98), gamma=ifelse(rule=="baseline",.995,.8))]
  old <- data.table::data.table(n=100L,content=.9,multiplier=1)
  z <- mdc_gate(rows,old,cfg)
  testthat::expect_true(z$policy$advance)
  testthat::expect_equal(z$policy$rule,"baseline")
  rows[, required_scale:=1]
  z <- mdc_gate(rows,old,cfg)
  testthat::expect_false(z$policy$advance)
  testthat::expect_equal(z$policy$rule,"baseline")
  rows[rule=="alternative1", `:=`(width=1.7,required_scale=1)]
  z <- mdc_gate(rows,old,cfg)
  testthat::expect_true(z$policy$advance)
  testthat::expect_equal(z$policy$rule,"alternative1")
  rows[rule=="alternative1" & response_id=="normal",width:=3]
  testthat::expect_false(mdc_gate(rows,old,cfg)$policy$advance)
})

testthat::test_that("target identities distinguish observations, tilt, prior and sampling", {
  y <- seq(-1,1,length.out=100); a<-list(q=.95); g<-list(delta=0)
  cfg<-list(prior_sd=2,learning_rate=1,sampling=list(n_mcmc=1000))
  key<-mdc_target_key(y,a,g,cfg)
  testthat::expect_identical(key,mdc_target_key(y,a,g,cfg))
  testthat::expect_false(identical(key,mdc_target_key(y+.1,a,g,cfg)))
  testthat::expect_false(identical(key,mdc_target_key(y,a,list(delta=.1),cfg)))
  cfg$prior_sd<-1
  testthat::expect_false(identical(key,mdc_target_key(y,a,g,cfg)))
  cfg$prior_sd<-2; cfg$sampling$n_mcmc<-2000
  testthat::expect_false(identical(key,mdc_target_key(y,a,g,cfg)))
})

testthat::test_that("identical alternate targets are sampled once and baseline fits are reused", {
  directory<-tempfile();dir.create(directory);on.exit(unlink(directory,recursive=TRUE))
  mgfp_json_write(list(identity=list(source_commit="original")),file.path(directory,"manifest.json"))
  y<-seq(-1,1,length.out=100); candidates<-mdc_fixture()
  selected<-mdc_select(candidates,.95)
  anchor<-list(q=selected$target_content,delta_reference=0,selected=selected,candidates=candidates,
    sample_sha256=mwd_hash(y))
  parameters<-data.frame(variable=c("midpoint","width"),mean=c(0,2),preferred_pass=TRUE,mean_precision=TRUE,
    rhat=1,ess_bulk=1000,ess_tail=1000,mcse_mean=.01,mcse_sd_ratio=.01)
  record<-list(adequate=TRUE,parameters=parameters,fits=list(list(draws=cbind(rep(-1,10),rep(1,10)))))
  old<-list(anchor=anchor,rows=data.frame(delta=0),records=list(original=record),comparators=data.frame())
  job<-mdc_job;environment(job)<-new.env(parent=environment(mdc_job))
  environment(job)$mdc_old<-function(...) old
  environment(job)$mdc_dp<-function(...) .9
  calls<-0L
  environment(job)$mbc_sample_target<-function(...) {calls<<-calls+1L;record}
  task<-data.frame(job_id=1L,n=100L,content=.9,tolerance_confidence=.95,response_id="normal",
    data_seed=17L,replication=1L,chain_base1=100L,chain_base2=200L,chain_base3=300L)
  cfg<-list(prior_sd=2,learning_rate=1,sampling=list(chains=4))
  context<-list(boundary=directory,laws=list(normal=list(r=function(n) y,p=pnorm)),
    old_tilts=data.frame(n=100L,content=.9,variant="original"))
  rules<-data.frame(n=100L,content=.9,rule=c("baseline","alternative1","alternative2"),gamma=c(.95,.65,.7))
  value<-job(task,"shortlist",rules,cfg,context)
  testthat::expect_true(value$ok)
  testthat::expect_equal(value$new_targets,1L)
  testthat::expect_equal(calls,1L)
  testthat::expect_length(value$records,2L)
  testthat::expect_equal(nrow(value$rows),3L)
})

testthat::test_that("compact evidence reproduces summaries without the fit directory", {
  directory<-tempfile();dir.create(directory);on.exit(unlink(directory,recursive=TRUE))
  dir.create(file.path(directory,"validation"))
  rows<-data.table::data.table(n=100L,content=.9,tolerance_confidence=.95,response_id="normal",
    method="gibbs_proposed",returned=TRUE,attained=c(TRUE,FALSE,TRUE),width=c(2,3,4),
    population_content=c(.91,.89,.93),dp_probability=c(.9,.91,.92))
  mwd_write(rows,file.path(directory,"validation/interval_results.csv"))
  mwd_write(mdc_summarize(rows),file.path(directory,"validation/comparison_summary.csv"))
  mdc_preserve_evidence(list(output_root=directory),list(source_commit="source"))
  invisible(capture.output(testthat::expect_true(mdc_verify_evidence(directory))))
  rows$width[1]<-9
  mwd_write(rows,file.path(directory,"validation/interval_results.csv"))
  testthat::expect_error(mdc_verify_evidence(directory),"Evidence changed")
})

testthat::test_that("resumption preserves complete results and rejects changes", {
  directory<-tempfile(); dir.create(directory); on.exit(unlink(directory,recursive=TRUE))
  calls<-0L; queue<-mdc_queue; environment(queue)<-new.env(parent=environment(mdc_queue))
  environment(queue)$mdc_job<-function(task,...) {
    calls<<-calls+1L
    list(ok=TRUE,rows=data.frame(job_id=task$job_id,adequate=TRUE),new_targets=1L)
  }
  tasks<-data.frame(job_id=1:2,n=100L,content=.9)
  cfg<-list(output_root=directory,disk_reserve_gib=0)
  choice<-data.frame(n=100L,content=.9,rule="baseline",gamma=.99)
  first<-queue(tasks,"shortlist",choice,cfg,list(),"source",1L)
  again<-queue(tasks,"shortlist",choice,cfg,list(),"source",1L)
  testthat::expect_equal(calls,2L)
  testthat::expect_equal(first$rows,again$rows)
  testthat::expect_error(queue(tasks,"shortlist",choice,cfg,list(),"changed",1L),"identity changed")
  choice$gamma<-.9
  testthat::expect_error(queue(tasks,"shortlist",choice,cfg,list(),"source",1L),"identity changed")
  unlink(file.path(directory,"shortlist/fits/fit_000001.rds.json"))
  choice$gamma<-.99
  testthat::expect_error(queue(tasks,"shortlist",choice,cfg,list(),"source",1L),"lacks a receipt")
})

testthat::test_that("data and chain seeds are independent across new stages", {
  cfg<-mwd_json(file.path(root,"application/config/mti_gibbs_decision_calibration_20261116.json"))
  context<-list(scan=read.csv(file.path(root,"tables/tolerance_validation_article_scan_calibration.csv")),
    laws=setNames(rep(list(NULL),8),c("normal","student_t3",cfg$asymmetric_laws)))
  context$old_tasks<-mps_design(list(expected_cells=72,replications_per_cell=200,
    data_seed_base=1850000000,kappa_grid=2),context)
  tasks<-mdc_designs(cfg,context)
  testthat::expect_equal(vapply(tasks,nrow,1L),c(calibration=72000L,validation=72000L,shortlist=7200L))
  testthat::expect_length(intersect(tasks$calibration$data_seed,tasks$validation$data_seed),0L)
  testthat::expect_length(intersect(tasks$shortlist$data_seed,tasks$validation$data_seed),0L)
  testthat::expect_equal(tasks$calibration$n[1:8],rep(500L,8))
  testthat::expect_equal(tasks$calibration$content[1:8],rep(.99,8))
  testthat::expect_equal(mdc_seed_blocks(1000,1:3,numeric()),1:3)
  testthat::expect_equal(mdc_seed_blocks(1000,1:3,1101:1199),c(80001L,2L,3L))
  testthat::expect_equal(mdc_seed_blocks(1000,1:3,c(1101:1199,8001101)),c(80002L,2L,3L))
  frame<-data.frame(n=100L,gamma=.99)
  key<-mdc_stage_id("source","stage",frame)
  frame$gamma<-.9; frame$gamma<-.99
  testthat::expect_identical(key,mdc_stage_id("source","stage",frame))
})
