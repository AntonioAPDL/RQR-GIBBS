cs_root<-normalizePath(if(file.exists("application/scripts/lib/mti_ecm_common_scale_study.R"))"." else "../../..")
for(name in c("mti_gibbs_width_diagnosis","mti_ecm_common_scale_study"))
  source(file.path(cs_root,"application/scripts/lib",paste0(name,".R")))
cs_config<-function()mwd_json(file.path(cs_root,"application/config/mti_ecm_common_scale_20261119.json"))

testthat::test_that("common-scale screening excludes prior leakage and automatic promotion", {
  cfg<-cs_config();testthat::expect_true(msc_validate(cfg))
  for(name in c("sample_gibbs","application_transfer","automatic_article_replacement")) {
    bad<-cfg;bad[[name]]<-TRUE;testthat::expect_error(msc_validate(bad),"specification")
  }
  bad<-cfg;bad$coordinates$estimated_from_analysis_sample<-TRUE
  testthat::expect_error(msc_validate(bad),"specification")
  bad<-cfg;bad$scale_ranges$primary<-c(0,Inf)
  testthat::expect_error(msc_validate(bad),"proper finite")
  testthat::expect_equal(log(cfg$scale_ranges$primary),log(c(.25,4)))
  testthat::expect_setequal(names(formals(msc_fit)),c("y","task","cfg","context","bounds"))
})

testthat::test_that("a boundary majority plus range dependence blocks a large run", {
  cfg<-cs_config();n<-100L
  a<-data.frame(method="primary",job_id=seq_len(n),returned=TRUE,
    at_upper_bound=TRUE,lower=-1,upper=1,width=2)
  b<-a;b$method<-"broad";b$lower<--1.1
  result<-msc_bound_review(rbind(a,b),cfg)
  testthat::expect_true(result$stop_bound_sensitive)
  testthat::expect_equal(result$primary_upper_bound,n)
  testthat::expect_gt(result$panel_lower_bound,.5)
  testthat::expect_equal(result$panel_lower_bound,1-sqrt(log(1/cfg$boundary_error)/(2*n)))
  testthat::expect_match(result$bound_method,"heterogeneous")
  b$lower<--1
  testthat::expect_false(msc_bound_review(rbind(a,b),cfg)$stop_bound_sensitive)
  a$at_upper_bound<-rep(c(TRUE,FALSE),50)
  b$lower<--1.1
  testthat::expect_false(msc_bound_review(rbind(a,b),cfg)$stop_bound_sensitive)
  a$returned<-FALSE
  testthat::expect_false(msc_bound_review(rbind(a,b),cfg)$majority_bound)
})

testthat::test_that("missing eligibility is recorded without a substitute interval", {
  task<-data.frame(job_id=1,response_id="not_given_to_fitter",n=50,content=.9,
    tolerance_confidence=.95,replication=1,data_seed=9)
  row<-msc_row(data.frame(),"primary",task,.99)
  testthat::expect_false(row$returned)
  testthat::expect_true(is.na(row$lower) && is.na(row$upper) && is.na(row$width))
  candidate<-data.frame(lower=-1,upper=2,width=3,target_content=.97,mean_tilt=.1,
    candidate_index=4,learning_rate=2,scale_at_upper_bound=FALSE,tilted_loss=1)
  good<-msc_row(candidate,"primary",task,.99)
  testthat::expect_true(good$returned)
  testthat::expect_equal(good$rate,2)
  testthat::expect_equal(good$candidate_index,4)
})

testthat::test_that("separate review refuses incomplete screening before any fitting", {
  directory<-tempfile();dir.create(directory);dir.create(file.path(directory,"screening"))
  on.exit(unlink(directory,recursive=TRUE))
  jsonlite::write_json(list(identity=list(configuration=list(expected_cells=72,screening_replications=12))),
    file.path(directory,"manifest.json"),auto_unbox=TRUE)
  jsonlite::write_json(list(remaining_datasets=1,numerical_pass=TRUE,completed_datasets=863),
    file.path(directory,"screening/progress.json"),auto_unbox=TRUE)
  testthat::expect_error(msc_review(list(output_root=directory),cs_config()),"controller")
  jsonlite::write_json(list(exit_code=1),file.path(directory,"controller_exit.json"),auto_unbox=TRUE)
  testthat::expect_error(msc_review(list(output_root=directory),cs_config()),"incomplete")
  testthat::expect_false("msc_fit" %in% all.names(body(msc_review)))
  testthat::expect_false("msc_job" %in% all.names(body(msc_review)))
  testthat::expect_true("write_verification" %in% names(formals(msc_verify)))
})

testthat::test_that("paired width comparisons preserve laws and actual observations", {
  d<-data.table::data.table(job_id=1:2,response_id=c("a","b"),n=100,content=.9,
    data_seed=1:2,method="fixed",returned=TRUE,lower=c(-1,-100),upper=c(1,100),width=c(2,200),attained=TRUE)
  other<-data.table::copy(d);other$method<-"primary";other$upper[2]<-110;other$width[2]<-210
  comparison<-msc_paired_comparison(rbind(d,other))
  testthat::expect_equal(comparison$totals$same_endpoints,1)
  testthat::expect_equal(comparison$totals$wider,1)
  testthat::expect_equal(comparison$casewise$mean_width_difference,c(0,10))
  testthat::expect_false("mean_width_difference" %in% names(comparison$totals))
  bad<-data.table::copy(other);bad$data_seed[1]<-99
  testthat::expect_error(msc_paired_comparison(rbind(d,bad)),"different observations")
  testthat::expect_error(msc_paired_comparison(rbind(d,other,other)),"Duplicate")
})
