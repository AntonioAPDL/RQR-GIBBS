gamma_root<-normalizePath(if(file.exists("application/scripts/lib/mti_gamma_scale_study.R"))"." else "../../..")
for(name in c("mti_gibbs_width_diagnosis","mti_ecm_common_scale_study","mti_gamma_scale_study"))
  source(file.path(gamma_root,"application/scripts/lib",paste0(name,".R")))
gamma_config<-function()mwd_json(file.path(gamma_root,"application/config/mti_gamma_scale_20261119.json"))

testthat::test_that("Gamma study holds prior information and costly followups fixed", {
  cfg<-gamma_config();testthat::expect_true(mgsc_validate(cfg))
  for(name in c("sample_gibbs","application_transfer","automatic_article_replacement","unrestricted_nonzero_tilt")) {
    bad<-cfg;bad[[name]]<-TRUE;testthat::expect_error(mgsc_validate(bad),"changed")
  }
  bad<-cfg;bad$coordinates$estimated_from_analysis_sample<-TRUE
  testthat::expect_error(mgsc_validate(bad),"changed")
  bad<-cfg;bad$launch_ceiling<-"validation"
  testthat::expect_error(mgsc_validate(bad),"changed")
  bad<-cfg;bad$prior_specs$G16$rate_bounds<-c(.5,2)
  testthat::expect_error(mgsc_validate(bad),"independent Gamma prior")
  testthat::expect_equal(mgsc_control(cfg$prior_specs$G16_Z,cfg)$log_rate_bounds,c(-Inf,log(4)))
  testthat::expect_equal(vapply(cfg$prior_specs,`[[`,0,"shape"),c(G4=4,G16=16,G64=64,G16_N=16,G16_W=16,G16_Z=16))
})

testthat::test_that("Gamma screening decisions distinguish absence of benefit and missing outputs", {
  x<-data.frame(model="G4",datasets=864,returned_pairs=864,same_endpoints=864,narrower=0,wider=0)
  testthat::expect_match(mgsc_decision(x)$status,"no_endpoint_effect")
  x$wider<-1;testthat::expect_match(mgsc_decision(x)$status,"no_width_gain")
  x$narrower<-1;testthat::expect_match(mgsc_decision(x)$status,"requires_prior_and_support_review")
  testthat::expect_false(mgsc_decision(x)$automatic_full_validation)
  x$returned_pairs<-863;testthat::expect_match(mgsc_decision(x)$status,"missing_intervals")
})

testthat::test_that("Gamma paired comparisons retain design identity and raw width differences", {
  d<-data.table::data.table(job_id=1:2,response_id=c("a","b"),n=100,content=.9,data_seed=1:2,
    method="fixed",returned=TRUE,width=c(2,200),lower=c(-1,-100),upper=c(1,100),attained=TRUE)
  other<-data.table::copy(d);other$method<-"G4";other$upper[2]<-90;other$width[2]<-190
  comparison<-mgsc_pairs(rbind(d,other),"G4")
  testthat::expect_equal(comparison$totals$narrower,1)
  testthat::expect_equal(comparison$casewise$mean_width_difference,c(0,-10))
  testthat::expect_false("mean_width_difference" %in% names(comparison$totals))
  bad<-data.table::copy(other);bad$data_seed[1]<-99
  testthat::expect_error(mgsc_pairs(rbind(d,bad),"G4"),"observations")
  testthat::expect_error(mgsc_pairs(rbind(d,other,other),"G4"),"Duplicate")
})

testthat::test_that("shared profile fitting keeps historical entry arguments intact", {
  testthat::expect_setequal(names(formals(msc_fit)),c("y","task","cfg","context","bounds"))
  testthat::expect_setequal(names(formals(msc_fit_control)),c("y","task","cfg","context","scale_control"))
  body<-paste(deparse(msc_fit_control),collapse="\n")
  testthat::expect_match(body,"ec[$]scale_control <- scale_control")
  testthat::expect_match(body,"scale_control = scale_control")
})
