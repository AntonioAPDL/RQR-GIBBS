#!/usr/bin/env Rscript
arguments <- commandArgs(FALSE)
script <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(script),"..",".."),mustWork=TRUE))
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
  "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R",
  "mti_q_grid_study.R","mti_final_reporting.R","mti_profile_adoption.R"))
  source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
cfg <- pma_read_config(pta_arg_value(args,"--config=",mpa_application_default))
run <- pta_arg_value(args,"--run-dir=","application/outputs/pharma_mti_profile_application_20261025/confirmatory")
context <- mpa_application_context(cfg)
manifest <- pma_verify_run(run,cfg)
z <- read.csv(file.path(run,"results.csv"))
old <- read.csv(file.path(cfg$historical_evidence,"results.csv"))
key <- function(x)paste(x$response_id,x$split_scheme,x$split_id,x$method_id)
old <- old[match(key(z),key(old)),]
stopifnot(!anyNA(old$method_id),all(z$interval_returned),
  all(abs(z$mti_maximum_candidate_objective_gap[z$method_id==pma_methods[1]])<=1e-8))
fields <- c("lower","upper","width","heldout_empirical_content","heldout_lower_omitted","heldout_upper_omitted")
benchmark <- z$method_id!=pma_methods[1]
stopifnot(isTRUE(all.equal(z[benchmark,fields],old[benchmark,fields],
  check.attributes=FALSE,tolerance=1e-12)))
splits <- pma_make_splits(context$data,cfg,"confirmatory")
split_key <- vapply(splits,function(s)paste(s$scheme,s$split_id),character(1))
for(i in seq_len(nrow(z))) {
  s <- splits[[match(paste(z$split_scheme[i],z$split_id[i]),split_key)]]
  y <- context$data[[z$response_id[i]]][s$heldout]
  empirical <- rqrgibbs::rqr_interval_empirical_content(y,z$lower[i],z$upper[i])
  stopifnot(abs(empirical$empirical_content-z$heldout_empirical_content[i])<1e-12,
    abs(empirical$lower_omitted-z$heldout_lower_omitted[i])<1e-12,
    abs(empirical$upper_omitted-z$heldout_upper_omitted[i])<1e-12)
}
primary <- z[z$method_id==pma_methods[1],]
checks <- parallel::mclapply(seq_len(nrow(primary)),function(i) {
  r <- primary[i,]; s <- splits[[match(paste(r$split_scheme,r$split_id),split_key)]]
  y <- context$data[[r$response_id]][s$train]
  fit <- rqrgibbs::rqr_mti_intercept_fit(y,r$mti_selected_q,r$mti_selected_tilt,
    learning_rate=1,prior_precision=1e-4)
  dp <- rqrgibbs::rqr_dp_fit(y,1,rqrgibbs::rqr_dp_base_normal(0,4),strict_bayes=TRUE)
  probability <- rqrgibbs::rqr_dp_content_probability(dp,r$lower,r$upper,.9)$posterior_content_probability
  error <- max(abs(c(fit$lower-r$lower,fit$upper-r$upper)))
  stopifnot(fit$converged,error<=1e-10*(1+abs(r$lower)+abs(r$upper)),
    abs(probability-r$mti_posterior_content_probability)<1e-12,
    probability>=r$mti_posterior_confidence)
  c(endpoint_error=error,relative_objective_gap=abs(fit$relative_objective_gap))
},mc.cores=4L,mc.set.seed=FALSE)
stopifnot(all(vapply(checks,is.numeric,logical(1))))
checks <- do.call(rbind,checks)
result <- list(passed=TRUE,calculation_commit=manifest$repository_commit,
  results_sha256=mp_sha(file.path(run,"results.csv")),
  policy_sha256=context$adopted$adoption$policy_sha256,
  split_membership_matches_historical=TRUE,paired_benchmark_results_unchanged=TRUE,
  empirical_content_rows_recomputed=nrow(z),fixed_target_profiles_recomputed=nrow(primary),
  maximum_endpoint_error=max(checks[,"endpoint_error"]),
  maximum_recomputed_relative_objective_gap=max(checks[,"relative_objective_gap"]),
  maximum_all_candidate_objective_gap=max(abs(primary$mti_maximum_candidate_objective_gap)),
  posterior_probabilities_recomputed=TRUE)
pta_atomic_write_json(result,file.path(run,"profile_validation.json"))
print(result)
