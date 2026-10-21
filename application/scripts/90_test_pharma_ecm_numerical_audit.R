#!/usr/bin/env Rscript

for(file in c("pharma_tolerance_application.R","pharma_mti_application.R","pharma_mti_reporting.R",
              "pharma_ecm_numerical_audit.R","pharma_ecm_audit_reporting.R")) {
  source(file.path("application/scripts/lib",file))
}
args <- commandArgs(TRUE)
directory <- pta_arg_value(args,"--run-dir=","figures/data/pharma_ecm_numerical_audit_20261020")
x <- pea_read_audit(directory)
app <- pma_read_config(x$config$application_config)
canonical <- pma_read_report_inputs(app,file.path(x$config$canonical_directory,"results.csv"))
expected <- pea_select_cases(canonical$results,x$config$selection$per_response_scheme_flag)
if(x$manifest$mode=="smoke") expected <- do.call(rbind,lapply(split(expected,expected$response_id),head,1L))
# CSV has no type information for a column consisting entirely of NA values.
for(name in names(expected)) if(all(is.na(expected[[name]])) && all(is.na(x$cases[[name]]))) {
  expected[[name]] <- x$cases[[name]]
}
stopifnot(isTRUE(all.equal(expected,x$cases,check.attributes=FALSE)),
          x$manifest$all_saved_rows==3004,
          identical(sort(unique(x$summary$multiplier)),c(1L,5L,20L)))
profile <- pta_mti_profile_config(app,getwd())
omega <- as.numeric(profile$config$learning_rate %||% 1)
precision <- 1/as.numeric(profile$config$beta_ridge_tau2 %||% 1e4)
base <- profile$direct_dp$base
for(i in seq_len(nrow(x$cases))) {
  r <- x$cases[i,]
  m <- canonical$membership[canonical$membership$split_scheme==r$split_scheme &
                            canonical$membership$split_id==r$split_id,]
  y <- canonical$observed[[r$response_id]][m$analysis_row_id[m$training]]
  heldout <- canonical$observed[[r$response_id]][m$analysis_row_id[!m$training]]
  refs <- x$references[x$references$case_id==r$case_id,]
  candidates <- x$candidates[x$candidates$case_id==r$case_id,]
  refs <- refs[match(candidates$candidate_index,refs$candidate_index),]
  stopifnot(nrow(refs)==r$mti_candidates_evaluated,
    max(abs(refs$target_content-candidates$target_content))<1e-14,
    max(abs(refs$mean_tilt-candidates$mean_tilt))<1e-14)
  for(j in seq_len(nrow(refs))) {
    z <- refs[j,]
    f <- pea_objective(y,z$lower,z$upper,z$target_content,z$mean_tilt,omega,precision)
    original <- pea_objective(y,candidates$lower[j],candidates$upper[j],
      z$target_content,z$mean_tilt,omega,precision)
    stationary <- pea_stationarity(y,z$lower,z$upper,z$target_content,z$mean_tilt,omega,precision)
    stopifnot(abs(f-z$objective)<1e-8*(1+abs(f)),
      f<=original+1e-8*(1+abs(original)),stationary$distance<1e-6)
  }
  s <- x$summary[x$summary$case_id==r$case_id,]
  for(j in seq_len(nrow(s))) {
    z <- s[j,]
    trace <- x$traces[x$traces$case_id==r$case_id & x$traces$multiplier==z$multiplier,]
    stopifnot(nrow(trace)>0,tail(trace$iteration,1)==z$iterations,
      abs(tail(trace$root1,1)-z$lower)<1e-12,
      abs(tail(trace$root2,1)-z$upper)<1e-12,
      max(diff(trace$objective))<1e-8,
      abs(z$width-(z$upper-z$lower))<1e-12,
      z$heldout_empirical_content==mean(heldout>=z$lower & heldout<=z$upper))
    train_count <- vapply(seq_len(nrow(trace)),function(k)
      sum(y>=trace$root1[k] & y<=trace$root2[k]),integer(1))
    h <- pnorm(trace$root2,base$mean,base$sd)-pnorm(trace$root1,base$mean,base$sd)
    probability <- pbeta(app$analysis$content,train_count+h,length(y)-train_count+1-h,lower.tail=FALSE)
    stopifnot(max(abs(trace$posterior_content_probability-probability))<1e-10)
    if(z$multiplier>1) {
      t0 <- x$traces[x$traces$case_id==r$case_id & x$traces$multiplier==1,]
      stopifnot(max(abs(trace$root1[seq_len(nrow(t0))]-t0$root1))<1e-10,
                max(abs(trace$root2[seq_len(nrow(t0))]-t0$root2))<1e-10)
    }
  }
}
tmp <- tempfile("pharma-ecm-report-test-")
dir.create(tmp)
on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
out <- system2("Rscript",c("figures/generate_pharma_ecm_numerical_audit_report.R",
  paste0("--run-dir=",directory),paste0("--output-dir=",tmp),
  paste0("--output-pdf=",file.path(tmp,"diagnostics.pdf"))),stdout=TRUE,stderr=TRUE)
if(!is.null(attr(out,"status"))) stop(paste(out,collapse="\n"))
stopifnot(file.info(file.path(tmp,"diagnostics.pdf"))$size>10000,
  identical(readBin(file.path(tmp,"diagnostics.pdf"),"raw",5),charToRaw("%PDF-")))
d <- pta_read_csv(file.path(tmp,"case_decisions.csv"))
stopifnot(nrow(d)==nrow(x$cases),setequal(d$case_id,x$cases$case_id))
saved_decisions <- file.path(directory,"case_decisions.csv")
if(file.exists(saved_decisions)) {
  stopifnot(identical(readLines(saved_decisions),readLines(file.path(tmp,"case_decisions.csv"))))
}
for(i in seq_len(nrow(x$manifest$protected_before))) {
  z <- x$manifest$protected_before[i,]; pma_verify_hash(z$path,z$sha256)
}
cat("Verified all numerical references, traces, probabilities, selected cases, PDF, and protected publication inputs.\n")
