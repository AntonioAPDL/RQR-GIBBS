#!/usr/bin/env Rscript
source("application/scripts/lib/pharma_tolerance_application.R")
source("application/scripts/lib/pharma_mti_application.R")
source("application/scripts/lib/pharma_ecm_numerical_audit.R")
set.seed(20261021)
samples <- list(tied=c(0,0,1,1,2),mixed=c(-3,-1,.2,2,4),constant=rep(2,5),
  normal=rnorm(12),exponential=rexp(15),boundary=c(0,1,2,10))
observations <- do.call(rbind,lapply(names(samples),function(id)
  data.frame(sample_id=id,index=seq_along(samples[[id]]),y=samples[[id]])))
targets <- expand.grid(sample_id=names(samples)[1:5],q=c(.6,.9,.9775),
  delta=c(-.1,0,.1),stringsAsFactors=FALSE)
targets$precision <- .01
targets <- rbind(targets,data.frame(sample_id="boundary",q=.625,
  delta=rqrgibbs::rqr_mti_empirical_tilt_bounds(samples$boundary,.625)$delta_lower,
  precision=1e-4))
references <- do.call(rbind,lapply(seq_len(nrow(targets)),function(i) {
  z <- targets[i,]
  ref <- pea_reference(samples[[z$sample_id]],z$q,z$delta,
    precision=z$precision,direct_checks=.Machine$integer.max)
  data.frame(z,lower=ref$lower,upper=ref$upper,objective=ref$objective)
}))
directory <- "application/tests/testthat/fixtures/mti_intercept_profile"
dir.create(directory,recursive=TRUE,showWarnings=FALSE)
pma_write_csv(observations,file.path(directory,"observations.csv"))
pma_write_csv(references,file.path(directory,"references.csv"))
cat("Wrote",nrow(references),"independent fixed-target references.\n")
