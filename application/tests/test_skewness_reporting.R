#!/usr/bin/env Rscript
Sys.setenv(MTI_SKEWNESS_NO_MAIN=1)
source("application/scripts/144_run_mti_skewness_extension.R")
cfg<-msx_configuration("application/config/mti_skewness_extension_20261121.json")
root<-Sys.getenv("MTI_SKEWNESS_CHECK_ROOT",file.path("/var/tmp/jaguir26-mti-studies",
  paste0(cfg$study_id,"_",mgfp_git("rev-parse","HEAD"))))
stopifnot(file.exists(file.path(root,"prelaunch_checks.json")))
directory<-tempfile("skewness-reporting-integration-",tmpdir="application/cache")
dir.create(directory)
directory<-normalizePath(directory)
context<-msx_context(cfg,directory)
small<-cfg;small$replications<-1L;small$data_stream_seed<-cfg$data_stream_seed-1L
design<-msx_design(small,msx_admission(cfg))
design$tasks$ecm_seed<-as.integer(1900000000+10*design$tasks$job_id)
prepared<-list(identity="numerical-check-reporting-test",design=design)
dir.create(file.path(directory,"fits"))
for(i in seq_len(nrow(design$tasks))){
  value<-readRDS(file.path(root,"checks",sprintf("test_%03d.rds",i)))
  stopifnot(identical(value$task,design$tasks[i,,drop=FALSE]))
  msx_save(value,file.path(directory,"fits",sprintf("fit_%06d.rds",i)),prepared$identity)
}
rows<-msx_verify(small,context,prepared,directory,4L)
stopifnot(nrow(rows)==3L*nrow(design$tasks))
output<-msx_export(small,context,directory)
summary<-data.table::fread(file.path(output,"combined_case_summary.csv"))
historical<-data.table::fread(file.path(cfg$historical_evidence,"simulation_cells.csv"))[method %in% cfg$methods]
matched<-merge(summary,historical,by.x=c("law_id","n","content","method"),
  by.y=c("response_id","n","content","method"),suffixes=c("_new","_old"))
stopifnot(nrow(matched)==216L,all(matched$attainment_new==matched$attainment_old),
  max(abs(matched$mean_width_new-matched$mean_width_old))<1e-12,
  max(abs(matched$width_q025_new-matched$width_q025_old))<1e-12,
  max(abs(matched$width_q975_new-matched$width_q975_old))<1e-12,
  all(summary[law_id %in% cfg$laws$id]$samples==1L),
  file.exists(file.path(output,"review_tables.pdf")))
cat("Full verification/export and preservation of all historical estimates passed.\n",
  "Numerical-check demonstration only, not assessment evidence:\n",output,"\n")
