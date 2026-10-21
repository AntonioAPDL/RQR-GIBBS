#!/usr/bin/env Rscript

arguments <- commandArgs(FALSE)
path <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(path),"..",".."),mustWork=TRUE))
for (file in c("pharma_tolerance_application.R","pharma_mti_application.R",
               "pharma_mti_reporting.R","pharma_ecm_numerical_audit.R")) {
  source(file.path("application/scripts/lib",file))
}
args <- commandArgs(TRUE)
cfg_path <- pta_arg_value(args,"--config=","application/config/pharma_ecm_numerical_audit_20261020.json")
cfg <- pta_read_config(cfg_path)
mode <- pta_arg_value(args,"--mode=","preflight")
if (!mode %in% c("preflight","smoke","run")) stop("Unknown diagnostic mode.")
if (isTRUE(cfg$promote_article_results)) stop("This study cannot replace article results.")
app <- pma_read_config(cfg$application_config)
inputs <- pma_read_report_inputs(app,file.path(cfg$canonical_directory,"results.csv"))
profile <- pta_mti_profile_config(app,getwd())
policy <- pta_mti_policy_row(app,getwd())
protected <- unique(c("main.tex","rqr-gibbs-supplement.tex",cfg$application_config,
  app$mti_ecm$policy_csv,app$mti_ecm$validation_config,app$scan_reference$scan_calibration_csv,
  list.files(cfg$canonical_directory,full.names=TRUE),
  list.files("tables","^pharma_application_.*\\.(csv|tex)$",full.names=TRUE),
  list.files("figures/generated","pharma.*\\.(csv|png)$",full.names=TRUE)))
before <- pma_file_record(protected)
cases <- pea_select_cases(inputs$results,as.integer(cfg$selection$per_response_scheme_flag))
if (mode=="smoke") cases <- do.call(rbind,lapply(split(cases,cases$response_id),head,1L))
if (mode=="preflight") {
  cat("Verified canonical evidence and frozen inputs. Prespecified cases:",nrow(cases),"\n")
  print(table(cases$response_id,cases$split_scheme))
  quit(status=0L)
}
pta_require_packages(c("rqrgibbs","digest","jsonlite"))
if (as.character(packageVersion("rqrgibbs"))!=inputs$manifest$package_versions$rqrgibbs) {
  stop("Use the frozen rqrgibbs package version.")
}
if (any(unlist(cfg$iteration_multipliers)<1) || cfg$threads_per_worker!=1) stop("Invalid audit controls.")
output <- pta_arg_value(args,"--output-dir=",file.path(cfg$output_root,
  paste0(mode,"_",format(Sys.time(),"%Y%m%dT%H%M%SZ",tz="UTC"))))
if (file.exists(output)) stop("A fresh diagnostic output directory is required.")
dir.create(output,recursive=TRUE)
Sys.setenv(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1")
workers <- as.integer(pta_arg_value(args,"--workers=",as.character(cfg$workers)))
if (!is.finite(workers) || workers<1) stop("Invalid worker count.")
workers <- min(workers,nrow(cases))
started <- format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC")
start <- proc.time()[["elapsed"]]
source_files <- c(cfg_path,"application/scripts/89_run_pharma_ecm_numerical_audit.R",
  "application/scripts/lib/pharma_ecm_numerical_audit.R")
source_hashes <- pma_file_record(source_files)
launch_commit <- pma_git("rev-parse","HEAD")
launch_status <- pma_git("status","--porcelain")
pma_write_csv(cases,file.path(output,"selected_cases.csv"))
omega <- as.numeric(profile$config$learning_rate %||% 1)
precision <- 1/as.numeric(profile$config$beta_ridge_tau2 %||% 1e4)
all_mti <- inputs$results[inputs$results$method_id==pma_methods[1],]
membership_key <- paste(inputs$membership$split_scheme,inputs$membership$split_id,sep="|")
memberships <- split(inputs$membership,membership_key)
all_diagnostics <- do.call(rbind,lapply(seq_len(nrow(all_mti)),function(i) {
  r <- all_mti[i,]
  s <- memberships[[paste(r$split_scheme,r$split_id,sep="|")]]
  y <- inputs$observed[[r$response_id]][s$analysis_row_id[s$training]]
  cbind(r[,c("response_id","split_scheme","split_id","mti_ecm_converged",
              "mti_ecm_iterations","mti_ecm_final_stationarity")],
        pea_saved_diagnostic(y,r,omega,precision))
}))
pma_write_csv(all_diagnostics,file.path(output,"all_saved_diagnostics.csv"))
one <- function(i) {
  tryCatch(pea_replay_case(cases[i,],inputs,cfg,profile,policy),error=function(e) {
    list(error=conditionMessage(e),case_id=cases$case_id[i])
  })
}
answers <- if(workers>1 && .Platform$OS.type!="windows") {
  parallel::mclapply(seq_len(nrow(cases)),one,mc.cores=workers,mc.set.seed=FALSE)
} else lapply(seq_len(nrow(cases)),one)
failed <- vapply(answers,function(x)!is.null(x$error),logical(1))
if (any(failed)) {
  failures <- do.call(rbind,lapply(answers[failed],as.data.frame))
  pma_write_csv(failures,file.path(output,"failures.csv"))
  stop("Diagnostic cases failed; all failure records retained in ",output)
}
for (name in c("summary","references","candidates")) {
  pma_write_csv(do.call(rbind,lapply(answers,`[[`,name)),file.path(output,paste0(name,".csv")))
}
con <- gzfile(file.path(output,"iteration_traces.csv.gz"),"wt")
traces <- do.call(rbind,lapply(answers,`[[`,"traces"))
portable <- traces
for (name in names(portable)) if(is.double(portable[[name]])) {
  x <- portable[[name]]; portable[[name]] <- sprintf("%.17g",x)
  portable[[name]][is.na(x)] <- NA_character_
}
write.csv(portable,con,row.names=FALSE); close(con)
writeLines(capture.output(sessionInfo()),file.path(output,"sessionInfo.txt"))
file.copy(cfg_path,file.path(output,"config_frozen.json"))
if (!identical(before,pma_file_record(protected)) ||
    !identical(source_hashes,pma_file_record(source_files))) {
  stop("Protected evidence or diagnostic source changed during the audit.")
}
artifacts <- list.files(output,full.names=TRUE)
records <- pma_file_record(artifacts); records$path <- basename(records$path)
manifest <- list(schema_version=cfg$schema_version,mode=mode,repository_commit=launch_commit,
  branch=pma_git("branch","--show-current"),git_status=launch_status,
  started_utc=started,completed_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
  elapsed_sec=proc.time()[["elapsed"]]-start,workers=workers,threads_per_worker=1,
  expected_cases=nrow(cases),completed_cases=length(answers),
  expected_budget_rows=nrow(cases)*length(cfg$iteration_multipliers),
  iteration_trace_rows=nrow(traces),all_saved_rows=nrow(all_diagnostics),
  source_files=source_hashes,protected_before=before,protected_unchanged=TRUE,
  R_version=R.version.string,package_version=as.character(packageVersion("rqrgibbs")),
  package_library=find.package("rqrgibbs"),
  shared_library_sha256=pta_file_sha256(file.path(find.package("rqrgibbs"),"libs","rqrgibbs.so")),
  configured_exdqlm_commit=inputs$manifest$pinned_exdqlm_commit,
  exdqlm_loaded="exdqlm" %in% loadedNamespaces(),
  canonical_results_sha256=pta_file_sha256(file.path(cfg$canonical_directory,"results.csv")),
  artifact_records=records,article_results_replaced=FALSE)
pta_atomic_write_json(manifest,file.path(output,"manifest.json"))
cat("Completed",nrow(cases),"diagnostic cases in",manifest$elapsed_sec,"seconds.\n")
cat("Preserved every published input. Output:",output,"\n")
