mec_finalization_context <- function(study) {
  original <- mwd_json(file.path(study$output_root,"manifest.json"))$identity
  joint <- mwd_json(file.path(original$source$output_root,"manifest.json"))$identity$configuration
  scratch <- tempfile(".ecm-finalization-context-",tmpdir=dirname(study$output_root))
  mwd_require(dir.create(scratch),"Cannot create a temporary generator workspace.")
  on.exit(unlink(scratch,recursive=TRUE))
  joint$output_root <- scratch
  mjs_context(joint,original$source$dependency)
}

mec_sampling_source <- function(study) {
  root <- normalizePath(study$checkout,mustWork=TRUE)
  directory <- normalizePath(study$output_root,mustWork=TRUE)
  commit <- paste(system2("git",c("--no-optional-locks","-C",shQuote(root),"rev-parse","HEAD"),
    stdout=TRUE),collapse="\n")
  manifest <- mwd_json(file.path(directory,"manifest.json"))$identity
  mwd_require(identical(commit,study$source_commit) && identical(commit,manifest$source_commit),
    "The pinned sampling checkout or commit changed.")
  paths <- names(manifest$programs)
  mwd_require(all(!grepl("^/|(^|/)\\.\\.(/|$)",paths)),"Invalid sampling program paths.")
  mgfp_check_hashes(manifest$programs,file.path(root,paths))
  mgfp_check_hashes(manifest$article_files,file.path(root,names(manifest$article_files)))
  mgfp_check_hashes(manifest$article_files)
  progress <- mwd_json(file.path(directory,"validation/progress.json"))
  exit <- mwd_json(file.path(directory,"controller_exit.json"))
  mwd_require(progress$completed_datasets==progress$expected_datasets && progress$remaining_datasets==0 &&
    isTRUE(progress$numerical_pass) && exit$exit_code %in% c(0L,1L),
    "Sampling is incomplete or its numerical results require review.")
  list(root=root,directory=directory,manifest=manifest)
}

mec_finalization_rows <- function(source,context,workers=8L) {
  directory <- source$directory;cfg <- source$manifest$configuration
  tasks <- readRDS(file.path(directory,"validation_tasks.rds"))
  identity <- mwd_json(file.path(directory,"validation/manifest.json"))$analysis_id
  paths <- file.path(directory,"validation/fits",sprintf("fit_%06d.rds",tasks$job_id))
  mwd_require(nrow(tasks)==cfg$expected_cells*cfg$validation_replications &&
    all(file.exists(paths)) && all(file.exists(paste0(paths,".json"))),"Saved fits or receipts are missing.")
  groups <- split(seq_len(nrow(tasks)),ceiling(seq_len(nrow(tasks))/1000L))
  pieces <- parallel::mclapply(groups,function(ids) {
    receipts <- character(length(ids))
    rows <- lapply(seq_along(ids),function(j) {
      i <- ids[j];task <- tasks[i,,drop=FALSE]
      value <- mbc_read_fit(paths[i],identity,task)
      set.seed(task$data_seed);y <- context$laws[[task$response_id]]$r(task$n)
      mwd_require(isTRUE(value$ok) && value$unresolved_candidates==0L &&
        identical(value$sample_sha256,mwd_hash(y)) && value$prior$prior_sd==cfg$prior_sd &&
        identical(value$prior$data_adaptive_prior,FALSE) && all(value$candidates$numerically_accurate) &&
        all(value$rows$numerically_accurate),"A saved sample, independent prior or numerical check differs.")
      d <- data.table::as.data.table(value$rows)
      actual <- context$laws[[task$response_id]]$p(d$upper)-context$laws[[task$response_id]]$p(d$lower)
      mwd_require(all(is.finite(actual[d$returned])) &&
        max(abs(actual-d$population_content),na.rm=TRUE)<1e-12 &&
        max(abs(d$width-(d$upper-d$lower)),na.rm=TRUE)<1e-10 &&
        all(d$attained==(d$returned & actual>=task$content)),"A full-precision saved interval does not reproduce.")
      receipts[j] <- mwd_file_hash(paste0(paths[i],".json"))
      d
    })
    list(rows=data.table::rbindlist(rows,use.names=TRUE),receipts=receipts)
  },mc.cores=max(1L,min(as.integer(workers),8L,parallelly::availableCores())),mc.set.seed=FALSE)
  mwd_require(all(vapply(pieces,function(x)is.list(x) && is.data.frame(x$rows),FALSE)),
    "A saved-fit audit failed; no fits were changed or replaced.")
  list(rows=data.table::rbindlist(lapply(pieces,`[[`,"rows"),use.names=TRUE),
    receipt_set_sha256=mwd_hash(unlist(lapply(pieces,`[[`,"receipts"),use.names=FALSE)),datasets=nrow(tasks))
}

mec_same_outcomes <- function(rows,path) {
  before <- data.table::fread(path);after <- data.table::copy(data.table::as.data.table(rows))
  keys <- c("job_id","method")
  data.table::setorderv(before,keys);data.table::setorderv(after,keys)
  mwd_require(nrow(before)==nrow(after) && identical(before$job_id,after$job_id) &&
    identical(before$method,after$method) && identical(before$returned,after$returned) &&
    identical(before$attained,after$attained),"Finalization would change a recorded return or attainment event.")
  invisible(TRUE)
}

mec_finalization_overview <- function(rows,cfg) {
  s <- mec_summary(rows,cfg)
  s[,.(cases=.N,datasets=sum(samples),returned=sum(returned),
    minimum_attainment=min(attainment),maximum_attainment=max(attainment),
    pointwise_underattainment_cases=sum(upper<tolerance_confidence),
    simultaneous_underattainment_cases=sum(simultaneous_upper<tolerance_confidence)),by=method]
}

mec_verify_finalization <- function(directory,context) {
  record <- mwd_json(file.path(directory,"finalization.json"))
  mwd_require(identical(record$identity_hash,mwd_hash(record$identity)),"Finalization identity changed.")
  mgfp_check_hashes(record$identity$analysis_programs)
  source <- mec_sampling_source(record$identity$sampling)
  mgfp_check_hashes(record$identity$original_files,
    file.path(source$directory,names(record$identity$original_files)))
  raw <- mec_finalization_rows(source,context)
  mwd_require(identical(raw$receipt_set_sha256,record$receipt_set_sha256),"Original fitted-result receipts changed.")
  mec_same_outcomes(raw$rows,file.path(source$directory,"validation/interval_results.csv"))
  mgfp_check_hashes(record$artifacts,file.path(directory,names(record$artifacts)))
  cfg <- source$manifest$configuration;cfg$output_root <- directory
  mec_verify(cfg,context,sampling_root=source$root)
  mwd_require(isTRUE(all.equal(as.data.frame(data.table::fread(file.path(directory,"validation/method_overview.csv"))),
    as.data.frame(mec_finalization_overview(raw$rows,cfg)),check.attributes=FALSE,tolerance=1e-12)),
    "The exploratory method overview does not reproduce.")
  cat("Separate finalization and every original saved fit verified.\n")
  invisible(TRUE)
}

mec_finalize <- function(study,context,analysis_programs) {
  source <- mec_sampling_source(study)
  commit <- mgfp_git("rev-parse","HEAD")
  directory <- paste0(source$directory,"_finalized_",commit)
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  lock <- file.path(directory,".finalization.lock")
  mwd_require(dir.create(lock,showWarnings=FALSE),"A finalization lock exists; inspect it before resuming.")
  on.exit(unlink(lock,recursive=TRUE))
  originals <- c("manifest.json","frozen_policy.json","frozen_selection.rds","frozen_selection.csv",
    "validation_tasks.rds","validation/manifest.json","validation/progress.json",
    "validation/interval_results.csv","validation/comparison_summary.csv","validation/paired_comparison.csv",
    "validation/exploratory_comparisons.pdf","study_summary.json","controller_exit.json")
  if(file.exists(file.path(source$directory,"frozen_data_generators.R")))
    originals <- c(originals,"frozen_data_generators.R")
  original_hashes <- as.list(setNames(vapply(file.path(source$directory,originals),mwd_file_hash,""),originals))
  identity <- list(analysis_commit=commit,analysis_checkout=normalizePath("."),analysis_programs=analysis_programs,
    sampling=study,original_files=original_hashes,no_refitting=TRUE,no_rule_reselection=TRUE,article_replaced=FALSE)
  record_path <- file.path(directory,"finalization.json")
  record <- if(file.exists(record_path))mwd_json(record_path) else NULL
  if(!is.null(record))mwd_require(identical(record$identity_hash,mwd_hash(identity)),"Finalization inputs changed.")
  result <- list(output_root=directory,sampling_output_root=source$directory,sampling_commit=study$source_commit,
    analysis_commit=commit,analysis_checkout=normalizePath("."))
  status_path <- file.path(directory,"finalization_status.json")
  if(file.exists(status_path) && identical(mwd_json(status_path)$status,"complete_exploratory_requires_scientific_review")) {
    mec_verify_finalization(directory,context);return(result)
  }
  tryCatch({
    mgfp_check_hashes(analysis_programs)
    raw <- mec_finalization_rows(source,context)
    mec_same_outcomes(raw$rows,file.path(source$directory,"validation/interval_results.csv"))
    dir.create(file.path(directory,"validation"),showWarnings=FALSE)
    for(name in c("manifest.json","frozen_policy.json","frozen_selection.rds","validation_tasks.rds"))
      mwd_require(file.copy(file.path(source$directory,name),file.path(directory,name),overwrite=TRUE),
        "Cannot preserve an original finalization input.")
    cfg <- source$manifest$configuration;cfg$output_root <- directory
    mec_write_intervals(raw$rows,file.path(directory,"validation/interval_results.csv"))
    mec_report(raw$rows,cfg)
    mwd_write(mec_finalization_overview(raw$rows,cfg),file.path(directory,"validation/method_overview.csv"))
    mec_verify(cfg,context,sampling_root=source$root)
    mgfp_check_hashes(original_hashes,file.path(source$directory,names(original_hashes)))
    artifacts <- c("manifest.json","frozen_policy.json","frozen_selection.rds","validation_tasks.rds",
      "validation/interval_results.csv","validation/comparison_summary.csv","validation/paired_comparison.csv",
      "validation/method_overview.csv","validation/exploratory_comparisons.pdf")
    artifact_hashes <- as.list(setNames(vapply(file.path(directory,artifacts),mwd_file_hash,""),artifacts))
    mgfp_json_write(list(identity=identity,identity_hash=mwd_hash(identity),datasets=raw$datasets,
      interval_rows=nrow(raw$rows),receipt_set_sha256=raw$receipt_set_sha256,artifacts=artifact_hashes,
      original_files_changed=FALSE,changed_attainment_events=0,refitted_datasets=0,time=mgfp_time()),record_path)
    mgfp_json_write(list(status="complete_exploratory_requires_scientific_review",datasets=raw$datasets,
      remaining_dataset_runs=0,numerical_pass=TRUE,original_controller_exit_preserved=TRUE,
      article_replaced=FALSE,time=mgfp_time()),status_path)
    result
  },error=function(e) {
    mgfp_json_write(list(status="stopped_requires_review",reason=conditionMessage(e),article_replaced=FALSE,
      original_input_preservation_verified=FALSE,time=mgfp_time()),status_path)
    stop(e)
  })
}
