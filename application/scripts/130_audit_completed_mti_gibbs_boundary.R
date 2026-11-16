#!/usr/bin/env Rscript
for (name in c("mti_gibbs_full_report_review", "mti_gibbs_full_publication", "mti_gibbs_width_diagnosis",
  "mti_profile_revision", "mti_gibbs_boundary_calibration"))
  source(file.path("application/scripts/lib", paste0(name, ".R")))
data.table::setDTthreads(1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
cfg <- mwd_json("application/config/mti_gibbs_boundary_closeout_20261116.json")
boundary <- mwd_json(cfg$boundary_config)
checkout <- file.path(mgfp_common(), cfg$completed_worktree)
p <- file.path(checkout, boundary$output_root)
manifest <- mwd_json(file.path(p, "manifest.json"))
original <- getwd(); setwd(checkout)
mgfp_check_hashes(manifest$identity$programs); mgfp_check_hashes(manifest$identity$inputs)
mgfp_check_hashes(manifest$identity$package$installed_files)
frozen <- mwd_json(file.path(p, "frozen_policy.json"))
mgfp_check_hashes(frozen$target_policy); mgfp_check_hashes(frozen$interval_policy)
setwd(original)
article <- mwd_json("application/config/mti_ecm_article_20261111.json")
validation <- jsonlite::read_json(article$validation_config, simplifyVector = FALSE)
generators <- mp_dgp_environment(file.path(p,"frozen_data_generators.R"))
laws <- setNames(lapply(validation$dgps, generators$dgp_meta), vapply(validation$dgps, `[[`, "", "dgp_id"))
numerical <- list()
for (stage in c("development", "calibration", "pilot")) {
  tasks <- as.data.frame(data.table::fread(file.path(p,stage,"tasks.csv")))
  id <- mwd_json(file.path(p,stage,"manifest.json"))$analysis_id
  batches <- split(seq_len(nrow(tasks)), ceiling(seq_len(nrow(tasks))/100))
  checked <- parallel::mclapply(batches, function(batch) {
    data.table::rbindlist(lapply(batch, function(i) {
      path <- file.path(p,stage,"fits",sprintf("fit_%06d.rds",tasks$job_id[i]))
      r <- readRDS(path); mbc_read_fit(path,id,r$task)
      mwd_require(isTRUE(r$ok) && isTRUE(all.equal(r$task,tasks[i, ],check.attributes=FALSE)),
        "Saved numerical result or task differs.")
      set.seed(r$task$data_seed); y <- laws[[r$task$response_id]]$r(r$task$n)
      mwd_require(all(r$rows$sample_sha256 == mwd_hash(y)), "Reconstructed sample hash differs.")
      for (record in r$records) mwd_require(isTRUE(record$adequate) && length(record$fits) == 4L &&
        all(record$parameters$preferred_pass & record$parameters$mean_precision) &&
        all(vapply(record$fits,function(f) all(is.finite(f$draws)) &&
          all(f$draws[,2] > f$draws[,1]) && all(is.finite(f$retained_objective)),FALSE)),
        "Saved posterior diagnostics require review.")
      data.table::as.data.table(r$rows)
    }))
  },mc.cores=min(8L,parallelly::availableCores()),mc.set.seed=FALSE)
  failed <- vapply(checked,inherits,FALSE,"try-error")
  if (any(failed)) stop(as.character(checked[[which(failed)[1]]]),call.=FALSE)
  d <- data.table::rbindlist(checked)
  stored <- data.table::fread(file.path(p,stage,"posterior_summary.csv"))
  data.table::setorderv(d,c("job_id","variant")); data.table::setorderv(stored,c("job_id","variant"))
  mwd_require(isTRUE(all.equal(as.data.frame(d),as.data.frame(stored),tolerance=1e-12,
    check.attributes=FALSE)),"Saved posterior summaries differ.")
  numerical[[stage]] <- list(datasets=data.table::uniqueN(d$job_id),targets=nrow(d),
    maximum_rhat=max(d$max_rhat),minimum_bulk_ess=min(d$min_bulk_ess),minimum_tail_ess=min(d$min_tail_ess))
  if (stage == "calibration") {
    calculated <- mbc_calibrate(d,boundary)$policy
    stored_policy <- data.table::fread(file.path(p,"calibration_policy.csv"))
    mwd_require(isTRUE(all.equal(as.data.frame(calculated),as.data.frame(stored_policy),
      tolerance=1e-12,check.attributes=FALSE)),"Calibrated multipliers differ.")
  }
  cat(stage,numerical[[stage]]$datasets,"datasets verified\n"); flush.console()
}
intervals <- data.table::fread(file.path(p,"pilot/interval_results.csv"))
errors <- vapply(names(laws), function(name) {
  z <- intervals[response_id == name]
  content <- laws[[name]]$p(z$upper)-laws[[name]]$p(z$lower)
  mwd_require(all((content >= z$content) == z$attained),"True-content decisions differ.")
  max(abs(content-z$population_content))
},0)
mwd_require(max(errors) < 1e-9 && nrow(intervals) == 36000L && all(intervals$returned),
  "Pilot population contents or counts differ.")
additional <- list()
for (stage in c("replay","confirmation")) {
  directory <- file.path(cfg$output_root,stage)
  tasks <- as.data.frame(data.table::fread(file.path(directory,"tasks.csv")))
  id <- mwd_json(file.path(directory,"manifest.json"))$identity
  values <- lapply(seq_len(nrow(tasks)),function(i) {
    path <- file.path(directory,"fits",sprintf("fit_%06d.rds",tasks$job_id[i]))
    r <- readRDS(path); mbc_read_fit(path,id,r$task)
    mwd_require(isTRUE(r$ok) && isTRUE(all.equal(r$task,tasks[i, ],check.attributes=FALSE)) &&
      r$prior_sd == 2 && !r$prior_uses_sample && all(r$candidates$numerically_accurate),
      "Closeout fitted object differs.")
    set.seed(r$task$data_seed); y <- laws[[r$task$response_id]]$r(r$task$n)
    mwd_require(identical(r$sample_sha256,mwd_hash(y)),"Closeout reconstructed sample differs.")
    data.frame(job_id=r$task$job_id,data_seed=r$task$data_seed,sample_sd=sd(y),
      prior_sd=r$prior_sd,selected_q=r$q,selected_delta=r$delta)
  })
  rows <- do.call(rbind,values)
  mwd_write(rows,file.path(cfg$evidence_root,paste0(stage,"_samples.csv")))
  additional[[stage]] <- list(datasets=nrow(rows),all_checks_passed=TRUE,mean_sample_sd=mean(rows$sample_sd))
}
summary <- list(review_checkout_head=mgfp_git("rev-parse","HEAD"),
  review_checkout_dirty=nzchar(mgfp_git("status","--porcelain")),
  audit_program=mgfp_hashes("application/scripts/130_audit_completed_mti_gibbs_boundary.R"),
  completed_source_commit=cfg$completed_commit,numerical=numerical,
  all_fits_tasks_and_sample_hashes_verified=TRUE,calibration_recomputed=TRUE,
  closeout_checks=additional,
  population_content_decisions_verified=TRUE,maximum_csv_cdf_roundtrip_error=max(errors),time=mgfp_time())
output <- file.path(cfg$evidence_root,"completed_audit.json")
mgfp_json_write(summary,output)
provenance_path <- file.path(cfg$evidence_root,"provenance.json")
provenance <- mwd_json(provenance_path)
for (input in names(provenance$inputs)) {
  mwd_require(mwd_file_hash(input) == provenance$inputs[[input]], "Completed-study input changed.")
  name <- if (basename(input) == "manifest.json") "completed_manifest.json" else basename(input)
  archived <- file.path(cfg$evidence_root,name)
  mwd_require(file.copy(input,archived,overwrite=TRUE), "Could not archive completed-study input.")
  provenance$artifacts[[archived]] <- mwd_file_hash(archived)
}
provenance$artifacts[[output]] <- mwd_file_hash(output)
for (stage in c("replay","confirmation")) {
  path <- file.path(cfg$evidence_root,paste0(stage,"_samples.csv"))
  provenance$artifacts[[path]] <- mwd_file_hash(path)
}
mgfp_json_write(provenance,provenance_path)
print(numerical)
