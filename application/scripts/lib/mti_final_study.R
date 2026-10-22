mpq_screen_reference <- function(run,spec,cfg,design) {
  mpr_require(!is.null(spec$screen_reference),"No external screen specification.")
  run <- normalizePath(run,mustWork=TRUE)
  root <- normalizePath(file.path(run,"..","..","..",".."),mustWork=TRUE)
  identity <- readRDS(file.path(run,"identity.rds"))
  old_path <- file.path(root,spec$screen_reference$config)
  mpr_require(identical(identity$commit,spec$screen_reference$commit) &&
    identical(mp_sha(old_path),spec$screen_reference$config_sha256),"Unexpected numerical screen identity.")
  for(records in list(identity$sources,identity$protected_inputs))for(i in seq_len(nrow(records)))
    mpr_require(identical(mp_sha(file.path(root,records$path[i])),records$sha256[i]),
      "Preserved screen inputs changed.")
  old <- jsonlite::read_json(old_path,simplifyVector=FALSE)
  for(field in c("primary_method","refinement_rule","threshold_rule","threshold_quantile_count",
    "baseline_commit","baseline_policy_sha256","screen"))
    mpr_require(identical(spec[[field]],old[[field]]),"Screen and final numerical specifications differ.")
  old_cfg <- jsonlite::read_json(file.path(root,old$revision_config),simplifyVector=FALSE)
  mpr_require(identical(cfg$profile,old_cfg$profile) &&
    identical(mp_sha(cfg$validation_config),mp_sha(file.path(root,old_cfg$validation_config))),
    "Numerical profile or distribution panel changed since the screen.")
  # Wrapper optimizations are tested separately; every packaged numerical input stays identical.
  numerical <- c(list.files("application/R",full.names=TRUE),
    list.files("application/src",pattern="[.](cpp|h)$|^Makevars",full.names=TRUE),
    "application/NAMESPACE","application/DESCRIPTION",cfg$historical_policy,
    "tables/tolerance_validation_article_scan_calibration.csv")
  for(path in numerical)mpr_require(identical(mp_sha(path),mp_sha(file.path(root,path))),
    "Numerical package, candidate menu, or scan anchor changed since the screen.")
  completed <- mpr_read_json(file.path(run,"screen","completion.json"))
  terminal <- mpr_read_json(file.path(run,"screen_exit.json"))
  mpr_require(!is.null(completed) && !is.null(terminal) && terminal$exit_status==0 &&
    isTRUE(completed$passed),"The referenced numerical screen is incomplete or unsuccessful.")
  rows <- read.csv(file.path(run,"screen","results.csv"))
  tasks <- mpq_tasks(design,cfg,"screen")
  expected_methods <- c(spec$primary_method,"mti_profile_fractional_admissible_v1")
  mpr_require(nrow(rows)==nrow(tasks)*length(expected_methods) &&
    setequal(unique(rows$method),expected_methods) &&
    !anyDuplicated(paste(mpr_task_key(rows),rows$method)),"Screen rows are incomplete or duplicated.")
  for(method in expected_methods) {
    s <- rows[rows$method==method,,drop=FALSE]
    index <- match(mpr_task_key(tasks),mpr_task_key(s))
    mpr_require(!anyNA(index),"Screen tasks are missing.")
    for(field in names(tasks))mpr_require(identical(s[[field]][index],tasks[[field]]),
      "Screen task, seed, or data role changed.")
  }
  assessment <- mpq_screen_assessment(rows,spec)
  for(field in names(assessment))mpr_require(isTRUE(all.equal(assessment[[field]],completed[[field]])),
    "Recorded screen assessment cannot be reproduced.")
  mpr_require(isTRUE(assessment$passed) && assessment$strictly_shorter_test_intervals>0,
    "The screen does not support the planned numerical refinement.")
  records <- pma_file_record(file.path(run,c("identity.rds","screen/completion.json",
    "screen/results.csv","screen/REPORT.md","screen_exit.json")))
  list(run=run,source_commit=identity$commit,records=records,assessment=assessment,
    package_inputs=pma_file_record(numerical))
}

mpq_visit_calibration <- function(tasks,cfg,path,visit,records=NULL) {
  files <- sort(list.files(file.path(path,"checkpoints"),"^batch_[0-9]+[.]rds$",full.names=TRUE))
  batch <- cfg$execution$checkpoint_batch
  expected <- ceiling(nrow(tasks)/batch)
  mpr_require(length(files)==expected && identical(basename(files),sprintf("batch_%05d.rds",seq_len(expected))),
    "Calibration checkpoints are incomplete or unexpected.")
  observed <- vector("list",length(files))
  for(b in seq_along(files)) {
    hash <- mp_sha(files[b])
    if(!is.null(records))mpr_require(identical(files[b],records$path[b]) &&
      identical(hash,records$sha256[b]),"Calibration checkpoint changed between assessment passes.")
    saved <- readRDS(files[b])
    indices <- seq.int((b-1L)*batch+1L,min(b*batch,nrow(tasks)))
    mpr_require(identical(saved$tasks,tasks[indices,,drop=FALSE]) && length(saved$values)==length(indices),
      "Calibration checkpoint task identity or result count changed.")
    for(i in seq_along(indices)) {
      value <- saved$values[[i]]
      mpr_require(is.list(value) && is.null(value$error),
        paste("Calibration task error retained in",files[b],"at",indices[i]))
      mpr_require(identical(as.list(value$identity),as.list(saved$tasks[i,,drop=FALSE])),
        "Calibration result identity changed.")
      visit(value)
    }
    mpr_require(identical(hash,mp_sha(files[b])),"Calibration checkpoint changed while reading.")
    observed[[b]] <- data.frame(path=files[b],sha256=hash,bytes=file.info(files[b])$size)
    pta_atomic_write_json(list(completed_batches=b,expected_batches=length(files),
      phase=if(is.null(records))"numerical_audit_and_threshold_development" else "threshold_assessment"),
      file.path(path,"assessment_progress.json"))
  }
  do.call(rbind,observed)
}

mpq_calibrate_checkpoints <- function(tasks,cfg,spec,path,context) {
  cells <- unique(tasks[c("n","content","tolerance_confidence")])
  families <- unique(tasks$dgp_id)
  cell_keys <- mpr_cell_key(cells)
  cases <- unique(tasks[c("cell_index","n","content","tolerance_confidence","dgp_id")])
  development <- setNames(vector("list",nrow(cells)),cell_keys)
  failures <- 0L
  records <- mpq_visit_calibration(tasks,cfg,path,function(value) {
    task <- value$identity; z <- value$candidates
    failures <<- failures+as.integer(mpr_check_candidates(z,cfg)>0)
    required <- c("candidate_index","target_content","mean_tilt","central_tilt","observed_count")
    mpr_require(all(required %in% names(z)) && !anyDuplicated(z$candidate_index) &&
      all(is.finite(z$target_content) & z$target_content>task$content & z$target_content<1) &&
      all(is.finite(z$mean_tilt) & is.finite(z$central_tilt)),"Candidate selection fields are invalid.")
    good <- z$profile_converged
    mpr_require(all(is.finite(z$observed_count[good]) & z$observed_count[good]>=0 &
      z$observed_count[good]<=task$n & z$observed_count[good]==as.integer(z$observed_count[good])),
      "Invalid candidate observed counts.")
    law <- context$laws[[task$dgp_id]]
    set.seed(task$seed)
    y <- sort(law$r(task$n))
    count <- findInterval(z$upper[good],y)-findInterval(z$lower[good],y,left.open=TRUE)
    mpr_require(all(count==z$observed_count[good]),
      "Candidate closed-interval counts disagree with independently regenerated samples.")
    truth <- law$p(z$upper[good])-law$p(z$lower[good])
    mpr_require(all(is.finite(truth) & abs(truth-z$true_content[good])<=1e-12),
      "Candidate population contents disagree with the generating CDF.")
    if(task$role=="threshold_development") {
      key <- mpr_cell_key(task)
      p <- z$posterior_content_probability
      development[[key]] <<- c(development[[key]],p[is.finite(p) & p>0 & p<1])
    }
  })
  pma_write_csv(records,file.path(path,"checkpoint_manifest.csv"))
  thresholds <- setNames(lapply(seq_len(nrow(cells)),function(i) {
    p <- development[[cell_keys[i]]]
    mpr_require(length(p)>0,"No independent development probabilities.")
    sort(unique(c(cells$tolerance_confidence[i],as.numeric(stats::quantile(p,
      seq(0,1,length.out=spec$threshold_quantile_count),type=8,names=FALSE)))))
  }),cell_keys)
  alpha <- (1-cfg$calibration$familywise_confidence)/
    (nrow(cells)*length(families)*(spec$threshold_quantile_count+1L))
  sums <- setNames(lapply(seq_len(nrow(cases)),function(i) {
    task <- cases[i,,drop=FALSE]
    t <- thresholds[[mpr_cell_key(task)]]
    data.frame(task[rep(1L,length(t)),c("n","content","tolerance_confidence","dgp_id"),drop=FALSE],
      threshold=t,replications=0L,returned=0L,successes=0L,width_sum=0)
  }),as.character(cases$cell_index))
  mpq_visit_calibration(tasks,cfg,path,function(value) {
    task <- value$identity
    if(task$role=="calibration_assessment") {
      key <- as.character(task$cell_index); current <- sums[[key]]
      decisions <- mpr_candidate_decisions(value$candidates,current$threshold,task$content)
      current$replications <- current$replications+1L
      for(field in names(decisions))current[[field]] <- current[[field]]+decisions[[field]]
      sums[[key]] <<- current
    }
  },records)
  policies <- assessments <- list()
  for(i in seq_len(nrow(cells))) {
    cell <- cells[i,,drop=FALSE]
    a <- do.call(rbind,lapply(families,function(family) {
      key <- cases$cell_index[mpr_cell_key(cases)==cell_keys[i] & cases$dgp_id==family]
      mpr_require(length(key)==1L,"No unique calibration distribution case.")
      s <- sums[[as.character(key)]]
      mpr_require(all(s$replications==cfg$calibration$replications),"Incomplete calibration assessment.")
      data.frame(s[setdiff(names(s),"width_sum")],content_attainment=s$successes/s$replications,
        lower_bound=vapply(s$successes,function(k)mp_binom_lower(k,cfg$calibration$replications,alpha),numeric(1)),
        mean_width=ifelse(s$returned>0,s$width_sum/pmax(1,s$returned),NA_real_),alpha=alpha)
    }))
    passing <- vapply(thresholds[[cell_keys[i]]],function(t) {
      s <- a[a$threshold==t,,drop=FALSE]
      all(s$returned==s$replications & s$lower_bound>=cell$tolerance_confidence)
    },logical(1))
    policies[[i]] <- data.frame(cell,threshold=if(any(passing))min(thresholds[[cell_keys[i]]][passing]) else NA_real_,
      method_id=spec$primary_method,decision=if(any(passing))"passed_new_calibration" else "unresolved")
    assessments[[i]] <- a
  }
  result <- mpq_write_calibration(do.call(rbind,policies),do.call(rbind,assessments),path)
  result$passed <- isTRUE(result$passed) && failures==0L
  result$numerical_failure_datasets <- failures
  result$checkpoint_manifest_sha256 <- mp_sha(file.path(path,"checkpoint_manifest.csv"))
  result
}

mpq_resource_check <- function(baseline_identity,cfg,spec,design,screen,workers) {
  requested <- spec$resource_requirements
  mpr_require(!is.null(requested) && is.finite(requested$disk_reserve_factor) &&
    requested$disk_reserve_factor>=1,"Missing final-study resource specification.")
  old_run <- baseline_identity$calibration_run
  old_identity <- readRDS(file.path(old_run,"identity.rds"))
  old_cfg <- jsonlite::read_json(spec$revision_config,simplifyVector=FALSE)
  per_case <- old_cfg$calibration$replications+old_cfg$calibration$threshold_development_replications
  old_batch <- old_cfg$execution$checkpoint_batch
  rows <- read.csv(file.path(screen$run,"screen","results.csv"))
  rows <- rows[rows$method==spec$primary_method,,drop=FALSE]
  estimates <- lapply(seq_len(nrow(design)),function(i) {
    b <- floor((i-1L)*per_case/old_batch)+1L
    file <- file.path(old_run,"calibration","checkpoints",sprintf("batch_%05d.rds",b))
    saved <- readRDS(file)
    mpr_require(nrow(saved$tasks)>0 && length(saved$values)==nrow(saved$tasks),"Invalid reference calibration batch.")
    original_count <- max(vapply(saved$values,function(v)nrow(v$candidates),integer(1)))
    s <- rows[match(design$cell_index[i],rows$cell_index),,drop=FALSE]
    ratio <- max(1,9*(s$original_q_count+s$added_q_count)/original_count)
    data.frame(cell_index=design$cell_index[i],reference_bytes_per_dataset=file.info(file)$size/nrow(saved$tasks),
      numerical_growth_factor=ratio)
  })
  estimates <- do.call(rbind,estimates)
  projected_disk <- sum(estimates$reference_bytes_per_dataset*estimates$numerical_growth_factor)*
    (cfg$calibration$replications+cfg$calibration$threshold_development_replications)
  disk <- read.table(text=paste(system2("df",c("-Pk","."),stdout=TRUE),collapse="\n"),
    header=FALSE,skip=1,col.names=c("fs","blocks","used","available","capacity","mount"))
  available_disk <- disk$available[1]*1024
  mem <- readLines("/proc/meminfo")
  available_memory <- as.numeric(strsplit(trimws(sub("^MemAvailable:","",mem[startsWith(mem,"MemAvailable:")])),"[[:space:]]+")[[1]][1])*1024
  cpu <- parallel::detectCores(logical=TRUE)
  load <- as.numeric(strsplit(readLines("/proc/loadavg")," ")[[1]][1])
  mpr_require(is.finite(available_disk) && projected_disk*requested$disk_reserve_factor<available_disk,
    "Insufficient projected disk headroom for the final calibration and retained evidence.")
  mpr_require(is.finite(available_memory) && available_memory/1024^3>=requested$minimum_available_memory_gib,
    "Insufficient available memory for bounded-batch calibration.")
  mpr_require(workers<=cpu-ceiling(load) && cfg$execution$checkpoint_batch>=workers,
    "Requested workers exceed current logical-CPU headroom or checkpoint batch size.")
  list(checked_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),workers=workers,
    threads_per_worker=1,logical_cpus=cpu,load_1minute=load,
    available_memory_gib=available_memory/1024^3,available_disk_gib=available_disk/1024^3,
    estimated_calibration_disk_gib=projected_disk/1024^3,
    disk_reserve_factor=requested$disk_reserve_factor,bounded_batch_assessment=TRUE,
    reference_source_commit=old_identity$commit,reference_estimates=estimates)
}

mpq_final_run <- function(output,cfg,spec,design,workers,guard) {
  mpr_require(isTRUE(spec$final_study$validation_after_passing_calibration) &&
    !isTRUE(spec$final_study$automatic_adoption),"Unsupported final-study controls.")
  receipt <- file.path(output,"final_study_completion.json")
  calibration <- mpq_stage("calibration",output,cfg,spec,design,workers)
  guard()
  if(!isTRUE(calibration$passed)) {
    status <- list(decision="stopped_unresolved_calibration",calibration=calibration,
      validation_started=FALSE,article_inputs_replaced=FALSE)
  } else {
    validation <- mpq_stage("validation",output,cfg,spec,design,workers)
    guard()
    review <- mpq_export_review(output,file.path(output,"review"),cfg,spec,design)
    guard()
    status <- list(decision="complete_requires_scientific_adoption_review",calibration=calibration,
      validation=validation,validation_started=TRUE,review=review,article_inputs_replaced=FALSE)
  }
  pta_atomic_write_json(status,receipt)
  status
}
