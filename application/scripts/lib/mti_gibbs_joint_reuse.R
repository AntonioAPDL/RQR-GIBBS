# Preserve original receipts when recovering an explicitly reviewed stopped stage.
mjr_function <- function(path, name, sampler_alias = FALSE) {
  expressions <- parse(path, keep.source = FALSE)
  hit <- Filter(function(x) is.call(x) && as.character(x[[1]]) %in% c("<-", "=") &&
    identical(x[[2]], as.name(name)), as.list(expressions))
  mwd_require(length(hit) == 1L, paste("Missing or ambiguous computation:", name))
  value <- hit[[1]][[3]]
  if (sampler_alias) value <- do.call(substitute,
    list(value, list(mjr_sample_checked = as.name("mjc_sample"))))
  mwd_hash(value)
}

mjr_check_source <- function(old, current, old_checkout, current_checkout) {
  for (key in c("package", "R", "rng", "generators", "seed_audit", "dependency", "exdqlm_reference_commit"))
    mwd_require(identical(old[[key]], current[[key]]), paste("Mode reuse inputs differ:", key))
  fields <- c("root_sd", "independent_coordinates", "learning_rate", "sampling", "seeds",
    "development_replications", "expected_cells", "predecessor_config")
  for (key in fields) mwd_require(identical(old$configuration[[key]], current$configuration[[key]]),
    paste("Mode reuse settings differ:", key))
  editable <- c("application/scripts/133_assess_mti_gibbs_joint.R", "application/scripts/134_launch_mti_gibbs_joint.sh",
    "application/config/mti_gibbs_joint_20261117.json", "application/scripts/lib/mti_gibbs_joint.R",
    "application/scripts/lib/mti_gibbs_joint_study.R", "application/scripts/lib/mti_gibbs_joint_reuse.R")
  for (path in setdiff(names(old$programs), editable))
    mwd_require(identical(old$programs[[path]], current$programs[[path]]) &&
      identical(old$programs[[path]], mwd_file_hash(file.path(old_checkout, path))),
      paste("Mode reuse dependency differs:", path))
  functions <- list(mti_gibbs_joint = c("mjc_fit_anchor"),
    mti_gibbs_joint_study = c("mjs_modes", "mjs_old"))
  hashes <- list()
  for (module in names(functions)) for (name in functions[[module]]) {
    path <- file.path("application/scripts/lib", paste0(module, ".R"))
    a <- mjr_function(file.path(old_checkout, path), name)
    b <- mjr_function(file.path(current_checkout, path), name)
    mwd_require(identical(a, b), paste("Mode fitting computation changed:", name))
    hashes[[name]] <- b
  }
  hashes
}

mjr_development_source <- function(identity) {
  origin <- identity$mode_reuse
  !is.null(origin) && identical(origin$stage, "development")
}

mjr_check_development <- function(old, current, tasks, selection = NULL) {
  origin <- current$mode_reuse; cfg <- current$configuration
  mwd_require(mjr_development_source(current) &&
    identical(old$source_commit, cfg$development_recovery$source_commit),
    "This development recovery has not been reviewed for the requested source.")
  for (key in setdiff(names(old$configuration), "output_root"))
    mwd_require(identical(old$configuration[[key]], cfg[[key]]),
      paste("Development recovery settings differ:", key))
  summary <- mwd_json(file.path(origin$output_root, "study_summary.json"))
  exit <- mwd_json(file.path(origin$output_root, "controller_exit.json"))
  mwd_require(summary$status == "stopped_requires_review" && summary$stage == "development" &&
    exit$exit_code == 1 && length(Sys.glob(file.path(origin$output_root, "development/fits/*.rds.json"))) ==
      cfg$development_recovery$saved_datasets, "The reviewed development stop changed.")
  for (path in names(origin$artifacts)) mwd_require(
    identical(origin$artifacts[[path]], mwd_file_hash(file.path(origin$output_root, path))),
    paste("Recovery origin changed:", path))
  modules <- list(mti_gibbs_joint = c("mjc_models", "mjc_probabilities", "mjc_reweight", "mjc_regions",
    "mjc_curve", "mjc_order", "mjc_nominate", "mjc_select", "mjc_sampler_hashes", "mjc_target_key",
    "mjc_sample", "mjc_interval", "mjc_reference_radius", "mjc_scores"),
    mti_gibbs_joint_study = c("mjs_designs", "mjs_seed_audit", "mjs_replay", "mjs_gibbs",
      "mjs_refinement", "mjs_freeze", "mjs_validation_domain", "mjs_final_job"))
  current_checkout <- if (file.exists("application/scripts/lib/mti_gibbs_joint.R")) "." else "../../.."
  for (module in names(modules)) for (name in modules[[module]]) {
    path <- file.path("application/scripts/lib", paste0(module, ".R"))
    mwd_require(identical(mjr_function(file.path(origin$checkout, path), name, TRUE),
      mjr_function(file.path(current_checkout, path), name, TRUE)), paste("Development computation changed:", name))
  }
  stage <- mwd_json(file.path(origin$output_root, "development/manifest.json"))
  mwd_require(identical(stage$identity[[1]], old) &&
    old$seed_audit$tasks_sha256$development == mwd_hash(tasks), "The original development tasks differ.")
  if (!is.null(selection)) mwd_require(
    stage$analysis_id == mwd_hash(list(list(old, selection), "development", tasks)),
    "The original full-precision selection identity does not reproduce.")
  invisible(TRUE)
}

mjs_import_modes <- function(tasks, cfg, identity, context) {
  origin <- identity$mode_reuse
  if (is.null(origin)) return(invisible(0L))
  source <- origin$output_root
  mwd_require(origin$manifest_sha256 == mwd_file_hash(file.path(source, "manifest.json")), "Mode origin manifest changed.")
  old <- mwd_json(file.path(source, "manifest.json"))$identity
  mwd_require(origin$source_commit == old$source_commit &&
    identical(old$source_commit, trimws(system2("git", c("--no-optional-locks", "-C", shQuote(origin$checkout), "rev-parse", "HEAD"), stdout = TRUE))),
    "Mode origin checkout differs.")
  current_checkout <- if (file.exists("application/scripts/lib/mti_gibbs_joint.R")) "." else "../../.."
  hashes <- mjr_check_source(old, identity, origin$checkout, current_checkout)
  old_stage <- mwd_json(file.path(source, "modes/manifest.json"))
  mwd_require(identical(old_stage$analysis_id, mwd_hash(list(old, "modes", tasks))), "Mode origin tasks changed.")
  if (mjr_development_source(identity)) mjr_check_development(old, identity, tasks) else
    mwd_require(!length(Sys.glob(file.path(source, "development/fits/*.json"))),
      "Gibbs development exists; mode-only reuse needs a separate review.")
  directory <- file.path(cfg$output_root, "modes")
  dir.create(file.path(directory, "fits"), recursive = TRUE, showWarnings = FALSE)
  id <- mwd_hash(list(identity, "modes", tasks))
  imports <- list()
  for (i in seq_len(nrow(tasks))) {
    task <- tasks[i, ]; name <- sprintf("fit_%06d.rds", task$job_id)
    from <- file.path(source, "modes/fits", name); to <- file.path(directory, "fits", name)
    if (!file.exists(paste0(from, ".json"))) next
    value <- mbc_read_fit(from, old_stage$analysis_id, task)
    mwd_require(isTRUE(value$ok) && identical(value$task, task), "A source mode fit failed or has different inputs.")
    set.seed(task$data_seed); sample_hash <- mwd_hash(context$laws[[task$response_id]]$r(task$n))
    for (sd in cfg$root_sd) {
      a <- value$anchors[[as.character(sd)]]
      mwd_require(identical(a$sample_sha256, sample_hash) && a$prior$prior_sd == sd &&
        all(a$candidates$finite_interval & a$candidates$numerically_accurate), "Source mode observations or prior differ.")
    }
    receipt_hash <- mwd_file_hash(paste0(from, ".json"))
    from_hash <- mwd_file_hash(from)
    if (file.exists(paste0(to, ".json"))) {
      mbc_read_fit(to, id, task)
      mwd_require(identical(from_hash, mwd_file_hash(to)), "An imported mode fit changed.")
    } else {
      mwd_require(!file.exists(to), "An incomplete imported mode requires review.")
      temporary <- tempfile(".mode-import-", tmpdir = dirname(to))
      mwd_require(file.copy(from, temporary) && mwd_file_hash(temporary) == from_hash &&
        file.rename(temporary, to), "Cannot preserve the source mode fit.")
      mgfp_json_write(list(identity = id, task_sha256 = mwd_hash(task), sha256 = from_hash,
        finished = mgfp_time(), origin = list(source_commit = old$source_commit,
          analysis_id = old_stage$analysis_id, receipt_sha256 = receipt_hash, file = from)), paste0(to, ".json"))
    }
    imports[[length(imports) + 1L]] <- list(job_id = task$job_id, sha256 = from_hash, receipt_sha256 = receipt_hash)
  }
  mgfp_json_write(list(origin = origin, computations = hashes, imported = length(imports),
    fits = imports, time = mgfp_time()), file.path(directory, "reuse_receipt.json"))
  cat("Verified mode fits reused:", length(imports), "\n")
  invisible(length(imports))
}

mjr_copy <- function(from, to, id, task, origin, value = NULL, recovered = FALSE) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(paste0(to, ".json"))) {
    saved <- mbc_read_fit(to, id, task)
    mwd_require(isTRUE(saved$ok), "An imported numerical failure requires review.")
    mwd_require(identical(mwd_json(paste0(to, ".json"))$origin, origin), "An imported origin changed.")
    if (is.null(value) && !recovered) mwd_require(mwd_file_hash(from) == mwd_file_hash(to), "An imported fit changed.")
    return(saved)
  }
  mwd_require(!file.exists(to), "An incomplete recovery write requires review.")
  if (is.null(value)) {
    tmp <- tempfile(".development-import-", tmpdir = dirname(to))
    on.exit(unlink(tmp))
    mwd_require(file.copy(from, tmp) && mwd_file_hash(tmp) == mwd_file_hash(from) &&
      file.rename(tmp, to), "Cannot preserve the original development fit.")
  } else mbc_atomic_fit(value, to, id, task)
  mgfp_json_write(list(identity = id, task_sha256 = mwd_hash(task), sha256 = mwd_file_hash(to),
    finished = mgfp_time(), origin = origin), paste0(to, ".json"))
  if (is.null(value)) readRDS(to) else value
}

mjr_check_result <- function(value, task, anchors, selection, cfg, context) {
  mwd_require(identical(value$task, task) && nrow(value$rows) > 0, "Recovered task differs.")
  set.seed(task$data_seed); y <- context$laws[[task$response_id]]$r(task$n)
  variant <- context$old_tilts$variant[context$old_tilts$n == task$n & context$old_tilts$content == task$content]
  rules <- as.data.frame(selection[selection$n == task$n & selection$content == task$content, ])
  mwd_require(setequal(value$rows$rule, rules$rule) && !anyDuplicated(value$rows$rule) &&
    all(value$rows$returned) && all(value$rows$sample_sha256 == mwd_hash(y)),
    "Recovered observations or rules differ.")
  keys <- character()
  for (j in seq_len(nrow(rules))) {
    rule <- rules[j, ]; row_index <- which(value$rows$rule == rule$rule)
    row <- value$rows[row_index, ]
    target <- mjc_select(y, anchors[[as.character(rule$prior_sd)]], rule, task$content, variant)
    mwd_require(!is.null(target), "An original posterior target is missing.")
    key <- mjc_target_key(y, target, rule$prior_sd, cfg, context$policy_diagnostics)
    r <- value$records[[key]]
    mwd_require(!is.null(r) && row$target_key == key && row$q == target$q && row$delta == target$geometry$delta &&
      row$prior_sd == rule$prior_sd && row$gamma == rule$gamma && row$concentration == rule$concentration &&
      row$base_sd == rule$base_sd && row$selected_candidate == target$selected$candidate_index &&
      row$ecm_lower == target$selected$lower && row$ecm_upper == target$selected$upper &&
      all(r$parameters$q == target$q) && all(r$parameters$delta == target$geometry$delta) &&
      identical(isTRUE(r$adequate), all(r$parameters$preferred_pass & r$parameters$mean_precision)) &&
      identical(isTRUE(row$adequate), isTRUE(r$adequate)) &&
      all(mjc_interval(r) == c(row$lower, row$upper)), "An imported posterior target or interval changed.")
    keys <- c(keys, key)
  }
  mwd_require(setequal(keys, names(value$records)) && identical(isTRUE(value$ok),
    all(vapply(value$records, function(r) isTRUE(r$adequate), FALSE))), "Unexpected imported posterior records or status.")
  invisible(y)
}

mjr_check_recovered <- function(before, after) {
  mwd_require(isTRUE(after$ok) && identical(before$task, after$task) &&
    identical(names(before$records), names(after$records)), "The recovered dataset is inconsistent.")
  for (key in names(before$records)) {
    a <- before$records[[key]]; b <- after$records[[key]]
    if (isTRUE(a$adequate)) mwd_require(identical(mwd_hash(a), mwd_hash(b)), "An adequate target was replaced.") else {
      mwd_require(mjp_precision_only(a) && isTRUE(b$adequate) &&
        all(b$parameters$preferred_pass & b$parameters$mean_precision) &&
        identical(a[c("prior_sd", "q", "delta", "seed")], b[c("prior_sd", "q", "delta", "seed")]) &&
        identical(b$precision_extension$original_record_sha256, mwd_hash(a)), "Invalid precision recovery.")
      mff_prefix_check(a$fits, b$fits)
    }
  }
  fixed <- c("rule", "prior_sd", "concentration", "base_sd", "gamma", "q", "delta", "original_delta",
    "tilt_variant", "original_margin", "selected_candidate", "ecm_lower", "ecm_upper", "ecm_dp_probability",
    "sample_sha256", "target_key", "radius_multiplier")
  mwd_require(isTRUE(all.equal(as.data.frame(before$rows)[fixed], as.data.frame(after$rows)[fixed],
    tolerance = 0, check.attributes = FALSE)), "Recovery changed selected targets or ECM endpoints.")
  invisible(TRUE)
}

mjs_import_development <- function(tasks, selection, modes, cfg, identity, context) {
  if (!mjr_development_source(identity)) return(invisible(0L))
  origin <- identity$mode_reuse; source <- origin$output_root
  old <- mwd_json(file.path(source, "manifest.json"))$identity
  # Replay exact mode objects; CSV and JSON alone lose original R column types.
  mjr_check_development(old, identity, tasks, selection)
  stage <- mwd_json(file.path(source, "development/manifest.json"))
  for (path in c("initial_selection.csv", "ecm_threshold_comparison.csv", "candidate_profile_review.json"))
    mwd_require(identical(mwd_file_hash(file.path(source, path)), mwd_file_hash(file.path(cfg$output_root, path))),
      paste("The frozen analytical comparison changed:", path))
  id <- mwd_hash(list(list(identity, selection), "development", tasks))
  directory <- file.path(cfg$output_root, "development")
  imported <- list(); failed <- integer()
  for (i in seq_len(nrow(tasks))) {
    task <- tasks[i, ]; name <- sprintf("fit_%06d.rds", task$job_id)
    from <- file.path(source, "development/fits", name); to <- file.path(directory, "fits", name)
    if (!file.exists(paste0(from, ".json"))) next
    value <- mbc_read_fit(from, stage$analysis_id, task)
    anchors <- mjs_read(modes, tasks, i)$anchors
    y <- mjr_check_result(value, task, anchors, selection, cfg, context)
    receipt <- list(source_commit = old$source_commit, analysis_id = stage$analysis_id,
      file = from, sha256 = mwd_file_hash(from), receipt_sha256 = mwd_file_hash(paste0(from, ".json")))
    if (!isTRUE(value$ok)) {
      failed <- c(failed, task$job_id)
      mwd_require(task$job_id == cfg$development_recovery$precision_only_job, "Unexpected original failed job.")
      archive <- file.path(directory, "original_failures", name)
      dir.create(dirname(archive), recursive = TRUE, showWarnings = FALSE)
      for (suffix in c("", ".json")) {
        a <- paste0(from, suffix); b <- paste0(archive, suffix)
        if (!file.exists(b)) mwd_require(file.copy(a, b), "Cannot archive original failure.")
        mwd_require(mwd_file_hash(a) == mwd_file_hash(b), "Original failure archive changed.")
      }
      if (file.exists(paste0(to, ".json"))) recovered <- mjr_copy(from, to, id, task, receipt, recovered = TRUE) else {
        previous <- value
        for (key in names(previous$records)) if (!isTRUE(previous$records[[key]]$adequate)) {
          r <- previous$records[[key]]
          row <- value$rows[value$rows$target_key == key, ][1, ]
          rule <- as.data.frame(selection[selection$n == task$n & selection$content == task$content & selection$rule == row$rule, ])
          target <- mjc_select(y, anchors[[as.character(rule$prior_sd)]], rule, task$content, row$tilt_variant)
          mwd_require(r$seed == as.integer(task$chain_seed + 10 * rule$slot), "Original chain seed differs.")
          previous$records[[key]] <- mjp_extend(r, y, target, rule$prior_sd, cfg, context$policy_diagnostics, r$seed)
        }
        recovered <- mjs_gibbs(task, anchors, selection, cfg, context, previous = previous)
        recovered$elapsed_seconds <- value$elapsed_seconds
        recovered$recovered_original_sha256 <- receipt$sha256
        mjr_check_recovered(value, recovered)
        recovered <- mjr_copy(from, to, id, task, receipt, recovered)
      }
      mjr_check_recovered(value, recovered)
      mjr_check_result(recovered, task, anchors, selection, cfg, context)
    } else mjr_copy(from, to, id, task, receipt)
    imported[[length(imported) + 1L]] <- receipt
  }
  mwd_require(length(imported) == cfg$development_recovery$saved_datasets &&
    identical(failed, cfg$development_recovery$precision_only_job) &&
    length(imported) - length(failed) == cfg$development_recovery$successful_datasets,
    "Development recovery counts differ from the reviewed stop.")
  mgfp_json_write(list(origin = origin, imported = length(imported),
    successful_originals = length(imported) - length(failed), precision_recovered_jobs = failed,
    initial_selection_sha256 = mwd_hash(selection), fits = imported, time = mgfp_time()),
    file.path(directory, "reuse_receipt.json"))
  cat("Verified development fits recovered:", length(imported), "\n")
  invisible(length(imported))
}
