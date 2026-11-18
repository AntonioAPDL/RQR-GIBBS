# Reuse only the interrupted prior-specific mode stage, with its original receipts.
mjr_function <- function(path, name) {
  expressions <- parse(path, keep.source = FALSE)
  hit <- Filter(function(x) is.call(x) && as.character(x[[1]]) %in% c("<-", "=") &&
    identical(x[[2]], as.name(name)), as.list(expressions))
  mwd_require(length(hit) == 1L, paste("Missing or ambiguous computation:", name))
  mwd_hash(hit[[1]][[3]])
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

mjs_import_modes <- function(tasks, cfg, identity, context) {
  origin <- identity$mode_reuse
  if (is.null(origin)) return(invisible(0L))
  source <- origin$output_root
  mwd_require(origin$manifest_sha256 == mwd_file_hash(file.path(source, "manifest.json")), "Mode origin manifest changed.")
  old <- mwd_json(file.path(source, "manifest.json"))$identity
  mwd_require(origin$source_commit == old$source_commit &&
    identical(old$source_commit, trimws(system2("git", c("-C", shQuote(origin$checkout), "rev-parse", "HEAD"), stdout = TRUE))),
    "Mode origin checkout differs.")
  current_checkout <- if (file.exists("application/scripts/lib/mti_gibbs_joint.R")) "." else "../../.."
  hashes <- mjr_check_source(old, identity, origin$checkout, current_checkout)
  old_stage <- mwd_json(file.path(source, "modes/manifest.json"))
  mwd_require(identical(old_stage$analysis_id, mwd_hash(list(old, "modes", tasks))), "Mode origin tasks changed.")
  # This transition is deliberately limited to a source stopped before Gibbs development.
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
