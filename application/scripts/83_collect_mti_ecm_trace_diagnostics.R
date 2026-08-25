#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
flag_value <- function(prefix, default = FALSE) {
  value <- arg_value(prefix, NA_character_)
  if (is.na(value)) return(default)
  tolower(value) %in% c("1", "true", "yes", "y")
}
`%||%` <- function(x, y) if (is.null(x)) y else x
stopf <- function(...) stop(paste0(...), call. = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
as_flag <- function(x) {
  if (is.logical(x)) return(x)
  value <- tolower(as.character(x))
  value %in% c("true", "t", "1", "yes", "y")
}

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/83_collect_mti_ecm_trace_diagnostics.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

required_packages <- c("jsonlite", "digest")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stopf("Package '", pkg, "' is required.")
  }
}
if (flag_value("--load-source=", TRUE)) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stopf("Package 'pkgload' is required when --load-source=true.")
  }
  pkgload::load_all("application", quiet = TRUE)
} else {
  library(rqrgibbs)
}

timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
config_path <- arg_value(
  "--config=",
  "application/config/rqr_bayes_uq_validation_mti_ecm_adaptive_selected_20260825.json"
)
results_path <- arg_value(
  "--results-csv=",
  "application/runs/rqr_bayes_uq_validation_mti_ecm_adaptive_selected_20260825/wave_confirmatory_20260825T072935Z/bayes_uq_validation_results.csv"
)
policy_path <- arg_value(
  "--policy-csv=",
  "application/config/mti_ecm_adaptive_cell_policy_20260825.csv"
)
output_dir <- arg_value(
  "--output-dir=",
  file.path("application/outputs/mti_ecm_trace_diagnostics",
            paste0("trace_current_winner_", timestamp))
)
mode <- arg_value("--mode=", "confirmatory")
method_id <- arg_value("--method-id=", "mti_ecm_adaptive_cell")
replications_per_cell <- as.integer(arg_value("--replications-per-cell=", "3"))
workers <- as.integer(arg_value("--workers=", "8"))
if (!is.finite(replications_per_cell) || replications_per_cell < 1L) {
  stopf("--replications-per-cell must be a positive integer.")
}
if (!is.finite(workers) || workers < 1L) workers <- 1L
available_cores <- parallel::detectCores(logical = FALSE)
if (!is.finite(available_cores) || available_cores < 1L) available_cores <- 1L
workers <- min(workers, as.integer(available_cores))

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

config <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
results <- utils::read.csv(results_path, stringsAsFactors = FALSE,
                           check.names = FALSE)
policy <- utils::read.csv(policy_path, stringsAsFactors = FALSE,
                          check.names = FALSE)
results$.row_id <- seq_len(nrow(results))
results <- results[results$method_id == method_id & results$mode == mode,
                   , drop = FALSE]
if (!nrow(results)) {
  stopf("No validation rows found for method '", method_id, "' and mode '",
        mode, "'.")
}

hash_to_seed <- function(text, base = 862100L) {
  bytes <- as.integer(charToRaw(as.character(text)))
  value <- as.integer(base)
  for (byte in bytes) {
    value <- as.integer((as.double(value) * 131 + byte) %% 2147483647)
  }
  if (value <= 0L) value <- value + 1L
  value
}

standardized_asymmetric_laplace_meta <- function(dgp) {
  tau <- num(dgp$tau %||% dgp$p %||% dgp$p0 %||% 0.10)[1L]
  scale <- num(dgp$scale %||% 1)[1L]
  raw_mean <- scale * (1 - 2 * tau) / (tau * (1 - tau))
  raw_variance <- scale^2 *
    (1 - 2 * tau + 2 * tau^2) / (tau^2 * (1 - tau)^2)
  raw_sd <- sqrt(raw_variance)
  q_raw <- function(p) {
    ifelse(
      p < tau,
      scale * log(p / tau) / (1 - tau),
      -scale * log((1 - p) / (1 - tau)) / tau
    )
  }
  list(
    r = function(n) (q_raw(stats::runif(n)) - raw_mean) / raw_sd,
    p = function(x) {
      z <- raw_mean + raw_sd * as.numeric(x)
      ifelse(
        z < 0,
        tau * exp((1 - tau) * z / scale),
        1 - (1 - tau) * exp(-tau * z / scale)
      )
    }
  )
}

standardized_two_piece_normal_meta <- function(dgp) {
  left_scale <- num(
    dgp$left_scale %||% dgp$scale_left %||%
      dgp$sigma_left %||% dgp$left_sd %||% 1
  )[1L]
  right_scale <- num(
    dgp$right_scale %||% dgp$scale_right %||%
      dgp$sigma_right %||% dgp$right_sd %||% 12
  )[1L]
  raw_mean <- sqrt(2 / pi) * (right_scale - left_scale)
  raw_second <- left_scale^2 - left_scale * right_scale + right_scale^2
  raw_sd <- sqrt(raw_second - raw_mean^2)
  threshold <- left_scale / (left_scale + right_scale)
  q_raw <- function(p) {
    p <- as.numeric(p)
    out <- rep(NaN, length(p))
    valid <- !is.na(p) & p >= 0 & p <= 1
    out[is.na(p)] <- NA_real_
    out[valid & p == 0] <- -Inf
    out[valid & p == 1] <- Inf
    left <- valid & p > 0 & p < threshold
    right <- valid & p >= threshold & p < 1
    out[left] <- left_scale * stats::qnorm(
      p[left] * (left_scale + right_scale) / (2 * left_scale)
    )
    out[right] <- right_scale * stats::qnorm(
      (p[right] * (left_scale + right_scale) -
         left_scale + right_scale) / (2 * right_scale)
    )
    out
  }
  list(
    r = function(n) (q_raw(stats::runif(n)) - raw_mean) / raw_sd,
    p = function(x) {
      z <- raw_mean + raw_sd * as.numeric(x)
      ifelse(
        z < 0,
        2 * left_scale / (left_scale + right_scale) *
          stats::pnorm(z / left_scale),
        (left_scale - right_scale) / (left_scale + right_scale) +
          2 * right_scale / (left_scale + right_scale) *
          stats::pnorm(z / right_scale)
      )
    }
  )
}

dgp_meta <- function(dgp) {
  family <- as.character(dgp$family)[1L]
  if (identical(family, "normal")) {
    return(list(r = function(n) stats::rnorm(n), p = stats::pnorm))
  }
  if (identical(family, "standardized_student_t")) {
    df <- num(dgp$df %||% 3)[1L]
    sd_raw <- sqrt(df / (df - 2))
    return(list(
      r = function(n) stats::rt(n, df = df) / sd_raw,
      p = function(x) stats::pt(x * sd_raw, df = df)
    ))
  }
  if (identical(family, "centered_exponential") ||
      identical(family, "standardized_exponential")) {
    rate <- num(dgp$rate %||% 1)[1L]
    return(list(
      r = function(n) rate * stats::rexp(n, rate = rate) - 1,
      p = function(x) stats::pexp((x + 1) / rate, rate = rate)
    ))
  }
  if (identical(family, "standardized_asymmetric_laplace")) {
    return(standardized_asymmetric_laplace_meta(dgp))
  }
  if (identical(family, "standardized_two_piece_normal")) {
    return(standardized_two_piece_normal_meta(dgp))
  }
  if (identical(family, "standardized_beta")) {
    shape1 <- num(dgp$shape1 %||% dgp$a %||% 2)[1L]
    shape2 <- num(dgp$shape2 %||% dgp$b %||% 5)[1L]
    mean_raw <- shape1 / (shape1 + shape2)
    sd_raw <- sqrt(
      shape1 * shape2 /
        ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
    )
    return(list(
      r = function(n) {
        (stats::rbeta(n, shape1 = shape1, shape2 = shape2) - mean_raw) /
          sd_raw
      },
      p = function(x) {
        stats::pbeta(x * sd_raw + mean_raw, shape1 = shape1, shape2 = shape2)
      }
    ))
  }
  if (identical(family, "standardized_gamma")) {
    shape <- num(dgp$shape %||% 2)[1L]
    scale <- num(dgp$scale %||% 1)[1L]
    mean_raw <- shape * scale
    sd_raw <- sqrt(shape) * scale
    return(list(
      r = function(n) {
        (stats::rgamma(n, shape = shape, scale = scale) - mean_raw) / sd_raw
      },
      p = function(x) {
        stats::pgamma(x * sd_raw + mean_raw, shape = shape, scale = scale)
      }
    ))
  }
  if (identical(family, "standardized_lognormal")) {
    logsd <- num(dgp$logsd %||% 0.75)[1L]
    mean_raw <- exp(logsd^2 / 2)
    sd_raw <- sqrt((exp(logsd^2) - 1) * exp(logsd^2))
    return(list(
      r = function(n) (stats::rlnorm(n, 0, logsd) - mean_raw) / sd_raw,
      p = function(x) stats::plnorm(x * sd_raw + mean_raw, 0, logsd)
    ))
  }
  stopf("Unsupported DGP family: ", family)
}

dgp_by_id <- stats::setNames(config$dgps, vapply(
  config$dgps, function(x) as.character(x$dgp_id)[1L], character(1L)
))
profile_cfg <- config$engine_defaults$mti_ecm_dp_profile
dp_cfg <- config$engine_defaults$direct_dp
base_cfg <- dp_cfg$base
if (!identical(as.character(base_cfg$family)[1L], "normal")) {
  stopf("Only the normal direct-DP base is supported by this diagnostic.")
}
dp_base <- rqr_dp_base_normal(mean = num(base_cfg$mean %||% 0)[1L],
                              sd = num(base_cfg$sd %||% 4)[1L])
base_seed <- as.integer(config$base_seed %||% 981250L)

policy_rule_for <- function(n, content, tolerance_confidence) {
  hit <- policy[
    as.integer(policy$n) == as.integer(n) &
      abs(num(policy$content) - num(content)[1L]) < 1e-12 &
      abs(num(policy$tolerance_confidence) -
            num(tolerance_confidence)[1L]) < 1e-12,
    ,
    drop = FALSE
  ]
  if (!nrow(hit)) {
    stopf("No MTI-ECM policy row for n=", n, ", content=", content,
          ", tolerance_confidence=", tolerance_confidence, ".")
  }
  as.list(hit[1L, , drop = FALSE])
}

cell_key <- function(dgp_id, n, content, tolerance_confidence,
                     posterior_confidence) {
  paste(dgp_id, n, sprintf("%.12g", num(content)[1L]),
        sprintf("%.12g", num(tolerance_confidence)[1L]),
        sprintf("%.12g", num(posterior_confidence)[1L]), sep = "|")
}

select_jobs <- function(x, reps_per_cell) {
  x$.cell_key <- mapply(
    cell_key, x$dgp_id, x$n, x$guaranteed_content,
    x$tolerance_confidence, x$posterior_confidence,
    USE.NAMES = FALSE
  )
  selected <- list()
  for (key in unique(x$.cell_key)) {
    block <- x[x$.cell_key == key, , drop = FALSE]
    block <- block[order(block$replication), , drop = FALSE]
    picks <- integer()
    reasons <- character()
    add_pick <- function(index, reason) {
      if (!length(index) || is.na(index) || index < 1L) return(invisible())
      row_id <- block$.row_id[[index]]
      if (!row_id %in% picks) {
        picks <<- c(picks, row_id)
        reasons <<- c(reasons, reason)
      }
      invisible()
    }
    add_pick(1L, "first_replication")
    finite_width <- is.finite(num(block$width))
    if (any(finite_width)) {
      med_width <- stats::median(num(block$width)[finite_width])
      add_pick(which.min(abs(num(block$width) - med_width)),
               "median_width")
      add_pick(which.max(num(block$width)), "largest_width")
    }
    finite_stationarity <- is.finite(num(block$ecm_final_stationarity))
    if (any(finite_stationarity)) {
      values <- num(block$ecm_final_stationarity)
      values[!finite_stationarity] <- -Inf
      add_pick(which.max(values), "largest_stationarity")
    }
    failed <- which(!as_flag(block$success))
    if (length(failed)) add_pick(failed[[1L]], "first_failed_delivery")
    nonconverged <- which(!as_flag(block$ecm_converged))
    if (length(nonconverged)) add_pick(nonconverged[[1L]],
                                       "first_nonconverged")
    if (length(picks) < reps_per_cell) {
      fill <- setdiff(block$.row_id, picks)
      needed <- reps_per_cell - length(picks)
      picks <- c(picks, head(fill, needed))
      reasons <- c(reasons, rep("deterministic_fill", min(length(fill), needed)))
    }
    pick_df <- x[match(head(picks, reps_per_cell), x$.row_id), , drop = FALSE]
    pick_df$selection_reason <- head(reasons, nrow(pick_df))
    selected[[length(selected) + 1L]] <- pick_df
  }
  out <- do.call(rbind, selected)
  out <- out[order(out$dgp_id, out$n, out$guaranteed_content,
                   out$tolerance_confidence, out$posterior_confidence,
                   out$replication), , drop = FALSE]
  out$job_id <- sprintf("mti_ecm_trace_%04d", seq_len(nrow(out)))
  rownames(out) <- NULL
  out
}

jobs <- select_jobs(results, replications_per_cell)
message(sprintf(
  "Selected %d diagnostic jobs across %d validation cells.",
  nrow(jobs),
  length(unique(mapply(cell_key, jobs$dgp_id, jobs$n,
                       jobs$guaranteed_content, jobs$tolerance_confidence,
                       jobs$posterior_confidence, USE.NAMES = FALSE)))
))

ecm_stationarity_tolerance <- num(
  profile_cfg$ecm_control$tol_stationarity %||% 1e-3
)[1L]

fit_one_job <- function(job) {
  dgp_id <- as.character(job$dgp_id)[1L]
  dgp <- dgp_by_id[[dgp_id]]
  if (is.null(dgp)) stopf("Unknown DGP id: ", dgp_id)
  meta <- dgp_meta(dgp)
  n <- as.integer(job$n)[1L]
  content <- num(job$guaranteed_content)[1L]
  tol_conf <- num(job$tolerance_confidence)[1L]
  post_conf <- num(job$posterior_confidence)[1L]
  seed <- as.integer(job$seed)[1L]
  set.seed(seed)
  y <- meta$r(n)
  method_seed <- hash_to_seed(
    paste("method", seed, method_id, tol_conf, post_conf, sep = "|"),
    base = base_seed + 10000L
  )
  calibration_rule <- policy_rule_for(n, content, tol_conf)
  scan_target <- num(job$scan_target_content %||% NA_real_)[1L]
  policy_config <- profile_cfg
  policy_config$policy_id <- policy_config$policy_id %||%
    policy_config$adaptive_policy_id %||% method_id
  adaptive_menu <- rqr_mti_ecm_adaptive_profile_menu(
    y = y,
    content = content,
    tolerance_confidence = tol_conf,
    scan_target_content = scan_target,
    policy = as.character(profile_cfg$adaptive_policy %||% "cell")[1L],
    policy_config = policy_config,
    calibration_rule = calibration_rule
  )
  ecm_control <- profile_cfg$ecm_control %||% list()
  ecm_control$seed <- method_seed
  ecm_control$store_iteration_trace <- TRUE
  action <- rqr_mti_ecm_dp_profile_action(
    y = y,
    content = content,
    posterior_confidence = adaptive_menu$posterior_confidence,
    dp_concentration = adaptive_menu$dp_concentration,
    dp_base_measure = dp_base,
    strict_bayes = isTRUE(profile_cfg$strict_bayes %||% TRUE),
    scan_target_content = scan_target,
    q_grid = adaptive_menu$q_grid,
    q_grid_control = list(),
    tilt_grid_control = adaptive_menu$tilt_grid_control,
    learning_rate = num(profile_cfg$learning_rate %||% 1)[1L],
    beta_prior_obj = beta_prior(
      "ridge",
      ridge = list(tau2 = num(profile_cfg$beta_ridge_tau2 %||% 1e4)[1L])
    ),
    ecm_control = ecm_control,
    expand_if_empty = isTRUE(profile_cfg$expand_if_empty %||% TRUE),
    return_candidate_traces = TRUE
  )
  candidates <- action$candidates
  candidates$job_id <- as.character(job$job_id)[1L]
  candidates$dgp_id <- dgp_id
  candidates$dgp <- as.character(dgp$label %||% dgp_id)[1L]
  candidates$n <- n
  candidates$content <- content
  candidates$tolerance_confidence <- tol_conf
  candidates$posterior_confidence <- post_conf
  candidates$replication <- as.integer(job$replication)[1L]
  candidates$seed <- seed
  candidates$method_seed <- method_seed
  candidates$candidate_selected <-
    candidates$candidate_index %in% action$selected$candidate_index

  trace <- action$candidate_traces
  if (!is.null(trace) && nrow(trace)) {
    trace$job_id <- as.character(job$job_id)[1L]
    trace$dgp_id <- dgp_id
    trace$dgp <- as.character(dgp$label %||% dgp_id)[1L]
    trace$n <- n
    trace$content <- content
    trace$tolerance_confidence <- tol_conf
    trace$posterior_confidence <- post_conf
    trace$replication <- as.integer(job$replication)[1L]
    trace$seed <- seed
    trace$method_seed <- method_seed
  } else {
    trace <- data.frame()
  }

  selected <- action$selected
  job_summary <- data.frame(
    job_id = as.character(job$job_id)[1L],
    dgp_id = dgp_id,
    dgp = as.character(dgp$label %||% dgp_id)[1L],
    n = n,
    content = content,
    tolerance_confidence = tol_conf,
    posterior_confidence = post_conf,
    replication = as.integer(job$replication)[1L],
    seed = seed,
    method_seed = method_seed,
    selection_reason = as.character(job$selection_reason)[1L],
    scan_target_content = scan_target,
    policy_screen = adaptive_menu$posterior_confidence,
    q_grid_size = length(adaptive_menu$q_grid),
    candidates_evaluated = action$candidates_evaluated,
    feasible_count = action$feasible_count,
    selected_candidate_index = if (nrow(selected)) {
      as.integer(selected$candidate_index[[1L]])
    } else {
      NA_integer_
    },
    selected_q = if (nrow(selected)) num(selected$target_content)[1L] else NA_real_,
    selected_tilt = if (nrow(selected)) num(selected$mean_tilt)[1L] else NA_real_,
    selected_width = if (nrow(selected)) num(selected$width)[1L] else NA_real_,
    selected_stationarity = if (nrow(selected)) {
      num(selected$ecm_final_stationarity)[1L]
    } else {
      NA_real_
    },
    selected_ecm_converged = if (nrow(selected)) {
      isTRUE(selected$ecm_converged[[1L]])
    } else {
      FALSE
    },
    original_width = num(job$width)[1L],
    original_content = num(job$content)[1L],
    original_success = isTRUE(as_flag(job$success[[1L]])),
    original_ecm_converged = isTRUE(as_flag(job$ecm_converged[[1L]])),
    original_final_stationarity = num(job$ecm_final_stationarity)[1L],
    stationarity_tolerance = ecm_stationarity_tolerance,
    status = "ok",
    message = "",
    stringsAsFactors = FALSE
  )
  list(job = job_summary, candidates = candidates, trace = trace)
}

run_job <- function(ii) {
  job <- jobs[ii, , drop = FALSE]
  tryCatch(
    fit_one_job(as.list(job)),
    error = function(e) {
      list(
        job = data.frame(
          job_id = as.character(job$job_id),
          dgp_id = as.character(job$dgp_id),
          dgp = NA_character_,
          n = as.integer(job$n),
          content = num(job$guaranteed_content)[1L],
          tolerance_confidence = num(job$tolerance_confidence)[1L],
          posterior_confidence = num(job$posterior_confidence)[1L],
          replication = as.integer(job$replication),
          seed = as.integer(job$seed),
          method_seed = NA_integer_,
          selection_reason = as.character(job$selection_reason),
          scan_target_content = num(job$scan_target_content %||% NA_real_)[1L],
          policy_screen = NA_real_,
          q_grid_size = NA_integer_,
          candidates_evaluated = NA_integer_,
          feasible_count = NA_integer_,
          selected_candidate_index = NA_integer_,
          selected_q = NA_real_,
          selected_tilt = NA_real_,
          selected_width = NA_real_,
          selected_stationarity = NA_real_,
          selected_ecm_converged = FALSE,
          original_width = num(job$width)[1L],
          original_content = num(job$content)[1L],
          original_success = isTRUE(as_flag(job$success[[1L]])),
          original_ecm_converged = isTRUE(as_flag(job$ecm_converged[[1L]])),
          original_final_stationarity = num(job$ecm_final_stationarity)[1L],
          stationarity_tolerance = ecm_stationarity_tolerance,
          status = "error",
          message = conditionMessage(e),
          stringsAsFactors = FALSE
        ),
        candidates = data.frame(),
        trace = data.frame()
      )
    }
  )
}

message(sprintf("Running MTI-ECM trace diagnostics with %d worker(s).",
                workers))
indices <- seq_len(nrow(jobs))
results_list <- if (workers > 1L && .Platform$OS.type != "windows") {
  parallel::mclapply(indices, run_job, mc.cores = workers,
                     mc.preschedule = FALSE)
} else {
  lapply(indices, run_job)
}

job_summary <- do.call(rbind, lapply(results_list, `[[`, "job"))
candidate_blocks <- lapply(results_list, `[[`, "candidates")
candidate_summary <- if (any(vapply(candidate_blocks, nrow, integer(1L)) > 0)) {
  do.call(rbind, candidate_blocks)
} else {
  data.frame()
}
trace_blocks <- lapply(results_list, `[[`, "trace")
iteration_traces <- if (any(vapply(trace_blocks, nrow, integer(1L)) > 0)) {
  do.call(rbind, trace_blocks)
} else {
  data.frame()
}

cell_summary <- do.call(rbind, lapply(
  split(job_summary, paste(job_summary$dgp_id, job_summary$n,
                           job_summary$content,
                           job_summary$tolerance_confidence, sep = "|")),
  function(block) {
    data.frame(
      dgp_id = block$dgp_id[[1L]],
      dgp = block$dgp[[1L]],
      n = block$n[[1L]],
      content = block$content[[1L]],
      tolerance_confidence = block$tolerance_confidence[[1L]],
      posterior_confidence = block$posterior_confidence[[1L]],
      traced_replications = nrow(block),
      errored_jobs = sum(block$status != "ok"),
      selected_convergence_rate = mean(block$selected_ecm_converged),
      selected_stationarity_median =
        stats::median(block$selected_stationarity, na.rm = TRUE),
      selected_stationarity_max =
        max(block$selected_stationarity, na.rm = TRUE),
      selected_width_median =
        stats::median(block$selected_width, na.rm = TRUE),
      selected_q_min = min(block$selected_q, na.rm = TRUE),
      selected_q_max = max(block$selected_q, na.rm = TRUE),
      selected_tilt_min = min(block$selected_tilt, na.rm = TRUE),
      selected_tilt_max = max(block$selected_tilt, na.rm = TRUE),
      median_candidates_evaluated =
        stats::median(block$candidates_evaluated, na.rm = TRUE),
      median_feasible_count =
        stats::median(block$feasible_count, na.rm = TRUE),
      original_success_rate = mean(block$original_success),
      original_ecm_convergence_rate = mean(block$original_ecm_converged),
      stationarity_tolerance = block$stationarity_tolerance[[1L]],
      stringsAsFactors = FALSE
    )
  }
))
cell_summary <- cell_summary[order(cell_summary$dgp_id, cell_summary$n,
                                   cell_summary$content), , drop = FALSE]

write_csv <- function(x, name) {
  path <- file.path(output_dir, name)
  utils::write.csv(x, path, row.names = FALSE)
  path
}
paths <- c(
  jobs = write_csv(job_summary, "mti_ecm_trace_jobs.csv"),
  candidates = write_csv(candidate_summary, "mti_ecm_candidate_summary.csv"),
  traces = write_csv(iteration_traces, "mti_ecm_iteration_traces.csv"),
  cell_summary = write_csv(cell_summary, "mti_ecm_trace_cell_summary.csv")
)

artifact_hashes <- data.frame(
  artifact = names(paths),
  path = unname(paths),
  sha256 = vapply(paths, digest::digest, character(1L),
                  file = TRUE, algo = "sha256", serialize = FALSE),
  stringsAsFactors = FALSE
)
hash_path <- write_csv(artifact_hashes, "artifact_hashes.csv")

manifest <- list(
  schema_version = "rqrgibbs_mti_ecm_trace_diagnostics/1.0.0",
  created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  git_commit = tryCatch(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
    error = function(e) NA_character_
  ),
  config_path = config_path,
  results_path = results_path,
  policy_path = policy_path,
  method_id = method_id,
  mode = mode,
  replications_per_cell = replications_per_cell,
  workers = workers,
  jobs = nrow(job_summary),
  validation_cells = nrow(cell_summary),
  output_dir = output_dir,
  artifact_hashes = basename(hash_path)
)
jsonlite::write_json(manifest, file.path(output_dir, "manifest.json"),
                     pretty = TRUE, auto_unbox = TRUE)

readme <- c(
  "# MTI-ECM Trace Diagnostics",
  "",
  paste("Created:", manifest$created_at_utc),
  paste("Method:", method_id),
  paste("Config:", config_path),
  paste("Validation results:", results_path),
  paste("Policy:", policy_path),
  "",
  "These files replay a deterministic subset of the completed validation study",
  "with per-candidate ECM traces enabled. They are diagnostic artifacts for",
  "checking objective decrease, stationarity, endpoint motion, width motion,",
  "candidate feasibility, and the selected profile candidate in each validation",
  "cell. They are not a replacement for the confirmatory validation table.",
  "",
  "Files:",
  "- `mti_ecm_trace_jobs.csv`: one row per diagnostic replay.",
  "- `mti_ecm_candidate_summary.csv`: one row per fitted profile candidate.",
  "- `mti_ecm_iteration_traces.csv`: one row per ECM iteration and candidate.",
  "- `mti_ecm_trace_cell_summary.csv`: cell-level convergence summaries.",
  "- `artifact_hashes.csv`: SHA-256 hashes for reproducibility."
)
writeLines(readme, file.path(output_dir, "README.md"))

message("Trace diagnostics written to: ", normalizePath(output_dir,
                                                       winslash = "/"))
message("Jobs: ", nrow(job_summary), "; cells: ", nrow(cell_summary),
        "; trace rows: ", nrow(iteration_traces), ".")
