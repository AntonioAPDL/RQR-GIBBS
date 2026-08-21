#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "application/scripts/69_validate_rqr_bayes_uq.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a
csv_values <- function(value) {
  if (is.null(value) || !nzchar(value)) return(NULL)
  trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
}

legacy_method_id_map <- c(
  tcsp_mtrqr_gibbs_median_mc = "tcsp_mti_gibbs_median_mc",
  tcsp_mtrqr_gibbs_mean_mc = "tcsp_mti_gibbs_mean_mc",
  tcsp_mtrqr_ecm_map_mc = "tcsp_mti_ecm_map_mc",
  tcsp_mtrqr_gibbs_median_oracle_tilt_mc =
    "tcsp_mti_gibbs_median_oracle_tilt_mc",
  tcsp_mtrqr_ecm_map_oracle_tilt_mc = "tcsp_mti_ecm_map_oracle_tilt_mc"
)
canonical_method_id <- function(x) {
  x <- as.character(x)
  mapped <- unname(legacy_method_id_map[x])
  ifelse(is.na(mapped), x, mapped)
}
canonical_engine_id <- function(x) {
  if (is.null(x)) return(x)
  sub("^mtrqr_", "mti_", as.character(x))
}
canonicalize_text_id <- function(x) {
  if (is.null(x)) return(x)
  gsub("mtrqr", "mti", as.character(x), fixed = TRUE)
}
canonicalize_bayes_uq_config <- function(config) {
  for (ii in seq_along(config$methods)) {
    config$methods[[ii]]$method_id <- canonical_method_id(
      config$methods[[ii]]$method_id
    )
    config$methods[[ii]]$action_lane <- canonicalize_text_id(
      config$methods[[ii]]$action_lane
    )
    config$methods[[ii]]$selected_interval_source <- canonicalize_text_id(
      config$methods[[ii]]$selected_interval_source
    )
    config$methods[[ii]]$uq_engine <- canonical_engine_id(
      config$methods[[ii]]$uq_engine
    )
  }
  for (mode_name in names(config$modes)) {
    config$modes[[mode_name]]$method_ids <- as.list(canonical_method_id(
      unlist(config$modes[[mode_name]]$method_ids, use.names = FALSE)
    ))
  }
  engines <- config$engine_defaults %||% list()
  if (!is.null(engines$mtrqr_gibbs) && is.null(engines$mti_gibbs)) {
    engines$mti_gibbs <- engines$mtrqr_gibbs
  }
  if (!is.null(engines$mtrqr_ecm) && is.null(engines$mti_ecm)) {
    engines$mti_ecm <- engines$mtrqr_ecm
  }
  config$engine_defaults <- engines
  config
}

for (package in c("rqrgibbs", "jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stopf("Required package is not installed: ", package)
  }
}
library(rqrgibbs)

mode <- tolower(arg_value("--mode=", "smoke"))

config_path <- normalizePath(arg_value(
  "--config=", file.path("application", "config",
                         "rqr_bayes_uq_validation_v1.json")
), winslash = "/", mustWork = TRUE)
config <- jsonlite::read_json(config_path, simplifyVector = FALSE)
config <- canonicalize_bayes_uq_config(config)
wave_id <- arg_value("--wave-id=", NA_character_)
wave_filters <- list(
  dgp_id = csv_values(arg_value("--wave-dgp=", NULL)),
  method_id = csv_values(arg_value("--wave-method=", NULL)),
  n = csv_values(arg_value("--wave-n=", NULL)),
  guaranteed_content = csv_values(arg_value("--wave-content=", NULL)),
  tolerance_confidence = csv_values(
    arg_value("--wave-tolerance-confidence=", NULL)
  ),
  posterior_confidence = csv_values(
    arg_value("--wave-posterior-confidence=", NULL)
  )
)
scan_calibration_cache_path <- arg_value("--scan-calibration-cache=", NULL)
oracle_cache_path <- arg_value("--oracle-cache=", NULL)

if (!mode %in% c(names(config$modes), "health-check-read-only")) {
  stopf("Unsupported Bayesian UQ validation mode: ", mode)
}

if (identical(mode, "health-check-read-only")) {
  run_dir <- normalizePath(arg_value("--run-dir=", ""),
                           winslash = "/", mustWork = TRUE)
  required <- c(
    "bayes_uq_validation_results.csv",
    "bayes_uq_validation_summary.csv",
    "artifact_hashes.csv",
    "manifest.json",
    "README.md"
  )
  missing <- required[!file.exists(file.path(run_dir, required))]
  if (length(missing)) {
    stopf("Bayesian UQ run is missing artifact(s): ",
          paste(missing, collapse = ", "))
  }
  manifest <- jsonlite::read_json(file.path(run_dir, "manifest.json"),
                                  simplifyVector = TRUE)
  cat("Bayesian UQ validation health check passed:", run_dir, "\n")
  cat("Study:", manifest$study_id, "\n")
  cat("Mode:", manifest$mode, "\n")
  cat("Rows:", manifest$n_result_rows, "\n")
  quit(save = "no", status = 0L)
}

if (!mode %in% names(config$modes)) {
  stopf("Mode not found in config: ", mode)
}
if (!isTRUE(config$execution[[paste0(mode, "_authorized")]])) {
  stopf("Bayesian UQ validation mode is not authorized: ", mode)
}

default_output <- file.path(
  "application", "outputs", "rqr_bayes_uq_validation_v1",
  paste0(mode, "_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
)
output_dir <- normalizePath(arg_value("--output-dir=", default_output),
                            winslash = "/", mustWork = FALSE)
if (file.exists(output_dir) || dir.exists(output_dir)) {
  stopf("The output directory must be fresh: ", output_dir)
}
dir.create(dirname(output_dir), recursive = TRUE, showWarnings = FALSE)
staging <- tempfile(paste0(".", basename(output_dir), "-"),
                    tmpdir = dirname(output_dir))
dir.create(staging, recursive = TRUE, showWarnings = FALSE)
published <- FALSE
on.exit({
  if (!published) unlink(staging, recursive = TRUE, force = TRUE)
}, add = TRUE)

git_commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(e) NA_character_
)
mode_cfg <- config$modes[[mode]]
dgp_by_id <- setNames(config$dgps, vapply(config$dgps, `[[`,
                                         character(1L), "dgp_id"))
method_by_id <- setNames(config$methods, vapply(config$methods, `[[`,
                                               character(1L), "method_id"))

character_values <- function(x) {
  as.character(unlist(x, use.names = FALSE))
}
numeric_values <- function(x) {
  as.numeric(unlist(x, use.names = FALSE))
}
integer_values <- function(x) {
  as.integer(numeric_values(x))
}

filter_character <- function(values, requested, label) {
  values <- character_values(values)
  if (is.null(requested)) return(values)
  keep <- values[values %in% requested]
  if (!length(keep)) {
    stopf("Wave filter for ", label, " selected no configured values.")
  }
  keep
}
filter_numeric <- function(values, requested, label) {
  values <- numeric_values(values)
  if (is.null(requested)) return(values)
  requested <- as.numeric(requested)
  if (any(!is.finite(requested))) {
    stopf("Wave filter for ", label, " contains nonnumeric value.")
  }
  keep <- values[vapply(values, function(x) {
    any(abs(x - requested) <= 100 * .Machine$double.eps * max(1, abs(x)))
  }, logical(1L))]
  if (!length(keep)) {
    stopf("Wave filter for ", label, " selected no configured values.")
  }
  keep
}

mode_design_cells <- function(mode_cfg) {
  if (!is.null(mode_cfg$design_cells)) {
    cells <- mode_cfg$design_cells
    rows <- lapply(seq_along(cells), function(ii) {
      cell <- cells[[ii]]
      content <- cell$guaranteed_content %||% cell$content
      confidence <- cell$tolerance_confidence %||% cell$confidence
      data.frame(
        cell_id = as.character(cell$cell_id %||% sprintf("cell%03d", ii)),
        n = as.integer(cell$n)[1L],
        guaranteed_content = as.numeric(content)[1L],
        tolerance_confidence = as.numeric(confidence)[1L],
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
  } else {
    out <- expand.grid(
      cell_id = NA_character_,
      n = integer_values(mode_cfg$sample_sizes),
      guaranteed_content = numeric_values(mode_cfg$guaranteed_contents),
      tolerance_confidence = numeric_values(mode_cfg$tolerance_confidences),
      stringsAsFactors = FALSE
    )
    out$cell_id <- sprintf(
      "n%04d_c%s_t%s",
      out$n,
      gsub("\\.", "", sprintf("%.3f", out$guaranteed_content)),
      gsub("\\.", "", sprintf("%.3f", out$tolerance_confidence))
    )
  }
  if (!nrow(out) ||
      any(!is.finite(out$n)) ||
      any(out$n < 2L) ||
      any(!is.finite(out$guaranteed_content)) ||
      any(out$guaranteed_content <= 0 | out$guaranteed_content >= 1) ||
      any(!is.finite(out$tolerance_confidence)) ||
      any(out$tolerance_confidence <= 0 | out$tolerance_confidence >= 1)) {
    stopf("Mode design cells must have finite n >= 2 and probabilities in (0, 1).")
  }
  out <- out[order(out$n, out$guaranteed_content,
                   out$tolerance_confidence, out$cell_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

filter_design_cells <- function(cells) {
  keep <- rep(TRUE, nrow(cells))
  if (!is.null(wave_filters$n)) {
    requested <- as.numeric(wave_filters$n)
    keep <- keep & vapply(cells$n, function(x) {
      any(abs(x - requested) <= 100 * .Machine$double.eps * max(1, abs(x)))
    }, logical(1L))
  }
  if (!is.null(wave_filters$guaranteed_content)) {
    requested <- as.numeric(wave_filters$guaranteed_content)
    keep <- keep & vapply(cells$guaranteed_content, function(x) {
      any(abs(x - requested) <= 100 * .Machine$double.eps * max(1, abs(x)))
    }, logical(1L))
  }
  if (!is.null(wave_filters$tolerance_confidence)) {
    requested <- as.numeric(wave_filters$tolerance_confidence)
    keep <- keep & vapply(cells$tolerance_confidence, function(x) {
      any(abs(x - requested) <= 100 * .Machine$double.eps * max(1, abs(x)))
    }, logical(1L))
  }
  cells <- cells[keep, , drop = FALSE]
  if (!nrow(cells)) {
    stopf("Wave filters selected no configured design cells.")
  }
  rownames(cells) <- NULL
  cells
}

mode_cfg$dgp_ids <- filter_character(
  mode_cfg$dgp_ids, wave_filters$dgp_id, "dgp_id"
)
mode_cfg$method_ids <- filter_character(
  mode_cfg$method_ids, wave_filters$method_id, "method_id"
)
mode_cfg$posterior_confidences <- filter_numeric(
  mode_cfg$posterior_confidences, wave_filters$posterior_confidence,
  "posterior_confidences"
)
design_cells <- filter_design_cells(mode_design_cells(mode_cfg))
mode_cfg$sample_sizes <- sort(unique(design_cells$n))
mode_cfg$guaranteed_contents <- sort(unique(design_cells$guaranteed_content))
mode_cfg$tolerance_confidences <- sort(unique(design_cells$tolerance_confidence))

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
  tau <- as.numeric(dgp$tau %||% dgp$p %||% dgp$p0 %||% 0.10)[1L]
  scale <- as.numeric(dgp$scale %||% 1)[1L]
  if (!is.finite(tau) || tau <= 0 || tau >= 1 ||
      !is.finite(scale) || scale <= 0) {
    stopf("standardized_asymmetric_laplace requires tau in (0,1) and positive scale.")
  }
  raw_mean <- scale * (1 - 2 * tau) / (tau * (1 - tau))
  raw_variance <- scale^2 *
    (1 - 2 * tau + 2 * tau^2) / (tau^2 * (1 - tau)^2)
  raw_sd <- sqrt(raw_variance)
  F_raw <- function(z) {
    ifelse(
      z < 0,
      tau * exp((1 - tau) * z / scale),
      1 - (1 - tau) * exp(-tau * z / scale)
    )
  }
  q_raw <- function(p) {
    ifelse(
      p < tau,
      scale * log(p / tau) / (1 - tau),
      -scale * log((1 - p) / (1 - tau)) / tau
    )
  }
  list(
    r = function(n) (q_raw(stats::runif(n)) - raw_mean) / raw_sd,
    p = function(x) F_raw(raw_mean + raw_sd * as.numeric(x))
  )
}

standardized_two_piece_normal_meta <- function(dgp) {
  left_scale <- as.numeric(
    dgp$left_scale %||% dgp$scale_left %||%
      dgp$sigma_left %||% dgp$left_sd %||% 1
  )[1L]
  right_scale <- as.numeric(
    dgp$right_scale %||% dgp$scale_right %||%
      dgp$sigma_right %||% dgp$right_sd %||% 12
  )[1L]
  if (!is.finite(left_scale) || left_scale <= 0 ||
      !is.finite(right_scale) || right_scale <= 0) {
    stopf("standardized_two_piece_normal requires positive scales.")
  }
  raw_mean <- sqrt(2 / pi) * (right_scale - left_scale)
  raw_second <- left_scale^2 - left_scale * right_scale + right_scale^2
  raw_sd <- sqrt(raw_second - raw_mean^2)
  if (!is.finite(raw_sd) || raw_sd <= 0) {
    stopf("standardized_two_piece_normal has nonpositive variance.")
  }
  threshold <- left_scale / (left_scale + right_scale)
  F_raw <- function(z) {
    z <- as.numeric(z)
    ifelse(
      z < 0,
      2 * left_scale / (left_scale + right_scale) *
        stats::pnorm(z / left_scale),
      (left_scale - right_scale) / (left_scale + right_scale) +
        2 * right_scale / (left_scale + right_scale) *
        stats::pnorm(z / right_scale)
    )
  }
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
    p = function(x) F_raw(raw_mean + raw_sd * as.numeric(x))
  )
}

dgp_meta <- function(dgp) {
  if (identical(dgp$family, "normal")) {
    return(list(
      r = function(n) stats::rnorm(n),
      p = stats::pnorm
    ))
  }
  if (identical(dgp$family, "standardized_laplace")) {
    scale <- 1 / sqrt(2)
    return(list(
      r = function(n) {
        signs <- sample(c(-1, 1), n, replace = TRUE)
        signs * stats::rexp(n, rate = 1 / scale)
      },
      p = function(x) {
        ifelse(
          x < 0,
          0.5 * exp(x / scale),
          1 - 0.5 * exp(-x / scale)
        )
      }
    ))
  }
  if (identical(dgp$family, "standardized_lognormal")) {
    logsd <- as.numeric(dgp$logsd %||% 0.75)[1L]
    mean_raw <- exp(logsd^2 / 2)
    sd_raw <- sqrt((exp(logsd^2) - 1) * exp(logsd^2))
    return(list(
      r = function(n) (stats::rlnorm(n, 0, logsd) - mean_raw) / sd_raw,
      p = function(x) stats::plnorm(x * sd_raw + mean_raw, 0, logsd)
    ))
  }
  if (identical(dgp$family, "standardized_normal_mixture")) {
    weights <- as.numeric(dgp$weights)
    means <- as.numeric(dgp$means)
    sds <- as.numeric(dgp$sds)
    mean_mix <- sum(weights * means)
    second <- sum(weights * (sds^2 + means^2))
    sd_mix <- sqrt(second - mean_mix^2)
    return(list(
      r = function(n) {
        comp <- sample.int(length(weights), n, replace = TRUE, prob = weights)
        (stats::rnorm(n, means[comp], sds[comp]) - mean_mix) / sd_mix
      },
      p = function(x) {
        raw <- x * sd_mix + mean_mix
        out <- numeric(length(raw))
        for (j in seq_along(weights)) {
          out <- out + weights[[j]] * stats::pnorm(raw, means[[j]], sds[[j]])
        }
        out
      }
    ))
  }
  if (identical(dgp$family, "standardized_student_t")) {
    df <- as.numeric(dgp$df %||% 3)[1L]
    if (!is.finite(df) || df <= 2) {
      stopf("standardized_student_t requires df > 2 for finite variance.")
    }
    sd_raw <- sqrt(df / (df - 2))
    return(list(
      r = function(n) stats::rt(n, df = df) / sd_raw,
      p = function(x) stats::pt(x * sd_raw, df = df)
    ))
  }
  if (identical(dgp$family, "standardized_gamma")) {
    shape <- as.numeric(dgp$shape %||% 2)[1L]
    scale <- as.numeric(dgp$scale %||% 1)[1L]
    if (!is.finite(shape) || shape <= 0 ||
        !is.finite(scale) || scale <= 0) {
      stopf("standardized_gamma requires positive shape and scale.")
    }
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
  if (identical(dgp$family, "centered_exponential") ||
      identical(dgp$family, "standardized_exponential")) {
    rate <- as.numeric(dgp$rate %||% 1)[1L]
    if (!is.finite(rate) || rate <= 0) {
      stopf("centered_exponential requires positive rate.")
    }
    return(list(
      r = function(n) rate * stats::rexp(n, rate = rate) - 1,
      p = function(x) stats::pexp((x + 1) / rate, rate = rate)
    ))
  }
  if (identical(dgp$family, "standardized_beta")) {
    shape1 <- as.numeric(dgp$shape1 %||% dgp$a %||% 2)[1L]
    shape2 <- as.numeric(dgp$shape2 %||% dgp$b %||% 5)[1L]
    if (!is.finite(shape1) || shape1 <= 0 ||
        !is.finite(shape2) || shape2 <= 0) {
      stopf("standardized_beta requires positive shape parameters.")
    }
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
        stats::pbeta(
          x * sd_raw + mean_raw, shape1 = shape1, shape2 = shape2
        )
      }
    ))
  }
  if (identical(dgp$family, "standardized_asymmetric_laplace")) {
    return(standardized_asymmetric_laplace_meta(dgp))
  }
  if (identical(dgp$family, "standardized_two_piece_normal")) {
    return(standardized_two_piece_normal_meta(dgp))
  }
  stopf("Unsupported DGP family: ", dgp$family)
}

oracle_spec_from_dgp <- function(dgp) {
  if (identical(dgp$family, "normal")) {
    return(list(family = "gaussian", params = list(mean = 0, sd = 1)))
  }
  if (identical(dgp$family, "standardized_laplace")) {
    return(list(
      family = "laplace",
      params = list(location = 0, scale = 1 / sqrt(2))
    ))
  }
  if (identical(dgp$family, "standardized_lognormal")) {
    return(list(
      family = "centered_standardized_lognormal",
      params = list(logmean = 0, logsd = as.numeric(dgp$logsd %||% 0.75)[1L])
    ))
  }
  if (identical(dgp$family, "standardized_normal_mixture")) {
    weights <- as.numeric(dgp$weights)
    weights <- weights / sum(weights)
    means <- as.numeric(dgp$means)
    sds <- as.numeric(dgp$sds)
    mean_mix <- sum(weights * means)
    second <- sum(weights * (sds^2 + means^2))
    sd_mix <- sqrt(second - mean_mix^2)
    return(list(
      family = "gaussian_mixture",
      params = list(
        weights = weights,
        means = (means - mean_mix) / sd_mix,
        sds = sds / sd_mix,
        center = FALSE
      )
    ))
  }
  if (identical(dgp$family, "standardized_student_t")) {
    df <- as.numeric(dgp$df %||% 3)[1L]
    if (!is.finite(df) || df <= 2) {
      stopf("standardized_student_t requires df > 2 for finite variance.")
    }
    return(list(
      family = "student_t",
      params = list(df = df, scale = sqrt((df - 2) / df))
    ))
  }
  if (identical(dgp$family, "standardized_gamma")) {
    shape <- as.numeric(dgp$shape %||% 2)[1L]
    scale <- as.numeric(dgp$scale %||% 1)[1L]
    if (!is.finite(shape) || shape <= 0 ||
        !is.finite(scale) || scale <= 0) {
      stopf("standardized_gamma requires positive shape and scale.")
    }
    return(list(
      family = "centered_gamma",
      params = list(shape = shape, scale = 1 / sqrt(shape))
    ))
  }
  if (identical(dgp$family, "centered_exponential") ||
      identical(dgp$family, "standardized_exponential")) {
    rate <- as.numeric(dgp$rate %||% 1)[1L]
    if (!is.finite(rate) || rate <= 0) {
      stopf("centered_exponential requires positive rate.")
    }
    return(list(
      family = "centered_exponential",
      params = list(shape = 1, scale = 1)
    ))
  }
  if (identical(dgp$family, "standardized_beta")) {
    return(list(
      family = "standardized_beta",
      params = list(
        shape1 = as.numeric(dgp$shape1 %||% dgp$a %||% 2)[1L],
        shape2 = as.numeric(dgp$shape2 %||% dgp$b %||% 5)[1L]
      )
    ))
  }
  if (identical(dgp$family, "standardized_asymmetric_laplace")) {
    return(list(
      family = "asymmetric_laplace",
      params = list(
        tau = as.numeric(dgp$tau %||% dgp$p %||% dgp$p0 %||% 0.10)[1L],
        scale = as.numeric(dgp$scale %||% 1)[1L],
        variance_standardized = TRUE
      )
    ))
  }
  if (identical(dgp$family, "standardized_two_piece_normal")) {
    return(list(
      family = "standardized_two_piece_normal",
      params = list(
        left_scale = as.numeric(
          dgp$left_scale %||% dgp$scale_left %||%
            dgp$sigma_left %||% dgp$left_sd %||% 1
        )[1L],
        right_scale = as.numeric(
          dgp$right_scale %||% dgp$scale_right %||%
            dgp$sigma_right %||% dgp$right_sd %||% 12
        )[1L],
        variance_standardized = TRUE
      )
    ))
  }
  stopf("Unsupported oracle DGP family: ", dgp$family)
}

oracle_key <- function(dgp_id, c_target) {
  paste(dgp_id, formatC(c_target, digits = 12, format = "fg"), sep = "|")
}

oracle_cache <- new.env(parent = emptyenv())
if (!is.null(oracle_cache_path) && nzchar(oracle_cache_path)) {
  oracle_cache_path <- normalizePath(oracle_cache_path, winslash = "/",
                                     mustWork = TRUE)
  loaded_oracles <- readRDS(oracle_cache_path)
  loaded_oracles <- loaded_oracles$certificates %||% loaded_oracles
  for (name in names(loaded_oracles)) {
    assign(name, loaded_oracles[[name]], envir = oracle_cache)
  }
}

oracle_for <- function(dgp_id, c_target) {
  key <- oracle_key(dgp_id, c_target)
  if (!exists(key, envir = oracle_cache, inherits = FALSE)) {
    dgp <- dgp_by_id[[dgp_id]]
    spec <- oracle_spec_from_dgp(dgp)
    oracle_cfg <- config$oracle %||% list()
    certificate <- mti_interval_oracle(
      family = spec$family,
      coverage_level = c_target,
      target = "SH",
      params = spec$params,
      tol = as.numeric(oracle_cfg$tol %||% 1e-10)[1L],
      grid_size = as.integer(oracle_cfg$grid_size %||% 1601L)[1L]
    )
    assign(key, certificate, envir = oracle_cache)
  }
  get(key, envir = oracle_cache, inherits = FALSE)
}

true_content <- function(lower, upper, cdf) {
  as.numeric(cdf(upper) - cdf(lower))
}

dp_base_from_config <- function() {
  base <- config$engine_defaults$direct_dp$base
  if (!identical(base$family, "normal")) {
    stopf("Unsupported direct-DP base family in config: ", base$family)
  }
  dp_base_normal(mean = as.numeric(base$mean)[1L],
                     sd = as.numeric(base$sd)[1L])
}

selected_interval <- function(selected) {
  if (is.null(selected) || !nrow(selected)) {
    return(list(lower = NA_real_, upper = NA_real_, width = NA_real_,
                posterior_probability = NA_real_, retained_count = NA_integer_,
                infeasible = TRUE, posterior_constraint_status =
                  "infeasible_within_candidate_class",
                candidate_feasible_count = 0L,
                candidates_evaluated = NA_integer_))
  }
  list(
    lower = selected$lower[[1L]],
    upper = selected$upper[[1L]],
    width = selected$width[[1L]],
    posterior_probability =
      selected$posterior_content_probability[[1L]] %||% NA_real_,
    retained_count = selected$observed_count[[1L]] %||% NA_integer_,
    infeasible = FALSE,
    posterior_constraint_status = NA_character_,
    candidate_feasible_count = NA_integer_,
    candidates_evaluated = NA_integer_
  )
}

scan_method_for <- function(method_id) {
  method_meta <- method_by_id[[method_id]]
  configured <- method_meta$scan_method %||% NULL
  if (!is.null(configured)) return(as.character(configured)[1L])
  scan_cfg <- config$scan_calibration %||% list()
  global <- scan_cfg$method %||% NULL
  if (method_id %in% c(
    "hdp_s_mc", "tcsp_mc", "tcsp_mti_gibbs_median_mc",
    "tcsp_mti_gibbs_mean_mc", "tcsp_mti_ecm_map_mc",
    "tcsp_mti_gibbs_median_oracle_tilt_mc",
    "tcsp_mti_ecm_map_oracle_tilt_mc"
  )) {
    if (!is.null(global)) return(as.character(global)[1L])
    return("monte_carlo_conservative")
  }
  if (method_id %in% c("hdp_s", "tcsp_dkw")) return("dkw_conservative")
  NA_character_
}

scan_adaptive_control_for <- function(method_id) {
  method_meta <- method_by_id[[method_id]]
  scan_cfg <- config$scan_calibration %||% list()
  control <- scan_cfg$adaptive_control %||% list()
  mode_control <- mode_cfg$scan_adaptive_control %||%
    mode_cfg$adaptive_control %||% list()
  method_control <- method_meta$scan_adaptive_control %||%
    method_meta$adaptive_control %||% list()
  control <- utils::modifyList(control, mode_control)
  utils::modifyList(control, method_control)
}

scan_args_for <- function(method_id, n, c_target, tol_conf) {
  method_meta <- method_by_id[[method_id]]
  scan_cfg <- config$scan_calibration %||% list()
  method <- scan_method_for(method_id)
  adaptive_control <- scan_adaptive_control_for(method_id)
  n_sim <- method_meta$scan_n_sim %||%
    mode_cfg$scan_n_sim %||%
    scan_cfg[[paste0(mode, "_n_sim")]] %||%
    scan_cfg$n_sim %||%
    20000L
  numerical_confidence <- method_meta$scan_numerical_confidence %||%
    mode_cfg$scan_numerical_confidence %||%
    scan_cfg[[paste0(mode, "_numerical_confidence")]] %||%
    scan_cfg$numerical_confidence %||%
    0.999
  seed_base <- as.integer(
    scan_cfg$seed %||% (as.integer(config$base_seed %||% 862100L) + 500000L)
  )
  key_parts <- c(mode, method, n, c_target, tol_conf, n_sim,
                 numerical_confidence)
  if (identical(method, "monte_carlo_cp_adaptive") ||
      length(adaptive_control)) {
    key_parts <- c(key_parts, digest::digest(adaptive_control, algo = "sha256",
                                             serialize = TRUE))
  }
  key <- paste(key_parts, collapse = "|")
  list(
    n_sim = as.integer(n_sim)[1L],
    numerical_confidence = as.numeric(numerical_confidence)[1L],
    seed = hash_to_seed(key, base = seed_base),
    adaptive_control = adaptive_control
  )
}

scan_calibration_cache <- new.env(parent = emptyenv())
if (!is.null(scan_calibration_cache_path) &&
    nzchar(scan_calibration_cache_path)) {
  scan_calibration_cache_path <- normalizePath(
    scan_calibration_cache_path, winslash = "/", mustWork = TRUE
  )
  loaded_calibrations <- readRDS(scan_calibration_cache_path)
  loaded_calibrations <- loaded_calibrations$calibrations %||%
    loaded_calibrations
  for (name in names(loaded_calibrations)) {
    assign(name, loaded_calibrations[[name]], envir = scan_calibration_cache)
  }
}
get_scan_calibration <- function(method_id, n, c_target, tol_conf) {
  method <- scan_method_for(method_id)
  args <- scan_args_for(method_id, n, c_target, tol_conf)
  key_parts <- c(method, n, c_target, tol_conf, args$n_sim,
                 args$numerical_confidence, args$seed)
  if (identical(method, "monte_carlo_cp_adaptive") ||
      length(args$adaptive_control)) {
    key_parts <- c(key_parts, digest::digest(args$adaptive_control,
                                             algo = "sha256",
                                             serialize = TRUE))
  }
  key <- paste(key_parts, collapse = "|")
  if (!exists(key, envir = scan_calibration_cache, inherits = FALSE)) {
    cal <- tryCatch(
      tcsp_calibrate_count(
        n = n,
        guaranteed_content = c_target,
        tolerance_confidence = tol_conf,
        method = method,
        n_sim = args$n_sim,
        numerical_confidence = args$numerical_confidence,
        seed = args$seed,
        adaptive_control = args$adaptive_control
      ),
      error = function(e) {
        list(
          schema_version = paste0(config$schema_version, "/scan_calibration"),
          method = "scan_calibrated_tcsp_mti",
          scan_critical_method = method,
          n = as.integer(n),
          guaranteed_content = as.numeric(c_target),
          tolerance_confidence = as.numeric(tol_conf),
          retained_count = as.integer(n + 1L),
          target_content = NA_real_,
          content_buffer = NA_real_,
          scan_probability = list(
            certified_lower_probability = NA_real_,
            numerical_confidence = args$numerical_confidence,
            n_sim = args$n_sim
          ),
          adaptive_control = args$adaptive_control,
          finite_sample_claim_available = FALSE,
          asymptotic_claim_available = FALSE,
          infeasible = TRUE,
          message = conditionMessage(e)
        )
      }
    )
    assign(key, cal, envir = scan_calibration_cache)
  }
  get(key, envir = scan_calibration_cache, inherits = FALSE)
}

dataset_seed_for <- function(dgp_id, n, c_target, tol_conf, post_conf,
                             rep, counter) {
  if (isTRUE(mode_cfg$paired_thresholds)) {
    key <- paste("data", dgp_id, n, c_target, rep, sep = "|")
    return(hash_to_seed(key, base = base_seed))
  }
  base_seed + counter
}

empty_fit_result <- function(
    lower = NA_real_, upper = NA_real_, width = NA_real_,
    posterior_probability = NA_real_, retained_count = NA_integer_,
    infeasible = FALSE, message = "", fit_class = NA_character_,
    scan_critical_method = NA_character_, content_buffer = NA_real_,
    scan_certified_lower_probability = NA_real_,
    posterior_constraint_status = NA_character_,
    candidate_feasible_count = NA_integer_,
    candidates_evaluated = NA_integer_,
    oracle_target = NA_character_, oracle_mean_tilt = NA_real_,
    oracle_certificate_digest = NA_character_,
    oracle_lower_probability = NA_real_,
    oracle_upper_probability = NA_real_,
    action_lane = NA_character_, selected_interval_source = NA_character_,
    formal_action_lower = NA_real_, formal_action_upper = NA_real_,
    formal_action_width = NA_real_, fitted_summary_lower = NA_real_,
    fitted_summary_upper = NA_real_, fitted_summary_width = NA_real_,
    uq_engine = NA_character_, tilt_source = NA_character_,
    target_content = NA_real_, target_mean_tilt = NA_real_,
    target_audit_digest = NA_character_, posterior_draws = NA_integer_,
    mcmc_n_burn = NA_integer_, mcmc_n_mcmc = NA_integer_,
    mcmc_thin = NA_integer_, ecm_converged = NA,
    ecm_iterations = NA_integer_, ecm_objective = NA_real_,
    ecm_trace_length = NA_integer_,
    ecm_initial_objective = NA_real_,
    ecm_final_objective = NA_real_,
    ecm_relative_objective_drop = NA_real_,
    ecm_final_stationarity = NA_real_,
    fit_reused_across_posterior_thresholds = FALSE) {
  list(
    lower = lower,
    upper = upper,
    width = width,
    posterior_probability = posterior_probability,
    retained_count = retained_count,
    infeasible = infeasible,
    message = message,
    fit_class = fit_class,
    scan_critical_method = scan_critical_method,
    content_buffer = content_buffer,
    scan_certified_lower_probability = scan_certified_lower_probability,
    posterior_constraint_status = posterior_constraint_status,
    candidate_feasible_count = candidate_feasible_count,
    candidates_evaluated = candidates_evaluated,
    oracle_target = oracle_target,
    oracle_mean_tilt = oracle_mean_tilt,
    oracle_certificate_digest = oracle_certificate_digest,
    oracle_lower_probability = oracle_lower_probability,
    oracle_upper_probability = oracle_upper_probability,
    action_lane = action_lane,
    selected_interval_source = selected_interval_source,
    formal_action_lower = formal_action_lower,
    formal_action_upper = formal_action_upper,
    formal_action_width = formal_action_width,
    fitted_summary_lower = fitted_summary_lower,
    fitted_summary_upper = fitted_summary_upper,
    fitted_summary_width = fitted_summary_width,
    uq_engine = uq_engine,
    tilt_source = tilt_source,
    target_content = target_content,
    target_mean_tilt = target_mean_tilt,
    target_audit_digest = target_audit_digest,
    posterior_draws = posterior_draws,
    mcmc_n_burn = mcmc_n_burn,
    mcmc_n_mcmc = mcmc_n_mcmc,
    mcmc_thin = mcmc_thin,
    ecm_converged = ecm_converged,
    ecm_iterations = ecm_iterations,
    ecm_objective = ecm_objective,
    ecm_trace_length = ecm_trace_length,
    ecm_initial_objective = ecm_initial_objective,
    ecm_final_objective = ecm_final_objective,
    ecm_relative_objective_drop = ecm_relative_objective_drop,
    ecm_final_stationarity = ecm_final_stationarity,
    fit_reused_across_posterior_thresholds =
      fit_reused_across_posterior_thresholds
  )
}

fit_scalar <- function(fit, name, default) {
  value <- fit[[name]]
  if (is.null(value) || !length(value)) return(default)
  value[[1L]]
}

ecm_trace_diagnostics <- function(fit) {
  trace <- fit$objective_trace
  objective <- if (!is.null(trace) && "objective" %in% names(trace)) {
    as.numeric(trace$objective)
  } else {
    numeric()
  }
  initial <- if (length(objective)) objective[[1L]] else NA_real_
  final <- if (length(objective)) {
    utils::tail(objective, 1L)
  } else {
    as.numeric(fit$objective %||% NA_real_)[1L]
  }
  relative_drop <- if (is.finite(initial) && is.finite(final)) {
    (initial - final) / (1 + abs(initial))
  } else {
    NA_real_
  }
  stationarity <- fit$stationarity_diagnostic$max_abs_midpoint_gradient %||%
    if (!is.null(trace) && "stationarity" %in% names(trace)) {
      utils::tail(as.numeric(trace$stationarity), 1L)
    } else {
      NA_real_
    }
  list(
    ecm_trace_length = as.integer(length(objective)),
    ecm_initial_objective = as.numeric(initial),
    ecm_final_objective = as.numeric(final),
    ecm_relative_objective_drop = as.numeric(relative_drop),
    ecm_final_stationarity = as.numeric(stationarity)[1L]
  )
}

extract_young_mathew <- function(raw) {
  raw_df <- as.data.frame(raw, check.names = FALSE)
  lower_cols <- grep("lower", names(raw_df), ignore.case = TRUE, value = TRUE)
  upper_cols <- grep("upper", names(raw_df), ignore.case = TRUE, value = TRUE)
  if (!length(lower_cols) || !length(upper_cols) || !nrow(raw_df)) {
    stopf("Could not identify two-sided Young-Mathew interval columns.")
  }
  list(
    lower = as.numeric(raw_df[[lower_cols[[1L]]]][[1L]]),
    upper = as.numeric(raw_df[[upper_cols[[1L]]]][[1L]]),
    raw = raw_df,
    selected_row = 1L,
    output_rows = nrow(raw_df)
  )
}

fit_young_mathew <- function(y, c_target, tol_conf, method_meta) {
  if (!requireNamespace("tolerance", quietly = TRUE)) {
    return(empty_fit_result(
      infeasible = TRUE,
      message = "The tolerance package is required for the Young-Mathew comparator.",
      fit_class = "tolerance_nptol_int|young_mathew|package_unavailable",
      action_lane = method_meta$action_lane %||%
        "external_nonparametric_tolerance",
      selected_interval_source = method_meta$selected_interval_source %||%
        "tolerance_nptol_int_YM",
      scan_critical_method = "young_mathew_package_nominal"
    ))
  }
  alpha <- 1 - tol_conf
  fit <- tryCatch({
    raw <- tolerance::nptol.int(
      x = y, alpha = alpha, P = c_target, side = 2, method = "YM"
    )
    interval <- extract_young_mathew(raw)
    lower <- interval$lower
    upper <- interval$upper
    if (!is.finite(lower) || !is.finite(upper) || upper < lower) {
      stopf("Young-Mathew interval is invalid.")
    }
    empty_fit_result(
      lower = lower,
      upper = upper,
      width = upper - lower,
      formal_action_lower = lower,
      formal_action_upper = upper,
      formal_action_width = upper - lower,
      posterior_probability = NA_real_,
      infeasible = FALSE,
      fit_class = "tolerance_nptol_int|young_mathew",
      action_lane = method_meta$action_lane %||%
        "external_nonparametric_tolerance",
      selected_interval_source = method_meta$selected_interval_source %||%
        "tolerance_nptol_int_YM",
      target_audit_digest = digest::digest(
        list(
          package = "tolerance",
          package_version = as.character(utils::packageVersion("tolerance")),
          call = list(alpha = alpha, P = c_target, side = 2, method = "YM"),
          selected_row = interval$selected_row,
          output_rows = interval$output_rows,
          raw = interval$raw
        ),
        algo = "sha256", serialize = TRUE
      ),
      scan_critical_method = "young_mathew_package_nominal"
    )
  }, error = function(e) {
    empty_fit_result(
      infeasible = TRUE,
      message = conditionMessage(e),
      fit_class = "tolerance_nptol_int|young_mathew|error",
      action_lane = method_meta$action_lane %||%
        "external_nonparametric_tolerance",
      selected_interval_source = method_meta$selected_interval_source %||%
        "tolerance_nptol_int_YM",
      scan_critical_method = "young_mathew_package_nominal"
    )
  })
  fit
}

mean_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

median_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  stats::median(x)
}

mti_plugin_method_ids <- c(
  "tcsp_mti_gibbs_median_mc",
  "tcsp_mti_gibbs_mean_mc",
  "tcsp_mti_ecm_map_mc",
  "tcsp_mti_gibbs_median_oracle_tilt_mc",
  "tcsp_mti_ecm_map_oracle_tilt_mc"
)

mti_plugin_cache <- new.env(parent = emptyenv())

mti_mode_control <- function(engine, control_name) {
  cfg <- config$engine_defaults[[engine]] %||% list()
  cfg[[paste0(mode, "_", control_name)]] %||%
    cfg[[paste0("moderate_", control_name)]] %||%
    cfg[[control_name]] %||%
    list()
}

mti_learning_rate <- function(engine) {
  cfg <- config$engine_defaults[[engine]] %||% list()
  as.numeric(cfg$learning_rate %||% 1)[1L]
}

mti_beta_prior <- function(engine) {
  cfg <- config$engine_defaults[[engine]] %||% list()
  beta_prior(
    "ridge",
    ridge = list(tau2 = as.numeric(cfg$beta_ridge_tau2 %||% 1e4)[1L])
  )
}

fit_tcsp_mti_plugin <- function(method_id, y, dgp_id, c_target, tol_conf,
                                  seed) {
  method_meta <- method_by_id[[method_id]]
  engine <- as.character(method_meta$uq_engine %||%
                          if (grepl("gibbs", method_id, fixed = TRUE)) {
                            "mti_gibbs"
                          } else {
                            "mti_ecm"
                          })[1L]
  tilt_source <- as.character(method_meta$tilt_source %||%
                                "sample_shortest_window")[1L]
  selected_source <- as.character(
    method_meta$selected_interval_source %||% method_id
  )[1L]
  calibration <- get_scan_calibration(method_id, length(y), c_target, tol_conf)
  if (isTRUE(calibration$infeasible) ||
      calibration$retained_count > length(y)) {
    return(empty_fit_result(
      infeasible = TRUE,
      message = "TCSP calibration is infeasible for this fixed-target MTI method.",
      fit_class = "rqr_tcsp_mti_calibration_infeasible",
      scan_critical_method = calibration$scan_critical_method,
      content_buffer = calibration$content_buffer,
      retained_count = calibration$retained_count,
      scan_certified_lower_probability =
        calibration$scan_probability$certified_lower_probability %||% NA_real_,
      action_lane = method_meta$action_lane %||% "fixed_target_mti_plugin",
      selected_interval_source = selected_source,
      uq_engine = engine,
      tilt_source = tilt_source,
      target_content = calibration$target_content %||% NA_real_
    ))
  }

  window <- tcsp_shortest_window(
    y, retained_count = calibration$retained_count, na_rm = FALSE
  )
  tilt <- tcsp_tilt_from_window(window)
  target_mean_tilt <- tilt$delta_raw
  if (identical(tilt_source, "oracle_sh_population")) {
    target_mean_tilt <- oracle_for(dgp_id, c_target)$mean_tilt
  }
  target_content <- calibration$target_content
  formal_digest <- digest::digest(
    list(calibration = calibration, window = window,
         tilt_source = tilt_source, target_mean_tilt = target_mean_tilt),
    algo = "sha256", serialize = TRUE
  )
  formal_fields <- list(
    formal_action_lower = window$lower_endpoint,
    formal_action_upper = window$upper_endpoint,
    formal_action_width = window$width,
    scan_critical_method = calibration$scan_critical_method,
    content_buffer = calibration$content_buffer,
    retained_count = calibration$retained_count,
    scan_certified_lower_probability =
      calibration$scan_probability$certified_lower_probability %||% NA_real_,
    target_content = target_content,
    target_mean_tilt = target_mean_tilt
  )
  if (!is.finite(target_content) || target_content >= 1) {
    return(do.call(empty_fit_result, c(formal_fields, list(
      infeasible = TRUE,
      message = "MTI fixed-target engine unavailable because calibrated target_content is not in (0, 1).",
      fit_class = "rqr_tcsp_mti_target_content_unavailable",
      action_lane = method_meta$action_lane %||% "fixed_target_mti_plugin",
      selected_interval_source = selected_source,
      uq_engine = engine,
      tilt_source = tilt_source,
      target_audit_digest = formal_digest
    ))))
  }

  cache_key <- digest::digest(
    list(method_id = method_id, y = y, c_target = c_target,
         tol_conf = tol_conf, target_content = target_content,
         target_mean_tilt = target_mean_tilt, engine = engine),
    algo = "sha256", serialize = TRUE
  )
  if (exists(cache_key, envir = mti_plugin_cache, inherits = FALSE)) {
    cached <- get(cache_key, envir = mti_plugin_cache, inherits = FALSE)
    cached$fit_reused_across_posterior_thresholds <- TRUE
    return(cached)
  }

  X <- matrix(1, length(y), 1L, dimnames = list(NULL, "(Intercept)"))
  learning_rate <- mti_learning_rate(engine)
  prior <- mti_beta_prior(engine)
  if (identical(engine, "mti_gibbs")) {
    control <- mti_mode_control("mti_gibbs", "mcmc_control")
    control$seed <- seed
    fit <- mti_mcmc_fit(
      y = y,
      X = X,
      coverage_level = target_content,
      learning_rate = learning_rate,
      mean_tilt = target_mean_tilt,
      learning_rate_mode = "fixed_rate",
      beta_prior_obj = prior,
      mcmc_control = control
    )
    if (!isTRUE(all.equal(fit$model_spec$coverage_level, target_content,
                          tolerance = 0))) {
      stopf("MTI Gibbs target audit failed: coverage_level drifted.")
    }
    if (!isTRUE(all.equal(fit$model_spec$fixed_learning_rate, learning_rate,
                          tolerance = 0))) {
      stopf("MTI Gibbs target audit failed: learning_rate drifted.")
    }
    if (isTRUE(fit$model_spec$response_likelihood)) {
      stopf("MTI Gibbs target audit failed: response_likelihood is TRUE.")
    }
    if (!isTRUE(all.equal(unique(fit$model_spec$mean_tilt),
                          target_mean_tilt, tolerance = 1e-12))) {
      stopf("MTI Gibbs target audit failed: mean_tilt drifted.")
    }
    pred <- predict_interval(fit, X_new = X[1L, , drop = FALSE])
    lower_draws <- as.numeric(pred$lower_draws[1L, ])
    upper_draws <- as.numeric(pred$upper_draws[1L, ])
    if (identical(selected_source, "mti_gibbs_posterior_mean")) {
      lower <- as.numeric(pred$lower_mean)[1L]
      upper <- as.numeric(pred$upper_mean)[1L]
    } else {
      lower <- stats::median(lower_draws)
      upper <- stats::median(upper_draws)
    }
    out <- do.call(empty_fit_result, c(formal_fields, list(
      lower = lower,
      upper = upper,
      width = upper - lower,
      fitted_summary_lower = lower,
      fitted_summary_upper = upper,
      fitted_summary_width = upper - lower,
      posterior_probability = NA_real_,
      infeasible = FALSE,
      fit_class = paste(class(fit), collapse = "|"),
      action_lane = method_meta$action_lane %||% "fixed_target_mti_plugin",
      selected_interval_source = selected_source,
      uq_engine = engine,
      tilt_source = tilt_source,
      target_audit_digest = digest::digest(
        list(formal_digest = formal_digest, model_spec = fit$model_spec),
        algo = "sha256", serialize = TRUE
      ),
      posterior_draws = nrow(fit$samp.beta_root1),
      mcmc_n_burn = as.integer(fit$misc$n_burn %||% NA_integer_),
      mcmc_n_mcmc = as.integer(fit$misc$n_mcmc %||% NA_integer_),
      mcmc_thin = as.integer(fit$misc$thin %||% NA_integer_)
    )))
  } else if (identical(engine, "mti_ecm")) {
    control <- mti_mode_control("mti_ecm", "ecm_control")
    control$seed <- seed
    fit <- mti_ecm_fit(
      y = y,
      X = X,
      coverage_level = target_content,
      learning_rate = learning_rate,
      mean_tilt = target_mean_tilt,
      beta_prior_obj = prior,
      ecm_control = control
    )
    if (!isTRUE(all.equal(fit$model_spec$coverage_level, target_content,
                          tolerance = 0))) {
      stopf("MTI ECM target audit failed: coverage_level drifted.")
    }
    if (isTRUE(fit$model_spec$response_likelihood)) {
      stopf("MTI ECM target audit failed: response_likelihood is TRUE.")
    }
    if (!isTRUE(all.equal(unique(fit$model_spec$mean_tilt),
                          target_mean_tilt, tolerance = 1e-12))) {
      stopf("MTI ECM target audit failed: mean_tilt drifted.")
    }
    pred <- predict_interval(fit, X_new = X[1L, , drop = FALSE])
    lower <- as.numeric(pred$lower)[1L]
    upper <- as.numeric(pred$upper)[1L]
    ecm_diag <- ecm_trace_diagnostics(fit)
    out <- do.call(empty_fit_result, c(formal_fields, ecm_diag, list(
      lower = lower,
      upper = upper,
      width = upper - lower,
      fitted_summary_lower = lower,
      fitted_summary_upper = upper,
      fitted_summary_width = upper - lower,
      posterior_probability = NA_real_,
      infeasible = FALSE,
      fit_class = paste(class(fit), collapse = "|"),
      action_lane = method_meta$action_lane %||% "fixed_target_mti_plugin",
      selected_interval_source = selected_source,
      uq_engine = engine,
      tilt_source = tilt_source,
      target_audit_digest = digest::digest(
        list(formal_digest = formal_digest, model_spec = fit$model_spec),
        algo = "sha256", serialize = TRUE
      ),
      ecm_converged = isTRUE(fit$converged),
      ecm_iterations = as.integer(fit$iterations %||% NA_integer_),
      ecm_objective = as.numeric(fit$objective %||% NA_real_)
    )))
  } else {
    stopf("Unsupported MTI plug-in engine: ", engine)
  }
  assign(cache_key, out, envir = mti_plugin_cache)
  out
}

fit_method <- function(method_id, y, dgp_id, c_target, tol_conf, post_conf,
                       seed) {
  direct <- config$engine_defaults$direct_dp
  base <- dp_base_from_config()
  if (identical(method_id, "oracle_sh")) {
    certificate <- oracle_for(dgp_id, c_target)
    return(empty_fit_result(
      lower = certificate$lower_root,
      upper = certificate$upper_root,
      width = certificate$width,
      posterior_probability = NA_real_,
      retained_count = NA_integer_,
      infeasible = FALSE,
      fit_class = "mti_interval_oracle|rqr_interval_oracle|shortest_population",
      oracle_target = certificate$target,
      oracle_mean_tilt = certificate$mean_tilt,
      oracle_certificate_digest = certificate$certificate_digest,
      oracle_lower_probability = certificate$lower_probability,
      oracle_upper_probability = certificate$upper_probability,
      action_lane = method_by_id[[method_id]]$action_lane %||%
        "oracle_reference",
      selected_interval_source = method_by_id[[method_id]]$
        selected_interval_source %||% "population_shortest_oracle"
    ))
  }
  if (method_id %in% c("hdp_s", "hdp_s_mc")) {
    calibration <- get_scan_calibration(method_id, length(y), c_target,
                                        tol_conf)
    if (isTRUE(calibration$infeasible) ||
        calibration$retained_count > length(y)) {
      return(empty_fit_result(
        infeasible = TRUE,
        message = "Hybrid Bayesian-scan calibration is infeasible for this sample size.",
        fit_class = "rqr_hybrid_bayes_tcsp_calibration_infeasible",
        scan_critical_method = calibration$scan_critical_method,
        content_buffer = calibration$content_buffer,
        retained_count = calibration$retained_count,
        scan_certified_lower_probability =
          calibration$scan_probability$certified_lower_probability %||%
            NA_real_,
        posterior_constraint_status = "infeasible_scan_count"
      ))
    }
    fit <- tcsp_hybrid_bayes_fit(
      y,
      guaranteed_content = c_target,
      tolerance_confidence = tol_conf,
      posterior_confidence = post_conf,
      distribution_engine = "direct_dp",
      scan_method = scan_method_for(method_id),
      distribution_args = list(
        concentration = as.numeric(direct$concentration)[1L],
        base_measure = base
      ),
      scan_args = list(calibration = calibration),
      action_control = list(
        n_shortest_draws = 0
      )
    )
    return(empty_fit_result(
      lower = fit$formal_tolerance_action$lower_endpoint,
      upper = fit$formal_tolerance_action$upper_endpoint,
      width = fit$formal_tolerance_action$width,
      formal_action_lower = fit$formal_tolerance_action$lower_endpoint,
      formal_action_upper = fit$formal_tolerance_action$upper_endpoint,
      formal_action_width = fit$formal_tolerance_action$width,
      posterior_probability = fit$posterior_content_probability,
      retained_count = fit$formal_tolerance_action$retained_count,
      infeasible = is.na(fit$formal_tolerance_action$lower_endpoint),
      fit_class = paste(class(fit), collapse = "|"),
      action_lane = method_by_id[[method_id]]$action_lane %||%
        "scan_certified_bayes_hybrid",
      selected_interval_source = method_by_id[[method_id]]$
        selected_interval_source %||% "hybrid_bayesian_scan_action",
      uq_engine = "direct_dp",
      target_content = fit$scan_contract$calibration$target_content %||%
        NA_real_,
      target_audit_digest = digest::digest(
        list(scan_contract = fit$scan_contract,
             formal_action = fit$formal_tolerance_action),
        algo = "sha256", serialize = TRUE
      ),
      scan_critical_method =
        fit$scan_contract$calibration$scan_critical_method,
      content_buffer = fit$scan_contract$calibration$content_buffer,
      scan_certified_lower_probability =
        fit$scan_contract$calibration$scan_probability$
          certified_lower_probability %||% NA_real_,
      posterior_constraint_status = fit$posterior_constraint_status,
      candidate_feasible_count =
        fit$hybrid_bayesian_scan_action$feasible_count %||% NA_integer_,
      candidates_evaluated =
        fit$hybrid_bayesian_scan_action$candidates_evaluated %||% NA_integer_
    ))
  }
  if (method_id %in% mti_plugin_method_ids) {
    return(fit_tcsp_mti_plugin(
      method_id = method_id,
      y = y,
      dgp_id = dgp_id,
      c_target = c_target,
      tol_conf = tol_conf,
      seed = seed
    ))
  }
  if (identical(method_id, "dp_bayes")) {
    fit <- dp_fit(
      y,
      concentration = as.numeric(direct$concentration)[1L],
      base_measure = base
    )
    action <- dp_bayes_tolerance_action(
      fit, content = c_target, posterior_confidence = post_conf
    )
    out <- selected_interval(action$selected)
    out$posterior_constraint_status <- action$posterior_constraint_status
    out$candidate_feasible_count <- action$feasible_count
    out$candidates_evaluated <- action$candidates_evaluated
    out$fit_class <- paste(class(action), collapse = "|")
    out$action_lane <- method_by_id[[method_id]]$action_lane %||%
      "response_likelihood_bayes_diagnostic"
    out$selected_interval_source <- method_by_id[[method_id]]$
      selected_interval_source %||% "dp_bayes_selected_action"
    out$uq_engine <- "direct_dp"
    out$fitted_summary_lower <- out$lower
    out$fitted_summary_upper <- out$upper
    out$fitted_summary_width <- out$width
    return(out)
  }
  if (identical(method_id, "dpm_bayes")) {
    dpm <- config$engine_defaults$gaussian_dpm
    control <- dpm[[paste0(mode, "_mcmc_control")]] %||%
      dpm$moderate_mcmc_control
    control$seed <- seed
    fit <- dpm_fit(
      y,
      truncation_level = as.integer(dpm$truncation_level)[1L],
      concentration = 1,
      mcmc_control = control
    )
    action <- dpm_bayes_tolerance_action(
      fit, content = c_target, posterior_confidence = post_conf
    )
    out <- selected_interval(action$selected)
    out$posterior_constraint_status <- action$posterior_constraint_status
    out$candidate_feasible_count <- action$feasible_count
    out$candidates_evaluated <- action$candidates_evaluated
    out$fit_class <- paste(class(action), collapse = "|")
    out$action_lane <- method_by_id[[method_id]]$action_lane %||%
      "response_likelihood_bayes_diagnostic"
    out$selected_interval_source <- method_by_id[[method_id]]$
      selected_interval_source %||% "dpm_bayes_selected_action"
    out$uq_engine <- "gaussian_dpm"
    out$fitted_summary_lower <- out$lower
    out$fitted_summary_upper <- out$upper
    out$fitted_summary_width <- out$width
    return(out)
  }
  if (identical(method_id, "bb_shortest_diag")) {
    bb <- config$engine_defaults$bayesian_bootstrap
    draws <- bayesian_bootstrap_shortest_draws(
      y, n_draws = as.integer(bb$draws)[1L], seed = seed
    )
    sh <- dp_shortest_draws(draws, target_content = c_target)
    lower <- stats::median(sh$lower)
    upper <- stats::median(sh$upper)
    return(list(
      lower = lower,
      upper = upper,
      width = upper - lower,
      posterior_probability = NA_real_,
      retained_count = NA_integer_,
      infeasible = FALSE,
      fit_class = "rqr_bayesian_bootstrap_draws|diagnostic",
      action_lane = method_by_id[[method_id]]$action_lane %||%
        "bootstrap_diagnostic",
      selected_interval_source = method_by_id[[method_id]]$
        selected_interval_source %||%
        "bayesian_bootstrap_posterior_median",
      fitted_summary_lower = lower,
      fitted_summary_upper = upper,
      fitted_summary_width = upper - lower
    ))
  }
  if (method_id %in% c("tcsp_dkw", "tcsp_mc")) {
    calibration <- get_scan_calibration(method_id, length(y), c_target,
                                        tol_conf)
    if (isTRUE(calibration$infeasible) ||
        calibration$retained_count > length(y)) {
      return(empty_fit_result(
        infeasible = TRUE,
        message = "TCSP calibration is infeasible for this sample size and target.",
        fit_class = "rqr_tcsp_calibration_infeasible",
        scan_critical_method = calibration$scan_critical_method,
        content_buffer = calibration$content_buffer,
        retained_count = calibration$retained_count,
        scan_certified_lower_probability =
          calibration$scan_probability$certified_lower_probability %||%
            NA_real_,
        action_lane = method_by_id[[method_id]]$action_lane %||%
          "scan_certified_empirical",
        selected_interval_source = method_by_id[[method_id]]$
          selected_interval_source %||% "formal_scan_action",
        target_content = calibration$target_content %||% NA_real_
      ))
    }
    window <- tcsp_shortest_window(
      y, retained_count = calibration$retained_count, na_rm = FALSE
    )
    return(empty_fit_result(
      lower = window$lower_endpoint,
      upper = window$upper_endpoint,
      width = window$width,
      formal_action_lower = window$lower_endpoint,
      formal_action_upper = window$upper_endpoint,
      formal_action_width = window$width,
      posterior_probability = NA_real_,
      retained_count = calibration$retained_count,
      infeasible = FALSE,
      fit_class = "rqr_tcsp_shortest_window|cached_calibration",
      action_lane = method_by_id[[method_id]]$action_lane %||%
        "scan_certified_empirical",
      selected_interval_source = method_by_id[[method_id]]$
        selected_interval_source %||% "formal_scan_action",
      target_content = calibration$target_content %||% NA_real_,
      target_audit_digest = digest::digest(
        list(calibration = calibration, window = window),
        algo = "sha256", serialize = TRUE
      ),
      scan_critical_method = calibration$scan_critical_method,
      content_buffer = calibration$content_buffer,
      scan_certified_lower_probability =
        calibration$scan_probability$certified_lower_probability %||% NA_real_
    ))
  }
  if (method_id %in% c("split_empirical_shortest", "split_ecm_fixed_tilt")) {
    pilot_method <- if (identical(method_id, "split_ecm_fixed_tilt")) {
      "ecm_fixed_tilt"
    } else {
      "empirical_shortest"
    }
    ecm_args <- if (identical(pilot_method, "ecm_fixed_tilt")) {
      list(ecm_control = mti_mode_control("mti_ecm", "ecm_control"))
    } else {
      list()
    }
    fit <- tryCatch(
      tcsp_split_exact_fit(
        y,
        guaranteed_content = c_target,
        tolerance_confidence = tol_conf,
        pilot_fraction =
          as.numeric(config$engine_defaults$split$pilot_fraction)[1L],
        pilot_method = pilot_method,
        split_seed = seed,
        ecm_args = ecm_args
      ),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      return(empty_fit_result(
        infeasible = TRUE,
        message = conditionMessage(fit),
        fit_class = "rqr_tcsp_split_exact_calibration_infeasible",
        action_lane = method_by_id[[method_id]]$action_lane %||%
          "split_exact_spacing",
        selected_interval_source = method_by_id[[method_id]]$
          selected_interval_source %||% paste0("split_", pilot_method, "_action"),
        posterior_constraint_status = "exact_spacing_infeasible",
        uq_engine = method_by_id[[method_id]]$uq_engine %||%
          if (identical(pilot_method, "ecm_fixed_tilt")) "mti_ecm" else
            NA_character_,
        tilt_source = method_by_id[[method_id]]$tilt_source %||%
          if (identical(pilot_method, "ecm_fixed_tilt")) {
            "pilot_shortest_window"
          } else {
            NA_character_
          }
      ))
    }
    ecm_fit <- fit$ecm_fit_optional
    ecm_diag <- if (inherits(ecm_fit, "mti_ecm")) {
      ecm_trace_diagnostics(ecm_fit)
    } else {
      list(
        ecm_trace_length = NA_integer_,
        ecm_initial_objective = NA_real_,
        ecm_final_objective = NA_real_,
        ecm_relative_objective_drop = NA_real_,
        ecm_final_stationarity = NA_real_
      )
    }
    return(list(
      lower = fit$contract$lower_endpoint,
      upper = fit$contract$upper_endpoint,
      width = fit$contract$width,
      formal_action_lower = fit$contract$lower_endpoint,
      formal_action_upper = fit$contract$upper_endpoint,
      formal_action_width = fit$contract$width,
      posterior_probability = NA_real_,
      retained_count = fit$contract$retained_count %||% NA_integer_,
      infeasible = FALSE,
      fit_class = paste(c(class(fit), class(ecm_fit)), collapse = "|"),
      action_lane = method_by_id[[method_id]]$action_lane %||%
        "split_exact_spacing",
      selected_interval_source = method_by_id[[method_id]]$
        selected_interval_source %||% paste0("split_", pilot_method, "_action"),
      uq_engine = method_by_id[[method_id]]$uq_engine %||%
        if (identical(pilot_method, "ecm_fixed_tilt")) "mti_ecm" else
          NA_character_,
      tilt_source = method_by_id[[method_id]]$tilt_source %||%
        if (identical(pilot_method, "ecm_fixed_tilt")) {
          "pilot_shortest_window"
        } else {
          NA_character_
        },
      target_content = fit$contract$effective_pilot_content %||% NA_real_,
      target_mean_tilt = fit$pilot_diagnostics$selected_tilt %||% NA_real_,
      target_audit_digest = digest::digest(
        list(contract = fit$contract,
             pilot_diagnostics = fit$pilot_diagnostics),
        algo = "sha256", serialize = TRUE
      ),
      ecm_converged = if (inherits(ecm_fit, "mti_ecm")) {
        isTRUE(ecm_fit$converged)
      } else {
        NA
      },
      ecm_iterations = if (inherits(ecm_fit, "mti_ecm")) {
        as.integer(ecm_fit$iterations %||% NA_integer_)
      } else {
        NA_integer_
      },
      ecm_objective = if (inherits(ecm_fit, "mti_ecm")) {
        as.numeric(ecm_fit$objective %||% NA_real_)
      } else {
        NA_real_
      },
      ecm_trace_length = ecm_diag$ecm_trace_length,
      ecm_initial_objective = ecm_diag$ecm_initial_objective,
      ecm_final_objective = ecm_diag$ecm_final_objective,
      ecm_relative_objective_drop = ecm_diag$ecm_relative_objective_drop,
      ecm_final_stationarity = ecm_diag$ecm_final_stationarity
    ))
  }
  if (identical(method_id, "wilks_minmax")) {
    return(list(
      lower = min(y),
      upper = max(y),
      width = diff(range(y)),
      formal_action_lower = min(y),
      formal_action_upper = max(y),
      formal_action_width = diff(range(y)),
      posterior_probability = NA_real_,
      retained_count = length(y),
      infeasible = FALSE,
      fit_class = "wilks_minmax_comparator",
      action_lane = method_by_id[[method_id]]$action_lane %||%
        "order_statistic_baseline",
      selected_interval_source = method_by_id[[method_id]]$
        selected_interval_source %||% "full_sample_minmax"
    ))
  }
  if (identical(method_id, "young_mathew")) {
    return(fit_young_mathew(
      y = y,
      c_target = c_target,
      tol_conf = tol_conf,
      method_meta = method_by_id[[method_id]]
    ))
  }
  stopf("Unsupported method_id: ", method_id)
}

rows <- list()
counter <- 0L
base_seed <- as.integer(config$base_seed %||% 862100)
total_datasets <- length(mode_cfg$dgp_ids) *
  nrow(design_cells) *
  length(mode_cfg$posterior_confidences) *
  as.integer(mode_cfg$replications)
total_rows <- total_datasets * length(mode_cfg$method_ids)
progress_path <- file.path(staging, "progress.json")
partial_results_path <- file.path(staging, "partial_results.csv")
checkpoint_every_datasets <- as.integer(
  mode_cfg$checkpoint_every_datasets %||%
    config$execution$checkpoint_every_datasets %||%
    0L
)[1L]
write_partial_results <- function() {
  if (!length(rows)) return(invisible(FALSE))
  write.csv(do.call(rbind, rows), partial_results_path, row.names = FALSE)
  invisible(TRUE)
}
write_progress <- function(status, current = list()) {
  jsonlite::write_json(
    list(
      schema_version = paste0(config$schema_version, "/progress"),
      study_id = config$study_id,
      mode = mode,
      wave_id = wave_id,
      status = status,
      git_commit = git_commit,
      datasets_completed = as.integer(counter),
      total_datasets = as.integer(total_datasets),
      rows_completed = as.integer(length(rows)),
      total_rows = as.integer(total_rows),
      rows_remaining = as.integer(total_rows - length(rows)),
      checkpoint_path = if (file.exists(partial_results_path))
        partial_results_path else NA_character_,
      updated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      current = current
    ),
    progress_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
}
write_progress("running")
for (dgp_id in as.character(mode_cfg$dgp_ids)) {
  dgp <- dgp_by_id[[dgp_id]]
  meta <- dgp_meta(dgp)
  for (cell_index in seq_len(nrow(design_cells))) {
    n <- as.integer(design_cells$n[[cell_index]])
    c_target <- as.numeric(design_cells$guaranteed_content[[cell_index]])
    tol_conf <- as.numeric(design_cells$tolerance_confidence[[cell_index]])
    for (post_conf in as.numeric(mode_cfg$posterior_confidences)) {
      for (rep in seq_len(as.integer(mode_cfg$replications))) {
            counter <- counter + 1L
            seed <- dataset_seed_for(dgp_id, n, c_target, tol_conf, post_conf,
                                     rep, counter)
            set.seed(seed)
            y <- meta$r(n)
            method_ids <- as.character(mode_cfg$method_ids)
            for (method_id in method_ids) {
              method_seed <- hash_to_seed(
                paste("method", seed, method_id, tol_conf, post_conf, sep = "|"),
                base = base_seed + 10000L
              )
              method_meta <- method_by_id[[method_id]]
              timing <- system.time({
                fit <- tryCatch(
                  fit_method(
                    method_id, y, dgp_id, c_target, tol_conf, post_conf,
                    method_seed
                  ),
                  error = function(e) e
                )
              })
              if (inherits(fit, "error")) {
                rows[[length(rows) + 1L]] <- data.frame(
                  mode = mode,
                  dgp_id = dgp_id,
                  n = n,
                  guaranteed_content = c_target,
                  tolerance_confidence = tol_conf,
                  posterior_confidence = post_conf,
                  replication = rep,
                  seed = seed,
                  method_id = method_id,
                  formal_tolerance_action =
                    isTRUE(method_meta$formal_tolerance_action),
                  response_likelihood = isTRUE(method_meta$response_likelihood),
                  generalized_bayes = isTRUE(method_meta$generalized_bayes),
                  action_lane = method_meta$action_lane %||% NA_character_,
                  selected_interval_source =
                    method_meta$selected_interval_source %||% NA_character_,
                  success = NA,
                  content = NA_real_,
                  lower = NA_real_,
                  upper = NA_real_,
                  width = NA_real_,
                  formal_action_lower = NA_real_,
                  formal_action_upper = NA_real_,
                  formal_action_width = NA_real_,
                  formal_action_content = NA_real_,
                  formal_action_success = NA,
                  fitted_summary_lower = NA_real_,
                  fitted_summary_upper = NA_real_,
                  fitted_summary_width = NA_real_,
                  posterior_probability = NA_real_,
                  retained_count = NA_integer_,
                  retained_fraction = NA_real_,
                  content_gap = NA_real_,
                  posterior_threshold_excess = NA_real_,
                  scan_critical_method = NA_character_,
                  content_buffer = NA_real_,
                  scan_certified_lower_probability = NA_real_,
                  posterior_constraint_status = NA_character_,
                  candidate_feasible_count = NA_integer_,
                  candidates_evaluated = NA_integer_,
                  reference_method_id = NA_character_,
                  reference_width = NA_real_,
                  width_ratio_to_reference = NA_real_,
                  width_diff_to_reference = NA_real_,
                  posterior_constraint_binding = NA,
                  oracle_target = NA_character_,
                  oracle_mean_tilt = NA_real_,
                  oracle_certificate_digest = NA_character_,
                  oracle_lower_probability = NA_real_,
                  oracle_upper_probability = NA_real_,
                  oracle_sh_lower = NA_real_,
                  oracle_sh_upper = NA_real_,
                  oracle_sh_width = NA_real_,
                  width_ratio_to_oracle_sh = NA_real_,
                  width_excess_vs_oracle_sh = NA_real_,
                  uq_engine = method_meta$uq_engine %||% NA_character_,
                  tilt_source = method_meta$tilt_source %||% NA_character_,
                  target_content = NA_real_,
                  target_mean_tilt = NA_real_,
                  target_audit_digest = NA_character_,
                  posterior_draws = NA_integer_,
                  mcmc_n_burn = NA_integer_,
                  mcmc_n_mcmc = NA_integer_,
                  mcmc_thin = NA_integer_,
                  ecm_converged = NA,
                  ecm_iterations = NA_integer_,
                  ecm_objective = NA_real_,
                  ecm_trace_length = NA_integer_,
                  ecm_initial_objective = NA_real_,
                  ecm_final_objective = NA_real_,
                  ecm_relative_objective_drop = NA_real_,
                  ecm_final_stationarity = NA_real_,
                  fit_reused_across_posterior_thresholds = NA,
                  infeasible = TRUE,
                  message = conditionMessage(fit),
                  fit_class = "error",
                  elapsed_sec = unname(timing[["elapsed"]])
                )
              } else {
                content <- true_content(fit$lower, fit$upper, meta$p)
                formal_lower <- fit_scalar(
                  fit, "formal_action_lower",
                  if (isTRUE(method_meta$formal_tolerance_action)) {
                    fit$lower
                  } else {
                    NA_real_
                  }
                )
                formal_upper <- fit_scalar(
                  fit, "formal_action_upper",
                  if (isTRUE(method_meta$formal_tolerance_action)) {
                    fit$upper
                  } else {
                    NA_real_
                  }
                )
                formal_width <- fit_scalar(
                  fit, "formal_action_width",
                  if (isTRUE(method_meta$formal_tolerance_action)) {
                    fit$width
                  } else {
                    NA_real_
                  }
                )
                formal_content <- true_content(formal_lower, formal_upper,
                                               meta$p)
                rows[[length(rows) + 1L]] <- data.frame(
                  mode = mode,
                  dgp_id = dgp_id,
                  n = n,
                  guaranteed_content = c_target,
                  tolerance_confidence = tol_conf,
                  posterior_confidence = post_conf,
                  replication = rep,
                  seed = seed,
                  method_id = method_id,
                  formal_tolerance_action =
                    isTRUE(method_meta$formal_tolerance_action),
                  response_likelihood = isTRUE(method_meta$response_likelihood),
                  generalized_bayes = isTRUE(method_meta$generalized_bayes),
                  action_lane = fit_scalar(
                    fit, "action_lane",
                    method_meta$action_lane %||% NA_character_
                  ),
                  selected_interval_source = fit_scalar(
                    fit, "selected_interval_source",
                    method_meta$selected_interval_source %||% NA_character_
                  ),
                  success = if (isTRUE(fit$infeasible)) NA else
                    content >= c_target - 1e-12,
                  content = content,
                  lower = fit$lower,
                  upper = fit$upper,
                  width = fit$width,
                  formal_action_lower = formal_lower,
                  formal_action_upper = formal_upper,
                  formal_action_width = formal_width,
                  formal_action_content = formal_content,
                  formal_action_success =
                    if (is.na(formal_content)) NA else
                      formal_content >= c_target - 1e-12,
                  fitted_summary_lower = fit_scalar(
                    fit, "fitted_summary_lower", NA_real_
                  ),
                  fitted_summary_upper = fit_scalar(
                    fit, "fitted_summary_upper", NA_real_
                  ),
                  fitted_summary_width = fit_scalar(
                    fit, "fitted_summary_width", NA_real_
                  ),
                  posterior_probability = fit$posterior_probability,
                  retained_count = as.integer(fit$retained_count),
                  retained_fraction =
                    as.numeric(fit$retained_count %||% NA_real_) / n,
                  content_gap = content - c_target,
                  posterior_threshold_excess =
                    as.numeric(fit$posterior_probability %||% NA_real_) -
                    post_conf,
                  scan_critical_method =
                    fit$scan_critical_method %||% NA_character_,
                  content_buffer = fit$content_buffer %||% NA_real_,
                  scan_certified_lower_probability =
                    fit$scan_certified_lower_probability %||% NA_real_,
                  posterior_constraint_status =
                    fit$posterior_constraint_status %||% NA_character_,
                  candidate_feasible_count =
                    as.integer(fit$candidate_feasible_count %||% NA_integer_),
                  candidates_evaluated =
                    as.integer(fit$candidates_evaluated %||% NA_integer_),
                  reference_method_id = NA_character_,
                  reference_width = NA_real_,
                  width_ratio_to_reference = NA_real_,
                  width_diff_to_reference = NA_real_,
                  posterior_constraint_binding = NA,
                  oracle_target = fit$oracle_target %||% NA_character_,
                  oracle_mean_tilt = fit$oracle_mean_tilt %||% NA_real_,
                  oracle_certificate_digest =
                    fit$oracle_certificate_digest %||% NA_character_,
                  oracle_lower_probability =
                    fit$oracle_lower_probability %||% NA_real_,
                  oracle_upper_probability =
                    fit$oracle_upper_probability %||% NA_real_,
                  oracle_sh_lower = NA_real_,
                  oracle_sh_upper = NA_real_,
                  oracle_sh_width = NA_real_,
                  width_ratio_to_oracle_sh = NA_real_,
                  width_excess_vs_oracle_sh = NA_real_,
                  uq_engine = fit_scalar(
                    fit, "uq_engine",
                    method_meta$uq_engine %||% NA_character_
                  ),
                  tilt_source = fit_scalar(
                    fit, "tilt_source",
                    method_meta$tilt_source %||% NA_character_
                  ),
                  target_content = fit_scalar(
                    fit, "target_content", NA_real_
                  ),
                  target_mean_tilt = fit_scalar(
                    fit, "target_mean_tilt", NA_real_
                  ),
                  target_audit_digest = fit_scalar(
                    fit, "target_audit_digest", NA_character_
                  ),
                  posterior_draws = as.integer(fit_scalar(
                    fit, "posterior_draws", NA_integer_
                  )),
                  mcmc_n_burn = as.integer(fit_scalar(
                    fit, "mcmc_n_burn", NA_integer_
                  )),
                  mcmc_n_mcmc = as.integer(fit_scalar(
                    fit, "mcmc_n_mcmc", NA_integer_
                  )),
                  mcmc_thin = as.integer(fit_scalar(
                    fit, "mcmc_thin", NA_integer_
                  )),
                  ecm_converged = fit_scalar(fit, "ecm_converged", NA),
                  ecm_iterations = as.integer(fit_scalar(
                    fit, "ecm_iterations", NA_integer_
                  )),
                  ecm_objective = as.numeric(fit_scalar(
                    fit, "ecm_objective", NA_real_
                  )),
                  ecm_trace_length = as.integer(fit_scalar(
                    fit, "ecm_trace_length", NA_integer_
                  )),
                  ecm_initial_objective = as.numeric(fit_scalar(
                    fit, "ecm_initial_objective", NA_real_
                  )),
                  ecm_final_objective = as.numeric(fit_scalar(
                    fit, "ecm_final_objective", NA_real_
                  )),
                  ecm_relative_objective_drop = as.numeric(fit_scalar(
                    fit, "ecm_relative_objective_drop", NA_real_
                  )),
                  ecm_final_stationarity = as.numeric(fit_scalar(
                    fit, "ecm_final_stationarity", NA_real_
                  )),
                  fit_reused_across_posterior_thresholds = fit_scalar(
                    fit, "fit_reused_across_posterior_thresholds", FALSE
                  ),
                  infeasible = isTRUE(fit$infeasible),
                  message = fit$message %||% "",
                  fit_class = fit$fit_class,
                  elapsed_sec = unname(timing[["elapsed"]])
                )
              }
            }
            if (checkpoint_every_datasets > 0L &&
                counter %% checkpoint_every_datasets == 0L) {
              write_partial_results()
            }
            write_progress("running", current = list(
              dgp_id = dgp_id,
              n = n,
              guaranteed_content = c_target,
              tolerance_confidence = tol_conf,
              posterior_confidence = post_conf,
              replication = rep
            ))
      }
    }
  }
}
results <- do.call(rbind, rows)
reference_method_id <- mode_cfg$reference_method_id %||%
  config$diagnostics$reference_method_id %||%
  if ("tcsp_mc" %in% as.character(mode_cfg$method_ids)) {
    "tcsp_mc"
  } else {
    "tcsp_dkw"
  }
results$row_id <- seq_len(nrow(results))
reference_keys <- c("mode", "dgp_id", "n", "guaranteed_content",
                    "tolerance_confidence", "posterior_confidence",
                    "replication", "seed")
reference_rows <- results[results$method_id == reference_method_id,
                          c(reference_keys, "width"), drop = FALSE]
if (nrow(reference_rows)) {
  names(reference_rows)[names(reference_rows) == "width"] <-
    "reference_width"
  results <- merge(
    results,
    reference_rows,
    by = reference_keys,
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", ".diagnostic_reference")
  )
  if ("reference_width.diagnostic_reference" %in% names(results)) {
    results$reference_width <- results$reference_width.diagnostic_reference
    results$reference_width.diagnostic_reference <- NULL
  }
  results <- results[order(results$row_id), , drop = FALSE]
  results$reference_method_id <- reference_method_id
  results$width_ratio_to_reference <- ifelse(
    is.finite(results$reference_width) & results$reference_width > 0,
    results$width / results$reference_width,
    NA_real_
  )
  results$width_diff_to_reference <- results$width - results$reference_width
  is_hybrid <- results$method_id %in% c("hdp_s", "hdp_s_mc")
  results$posterior_constraint_binding <- ifelse(
    is_hybrid,
    results$posterior_constraint_status == "binding",
    NA
  )
}
oracle_reference_rows <- results[results$method_id == "oracle_sh",
                                 c(reference_keys, "lower", "upper", "width",
                                   "oracle_mean_tilt",
                                   "oracle_certificate_digest"),
                                 drop = FALSE]
if (nrow(oracle_reference_rows)) {
  names(oracle_reference_rows)[names(oracle_reference_rows) == "lower"] <-
    "oracle_reference_lower"
  names(oracle_reference_rows)[names(oracle_reference_rows) == "upper"] <-
    "oracle_reference_upper"
  names(oracle_reference_rows)[names(oracle_reference_rows) == "width"] <-
    "oracle_reference_width"
  names(oracle_reference_rows)[
    names(oracle_reference_rows) == "oracle_mean_tilt"
  ] <- "oracle_reference_mean_tilt"
  names(oracle_reference_rows)[
    names(oracle_reference_rows) == "oracle_certificate_digest"
  ] <- "oracle_reference_certificate_digest"
  results <- merge(
    results,
    oracle_reference_rows,
    by = reference_keys,
    all.x = TRUE,
    sort = FALSE
  )
  results <- results[order(results$row_id), , drop = FALSE]
  results$oracle_sh_lower <- results$oracle_reference_lower
  results$oracle_sh_upper <- results$oracle_reference_upper
  results$oracle_sh_width <- results$oracle_reference_width
  results$oracle_mean_tilt <- ifelse(
    is.na(results$oracle_mean_tilt),
    results$oracle_reference_mean_tilt,
    results$oracle_mean_tilt
  )
  results$oracle_certificate_digest <- ifelse(
    is.na(results$oracle_certificate_digest),
    results$oracle_reference_certificate_digest,
    results$oracle_certificate_digest
  )
  results$width_ratio_to_oracle_sh <- ifelse(
    is.finite(results$oracle_sh_width) & results$oracle_sh_width > 0,
    results$width / results$oracle_sh_width,
    NA_real_
  )
  results$width_excess_vs_oracle_sh <- results$width - results$oracle_sh_width
  results$oracle_reference_lower <- NULL
  results$oracle_reference_upper <- NULL
  results$oracle_reference_width <- NULL
  results$oracle_reference_mean_tilt <- NULL
  results$oracle_reference_certificate_digest <- NULL
}
results$row_id <- NULL
write.csv(results, file.path(staging, "bayes_uq_validation_results.csv"),
          row.names = FALSE)
if (file.exists(partial_results_path)) unlink(partial_results_path)

keys <- c("mode", "dgp_id", "n", "guaranteed_content",
          "tolerance_confidence", "posterior_confidence", "method_id")
split_key <- interaction(results[keys], drop = TRUE, lex.order = TRUE)
summary_rows <- lapply(split(results, split_key), function(df) {
  ok <- !is.na(df$success)
  data.frame(
    mode = df$mode[[1L]],
    dgp_id = df$dgp_id[[1L]],
    n = df$n[[1L]],
    guaranteed_content = df$guaranteed_content[[1L]],
    tolerance_confidence = df$tolerance_confidence[[1L]],
    posterior_confidence = df$posterior_confidence[[1L]],
    method_id = df$method_id[[1L]],
    replications = nrow(df),
    infeasible_rate = mean(df$infeasible),
    success_rate = if (any(ok)) mean(df$success[ok]) else NA_real_,
    success_rate_minus_tolerance_confidence =
      if (any(ok)) mean(df$success[ok]) - df$tolerance_confidence[[1L]]
      else NA_real_,
    mean_content = mean_or_na(df$content),
    mean_content_gap = mean_or_na(df$content_gap),
    median_width = median_or_na(df$width),
    mean_width = mean_or_na(df$width),
    median_formal_action_width = median_or_na(df$formal_action_width),
    mean_formal_action_content = mean_or_na(df$formal_action_content),
    formal_action_success_rate = mean_or_na(df$formal_action_success),
    median_fitted_summary_width = median_or_na(df$fitted_summary_width),
    mean_target_content = mean_or_na(df$target_content),
    mean_target_mean_tilt = mean_or_na(df$target_mean_tilt),
    mean_posterior_draws = mean_or_na(df$posterior_draws),
    mcmc_fit_reuse_rate =
      mean_or_na(df$fit_reused_across_posterior_thresholds),
    ecm_convergence_rate = mean_or_na(df$ecm_converged),
    mean_ecm_iterations = mean_or_na(df$ecm_iterations),
    mean_ecm_trace_length = mean_or_na(df$ecm_trace_length),
    median_ecm_relative_objective_drop =
      median_or_na(df$ecm_relative_objective_drop),
    median_ecm_final_stationarity = median_or_na(df$ecm_final_stationarity),
    median_width_ratio_to_reference =
      median_or_na(df$width_ratio_to_reference),
    mean_width_ratio_to_reference =
      mean_or_na(df$width_ratio_to_reference),
    median_width_diff_to_reference =
      median_or_na(df$width_diff_to_reference),
    median_width_ratio_to_oracle_sh =
      median_or_na(df$width_ratio_to_oracle_sh),
    mean_width_ratio_to_oracle_sh =
      mean_or_na(df$width_ratio_to_oracle_sh),
    median_width_excess_vs_oracle_sh =
      median_or_na(df$width_excess_vs_oracle_sh),
    mean_width_excess_vs_oracle_sh =
      mean_or_na(df$width_excess_vs_oracle_sh),
    mean_retained_fraction = mean_or_na(df$retained_fraction),
    mean_content_buffer = mean_or_na(df$content_buffer),
    mean_scan_certified_lower_probability =
      mean_or_na(df$scan_certified_lower_probability),
    mean_posterior_probability =
      mean_or_na(df$posterior_probability),
    mean_posterior_threshold_excess =
      mean_or_na(df$posterior_threshold_excess),
    posterior_binding_rate =
      mean_or_na(df$posterior_constraint_binding),
    mean_candidate_feasible_count =
      mean_or_na(df$candidate_feasible_count),
    mean_candidates_evaluated =
      mean_or_na(df$candidates_evaluated),
    mean_elapsed_sec = mean_or_na(df$elapsed_sec)
  )
})
summary <- do.call(rbind, summary_rows)
write.csv(summary, file.path(staging, "bayes_uq_validation_summary.csv"),
          row.names = FALSE)

readme <- c(
  paste0("# ", config$study_id),
  "",
  paste0("- Mode: `", mode, "`"),
  paste0("- Wave ID: `", wave_id, "`"),
  paste0("- Git commit: `", git_commit, "`"),
  paste0("- Result rows: `", nrow(results), "`"),
  paste0("- Summary rows: `", nrow(summary), "`"),
  paste0("- Diagnostic reference method: `", reference_method_id, "`"),
  paste0("- Oracle shortest reference present: `",
         any(results$method_id == "oracle_sh"), "`"),
  "",
  "This pilot separates response-distribution Bayesian UQ from RQR generalized-Bayes plug-in summaries.",
  "The hybrid direct-DP scan method fixes the scan count before evaluating posterior content probability.",
  "The `tcsp_mti_gibbs_*` and `tcsp_mti_ecm_*` rows are fixed-target MTI fitted summaries after scan calibration.",
  "For those rows, `formal_action_*` records the associated scan action and `fitted_summary_*` records the Gibbs or ECM endpoint summary.",
  "These artifacts are validation evidence only; they do not prove posterior endpoint coverage.",
  "The `oracle_sh` rows are non-deployable synthetic-DGP benchmarks for the true population shortest interval and oracle mean tilt.",
  "Width-ratio, oracle-gap, and posterior-binding diagnostics are included for post-run method tuning."
)
writeLines(readme, file.path(staging, "README.md"))
write_progress("complete")

artifact_paths <- file.path(staging, c(
  "bayes_uq_validation_results.csv",
  "bayes_uq_validation_summary.csv",
  "progress.json",
  "README.md"
))
artifact_hashes <- data.frame(
  file = basename(artifact_paths),
  sha256 = vapply(artifact_paths, digest::digest, character(1L),
                  algo = "sha256", file = TRUE)
)
write.csv(artifact_hashes, file.path(staging, "artifact_hashes.csv"),
          row.names = FALSE)

manifest <- list(
  schema_version = config$schema_version,
  study_id = config$study_id,
  mode = mode,
  git_commit = git_commit,
  config_path = config_path,
  output_dir = output_dir,
  n_result_rows = nrow(results),
  n_summary_rows = nrow(summary),
  wave_id = wave_id,
  wave_filters = wave_filters,
  started_from_response_likelihood_engines = TRUE,
  generalized_bayes_plugin_comparators_present = TRUE,
  posterior_endpoint_coverage_claim_available = FALSE,
  oracle_sh_reference_present = any(results$method_id == "oracle_sh"),
  oracle_sh_is_deployable_method = FALSE,
  scan_count_fixed_not_resampled = TRUE,
  diagnostic_reference_method_id = reference_method_id,
  scan_calibration_cache_path = scan_calibration_cache_path %||%
    NA_character_,
  oracle_cache_path = oracle_cache_path %||% NA_character_,
  checkpoint_every_datasets = checkpoint_every_datasets,
  created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  artifact_hashes = artifact_hashes
)
jsonlite::write_json(
  manifest, file.path(staging, "manifest.json"),
  pretty = TRUE, auto_unbox = TRUE
)

if (!file.rename(staging, output_dir)) {
  stopf("Could not publish Bayesian UQ validation output.")
}
published <- TRUE
cat("Bayesian UQ validation", mode, "completed:", output_dir, "\n")
