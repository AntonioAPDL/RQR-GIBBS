#!/usr/bin/env Rscript

# Deterministically reproduce the ordinary-RQR version-1 F01 collapsed
# generalized-posterior means and event probabilities.  This is a quadrature
# calculation only: it does not load rqrgibbs, draw an MCMC state, or fit a
# model.

required_packages <- c("digest", "pracma")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, quietly = TRUE,
  FUN.VALUE = logical(1L)
)]
if (length(missing_packages)) {
  stop(
    "Missing F01 quadrature packages: ",
    paste(missing_packages, collapse = ", "),
    ".",
    call. = FALSE
  )
}

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 1L) {
  stop(
    "Usage: 30_generate_ordinary_v1_f01_references.R [output.csv]",
    call. = FALSE
  )
}

script_argument <- grep(
  "^--file=", commandArgs(trailingOnly = FALSE), value = TRUE
)
if (length(script_argument) != 1L) {
  stop("Cannot identify the F01 generator source file.", call. = FALSE)
}
generator_absolute_path <- normalizePath(
  sub("^--file=", "", script_argument),
  winslash = "/", mustWork = TRUE
)
package_root <- normalizePath(
  file.path(dirname(generator_absolute_path), ".."),
  winslash = "/", mustWork = TRUE
)
if (!file.exists(file.path(package_root, "DESCRIPTION"))) {
  stop(
    "The F01 generator must reside in the rqrgibbs package source.",
    call. = FALSE
  )
}

canonical_path <- function(path) {
  if (!startsWith(path, "application/")) {
    stop("Canonical F01 paths must begin with application/.", call. = FALSE)
  }
  file.path(package_root, sub("^application/", "", path))
}

generator_path <- "application/scripts/30_generate_ordinary_v1_f01_references.R"
if (!identical(
      normalizePath(
        canonical_path(generator_path), winslash = "/", mustWork = TRUE
      ),
      generator_absolute_path
    )) {
  stop("The executing F01 generator path is not canonical.", call. = FALSE)
}

mean_source_path <-
  "application/inst/extdata/ordinary_v1_f01_independent_mean_references.csv"
cdf_source_path <-
  "application/inst/extdata/output7_corrected_cdf_references.csv"
mean_source_absolute <- normalizePath(
  canonical_path(mean_source_path), winslash = "/", mustWork = TRUE
)
cdf_source_absolute <- normalizePath(
  canonical_path(cdf_source_path), winslash = "/", mustWork = TRUE
)

output_path <- if (length(arguments)) {
  arguments[[1L]]
} else {
  file.path(
    package_root, "inst", "extdata",
    "ordinary_v1_f01_quadrature_references.csv"
  )
}
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
output_path <- normalizePath(
  output_path, winslash = "/", mustWork = FALSE
)

schema_version <- "rqrgibbs_ordinary_v1_f01_quadrature/1.0.0"
fixture_id <- "F01"
orders <- c(48L, 64L, 80L)
order_convergence_tolerance <- 5e-11
mean_comparison_tolerance <- 1e-9
cdf_comparison_tolerance <- 5e-11

response <- c(
  -2.0, -1.3, -0.8, -0.4, -0.1, 0.1,
   0.35, 0.7, 1.1, 1.6, 2.2, 3.0
)
coverage_level <- 0.80
loss_reference_scale <- 1
root_prior_sd <- 5
lambda_prior_shape <- 4
lambda_prior_rate <- 4
marginal_shape <- lambda_prior_shape + length(response)
response_probability_cuts <- sort(unique(c(
  0, stats::pnorm(response / root_prior_sd), 1
)))

gauss_cache <- new.env(parent = emptyenv())

gauss_rule <- function(order, lower, upper) {
  key <- as.character(order)
  if (!exists(key, envir = gauss_cache, inherits = FALSE)) {
    assign(
      key, pracma::gaussLegendre(order, -1, 1),
      envir = gauss_cache
    )
  }
  standard <- get(key, envir = gauss_cache, inherits = FALSE)
  half_width <- 0.5 * (upper - lower)
  midpoint <- 0.5 * (upper + lower)
  list(
    x = midpoint + half_width * standard$x,
    w = half_width * standard$w
  )
}

root_values <- function(probability1, probability2) {
  root1 <- stats::qnorm(probability1) * root_prior_sd
  root2 <- stats::qnorm(probability2) * root_prior_sd
  loss <- numeric(length(root1))
  for (value in response) {
    residual_product <- (value - root1) * (value - root2)
    loss <- loss + residual_product * (
      coverage_level - as.numeric(residual_product < 0)
    )
  }
  rate <- lambda_prior_rate + loss / loss_reference_scale
  if (any(!is.finite(rate)) || any(rate <= 0)) {
    stop("The F01 collapsed rate is not finite and positive.", call. = FALSE)
  }
  list(
    root1 = root1,
    root2 = root2,
    loss = loss,
    rate = rate,
    weight = rate^(-marginal_shape)
  )
}

axis_cuts <- function(lower, upper, extra = numeric(0)) {
  sort(unique(c(
    lower,
    response_probability_cuts[
      response_probability_cuts > lower &
        response_probability_cuts < upper
    ],
    extra[extra > lower & extra < upper],
    upper
  )))
}

integrate_rectangles <- function(
    order, functional = function(values) 1,
    lower1 = 0, upper1 = 1, lower2 = 0, upper2 = 1,
    extra_cuts1 = numeric(0), extra_cuts2 = numeric(0)) {
  if (lower1 >= upper1 || lower2 >= upper2) return(0)
  cuts1 <- axis_cuts(lower1, upper1, extra_cuts1)
  cuts2 <- axis_cuts(lower2, upper2, extra_cuts2)
  result <- 0
  for (ii in seq_len(length(cuts1) - 1L)) {
    nodes1 <- gauss_rule(order, cuts1[ii], cuts1[ii + 1L])
    for (jj in seq_len(length(cuts2) - 1L)) {
      nodes2 <- gauss_rule(order, cuts2[jj], cuts2[jj + 1L])
      grid <- expand.grid(
        probability1 = nodes1$x,
        probability2 = nodes2$x
      )
      values <- root_values(grid$probability1, grid$probability2)
      weights <- as.vector(outer(nodes1$w, nodes2$w))
      functional_values <- functional(values)
      if (length(functional_values) == 1L) {
        functional_values <- rep(functional_values, length(weights))
      }
      if (length(functional_values) != length(weights) ||
          any(!is.finite(functional_values))) {
        stop("An F01 rectangular functional is invalid.", call. = FALSE)
      }
      result <- result + sum(
        weights * values$weight * functional_values
      )
    }
  }
  result
}

integrate_nested <- function(
    order, inner_limits, functional = function(values) 1,
    extra_outer_cuts = numeric(0),
    split_at_diagonal = FALSE) {
  outer_cuts <- axis_cuts(0, 1, extra_outer_cuts)
  result <- 0
  for (ii in seq_len(length(outer_cuts) - 1L)) {
    outer <- gauss_rule(
      order, outer_cuts[ii], outer_cuts[ii + 1L]
    )
    inner_values <- numeric(length(outer$x))
    for (kk in seq_along(outer$x)) {
      root1 <- stats::qnorm(outer$x[kk]) * root_prior_sd
      limits <- inner_limits(root1)
      if (!is.numeric(limits) || length(limits) != 2L ||
          anyNA(limits)) {
        stop("An F01 nested integration limit is invalid.", call. = FALSE)
      }
      lower <- max(0, limits[[1L]])
      upper <- min(1, limits[[2L]])
      if (lower >= upper) next
      diagonal_cut <- if (isTRUE(split_at_diagonal)) {
        outer$x[[kk]]
      } else {
        numeric(0)
      }
      inner_cuts <- axis_cuts(
        lower, upper, extra = diagonal_cut
      )
      for (jj in seq_len(length(inner_cuts) - 1L)) {
        inner <- gauss_rule(
          order, inner_cuts[jj], inner_cuts[jj + 1L]
        )
        values <- root_values(
          rep(outer$x[[kk]], length(inner$x)), inner$x
        )
        functional_values <- functional(values)
        if (length(functional_values) == 1L) {
          functional_values <- rep(
            functional_values, length(inner$x)
          )
        }
        if (length(functional_values) != length(inner$x) ||
            any(!is.finite(functional_values))) {
          stop("An F01 nested functional is invalid.", call. = FALSE)
        }
        inner_values[[kk]] <- inner_values[[kk]] + sum(
          inner$w * values$weight * functional_values
        )
      }
    }
    result <- result + sum(outer$w * inner_values)
  }
  result
}

compute_references <- function(order) {
  denominator <- integrate_rectangles(order)
  if (!is.finite(denominator) || denominator <= 0) {
    stop("The F01 normalizing integral is invalid.", call. = FALSE)
  }

  lambda_numerator <- integrate_rectangles(
    order,
    functional = function(values) marginal_shape / values$rate
  )
  midpoint_numerator <- integrate_rectangles(
    order,
    functional = function(values) {
      0.5 * (values$root1 + values$root2)
    }
  )
  width_numerator <- integrate_nested(
    order,
    inner_limits = function(root1) c(0, 1),
    functional = function(values) abs(values$root1 - values$root2),
    split_at_diagonal = TRUE
  )
  total_loss_numerator <- integrate_rectangles(
    order, functional = function(values) values$loss
  )

  mean_midpoint <- midpoint_numerator / denominator
  mean_width <- width_numerator / denominator
  means <- c(
    lambda = lambda_numerator / denominator,
    lower_root = mean_midpoint - 0.5 * mean_width,
    upper_root = mean_midpoint + 0.5 * mean_width,
    width = mean_width,
    midpoint = mean_midpoint,
    total_loss = total_loss_numerator / denominator
  )

  lambda_cdf_numerator <- integrate_rectangles(
    order,
    functional = function(values) {
      stats::pgamma(
        1, shape = marginal_shape, rate = values$rate
      )
    }
  )

  lower_threshold <- -1.5
  lower_probability <- stats::pnorm(
    lower_threshold / root_prior_sd
  )
  both_above_lower <- integrate_rectangles(
    order,
    lower1 = lower_probability, upper1 = 1,
    lower2 = lower_probability, upper2 = 1,
    extra_cuts1 = lower_probability,
    extra_cuts2 = lower_probability
  )

  upper_threshold <- 2.5
  upper_probability <- stats::pnorm(
    upper_threshold / root_prior_sd
  )
  both_below_upper <- integrate_rectangles(
    order,
    lower1 = 0, upper1 = upper_probability,
    lower2 = 0, upper2 = upper_probability,
    extra_cuts1 = upper_probability,
    extra_cuts2 = upper_probability
  )

  width_threshold <- 4
  width_cdf_numerator <- integrate_nested(
    order,
    inner_limits = function(root1) {
      stats::pnorm(
        c(
          root1 - width_threshold,
          root1 + width_threshold
        ) / root_prior_sd
      )
    },
    extra_outer_cuts = stats::pnorm(
      c(
        response - width_threshold,
        response + width_threshold
      ) / root_prior_sd
    )
  )

  midpoint_threshold <- 0.5
  midpoint_cdf_numerator <- integrate_nested(
    order,
    inner_limits = function(root1) {
      c(
        0,
        stats::pnorm(
          (2 * midpoint_threshold - root1) / root_prior_sd
        )
      )
    },
    extra_outer_cuts = stats::pnorm(
      (2 * midpoint_threshold - response) / root_prior_sd
    )
  )

  c(
    setNames(means, paste0("mean:", names(means))),
    "cdf:lambda" = lambda_cdf_numerator / denominator,
    "cdf:lower_root" = 1 - both_above_lower / denominator,
    "cdf:upper_root" = both_below_upper / denominator,
    "cdf:width" = width_cdf_numerator / denominator,
    "cdf:midpoint" = midpoint_cdf_numerator / denominator
  )
}

order_results <- vapply(
  orders, compute_references, numeric(11L)
)
if (any(!is.finite(order_results))) {
  stop("The F01 quadrature produced a nonfinite value.", call. = FALSE)
}
reproduced <- order_results[, ncol(order_results)]
previous <- order_results[, ncol(order_results) - 1L]
order_convergence_difference <- abs(reproduced - previous)
if (any(order_convergence_difference > order_convergence_tolerance)) {
  stop(
    "The F01 quadrature failed its order-convergence gate.",
    call. = FALSE
  )
}

mean_source <- utils::read.csv(
  mean_source_absolute, stringsAsFactors = FALSE,
  check.names = FALSE
)
mean_estimands <- c(
  "lambda", "lower_root", "upper_root", "width", "midpoint", "total_loss"
)
mean_rows <- mean_source[
  mean_source$comparison_type == "mean" &
    mean_source$estimand %in% mean_estimands,
  ,
  drop = FALSE
]
mean_rows <- mean_rows[
  match(mean_estimands, mean_rows$estimand),
  ,
  drop = FALSE
]
if (nrow(mean_rows) != length(mean_estimands) ||
    anyNA(mean_rows$estimand) ||
    !identical(as.character(mean_rows$estimand), mean_estimands) ||
    any(!is.finite(mean_rows$reference_value)) ||
    any(mean_rows$reference_schema !=
          "rqrgibbs_ordinary_v1_f01_mean_reference/1.0.0") ||
    any(mean_rows$fixture_id != fixture_id) ||
    any(mean_rows$status != "authoritative_mean_only") ||
    any(!grepl("^[0-9a-f]{64}$", mean_rows$provenance_sha256))) {
  stop("The tracked F01 mean-only contract is incomplete.", call. = FALSE)
}

cdf_source <- utils::read.csv(
  cdf_source_absolute, stringsAsFactors = FALSE,
  check.names = FALSE
)
cdf_estimands <- c(
  "lambda", "lower_root", "upper_root", "width", "midpoint"
)
cdf_rows <- cdf_source[
  cdf_source$comparison_type == "cdf" &
    cdf_source$estimand %in% cdf_estimands,
  ,
  drop = FALSE
]
cdf_rows <- cdf_rows[
  match(cdf_estimands, cdf_rows$estimand),
  ,
  drop = FALSE
]
expected_thresholds <- c(
  lambda = 1, lower_root = -1.5, upper_root = 2.5,
  width = 4, midpoint = 0.5
)
if (nrow(cdf_rows) != length(cdf_estimands) ||
    anyNA(cdf_rows$estimand) ||
    !identical(as.character(cdf_rows$estimand), cdf_estimands) ||
    any(!is.finite(cdf_rows$reference_value)) ||
    any(cdf_rows$reference_schema !=
          "rqrgibbs_intercept_cdf_reference/2.0.0") ||
    !identical(
      as.numeric(cdf_rows$threshold),
      unname(expected_thresholds[cdf_estimands])
    ) ||
    any(cdf_rows$order_convergence_difference > 5e-10)) {
  stop("The tracked corrected Output-7 CDF contract is incomplete.",
       call. = FALSE)
}

cdf_generator_paths <- unique(as.character(cdf_rows$generator))
if (length(cdf_generator_paths) != 1L) {
  stop("The tracked corrected CDF generator is ambiguous.", call. = FALSE)
}
cdf_generator_absolute <- normalizePath(
  canonical_path(cdf_generator_paths),
  winslash = "/", mustWork = TRUE
)
cdf_generator_sha256 <- digest::digest(
  file = cdf_generator_absolute, algo = "sha256", serialize = FALSE
)
if (!identical(
      unique(as.character(cdf_rows$generator_sha256)),
      cdf_generator_sha256
    )) {
  stop("The tracked corrected CDF generator hash is stale.",
       call. = FALSE)
}

comparison_type <- c(
  rep("mean", length(mean_estimands)),
  rep("cdf", length(cdf_estimands))
)
estimand <- c(mean_estimands, cdf_estimands)
key <- paste(comparison_type, estimand, sep = ":")
tracked_value <- c(
  as.numeric(mean_rows$reference_value),
  as.numeric(cdf_rows$reference_value)
)
reproduced_value <- unname(reproduced[key])
threshold <- c(
  rep(NA_real_, length(mean_estimands)),
  unname(expected_thresholds[cdf_estimands])
)
tracked_source_path <- c(
  rep(mean_source_path, length(mean_estimands)),
  rep(cdf_source_path, length(cdf_estimands))
)
tracked_source_sha256 <- c(
  rep(
    digest::digest(
      file = mean_source_absolute, algo = "sha256", serialize = FALSE
    ),
    length(mean_estimands)
  ),
  rep(
    digest::digest(
      file = cdf_source_absolute, algo = "sha256", serialize = FALSE
    ),
    length(cdf_estimands)
  )
)
tracked_reference_schema <- c(
  as.character(mean_rows$reference_schema),
  as.character(cdf_rows$reference_schema)
)
tracked_provenance_sha256 <- c(
  as.character(mean_rows$provenance_sha256),
  as.character(cdf_rows$generator_sha256)
)
reference_method <- c(
  rep(
    paste(
      "collapsed Gamma-scale root quadrature with response-kink",
      "and root-order boundary splits"
    ),
    length(mean_estimands)
  ),
  as.character(cdf_rows$reference_method)
)
absolute_difference <- abs(reproduced_value - tracked_value)
comparison_tolerance <- c(
  rep(mean_comparison_tolerance, length(mean_estimands)),
  rep(cdf_comparison_tolerance, length(cdf_estimands))
)
pass <- is.finite(absolute_difference) &
  absolute_difference <= comparison_tolerance &
  order_convergence_difference[key] <= order_convergence_tolerance

result <- data.frame(
  schema_version = rep(schema_version, length(key)),
  fixture_id = rep(fixture_id, length(key)),
  comparison_type = comparison_type,
  estimand = estimand,
  threshold = threshold,
  tracked_reference_schema = tracked_reference_schema,
  tracked_value = tracked_value,
  reproduced_value = reproduced_value,
  absolute_difference = absolute_difference,
  comparison_tolerance = comparison_tolerance,
  quadrature_order = rep(max(orders), length(key)),
  previous_order = rep(orders[length(orders) - 1L], length(key)),
  order_convergence_difference =
    unname(order_convergence_difference[key]),
  order_convergence_tolerance =
    rep(order_convergence_tolerance, length(key)),
  reference_method = reference_method,
  tracked_source_path = tracked_source_path,
  tracked_source_sha256 = tracked_source_sha256,
  tracked_provenance_sha256 = tracked_provenance_sha256,
  generator_path = rep(generator_path, length(key)),
  generator_sha256 = rep(
    digest::digest(
      file = generator_absolute_path,
      algo = "sha256", serialize = FALSE
    ),
    length(key)
  ),
  pass = pass,
  stringsAsFactors = FALSE
)

if (!all(result$pass)) {
  failed <- paste(
    sprintf(
      "%s:%s tracked=%.17g reproduced=%.17g difference=%.3g convergence=%.3g",
      result$comparison_type[!result$pass],
      result$estimand[!result$pass],
      result$tracked_value[!result$pass],
      result$reproduced_value[!result$pass],
      result$absolute_difference[!result$pass],
      result$order_convergence_difference[!result$pass]
    ),
    collapse = "; "
  )
  stop("The F01 tracked-reference comparisons failed: ", failed, ".",
       call. = FALSE)
}

old_options <- options(digits = 17, scipen = 999)
on.exit(options(old_options), add = TRUE)
temporary <- tempfile(
  paste0(".", basename(output_path), "-"),
  tmpdir = dirname(output_path)
)
on.exit(unlink(temporary, force = TRUE), add = TRUE)
serialized_result <- result
numeric_columns <- c(
  "threshold", "tracked_value", "reproduced_value",
  "absolute_difference", "comparison_tolerance",
  "order_convergence_difference", "order_convergence_tolerance"
)
serialized_result[numeric_columns] <- lapply(
  serialized_result[numeric_columns],
  function(value) {
    ifelse(is.na(value), NA_character_, sprintf("%.17g", value))
  }
)
utils::write.csv(
  serialized_result, temporary, row.names = FALSE, na = "",
  fileEncoding = "UTF-8"
)
readback <- utils::read.csv(
  temporary, stringsAsFactors = FALSE, check.names = FALSE,
  na.strings = ""
)
if (nrow(readback) != nrow(serialized_result) ||
    !identical(names(readback), names(serialized_result)) ||
    any(as.character(readback$schema_version) != schema_version) ||
    any(as.character(readback$pass) != "TRUE")) {
  stop("The F01 output failed deterministic readback validation.",
       call. = FALSE)
}
if (!file.rename(temporary, output_path)) {
  stop("Cannot atomically publish the F01 quadrature artifact.",
       call. = FALSE)
}

cat(
  "ordinary-v1 F01 quadrature PASS\n",
  "output: ", normalizePath(output_path, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)
