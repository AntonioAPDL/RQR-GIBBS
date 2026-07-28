#!/usr/bin/env Rscript

# Deterministic population figures for the RQR article.
# This script does not fit a model, run MCMC, or simulate responses.

SCRIPT_VERSION <- "2026-07-27-restore-figure02-add-cf-figure03-1"
DEFAULT_CONTENT <- 0.80
ILLUSTRATION_AL_TAU <- 0.65
NUMERICAL_TOLERANCES <- list(
  probability_margin = 1e-8,
  integration_relative = 1e-10,
  root_absolute = 1e-10,
  identity_absolute = 2e-7,
  boundary_index = 5e-6,
  plot_margin_fraction = 0.04,
  label_anchor_fraction = 0.018,
  label_extent_fraction = 0.085
)

GENERATOR_SOURCE_PATH <- local({
  sourced <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (!is.null(sourced) &&
      identical(basename(sourced), "generate_rqr_theory_figures.R")) {
    return(normalizePath(sourced, mustWork = TRUE))
  }
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit)) {
    return(normalizePath(sub("^--file=", "", hit[1L]), mustWork = TRUE))
  }
  normalizePath("figures/generate_rqr_theory_figures.R", mustWork = FALSE)
})

fail <- function(...) stop(sprintf(...), call. = FALSE)

assert_scalar <- function(x, name, lower = -Inf, upper = Inf,
                          lower_open = FALSE, upper_open = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x)
  if (ok) {
    ok <- if (lower_open) x > lower else x >= lower
    ok <- ok && if (upper_open) x < upper else x <= upper
  }
  if (!ok) fail("%s is outside its declared domain.", name)
  invisible(x)
}

script_path <- function() {
  GENERATOR_SOURCE_PATH
}

repository_root <- function() {
  normalizePath(file.path(dirname(script_path()), ".."), mustWork = TRUE)
}

parse_single_argument <- function(args, name, default = NA_character_) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit) > 1L) fail("Use at most one --%s argument.", name)
  if (!length(hit)) return(default)
  value <- sub(paste0("^", prefix), "", hit)
  if (!nzchar(value)) fail("--%s must not be empty.", name)
  value
}

parse_generator_arguments <- function(args = commandArgs(trailingOnly = TRUE)) {
  known <- grepl(
    "^--(output-dir|source-commit|source-archive-sha256)=",
    args
  )
  if (length(args) && any(!known)) {
    fail("Unknown generator argument: %s", args[which(!known)[1L]])
  }
  out_dir <- parse_single_argument(
    args, "output-dir", file.path(tempdir(), "rqr_theory_figures")
  )
  source_commit <- parse_single_argument(args, "source-commit")
  archive_sha256 <- parse_single_argument(args, "source-archive-sha256")
  if (!is.na(source_commit) &&
      !grepl("^([[:xdigit:]]{40}|[[:xdigit:]]{64})$", source_commit)) {
    fail("--source-commit must be a full 40- or 64-character hexadecimal ID.")
  }
  if (!is.na(archive_sha256) &&
      !grepl("^[[:xdigit:]]{64}$", archive_sha256)) {
    fail("--source-archive-sha256 must be a 64-character hexadecimal digest.")
  }
  list(
    output_dir = out_dir,
    source_commit = if (is.na(source_commit)) NA_character_ else
      tolower(source_commit),
    source_archive_sha256 = if (is.na(archive_sha256)) NA_character_ else
      tolower(archive_sha256)
  )
}

git_output <- function(args, root = repository_root(), executable = "git") {
  tryCatch({
    out <- suppressWarnings(system2(
      executable, c("-C", shQuote(root), args),
      stdout = TRUE, stderr = TRUE
    ))
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    list(
      ok = identical(as.integer(status), 0L),
      status = as.integer(status),
      output = as.character(out),
      error = NA_character_
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      status = NA_integer_,
      output = character(),
      error = conditionMessage(e)
    )
  })
}

source_state <- function(declared_commit = NA_character_,
                         source_archive_sha256 = NA_character_,
                         root = repository_root(),
                         git_executable = "git") {
  commit_result <- git_output(
    c("rev-parse", "--verify", "HEAD"), root, git_executable
  )
  status_result <- git_output(
    c("status", "--porcelain", "--untracked-files=normal"),
    root, git_executable
  )
  detected_commit <- if (
    commit_result$ok && length(commit_result$output) == 1L &&
      grepl("^([[:xdigit:]]{40}|[[:xdigit:]]{64})$", commit_result$output)
  ) {
    tolower(commit_result$output)
  } else {
    NA_character_
  }
  git_state_ok <- commit_result$ok && status_result$ok &&
    !is.na(detected_commit)
  if (!is.na(declared_commit) && !is.na(detected_commit) &&
      !identical(tolower(declared_commit), detected_commit)) {
    fail(
      "Declared source commit %s does not match detected HEAD %s.",
      declared_commit, detected_commit
    )
  }
  list(
    commit = detected_commit,
    clean = if (git_state_ok) length(status_result$output) == 0L else NA,
    declared_commit = declared_commit,
    source_archive_sha256 = source_archive_sha256,
    source_identity_consistent = if (
      is.na(declared_commit) || is.na(detected_commit)
    ) {
      NA
    } else {
      identical(tolower(declared_commit), detected_commit)
    },
    rev_parse_status = commit_result$status,
    worktree_status = status_result$status
  )
}

sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    fail("Package 'digest' is required for SHA-256 manifests.")
  }
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

asymmetric_laplace_components <- function(
    mu = 0, scale = 1, tau = ILLUSTRATION_AL_TAU) {
  assert_scalar(mu, "mu")
  assert_scalar(scale, "scale", lower = 0, lower_open = TRUE)
  assert_scalar(tau, "tau", lower = 0, upper = 1,
                lower_open = TRUE, upper_open = TRUE)

  pfun <- function(y) {
    z <- (y - mu) / scale
    out <- rep(NA_real_, length(z))
    left <- !is.na(z) & z < 0
    right <- !is.na(z) & !left
    out[left] <- tau * exp((1 - tau) * z[left])
    # -expm1(.) avoids cancellation when the right-tail probability is small.
    out[right] <- -expm1(log1p(-tau) - tau * z[right])
    out
  }

  qfun <- function(p) {
    out <- rep(NaN, length(p))
    valid <- !is.na(p) & p >= 0 & p <= 1
    out[is.na(p)] <- NA_real_
    out[valid & p == 0] <- -Inf
    out[valid & p == 1] <- Inf
    left <- valid & p > 0 & p < tau
    right <- valid & p >= tau & p < 1
    out[left] <- mu + scale *
      (log(p[left]) - log(tau)) / (1 - tau)
    out[right] <- mu - scale *
      (log1p(-p[right]) - log1p(-tau)) / tau
    out
  }

  dfun <- function(y) {
    z <- (y - mu) / scale
    exponent <- ifelse(z < 0, (1 - tau) * z, -tau * z)
    tau * (1 - tau) * exp(exponent) / scale
  }

  standardized_below_moment <- function(z) {
    out <- rep(NA_real_, length(z))
    left_infinite <- !is.na(z) & is.infinite(z) & z < 0
    right_infinite <- !is.na(z) & is.infinite(z) & z > 0
    left <- !is.na(z) & is.finite(z) & z < 0
    right <- !is.na(z) & is.finite(z) & z >= 0
    out[left_infinite] <- 0
    out[right_infinite] <- (1 - 2 * tau) / (tau * (1 - tau))
    out[left] <- tau * exp((1 - tau) * z[left]) *
      (z[left] - 1 / (1 - tau))
    if (any(right)) {
      zr <- z[right]
      positive_part <- (1 - tau) / tau * (
        -expm1(-tau * zr) - tau * zr * exp(-tau * zr)
      )
      out[right] <- -tau / (1 - tau) + positive_part
    }
    out
  }

  truncated_below <- function(y) {
    mu * pfun(y) + scale *
      standardized_below_moment((y - mu) / scale)
  }

  positive_tail_first_moment <- function(z) {
    out <- rep(NA_real_, length(z))
    finite <- !is.na(z) & is.finite(z)
    out[finite] <- (1 - tau) * exp(-tau * z[finite]) *
      (z[finite] + 1 / tau)
    out[!is.na(z) & is.infinite(z) & z > 0] <- 0
    out
  }

  moment_between <- function(lower, upper) {
    if (length(lower) != 1L || length(upper) != 1L ||
        is.na(lower) || is.na(upper) || lower > upper) {
      fail("AL truncated-moment limits must be ordered scalars.")
    }
    zl <- (lower - mu) / scale
    zu <- (upper - mu) / scale
    probability <- pfun(upper) - pfun(lower)
    standardized_moment <- if (zu <= 0) {
      standardized_below_moment(zu) - standardized_below_moment(zl)
    } else if (zl >= 0) {
      positive_tail_first_moment(zl) -
        positive_tail_first_moment(zu)
    } else {
      (
        standardized_below_moment(0) -
          standardized_below_moment(zl)
      ) + (
        positive_tail_first_moment(0) -
          positive_tail_first_moment(zu)
      )
    }
    mu * probability + scale * standardized_moment
  }

  mean <- mu + scale * (1 - 2 * tau) / (tau * (1 - tau))
  raw_second <- 2 * (
    tau / (1 - tau)^2 + (1 - tau) / tau^2
  )
  raw_third <- 6 * (
    (1 - tau) / tau^3 - tau / (1 - tau)^3
  )
  variance <- scale^2 * (1 - 2 * tau + 2 * tau^2) /
    (tau^2 * (1 - tau)^2)
  standardized_mean <- (1 - 2 * tau) / (tau * (1 - tau))
  standardized_central_third <- raw_third -
    3 * standardized_mean * raw_second + 2 * standardized_mean^3
  skewness <- standardized_central_third /
    (variance / scale^2)^(3 / 2)
  list(
    q = qfun,
    p = pfun,
    d = dfun,
    moment_between = moment_between,
    truncated_below = truncated_below,
    standardized_below_moment = standardized_below_moment,
    mean = mean,
    variance = variance,
    sd = sqrt(variance),
    skewness = skewness,
    mu = mu,
    scale = scale,
    tau = tau
  )
}

make_distribution <- function(id, label, short_label, subtitle, qfun, pfun,
                              dfun, moment_fun, mean, sd,
                              support = c(-Inf, Inf),
                              plot_knots = numeric(),
                              plot_probabilities = c(0.005, 0.995),
                              skewness = NA_real_,
                              raw_parameters = "") {
  funs <- list(qfun, pfun, dfun, moment_fun)
  if (!all(vapply(funs, is.function, logical(1)))) {
    fail("Distribution %s must provide q, p, d, and truncated-moment functions.",
         id)
  }
  assert_scalar(mean, paste0(id, "$mean"))
  assert_scalar(sd, paste0(id, "$sd"), lower = 0, lower_open = TRUE)
  if (!is.na(skewness)) assert_scalar(skewness, paste0(id, "$skewness"))
  if (length(plot_probabilities) != 2L ||
      any(!is.finite(plot_probabilities)) ||
      plot_probabilities[1L] <= 0 ||
      plot_probabilities[2L] >= 1 ||
      plot_probabilities[1L] >= plot_probabilities[2L]) {
    fail("Distribution %s has invalid plotting probabilities.", id)
  }
  list(
    id = id, label = label, short_label = short_label, subtitle = subtitle,
    q = qfun, p = pfun, d = dfun, moment_between = moment_fun,
    mean = mean, sd = sd, support = support,
    skewness = skewness,
    plot_knots = plot_knots,
    plot_probabilities = plot_probabilities,
    raw_parameters = raw_parameters
  )
}

distribution_descriptor <- function(dist) {
  if (nzchar(dist$raw_parameters)) {
    sprintf("%s[%s]", dist$id, dist$raw_parameters)
  } else {
    dist$id
  }
}

rqr_theory_distributions <- function(content = DEFAULT_CONTENT) {
  assert_scalar(
    content, "content", lower = 0, upper = 1,
    lower_open = TRUE, upper_open = TRUE
  )
  al <- asymmetric_laplace_components(
    mu = 0, scale = 1, tau = ILLUSTRATION_AL_TAU
  )
  normal_moment <- function(lower, upper) {
    stats::dnorm(lower) - stats::dnorm(upper)
  }
  exponential_term <- function(x) {
    ifelse(is.infinite(x) & x > 0, 0, (x + 1) * exp(-x))
  }
  exponential_moment <- function(lower, upper) {
    exponential_term(lower) - exponential_term(upper)
  }
  lognormal_moment <- function(lower, upper) {
    sigma <- 0.6
    zfun <- function(x) {
      ifelse(
        x <= 0, -Inf,
        ifelse(is.infinite(x) & x > 0, Inf, (log(x) - sigma^2) / sigma)
      )
    }
    exp(sigma^2 / 2) *
      (stats::pnorm(zfun(upper)) - stats::pnorm(zfun(lower)))
  }
  beta_moment <- function(lower, upper) {
    (2 / 7) * (
      stats::pbeta(upper, shape1 = 3, shape2 = 5) -
        stats::pbeta(lower, shape1 = 3, shape2 = 5)
    )
  }
  beta_skewness <- function(a, b) {
    2 * (b - a) * sqrt(a + b + 1) / ((a + b + 2) * sqrt(a * b))
  }
  lognormal_skewness <- function(sigma) {
    (exp(sigma^2) + 2) * sqrt(exp(sigma^2) - 1)
  }
  list(
    normal = make_distribution(
      "normal", "Normal(0, 1)", "Normal", "symmetric unimodal benchmark",
      stats::qnorm, stats::pnorm, stats::dnorm, normal_moment, 0, 1,
      plot_knots = 0, skewness = 0
    ),
    exponential = make_distribution(
      "exponential", "Exponential(1)", "Exponential",
      "support-boundary shortest interval",
      function(p) stats::qexp(p, rate = 1),
      function(y) stats::pexp(y, rate = 1),
      function(y) stats::dexp(y, rate = 1),
      exponential_moment, 1, 1, c(0, Inf),
      plot_knots = 0, skewness = 2
    ),
    asymmetric_laplace = make_distribution(
      "asymmetric_laplace",
      sprintf(
        "Asymmetric Laplace(mu=0, scale=1, tau=%.2f)",
        ILLUSTRATION_AL_TAU
      ),
      "Left-skewed",
      bquote(
        "Left-skewed illustration;" ~~
          "interval content" ~~ c == .(content)
      ),
      al$q, al$p, al$d, al$moment_between, al$mean, al$sd,
      c(-Inf, Inf),
      plot_knots = al$mu,
      plot_probabilities = c(0.005, 0.995),
      skewness = al$skewness,
      raw_parameters = sprintf(
        "mu_AL=0,s_AL=1,tau_AL=%.2f", ILLUSTRATION_AL_TAU
      )
    ),
    lognormal = make_distribution(
      "lognormal", "Lognormal(meanlog=0, sdlog=0.6)", "Lognormal(0, 0.6)",
      "heavier right tail",
      function(p) stats::qlnorm(p, meanlog = 0, sdlog = 0.6),
      function(y) stats::plnorm(y, meanlog = 0, sdlog = 0.6),
      function(y) stats::dlnorm(y, meanlog = 0, sdlog = 0.6),
      lognormal_moment,
      exp(0.6^2 / 2),
      sqrt((exp(0.6^2) - 1) * exp(0.6^2)),
      c(0, Inf), plot_knots = exp(-0.6^2),
      skewness = lognormal_skewness(0.6)
    ),
    beta25 = make_distribution(
      "beta25", "Beta(2, 5)", "Beta(2, 5)", "bounded right skew",
      function(p) stats::qbeta(p, shape1 = 2, shape2 = 5),
      function(y) stats::pbeta(y, shape1 = 2, shape2 = 5),
      function(y) stats::dbeta(y, shape1 = 2, shape2 = 5),
      beta_moment, 2 / 7, sqrt(2 * 5 / (7^2 * 8)), c(0, 1),
      plot_knots = 1 / 5, skewness = beta_skewness(2, 5)
    )
  )
}

mirror_distribution <- function(dist, id = paste0("mirror_", dist$id)) {
  make_distribution(
    id = id,
    label = paste0("Mirror of ", dist$label),
    short_label = paste0("Mirror ", dist$short_label),
    subtitle = paste0("reflection of ", dist$id),
    qfun = function(p) -dist$q(1 - p),
    pfun = function(y) 1 - dist$p(-y),
    dfun = function(y) dist$d(-y),
    moment_fun = function(lower, upper) {
      -dist$moment_between(-upper, -lower)
    },
    mean = -dist$mean,
    sd = dist$sd,
    support = -rev(dist$support),
    plot_knots = -dist$plot_knots,
    plot_probabilities = 1 - rev(dist$plot_probabilities),
    skewness = -dist$skewness,
    raw_parameters = paste0("reflection_of=", dist$id)
  )
}

affine_distribution <- function(dist, shift, scale,
                                id = paste0("affine_", dist$id)) {
  assert_scalar(shift, "shift")
  assert_scalar(scale, "scale", lower = 0, lower_open = TRUE)
  make_distribution(
    id = id,
    label = paste0("Affine transform of ", dist$label),
    short_label = paste0("Affine ", dist$short_label),
    subtitle = sprintf("shift=%g, scale=%g", shift, scale),
    qfun = function(p) shift + scale * dist$q(p),
    pfun = function(y) dist$p((y - shift) / scale),
    dfun = function(y) dist$d((y - shift) / scale) / scale,
    moment_fun = function(lower, upper) {
      lower0 <- (lower - shift) / scale
      upper0 <- (upper - shift) / scale
      probability <- dist$p(upper0) - dist$p(lower0)
      shift * probability +
        scale * dist$moment_between(lower0, upper0)
    },
    mean = shift + scale * dist$mean,
    sd = scale * dist$sd,
    support = shift + scale * dist$support,
    plot_knots = shift + scale * dist$plot_knots,
    plot_probabilities = dist$plot_probabilities,
    skewness = dist$skewness,
    raw_parameters = sprintf(
      "affine_of=%s;shift=%.12g;scale=%.12g", dist$id, shift, scale
    )
  )
}

quantile_window <- function(dist, u, content = DEFAULT_CONTENT) {
  assert_scalar(content, "content", 0, 1, TRUE, TRUE)
  assert_scalar(u, "u", 0, 1 - content)
  lower <- dist$q(u)
  upper <- dist$q(u + content)
  if (!is.finite(lower) && u > 0) fail("Nonfinite lower endpoint at interior u.")
  if (!is.finite(upper) && u + content < 1) {
    fail("Nonfinite upper endpoint at interior u.")
  }
  list(lower = lower, upper = upper, width = upper - lower)
}

window_content <- function(dist, interval) {
  ans <- dist$p(interval$upper) - dist$p(interval$lower)
  if (!is.finite(ans)) fail("Nonfinite content for %s.", dist$id)
  ans
}

quantile_window_mean <- function(dist, u, content = DEFAULT_CONTENT) {
  interval <- quantile_window(dist, u, content)
  probability <- window_content(dist, interval)
  if (abs(probability - content) >
      NUMERICAL_TOLERANCES$identity_absolute) {
    fail("Content identity failed before computing the retained mean for %s.",
         dist$id)
  }
  ans <- dist$moment_between(interval$lower, interval$upper) / probability
  if (!is.finite(ans)) {
    fail("Nonfinite quantile-window mean for %s at u=%g.", dist$id, u)
  }
  ans
}

solve_window_for_tilt <- function(dist, delta, content = DEFAULT_CONTENT) {
  assert_scalar(delta, "delta")
  target <- dist$mean + delta
  upper_u <- 1 - content
  objective <- function(u) quantile_window_mean(dist, u, content) - target
  f0 <- objective(0)
  f1 <- objective(upper_u)
  tol <- NUMERICAL_TOLERANCES$identity_absolute
  if (f0 > tol || f1 < -tol) {
    fail("Tilt %g is outside the admissible range for %s.", delta, dist$id)
  }
  if (abs(f0) <= tol) return(0)
  if (abs(f1) <= tol) return(upper_u)
  stats::uniroot(
    objective, interval = c(0, upper_u),
    tol = NUMERICAL_TOLERANCES$root_absolute
  )$root
}

shortest_contiguous_window <- function(dist, content = DEFAULT_CONTENT) {
  assert_scalar(content, "content", 0, 1, TRUE, TRUE)
  max_u <- 1 - content
  width <- function(u) quantile_window(dist, u, content)$width
  margin <- min(
    NUMERICAL_TOLERANCES$probability_margin,
    max_u / 1000
  )
  opt <- stats::optimize(
    width, interval = c(margin, max_u - margin),
    tol = NUMERICAL_TOLERANCES$root_absolute
  )
  candidates <- data.frame(
    u = c(0, opt$minimum, max_u),
    width = c(width(0), opt$objective, width(max_u))
  )
  if (all(!is.finite(candidates$width))) {
    fail("No finite shortest-window candidate for %s.", dist$id)
  }
  idx <- which.min(candidates$width)
  u_star <- candidates$u[idx]
  boundary_tol <- NUMERICAL_TOLERANCES$boundary_index
  status <- if (u_star <= boundary_tol) {
    "lower_support_boundary"
  } else if (max_u - u_star <= boundary_tol) {
    "upper_support_boundary"
  } else {
    "interior"
  }
  list(
    u = u_star,
    interval = quantile_window(dist, u_star, content),
    status = status,
    candidates = candidates
  )
}

oracle_interval_summary <- function(dist, content = DEFAULT_CONTENT) {
  u_et <- (1 - content) / 2
  u_rqr <- solve_window_for_tilt(dist, delta = 0, content = content)
  shortest <- shortest_contiguous_window(dist, content)
  specs <- list(
    shortest = list(u = shortest$u, status = shortest$status),
    equal_tailed = list(u = u_et, status = "fixed_probability"),
    ordinary_rqr = list(u = u_rqr, status = "mean_preserving")
  )
  rows <- lapply(names(specs), function(target) {
    spec <- specs[[target]]
    interval <- quantile_window(dist, spec$u, content)
    retained_mean <- quantile_window_mean(dist, spec$u, content)
    data.frame(
      distribution = dist$id,
      target = target,
      u = spec$u,
      lower = interval$lower,
      upper = interval$upper,
      content = window_content(dist, interval),
      lower_tail_probability = dist$p(interval$lower),
      upper_tail_probability = 1 - dist$p(interval$upper),
      lower_endpoint_density = dist$d(interval$lower),
      upper_endpoint_density = dist$d(interval$upper),
      width = interval$width,
      retained_mean = retained_mean,
      delta = retained_mean - dist$mean,
      standardized_delta = (retained_mean - dist$mean) / dist$sd,
      standardized_width = interval$width / dist$sd,
      optimum_status = spec$status,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

cornish_fisher_constant <- function(content = DEFAULT_CONTENT) {
  assert_scalar(content, "content", 0, 1, TRUE, TRUE)
  q_c <- stats::qnorm((1 + content) / 2)
  q_c * stats::dnorm(q_c) / content
}

cornish_fisher_tilt_summary <- function(dist, summary,
                                        content = DEFAULT_CONTENT) {
  if (is.null(dist$skewness) || is.na(dist$skewness)) {
    fail("Distribution %s does not declare a skewness.", dist$id)
  }
  constant <- cornish_fisher_constant(content)
  specs <- data.frame(
    approximation = c(
      "cornish_fisher_equal_tailed", "cornish_fisher_shortest"
    ),
    target = c("equal_tailed", "shortest"),
    multiplier = c(1 / 3, 1),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(specs)), function(index) {
    target <- specs$target[[index]]
    standardized_delta <- -dist$skewness * constant *
      specs$multiplier[[index]]
    delta <- standardized_delta * dist$sd
    solved <- tryCatch({
      u <- solve_window_for_tilt(dist, delta, content)
      interval <- quantile_window(dist, u, content)
      retained_mean <- quantile_window_mean(dist, u, content)
      list(
        within_admissible = TRUE,
        u = u,
        lower = interval$lower,
        upper = interval$upper,
        width = interval$width,
        retained_mean = retained_mean,
        standardized_width = interval$width / dist$sd
      )
    }, error = function(e) {
      list(
        within_admissible = FALSE,
        u = NA_real_, lower = NA_real_, upper = NA_real_,
        width = NA_real_, retained_mean = NA_real_,
        standardized_width = NA_real_
      )
    })
    oracle <- summary[summary$target == target, , drop = FALSE]
    data.frame(
      distribution = dist$id,
      approximation = specs$approximation[[index]],
      target = target,
      skewness = dist$skewness,
      cf_constant = constant,
      delta = delta,
      standardized_delta = standardized_delta,
      u = solved$u,
      lower = solved$lower,
      upper = solved$upper,
      width = solved$width,
      retained_mean = solved$retained_mean,
      standardized_width = solved$standardized_width,
      within_admissible = solved$within_admissible,
      oracle_standardized_delta = oracle$standardized_delta,
      standardized_delta_error =
        standardized_delta - oracle$standardized_delta,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

check_oracle_summary <- function(dist, summary, content = DEFAULT_CONTENT) {
  tol <- NUMERICAL_TOLERANCES$identity_absolute
  if (any(abs(summary$content - content) > tol)) {
    fail("Content identity failed for %s.", dist$id)
  }
  rqr <- summary[summary$target == "ordinary_rqr", , drop = FALSE]
  if (nrow(rqr) != 1L || abs(rqr$retained_mean - dist$mean) > tol) {
    fail("Mean-preserving identity failed for %s.", dist$id)
  }
  invisible(TRUE)
}

standardize_response <- function(dist, y) {
  (y - dist$mean) / dist$sd
}

interval_plot_geometry <- function(dist, summary, labels = TRUE) {
  endpoints <- standardize_response(
    dist, c(summary$lower, summary$upper)
  )
  quantile_limits <- standardize_response(
    dist, dist$q(dist$plot_probabilities)
  )
  knots <- standardize_response(dist, dist$plot_knots)
  core <- c(quantile_limits, endpoints, 0, knots)
  core <- core[is.finite(core)]
  if (length(core) < 2L || diff(range(core)) <= 0) {
    fail("Could not form a finite plotting domain for %s.", dist$id)
  }
  base_span <- diff(range(core))
  upper <- setNames(
    standardize_response(dist, summary$upper), summary$target
  )
  label_anchors <- upper +
    NUMERICAL_TOLERANCES$label_anchor_fraction * base_span
  label_extents <- label_anchors +
    NUMERICAL_TOLERANCES$label_extent_fraction * base_span
  if (labels) core <- c(core, label_anchors, label_extents)
  core_range <- range(core[is.finite(core)])
  margin <- NUMERICAL_TOLERANCES$plot_margin_fraction * diff(core_range)
  zlim <- core_range + c(-margin, margin)
  required <- c(endpoints, quantile_limits, 0, knots)
  if (labels) required <- c(required, label_anchors, label_extents)
  if (any(required < zlim[1L] | required > zlim[2L])) {
    fail("Adaptive plotting domain clips required geometry for %s.", dist$id)
  }
  list(
    zlim = zlim,
    label_anchors = label_anchors,
    label_extents = label_extents,
    standardized_endpoints = endpoints,
    standardized_quantile_limits = quantile_limits,
    standardized_knots = knots
  )
}

standardized_density_data <- function(dist, summary = NULL, geometry = NULL,
                                      n = 851L) {
  if (is.null(geometry)) {
    if (is.null(summary)) {
      summary <- oracle_interval_summary(dist, DEFAULT_CONTENT)
    }
    geometry <- interval_plot_geometry(dist, summary)
  }
  zlim <- geometry$zlim
  special <- c(
    0,
    geometry$standardized_endpoints,
    geometry$standardized_quantile_limits,
    geometry$standardized_knots
  )
  special <- special[is.finite(special) &
                       special >= zlim[1L] & special <= zlim[2L]]
  z <- sort(unique(c(
    seq(zlim[1L], zlim[2L], length.out = n),
    special
  )))
  y <- dist$mean + dist$sd * z
  density <- dist$sd * dist$d(y)
  if (any(!is.finite(density))) {
    fail("Nonfinite standardized density for %s.", dist$id)
  }
  data.frame(z = z, density = density, distribution = dist$id)
}

mean_tilt_path_data <- function(dist, content, n = 401L) {
  max_u <- 1 - content
  margin <- min(NUMERICAL_TOLERANCES$probability_margin, max_u / 1000)
  u <- seq(margin, max_u - margin, length.out = n)
  retained_mean <- vapply(
    u, quantile_window_mean, numeric(1),
    dist = dist, content = content
  )
  width <- vapply(
    u,
    function(ui) quantile_window(dist, ui, content)$width,
    numeric(1)
  )
  data.frame(
    u = u,
    M_minus_mu = retained_mean - dist$mean,
    standardized_delta = (retained_mean - dist$mean) / dist$sd,
    width = width,
    standardized_width = width / dist$sd,
    stringsAsFactors = FALSE
  )
}

mean_tilt_loss <- function(y, root1, root2, content, delta) {
  residual_product <- (y - root1) * (y - root2)
  check_loss <- residual_product *
    (content - as.numeric(residual_product < 0))
  check_loss - content * delta * (root1 + root2 - 2 * y)
}

COL <- c(
  shortest = "#D97706",
  equal_tailed = "#238B57",
  ordinary_rqr = "#2563EB",
  density = "#1F2933",
  mean = "#65727E",
  tilt = "#7C3AED"
)
PCH <- c(equal_tailed = 15, ordinary_rqr = 16, shortest = 17)
OPEN_PCH <- c(equal_tailed = 0, ordinary_rqr = 1, shortest = 2)
TARGET_ORDER <- c("equal_tailed", "ordinary_rqr", "shortest")
INTERVAL_SEGMENT_LTY <- 1L
TARGET_LABEL <- c(
  equal_tailed = "ET",
  ordinary_rqr = "RQR",
  shortest = "SH"
)
CF_TARGET_ORDER <- c("cornish_fisher_equal_tailed", "cornish_fisher_shortest")
CF_TARGET_LABEL <- c(
  cornish_fisher_equal_tailed = "CF-ET",
  cornish_fisher_shortest = "CF-SH"
)
CF_TARGET_TO_ORACLE <- c(
  cornish_fisher_equal_tailed = "equal_tailed",
  cornish_fisher_shortest = "shortest"
)
FIGURE_02_MAP_LABEL_POSITION <- c(
  equal_tailed = 4L,
  ordinary_rqr = 1L,
  shortest = 2L
)
FIGURE_02_WIDTH_LABEL_POSITION <- c(
  equal_tailed = 4L,
  ordinary_rqr = 4L,
  shortest = 3L
)
FIGURE_02_LABEL_OFFSET <- 0.45
FIGURE_02_CF_LABEL_OFFSET <- 0.35

figure_02_map_label <- function(target, standardized_delta) {
  if (!target %in% TARGET_ORDER) {
    fail("Unknown Figure 2 target label '%s'.", target)
  }
  if (identical(target, "ordinary_rqr")) {
    return(sprintf("%s %.2f", TARGET_LABEL[target], 0))
  }
  sprintf("%s %.3f", TARGET_LABEL[target], standardized_delta)
}

figure_02_cf_color <- function(approximation) {
  target <- CF_TARGET_TO_ORACLE[approximation]
  COL[target]
}

figure_02_cf_pch <- function(approximation) {
  target <- CF_TARGET_TO_ORACLE[approximation]
  OPEN_PCH[target]
}

draw_to_device <- function(open_device, draw_fun) {
  open_device()
  tryCatch(
    draw_fun(),
    finally = {
      if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    }
  )
}

with_graphics_devices <- function(base_path, width, height, draw_fun,
                                  preview_dpi = 300) {
  pdf_path <- paste0(base_path, ".pdf")
  png_path <- paste0(base_path, ".png")
  draw_to_device(
    function() grDevices::pdf(
      pdf_path, width = width, height = height, useDingbats = FALSE
    ),
    draw_fun
  )
  draw_to_device(
    function() grDevices::png(
      png_path,
      width = round(width * preview_dpi),
      height = round(height * preview_dpi),
      res = preview_dpi
    ),
    draw_fun
  )
  c(pdf = pdf_path, preview = png_path)
}

write_panel_data <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE, na = "")
  path
}

plot_interval_bars <- function(dist, summary, geometry, y0, dy,
                               labels = TRUE, targets = TARGET_ORDER) {
  for (i in seq_along(targets)) {
    target <- targets[i]
    row <- summary[summary$target == target, , drop = FALSE]
    lower_z <- (row$lower - dist$mean) / dist$sd
    upper_z <- (row$upper - dist$mean) / dist$sd
    yy <- y0 - (i - 1) * dy
    graphics::segments(
      lower_z, yy, upper_z, yy,
      lwd = 4.4, lty = INTERVAL_SEGMENT_LTY,
      col = COL[target], lend = "round"
    )
    graphics::points(
      c(lower_z, upper_z), c(yy, yy),
      pch = PCH[target], col = COL[target], cex = 0.62
    )
    if (labels) {
      label <- TARGET_LABEL[target]
      graphics::text(
        geometry$label_anchors[target], yy, label, adj = c(0, 0.5),
        cex = 0.66, col = COL[target]
      )
    }
  }
}

shade_interval <- function(density, lower_z, upper_z, color) {
  inside <- density$z >= lower_z & density$z <= upper_z
  x <- density$z[inside]
  y <- density$density[inside]
  if (length(x) > 1L) {
    graphics::polygon(
      c(x, rev(x)), c(rep(0, length(x)), rev(y)),
      border = NA, col = grDevices::adjustcolor(color, alpha.f = 0.20)
    )
  }
}

figure_01_three_principles <- function(out_dir, dist, content) {
  summary <- oracle_interval_summary(dist, content)
  check_oracle_summary(dist, summary, content)
  geometry <- interval_plot_geometry(dist, summary)
  density <- standardized_density_data(
    dist, summary = summary, geometry = geometry
  )
  panel_files <- c(
    write_panel_data(
      transform(
        summary[summary$target == "equal_tailed", ],
        panel = "A_equal_tailed"
      ),
      file.path(out_dir, "fig01_panelA_equal_tailed.csv")
    ),
    write_panel_data(
      transform(
        summary[summary$target == "ordinary_rqr", ],
        panel = "B_rqr"
      ),
      file.path(out_dir, "fig01_panelB_rqr.csv")
    ),
    write_panel_data(
      transform(
        summary[summary$target == "shortest", ],
        panel = "C_shortest"
      ),
      file.path(out_dir, "fig01_panelC_shortest.csv")
    ),
    write_panel_data(density, file.path(out_dir, "fig01_density.csv"))
  )
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old))
    graphics::par(
      mfrow = c(1, 3), mar = c(4.0, 3.5, 3.6, 0.7),
      oma = c(0, 0, 1.15, 0), mgp = c(2.2, 0.65, 0), tcl = -0.3
    )
    targets <- TARGET_ORDER
    titles <- c("Equal-tailed", "Ordinary RQR", "Shortest contiguous")
    subtitles <- c(
      "Balances omitted probabilities",
      "Balances omitted first moments",
      "Minimizes geometric width"
    )
    annotations <- list(
      expression(P(Y < L) == 0.10 ~~ "and" ~~ P(Y > U) == 0.10),
      "retained mean = population mean",
      expression(f(L) == f(U))
    )
    for (j in seq_along(targets)) {
      target <- targets[j]
      row <- summary[summary$target == target, , drop = FALSE]
      graphics::plot(
        density$z, density$density, type = "l", lwd = 2.1,
        col = COL["density"], xlab = "Standardized response, z",
        ylab = "Density", xlim = geometry$zlim,
        ylim = c(0, max(density$density) * 1.12),
        main = titles[j], cex.main = 0.98
      )
      graphics::mtext(subtitles[j], side = 3, line = 0.25, cex = 0.70)
      graphics::abline(v = 0, lty = 1, lwd = 0.75, col = COL["mean"])
      lower_z <- (row$lower - dist$mean) / dist$sd
      upper_z <- (row$upper - dist$mean) / dist$sd
      shade_interval(density, lower_z, upper_z, COL[target])
      bar_y <- 0.018 * max(density$density)
      graphics::segments(
        lower_z, bar_y, upper_z, bar_y,
        lwd = 5.0, lty = INTERVAL_SEGMENT_LTY,
        col = COL[target], lend = "round"
      )
      graphics::points(
        c(lower_z, upper_z), c(bar_y, bar_y),
        pch = PCH[target], col = COL[target], cex = 0.84
      )
      graphics::text(
        geometry$label_anchors[target], bar_y,
        labels = TARGET_LABEL[target], adj = c(0, 0.5),
        cex = 0.68, col = COL[target]
      )
      graphics::legend(
        "topright", legend = annotations[[j]],
        bty = "n", cex = 0.72
      )
    }
    graphics::mtext(
      dist$subtitle, side = 3, outer = TRUE, line = -0.20, cex = 0.72
    )
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "fig01_three_balance_principles"),
    7.2, 3.10, draw
  )
  list(
    id = "fig01_three_balance_principles",
    data = panel_files, outputs = outputs,
    distributions = distribution_descriptor(dist)
  )
}

figure_02_mean_tilt_map <- function(out_dir, dist, content) {
  summary <- oracle_interval_summary(dist, content)
  check_oracle_summary(dist, summary, content)
  path <- mean_tilt_path_data(dist, content)
  geometry <- interval_plot_geometry(dist, summary)
  density <- standardized_density_data(
    dist, summary = summary, geometry = geometry
  )
  panel_files <- c(
    write_panel_data(
      path[, c("u", "M_minus_mu", "standardized_delta")],
      file.path(out_dir, "fig02_panelA_window_mean.csv")
    ),
    write_panel_data(
      path[, c("standardized_delta", "standardized_width")],
      file.path(out_dir, "fig02_panelB_width_profile.csv")
    ),
    write_panel_data(
      summary, file.path(out_dir, "fig02_panelC_selected_intervals.csv")
    ),
    write_panel_data(density, file.path(out_dir, "fig02_density.csv"))
  )
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old))
    graphics::par(
      mfrow = c(1, 3), mar = c(4.2, 3.8, 2.7, 0.7),
      oma = c(0, 0, 1.15, 0), mgp = c(2.3, 0.65, 0), tcl = -0.3
    )
    delta_min <- min(summary$standardized_delta) - 0.06
    delta_max <- max(0.20, max(summary$standardized_delta) + 0.05)
    visible <- is.finite(path$standardized_width) &
      path$standardized_delta >= delta_min &
      path$standardized_delta <= delta_max
    plot_path <- path[visible, , drop = FALSE]
    graphics::plot(
      path$u, path$standardized_delta, type = "l", lwd = 2,
      col = COL["density"], xlab = "Lower-tail index, u",
      ylab = expression(d == (M[c](u) - mu) / SD(Y)),
      main = "Window-to-tilt map"
    )
    graphics::abline(h = 0, lty = 1, lwd = 0.75, col = COL["mean"])
    for (target in TARGET_ORDER) {
      row <- summary[summary$target == target, ]
      graphics::points(
        row$u, row$standardized_delta,
        pch = PCH[target], col = COL[target], cex = 1.05
      )
      graphics::text(
        row$u, row$standardized_delta,
        labels = figure_02_map_label(target, row$standardized_delta),
        pos = FIGURE_02_MAP_LABEL_POSITION[target],
        offset = FIGURE_02_LABEL_OFFSET, cex = 0.64,
        col = COL[target]
      )
    }
    graphics::plot(
      plot_path$standardized_delta, plot_path$standardized_width,
      type = "l", lwd = 2, col = COL["density"],
      xlab = expression(d == delta / SD(Y)),
      ylab = expression((U - L) / SD(Y)),
      main = "Width near target tilts", xlim = c(delta_min, delta_max)
    )
    graphics::abline(v = 0, lty = 1, lwd = 0.75, col = COL["mean"])
    for (target in TARGET_ORDER) {
      row <- summary[summary$target == target, ]
      graphics::points(
        row$standardized_delta, row$standardized_width,
        pch = PCH[target], col = COL[target], cex = 1.05
      )
      graphics::text(
        row$standardized_delta, row$standardized_width,
        labels = TARGET_LABEL[target],
        pos = FIGURE_02_WIDTH_LABEL_POSITION[target],
        offset = FIGURE_02_LABEL_OFFSET, cex = 0.68,
        col = COL[target]
      )
    }
    ymax <- max(density$density)
    graphics::plot(
      density$z, density$density, type = "l", lwd = 2,
      col = COL["density"], xlab = "Standardized response, z",
      ylab = "Density", main = "Selected intervals",
      xlim = geometry$zlim, ylim = c(-0.34 * ymax, 1.06 * ymax)
    )
    graphics::abline(v = 0, lty = 1, lwd = 0.75, col = COL["mean"])
    plot_interval_bars(
      dist, summary, geometry, -0.055 * ymax, 0.075 * ymax,
      labels = TRUE
    )
    graphics::mtext(
      dist$subtitle, side = 3, outer = TRUE, line = -0.20, cex = 0.72
    )
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "fig02_mean_tilt_recovery_map"),
    7.2, 3.22, draw
  )
  list(
    id = "fig02_mean_tilt_recovery_map",
    data = panel_files, outputs = outputs,
    distributions = distribution_descriptor(dist)
  )
}

figure_03_mean_tilt_cf_anchors <- function(out_dir, dist, content) {
  summary <- oracle_interval_summary(dist, content)
  check_oracle_summary(dist, summary, content)
  cf_summary <- cornish_fisher_tilt_summary(dist, summary, content)
  if (!all(cf_summary$within_admissible)) {
    fail(
      "Figure 3 requires admissible Cornish-Fisher anchors; ",
      "out-of-range anchors: ",
      paste(cf_summary$approximation[!cf_summary$within_admissible], collapse = ", ")
    )
  }
  path <- mean_tilt_path_data(dist, content)
  panel_files <- c(
    write_panel_data(
      path[, c("u", "M_minus_mu", "standardized_delta")],
      file.path(out_dir, "fig03_panelA_window_mean.csv")
    ),
    write_panel_data(
      path[, c("standardized_delta", "standardized_width")],
      file.path(out_dir, "fig03_panelB_width_profile.csv")
    ),
    write_panel_data(
      cf_summary, file.path(out_dir, "fig03_panelC_cornish_fisher_anchors.csv")
    )
  )
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old))
    graphics::par(
      mfrow = c(1, 2), mar = c(4.2, 4.2, 2.7, 0.9),
      oma = c(0, 0, 1.10, 0), mgp = c(2.35, 0.65, 0), tcl = -0.3
    )
    admissible_cf_delta <- cf_summary$standardized_delta[
      is.finite(cf_summary$standardized_delta) &
        cf_summary$within_admissible
    ]
    all_cf_delta <- cf_summary$standardized_delta[
      is.finite(cf_summary$standardized_delta)
    ]
    target_delta_min <- min(summary$standardized_delta, admissible_cf_delta) -
      0.06
    target_delta_max <- max(
      0.20, max(summary$standardized_delta, admissible_cf_delta) + 0.05
    )
    width_x_min <- min(target_delta_min, all_cf_delta - 0.04)
    width_x_max <- max(target_delta_max, all_cf_delta + 0.04)
    visible <- is.finite(path$standardized_width) &
      path$standardized_delta >= target_delta_min &
      path$standardized_delta <= target_delta_max
    plot_path <- path[visible, , drop = FALSE]
    map_ylim <- range(
      c(path$standardized_delta, cf_summary$standardized_delta),
      finite = TRUE
    )
    map_pad <- 0.06 * diff(map_ylim)
    if (!is.finite(map_pad) || map_pad <= 0) map_pad <- 0.05
    map_xlim <- range(path$u, finite = TRUE)
    map_xlim[2L] <- map_xlim[2L] + 0.14 * diff(map_xlim)
    graphics::plot(
      path$u, path$standardized_delta, type = "l", lwd = 2.2,
      col = COL["density"], xlab = "Lower-tail index, u",
      ylab = expression(d == (M[c](u) - mu) / SD(Y)),
      main = "Window-to-tilt map",
      xlim = map_xlim,
      ylim = map_ylim + c(-map_pad, map_pad)
    )
    graphics::abline(h = 0, lty = 1, lwd = 0.75, col = COL["mean"])
    for (target in TARGET_ORDER) {
      row <- summary[summary$target == target, ]
      graphics::points(
        row$u, row$standardized_delta,
        pch = PCH[target], col = COL[target], cex = 1.12
      )
      graphics::text(
        row$u, row$standardized_delta,
        labels = figure_02_map_label(target, row$standardized_delta),
        pos = FIGURE_02_MAP_LABEL_POSITION[target],
        offset = FIGURE_02_LABEL_OFFSET, cex = 0.72,
        col = COL[target]
      )
    }
    for (approximation in CF_TARGET_ORDER) {
      row <- cf_summary[
        cf_summary$approximation == approximation,
        , drop = FALSE
      ]
      if (!nrow(row)) next
      graphics::points(
        row$u, row$standardized_delta,
        pch = figure_02_cf_pch(approximation),
        col = figure_02_cf_color(approximation),
        cex = 1.20, lwd = 1.7
      )
      graphics::text(
        row$u, row$standardized_delta,
        labels = CF_TARGET_LABEL[approximation],
        pos = if (identical(approximation, "cf_shortest")) 4L else 2L,
        offset = 0.62, cex = 0.62,
        col = figure_02_cf_color(approximation)
      )
    }
    graphics::plot(
      plot_path$standardized_delta, plot_path$standardized_width,
      type = "l", lwd = 2.2, col = COL["density"],
      xlab = expression(d == delta / SD(Y)),
      ylab = expression((U - L) / SD(Y)),
      main = "Width near target tilts", xlim = c(width_x_min, width_x_max)
    )
    graphics::abline(v = 0, lty = 1, lwd = 0.75, col = COL["mean"])
    for (target in TARGET_ORDER) {
      row <- summary[summary$target == target, ]
      graphics::points(
        row$standardized_delta, row$standardized_width,
        pch = PCH[target], col = COL[target], cex = 1.12
      )
      graphics::text(
        row$standardized_delta, row$standardized_width,
        labels = TARGET_LABEL[target],
        pos = FIGURE_02_WIDTH_LABEL_POSITION[target],
        offset = FIGURE_02_LABEL_OFFSET, cex = 0.76,
        col = COL[target]
      )
    }
    for (approximation in CF_TARGET_ORDER) {
      row <- cf_summary[
        cf_summary$approximation == approximation,
        , drop = FALSE
      ]
      if (!nrow(row)) next
      graphics::points(
        row$standardized_delta, row$standardized_width,
        pch = figure_02_cf_pch(approximation),
        col = figure_02_cf_color(approximation),
        cex = 1.20, lwd = 1.7
      )
      graphics::text(
        row$standardized_delta, row$standardized_width,
        labels = CF_TARGET_LABEL[approximation],
        pos = 2L, offset = 0.62, cex = 0.62,
        col = figure_02_cf_color(approximation)
      )
    }
    graphics::mtext(
      sprintf("Left-skewed illustration; interval content c = %.2f", content),
      side = 3, outer = TRUE, line = -0.20, cex = 0.82
    )
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "fig03_mean_tilt_cf_anchors"),
    7.2, 3.20, draw
  )
  list(
    id = "fig03_mean_tilt_cf_anchors",
    data = panel_files, outputs = outputs,
    distributions = distribution_descriptor(dist)
  )
}
figure_s01_cross_distribution <- function(out_dir, dists, content) {
  chosen <- dists[c(
    "normal", "asymmetric_laplace", "lognormal", "beta25"
  )]
  summaries <- lapply(chosen, oracle_interval_summary, content = content)
  paths <- lapply(chosen, mean_tilt_path_data, content = content)
  geometries <- Map(interval_plot_geometry, chosen, summaries)
  densities <- Map(
    function(dist, summary, geometry) {
      standardized_density_data(
        dist, summary = summary, geometry = geometry
      )
    },
    chosen, summaries, geometries
  )
  panel_files <- character()
  for (i in seq_along(chosen)) {
    check_oracle_summary(chosen[[i]], summaries[[i]], content)
    id <- chosen[[i]]$id
    panel_files <- c(
      panel_files,
      write_panel_data(
        summaries[[i]], file.path(out_dir, sprintf("figS01_%s_intervals.csv", id))
      ),
      write_panel_data(
        paths[[i]], file.path(out_dir, sprintf("figS01_%s_width_path.csv", id))
      ),
      write_panel_data(
        densities[[i]], file.path(out_dir, sprintf("figS01_%s_density.csv", id))
      )
    )
  }
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old))
    graphics::par(
      mfrow = c(2, 4), mar = c(3.4, 3.3, 2.5, 0.5),
      oma = c(0.5, 0, 0.3, 0), mgp = c(1.95, 0.55, 0), tcl = -0.25,
      cex = 0.82
    )
    for (i in seq_along(chosen)) {
      dist <- chosen[[i]]
      density <- densities[[i]]
      summary <- summaries[[i]]
      geometry <- geometries[[i]]
      ymax <- max(density$density)
      graphics::plot(
        density$z, density$density, type = "l", lwd = 1.8,
        col = COL["density"], xlab = "z", ylab = "Density",
        main = dist$short_label, xlim = geometry$zlim,
        ylim = c(-0.29 * ymax, 1.05 * ymax), cex.main = 0.92
      )
      graphics::abline(v = 0, lty = 1, lwd = 0.75, col = COL["mean"])
      plot_interval_bars(
        dist, summary, geometry, -0.050 * ymax, 0.068 * ymax,
        labels = TRUE
      )
    }
    for (i in seq_along(chosen)) {
      dist <- chosen[[i]]
      path <- paths[[i]]
      summary <- summaries[[i]]
      delta_min <- min(summary$standardized_delta) - 0.06
      delta_max <- max(0.20, max(summary$standardized_delta) + 0.05)
      visible <- is.finite(path$standardized_width) &
        path$standardized_delta >= delta_min &
        path$standardized_delta <= delta_max
      plot_path <- path[visible, , drop = FALSE]
      graphics::plot(
        plot_path$standardized_delta, plot_path$standardized_width,
        type = "l", lwd = 1.8, col = COL["density"],
        xlab = "Standardized tilt, d", ylab = "Standardized width",
        xlim = c(delta_min, delta_max)
      )
      graphics::abline(v = 0, lty = 1, lwd = 0.75, col = COL["mean"])
      if (identical(dist$id, "normal")) {
        point <- summary[summary$target == "ordinary_rqr", ]
        coincidence_cex <- c(
          equal_tailed = 1.30, ordinary_rqr = 0.95, shortest = 0.68
        )
        for (target in TARGET_ORDER) {
          graphics::points(
            point$standardized_delta, point$standardized_width,
            pch = OPEN_PCH[target], col = COL[target],
            cex = coincidence_cex[target], lwd = 1.5
          )
        }
        graphics::text(
          point$standardized_delta, point$standardized_width,
          labels = "ET = RQR = SH", pos = 4, offset = 0.45, cex = 0.70
        )
      } else {
        for (target in TARGET_ORDER) {
          row <- summary[summary$target == target, ]
          label_pos <- if (target == "shortest") {
            if (row$standardized_delta >= 0) 2 else 4
          } else if (target == "equal_tailed") {
            3
          } else {
            4
          }
          graphics::points(
            row$standardized_delta, row$standardized_width,
            pch = PCH[target], col = COL[target], cex = 0.95
          )
          graphics::text(
            row$standardized_delta, row$standardized_width,
            labels = TARGET_LABEL[target], pos = label_pos,
            offset = 0.35, cex = 0.66, col = COL[target]
          )
        }
      }
    }
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "figS01_cross_distribution_recovery"),
    7.2, 5.45, draw
  )
  list(
    id = "figS01_cross_distribution_recovery",
    data = panel_files, outputs = outputs,
    distributions = paste(
      vapply(chosen, distribution_descriptor, character(1)), collapse = ";"
    )
  )
}

figure_s02_loss_geometry <- function(out_dir, content) {
  midpoint <- 0
  half_width <- 1
  delta <- 0.25
  y <- seq(-2.5, 2.5, length.out = 1001L)
  residual_product <- (y - midpoint)^2 - half_width^2
  inside <- abs(y - midpoint) < half_width
  half_width_score <- -2 * half_width * (content - as.numeric(inside))
  midpoint_score <- 2 * (midpoint - y) *
    (content - as.numeric(inside))
  tilted_midpoint_score <- midpoint_score - 2 * content * delta
  data <- data.frame(
    y = y,
    residual_product = residual_product,
    inside = inside,
    half_width_score = half_width_score,
    midpoint_score = midpoint_score,
    tilted_midpoint_score = tilted_midpoint_score
  )
  panel_files <- write_panel_data(
    data, file.path(out_dir, "figS02_loss_geometry.csv")
  )
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old))
    graphics::par(
      mfrow = c(1, 3), mar = c(4.0, 3.8, 3.2, 0.7),
      oma = c(0, 0, 0.2, 0), mgp = c(2.25, 0.65, 0), tcl = -0.3
    )
    graphics::plot(
      y, residual_product, type = "l", lwd = 2,
      col = COL["density"], xlab = "Response, y",
      ylab = expression((y - m)^2 - h^2),
      main = "(a) Residual product"
    )
    graphics::abline(h = 0, lty = 3, col = COL["mean"])
    graphics::abline(v = c(-half_width, half_width), lty = 2,
                     col = COL["mean"])
    graphics::rect(
      -half_width, par("usr")[3], half_width, par("usr")[4],
      border = NA, col = grDevices::adjustcolor(COL["ordinary_rqr"], 0.09)
    )
    graphics::lines(y, residual_product, lwd = 2, col = COL["density"])
    graphics::text(0, -0.65, "inside: e < 0", cex = 0.78)
    graphics::plot(
      y, half_width_score, type = "s", lwd = 2,
      col = COL["ordinary_rqr"], xlab = "Response, y",
      ylab = expression(partialdiff[ h ] * ell[c]),
      main = "(b) Half-width score", ylim = c(-1.8, 0.7)
    )
    graphics::abline(v = c(-half_width, half_width), lty = 2,
                     col = COL["mean"])
    graphics::abline(h = 0, lty = 3, col = COL["mean"])
    graphics::points(
      c(-half_width, half_width), rep(-2 * half_width * content, 2),
      pch = 16, col = COL["ordinary_rqr"], cex = 0.75
    )
    graphics::points(
      c(-half_width, half_width),
      rep(2 * half_width * (1 - content), 2),
      pch = 1, col = COL["ordinary_rqr"], cex = 0.82
    )
    graphics::text(0, 0.50, "covered: contracts h", cex = 0.76)
    graphics::text(0, -1.48, "misses: expand h", cex = 0.76)
    graphics::plot(
      y, midpoint_score, type = "l", lwd = 2,
      col = COL["ordinary_rqr"], xlab = "Response, y",
      ylab = expression(partialdiff[m] * ell[c * "," * delta]),
      main = "(c) Midpoint score"
    )
    graphics::lines(
      y, tilted_midpoint_score, lwd = 2, col = COL["tilt"], lty = 2
    )
    graphics::abline(v = c(-half_width, half_width), lty = 3,
                     col = COL["mean"])
    graphics::abline(h = 0, lty = 3, col = COL["mean"])
    graphics::legend(
      "topleft",
      legend = c(expression(delta == 0), expression(delta == 0.25)),
      col = c(COL["ordinary_rqr"], COL["tilt"]),
      lty = c(1, 2), lwd = 2, bty = "n", cex = 0.80
    )
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "figS02_loss_geometry"), 7.2, 2.90, draw
  )
  list(
    id = "figS02_loss_geometry",
    data = panel_files, outputs = outputs, distributions = "deterministic_scores"
  )
}

write_figure_manifest <- function(records, out_dir, content, state) {
  generator <- script_path()
  rows <- lapply(records, function(record) {
    data_hashes <- vapply(record$data, sha256_file, character(1))
    output_hashes <- vapply(record$outputs, sha256_file, character(1))
    data.frame(
      figure_id = record$id,
      repository_commit = state$commit,
      repository_clean = state$clean,
      declared_source_commit = state$declared_commit,
      source_archive_sha256 = state$source_archive_sha256,
      source_identity_consistent = state$source_identity_consistent,
      git_rev_parse_status = state$rev_parse_status,
      git_worktree_status = state$worktree_status,
      generator = file.path("figures", basename(generator)),
      generator_sha256 = sha256_file(generator),
      script_version = SCRIPT_VERSION,
      configuration = sprintf("content=%.12g", content),
      distributions = record$distributions,
      dependencies = sprintf(
        "R=%s;digest=%s",
        getRversion(), as.character(utils::packageVersion("digest"))
      ),
      numerical_tolerances = paste(
        names(NUMERICAL_TOLERANCES), unlist(NUMERICAL_TOLERANCES),
        sep = "=", collapse = ";"
      ),
      data_files = paste(basename(record$data), collapse = ";"),
      data_sha256 = paste(data_hashes, collapse = ";"),
      output_files = paste(basename(record$outputs), collapse = ";"),
      output_sha256 = paste(output_hashes, collapse = ";"),
      byte_reproducibility = paste(
        "CSV and PNG bytes are tested across repeated runs;",
        "base-R PDF metadata contains generation timestamps"
      ),
      evidence_class = paste(
        "deterministic population illustration;",
        "not fitted, calibration, or response-predictive evidence"
      ),
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, rows)
  path <- file.path(out_dir, "rqr_theory_figure_manifest.csv")
  utils::write.csv(manifest, path, row.names = FALSE)
  path
}

write_publication_receipt <- function(records, out_dir, state, content,
                                      al_tau = ILLUSTRATION_AL_TAU) {
  generator <- script_path()
  rows <- lapply(records, function(record) {
    png <- record$outputs[grepl("\\.png$", record$outputs)]
    tex <- record$outputs[grepl("\\.tex$", record$outputs)]
    publication <- if (length(png) == 1L) {
      png
    } else if (length(tex) == 1L) {
      tex
    } else {
      fail(
        "Figure %s must have exactly one publication PNG or TeX source.",
        record$id
      )
    }
    data.frame(
      figure_id = record$id,
      publication_file = basename(publication),
      bytes = unname(file.info(publication)$size),
      sha256 = sha256_file(publication),
      generator_sha256 = sha256_file(generator),
      repository_commit = state$commit,
      repository_clean = state$clean,
      declared_source_commit = state$declared_commit,
      source_archive_sha256 = state$source_archive_sha256,
      source_identity_consistent = state$source_identity_consistent,
      distributions = record$distributions,
      interval_content = content,
      al_quantile_index = if (
        grepl("asymmetric_laplace", record$distributions, fixed = TRUE)
      ) {
        al_tau
      } else {
        NA_real_
      },
      oracle_scale_contract = paste(
        "targets computed on each raw population law;",
        "display coordinates use raw mean/SD standardization"
      ),
      stringsAsFactors = FALSE
    )
  })
  path <- file.path(out_dir, "rqr_theory_figure_provenance.csv")
  utils::write.csv(do.call(rbind, rows), path, row.names = FALSE)
  path
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    fail("Install package 'digest' before generating theory figures.")
  }
  parsed <- parse_generator_arguments(args)
  state <- source_state(
    declared_commit = parsed$source_commit,
    source_archive_sha256 = parsed$source_archive_sha256
  )
  out_dir <- parsed$output_dir
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir, mustWork = TRUE)
  content <- DEFAULT_CONTENT
  dists <- rqr_theory_distributions(content)
  invisible(lapply(dists, function(dist) {
    check_oracle_summary(dist, oracle_interval_summary(dist, content), content)
  }))
  records <- list(
    figure_01_three_principles(
      out_dir, dists$asymmetric_laplace, content
    ),
    figure_02_mean_tilt_map(
      out_dir, dists$asymmetric_laplace, content
    ),
    figure_03_mean_tilt_cf_anchors(
      out_dir, dists$asymmetric_laplace, content
    ),
    figure_s01_cross_distribution(out_dir, dists, content),
    figure_s02_loss_geometry(out_dir, content)
  )
  manifest <- write_figure_manifest(records, out_dir, content, state)
  receipt <- write_publication_receipt(
    records, out_dir, state, content, ILLUSTRATION_AL_TAU
  )
  message("Generated deterministic population figures under: ", out_dir)
  message("Manifest: ", manifest)
  message("Publication receipt: ", receipt)
  invisible(list(
    output_dir = out_dir, records = records, manifest = manifest,
    publication_receipt = receipt, source_state = state
  ))
}

if (!identical(Sys.getenv("RQR_THEORY_FIGURES_LIBRARY_ONLY"), "1")) {
  main()
}
