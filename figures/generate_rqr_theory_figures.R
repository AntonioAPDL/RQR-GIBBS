#!/usr/bin/env Rscript

# Deterministic population/oracle figures for the RQR article.
# This script does not fit a model, run MCMC, or simulate responses.

SCRIPT_VERSION <- "2026-07-25-article-1"
DEFAULT_CONTENT <- 0.80
NUMERICAL_TOLERANCES <- list(
  probability_margin = 1e-8,
  integration_relative = 1e-10,
  root_absolute = 1e-10,
  identity_absolute = 2e-7,
  boundary_index = 5e-6
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

parse_output_dir <- function(args = commandArgs(trailingOnly = TRUE)) {
  hit <- grep("^--output-dir=", args, value = TRUE)
  if (length(hit) > 1L) fail("Use at most one --output-dir argument.")
  if (!length(hit)) return(file.path(tempdir(), "rqr_theory_figures"))
  path <- sub("^--output-dir=", "", hit)
  if (!nzchar(path)) fail("--output-dir must not be empty.")
  path
}

git_output <- function(args) {
  root <- repository_root()
  out <- suppressWarnings(system2(
    "git", c("-C", shQuote(root), args),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) return(character())
  out
}

source_state <- function() {
  commit <- git_output(c("rev-parse", "HEAD"))
  status <- git_output(c("status", "--porcelain", "--untracked-files=normal"))
  list(
    commit = if (length(commit)) commit[1L] else NA_character_,
    clean = length(status) == 0L
  )
}

sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    fail("Package 'digest' is required for SHA-256 manifests.")
  }
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

make_distribution <- function(id, label, short_label, subtitle, qfun, pfun,
                              dfun, moment_fun, mean, sd,
                              support = c(-Inf, Inf)) {
  funs <- list(qfun, pfun, dfun, moment_fun)
  if (!all(vapply(funs, is.function, logical(1)))) {
    fail("Distribution %s must provide q, p, d, and truncated-moment functions.",
         id)
  }
  assert_scalar(mean, paste0(id, "$mean"))
  assert_scalar(sd, paste0(id, "$sd"), lower = 0, lower_open = TRUE)
  list(
    id = id, label = label, short_label = short_label, subtitle = subtitle,
    q = qfun, p = pfun, d = dfun, moment_between = moment_fun,
    mean = mean, sd = sd, support = support
  )
}

rqr_theory_distributions <- function() {
  normal_moment <- function(lower, upper) {
    stats::dnorm(lower) - stats::dnorm(upper)
  }
  exponential_term <- function(x) {
    ifelse(is.infinite(x) & x > 0, 0, (x + 1) * exp(-x))
  }
  exponential_moment <- function(lower, upper) {
    exponential_term(lower) - exponential_term(upper)
  }
  gamma_moment <- function(lower, upper) {
    2 * (stats::pgamma(upper, shape = 3) -
           stats::pgamma(lower, shape = 3))
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
  list(
    normal = make_distribution(
      "normal", "Normal(0, 1)", "Normal", "symmetric unimodal benchmark",
      stats::qnorm, stats::pnorm, stats::dnorm, normal_moment, 0, 1
    ),
    exponential = make_distribution(
      "exponential", "Exponential(1)", "Exponential",
      "support-boundary shortest interval",
      function(p) stats::qexp(p, rate = 1),
      function(y) stats::pexp(y, rate = 1),
      function(y) stats::dexp(y, rate = 1),
      exponential_moment, 1, 1, c(0, Inf)
    ),
    gamma = make_distribution(
      "gamma", "Gamma(shape=2, scale=1)", "Gamma(2, 1)",
      "moderate right skew",
      function(p) stats::qgamma(p, shape = 2, scale = 1),
      function(y) stats::pgamma(y, shape = 2, scale = 1),
      function(y) stats::dgamma(y, shape = 2, scale = 1),
      gamma_moment, 2, sqrt(2), c(0, Inf)
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
      c(0, Inf)
    ),
    beta25 = make_distribution(
      "beta25", "Beta(2, 5)", "Beta(2, 5)", "bounded right skew",
      function(p) stats::qbeta(p, shape1 = 2, shape2 = 5),
      function(y) stats::pbeta(y, shape1 = 2, shape2 = 5),
      function(y) stats::dbeta(y, shape1 = 2, shape2 = 5),
      beta_moment, 2 / 7, sqrt(2 * 5 / (7^2 * 8)), c(0, 1)
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
    support = -rev(dist$support)
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
    support = shift + scale * dist$support
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

standardized_density_data <- function(dist, zlim = c(-3.25, 5.25), n = 851L) {
  z <- seq(zlim[1], zlim[2], length.out = n)
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
PCH <- c(shortest = 15, equal_tailed = 16, ordinary_rqr = 17)
TARGET_LABEL <- c(
  shortest = "SH",
  equal_tailed = "ET",
  ordinary_rqr = "RQR"
)

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
                                  preview_dpi = 180) {
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

plot_interval_bars <- function(dist, summary, y0, dy, labels = TRUE,
                               targets = c(
                                 "shortest", "equal_tailed", "ordinary_rqr"
                               ),
                               label_x = 1.9) {
  for (i in seq_along(targets)) {
    target <- targets[i]
    row <- summary[summary$target == target, , drop = FALSE]
    lower_z <- (row$lower - dist$mean) / dist$sd
    upper_z <- (row$upper - dist$mean) / dist$sd
    yy <- y0 - (i - 1) * dy
    graphics::segments(
      lower_z, yy, upper_z, yy,
      lwd = 4.4, col = COL[target], lend = "round"
    )
    graphics::points(
      c(lower_z, upper_z), c(yy, yy),
      pch = PCH[target], col = COL[target], cex = 0.62
    )
    if (labels) {
      label <- TARGET_LABEL[target]
      graphics::text(
        max(label_x, upper_z + 0.18), yy, label, adj = c(0, 0.5),
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
  density <- standardized_density_data(dist, zlim = c(-1.6, 4.8))
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
      oma = c(0, 0, 2.4, 0), mgp = c(2.2, 0.65, 0), tcl = -0.3
    )
    targets <- c("equal_tailed", "ordinary_rqr", "shortest")
    titles <- c("Equal-tailed", "Ordinary RQR", "Shortest contiguous")
    subtitles <- c(
      "Balances omitted probabilities",
      "Balances omitted first moments",
      "Minimizes geometric width"
    )
    annotations <- list(
      expression(P(Y < L) == 0.10 ~~ "and" ~~ P(Y > U) == 0.10),
      expression(E(Y~"|"~L < Y & Y < U) == E(Y)),
      expression(f(L) == f(U))
    )
    for (j in seq_along(targets)) {
      target <- targets[j]
      row <- summary[summary$target == target, , drop = FALSE]
      graphics::plot(
        density$z, density$density, type = "l", lwd = 2.1,
        col = COL["density"], xlab = "Standardized response, z",
        ylab = "Density", xlim = range(density$z),
        ylim = c(0, max(density$density) * 1.12),
        main = titles[j], cex.main = 0.98
      )
      graphics::mtext(subtitles[j], side = 3, line = 0.25, cex = 0.70)
      graphics::abline(v = 0, lty = 3, col = COL["mean"])
      lower_z <- (row$lower - dist$mean) / dist$sd
      upper_z <- (row$upper - dist$mean) / dist$sd
      shade_interval(density, lower_z, upper_z, COL[target])
      graphics::segments(
        lower_z, 0.008, upper_z, 0.008,
        lwd = 5.0, col = COL[target], lend = "round"
      )
      graphics::points(
        c(lower_z, upper_z), c(0.008, 0.008),
        pch = PCH[target], col = COL[target], cex = 0.72
      )
      graphics::legend(
        "topright", legend = annotations[[j]],
        bty = "n", cex = 0.66
      )
    }
    graphics::mtext(
      sprintf(
        "Three interval principles at common content c = %.2f (population/oracle theory)",
        content
      ),
      outer = TRUE, line = 0.65, cex = 0.98, font = 2
    )
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "fig01_three_balance_principles"),
    7.2, 2.90, draw
  )
  list(
    id = "fig01_three_balance_principles",
    data = panel_files, outputs = outputs, distributions = dist$id
  )
}

figure_02_symmetry_skewness <- function(out_dir, dists, content) {
  chosen <- dists[c("normal", "lognormal")]
  summaries <- lapply(chosen, oracle_interval_summary, content = content)
  densities <- lapply(chosen, standardized_density_data)
  panel_files <- character()
  for (i in seq_along(chosen)) {
    check_oracle_summary(chosen[[i]], summaries[[i]], content)
    panel_files <- c(
      panel_files,
      write_panel_data(
        summaries[[i]],
        file.path(
          out_dir, sprintf("fig02_%s_intervals.csv", chosen[[i]]$id)
        )
      ),
      write_panel_data(
        densities[[i]],
        file.path(out_dir, sprintf("fig02_%s_density.csv", chosen[[i]]$id))
      )
    )
  }
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old))
    graphics::par(
      mfrow = c(1, 2), mar = c(4.1, 3.9, 3.0, 0.8),
      oma = c(0, 0, 2.2, 0), mgp = c(2.25, 0.65, 0), tcl = -0.3
    )
    for (i in seq_along(chosen)) {
      dist <- chosen[[i]]
      density <- densities[[i]]
      summary <- summaries[[i]]
      ymax <- max(density$density)
      title <- if (dist$id == "normal") {
        "Normal: symmetry"
      } else {
        "Lognormal: right skew"
      }
      graphics::plot(
        density$z, density$density, type = "l", lwd = 2.1,
        col = COL["density"], xlab = "Standardized response, z",
        ylab = "Density", main = title, xlim = c(-3.25, 5.25),
        ylim = c(-0.34 * ymax, 1.06 * ymax), cex.main = 0.98
      )
      graphics::mtext(dist$subtitle, side = 3, line = 0.20, cex = 0.70)
      graphics::abline(v = 0, lty = 3, col = COL["mean"])
      plot_interval_bars(
        dist, summary,
        y0 = -0.055 * ymax, dy = 0.075 * ymax,
        labels = TRUE, label_x = 1.9
      )
    }
    graphics::mtext(
      sprintf(
        "Symmetry and skewness at common content c = %.2f (population/oracle theory)",
        content
      ),
      outer = TRUE, line = 0.65, cex = 1.0, font = 2
    )
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "fig02_symmetry_vs_skewness"),
    7.2, 3.35, draw
  )
  list(
    id = "fig02_symmetry_vs_skewness",
    data = panel_files, outputs = outputs,
    distributions = paste(
      vapply(chosen, function(x) x$id, character(1)), collapse = ";"
    )
  )
}

figure_03_mean_tilt_map <- function(out_dir, dist, content) {
  summary <- oracle_interval_summary(dist, content)
  check_oracle_summary(dist, summary, content)
  path <- mean_tilt_path_data(dist, content)
  density <- standardized_density_data(dist)
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
      summary, file.path(out_dir, "fig03_panelC_selected_intervals.csv")
    ),
    write_panel_data(density, file.path(out_dir, "fig03_density.csv"))
  )
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old))
    graphics::par(
      mfrow = c(1, 3), mar = c(4.2, 3.8, 2.7, 0.7),
      oma = c(0, 0, 2.2, 0), mgp = c(2.3, 0.65, 0), tcl = -0.3
    )
    delta_min <- min(summary$standardized_delta) - 0.06
    delta_max <- max(0.20, max(summary$standardized_delta) + 0.05)
    visible <- is.finite(path$standardized_width) &
      path$standardized_delta >= delta_min &
      path$standardized_delta <= delta_max
    plot_path <- path[visible, , drop = FALSE]
    graphics::plot(
      path$u, path$M_minus_mu, type = "l", lwd = 2,
      col = COL["density"], xlab = "Lower-tail index, u",
      ylab = expression(M[c](u) - mu), main = "Tilt-to-window map"
    )
    graphics::abline(h = 0, lty = 3, col = COL["mean"])
    for (target in names(TARGET_LABEL)) {
      row <- summary[summary$target == target, ]
      graphics::points(
        row$u, row$delta, pch = PCH[target], col = COL[target], cex = 1.05
      )
      graphics::text(
        row$u, row$delta, labels = TARGET_LABEL[target],
        pos = 3, offset = 0.35, cex = 0.68
      )
    }
    graphics::plot(
      plot_path$standardized_delta, plot_path$standardized_width,
      type = "l", lwd = 2, col = COL["density"],
      xlab = expression(d == delta / SD(Y)),
      ylab = expression((U - L) / SD(Y)),
      main = "Width profile", xlim = c(delta_min, delta_max)
    )
    graphics::abline(v = 0, lty = 3, col = COL["mean"])
    for (target in names(TARGET_LABEL)) {
      row <- summary[summary$target == target, ]
      graphics::points(
        row$standardized_delta, row$standardized_width,
        pch = PCH[target], col = COL[target], cex = 1.05
      )
      graphics::text(
        row$standardized_delta, row$standardized_width,
        labels = TARGET_LABEL[target], pos = 3, offset = 0.35, cex = 0.68
      )
    }
    ymax <- max(density$density)
    graphics::plot(
      density$z, density$density, type = "l", lwd = 2,
      col = COL["density"], xlab = "Standardized response, z",
      ylab = "Density", main = "Selected intervals",
      xlim = c(-3.25, 5.25), ylim = c(-0.34 * ymax, 1.06 * ymax)
    )
    graphics::abline(v = 0, lty = 3, col = COL["mean"])
    plot_interval_bars(
      dist, summary, -0.055 * ymax, 0.075 * ymax,
      labels = TRUE, label_x = 1.9
    )
    graphics::mtext(
      sprintf(
        "Mean-tilt recovery map for %s, c = %.2f (population/oracle theory)",
        dist$short_label, content
      ),
      outer = TRUE, line = 0.65, cex = 0.98, font = 2
    )
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "fig03_mean_tilt_recovery_map"),
    7.2, 3.05, draw
  )
  list(
    id = "fig03_mean_tilt_recovery_map",
    data = panel_files, outputs = outputs, distributions = dist$id
  )
}

figure_s01_cross_distribution <- function(out_dir, dists, content) {
  chosen <- dists[c("normal", "gamma", "lognormal", "beta25")]
  summaries <- lapply(chosen, oracle_interval_summary, content = content)
  paths <- lapply(chosen, mean_tilt_path_data, content = content)
  densities <- lapply(chosen, standardized_density_data)
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
      mfrow = c(2, 4), mar = c(3.3, 3.2, 2.4, 0.4),
      oma = c(0.5, 0, 2.2, 0), mgp = c(1.95, 0.55, 0), tcl = -0.25,
      cex = 0.78
    )
    for (i in seq_along(chosen)) {
      dist <- chosen[[i]]
      density <- densities[[i]]
      summary <- summaries[[i]]
      ymax <- max(density$density)
      graphics::plot(
        density$z, density$density, type = "l", lwd = 1.8,
        col = COL["density"], xlab = "z", ylab = "Density",
        main = dist$short_label, xlim = c(-3.25, 5.25),
        ylim = c(-0.29 * ymax, 1.05 * ymax), cex.main = 0.92
      )
      graphics::abline(v = 0, lty = 3, col = COL["mean"])
      plot_interval_bars(
        dist, summary, -0.050 * ymax, 0.068 * ymax, labels = FALSE
      )
    }
    for (i in seq_along(chosen)) {
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
      graphics::abline(v = 0, lty = 3, col = COL["mean"])
      for (target in names(TARGET_LABEL)) {
        row <- summary[summary$target == target, ]
        graphics::points(
          row$standardized_delta, row$standardized_width,
          pch = PCH[target], col = COL[target], cex = 0.90
        )
      }
    }
    graphics::mtext(
      sprintf(
        "Fixed-content interval families across distributions, c = %.2f (population/oracle theory)",
        content
      ),
      outer = TRUE, line = 0.65, cex = 0.98, font = 2
    )
  }
  outputs <- with_graphics_devices(
    file.path(out_dir, "figS01_cross_distribution_recovery"),
    7.2, 5.25, draw
  )
  list(
    id = "figS01_cross_distribution_recovery",
    data = panel_files, outputs = outputs,
    distributions = paste(
      vapply(chosen, function(x) x$id, character(1)), collapse = ";"
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
      oma = c(0, 0, 2.2, 0), mgp = c(2.25, 0.65, 0), tcl = -0.3
    )
    graphics::plot(
      y, residual_product, type = "l", lwd = 2,
      col = COL["density"], xlab = "Response, y",
      ylab = expression((y - m)^2 - h^2),
      main = "Inside and outside"
    )
    graphics::abline(h = 0, lty = 3, col = COL["mean"])
    graphics::abline(v = c(-half_width, half_width), lty = 2,
                     col = COL["mean"])
    graphics::rect(
      -half_width, par("usr")[3], half_width, par("usr")[4],
      border = NA, col = grDevices::adjustcolor(COL["ordinary_rqr"], 0.09)
    )
    graphics::lines(y, residual_product, lwd = 2, col = COL["density"])
    graphics::text(0, -0.65, "inside: e < 0", cex = 0.72)
    graphics::plot(
      y, half_width_score, type = "s", lwd = 2,
      col = COL["ordinary_rqr"], xlab = "Response, y",
      ylab = expression(partialdiff[ h ] * ell[c]),
      main = "Half-width score", ylim = c(-1.8, 0.7)
    )
    graphics::abline(v = c(-half_width, half_width), lty = 2,
                     col = COL["mean"])
    graphics::abline(h = 0, lty = 3, col = COL["mean"])
    graphics::text(0, 0.50, "covered: contracts h", cex = 0.70)
    graphics::text(0, -1.48, "misses: expand h", cex = 0.70)
    graphics::plot(
      y, midpoint_score, type = "l", lwd = 2,
      col = COL["ordinary_rqr"], xlab = "Response, y",
      ylab = expression(partialdiff[m] * ell[c * "," * delta]),
      main = "Midpoint score"
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
      lty = c(1, 2), lwd = 2, bty = "n", cex = 0.72
    )
    graphics::mtext(
      sprintf(
        "RQR loss geometry at c = %.2f, m = 0, h = 1 (population/oracle theory)",
        content
      ),
      outer = TRUE, line = 0.65, cex = 0.98, font = 2
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

write_figure_manifest <- function(records, out_dir, content) {
  state <- source_state()
  generator <- script_path()
  rows <- lapply(records, function(record) {
    data_hashes <- vapply(record$data, sha256_file, character(1))
    output_hashes <- vapply(record$outputs, sha256_file, character(1))
    data.frame(
      figure_id = record$id,
      repository_commit = state$commit,
      repository_clean = state$clean,
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
        "deterministic population/oracle theory;",
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

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    fail("Install package 'digest' before generating theory figures.")
  }
  out_dir <- parse_output_dir(args)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir, mustWork = TRUE)
  content <- DEFAULT_CONTENT
  dists <- rqr_theory_distributions()
  invisible(lapply(dists, function(dist) {
    check_oracle_summary(dist, oracle_interval_summary(dist, content), content)
  }))
  records <- list(
    figure_01_three_principles(out_dir, dists$gamma, content),
    figure_02_symmetry_skewness(out_dir, dists, content),
    figure_03_mean_tilt_map(out_dir, dists$gamma, content),
    figure_s01_cross_distribution(out_dir, dists, content),
    figure_s02_loss_geometry(out_dir, content)
  )
  manifest <- write_figure_manifest(records, out_dir, content)
  message("Generated deterministic population/oracle figures under: ", out_dir)
  message("Manifest: ", manifest)
  invisible(list(output_dir = out_dir, records = records, manifest = manifest))
}

if (!identical(Sys.getenv("RQR_THEORY_FIGURES_LIBRARY_ONLY"), "1")) {
  main()
}
