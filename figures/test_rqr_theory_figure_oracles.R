#!/usr/bin/env Rscript

# Independent numerical and end-to-end checks for the deterministic
# population figure generator.

test_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (!length(hit)) {
    return(normalizePath(
      "figures/test_rqr_theory_figure_oracles.R", mustWork = FALSE
    ))
  }
  normalizePath(sub("^--file=", "", hit[1L]), mustWork = TRUE)
}

generator <- file.path(
  dirname(test_script_path()), "generate_rqr_theory_figures.R"
)
if (!file.exists(generator)) {
  stop("Generator script not found beside the oracle test.", call. = FALSE)
}
Sys.setenv(RQR_THEORY_FIGURES_LIBRARY_ONLY = "1")
source(generator, local = .GlobalEnv)

assert_close <- function(actual, expected, tolerance, label) {
  ok <- length(actual) == length(expected) &&
    all(is.finite(actual)) && all(is.finite(expected))
  if (ok) ok <- max(abs(actual - expected)) <= tolerance
  if (!ok) {
    stop(
      sprintf(
        "FAIL %s: actual=%s expected=%s tolerance=%g",
        label,
        paste(signif(actual, 12), collapse = ","),
        paste(signif(expected, 12), collapse = ","),
        tolerance
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop(sprintf("FAIL %s", label), call. = FALSE)
  invisible(TRUE)
}

assert_error <- function(expr, label) {
  failed <- FALSE
  tryCatch(
    force(expr),
    error = function(e) {
      failed <<- TRUE
    }
  )
  assert_true(failed, label)
}

# 0. Source-provenance parsing and Git failures are explicit.
nonrepo <- tempfile("rqr_theory_nonrepo_")
dir.create(nonrepo)
failed_state <- source_state(root = nonrepo)
assert_true(is.na(failed_state$clean), "Git failure gives unknown cleanliness")
assert_true(
  is.na(failed_state$commit), "Git failure gives unknown detected commit"
)
assert_error(
  parse_generator_arguments(c(
    "--source-commit=0123456789012345678901234567890123456789",
    "--source-commit=abcdefabcdefabcdefabcdefabcdefabcdefabcd"
  )),
  "duplicate declared source commit is rejected"
)
assert_error(
  parse_generator_arguments("--source-commit=abc"),
  "short declared source commit is rejected"
)
assert_error(
  parse_generator_arguments("--source-archive-sha256=abc"),
  "short source archive digest is rejected"
)
assert_error(
  parse_generator_arguments("--unexpected=value"),
  "unknown generator argument is rejected"
)
archive_digest <- paste(rep("a", 64L), collapse = "")
archive_state <- source_state(
  source_archive_sha256 = archive_digest,
  root = nonrepo
)
assert_true(
  identical(archive_state$source_archive_sha256, archive_digest),
  "declared source archive digest is retained"
)
assert_true(
  is.na(archive_state$clean),
  "declared archive identity does not manufacture Git cleanliness"
)
detected_state <- source_state()
assert_true(!is.na(detected_state$commit), "test checkout has a detected commit")
matched_state <- source_state(declared_commit = detected_state$commit)
assert_true(
  isTRUE(matched_state$source_identity_consistent),
  "matching declared source commit is accepted"
)
wrong_commit <- paste(rep(
  if (substr(detected_state$commit, 1L, 1L) == "0") "1" else "0",
  nchar(detected_state$commit)
), collapse = "")
assert_error(
  source_state(declared_commit = wrong_commit),
  "mismatched declared source commit is rejected"
)

response_window_mean <- function(dist, lower, upper, content) {
  value <- stats::integrate(
    function(y) y * dist$d(y),
    lower = lower, upper = upper,
    subdivisions = 3000L,
    rel.tol = 1e-10,
    stop.on.error = TRUE
  )$value / content
  if (!is.finite(value)) {
    stop("Nonfinite independent response-space window mean.", call. = FALSE)
  }
  value
}

content <- DEFAULT_CONTENT
al_tau <- ILLUSTRATION_AL_TAU
tolerance <- 3e-7
dists <- rqr_theory_distributions(content)
assert_close(
  c(content, al_tau), c(0.80, 0.65), 1e-15,
  "interval content and AL quantile index are separately declared"
)
assert_true(
  length(unique(unname(PCH))) == length(PCH),
  "target symbols are shape-distinct"
)
assert_true(
  identical(unname(PCH[TARGET_ORDER]), c(15, 16, 17)),
  "ET, RQR, and SH use filled square, circle, and triangle symbols"
)
assert_true(
  identical(
    unname(CF_TARGET_LABEL[CF_TARGET_ORDER]),
    c("CF-ET", "CF-SH")
  ),
  "Cornish-Fisher approximation labels are stable"
)
assert_true(
  identical(TARGET_ORDER, c("equal_tailed", "ordinary_rqr", "shortest")),
  "target stacking order is ET, RQR, SH"
)
assert_true(
  identical(INTERVAL_SEGMENT_LTY, 1L),
  "all target interval bars are solid"
)
assert_true(
  identical(
    unname(FIGURE_02_MAP_LABEL_POSITION[TARGET_ORDER]),
    c(4L, 1L, 2L)
  ),
  "Figure 2 map labels place RQR below its marker"
)
assert_true(
  identical(
    unname(FIGURE_02_WIDTH_LABEL_POSITION[TARGET_ORDER]),
    c(4L, 4L, 3L)
  ),
  "Figure 2 width labels place SH above its marker"
)
assert_true(
  identical(
    unname(FIGURE_MAIN_02_CROSS_WIDTH_LABEL_POSITION[TARGET_ORDER]),
    c(3L, 4L, 3L)
  ),
  paste(
    "cross-distribution Figure 2 width panels place every non-Normal",
    "SH label above its marker"
  )
)
assert_true(
  identical(figure_02_map_label("ordinary_rqr", 9e-12), "RQR 0.00"),
  "Figure 2 ordinary-RQR map label uses the requested literal"
)
assert_true(
  identical(
    unname(COL[c("ordinary_rqr", "shortest")]),
    c("#2563EB", "#D97706")
  ),
  "Figure 2 RQR and SH annotations retain blue and orange encodings"
)
assert_close(
  FIGURE_02_LABEL_OFFSET, 0.45, 1e-15,
  "Figure 2 annotation offset"
)

# 1. The asymmetric-Laplace population benchmark uses the declared
# quantile-regression convention. This is a population law, not the
# pseudo-residual loss-kernel augmentation used by the sampler.
al <- asymmetric_laplace_components(mu = 0, scale = 1, tau = al_tau)
al_dist <- dists$asymmetric_laplace
assert_close(al$p(0), al_tau, 1e-14, "AL location is the tau quantile")
assert_close(
  c(al$mean, al$variance, al$sd, al$skewness),
  c(
    -1.3186813186813189,
    10.530129211447894,
    3.2450160571941544,
    -1.1520066473223101
  ),
  5e-13,
  "AL analytic moments and skewness"
)
assert_true(
  al$mean < al$q(0.5) && al$q(0.5) < al$mu,
  "tau=0.65 AL is left-skewed under the declared convention"
)
public_al_label_text <- paste(
  c(
    deparse(al_dist$short_label, width.cutoff = 500L),
    deparse(al_dist$subtitle, width.cutoff = 500L)
  ),
  collapse = " "
)
assert_true(
  identical(al_dist$short_label, "Left-skewed") &&
    grepl("Left-skewed illustration", public_al_label_text,
          fixed = TRUE),
  "reader-facing labels describe the illustrative shape without naming its law"
)
assert_true(
  !grepl("Laplace|tau|AL\\[|AL_", public_al_label_text),
  "reader-facing labels omit the internal population-law parameterization"
)
assert_true(
  grepl("0.8", public_al_label_text, fixed = TRUE),
  "reader-facing figure subtitle retains interval content"
)
extract_figure_block <- function(path, publication_file) {
  lines <- readLines(
    file.path(repository_root(), path), warn = FALSE, encoding = "UTF-8"
  )
  image_line <- grep(publication_file, lines, fixed = TRUE)
  assert_true(
    length(image_line) == 1L,
    sprintf("%s has one include for %s", path, publication_file)
  )
  starts <- which(
    seq_along(lines) <= image_line &
      grepl("\\begin{figure}", lines, fixed = TRUE)
  )
  ends <- which(
    seq_along(lines) >= image_line &
      grepl("\\end{figure}", lines, fixed = TRUE)
  )
  assert_true(
    length(starts) >= 1L && length(ends) >= 1L,
    sprintf("%s has a complete figure block for %s", path, publication_file)
  )
  paste(lines[max(starts):min(ends)], collapse = "\n")
}
fig02_block <- extract_figure_block(
  "main.tex", "figS01_cross_distribution_recovery.png"
)
fig03_block <- extract_figure_block(
  "main.tex", "fig03_mean_tilt_cf_anchors.png"
)
public_figure_blocks <- c(
  extract_figure_block("main.tex", "fig01_three_balance_principles.png"),
  fig02_block,
  fig03_block
)
assert_true(
  !grepl("Cornish--Fisher|CF-ET|CF-SH", fig02_block),
  "Main Figure 2 remains a non-CF population recovery figure"
)
assert_true(
  grepl("Cornish--Fisher", fig03_block, fixed = TRUE) &&
    grepl("fig:mean-tilt-cf-anchors", fig03_block, fixed = TRUE),
  "Main Figure 3 is the labelled CF-anchor diagnostic"
)
main_text <- paste(
  readLines(file.path(repository_root(), "main.tex"), warn = FALSE),
  collapse = "\n"
)
supplement_text <- paste(
  readLines(
    file.path(repository_root(), "rqr-gibbs-supplement.tex"), warn = FALSE
  ),
  collapse = "\n"
)
assert_true(
  lengths(regmatches(
    main_text,
    gregexpr("fig03_mean_tilt_cf_anchors.png", main_text, fixed = TRUE)
  )) == 1L &&
    !grepl("fig03_mean_tilt_cf_anchors.png", supplement_text, fixed = TRUE),
  "CF figure is included exactly once in the main article"
)
assert_true(
  lengths(regmatches(
    main_text,
    gregexpr("tables/mean_tilt_cf_mini_study.tex", main_text, fixed = TRUE)
  )) == 1L &&
    !grepl("tables/mean_tilt_cf_mini_study.tex", supplement_text,
           fixed = TRUE),
  "CF population table is included exactly once in the main article"
)
arxiv_script <- paste(
  readLines(
    file.path(repository_root(), "application", "scripts",
              "31_prepare_arxiv_source.sh"),
    warn = FALSE
  ),
  collapse = "\n"
)
assert_true(
  grepl("figures/generated/fig03_mean_tilt_cf_anchors.png", arxiv_script,
        fixed = TRUE) &&
    grepl("tables/mean_tilt_cf_mini_study.tex", arxiv_script, fixed = TRUE),
  "arXiv source contract includes the main-text CF figure and table"
)
for (term in c(
    "\\operatorname{AL}", "asymmetric-Laplace", "\\tau_{\\mathrm{AL}}"
)) {
  assert_true(
    !any(grepl(term, public_figure_blocks, fixed = TRUE)),
    sprintf("publication figure blocks omit the internal fixture term %s", term)
  )
}

p_grid <- sort(unique(c(
  exp(seq(log(1e-12), log(0.01), length.out = 80L)),
  seq(0.01, al_tau - 0.01, length.out = 300L),
  al_tau + c(-1e-10, -1e-12, 0, 1e-12, 1e-10),
  seq(al_tau + 0.001, 0.999, length.out = 100L),
  1 - exp(seq(log(1e-12), log(5e-4), length.out = 80L))
)))
p_grid <- p_grid[p_grid > 0 & p_grid < 1]
assert_close(
  al$p(al$q(p_grid)), p_grid, 2e-13,
  "AL CDF-quantile inversion"
)
y_grid <- al$q(p_grid)
assert_close(
  al$q(al$p(y_grid)), y_grid, 2e-8,
  "AL quantile-CDF inversion"
)
monotonicity_grid <- seq(
  al$q(1e-10), al$q(1 - 1e-10), length.out = 2001L
)
assert_true(
  all(diff(al$p(monotonicity_grid)) >= 0),
  "AL CDF is monotone"
)
assert_true(
  identical(
    c(al$p(-Inf), al$p(Inf), al$q(0), al$q(1)),
    c(0, 1, -Inf, Inf)
  ),
  "AL exact support limits"
)

integrate_split <- function(fun, rel.tol = 1e-11) {
  stats::integrate(
    fun, lower = -Inf, upper = 0,
    subdivisions = 5000L, rel.tol = rel.tol, stop.on.error = TRUE
  )$value +
    stats::integrate(
      fun, lower = 0, upper = Inf,
      subdivisions = 5000L, rel.tol = rel.tol, stop.on.error = TRUE
    )$value
}
mass_numeric <- integrate_split(al$d)
mean_numeric <- integrate_split(function(y) y * al$d(y))
second_numeric <- integrate_split(function(y) y^2 * al$d(y))
assert_close(mass_numeric, 1, 2e-11, "AL numerical total mass")
assert_close(mean_numeric, al$mean, 2e-8, "AL numerical mean")
assert_close(
  second_numeric - mean_numeric^2, al$variance, 5e-6,
  "AL numerical variance"
)
for (limits in list(c(-30, -8), c(-8, 1), c(0.1, 8))) {
  numerical <- stats::integrate(
    function(y) y * al$d(y),
    lower = limits[1L], upper = limits[2L],
    subdivisions = 5000L, rel.tol = 1e-11, stop.on.error = TRUE
  )$value
  analytic <- al$moment_between(limits[1L], limits[2L])
  assert_close(
    analytic, numerical, 8e-9,
    sprintf(
      "AL truncated first moment on [%g,%g]", limits[1L], limits[2L]
    )
  )
  assert_close(
    al$truncated_below(limits[2L]) -
      al$truncated_below(limits[1L]),
    numerical, 8e-9,
    sprintf(
      "AL below-moment identity on [%g,%g]", limits[1L], limits[2L]
    )
  )
}

# 2. Content and ordinary-RQR mean preservation.
for (dist in dists) {
  summary <- oracle_interval_summary(dist, content)
  for (i in seq_len(nrow(summary))) {
    interval <- list(lower = summary$lower[i], upper = summary$upper[i])
    assert_close(
      window_content(dist, interval), content, tolerance,
      paste(dist$id, summary$target[i], "content")
    )
  }
  rqr <- summary[summary$target == "ordinary_rqr", ]
  independent <- response_window_mean(
    dist, rqr$lower, rqr$upper, content
  )
  assert_close(
    independent, dist$mean, tolerance,
    paste(dist$id, "ordinary-RQR retained mean")
  )
}

# Perturb the illustration law and the interval content separately to guard
# against accidental coupling in the generator.
probe_al <- asymmetric_laplace_components(mu = 0, scale = 1, tau = 0.79)
probe_dist <- make_distribution(
  "asymmetric_laplace_probe",
  "Asymmetric Laplace probe",
  "AL probe",
  "tau/content independence probe",
  probe_al$q, probe_al$p, probe_al$d, probe_al$moment_between,
  probe_al$mean, probe_al$sd,
  support = c(-Inf, Inf), plot_knots = probe_al$mu,
  raw_parameters = "mu_AL=0,s_AL=1,tau_AL=0.79"
)
assert_close(
  probe_al$p(0), 0.79, 1e-14,
  "changing AL tau changes the distribution quantile index"
)
assert_close(
  oracle_interval_summary(probe_dist, content)$content,
  rep(content, 3L), 2e-8,
  "changing AL tau does not change interval content"
)
probe_content <- 0.79
assert_close(
  oracle_interval_summary(al_dist, probe_content)$content,
  rep(probe_content, 3L), 2e-8,
  "changing interval content does not change AL tau"
)
assert_close(
  al_dist$p(al$mu), al_tau, 1e-14,
  "changing interval content leaves the AL quantile index fixed"
)

# 3. Analytic truncated moments agree with independent response-space
# integration, including windows close to each probability boundary.
for (dist in dists) {
  max_u <- 1 - content
  margin <- NUMERICAL_TOLERANCES$probability_margin
  for (u in c(margin, max_u / 2, max_u - margin)) {
    interval <- quantile_window(dist, u, content)
    analytic <- quantile_window_mean(dist, u, content)
    independent <- response_window_mean(
      dist, interval$lower, interval$upper, content
    )
    assert_close(
      analytic, independent, 5e-7,
      sprintf("%s truncated moment at u=%.8g", dist$id, u)
    )
  }
}

# 4. Mean-tilt identity at prespecified interior tilts.
for (id in c("normal", "asymmetric_laplace", "lognormal", "beta25")) {
  dist <- dists[[id]]
  delta <- 0.05 * dist$sd
  u <- solve_window_for_tilt(dist, delta, content)
  interval <- quantile_window(dist, u, content)
  independent <- response_window_mean(
    dist, interval$lower, interval$upper, content
  )
  assert_close(
    independent, dist$mean + delta, tolerance,
    paste(id, "mean-tilt retained mean")
  )
}

# 5. Symmetric coincidence.
normal_summary <- oracle_interval_summary(dists$normal, content)
assert_close(
  normal_summary$u, rep((1 - content) / 2, 3), 2e-6,
  "Normal symmetry coincidence in lower-tail index"
)
assert_close(
  normal_summary$standardized_width,
  rep(normal_summary$standardized_width[1], 3), 2e-6,
  "Normal symmetry coincidence in width"
)

# 6. Shortest-window optimality and boundary classification.
for (id in names(dists)) {
  dist <- dists[[id]]
  shortest <- shortest_contiguous_window(dist, content)
  grid <- seq(0, 1 - content, length.out = 5001L)
  width <- vapply(
    grid,
    function(u) quantile_window(dist, u, content)$width,
    numeric(1)
  )
  assert_true(
    shortest$interval$width <= min(width) + 5e-6 * max(1, dist$sd),
    paste(id, "shortest-window grid optimality")
  )
}
for (id in c("asymmetric_laplace", "lognormal", "beta25")) {
  dist <- dists[[id]]
  shortest <- shortest_contiguous_window(dist, content)
  assert_true(shortest$status == "interior", paste(id, "interior shortest"))
  h <- 1e-5
  derivative <- (
    quantile_window(dist, shortest$u + h, content)$width -
      quantile_window(dist, shortest$u - h, content)$width
  ) / (2 * h)
  assert_close(
    derivative, 0, 3e-4 * max(1, dist$sd),
    paste(id, "shortest-width derivative")
  )
}
assert_true(
  shortest_contiguous_window(dists$exponential, content)$status ==
    "lower_support_boundary",
  "Exponential shortest interval is a lower-support boundary solution"
)

# 7. The complete AL interval oracle agrees with independently frozen values.
al_summary <- oracle_interval_summary(al_dist, content)
al_summary <- al_summary[
  match(c("shortest", "equal_tailed", "ordinary_rqr"), al_summary$target),
]
assert_close(
  al_summary$u,
  c(
    0.129999998115306215,
    0.099999999999999978,
    0.072050567860342807
  ),
  8e-10,
  "AL lower-tail indices"
)
assert_close(
  al_summary$lower,
  c(
    -4.5983940769478426,
    -5.3480062197188341,
    -6.2845833114264344
  ),
  2e-7,
  "AL raw lower endpoints"
)
assert_close(
  al_summary$upper,
  c(
    2.4760582853998514,
    1.9273276438390277,
    1.5481508154309045
  ),
  2e-7,
  "AL raw upper endpoints"
)
assert_close(
  (al_summary$lower - al_dist$mean) / al_dist$sd,
  c(
    -1.0106923048949010,
    -1.2416964446461087,
    -1.5303166163803046
  ),
  2e-9,
  "AL standardized lower endpoints"
)
assert_close(
  (al_summary$upper - al_dist$mean) / al_dist$sd,
  c(
    1.1694054935932556,
    1.0003059785556350,
    0.8834569948449090
  ),
  2e-9,
  "AL standardized upper endpoints"
)
assert_close(
  al_summary$standardized_delta,
  c(
    0.163507329750590735,
    0.080970217872203806,
    6.5950684787640499e-12
  ),
  2e-9,
  "AL standardized recovery tilts"
)
assert_close(
  al_summary$standardized_width,
  c(
    2.1800977984881564,
    2.2420024232017433,
    2.4137736112252139
  ),
  2e-9,
  "AL standardized widths"
)
assert_true(
  al_summary$standardized_delta[al_summary$target == "equal_tailed"] > 0 &&
    al_summary$standardized_delta[al_summary$target == "shortest"] > 0,
  "left-skew AL has positive ET and SH recovery tilts"
)
assert_close(
  shortest_contiguous_window(al_dist, content)$u,
  al$tau * (1 - content),
  8e-8,
  "general AL width optimizer agrees with its crossing-mode identity"
)
cf_al <- cornish_fisher_tilt_summary(al_dist, al_summary, content)
cf_al <- cf_al[match(CF_TARGET_ORDER, cf_al$approximation), ]
assert_close(
  cornish_fisher_constant(content),
  0.281137702548180,
  2e-15,
  "content-0.80 Cornish-Fisher constant"
)
assert_close(
  cf_al$standardized_delta,
  c(0.10795750071614209, 0.32387250214842628),
  2e-12,
  "AL Cornish-Fisher standardized tilts"
)
assert_true(
  identical(cf_al$within_admissible, c(TRUE, TRUE)),
  "Figure 3 records admissible CF-ET and CF-SH anchors"
)
assert_true(
  cf_al$standardized_delta_error[
    cf_al$approximation == "cornish_fisher_equal_tailed"
  ] > 0 &&
    cf_al$standardized_delta_error[
      cf_al$approximation == "cornish_fisher_shortest"
    ] > 0.10,
  "left-skew illustration exposes CF approximation error without hiding it"
)

# 8. Positive-affine equivariance for the target and complete fixed-tilt loss.
base <- dists$asymmetric_laplace
shift <- 3.25
scale <- 2.4
affine <- affine_distribution(base, shift, scale)
for (delta in c(-0.10, 0, 0.08) * base$sd) {
  u_base <- solve_window_for_tilt(base, delta, content)
  u_affine <- solve_window_for_tilt(affine, scale * delta, content)
  interval_base <- quantile_window(base, u_base, content)
  interval_affine <- quantile_window(affine, u_affine, content)
  assert_close(u_affine, u_base, 2e-7, "affine lower-tail-index invariance")
  assert_close(
    c(interval_affine$lower, interval_affine$upper),
    shift + scale * c(interval_base$lower, interval_base$upper),
    3e-7, "affine endpoint equivariance"
  )
}
y <- 1.2
root1 <- 0.3
root2 <- 2.1
delta <- 0.12
loss <- mean_tilt_loss(y, root1, root2, content, delta)
loss_affine <- mean_tilt_loss(
  shift + scale * y,
  shift + scale * root1,
  shift + scale * root2,
  content,
  scale * delta
)
assert_close(loss_affine, scale^2 * loss, 1e-12, "affine loss scaling")
omega <- 1.7
assert_close(
  (omega / scale^2) * loss_affine,
  omega * loss,
  1e-12,
  "affine generalized-posterior exponent"
)

# 9. Reflection reverses the recovery-tilt sign.
mirrored <- mirror_distribution(dists$asymmetric_laplace)
base_summary <- oracle_interval_summary(dists$asymmetric_laplace, content)
mirror_summary <- oracle_interval_summary(mirrored, content)
for (target in c("equal_tailed", "shortest", "ordinary_rqr")) {
  base_row <- base_summary[base_summary$target == target, ]
  mirror_row <- mirror_summary[mirror_summary$target == target, ]
  assert_close(
    mirror_row$standardized_delta,
    -base_row$standardized_delta,
    3e-6,
    paste(target, "mirrored tilt sign")
  )
  assert_close(
    c(mirror_row$lower, mirror_row$upper),
    c(-base_row$upper, -base_row$lower),
    3e-6,
    paste(target, "mirrored endpoints")
  )
}

# 10. Adaptive domains contain all endpoints, density knots, and direct-label
# extents. The AL density grid contains its standardized sharp mode exactly.
for (dist in dists[c(
  "normal", "asymmetric_laplace", "lognormal", "beta25"
)]) {
  summary <- oracle_interval_summary(dist, content)
  geometry <- interval_plot_geometry(dist, summary)
  required <- c(
    geometry$standardized_endpoints,
    geometry$standardized_quantile_limits,
    geometry$standardized_knots,
    geometry$label_anchors,
    geometry$label_extents,
    0
  )
  assert_true(
    all(required > geometry$zlim[1L] & required < geometry$zlim[2L]),
    paste(dist$id, "adaptive plot domain contains required geometry")
  )
  density <- standardized_density_data(
    dist, summary = summary, geometry = geometry
  )
  assert_close(
    range(density$z), geometry$zlim, 1e-14,
    paste(dist$id, "density spans adaptive domain")
  )
}
al_geometry <- interval_plot_geometry(al_dist, al_summary)
al_density <- standardized_density_data(
  al_dist, summary = al_summary, geometry = al_geometry
)
al_location_z <- (al$mu - al$mean) / al$sd
assert_true(
  any(al_density$z == al_location_z),
  "AL standardized density grid contains the exact location/mode knot"
)
assert_true(
  al_geometry$zlim[1L] < min(
    (al_summary$lower - al_dist$mean) / al_dist$sd
  ),
  "AL adaptive domain does not clip the RQR lower endpoint"
)
generator_text <- paste(readLines(generator, warn = FALSE), collapse = "\n")
assert_true(
  !grepl("gamma", generator_text, ignore.case = TRUE),
  "the deterministic figure generator has no stale Gamma branch"
)

# 11. Complete generation succeeds. CSV and PNG bytes reproduce exactly across
# two runs; PDF bytes are not compared because base R writes generation times.
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required for the end-to-end generator check.",
       call. = FALSE)
}
out1 <- tempfile("rqr_theory_figures_test1_")
out2 <- tempfile("rqr_theory_figures_test2_")
dir.create(out1)
dir.create(out2)
run1 <- main(sprintf("--output-dir=%s", out1))
run2 <- main(sprintf("--output-dir=%s", out2))

expected_outputs <- c(
  "fig01_three_balance_principles.pdf",
  "fig01_three_balance_principles.png",
  "fig02_mean_tilt_recovery_map.pdf",
  "fig02_mean_tilt_recovery_map.png",
  "fig03_mean_tilt_cf_anchors.pdf",
  "fig03_mean_tilt_cf_anchors.png",
  "figS01_cross_distribution_recovery.pdf",
  "figS01_cross_distribution_recovery.png",
  "figS02_loss_geometry.pdf",
  "figS02_loss_geometry.png",
  "rqr_theory_figure_manifest.csv",
  "rqr_theory_figure_provenance.csv"
)
assert_true(
  all(file.exists(file.path(out1, expected_outputs))),
  "complete expected figure bundle"
)
assert_true(file.exists(run1$manifest), "first manifest exists")
assert_true(file.exists(run2$manifest), "second manifest exists")

stable_files <- list.files(
  out1, pattern = "\\.(csv|png)$", full.names = FALSE
)
stable_files <- setdiff(stable_files, "rqr_theory_figure_manifest.csv")
for (name in stable_files) {
  hash1 <- sha256_file(file.path(out1, name))
  hash2 <- sha256_file(file.path(out2, name))
  assert_true(identical(hash1, hash2), paste(name, "byte reproduction"))
}
manifest <- utils::read.csv(run1$manifest, stringsAsFactors = FALSE)
assert_true(nrow(manifest) == 5L, "five-figure manifest")
assert_true(
  all(grepl("deterministic population illustration", manifest$evidence_class,
            fixed = TRUE)),
  "population-illustration evidence labels"
)
receipt <- utils::read.csv(
  run1$publication_receipt, stringsAsFactors = FALSE
)
assert_true(nrow(receipt) == 5L, "five-figure publication receipt")
assert_true(
  all(file.exists(file.path(out1, receipt$publication_file))),
  "publication receipt paths"
)
assert_true(
  all(c(
    "distributions", "interval_content", "al_quantile_index",
    "oracle_scale_contract"
  ) %in% names(receipt)),
  paste(
    "publication receipt separately records the distribution index,",
    "interval content, and raw-scale oracle contract"
  )
)
al_receipt <- receipt[
  receipt$figure_id %in% c(
    "fig01_three_balance_principles",
    "fig02_mean_tilt_recovery_map",
    "fig03_mean_tilt_cf_anchors",
    "figS01_cross_distribution_recovery"
  ),
]
assert_true(
  all(grepl("tau_AL=0.65", al_receipt$distributions, fixed = TRUE)),
  "AL publication receipts retain the raw parameter metadata"
)
assert_close(
  al_receipt$interval_content, rep(content, nrow(al_receipt)), 1e-15,
  "AL publication receipts record interval content separately"
)
assert_close(
  al_receipt$al_quantile_index, rep(al_tau, nrow(al_receipt)), 1e-15,
  "AL publication receipts record the AL quantile index separately"
)
assert_true(
  !any(grepl("tau_AL=0.8|tau_AL=0.99", al_receipt$distributions)),
  "AL publication receipts contain no stale tau metadata"
)
assert_true(
  all(grepl(
    "raw population law", al_receipt$oracle_scale_contract, fixed = TRUE
  )),
  "AL publication receipts retain the raw-target standardization contract"
)
assert_true(
  !any(grepl("gamma", receipt$distributions, ignore.case = TRUE)),
  "publication receipt has no stale Gamma benchmark"
)
assert_true(
  all(vapply(seq_len(nrow(receipt)), function(i) {
    identical(
      sha256_file(file.path(out1, receipt$publication_file[i])),
      receipt$sha256[i]
    )
  }, logical(1))),
  "publication receipt hashes"
)

loss_geometry <- utils::read.csv(
  file.path(out1, "figS02_loss_geometry.csv"),
  stringsAsFactors = FALSE
)
boundary <- loss_geometry[loss_geometry$y %in% c(-1, 1), , drop = FALSE]
assert_true(nrow(boundary) == 2L, "strict-boundary rows are present")
assert_true(
  all(!as.logical(boundary$inside)),
  "strict interval convention excludes both endpoint observations"
)
assert_close(
  boundary$half_width_score,
  rep(-2 * content, 2),
  1e-12,
  "strict-boundary half-width score uses indicator zero"
)

cat("PASS: deterministic RQR theory figures and oracle checks completed.\n")
