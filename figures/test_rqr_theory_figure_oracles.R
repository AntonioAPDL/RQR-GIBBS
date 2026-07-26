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

content <- 0.80
tolerance <- 3e-7
dists <- rqr_theory_distributions()

# 1. Content and ordinary-RQR mean preservation.
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

# 2. Analytic truncated moments agree with independent response-space
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

# 3. Mean-tilt identity at prespecified interior tilts.
for (id in c("normal", "gamma", "lognormal", "beta25")) {
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

# 4. Symmetric coincidence.
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

# 5. Shortest-window optimality and boundary classification.
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
for (id in c("gamma", "lognormal", "beta25")) {
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

# 6. Positive-affine equivariance for the target and complete fixed-tilt loss.
base <- dists$gamma
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

# 7. Reflection reverses the recovery-tilt sign.
mirrored <- mirror_distribution(dists$gamma)
base_summary <- oracle_interval_summary(dists$gamma, content)
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

# 8. Complete generation succeeds. CSV and PNG bytes reproduce exactly across
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
assert_true(nrow(manifest) == 4L, "four-figure manifest")
assert_true(
  all(grepl("deterministic population illustration", manifest$evidence_class,
            fixed = TRUE)),
  "population-illustration evidence labels"
)
receipt <- utils::read.csv(
  run1$publication_receipt, stringsAsFactors = FALSE
)
assert_true(nrow(receipt) == 4L, "four-figure publication receipt")
assert_true(
  all(file.exists(file.path(out1, receipt$publication_file))),
  "publication receipt paths"
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

cat("PASS: deterministic RQR theory figures and oracle checks completed.\n")
