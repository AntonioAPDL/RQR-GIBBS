#!/usr/bin/env Rscript

Sys.setenv(RQR_CF_TABLE_LIBRARY_ONLY = "1")
source("tables/generate_mean_tilt_cf_mini_study_table.R",
       local = .GlobalEnv)

assert_true <- function(value, label) {
  if (!isTRUE(value)) stop(sprintf("FAIL %s", label), call. = FALSE)
  invisible(TRUE)
}

assert_close <- function(actual, expected, tolerance, label) {
  if (length(actual) != length(expected) ||
      any(!is.finite(actual)) ||
      any(abs(actual - expected) > tolerance)) {
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

tmp <- tempfile("rqr_cf_table_test_")
dir.create(tmp)
result <- main(c(
  sprintf("--output-dir=%s", tmp),
  "--coverage=0.80",
  "--n=40",
  "--reps=3",
  "--seed=20260727",
  "--repo-root=."
))

assert_true(file.exists(result$csv), "CSV output exists")
assert_true(file.exists(result$tex), "TeX output exists")
tab <- utils::read.csv(result$csv, stringsAsFactors = FALSE)
assert_true(nrow(tab) == 7L, "seven DGP rows")
assert_true(
  identical(
    tab$dgp_id,
    c("normal", "gamma16", "gamma4", "lognormal", "exponential",
      "beta_right", "beta_left")
  ),
  "DGP order is stable"
)
normal <- tab[tab$dgp_id == "normal", ]
assert_close(
  c(normal$d_cf_shortest, normal$d_cf_equal_tailed),
  c(0, 0), 1e-14,
  "normal CF tilts are zero"
)
gamma16 <- tab[tab$dgp_id == "gamma16", ]
assert_true(
  gamma16$abs_error_shortest < 0.002 &&
    gamma16$abs_error_equal_tailed < 0.001,
  "near-normal Gamma CF approximation remains close"
)
exponential <- tab[tab$dgp_id == "exponential", ]
assert_true(
  identical(exponential$shortest_boundary, "lower"),
  "exponential shortest target is a boundary case"
)
tex <- paste(readLines(result$tex, warn = FALSE), collapse = "\n")
assert_true(
  grepl("Cornish--Fisher mean-tilt approximation check", tex, fixed = TRUE),
  "caption states table purpose"
)
assert_true(
  grepl("not MCMC or response-prediction evidence", tex, fixed = TRUE),
  "caption preserves claim boundary"
)

message("Mean-tilt CF mini-study table checks passed.")
