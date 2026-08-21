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
  "--repo-root=."
))

assert_true(file.exists(result$csv), "CSV output exists")
assert_true(file.exists(result$tex), "TeX output exists")
tab <- utils::read.csv(result$csv, stringsAsFactors = FALSE)
assert_true(nrow(tab) == 7L, "seven distribution rows")
assert_true(
  identical(
    tab$dgp_id,
    c("normal", "gamma16", "gamma4", "lognormal", "exponential",
      "beta_right", "beta_left")
  ),
  "distribution order is stable"
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
expected <- data.frame(
  dgp_id = c(
    "normal", "gamma16", "gamma4", "lognormal", "exponential",
    "beta_right", "beta_left"
  ),
  skewness = c(
    0, 0.5, 1, 1.75018965506972, 2,
    0.596284793999944, -0.596284793999944
  ),
  d_shortest = c(
    -4.77415283434953e-09, -0.139715554010781,
    -0.272727316161112, -0.322632068641998,
    -0.402359478108525, -0.276779155185503, 0.27677916129714
  ),
  d_cf_shortest = c(
    0, -0.14056885127409, -0.28113770254818,
    -0.492044298649893, -0.562275405096361,
    -0.167638137049559, 0.167638137049559
  ),
  d_equal_tailed = c(
    0, -0.0466246371826502, -0.0917510930574674,
    -0.128181031011528, -0.169292556509201,
    -0.0690296872864341, 0.0690296872864351
  ),
  d_cf_equal_tailed = c(
    0, -0.0468562837580301, -0.0937125675160602,
    -0.164014766216631, -0.18742513503212,
    -0.0558793790165198, 0.0558793790165198
  ),
  stringsAsFactors = FALSE
)
for (column in setdiff(names(expected), "dgp_id")) {
  assert_close(
    tab[[column]], expected[[column]], 5e-9,
    paste("full population regression check for", column)
  )
}
assert_close(
  c(
    tab$d_shortest[tab$dgp_id == "beta_right"],
    tab$d_equal_tailed[tab$dgp_id == "beta_right"]
  ),
  -c(
    tab$d_shortest[tab$dgp_id == "beta_left"],
    tab$d_equal_tailed[tab$dgp_id == "beta_left"]
  ),
  1e-8, "reflected Beta population tilts reverse sign"
)
tex <- paste(readLines(result$tex, warn = FALSE), collapse = "\n")
assert_true(
  grepl("Population accuracy of first-order Cornish--Fisher recovery-tilt approximations",
        tex, fixed = TRUE),
  "caption states table purpose"
)
assert_true(
  grepl("not MCMC, tilt-selection, or response-prediction evidence",
        tex, fixed = TRUE),
  "caption preserves claim boundary"
)
assert_true(
  grepl("\\begin{tabular}{@{}l@{\\hspace{0.8em}}rrrrrrr@{}}",
        tex, fixed = TRUE),
  "table uses natural-width first column"
)
assert_true(
  grepl("\\multicolumn{3}{c}{Equal-tailed}", tex, fixed = TRUE) &&
    grepl("\\multicolumn{3}{c}{Shortest-contiguous}", tex, fixed = TRUE),
  "table groups exact and CF values by interval functional"
)
assert_true(
  grepl("Exponential$^{\\dagger}$", tex, fixed = TRUE) &&
    grepl("SH population value at the lower support boundary", tex, fixed = TRUE),
  "table identifies the boundary shortest-window case"
)
assert_true(
  !grepl("tabularx", tex, fixed = TRUE),
  "table does not use stretched tabularx layout"
)
assert_true(
  !any(grepl("^rmse_", names(tab))),
  "population table omits finite-sample RMSE columns"
)

message("Mean-tilt CF mini-study table checks passed.")
