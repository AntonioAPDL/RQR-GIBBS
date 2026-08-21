#!/usr/bin/env Rscript

# Generate the manuscript table for the first-order Cornish--Fisher
# mean-tilt approximation check.  The table is deliberately population-only:
# it uses known laws, deterministic oracle recovery tilts, and true population
# skewness so the reader sees exactly what the first-order formula approximates.

DEFAULT_COVERAGE <- 0.80

fail <- function(...) stop(sprintf(...), call. = FALSE)

script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit)) {
    return(normalizePath(sub("^--file=", "", hit[1L]), mustWork = TRUE))
  }
  normalizePath("tables/generate_mean_tilt_cf_mini_study_table.R",
                mustWork = FALSE)
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

parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  known <- grepl("^--(output-dir|coverage|repo-root)=", args)
  if (length(args) && any(!known)) {
    fail("Unknown table-generator argument: %s", args[which(!known)[1L]])
  }
  output_dir <- parse_single_argument(args, "output-dir", "tables")
  coverage <- as.numeric(parse_single_argument(
    args, "coverage", as.character(DEFAULT_COVERAGE)
  ))
  repo_root <- parse_single_argument(args, "repo-root", repository_root())
  if (!is.finite(coverage) || coverage <= 0 || coverage >= 1) {
    fail("coverage must be one finite scalar in (0, 1).")
  }
  list(output_dir = output_dir, coverage = coverage, repo_root = repo_root)
}

load_validation_functions <- function(repo_root) {
  env <- new.env(parent = globalenv())
  sys.source(
    file.path(repo_root, "application", "scripts",
              "validate_mt_rqr_cf_dgps.R"),
    envir = env
  )
  env
}

dgp_display_label <- function(dgp_id) {
  labels <- c(
    normal = "Normal(mean=0, sd=1)",
    gamma16 = "Gamma(shape=16, scale=0.25)",
    gamma4 = "Gamma(shape=4, scale=1)",
    lognormal = "Lognormal(meanlog=0, sdlog=0.5)",
    exponential = "Exponential(rate=1)",
    beta_right = "Beta(shape1=2, shape2=5)",
    beta_left = "Beta(shape1=5, shape2=2)"
  )
  out <- unname(labels[dgp_id])
  ifelse(is.na(out), dgp_id, out)
}

dgp_table_label <- function(dgp_id) {
  labels <- c(
    normal = "Normal",
    gamma16 = "Gamma(16, 0.25)",
    gamma4 = "Gamma(4, 1)",
    lognormal = "Lognormal(0, 0.5)",
    exponential = "Exponential$^{\\dagger}$",
    beta_right = "Beta(2, 5)",
    beta_left = "Beta(5, 2)"
  )
  out <- unname(labels[dgp_id])
  ifelse(is.na(out), dgp_id, out)
}

format_decimal <- function(x, digits = 3L) {
  out <- formatC(
    round(as.numeric(x), digits),
    format = "f", digits = digits, flag = "#"
  )
  out[abs(as.numeric(x)) < 0.5 * 10^(-digits)] <- formatC(
    0, format = "f", digits = digits, flag = "#"
  )
  out
}

build_cf_mini_study <- function(coverage = DEFAULT_COVERAGE,
                                repo_root = repository_root()) {
  validation <- load_validation_functions(repo_root)
  init <- validation$load_mean_tilt_initializers(repo_root)
  dgps <- validation$dgp_registry()
  oracle <- validation$make_oracle_table(coverage, init, dgps)
  checks <- validation$make_oracle_check_table(oracle, coverage, dgps)
  if (!all(checks$coverage_pass) || !all(checks$rqr_mean_pass) ||
      !all(checks$shortest_grid_pass) || !all(checks$finite_oracle_pass)) {
    fail("Deterministic oracle checks failed; refusing to write table.")
  }
  out <- data.frame(
    dgp_id = oracle$dgp_id,
    dgp_label = dgp_display_label(oracle$dgp_id),
    coverage = coverage,
    skewness = oracle$skewness,
    shortest_boundary = oracle$shortest_boundary,
    d_shortest = oracle$d_shortest,
    d_cf_shortest = oracle$d_cf_shortest,
    abs_error_shortest = abs(oracle$d_cf_shortest - oracle$d_shortest),
    d_equal_tailed = oracle$d_equal_tailed,
    d_cf_equal_tailed = oracle$d_cf_equal_tailed,
    abs_error_equal_tailed =
      abs(oracle$d_cf_equal_tailed - oracle$d_equal_tailed),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

write_cf_table_tex <- function(table, path) {
  rows <- apply(table, 1L, function(row) {
    paste(
      dgp_table_label(row[["dgp_id"]]),
      format_decimal(row[["skewness"]]),
      format_decimal(row[["d_equal_tailed"]]),
      format_decimal(row[["d_cf_equal_tailed"]]),
      format_decimal(row[["abs_error_equal_tailed"]]),
      format_decimal(row[["d_shortest"]]),
      format_decimal(row[["d_cf_shortest"]]),
      format_decimal(row[["abs_error_shortest"]]),
      sep = " & "
    )
  })
  rows <- paste0(rows, " \\\\")
  header <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Population accuracy of first-order Cornish--Fisher recovery-tilt approximations.} Each row uses a known population law, content $c=0.80$, and true skewness $\\gamma_1$. Population and CF columns report standardized recovery tilts $d=\\delta/\\operatorname{SD}(Y)$; $|e|$ is their absolute gap. Gamma arguments are shape and scale, lognormal arguments are meanlog and sdlog, and the exponential law has unit rate. $^{\\dagger}$ denotes an SH population value at the lower support boundary. This is a deterministic population calculation, not MCMC, tilt-selection, or response-prediction evidence.}",
    "\\label{tab:mean-tilt-cf-mini-study}",
    "\\TableStyle",
    "\\begin{tabular}{@{}l@{\\hspace{0.8em}}rrrrrrr@{}}",
    "\\toprule",
    "Distribution & $\\gamma_1$ & \\multicolumn{3}{c}{Equal-tailed} & \\multicolumn{3}{c}{Shortest-contiguous} \\\\",
    "\\cmidrule(lr){3-5} \\cmidrule(l){6-8}",
    "& & Population $d$ & CF $d$ & $|e|$ & Population $d$ & CF $d$ & $|e|$ \\\\",
    "\\midrule"
  )
  footer <- c(
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(c(header, rows, footer), path, useBytes = TRUE)
  invisible(path)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- parse_args(args)
  dir.create(parsed$output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(parsed$output_dir, mustWork = TRUE)
  table <- build_cf_mini_study(
    coverage = parsed$coverage, repo_root = parsed$repo_root
  )
  csv_path <- file.path(output_dir, "mean_tilt_cf_mini_study.csv")
  tex_path <- file.path(output_dir, "mean_tilt_cf_mini_study.tex")
  utils::write.csv(table, csv_path, row.names = FALSE)
  write_cf_table_tex(table, tex_path)
  message("Wrote CF population table CSV: ", csv_path)
  message("Wrote CF population table TeX: ", tex_path)
  invisible(list(table = table, csv = csv_path, tex = tex_path))
}

if (!identical(Sys.getenv("RQR_CF_TABLE_LIBRARY_ONLY"), "1")) {
  main()
}
