#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}
stopf <- function(...) stop(paste0(...), call. = FALSE)

arguments <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", arguments[startsWith(arguments, file_arg)][1L])
if (!length(script_path) || is.na(script_path)) {
  script_path <- "tables/generate_tolerance_validation_gibbs_diagnostic_table.R"
}
script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."),
                           winslash = "/", mustWork = TRUE)
setwd(repo_root)

default_diagnostics <- file.path(
  "application", "outputs", "tolerance_mti_gibbs_diagnostics",
  "gibbs_diagnostics_long_20260818T013452Z",
  "gibbs_estimator_diagnostics.csv"
)
diagnostics_arg <- arg_value("--diagnostics-csv=", default_diagnostics)
output_dir <- normalizePath(arg_value("--output-dir=", "tables"),
                            winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

csv_path <- file.path(output_dir, "tolerance_validation_gibbs_diagnostics.csv")
tex_path <- file.path(output_dir, "tolerance_validation_gibbs_diagnostics.tex")
committed_csv <- file.path(repo_root, "tables",
                           "tolerance_validation_gibbs_diagnostics.csv")
committed_tex <- file.path(repo_root, "tables",
                           "tolerance_validation_gibbs_diagnostics.tex")

if (!file.exists(diagnostics_arg)) {
  if (file.exists(committed_csv) && file.exists(committed_tex)) {
    if (!identical(normalizePath(output_dir, winslash = "/", mustWork = FALSE),
                   normalizePath(file.path(repo_root, "tables"),
                                 winslash = "/", mustWork = TRUE))) {
      file.copy(committed_csv, csv_path, overwrite = TRUE)
      file.copy(committed_tex, tex_path, overwrite = TRUE)
    }
    cat("Using committed MTI Gibbs diagnostic table; provide diagnostics CSV to regenerate.\n")
    quit(status = 0)
  }
  stopf("Missing Gibbs diagnostic CSV: ", diagnostics_arg)
}

diagnostics_path <- normalizePath(diagnostics_arg, winslash = "/",
                                  mustWork = TRUE)
diagnostics <- utils::read.csv(diagnostics_path, stringsAsFactors = FALSE,
                               check.names = FALSE)
required <- c(
  "cell_role", "dgp_id", "n", "guaranteed_content", "estimand", "rhat",
  "ess", "chains", "draws_per_chain"
)
missing <- setdiff(required, names(diagnostics))
if (length(missing)) {
  stopf("Gibbs diagnostics are missing column(s): ",
        paste(missing, collapse = ", "))
}

num <- function(x) suppressWarnings(as.numeric(x))
max_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) max(x) else NA_real_
}
min_or_na <- function(x) {
  x <- num(x)
  x <- x[is.finite(x)]
  if (length(x)) min(x) else NA_real_
}
format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  out <- ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), "--")
  sub("\\.?0+$", "", out)
}
format_int <- function(x) {
  x <- as.numeric(x)
  ifelse(is.finite(x), format(round(x), big.mark = ",", scientific = FALSE),
         "--")
}
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([_%&#])", "\\\\\\1", x, perl = TRUE)
  x
}

role_labels <- c(
  hard_feasible_large = "Large hard feasible cell",
  small_feasible = "Small feasible cell"
)
dgp_labels <- c(
  normal = "Gaussian",
  lognormal_hard = "hard log-normal",
  sharp_mixture = "sharp mixture",
  contaminated_normal = "contaminated normal",
  student_t3 = "Student t3"
)
status_label <- function(max_rhat) {
  if (!is.finite(max_rhat)) return("Unavailable")
  if (max_rhat <= 1.05) return("Passed")
  if (max_rhat <= 1.10) return("Borderline")
  "Diagnostic only"
}

keys <- interaction(
  diagnostics$cell_role, diagnostics$dgp_id, diagnostics$n,
  sprintf("%.2f", num(diagnostics$guaranteed_content)),
  drop = TRUE, sep = "||"
)
out <- do.call(rbind, lapply(split(diagnostics, keys), function(df) {
  data.frame(
    cell_role = df$cell_role[[1L]],
    role = role_labels[df$cell_role[[1L]]],
    dgp_id = df$dgp_id[[1L]],
    dgp = dgp_labels[df$dgp_id[[1L]]],
    n = as.integer(df$n[[1L]]),
    guaranteed_content = num(df$guaranteed_content[[1L]]),
    chains = as.integer(max_or_na(df$chains)),
    draws_per_chain = as.integer(max_or_na(df$draws_per_chain)),
    max_rhat = max_or_na(df$rhat),
    min_ess = min_or_na(df$ess),
    stringsAsFactors = FALSE
  )
}))
out$role[is.na(out$role)] <- out$cell_role[is.na(out$role)]
out$dgp[is.na(out$dgp)] <- out$dgp_id[is.na(out$dgp)]
out$status <- vapply(out$max_rhat, status_label, character(1L))
out <- out[order(
  match(out$cell_role, c("hard_feasible_large", "small_feasible")),
  out$n, out$guaranteed_content, out$dgp
), ]

utils::write.csv(out, csv_path, row.names = FALSE)

body <- vapply(seq_len(nrow(out)), function(ii) {
  sprintf(
    "%s & %s & %s & %.2f & %s & %s & %s \\\\",
    escape_latex(out$role[[ii]]),
    escape_latex(out$dgp[[ii]]),
    format_int(out$n[[ii]]),
    out$guaranteed_content[[ii]],
    format_num(out$max_rhat[[ii]], 3),
    format_int(out$min_ess[[ii]]),
    escape_latex(out$status[[ii]])
  )
}, character(1L))
writeLines(c(
  "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}Xlrrrrl@{}}",
  "\\toprule",
  "Diagnostic cell & DGP & \\(n\\) & \\(c\\) & Max \\(\\hat R\\) & Min ESS & Status\\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabularx}"
), tex_path)

cat("Wrote MTI Gibbs diagnostic table:\n")
cat("  csv: ", csv_path, "\n", sep = "")
cat("  tex: ", tex_path, "\n", sep = "")
