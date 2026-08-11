#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root_arg <- args[startsWith(args, "--repo-root=")]
repo_root <- if (length(root_arg)) {
  sub("^--repo-root=", "", root_arg[[1L]])
} else {
  getwd()
}
repo_root <- normalizePath(repo_root, mustWork = TRUE)

manuscripts <- c("main.tex", "rqr-gibbs-supplement.tex")
included_text <- c("figures/rqr_dlm_blocked_state_schematic.tex")
source_files <- c(manuscripts, included_text)
paths <- file.path(repo_root, source_files)
if (any(!file.exists(paths))) {
  stop(
    "Missing manuscript source file(s): ",
    paste(source_files[!file.exists(paths)], collapse = ", "),
    call. = FALSE
  )
}

lines <- lapply(paths, readLines, warn = FALSE, encoding = "UTF-8")
names(lines) <- source_files

forbidden <- c(
  "posterior-like" = "posterior-like",
  "exponentiated loss contribution" = "exponentiated loss contribution",
  "loss-kernel augmentation" = "loss[- ]kernel augmentation",
  "exponentiated loss kernel" = "exponentiated loss kernel",
  "response-likelihood posterior" = "response[- ]likelihood posterior",
  "declared generalized target" = "declared generalized target",
  "Gaussian root blocks" = "Gaussian root blocks"
)

failures <- character()
for (file in source_files) {
  for (label in names(forbidden)) {
    hit <- grep(forbidden[[label]], lines[[file]], ignore.case = TRUE, perl = TRUE)
    if (length(hit)) {
      failures <- c(
        failures,
        sprintf(
          "%s:%d: forbidden phrase '%s': %s",
          file,
          hit,
          label,
          trimws(lines[[file]][hit])
        )
      )
    }
  }
}

required <- list(
  "main.tex" = c(
    "defines the generalized posterior induced by",
    "augmented generalized posterior",
    "Gaussian full conditional",
    "not a sampling model for \\(y_i\\)",
    "posterior predictive distribution for future responses"
  ),
  "rqr-gibbs-supplement.tex" = c(
    "Pseudo-AL Augmentation of the Generalized Posterior",
    "augmented generalized posterior",
    "generalized-posterior distributions for endpoint functions",
    "not a sampling density for \\(y_i\\)",
    "exact blocked Gibbs sampler"
  )
)

for (file in manuscripts) {
  text <- gsub("[[:space:]]+", " ", paste(lines[[file]], collapse = " "))
  for (pattern in required[[file]]) {
    if (!grepl(pattern, text, fixed = TRUE)) {
      failures <- c(
        failures,
        sprintf("%s: required terminology pattern not found: %s", file, pattern)
      )
    }
  }
}

unsafe_claims <- c(
  "interval-root draws are posterior predictive",
  "root-state draws are posterior predictive",
  "root-trajectory draws are posterior predictive",
  "the pseudo-AL representation is a response likelihood"
)
for (file in source_files) {
  file_lines <- tolower(lines[[file]])
  for (claim in unsafe_claims) {
    hit <- grep(tolower(claim), file_lines, fixed = TRUE)
    if (length(hit)) {
      failures <- c(
        failures,
        sprintf("%s:%d: unsafe interpretation: %s", file, hit, claim)
      )
    }
  }
}

if (length(failures)) {
  cat(paste(failures, collapse = "\n"), "\n", sep = "")
  stop("Bayesian manuscript-language validation failed.", call. = FALSE)
}

summary <- data.frame(
  file = manuscripts,
  lines = vapply(lines[manuscripts], length, integer(1)),
  generalized_posterior_mentions = vapply(
    lines[manuscripts],
    function(x) sum(grepl("generalized[- ]posterior", x, ignore.case = TRUE)),
    integer(1)
  ),
  full_conditional_mentions = vapply(
    lines[manuscripts],
    function(x) sum(grepl("full[- ]conditional", x, ignore.case = TRUE)),
    integer(1)
  ),
  row.names = NULL
)
print(summary)
cat("Bayesian manuscript-language validation passed.\n")
