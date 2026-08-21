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
  "Gaussian root blocks" = "Gaussian root blocks",
  "duplicated generalized" = "generalized[[:space:]-]+generalized",
  "duplicated full" = "full[[:space:]-]+full"
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

main_only_forbidden <- c(
  "branch-level development language" = "branch[- ]level",
  "proof-gated development language" = "proof[- ]gated",
  "internal theorem-gate identifier" = "\\\\texttt\\{T[-]",
  "internal proof-gate prose" = "proof gates",
  "internal audit status" = "audit[- ]pending",
  "theorem-ledger development language" = "current theorem ledger",
  "source TODO marker" = "TODO[-:]",
  "placeholder latent scales" = "placeholder latent scales",
  "repository-facing audit design" = "audit design|audit domain",
  "ambiguous static scan" = "static scan",
  "ambiguous Gibbs scan order" = "transition and scan order"
)
for (label in names(main_only_forbidden)) {
  hit <- grep(
    main_only_forbidden[[label]],
    lines[["main.tex"]],
    ignore.case = TRUE,
    perl = TRUE
  )
  if (length(hit)) {
    failures <- c(
      failures,
      sprintf(
        "main.tex:%d: forbidden article phrase '%s': %s",
        hit,
        label,
        trimws(lines[["main.tex"]][hit])
      )
    )
  }
}

required <- list(
  "main.tex" = c(
    "The loss is built from",
    "mean-preserving interval (MPI)",
    "mean-tilted interval (MTI) family",
    "Tolerance confidence is supplied by scan calibration",
    "posterior credibility describes the fitted generalized posterior",
    "direct Dirichlet-process response-distribution layer gives exact fixed-interval Beta",
    "The pointwise MPI loss is",
    "The familiar check-loss notation gives an equivalent compact representation",
    "The check loss therefore acts on the scalar product residual",
    "Thus the prior is updated by the cumulative interval loss",
    "augmented generalized posterior",
    "Gaussian full conditional",
    "any sampling model for \\(Y\\) belongs to a separate response-distribution layer",
    "response-predictive draws require a response-distribution layer",
    "Calibrated Minimum-Width Tolerance Intervals",
    "The available theory is deliberately narrower than the computational procedures",
    "L_q'=\\frac{b_q}{\\lambda_q(a_q-b_q)}<0",
    "U_q'=\\frac{a_q}{\\lambda_q(a_q-b_q)}>0"
  ),
  "rqr-gibbs-supplement.tex" = c(
    "mean-preserving interval (MPI)",
    "Mean-Preserving Interval Scores",
    "The check function is applied to the product residual",
    "Pseudo-AL Augmentation and Full Conditionals",
    "augmented generalized posterior",
    "generalized-posterior distributions for endpoint functions",
    "density in the abstract pseudo-residual",
    "Response likelihoods belong to a separately specified response-distribution model",
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
