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
included_text <- file.path("tables", list.files(file.path(repo_root, "tables"),
  pattern = "^(pharma_application_.*|mti_gibbs_comparison_(main|supplement))\\.tex$"))
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
source(file.path(repo_root, "application", "scripts", "lib", "pharma_manuscript_language.R"))
for (file in manuscripts) {
  start <- grep("\\\\section\\{Pharmaceutical", lines[[file]])
  if (length(start) != 1L) stop("Expected one pharmaceutical section in ", file)
  later <- grep("^\\\\section\\{", lines[[file]])
  end <- later[later > start][1L]
  if (is.na(end)) end <- length(lines[[file]]) + 1L
  application <- paste(lines[[file]][seq.int(start, end - 1L)], collapse = " ")
  findings <- pma_language_findings(application)
  if (length(findings)) failures <- c(failures, paste(file, findings, sep = ": "))
}
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
  "article-facing terminology" = "article[- ]facing",
  "paper-matched terminology" = "paper[- ]matched",
  "internal winner terminology" = "\\bwinner\\b",
  "fail-closed terminology" = "fail[- ]closed",
  "source-of-truth terminology" = "source[- ]of[- ]truth",
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
  "ambiguous Gibbs scan order" = "transition and scan order",
  "certified interval action" = "certified interval action",
  "tolerance-action wording" = "tolerance action",
  "scan-action wording" = "scan action",
  "content-confidence action wording" = "content-confidence action",
  "formal-action wording" = "formal action"
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
    "mean-preserving interval (MPI)",
    "mean-tilted interval (MTI)",
    "ECM estimates endpoints under the MTI generalized-posterior criterion",
    "repeated-sampling content--confidence statement",
    "shortest eligible fitted interval is reported",
    "Dirichlet-process (DP)",
    "The pointwise MPI loss is",
    "The familiar check-loss notation gives an equivalent compact representation",
    "The check loss therefore acts on the scalar product residual",
    "learning rate controls the strength of the loss update rather than the population content",
    "loss-based generalized posterior for a fixed interval target",
    "conditionally conjugate Gibbs updates",
    "response prediction would require a separate response-distribution model",
    "content-attainment rate",
    "known simulation distribution",
    "Widths are reported as distribution-level empirical",
    "Calibrated MTI Tolerance Intervals",
    "MTI is the proposed method in the comparison",
    "all reported cells",
    "allowing paired comparisons of attainment and width",
    "a distribution-free tolerance theorem for the fitted-content and tilt selection rule",
    "Regression-family and dynamic tolerance calibration require additional theory",
    "L_q'=\\frac{b_q}{\\lambda_q(a_q-b_q)}<0",
    "U_q'=\\frac{a_q}{\\lambda_q(a_q-b_q)}>0"
  ),
  "rqr-gibbs-supplement.tex" = c(
    "mean-preserving interval (MPI)",
    "Mean-Preserving Interval Scores",
    "The check function is applied to the product residual",
    "Empirical Balance",
    "Fixed-Target MTI Computation",
    "Direct DP Content Check",
    "Direct DP fixed-interval content law",
    "The direct DP check determines eligible MTI candidates",
    "Gibbs Conditional Distributions",
    "ECM Conditional Maximizations",
    "Generalized Posterior and Propriety",
    "Proper fixed-target generalized posterior",
    "Posterior Modes and Empirical Balance",
    "216,000",
    "Tolerance-Validation Protocol and Feasibility",
    "Full-range feasibility thresholds",
    "The validation evidence is confined"
  )
)

removed_solver_material <- c("Convex midpoint profile", "sec:supp-profiled-intercept",
  "prop:supp-convex-intercept-profile", "Profile calculation and numerical checks")
for (file in manuscripts) {
  for (pattern in removed_solver_material) {
    if (any(grepl(pattern, lines[[file]], fixed = TRUE))) {
      failures <- c(failures, sprintf("%s: removed numerical solver material: %s", file, pattern))
    }
  }
}
has_posterior_assessment <- FALSE
for (suffix in c("main", "supplement")) {
  file <- paste0("tables/mti_gibbs_comparison_", suffix, ".tex")
  path <- file.path(repo_root, file)
  if (!file.exists(path)) failures <- c(failures, paste("Missing posterior assessment inclusion:", file))
  else if (any(nzchar(trimws(readLines(path, warn = FALSE))))) has_posterior_assessment <- TRUE
  manuscript <- if (suffix == "main") "main.tex" else "rqr-gibbs-supplement.tex"
  if (!any(grepl(paste0("\\input{", file, "}"), lines[[manuscript]], fixed = TRUE))) {
    failures <- c(failures, paste(manuscript, "is missing its Gibbs comparison inclusion"))
  }
}
if (has_posterior_assessment) {
  assessment <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c(shQuote(file.path(repo_root, "application/scripts/110_review_mti_gibbs.R")), "--mode=verify"),
    stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(assessment, "status")) && attr(assessment, "status") != 0L) {
    failures <- c(failures, "Posterior assessment text lacks verified matching evidence.", assessment)
  }
}

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
