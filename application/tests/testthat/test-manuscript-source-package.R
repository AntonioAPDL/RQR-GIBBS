root <- normalizePath(if (file.exists("application/scripts/lib/manuscript_source_package.R")) "." else "../../..")
source(file.path(root, "application/scripts/lib/manuscript_source_package.R"))

source_fixture <- function() {
  directory <- tempfile("manuscript source "); dir.create(directory)
  source <- file.path(directory, "source"); build <- file.path(directory, "build")
  dir.create(source); dir.create(build); dir.create(file.path(source, "tables"))
  files <- c("main.tex", "tables/first.tex", "tables/nested.tex", "tables/unused.tex", "refs.bib")
  for (name in files) writeLines(name, file.path(source, name))
  for (name in c("main.bbl", "main.aux", "main.pdf")) writeLines(name, file.path(build, name))
  external <- file.path(directory, "external.sty"); writeLines("external", external)
  recorder <- file.path(build, "main.fls")
  writeLines(c(paste("PWD", source), "INPUT main.tex", "INPUT tables/first.tex",
    paste("INPUT", file.path(source, "tables/nested.tex")), "INPUT tables/nested.tex",
    paste("INPUT", file.path(build, "main.bbl")), paste("INPUT", file.path(build, "main.aux")),
    paste("INPUT", file.path(build, "main.pdf")), paste("INPUT", external)), recorder)
  list(directory = directory, source = source, build = build, recorder = recorder)
}

testthat::test_that("recorder inputs include nested actual dependencies and fresh bibliography only", {
  x <- source_fixture(); on.exit(unlink(x$directory, recursive = TRUE))
  files <- msp_inputs(x$recorder, x$source, x$build, "main")
  testthat::expect_setequal(names(files), c("main.tex", "tables/first.tex", "tables/nested.tex", "main.bbl"))
  testthat::expect_false(anyDuplicated(names(files)) > 0)
  testthat::expect_true(all(file.exists(files)))
  testthat::expect_false(msp_inside(paste0(x$source, "-other/secret.tex"), x$source))
})

testthat::test_that("missing inputs, bibliography, and malformed recorders stop packaging", {
  x <- source_fixture(); on.exit(unlink(x$directory, recursive = TRUE))
  unlink(file.path(x$source, "tables/nested.tex"))
  testthat::expect_error(msp_inputs(x$recorder, x$source, x$build, "main"), "input is missing")
  writeLines(c(paste("PWD", x$source), "INPUT main.tex"), x$recorder)
  testthat::expect_error(msp_inputs(x$recorder, x$source, x$build, "main"), "generated bibliography")
  writeLines("INPUT main.tex", x$recorder)
  testthat::expect_error(msp_inputs(x$recorder, x$source, x$build, "main"), "one working directory")
})

testthat::test_that("source copies preserve hashes and never overwrite or escape the destination", {
  x <- source_fixture(); on.exit(unlink(x$directory, recursive = TRUE))
  files <- msp_inputs(x$recorder, x$source, x$build, "main")
  destination <- file.path(x$directory, "upload"); dir.create(destination)
  msp_copy(files, destination)
  testthat::expect_identical(msp_hash(file.path(destination, "tables/nested.tex")), msp_hash(files[["tables/nested.tex"]]))
  testthat::expect_error(msp_copy(files, destination), "Could not copy")
  testthat::expect_error(msp_copy(setNames(files[1], "../escape.tex"), destination), "stay inside")
  testthat::expect_error(msp_copy(setNames(files[1], "/escape.tex"), destination), "stay inside")
  testthat::expect_error(msp_prepare(x$source, stamp = "../unsafe"), "Invalid package stamp")
  testthat::expect_error(msp_prepare(x$source, include_supplement = NA), "TRUE or FALSE")
})

testthat::test_that("the source command delegates dependency discovery and avoids deleting old packages", {
  wrapper <- readLines(file.path(root, "application/scripts/31_prepare_arxiv_source.sh"))
  testthat::expect_true(any(grepl("115_prepare_mti_manuscript_source.R", wrapper, fixed = TRUE)))
  testthat::expect_false(any(grepl("rm -rf|required_files=", wrapper)))
  make <- readLines(file.path(root, "Makefile"))
  testthat::expect_true(any(grepl("^article-handoff:", make)))
  for (target in c("arxiv-source", "article-handoff")) {
    dependencies <- grep(paste0("^",target,":"), make, value = TRUE)
    testthat::expect_true(any(grepl("verify-mti-fixed-prior-report", dependencies, fixed = TRUE)))
    testthat::expect_true(any(grepl("verify-mti-gibbs-closeout", dependencies, fixed = TRUE)))
    testthat::expect_true(any(grepl("verify-mti-skewness-publication", dependencies, fixed = TRUE)))
  }
})

testthat::test_that("the completed-study reference points to existing derivations and fixed study identity", {
  guide <- paste(readLines(file.path(root, "docs/reproducibility/mti_fixed_prior_study_reference_20261115.md")), collapse = "\n")
  labels <- unique(regmatches(guide, gregexpr("(?:sec|prop):[[:alnum:]:-]+", guide, perl = TRUE))[[1]])
  sources <- paste(c(readLines(file.path(root, "main.tex")), readLines(file.path(root, "rqr-gibbs-supplement.tex"))),
    collapse = "\n")
  testthat::expect_true(all(vapply(labels, function(x) grepl(paste0("\\label{", x, "}"), sources, fixed = TRUE), logical(1))))
  summary <- jsonlite::read_json(file.path(root, "figures/data/mti_fixed_prior_20261115/provenance.json"))
  testthat::expect_true(grepl(summary$simulation_source, guide, fixed = TRUE))
  testthat::expect_true(grepl("72,000", guide, fixed = TRUE))
  testthat::expect_true(grepl("marginal slice-within-Gibbs", guide, fixed = TRUE))
  testthat::expect_true(grepl("remain fixed throughout its chains", guide, fixed = TRUE))
  expanded <- paste(readLines(file.path(root, "docs/reproducibility/mti_skewness_study_reference_20261122.md")),
    collapse = "\n")
  evidence <- jsonlite::read_json(file.path(root, "figures/data/mti_skewness_extension_20261121/provenance.json"))
  testthat::expect_true(grepl(evidence$study_source_commit, expanded, fixed = TRUE))
  testthat::expect_true(grepl("324,000", expanded, fixed = TRUE))
  testthat::expect_true(grepl("no new Gibbs or application fits",
    gsub("[[:space:]]+", " ", expanded), fixed = TRUE))
  builder <- paste(readLines(file.path(root, "application/scripts/lib/manuscript_source_package.R")), collapse = "\n")
  testthat::expect_true(grepl("Float too large", builder, fixed = TRUE))
  testthat::expect_true(grepl("primary_extension_commit = expanded$study_source_commit", builder, fixed = TRUE))
})

testthat::test_that("source receipts distinguish the primary and separate Gibbs extensions", {
  records <- msp_study_records(root)
  testthat::expect_equal(records$primary_cases, 108L)
  testthat::expect_false(records$extension_includes_gibbs)
  testthat::expect_equal(records$gibbs_cases, 108L)
  testthat::expect_equal(records$gibbs_added_fits, 36000L)
  testthat::expect_match(records$gibbs_extension_commit, "^[a-f0-9]{40}$")
  testthat::expect_match(records$gibbs_extension_identity, "^[a-f0-9]{64}$")
})
