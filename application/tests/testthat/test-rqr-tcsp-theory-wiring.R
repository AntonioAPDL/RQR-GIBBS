test_that("TCSP-MTI proof ledger exposes report3 theorem gates", {
  repo_root <- normalizePath(testthat::test_path("..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
  ledger <- readLines(file.path(
    repo_root, "docs", "theory",
    "tcsp_mti_proof_ledger_20260811.md"
  ), warn = FALSE)
  text <- paste(ledger, collapse = "\n")

  required_ids <- c(
    "T-ACTION", "T-SCAN-1", "T-SCAN-2", "T-FEAS", "T-OPT-1",
    "T-CT-1", "T-CT-2", "T-SH-1", "T-SH-2", "T-FAIL",
    "T-MIXED", "T-CONTENT-IF", "T-ORTH", "T-QASY",
    "T-ORACLE-CAL", "T-WIDTH", "T-OPT-2", "T-REGRET",
    "T-SCAN-ASY", "T-GB-PROP", "T-GB-PLUGIN", "T-GB-ACTION",
    "T-SANDWICH", "T-LOCAL", "T-REG", "T-COND-SCOPE"
  )
  for (id in required_ids) {
    expect_match(text, id, fixed = TRUE)
  }
  expect_match(text, "posterior credibility equals tolerance confidence",
               fixed = TRUE)
  expect_match(text, "the Monte Carlo scan calibration is exact", fixed = TRUE)
})

test_that("TCSP manuscript exposes publication-facing claim boundaries", {
  repo_root <- normalizePath(testthat::test_path("..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
  main <- paste(readLines(file.path(repo_root, "main.tex"), warn = FALSE),
                collapse = "\n")
  supplement <- paste(readLines(
    file.path(repo_root, "rqr-gibbs-supplement.tex"), warn = FALSE
  ), collapse = "\n")
  registry <- paste(readLines(file.path(
    repo_root, "docs", "validation",
    "tcsp_benchmark_registry_20260812.md"
  ), warn = FALSE), collapse = "\n")

  expect_match(main, "Calibrated Minimum-Width Tolerance Intervals",
               fixed = TRUE)
  expect_match(main,
               "The theoretical scope is deliberately narrower",
               fixed = TRUE)
  expect_false(grepl("\\\\texttt\\{T[-]", main, perl = TRUE))
  expect_match(main, "posterior-to-action transfer", fixed = TRUE)
  expect_match(main, "Posterior credibility is not", fixed = TRUE)
  expect_match(supplement, "Scan-calibrated shortest-window tolerance",
               fixed = TRUE)
  expect_match(registry, "smallest_nonparametric_tolerance_regions",
               fixed = TRUE)
  expect_match(registry, "cal_gibbs_tolerance_gibbsTI", fixed = TRUE)
  expect_match(registry, "dp_tolerance_intervals", fixed = TRUE)
})
