ordinary_v1_f01_source_path <- function(...) {
  testthat::test_path("..", "..", ...)
}

ordinary_v1_f01_read <- function(path) {
  utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = ""
  )
}

test_that("ordinary-v1 F01 quadrature is source-bound and complete", {
  generator <- ordinary_v1_f01_source_path(
    "scripts", "30_generate_ordinary_v1_f01_references.R"
  )
  artifact <- ordinary_v1_f01_source_path(
    "inst", "extdata", "ordinary_v1_f01_quadrature_references.csv"
  )
  mean_source <- ordinary_v1_f01_source_path(
    "inst", "extdata",
    "ordinary_v1_f01_independent_mean_references.csv"
  )
  cdf_source <- ordinary_v1_f01_source_path(
    "inst", "extdata", "output7_corrected_cdf_references.csv"
  )

  expect_true(all(file.exists(c(
    generator, artifact, mean_source, cdf_source
  ))))
  observed <- ordinary_v1_f01_read(artifact)
  expected_columns <- c(
    "schema_version", "fixture_id", "comparison_type", "estimand",
    "threshold", "tracked_reference_schema", "tracked_value",
    "reproduced_value", "absolute_difference",
    "comparison_tolerance", "quadrature_order", "previous_order",
    "order_convergence_difference", "order_convergence_tolerance",
    "reference_method", "tracked_source_path",
    "tracked_source_sha256", "tracked_provenance_sha256",
    "generator_path", "generator_sha256", "pass"
  )
  expect_identical(names(observed), expected_columns)
  expect_identical(nrow(observed), 11L)
  expect_true(all(
    observed$schema_version ==
      "rqrgibbs_ordinary_v1_f01_quadrature/1.0.0"
  ))
  expect_true(all(observed$fixture_id == "F01"))
  expect_identical(
    as.character(observed$comparison_type),
    c(rep("mean", 6L), rep("cdf", 5L))
  )
  expect_identical(
    as.character(observed$estimand),
    c(
      "lambda", "lower_root", "upper_root", "width", "midpoint",
      "total_loss", "lambda", "lower_root", "upper_root", "width",
      "midpoint"
    )
  )
  expect_true(all(observed$pass))
  expect_true(all(
    observed$absolute_difference <= observed$comparison_tolerance
  ))
  expect_true(all(
    observed$order_convergence_difference <=
      observed$order_convergence_tolerance
  ))
  expect_true(all(observed$quadrature_order == 80L))
  expect_true(all(observed$previous_order == 64L))
  expect_true(all(
    observed$tracked_reference_schema[
      observed$comparison_type == "mean"
    ] == "rqrgibbs_ordinary_v1_f01_mean_reference/1.0.0"
  ))
  expect_true(all(
    observed$tracked_reference_schema[
      observed$comparison_type == "cdf"
    ] == "rqrgibbs_intercept_cdf_reference/2.0.0"
  ))
  expect_identical(
    as.numeric(observed$threshold[observed$comparison_type == "cdf"]),
    c(1, -1.5, 2.5, 4, 0.5)
  )
  expect_true(all(is.na(
    observed$threshold[observed$comparison_type == "mean"]
  )))

  generator_sha256 <- digest::digest(
    file = generator, algo = "sha256", serialize = FALSE
  )
  expect_identical(unique(observed$generator_sha256), generator_sha256)
  expect_identical(
    unique(observed$generator_path),
    "application/scripts/30_generate_ordinary_v1_f01_references.R"
  )
  source_hashes <- c(
    "application/inst/extdata/ordinary_v1_f01_independent_mean_references.csv" =
      digest::digest(
        file = mean_source, algo = "sha256", serialize = FALSE
      ),
    "application/inst/extdata/output7_corrected_cdf_references.csv" =
      digest::digest(
        file = cdf_source, algo = "sha256", serialize = FALSE
      )
  )
  expect_identical(
    unname(source_hashes[observed$tracked_source_path]),
    as.character(observed$tracked_source_sha256)
  )
  expect_true(all(grepl(
    "^[0-9a-f]{64}$", observed$tracked_provenance_sha256
  )))
})

test_that("ordinary-v1 F01 values match the independent references", {
  artifact <- ordinary_v1_f01_source_path(
    "inst", "extdata", "ordinary_v1_f01_quadrature_references.csv"
  )
  observed <- ordinary_v1_f01_read(artifact)
  expected <- c(
    1.1347690848653513,
    -1.429561444961876,
    2.4442393354324303,
    3.8738007803943084,
    0.5073389452352769,
    10.163729538711271,
    0.347247584303805,
    0.408193003274045,
    0.562140568140968,
    0.573003849468578,
    0.489059519337561
  )
  expect_equal(observed$tracked_value, expected, tolerance = 5e-15)
  expect_equal(observed$reproduced_value, expected, tolerance = 1e-9)
})

test_that("ordinary-v1 F01 generator is deterministic and sampler-free", {
  generator <- ordinary_v1_f01_source_path(
    "scripts", "30_generate_ordinary_v1_f01_references.R"
  )
  artifact <- ordinary_v1_f01_source_path(
    "inst", "extdata", "ordinary_v1_f01_quadrature_references.csv"
  )
  source_text <- paste(readLines(generator, warn = FALSE), collapse = "\n")
  expect_false(grepl(
    "rqr_mcmc|rqr_dlm_fit|rqr_desn_fit|sample.int|runif|rnorm",
    source_text
  ))
  expect_false(grepl(
    "library\\(rqrgibbs\\)|requireNamespace\\([\"']rqrgibbs",
    source_text
  ))

  reproduced <- tempfile(fileext = ".csv")
  on.exit(unlink(reproduced, force = TRUE), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  status <- system2(
    rscript, c(shQuote(generator), shQuote(reproduced)),
    stdout = TRUE, stderr = TRUE
  )
  expect_null(attr(status, "status"))
  expect_identical(
    digest::digest(
      file = reproduced, algo = "sha256", serialize = FALSE
    ),
    digest::digest(file = artifact, algo = "sha256", serialize = FALSE)
  )
})
