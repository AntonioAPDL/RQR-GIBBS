psa_test_env <- function() {
  e <- new.env(parent = globalenv())
  for (name in c("pharma_tolerance_application.R", "pharma_mti_application.R",
    "pharma_mti_reporting.R", "pharma_source_structure.R"))
    sys.source(testthat::test_path("..", "..", "scripts", "lib", name), envir = e)
  e
}

test_that("campaign mapping is exact, order independent, and rejects ambiguous identifiers", {
  e <- psa_test_env()
  p <- data.frame(batch = c(3, 1, 1, 2), campaign = c(2, 1, 1, 1), code = 23)
  m <- e$psa_campaign_mapping(p, 1:3)
  expect_equal(m$batch, 1:3)
  expect_equal(e$psa_campaign_mapping(p[4:1, ], 3:1), m)
  expect_error(e$psa_campaign_mapping(p, 1:4), "match every")
  p$campaign[3] <- 9
  expect_error(e$psa_campaign_mapping(p, 1:3), "exactly once")
  p$campaign[3] <- NA
  expect_error(e$psa_campaign_mapping(p, 1:3), "missing")
  p$code[3] <- 24
  p$campaign[3] <- 1
  expect_error(e$psa_campaign_mapping(p, 1:3), "Unexpected")
})

test_that("measurement diagnostics flag questions without altering responses", {
  e <- psa_test_env()
  d <- data.frame(batch = 1:2, code = 23, fct_max_thickness = 5,
    fct_max_diameter = 10, fct_min_hardness = 35, fct_av_hardness = c(62.72, 40),
    fct_max_hardness = 61, tbl_min_hardness = 30, tbl_av_hardness = c(20, 35),
    tbl_max_hardness = 40)
  d$fct_tensile <- with(d, 2 * fct_av_hardness / (3.14 * fct_max_thickness * fct_max_diameter))
  original <- d
  checks <- e$psa_measurement_checks(d)
  expect_identical(d, original)
  expect_equal(checks$counts$product_questions, c(1, 1))
  expect_equal(checks$maximum_tensile_error, 0)
  d$fct_max_diameter[1] <- 0
  expect_error(e$psa_measurement_checks(d), "Nonpositive")
})

test_that("sharing counts do not confuse represented campaigns with separate API groups", {
  e <- psa_test_env()
  observed <- data.frame(.analysis_row = 1:4, batch = 11:14, api_batch = c(1, 2, 1, 2))
  mapping <- data.frame(batch = 11:14, campaign = c(1, 1, 2, 2))
  membership <- data.frame(analysis_row_id = rep(1:4, 2), split_scheme = "month_grouped",
    split_id = rep(1:2, each = 4), training = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))
  z <- e$psa_partition_sharing(observed, membership, mapping, 2)
  expect_equal(z$campaign_shared, c(0, 2))
  expect_equal(z$campaign_heldout_shared, c(0, 2))
  expect_equal(z$api_shared, c(2, 0))
  expect_equal(e$psa_partition_sharing(observed[4:1, ], membership[8:1, ], mapping[4:1, ], 2), z)
  observed$unrelated_response <- c(100, -100, 5, 2)
  expect_equal(e$psa_partition_sharing(observed, membership, mapping, 2), z)
  expect_error(e$psa_partition_sharing(observed, membership[-1, ], mapping, 2), "Incomplete")
  membership$analysis_row_id[1] <- 9
  expect_error(e$psa_partition_sharing(observed, membership, mapping, 2), "unknown")
})

test_that("joint components preserve every stated identifier and impossible sizes are rejected", {
  skip_if_not_installed("igraph")
  e <- psa_test_env()
  d <- data.frame(batch = 1:8, campaign = rep(1:4, each = 2),
    api_batch = c(1, 1, 2, 2, 3, 3, 4, 4))
  a <- e$psa_separation_design(d, c("campaign", "api_batch"), 4)
  expect_equal(a$feasible_training_sets, 6)
  prepared <- e$psa_prepare_partitions(d, c("campaign", "api_batch"), 4, 100, 123)
  expect_equal(length(unique(prepared$partition_id)), 6)
  expect_equal(prepared, e$psa_prepare_partitions(d, c("campaign", "api_batch"), 4, 100, 123))
  for (i in unique(prepared$partition_id)) {
    z <- prepared[prepared$partition_id == i, ]
    train <- d[d$batch %in% z$batch[z$training], ]
    heldout <- d[!d$batch %in% train$batch, ]
    expect_equal(nrow(train), 4)
    for (v in c("campaign", "api_batch")) expect_length(intersect(train[[v]], heldout[[v]]), 0)
  }
  d$api_batch <- c(1, 1, 1, 1, 2, 2, 2, 2)
  expect_equal(e$psa_separation_design(d, c("campaign", "api_batch"), 3)$feasible_training_sets, 0)
  expect_error(e$psa_prepare_partitions(d, c("campaign", "api_batch"), 3, 2, 1), "No whole-group")
  expect_equal(e$psa_group_components(d[8:1, ], c("campaign", "api_batch")),
    rev(e$psa_group_components(d, c("campaign", "api_batch"))))
  d$api_batch[1] <- NA
  expect_error(e$psa_group_components(d, c("campaign", "api_batch")), "missing")
})

test_that("source verification never accepts changed bytes or downloads implicitly", {
  e <- psa_test_env()
  path <- tempfile()
  expect_error(e$psa_source_file(path, list()), "absent")
  writeLines("example", path)
  record <- list(bytes = file.info(path)$size, sha256 = e$pta_file_sha256(path))
  expect_equal(e$psa_source_file(path, record), path)
  writeLines("changed", path)
  expect_error(e$psa_source_file(path, record), "source bytes")
  unlink(path)
})

test_that("canonical retrospective summaries preserve pairing and all original results", {
  e <- psa_test_env()
  root <- testthat::test_path("..", "..", "..")
  evidence <- file.path(root, "figures/data/pharma_source_structure_20261026")
  skip_if_not(file.exists(file.path(evidence, "partition_sharing.csv")))
  sharing <- read.csv(file.path(evidence, "partition_sharing.csv"))
  results <- read.csv(file.path(root, "figures/data/pharma_mti_profile_application_20261025/results.csv"))
  original <- results
  a <- e$psa_retrospective_summary(results, sharing)
  expect_identical(results, original)
  expect_equal(nrow(a$selected), 63)
  expect_equal(sum(a$selected$api_shared > 0), 58)
  expect_equal(sort(unique(a$summary$splits)), c(63, 250))
  bad <- results[-which(results$split_scheme == "month_grouped" &
    results$split_id == a$selected$split_id[1])[1], ]
  expect_error(e$psa_retrospective_summary(bad, sharing), "missing methods")
  expect_error(e$psa_retrospective_summary(rbind(results, results[1, ]), sharing), "Duplicate")
  changed <- results
  i <- which(changed$split_scheme == "month_grouped" &
    changed$split_id == a$selected$split_id[1] & changed$response_id == "fct_tensile" &
    changed$method_id == e$pma_methods[1])[1]
  changed$interval_returned[i] <- FALSE
  changed$heldout_attains_content[i] <- FALSE
  changed$width[i] <- NA_real_
  b <- e$psa_retrospective_summary(changed, sharing)
  row <- b$summary[b$summary$response_id == "fct_tensile" &
    b$summary$method_id == e$pma_methods[1] &
    b$summary$comparison == "campaign_disjoint_month_subset", ]
  expect_equal(row$splits, 63)
  expect_equal(row$interval_return_rate, 62 / 63)
})

test_that("committed source evidence preserves the frozen method and prepared designs have no fits", {
  skip_if_not_installed("igraph")
  e <- psa_test_env()
  root <- normalizePath(testthat::test_path("..", "..", ".."))
  skip_if_not(file.exists(file.path(root, e$psa_default_config)))
  previous <- getwd(); setwd(root); on.exit(setwd(previous), add = TRUE)
  config <- e$pta_read_config(e$psa_default_config)
  expect_invisible(e$psa_verify(config))
  manifest <- e$pta_read_config(file.path(config$evidence_dir, "manifest.json"))
  expect_equal(manifest$new_fits, 0)
  expect_false(manifest$source_clarification_received)
  expect_identical(manifest$prepared_status, "prepared_not_fitted_response_definition_unresolved")
  expect_equal(manifest$campaigns, 28)
  expect_lte(manifest$maximum_tensile_formula_error, 5e-10)
  counts <- read.csv(file.path(config$evidence_dir, "measurement_counts.csv"))
  expect_equal(counts$product_questions[match(c("fct", "tbl"), counts$response)], c(36, 55))
  frozen <- "figures/data/pharma_mti_profile_application_20261025"
  expect_identical(e$pta_file_sha256(file.path(frozen, "results.csv")), config$results_sha256)
  expect_identical(e$pta_file_sha256(file.path(frozen, "split_membership.csv.gz")), config$membership_sha256)
  mapping <- read.csv(file.path(config$evidence_dir, "campaign_mapping.csv"))
  design <- read.csv(file.path(config$evidence_dir, "separation_designs.csv"))
  expect_equal(design$feasible_training_sets, c(5287020, 15868, 0))
  expect_equal(design$feasible, c(TRUE, TRUE, FALSE))
  observed <- read.csv(file.path(frozen, "observed_batches.csv"))
  prepared <- read.csv(gzfile(file.path(config$evidence_dir, "prepared_campaign_api_membership.csv.gz")))
  expect_equal(length(unique(prepared$partition_id)), 250)
  expect_equal(manifest$prepared_partitions, 250)
  expect_equal(nrow(prepared), 250 * 187)
  expect_false(anyDuplicated(prepared[c("partition_id", "batch")]) > 0)
  for (id in unique(prepared$partition_id)) {
    z <- prepared[prepared$partition_id == id, ]
    expect_equal(sum(z$training), 100)
    expect_setequal(z$batch, observed$batch)
    for (g in list(mapping$campaign[match(z$batch, mapping$batch)],
      observed$api_batch[match(z$batch, observed$batch)]))
      expect_length(intersect(g[z$training], g[!z$training]), 0)
  }
  temp <- tempfile("source-evidence-"); dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE), add = TRUE)
  file.copy(list.files(config$evidence_dir, full.names = TRUE), temp)
  writeLines("changed", file.path(temp, "measurement_counts.csv"))
  expect_error(e$psa_render(temp, tempfile()), "hash mismatch")
})
