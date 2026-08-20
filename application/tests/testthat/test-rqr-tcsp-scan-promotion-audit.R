`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("TCSP scan-audit script respects explicit design cells", {
  output_dir <- tempfile("tcsp-scan-audit-")
  config_path <- tempfile("tcsp-scan-audit-config-", fileext = ".json")
  on.exit(unlink(c(output_dir, config_path), recursive = TRUE, force = TRUE),
          add = TRUE)

  config <- list(
    scan_calibration = list(
      smoke_n_sim = 60L,
      smoke_numerical_confidence = 0.80,
      seed = 1234L
    ),
    modes = list(
      smoke = list(
        design_cells = list(
          list(cell_id = "first", n = 20L, guaranteed_content = 0.40,
               tolerance_confidence = 0.80),
          list(cell_id = "second", n = 30L, guaranteed_content = 0.60,
               tolerance_confidence = 0.80)
        )
      )
    )
  )
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)

  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts", "78_audit_tcsp_scan_calibration_adaptive.R"
    ),
    winslash = "/", mustWork = TRUE
  )
  status <- system2(
    "Rscript",
    c(
      script,
      "--mode=smoke",
      paste0("--config=", config_path),
      "--n-sim=60",
      "--numerical-confidence=0.80",
      "--max-n-sim=60",
      "--stable-looks=1",
      paste0("--output-dir=", output_dir)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  cells <- read.csv(file.path(output_dir, "calibration_cells.csv"))
  boundary <- read.csv(file.path(output_dir, "boundary_map.csv"))
  manifest <- jsonlite::read_json(file.path(output_dir, "manifest.json"),
                                  simplifyVector = TRUE)

  expect_equal(nrow(cells), 2L)
  expect_equal(nrow(boundary), 2L)
  expect_equal(boundary$cell_id, c("first", "second"))
  expect_true(manifest$design_cells_from_config)
  expect_equal(manifest$n_design_cells, 2L)
})

test_that("TCSP promotion audit produces old-vs-adaptive comparison", {
  output_dir <- tempfile("tcsp-scan-promotion-")
  config_path <- tempfile("tcsp-scan-promotion-config-", fileext = ".json")
  on.exit(unlink(c(output_dir, config_path), recursive = TRUE, force = TRUE),
          add = TRUE)

  config <- list(
    scan_calibration = list(
      smoke_n_sim = 60L,
      smoke_numerical_confidence = 0.80,
      seed = 2234L
    ),
    modes = list(
      smoke = list(
        design_cells = list(
          list(cell_id = "only", n = 30L, guaranteed_content = 0.40,
               tolerance_confidence = 0.80)
        )
      )
    )
  )
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)

  script <- normalizePath(
    testthat::test_path(
      "..", "..", "scripts",
      "79_compare_tcsp_scan_calibration_promotion.R"
    ),
    winslash = "/", mustWork = TRUE
  )
  status <- system2(
    "Rscript",
    c(
      script,
      "--mode=smoke",
      paste0("--config=", config_path),
      "--n-sim=60",
      "--numerical-confidence=0.80",
      "--max-n-sim=60",
      "--stable-looks=1",
      "--workers=1",
      paste0("--output-dir=", output_dir)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_equal(attr(status, "status") %||% 0L, 0L)

  comparison <- read.csv(
    file.path(output_dir, "old_vs_adaptive_comparison.csv")
  )
  stability <- read.csv(
    file.path(output_dir, "adaptive_stability_summary.csv")
  )
  manifest <- jsonlite::read_json(file.path(output_dir, "manifest.json"),
                                  simplifyVector = TRUE)

  expect_equal(nrow(comparison), 1L)
  expect_true(all(c("delta_k", "promotion_relevance") %in%
                    names(comparison)))
  expect_equal(nrow(stability), 1L)
  expect_true(manifest$gate_status %in% c(
    "hold_unstable_calibration",
    "no_validation_relaunch_needed",
    "targeted_tcsp_relaunch_recommended",
    "investigate_before_promotion"
  ))
})
