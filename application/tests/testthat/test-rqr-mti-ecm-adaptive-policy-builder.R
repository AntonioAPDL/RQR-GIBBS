test_that("adaptive MTI-ECM policy builder writes configured cell decisions", {
  script <- test_path(
    "..", "..", "scripts", "82_build_mti_ecm_adaptive_policy.R"
  )
  skip_if_not(file.exists(script), "policy builder is outside package build")

  rows <- list()
  idx <- 0L
  add_rows <- function(method_id, dgp_id, n, content, tol_conf, successes,
                       reps, width_base, screen) {
    for (rr in seq_len(reps)) {
      idx <<- idx + 1L
      rows[[idx]] <<- data.frame(
        method_id = method_id,
        dgp_id = dgp_id,
        n = n,
        guaranteed_content = content,
        tolerance_confidence = tol_conf,
        replication = rr,
        success = rr <= successes,
        infeasible = FALSE,
        width = width_base + rr / 1000,
        effective_posterior_confidence = screen,
        stringsAsFactors = FALSE
      )
    }
  }
  for (dgp in c("normal", "exponential")) {
    add_rows("mti_ecm_dp_profile_tune_low", dgp, 20, 0.50, 0.80,
             successes = 7, reps = 10, width_base = 2, screen = 0.90)
    add_rows("mti_ecm_dp_profile_tune_high", dgp, 20, 0.50, 0.80,
             successes = 10, reps = 10, width_base = 3, screen = 0.98)
  }
  input <- tempfile("adaptive-policy-input-", fileext = ".csv")
  output <- tempfile("adaptive-policy-output-", fileext = ".csv")
  write.csv(do.call(rbind, rows), input, row.names = FALSE)

  status <- system2(
    "Rscript",
    c(
      script,
      paste0("--results=", input),
      paste0("--output=", output),
      "--policy-id=unit_policy",
      "--selection=cell",
      "--bound-method=wilson",
      "--bound-confidence=0.50",
      "--margin=0"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  out <- read.csv(output)
  expect_equal(nrow(out), 1L)
  expect_equal(out$policy_id, "unit_policy")
  expect_equal(out$method_id, "mti_ecm_adaptive_cell")
  expect_equal(out$source_method_id, "mti_ecm_dp_profile_tune_high")
  expect_equal(out$bound_method, "wilson")
  expect_equal(out$bound_confidence, 0.50)
  expect_equal(out$delivery_target, 0.80)
  expected_required <- rqrgibbs::rqr_delivery_min_successes(
    replications = out$calibration_replications[[1L]],
    target = 0.80,
    confidence = 0.50,
    method = "wilson"
  )
  expect_equal(out$minimum_successes_required, expected_required)
  expect_equal(
    out$success_margin_to_requirement,
    out$calibration_successes - expected_required
  )
  expect_true("min_success_margin_to_requirement" %in% names(out))
  expect_true(nzchar(out$input_results_digest))
})

test_that("adaptive MTI-ECM policy builder supports pooled cell deployment", {
  script <- test_path(
    "..", "..", "scripts", "82_build_mti_ecm_adaptive_policy.R"
  )
  skip_if_not(file.exists(script), "policy builder is outside package build")

  rows <- list()
  idx <- 0L
  add_rows <- function(method_id, dgp_id, n, content, tol_conf, successes,
                       reps, width_base, screen) {
    for (rr in seq_len(reps)) {
      idx <<- idx + 1L
      rows[[idx]] <<- data.frame(
        method_id = method_id,
        dgp_id = dgp_id,
        n = n,
        guaranteed_content = content,
        tolerance_confidence = tol_conf,
        replication = rr,
        success = rr <= successes,
        infeasible = FALSE,
        width = width_base + rr / 1000,
        effective_posterior_confidence = screen,
        stringsAsFactors = FALSE
      )
    }
  }
  add_rows("mti_ecm_adaptive_screen_p980", "hard_left", 20, 0.50, 0.80,
           successes = 7, reps = 10, width_base = 2, screen = 0.980)
  add_rows("mti_ecm_adaptive_screen_p980", "hard_right", 20, 0.50, 0.80,
           successes = 10, reps = 10, width_base = 2, screen = 0.980)
  add_rows("mti_ecm_adaptive_screen_p985", "hard_left", 20, 0.50, 0.80,
           successes = 10, reps = 10, width_base = 3, screen = 0.985)
  add_rows("mti_ecm_adaptive_screen_p985", "hard_right", 20, 0.50, 0.80,
           successes = 10, reps = 10, width_base = 3, screen = 0.985)

  input <- tempfile("adaptive-pooled-input-", fileext = ".csv")
  output <- tempfile("adaptive-pooled-output-", fileext = ".csv")
  diagnostics <- tempfile("adaptive-pooled-diagnostics-", fileext = ".csv")
  write.csv(do.call(rbind, rows), input, row.names = FALSE)

  status <- system2(
    "Rscript",
    c(
      script,
      paste0("--results=", input),
      paste0("--output=", output),
      paste0("--diagnostics-output=", diagnostics),
      "--policy-id=pooled_policy",
      "--selection=pooled-cell",
      "--method-pattern=^mti_ecm_adaptive_screen_",
      "--bound-method=wilson",
      "--bound-confidence=0.50",
      "--margin=0"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  out <- read.csv(output)
  expect_equal(nrow(out), 1L)
  expect_equal(out$policy_id, "pooled_policy")
  expect_equal(out$method_id, "mti_ecm_adaptive_cell")
  expect_equal(out$selection_rule, "pooled-cell")
  expect_equal(out$source_method_id, "mti_ecm_adaptive_screen_p980")
  expect_equal(out$calibration_scope, "pooled_cell")
  expect_equal(out$calibration_replications, 20L)
  expect_equal(out$calibration_successes, 17L)
  expect_equal(out$delivery_target, 0.80)
  expect_equal(
    out$minimum_successes_required,
    rqrgibbs::rqr_delivery_min_successes(
      replications = 20,
      target = 0.80,
      confidence = 0.50,
      method = "wilson"
    )
  )
  expect_equal(
    out$success_margin_to_requirement,
    out$calibration_successes - out$minimum_successes_required
  )
  expect_equal(out$screen, 0.980)

  diag <- read.csv(diagnostics)
  expect_true(all(c(
    "minimum_successes_required",
    "success_margin_to_requirement"
  ) %in% names(diag)))
  expect_true(all(c("pooled_cell", "distribution_cell") %in%
                    diag$calibration_scope))
  hard_left <- diag[
    diag$calibration_scope == "distribution_cell" &
      diag$source_method_id == "mti_ecm_adaptive_screen_p980" &
      diag$dgp_id == "hard_left",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(hard_left), 1L)
  expect_false(hard_left$admissible)
})

test_that("adaptive MTI-ECM policy builder labels unresolved strict cells", {
  script <- test_path(
    "..", "..", "scripts", "82_build_mti_ecm_adaptive_policy.R"
  )
  skip_if_not(file.exists(script), "policy builder is outside package build")

  rows <- list()
  idx <- 0L
  add_rows <- function(method_id, successes, width_base, screen) {
    for (rr in seq_len(10L)) {
      idx <<- idx + 1L
      rows[[idx]] <<- data.frame(
        method_id = method_id,
        dgp_id = "hard_case",
        n = 40L,
        guaranteed_content = 0.90,
        tolerance_confidence = 0.95,
        replication = rr,
        success = rr <= successes,
        infeasible = FALSE,
        width = width_base + rr / 1000,
        effective_posterior_confidence = screen,
        stringsAsFactors = FALSE
      )
    }
  }
  add_rows("mti_ecm_adaptive_strict_screen_p980", 8L, 2, 0.980)
  add_rows("mti_ecm_adaptive_strict_screen_p9995", 9L, 3, 0.9995)

  input <- tempfile("adaptive-unresolved-input-", fileext = ".csv")
  output <- tempfile("adaptive-unresolved-output-", fileext = ".csv")
  diagnostics <- tempfile("adaptive-unresolved-diagnostics-", fileext = ".csv")
  write.csv(do.call(rbind, rows), input, row.names = FALSE)

  status <- system2(
    "Rscript",
    c(
      script,
      paste0("--results=", input),
      paste0("--output=", output),
      paste0("--diagnostics-output=", diagnostics),
      "--policy-id=strict_unresolved",
      "--selection=cell",
      "--method-pattern=^mti_ecm_adaptive_strict_screen_",
      "--bound-method=wilson",
      "--bound-confidence=0.50",
      "--margin=0"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  out <- read.csv(output)
  expect_equal(nrow(out), 1L)
  expect_equal(out$source_method_id, "mti_ecm_adaptive_strict_screen_p9995")
  expect_equal(out$decision, "best_effort_unresolved_by_configured_bound")
  expect_false(out$admissible)
  expect_lt(out$success_margin_to_requirement, 0)
  expect_equal(out$min_success_margin_to_requirement,
               out$success_margin_to_requirement)
})
