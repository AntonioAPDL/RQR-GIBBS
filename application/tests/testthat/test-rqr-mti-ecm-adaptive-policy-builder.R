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

test_that("adaptive MTI-ECM policy builder rejects unstable width tails", {
  script <- test_path(
    "..", "..", "scripts", "82_build_mti_ecm_adaptive_policy.R"
  )
  skip_if_not(file.exists(script), "policy builder is outside package build")

  rows <- list()
  idx <- 0L
  add_rows <- function(method_id, widths, screen) {
    for (rr in seq_along(widths)) {
      idx <<- idx + 1L
      rows[[idx]] <<- data.frame(
        method_id = method_id,
        dgp_id = "skew_cell",
        n = 50L,
        guaranteed_content = 0.90,
        tolerance_confidence = 0.80,
        replication = rr,
        success = TRUE,
        infeasible = FALSE,
        width = widths[[rr]],
        effective_posterior_confidence = screen,
        candidate_feasible_count = 3L,
        stringsAsFactors = FALSE
      )
    }
  }
  add_rows(
    "mti_ecm_adaptive_refine_screen_p995",
    c(rep(1, 19), 500),
    0.995
  )
  add_rows(
    "mti_ecm_adaptive_refine_screen_p9975",
    rep(3, 20),
    0.9975
  )

  input <- tempfile("adaptive-stability-input-", fileext = ".csv")
  output <- tempfile("adaptive-stability-output-", fileext = ".csv")
  diagnostics <- tempfile("adaptive-stability-diagnostics-", fileext = ".csv")
  write.csv(do.call(rbind, rows), input, row.names = FALSE)

  status <- system2(
    "Rscript",
    c(
      script,
      paste0("--results=", input),
      paste0("--output=", output),
      paste0("--diagnostics-output=", diagnostics),
      "--policy-id=stability_policy",
      "--selection=cell",
      "--method-pattern=^mti_ecm_adaptive_refine_screen_",
      "--bound-method=wilson",
      "--bound-confidence=0.50",
      "--margin=0",
      "--width-objective=median-q975",
      "--max-width-q975-to-median=10"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  out <- read.csv(output)
  expect_equal(out$source_method_id, "mti_ecm_adaptive_refine_screen_p9975")
  expect_true(out$admissible)
  expect_equal(out$width_objective, "median-q975")

  diag <- read.csv(diagnostics)
  unstable <- diag[
    diag$calibration_scope == "all_distribution_cell" &
      diag$source_method_id == "mti_ecm_adaptive_refine_screen_p995",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(unstable), 1L)
  expect_true(unstable$admissible_before_stability)
  expect_false(unstable$width_stability_pass)
  expect_false(unstable$admissible)
})

test_that("adaptive MTI-ECM policy builder applies infeasible-rate limits", {
  script <- test_path(
    "..", "..", "scripts", "82_build_mti_ecm_adaptive_policy.R"
  )
  skip_if_not(file.exists(script), "policy builder is outside package build")

  rows <- list()
  idx <- 0L
  add_rows <- function(method_id, infeasible_at, width_base, screen) {
    for (rr in seq_len(20L)) {
      idx <<- idx + 1L
      is_infeasible <- rr %in% infeasible_at
      rows[[idx]] <<- data.frame(
        method_id = method_id,
        dgp_id = "skew_cell",
        n = 100L,
        guaranteed_content = 0.90,
        tolerance_confidence = 0.70,
        replication = rr,
        success = TRUE,
        infeasible = is_infeasible,
        width = width_base + rr / 1000,
        effective_posterior_confidence = screen,
        candidate_feasible_count = if (is_infeasible) 0L else 2L,
        stringsAsFactors = FALSE
      )
    }
  }
  add_rows("mti_ecm_adaptive_refine_screen_p996", 1L, 1, 0.996)
  add_rows("mti_ecm_adaptive_refine_screen_p998", integer(), 2, 0.998)

  input <- tempfile("adaptive-infeasible-input-", fileext = ".csv")
  output <- tempfile("adaptive-infeasible-output-", fileext = ".csv")
  diagnostics <- tempfile("adaptive-infeasible-diagnostics-", fileext = ".csv")
  write.csv(do.call(rbind, rows), input, row.names = FALSE)

  status <- system2(
    "Rscript",
    c(
      script,
      paste0("--results=", input),
      paste0("--output=", output),
      paste0("--diagnostics-output=", diagnostics),
      "--policy-id=infeasible_policy",
      "--selection=cell",
      "--method-pattern=^mti_ecm_adaptive_refine_screen_",
      "--bound-method=wilson",
      "--bound-confidence=0.50",
      "--margin=0",
      "--max-infeasible-rate=0",
      "--min-min-candidate-feasible-count=1"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  out <- read.csv(output)
  expect_equal(out$source_method_id, "mti_ecm_adaptive_refine_screen_p998")
  expect_true(out$admissible)

  diag <- read.csv(diagnostics)
  unstable <- diag[
    diag$calibration_scope == "all_distribution_cell" &
      diag$source_method_id == "mti_ecm_adaptive_refine_screen_p996",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(unstable), 1L)
  expect_true(unstable$admissible_before_stability)
  expect_false(unstable$infeasible_rate_pass)
  expect_false(unstable$min_candidate_feasible_count_pass)
  expect_false(unstable$admissible)
})

test_that("adaptive MTI-ECM policy builder supports median width ranking", {
  script <- test_path(
    "..", "..", "scripts", "82_build_mti_ecm_adaptive_policy.R"
  )
  skip_if_not(file.exists(script), "policy builder is outside package build")

  rows <- list()
  idx <- 0L
  add_rows <- function(method_id, widths, screen) {
    for (rr in seq_along(widths)) {
      idx <<- idx + 1L
      rows[[idx]] <<- data.frame(
        method_id = method_id,
        dgp_id = "regular_cell",
        n = 100L,
        guaranteed_content = 0.90,
        tolerance_confidence = 0.80,
        replication = rr,
        success = TRUE,
        infeasible = FALSE,
        width = widths[[rr]],
        effective_posterior_confidence = screen,
        candidate_feasible_count = 2L,
        stringsAsFactors = FALSE
      )
    }
  }
  add_rows("mti_ecm_adaptive_refine_screen_p996", c(rep(1, 19), 20), 0.996)
  add_rows("mti_ecm_adaptive_refine_screen_p998", rep(1.5, 20), 0.998)

  input <- tempfile("adaptive-objective-input-", fileext = ".csv")
  output <- tempfile("adaptive-objective-output-", fileext = ".csv")
  write.csv(do.call(rbind, rows), input, row.names = FALSE)

  status <- system2(
    "Rscript",
    c(
      script,
      paste0("--results=", input),
      paste0("--output=", output),
      "--policy-id=objective_policy",
      "--selection=cell",
      "--method-pattern=^mti_ecm_adaptive_refine_screen_",
      "--bound-method=wilson",
      "--bound-confidence=0.50",
      "--margin=0",
      "--width-objective=median-q975"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_status <- attr(status, "status")
  if (is.null(exit_status)) exit_status <- 0L
  expect_equal(exit_status, 0L, info = paste(status, collapse = "\n"))

  out <- read.csv(output)
  expect_equal(out$source_method_id, "mti_ecm_adaptive_refine_screen_p996")
  expect_equal(out$width_objective, "median-q975")
})
