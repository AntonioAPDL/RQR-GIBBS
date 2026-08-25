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
  expect_true(nzchar(out$input_results_digest))
})
