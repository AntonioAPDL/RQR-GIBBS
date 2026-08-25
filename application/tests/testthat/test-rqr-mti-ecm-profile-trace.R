test_that("MTI-ECM profile action can return candidate traces", {
  set.seed(17)
  y <- rnorm(24)
  action <- rqr_mti_ecm_dp_profile_action(
    y = y,
    content = 0.80,
    posterior_confidence = 0.80,
    q_grid = c(0.86, 0.90),
    tilt_grid_control = list(
      tilt_offsets_sd = c(0),
      include_zero_tilt = FALSE,
      max_abs_tilt_sd = 2
    ),
    ecm_control = list(
      max_iter = 8,
      stable_iterations = 1,
      tol_stationarity = 1e-3,
      ecm_backend = "cpp",
      multistart = FALSE
    ),
    return_candidate_traces = TRUE
  )
  expect_s3_class(action, "rqr_mti_ecm_dp_profile_action")
  expect_s3_class(action, "mti_ecm_dp_profile_action")
  expect_true(is.data.frame(action$candidates))
  expect_true(is.data.frame(action$candidate_traces))
  expect_gt(nrow(action$candidate_traces), 0)
  expect_true(all(c(
    "candidate_index",
    "iteration",
    "objective",
    "stationarity",
    "root1",
    "root2",
    "width",
    "candidate_selected",
    "posterior_constraint_satisfied"
  ) %in% names(action$candidate_traces)))
  expect_true(all(action$candidate_traces$candidate_index %in%
                    action$candidates$candidate_index))
  expect_true(any(action$candidate_traces$iteration == 0L))
  expect_true(all(is.finite(action$candidate_traces$width)))
})
