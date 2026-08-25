test_that("delivery lower bounds and required successes are configurable", {
  cp <- rqr_delivery_lower_bound(
    successes = 19, replications = 20, confidence = 0.90,
    method = "clopper_pearson"
  )
  wilson <- rqr_delivery_lower_bound(
    successes = 19, replications = 20, confidence = 0.90,
    method = "wilson"
  )
  jeff <- rqr_delivery_lower_bound(
    successes = 19, replications = 20, confidence = 0.90,
    method = "jeffreys"
  )

  expect_true(all(c(cp, wilson, jeff) > 0))
  expect_true(all(c(cp, wilson, jeff) < 1))
  expect_false(isTRUE(all.equal(cp, wilson)))

  low_target <- rqr_delivery_min_successes(
    replications = 40, target = 0.70, confidence = 0.90,
    method = "clopper_pearson", margin = 0
  )
  high_target <- rqr_delivery_min_successes(
    replications = 40, target = 0.80, confidence = 0.90,
    method = "clopper_pearson", margin = 0
  )
  with_margin <- rqr_delivery_min_successes(
    replications = 40, target = 0.70, confidence = 0.90,
    method = "clopper_pearson", margin = 0.05
  )

  expect_lte(low_target, high_target)
  expect_lte(low_target, with_margin)
})

test_that("adaptive MTI-ECM menu respects fitted-content constraints", {
  y <- sort(c(-2.4, -1.9, -1.2, -0.7, -0.1, 0.2, 0.8, 1.1, 1.6, 2.8))
  menu <- rqr_mti_ecm_adaptive_profile_menu(
    y = y,
    content = 0.70,
    tolerance_confidence = 0.90,
    scan_target_content = 0.92,
    policy = "cell",
    policy_config = list(
      policy_id = "unit_cell_policy",
      posterior_confidence = 0.93,
      q_offsets = c(0, -0.01, -0.03, -0.05),
      q_min_buffer = 0.005,
      q_max = 0.98,
      tilt_grid_control = list(
        tilt_offsets_sd = c(-0.1, 0, 0.1),
        include_zero_tilt = TRUE,
        max_abs_tilt_sd = 2
      )
    )
  )

  expect_s3_class(menu, "rqr_mti_ecm_adaptive_profile_menu")
  expect_equal(menu$posterior_confidence, 0.93)
  expect_true(all(menu$q_grid > 0.70))
  expect_true(all(menu$q_grid < 1))
  expect_true(any(abs(menu$q_grid - 0.92) < 1e-12))
  expect_equal(menu$policy_id, "unit_cell_policy")
  expect_match(menu$cell_key, "^n0010_c0700_t0900$")
  expect_true(is.character(menu$provenance_digest))
})

test_that("adaptive MTI-ECM menu adds near-boundary q candidates", {
  y <- sort(seq(-1, 1, length.out = 50))
  menu <- rqr_mti_ecm_adaptive_profile_menu(
    y = y,
    content = 0.90,
    tolerance_confidence = 0.95,
    scan_target_content = 1,
    policy = "cell",
    policy_config = list(
      posterior_confidence = 0.985,
      q_offsets = c(0, -0.0025),
      q_min_buffer = 0.0001,
      q_max = 0.9995,
      boundary_anchor_cutoff = 0.995,
      boundary_q_values = c(0.995, 0.9975, 0.999, 0.9995)
    )
  )

  expect_true(all(menu$q_grid > 0.90))
  expect_true(all(menu$q_grid < 1))
  expect_true(any(menu$q_grid >= 0.999))
  expect_true(menu$diagnostics$tcsp_full_sample[[1L]])
})

test_that("explicit calibration rule overrides adaptive menu screen and grid", {
  y <- stats::rnorm(30)
  menu <- rqr_mti_ecm_adaptive_profile_menu(
    y = y,
    content = 0.80,
    tolerance_confidence = 0.95,
    scan_target_content = 0.94,
    policy = "cell",
    policy_config = list(posterior_confidence = 0.99),
    calibration_rule = list(
      menu_id = "frozen_unit_rule",
      screen = 0.975,
      q_grid = "0.86,0.90,0.93",
      dp_concentration = 1.5
    )
  )

  expect_equal(menu$posterior_confidence, 0.975)
  expect_equal(menu$q_grid, c(0.86, 0.90, 0.93))
  expect_equal(menu$dp_concentration, 1.5)
  expect_equal(menu$menu_id, "frozen_unit_rule")
})
