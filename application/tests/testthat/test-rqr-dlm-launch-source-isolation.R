load_main_simulation_helpers <- function() {
  environment <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path(
      "..", "..", "scripts", "lib",
      "rqr_dlm_main_simulation.R"
    ),
    envir = environment
  )
  environment
}

test_that("detached launch-source worktrees require explicit opt-in", {
  environment <- load_main_simulation_helpers()

  expect_true(environment$rqr_main_source_branch_allowed("main"))
  expect_identical(
    environment$rqr_main_source_branch_contract("main"),
    "clean_main"
  )

  expect_false(environment$rqr_main_source_branch_allowed("HEAD", FALSE))
  expect_identical(
    environment$rqr_main_source_branch_contract("HEAD", FALSE),
    "invalid"
  )

  expect_true(environment$rqr_main_source_branch_allowed("HEAD", TRUE))
  expect_identical(
    environment$rqr_main_source_branch_contract("HEAD", TRUE),
    "clean_detached_exact_launch_source"
  )

  old <- Sys.getenv("RQR_ALLOW_DETACHED_LAUNCH_SOURCE", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("RQR_ALLOW_DETACHED_LAUNCH_SOURCE")
    } else {
      Sys.setenv(RQR_ALLOW_DETACHED_LAUNCH_SOURCE = old)
    }
  }, add = TRUE)
  Sys.setenv(RQR_ALLOW_DETACHED_LAUNCH_SOURCE = "TRUE")
  expect_true(environment$rqr_main_allow_detached_launch_source())
  expect_true(environment$rqr_main_source_branch_allowed("HEAD"))
})
