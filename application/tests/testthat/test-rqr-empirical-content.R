test_that("empirical content helper evaluates closed intervals", {
  y <- c(-2, -1, 0, 1, 2, 3)
  got <- rqr_interval_empirical_content(y, lower = -1, upper = 2)

  expect_s3_class(got, "data.frame")
  expect_identical(got$n, 6L)
  expect_equal(got$width, 3)
  expect_equal(got$empirical_content, 4 / 6)
  expect_equal(got$lower_omitted, 1 / 6)
  expect_equal(got$upper_omitted, 1 / 6)
})

test_that("empirical content helper handles missing values deliberately", {
  y <- c(0, 1, NA, 2, 3)

  expect_error(
    rqr_interval_empirical_content(y, lower = 0, upper = 2),
    "contains NA"
  )
  got <- rqr_interval_empirical_content(
    y, lower = 0, upper = 2, na_rm = TRUE
  )
  expect_identical(got$n, 4L)
  expect_equal(got$empirical_content, 3 / 4)
  expect_equal(got$upper_omitted, 1 / 4)
})

test_that("empirical content helper rejects invalid endpoints", {
  expect_error(
    rqr_interval_empirical_content(1:3, lower = Inf, upper = 3),
    "finite scalar"
  )
  expect_error(
    rqr_interval_empirical_content(1:3, lower = 3, upper = 2),
    "upper cannot be smaller"
  )
})
