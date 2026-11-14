root <- normalizePath(if (file.exists("application/scripts/lib/mti_gibbs_practical.R")) "." else "../../..")
source(file.path(root, "application/scripts/lib/mti_gibbs_practical.R"))
source(file.path(root, "application/scripts/lib/mti_gibbs_width_diagnosis.R"))

testthat::test_that("normal-exponential augmentation integrates to the check-loss factor", {
  for (q in c(.9, .95, .99)) for (e in c(-2, 0, .1, 2)) for (sigma in c(.5, 2)) {
    xi <- (1 - 2 * q) / (q * (1 - q)); phi <- 2 / (q * (1 - q))
    integrand <- function(t) {
      v <- exp(t)
      exp(dnorm(e, xi * v, sqrt(phi * sigma * v), log = TRUE) + dexp(v, 1 / sigma, log = TRUE) + t)
    }
    value <- integrate(integrand, -55, 55, rel.tol = 1e-9, subdivisions = 1000)$value
    expected <- q * (1 - q) / sigma * exp(-e * (q - (e < 0)) / sigma)
    testthat::expect_equal(value, expected, tolerance = 1e-7)
    if (e != 0) {
      moment <- integrate(function(t) integrand(t) * exp(-t), -55, 55,
        rel.tol = 1e-9, subdivisions = 1000)$value / value
      testthat::expect_equal(moment, 1 / (q * (1 - q) * abs(e)), tolerance = 1e-6)
    }
  }
})

testthat::test_that("augmented root precision and tilt sign match the direct normal kernel", {
  y <- c(-1, -.2, .5, 1.5); v <- c(.2, .5, 1, 2); q <- .95
  sigma <- .7; phi <- 2 / (q * (1 - q)); xi <- (1 - 2 * q) / (q * (1 - q))
  for (other in c(-2, 1)) for (delta in c(-.3, 0, .3)) {
    d <- y - other; precision <- .01 + sum(d^2 / (phi * sigma * v))
    linear <- sum(d^2 * y / (phi * sigma * v) - xi * d / (phi * sigma)) + length(y) * q * delta / sigma
    direct <- function(a) -sum(dnorm((y - a) * d, xi * v, sqrt(phi * sigma * v), log = TRUE)) -
      q * delta * sum(a + other - 2 * y) / sigma + .01 * a^2 / 2
    x <- c(-4, -.5, 0, 3)
    testthat::expect_equal(vapply(x, direct, 0) - direct(0), precision * x^2 / 2 - linear * x,
      tolerance = 1e-9)
  }
})

testthat::test_that("the empirical limiting tilt has an unbounded flat rectangle at q=k/n", {
  y <- c(-1, -.2, .5, 1.5); q <- .75; delta <- unname(mwd_limits(y, q)[1])
  z <- expand.grid(a = c(-100, -10, -2), b = c(.6, .9, 1.4))
  loss <- mapply(function(a, b) mwd_loss(y, a, b, q, delta), z$a, z$b)
  testthat::expect_lt(diff(range(loss)), 1e-10)
  conditional <- mgp_conditional(y, .9, q, delta, precision = 1e-4)
  x <- c(-100, -10, -2)
  testthat::expect_equal(conditional$energy(x) - conditional$energy(-2),
    (x^2 - 4) / 20000, tolerance = 1e-9)
})

testthat::test_that("analytic scores and conditional energies agree with independent loss differences", {
  y <- c(-2, -.3, .1, .7, 2); q <- .99; delta <- -.2
  for (a in c(-30, -.8)) for (b in c(.5, 40)) {
    f <- function(x) mwd_loss(y, x[1], x[2], q, delta) * 1.7 + sum((x - .4)^2) / 6
    x <- c(a, b); h <- 1e-5
    numeric <- vapply(1:2, function(j) {
      e <- replace(c(0, 0), j, h); (f(x + e) - f(x - e)) / (2 * h)
    }, 0)
    testthat::expect_equal(as.numeric(mwd_gradient(y, a, b, q, delta, 1.7, 3, .4)), numeric,
      tolerance = 1e-6)
    cond <- mgp_conditional(y, b, q, delta, 1.7, .0001)
    testthat::expect_equal(cond$energy(a) - cond$energy(0),
      mgp_objective(y, a, b, q, delta, 1.7, .0001) - mgp_objective(y, 0, b, q, delta, 1.7, .0001),
      tolerance = 1e-9)
    testthat::expect_equal(mwd_loss(y + 17, a + 17, b + 17, q, delta), mwd_loss(y, a, b, q, delta),
      tolerance = 1e-10)
  }
})

testthat::test_that("diagnostic settings isolate the contrasts and retain wide initial values", {
  cfg <- jsonlite::read_json(file.path(root, "application/config/mti_gibbs_width_diagnosis_20261113.json"),
    simplifyVector = TRUE)
  task <- data.frame(case_id = "fixture", dataset_id = 1L, q = .9, delta = -.2)
  y <- c(-1, -.2, .5, 1.5); z <- mwd_variants(task, y, cfg)
  testthat::expect_equal(nrow(z), 10)
  p <- z[z$contrast == "prior", ]
  testthat::expect_equal(p$variance, cfg$prior_variances)
  testthat::expect_true(all(p$q == task$q & p$delta == task$delta & p$omega == 1 & p$center == 0))
  initial <- mwd_initial(y, -1, 1, 10000, 0)
  testthat::expect_equal(initial$roots[4, ], c(-300, 300))
  testthat::expect_false(cfg$promote_results)
  testthat::expect_false(cfg$new_tolerance_guarantee)
})
