profile_reference_env <- function() {
  e <- new.env(parent = globalenv())
  roots <- c(testthat::test_path("..",".."),
    testthat::test_path("..","..","00_pkg_src","rqrgibbs"))
  paths <- file.path(roots,"scripts","lib","pharma_ecm_numerical_audit.R")
  if(!any(file.exists(paths))) stop("Missing independent objective reference source.")
  sys.source(paths[file.exists(paths)][1L], e)
  e
}

test_that("profiled intercept estimates agree with independent cell minima", {
  e <- profile_reference_env()
  set.seed(20261021)
  examples <- list(c(0,0,1,1,2), c(-3,-1,.2,2,4), rep(2,5),
    rnorm(12), rexp(15))
  for (y in examples) for (q in c(.6,.9,.9775)) for (delta in c(-.1,0,.1)) {
    ref <- e$pea_reference(y,q,delta,precision=.01,
      direct_checks=.Machine$integer.max)
    fits <- lapply(c("R","cpp"), function(backend)
      rqr_mti_intercept_fit(y,q,delta,prior_precision=.01,backend=backend))
    for (fit in fits) {
      expect_true(fit$converged)
      expect_equal(fit$objective,ref$objective,tolerance=1e-8)
      expect_equal(sum(fit$fractional_weights),fit$effective_mass,tolerance=1e-10)
      expect_gte(fit$objective+1e-7,fit$objective_lower_bound)
      expect_false(fit$tolerance_claim_available)
    }
    expect_equal(fits[[1]]$objective,fits[[2]]$objective,tolerance=1e-9)
  }
})

test_that("fractional shortest support has coherent mass and conventional endpoints", {
  y <- c(0,1,2,10)
  for (allocation in c("left","right","balanced")) {
    z <- rqr_mti_fractional_window(y,.625,allocation)
    expect_equal(sum(z$membership_weights),2.5)
    expect_true(all(z$membership_weights>=0 & z$membership_weights<=1))
    expect_gte(z$delta_raw,z$delta_lower-1e-12)
    expect_lte(z$delta_raw,z$delta_upper+1e-12)
    expect_equal(z$closed_count,3)
  }
  integer <- rqr_mti_fractional_window(c(0,100,101,201),.5)
  expect_equal(c(integer$lower,integer$upper),c(100,101))
  expect_equal(integer$width,1)
  expect_equal(integer$retained_mean,100.5)
  tiny <- rqr_mti_fractional_window(y,1e-10)
  expect_equal(sum(tiny$membership_weights),4e-10)
  expect_true(is.finite(tiny$delta_raw))
  for (q in c(.01,.5,.625,.9999)) {
    z <- rqr_mti_fractional_window(c(1,1,1,2,2),q)
    expect_equal(sum(z$membership_weights),5*q,tolerance=1e-12)
  }
})

test_that("boundary tilts retain legitimate prior-centered exterior solutions", {
  e <- profile_reference_env()
  y <- c(0,1,2,10)
  delta <- rqr_mti_empirical_tilt_bounds(y,.625)$delta_lower
  fit <- rqr_mti_intercept_fit(y,.625,delta)
  ref <- e$pea_reference(y,.625,delta)
  expect_equal(fit$objective,ref$objective,tolerance=1e-8)
  extreme <- rqr_mti_intercept_fit(y,.625,delta-.05)
  expect_lt(extreme$lower,min(y)-100)
  expect_true(extreme$converged)
  collapsed <- rqr_mti_intercept_fit(c(-2,2),.5,prior_precision=1)
  expect_equal(collapsed$width,0)
  expect_equal(collapsed$objective,4)
})

test_that("coherent affine target changes preserve the fitted interval", {
  y <- c(-3,-1,.2,2,4)
  fit <- rqr_mti_intercept_fit(y,.9,.05,prior_precision=.01)
  a <- 23; b <- 2.5
  shifted <- rqr_mti_intercept_fit(a+b*y,.9,b*.05,
    learning_rate=1/b^2,prior_precision=.01/b^2,prior_mean=a)
  expect_equal((c(shifted$lower,shifted$upper)-a)/b,
    c(fit$lower,fit$upper),tolerance=1e-8)
  expect_equal(shifted$objective,fit$objective,tolerance=1e-8)
})

test_that("profile inputs and numerical failures are explicit", {
  expect_error(rqr_mti_intercept_fit(1:4,1),"between|probability|\\(0")
  expect_error(rqr_mti_intercept_fit(1:4,.9,prior_precision=0),"positive")
  expect_error(rqr_mti_intercept_fit(c(1,NA),.9),"finite|missing")
  expect_error(rqr_mti_intercept_fit(1:4,.9,control=list(max_iter=1)),"accuracy")
  fit <- rqr_mti_intercept_fit(1:4,.9,control=list(store_trace=TRUE))
  expect_gt(nrow(fit$trace),0)
  expect_true(all(c("midpoint","bracket_lower","gradient_upper") %in% names(fit$trace)))
})

test_that("profile selection keeps candidate diagnostics and transfers no calibration", {
  set.seed(217)
  y <- rexp(100)
  z <- rqr_mti_dp_profile(y,.9,.95,q_grid=c(.97,.9775,.98))
  expect_true(z$interval_returned)
  expect_true(all(z$candidates$empirical_tilt_admissible))
  expect_false(z$historical_policy_transferred)
  expect_false(z$frequentist_tolerance_theorem_available)
  expect_equal(z$selected$observed_count,sum(y>=z$selected$lower & y<=z$selected$upper))
  expect_true(all(z$candidates$relative_objective_gap[z$candidates$profile_converged]<1e-8))
  none <- rqr_mti_dp_profile(y,.9,1-1e-14,q_grid=.91)
  expect_false(none$interval_returned)
  expect_equal(none$status,"no_eligible_interval")
  expect_error(rqr_mti_dp_profile(y,.9,.95,q_grid=.8),"q_grid")
})

test_that("rounded observations and extreme fitted contents retain feasible weights", {
  set.seed(3917)
  for (n in c(50,100,500,1000)) {
    y <- round(rexp(n),2)
    for (q in c(.95,.985,1-1/(8*n))) {
      bounds <- rqr_mti_empirical_tilt_bounds(y,q)
      for (position in c(0,1/6,4/6,1)) {
        delta <- bounds$delta_lower+position*(bounds$delta_upper-bounds$delta_lower)
        fits <- lapply(c("R","cpp"),function(backend)
          rqr_mti_intercept_fit(y,q,delta,backend=backend))
        for (fit in fits) {
          expect_true(fit$converged)
          expect_equal(sum(fit$fractional_weights),fit$effective_mass,tolerance=1e-10)
          expect_true(all(fit$fractional_weights>=0 & fit$fractional_weights<=1))
        }
        expect_equal(fits[[1]]$objective,fits[[2]]$objective,tolerance=1e-8)
      }
    }
  }
})
