test_that("effective-mass refinement fills missing cells without removing old targets", {
  x <- rqr_mti_refine_content_grid(c(.93,.995),50,.9)
  expect_identical(x$original_q_grid,c(.93,.995))
  expect_equal(x$added_q,c(.950002,.970002),tolerance=1e-14)
  expect_true(all(x$original_q_grid %in% x$q_grid))
  expect_equal(x$cells$radius_rank,47:50)
  expect_equal(x$cells$original_targets,c(1,0,0,1))
  expect_false(x$continuous_optimum_claim)
  expect_false(x$tolerance_claim_available)
  expect_equal(rqr_mti_refine_content_grid(c(.97,.995),100,.95)$added_q,
    c(.975001,.985001),tolerance=1e-14)
})

test_that("prior-to-rate adjustment and integer radius ranks agree with the solver", {
  x <- rqr_mti_refine_content_grid(c(.8,.99),10,.7,
    learning_rate=2,prior_precision=.4)
  expect_equal(x$added_q,.87)
  expect_equal(10*x$added_q-.2,8.5)
  # Exact integer masses use the preceding radius cell, (j,j+1].
  y <- rqr_mti_refine_content_grid(c(.5,.75),4,.1,
    learning_rate=1,prior_precision=1)
  expect_equal(y$cells$radius_rank,1:2)
  expect_length(y$added_q,0)
})

test_that("refinement is idempotent and handles collapsed or singleton grids", {
  x <- rqr_mti_refine_content_grid(c(.995,.93,.93),50,.9)
  y <- rqr_mti_refine_content_grid(x$q_grid,50,.9)
  expect_identical(x$q_grid,y$q_grid)
  expect_length(y$added_q,0)
  expect_length(rqr_mti_refine_content_grid(.995,50,.9)$added_q,0)
  z <- rqr_mti_refine_content_grid(c(.5,.9),4,.1,prior_precision=5)
  expect_equal(z$collapsed_original_targets,2)
  expect_identical(z$q_grid,c(.5,.9))
  expect_equal(nrow(z$cells),0L)
  expect_error(rqr_mti_refine_content_grid(c(.93,.995),50,.9,max_added_points=1),"budget|exceeds")
  expect_silent(rqr_mti_refine_content_grid(.995,50,.9,max_added_points=0))
  expect_error(rqr_mti_refine_content_grid(c(.1,.99),1e9,.01,max_added_points=2),"exceeds")
})

test_that("invalid content-grid controls are rejected", {
  expect_error(rqr_mti_refine_content_grid(c(.9,.99),50,.9),"strictly")
  expect_error(rqr_mti_refine_content_grid(c(.95,1),50,.9),"strictly")
  expect_error(rqr_mti_refine_content_grid(numeric(),50,.9),"q_grid")
  expect_error(rqr_mti_refine_content_grid(NA_real_,50,.9),"q_grid")
  expect_error(rqr_mti_refine_content_grid(.99,1,.9),"sample_size")
  expect_error(rqr_mti_refine_content_grid(.99,50,1),"content")
  expect_error(rqr_mti_refine_content_grid(.99,50,.9,learning_rate=0),"positive")
  expect_error(rqr_mti_refine_content_grid(.99,50,.9,prior_precision=-1),"positive")
  expect_error(rqr_mti_refine_content_grid(.99,50,.9,
    learning_rate=1e-300,prior_precision=1e300),"ratio must be finite")
})

test_that("refined grids preserve all represented positive radius ranks", {
  set.seed(321022)
  for(i in seq_len(50)) {
    n <- sample(10:1000,1)
    content <- runif(1,.1,.8)
    q <- sort(runif(4,content+.01,.9999))
    lambda <- exp(runif(1,-8,1))
    omega <- exp(runif(1,-2,2))
    x <- rqr_mti_refine_content_grid(q,n,content,omega,lambda)
    expect_true(all(q %in% x$q_grid))
    expect_identical(range(q),range(x$q_grid))
    mass <- n*x$q_grid-lambda/omega
    expect_setequal(ceiling(mass[mass>0]),x$cells$radius_rank)
    expect_length(rqr_mti_refine_content_grid(x$q_grid,n,content,omega,lambda)$added_q,0)
  }
})
