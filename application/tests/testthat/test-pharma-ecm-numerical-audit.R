pea_env <- function() {
  e <- new.env(parent=globalenv())
  sys.source(testthat::test_path("..","..","scripts","lib","pharma_ecm_numerical_audit.R"),e)
  e
}

test_that("the standalone diagnostic runner parses completely", {
  path <- testthat::test_path("..","..","scripts","89_run_pharma_ecm_numerical_audit.R")
  expect_silent(invisible(parse(path)))
})

test_that("one-sided derivative intervals distinguish midpoint and stationarity", {
  e <- pea_env(); y <- c(0,2)
  s <- e$pea_stationarity(y,0,2,.5,0,precision=.1)
  expect_equal(s$distance,0)
  expect_equal(s$midpoint,.2)
  expect_equal(s$exact_zero_products,2L)
  for (j in 1:2) {
    z <- c(0,2); h <- 1e-7
    f <- function(z) e$pea_objective(y,z[1],z[2],.5,0,precision=.1)
    left <- right <- z; left[j] <- left[j]-h; right[j] <- right[j]+h
    lo <- (f(z)-f(left))/h; hi <- (f(right)-f(z))/h
    expect_equal(lo,c(s$lower_gradient_lo,s$upper_gradient_lo)[j],tolerance=1e-6)
    expect_equal(hi,c(s$lower_gradient_hi,s$upper_gradient_hi)[j],tolerance=1e-6)
  }
  saddle <- e$pea_stationarity(y,1,1,.5,.1,precision=.1)
  expect_equal(saddle$distance,0,tolerance=1e-12)
  ref <- e$pea_reference(y,.5,.1,precision=.1)
  expect_lt(ref$objective,e$pea_objective(y,1,1,.5,.1,precision=.1))
})

test_that("piecewise reference agrees with direct objectives and numerical searches", {
  e <- pea_env()
  set.seed(9912)
  examples <- list(c(0,0,1,1,2),c(-3,-1,.2,2,4),rep(2,5),rnorm(12),rexp(15))
  for (y in examples) for (q in c(.6,.9,.9775)) for (delta in c(-.1,0,.1)) {
    r <- e$pea_reference(y,q,delta,precision=.01)
    f <- function(z) e$pea_objective(y,z[1],z[2],q,delta,precision=.01)
    expect_equal(r$objective,f(c(r$lower,r$upper)),tolerance=1e-10)
    expect_lte(r$objective,f(range(y))+1e-8)
    # Nelder-Mead is a cross-check, never the authority for global optimality.
    for (start in list(range(y),range(y)+.3,c(mean(y)-1,mean(y)+1))) {
      o <- optim(start,f,method="Nelder-Mead",control=list(maxit=1500,reltol=1e-11))
      expect_lte(r$objective,o$value+1e-7*(1+abs(o$value)))
    }
  }
  # Singular PSD cell Hessian: edge enumeration must retain a minimum.
  r <- e$pea_reference(c(-2,2),.5,.3,precision=1)
  expect_true(is.finite(r$objective))
  expect_error(e$pea_reference(1:3,.9,0,precision=0),"Invalid")
})

test_that("piecewise reference beats a dense grid including exterior roots", {
  e <- pea_env(); y <- c(-1,-.2,.5,2)
  for (delta in c(-2,0,2)) {
    r <- e$pea_reference(y,.75,delta,precision=.2)
    g <- expand.grid(l=seq(-12,12,length.out=61),u=seq(-12,12,length.out=61))
    v <- vapply(seq_len(nrow(g)),function(i)
      e$pea_objective(y,g$l[i],g$u[i],.75,delta,precision=.2),numeric(1))
    expect_lte(r$objective,min(v)+1e-8)
  }
})

test_that("case selection uses only design, original flag, and split ID", {
  e <- pea_env()
  z <- expand.grid(response_id=c("a","b"),split_scheme=c("random","month_grouped"),
    mti_ecm_converged=c(FALSE,TRUE),split_id=1:4,stringsAsFactors=FALSE)
  z$method_id <- "mti_ecm_adaptive_cell"
  a <- e$pea_select_cases(z,2L)
  z$width <- rev(seq_len(nrow(z)))
  b <- e$pea_select_cases(z[nrow(z):1,],2L)
  expect_equal(a,b[,names(a)])
  expect_equal(nrow(a),16L)
  expect_true(all(a$split_id %in% 1:2))
})
