test_that("common-scale prior support and unsafe requests are explicit", {
  expect_error(.rqr_cs_spec(list()), "log_rate_bounds")
  expect_error(.rqr_cs_spec(list(log_rate_bounds=c(-Inf,Inf))), "finite")
  expect_error(.rqr_cs_spec(list(prior="jeffreys",log_rate_bounds=c(-1,1))), "proper bounded")
  expect_error(.rqr_cs_spec(list(log_rate_bounds=c(-1,1),guessed=1)), "Unknown")
  b <- log(c(.25,4)); spec <- .rqr_cs_spec(list(log_rate_bounds=b))
  expect_identical(spec$mode_parameterization, "roots_and_log_inverse_loss_scale")
  for (L in c(-10,0,.1,2,100)) {
    eta <- .rqr_cs_scale_mode(L,10,b)
    reference <- optimize(function(x)exp(x)*L-10*x,b)$objective
    expect_lte(exp(eta)*L-10*eta, reference+1e-7)
  }
  L <- .rqr_cs_loss(c(-.1,.1),c(0,20),.95,.5)
  expect_equal(L,-16.991,tolerance=1e-12)
  expect_lt(4+L,0)
  expect_equal(.rqr_cs_scale_mode(L,2,b),b[2])
})

test_that("common-scale joint density retains the marginal power and tilt constant", {
  y <- c(-1,.2,2); q <- .8; d <- .2; roots <- c(-.7,1.5); eta <- .3
  L <- rqr_mean_tilt_loss(y,rep(roots[1],length(y)),rep(roots[2],length(y)),q,mean_tilt=d)
  expect_equal(.rqr_cs_objective(y,roots,q,d,eta,.25),
    exp(eta)*sum(L)+.125*sum(roots^2)-length(y)*eta)
  e <- (y-roots[1])*(y-roots[2]); cc <- rqr_constants(q,exp(eta))
  integrated <- vapply(e,function(z) integrate(function(v)
    dnorm(z,cc$xi*v,sqrt(cc$phi*cc$sigma*v))*dexp(v,rate=exp(eta)),0,Inf,rel.tol=1e-8)$value,0)
  direct <- q*(1-q)*exp(eta)*exp(-exp(eta)*e*(q-(e<0)))
  expect_equal(integrated,direct,tolerance=1e-7)
  expected <- sum(log(integrated))+exp(eta)*q*d*sum(sum(roots)-2*y)-.125*sum(roots^2)
  expect_equal(expected+ .rqr_cs_objective(y,roots,q,d,eta,.25),length(y)*log(q*(1-q)),tolerance=1e-7)
  without_y_constant <- sum(e*(q-(e<0)))-length(y)*q*d*sum(roots)
  expect_gt(abs(.rqr_cs_loss(y,roots,q,d)-without_y_constant),.1)
  # Reciprocal changes of density do not preserve a joint mode.
  omega <- exp(eta); sigma <- 1/omega
  expect_equal((1/omega)*omega,1)
  expect_equal((1/sigma)*sigma,1)
})

test_that("latent moments and scale CM agree with independent integration", {
  q <- .82; omega <- 1.7; z <- .31; cc <- rqr_constants(q,omega)
  kernel <- function(v) dnorm(z,cc$xi*v,sqrt(cc$phi*cc$sigma*v))*dexp(v,rate=omega)
  norm <- integrate(kernel,0,Inf)$value
  expect_equal(integrate(function(v)kernel(v)/v,0,Inf)$value/norm,
    .rqr_ecm_latent_inverse_mean(z,q)$inverse_mean,tolerance=1e-6)
  expect_equal(integrate(function(v)kernel(v)*v,0,Inf)$value/norm,
    .rqr_ecm_latent_mean(z,q,omega),tolerance=1e-6)
  expect_error(.rqr_ecm_latent_inverse_mean(0,q),"infinite")
  for(h in c(-1,0,1,20)) {
    eta <- .rqr_cs_scale_mode(h,15,log(c(.25,4)))
    ref <- optimize(function(x)exp(x)*h-15*x,log(c(.25,4)))$objective
    expect_lte(exp(eta)*h-15*eta,ref+1e-6)
  }
})

test_that("root conditional modes and increments work away from unit rate", {
  y <- c(-1,-1,.2,.7,2); roots <- c(-.3,1.3)
  for(w in c(.0625,1,16)) for(j in 1:2) {
    mode <- .rqr_cs_root_mode(y,roots[3-j],.91,-.12,w,.25)
    f <- function(x) {r<-roots;r[j]<-x;.rqr_cs_objective(y,r,.91,-.12,log(w),.25)}
    ref <- optimize(f,c(-100,100))
    expect_lte(f(mode),ref$objective+1e-6)
    expect_equal(.rqr_cs_root_increment(y,roots,j,mode,.91,-.12,w,.25),f(mode)-f(roots[j]),tolerance=1e-8)
  }
})

test_that("R and C++ common-scale ECM fit the declared target", {
  y <- c(-2,-1.1,-.3,.2,.8,1.7,3); X <- matrix(1,length(y),1)
  for(update in c("ecm","ecme")) {
    sc <- list(log_rate_bounds=log(c(.25,4)),update=update,multistart_rates=FALSE)
    control <- list(max_iter=50,multistart=FALSE)
    a <- rqr_ecm_fit(y,X,.85,mean_tilt=.1,ecm_control=control,scale_control=sc)
    b <- rqr_ecm_fit(y,X,.85,mean_tilt=.1,ecm_control=c(control,list(ecm_backend="cpp")),scale_control=sc)
    expect_true(a$numerically_accurate)
    expect_true(b$numerically_accurate)
    expect_equal(a$objective,b$objective,tolerance=1e-7)
    expect_equal(a$beta_root1,b$beta_root1,tolerance=1e-7)
    expect_equal(a$beta_root2,b$beta_root2,tolerance=1e-7)
    expect_equal(a$learning_rate,b$learning_rate,tolerance=1e-7)
    expect_equal(a$objective_trace$objective,b$objective_trace$objective,tolerance=1e-7)
    expect_true(all(diff(a$objective_trace$objective)<=1e-8))
    expect_equal(predict_interval(a,matrix(1,1,1))$width,abs(a$beta_root2-a$beta_root1))
    expect_false(a$response_likelihood)
    expect_false(a$formal_tolerance_action)
    ref <- rqr_ecm_fit(y,X,.85,learning_rate=a$learning_rate,mean_tilt=.1,
      beta_prior_obj=beta_prior("ridge",ridge=list(tau2=4)),ecm_control=control,
      init=list(beta1=a$beta_root1,beta2=a$beta_root2))
    expect_equal(a$objective,ref$objective-length(y)*log(a$learning_rate),tolerance=1e-7)
  }
})

test_that("small-case joint profile references bound common-scale solutions", {
  y<-c(-1,-.4,.3,1.2); q<-.8; d<-.1; bounds<-log(c(.25,4))
  fit<-rqr_ecm_fit(y,matrix(1,4,1),q,mean_tilt=d,
    ecm_control=list(ecm_backend="cpp",max_iter=80),scale_control=list(log_rate_bounds=bounds))
  profile<-function(r) {
    eta<-.rqr_cs_scale_mode(.rqr_cs_loss(y,r,q,d),length(y),bounds)
    .rqr_cs_objective(y,r,q,d,eta,.25)
  }
  grid<-expand.grid(a=seq(-2,1,length.out=25),b=seq(-1,3,length.out=25))
  values<-apply(grid,1,profile)
  reference<-optim(as.numeric(grid[which.min(values),]),profile,method="Nelder-Mead",control=list(reltol=1e-11))
  expect_lte(fit$objective,reference$value+1e-5)
})

test_that("common-scale rejection and DP candidate integration are explicit", {
  y<-c(-2,-1,-.3,.1,.6,1.4,3); X<-matrix(1,length(y),1)
  sc<-list(log_rate_bounds=log(c(.25,4)))
  expect_error(rqr_ecm_fit(y,cbind(1,seq_along(y)),.8,scale_control=sc),"intercept-only")
  expect_error(rqr_ecm_fit(y,X,.8,mean_tilt=seq_along(y),scale_control=sc),"scalar fixed")
  expect_error(rqr_ecm_fit(y,X,.8,learning_rate=10,scale_control=sc),"prior range")
  expect_error(rqr_mcmc_fit(y,X,.8,mean_tilt=.1,learning_rate_mode="learned_pure"),"fixed_rate")
  action<-rqr_mti_ecm_dp_profile_action(y,.7,.5,q_grid=c(.8,.92),
    tilt_grid_control=list(tilt_offsets_sd=0,include_zero_tilt=FALSE),
    dp_base_measure=rqr_dp_base_normal(0,4),expand_if_empty=FALSE,return_candidate_traces=TRUE,
    ecm_control=list(max_iter=50,multistart=FALSE,ecm_backend="cpp",scale_control=sc))
  expect_gt(nrow(action$candidates),0)
  expect_true(all(c("numerically_accurate","learning_rate","loss_scale") %in% names(action$candidates)))
  expect_length(action$candidates$learning_rate,nrow(action$candidates))
  expect_true(all(action$candidates$numerically_accurate))
  expect_true(all(is.finite(action$candidates$loss_scale)))
  prob<-rqr_dp_content_probability(action$posterior_distribution_fit,
    action$candidates$lower,action$candidates$upper,.7)
  expect_equal(action$candidates$posterior_content_probability,prob$posterior_content_probability)
  baseline<-rqr_ecm_fit(y,X,.8,ecm_control=list(max_iter=10))
  expect_null(baseline$common_scale)
  expect_equal(baseline$learning_rate,1)
  via_control<-rqr_ecm_fit(y,X,.8,ecm_control=list(max_iter=50,scale_control=sc))
  expect_false(is.null(via_control$common_scale))
  expect_error(rqr_ecm_fit(y,X,.8,ecm_control=list(scale_control=sc),
    scale_control=list(log_rate_bounds=log(c(.5,2)))),"Conflicting")
})

test_that("loss scale has squared-response units under a fixed external transformation", {
  y<-c(-2,-.3,.2,1.4);r<-c(-1,1);q<-.87;d<-.11;s<-3;m<-7;omega<-1.4
  original<-.rqr_cs_loss(y,r,q,d)
  transformed<-.rqr_cs_loss(m+s*y,m+s*r,q,s*d)
  expect_equal(transformed,s^2*original,tolerance=1e-12)
  expect_equal((omega/s^2)*transformed,omega*original,tolerance=1e-12)
  expect_equal(log(omega/s^2),log(omega)-2*log(s))
})

test_that("negative tilted losses remain computable under proper finite bounds", {
  y<-c(-.1,.1);X<-matrix(1,2,1);sc<-list(log_rate_bounds=log(c(.25,4)))
  fits<-lapply(c("R","cpp"),function(backend)rqr_ecm_fit(y,X,.95,mean_tilt=.5,
    ecm_control=list(ecm_backend=backend,max_iter=50),scale_control=sc))
  expect_equal(fits[[1]]$objective,fits[[2]]$objective,tolerance=1e-7)
  expect_true(all(vapply(fits,function(f)is.finite(f$objective) && f$numerically_accurate,FALSE)))
  expect_true(fits[[1]]$common_scale$at_upper_bound)
  expect_true(all(is.finite(fits[[1]]$objective_trace$objective)))
})
