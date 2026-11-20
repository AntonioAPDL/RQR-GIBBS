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
    expect_equal(summary(a)$common_scale,a$common_scale)
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
  expect_equal(action$common_scale_prior$log_rate_bounds,sc$log_rate_bounds)
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

test_that("Gamma support and parameters preserve propriety restrictions", {
  sc <- list(prior="gamma_rate",shape=16,rate=16,log_rate_bounds=log(c(.25,4)))
  expect_equal(.rqr_cs_spec(sc)$shape,16)
  for(key in c("shape","rate"))for(value in c(0,-1,Inf,NA)) {
    bad<-sc;bad[[key]]<-value
    expect_error(.rqr_cs_spec(bad),"positive")
  }
  expect_error(.rqr_cs_spec(list(log_rate_bounds=c(-1,1),shape=4)),"only")
  expect_equal(.rqr_cs_spec(modifyList(sc,list(log_rate_bounds=c(-Inf,log(4)))))$log_rate_bounds[1],-Inf)
  y<-c(-1,.2,1);X<-matrix(1,3,1)
  expect_error(rqr_ecm_fit(y,X,.9,mean_tilt=.1,
    scale_control=modifyList(sc,list(log_rate_bounds=c(-Inf,Inf)))),"zero tilt")
  expect_error(.rqr_cs_scale_mode(-1,4,c(-Inf,Inf)),"Nonpositive")
  expect_error(rqr_ecm_common_scale_run_cpp(y,.9,.1,.25,c(-1,1),0,c(-Inf,Inf),
    10L,10L,1e-8,"hard",1e-6,1e-6,1e-8,10L,"ecm",TRUE,"invalid",16,16),"Invalid")
})

test_that("Gamma conditionals and reciprocal Jacobians match direct integration", {
  alpha<-4;beta<-4;n<-3;loss<-2.1
  integral<-integrate(function(w)dgamma(w,alpha,rate=beta)*w^n*exp(-w*loss),0,Inf)$value
  expected<-exp(alpha*log(beta)-lgamma(alpha)+lgamma(alpha+n)-(alpha+n)*log(beta+loss))
  expect_equal(integral,expected,tolerance=1e-8)
  for(w in c(.25,.7,1,2,4)) {
    sigma<-1/w
    ig<-exp(alpha*log(beta)-lgamma(alpha)-(alpha+1)*log(sigma)-beta/sigma)
    expect_equal(dgamma(w,alpha,rate=beta),ig/w^2,tolerance=1e-12)
    expect_equal(log(dgamma(w,alpha,rate=beta)*w),alpha*log(w)-beta*w+
      alpha*log(beta)-lgamma(alpha),tolerance=1e-12)
  }
  for(coefficient in c(-3,0,.1,12))for(alpha in c(4,16,64))for(power in c(n+alpha,1.5*n+alpha)) {
    bounds<-log(c(.25,4));eta<-.rqr_cs_scale_mode(coefficient,power,bounds)
    f<-function(z)coefficient*exp(z)-power*z
    expect_lte(f(eta),optimize(f,bounds)$objective+1e-7)
  }
  for(B in c(-2,0,2)) {
    value<-integrate(function(w)w^(alpha+n-1)*exp(-B*w),0,4)$value
    expect_true(is.finite(value) && value>0)
  }
})

test_that("Gamma R and native reference fixtures agree across supports and updates", {
  index<-0L
  for(n in c(2L,3L,6L,20L))for(delta in c(0,.3))for(alpha in c(4,16,64))for(update in c("ecm","ecme")) {
    y<-seq(-1.2,1.8,length.out=n);X<-matrix(1,n,1)
    sc<-list(prior="gamma_rate",shape=alpha,rate=alpha,log_rate_bounds=log(c(.25,4)),
      update=update,multistart_rates=FALSE)
    control<-list(max_iter=80,multistart=FALSE)
    a<-rqr_ecm_fit(y,X,.91,mean_tilt=delta,ecm_control=control,scale_control=sc)
    b<-rqr_ecm_fit(y,X,.91,mean_tilt=delta,ecm_control=c(control,list(ecm_backend="cpp")),scale_control=sc)
    expect_true(a$numerically_accurate && b$numerically_accurate)
    expect_equal(c(a$beta_root1,a$beta_root2,a$learning_rate,a$objective),
      c(b$beta_root1,b$beta_root2,b$learning_rate,b$objective),tolerance=1e-7)
    expect_equal(a$objective,.rqr_cs_objective(y,c(a$beta_root1,a$beta_root2),.91,delta,
      log(a$learning_rate),.25,a$common_scale),tolerance=1e-8)
    expect_true(all(diff(b$objective_trace$objective)<=1e-8))
    index<-index+1L
  }
  expect_equal(index,48L)
  for(bounds in list(c(-Inf,log(4)),c(-Inf,Inf))) {
    y<-c(-1,-.2,.7,2);X<-matrix(1,4,1)
    sc<-list(prior="gamma_rate",shape=16,rate=16,log_rate_bounds=bounds)
    a<-rqr_ecm_fit(y,X,.85,scale_control=sc)
    b<-rqr_ecm_fit(y,X,.85,scale_control=sc,ecm_control=list(ecm_backend="cpp"))
    expect_true(a$numerically_accurate && b$numerically_accurate)
    expect_equal(c(a$beta_root1,a$beta_root2,a$objective),c(b$beta_root1,b$beta_root2,b$objective),tolerance=1e-7)
    expect_true(all(is.finite(a$objective_trace$log_learning_rate)))
  }
})

test_that("signed-loss ray diagnosis is a sufficient obstruction and not a proof of safety", {
  y<-c(-.1,.1);d<-.rqr_cs_tilt_domain(y,.95,.5)
  expect_true(d$unbounded_loss_ray)
  expect_lt(d$positive_ray_slope,0)
  expect_false(d$unrestricted_gamma_proved)
  expect_equal(d$retained_tilt_upper,-d$retained_tilt_lower)
  expect_true(.rqr_cs_tilt_domain(y,.95,0)$unrestricted_gamma_proved)
  lo<-.rqr_cs_loss(y,c(min(y),100),.95,.5)
  hi<-.rqr_cs_loss(y,c(min(y),1000),.95,.5)
  expect_equal((hi-lo)/900,d$positive_ray_slope,tolerance=1e-12)
})

test_that("Gamma objective transforms with fixed externally specified response units", {
  y<-c(-1,.2,2);r<-c(-.7,1.1);q<-.9;d<-.2;w<-1.3;s<-3
  sc<-list(shape=16,rate=16)
  original<-.rqr_cs_objective(y,r,q,d,log(w),.25,sc)
  transformed<-.rqr_cs_objective(s*y,s*r,q,s*d,log(w/s^2),.25/s^2,
    list(shape=16,rate=16*s^2))
  expect_equal(transformed-original,2*(length(y)+16)*log(s),tolerance=1e-10)
  strong<-rqr_ecm_fit(y,matrix(1,3,1),q,scale_control=list(prior="gamma_rate",shape=1e6,
    rate=1e6,log_rate_bounds=log(c(.25,4))),ecm_control=list(ecm_backend="cpp"))
  expect_equal(strong$learning_rate,1,tolerance=1e-5)
})

test_that("Gamma prior reaches actual DP profile candidates and their scale checks", {
  y<-c(-2,-1,-.3,.1,.6,1.4,3)
  sc<-list(prior="gamma_rate",shape=16,rate=16,log_rate_bounds=c(-Inf,log(4)))
  action<-rqr_mti_ecm_dp_profile_action(y,.7,.5,q_grid=c(.8,.92),
    tilt_grid_control=list(tilt_offsets_sd=0,include_zero_tilt=FALSE),
    dp_base_measure=rqr_dp_base_normal(0,4),expand_if_empty=FALSE,return_candidate_traces=TRUE,
    ecm_control=list(max_iter=80,multistart=FALSE,ecm_backend="cpp",scale_control=sc))
  expect_equal(action$common_scale_prior$prior,"gamma_rate")
  expect_equal(action$common_scale_prior$shape,16)
  expect_gt(nrow(action$candidates),0)
  expect_length(action$candidates$learning_rate,nrow(action$candidates))
  expect_true(all(action$candidates$numerically_accurate))
  expect_true(any(abs(action$candidates$learning_rate-1)>1e-5))
  expect_true(all(is.finite(action$candidate_traces$objective)))
})

test_that("small Gamma joint modes agree with independent root-profile optimization", {
  y<-c(-1,-.4,.3,1.2);q<-.8;delta<-.1;bounds<-log(c(.25,4))
  spec<-list(prior="gamma_rate",shape=4,rate=4,log_rate_bounds=bounds)
  fit<-rqr_ecm_fit(y,matrix(1,4,1),q,mean_tilt=delta,scale_control=spec,
    ecm_control=list(ecm_backend="cpp",max_iter=100))
  profile<-function(r) {
    loss<-.rqr_cs_loss(y,r,q,delta)
    eta<-.rqr_cs_scale_mode(loss+4,length(y)+4,bounds)
    .rqr_cs_objective(y,r,q,delta,eta,.25,spec)
  }
  grid<-expand.grid(a=seq(-2,1,length.out=25),b=seq(-1,3,length.out=25))
  values<-apply(grid,1,profile)
  reference<-optim(as.numeric(grid[which.min(values),]),profile,method="Nelder-Mead",control=list(reltol=1e-11))
  expect_lte(fit$objective,reference$value+1e-5)
})
