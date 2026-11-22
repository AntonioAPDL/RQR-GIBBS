msx_law <- function(spec) {
  family <- spec$family
  if (family == "lognormal") {
    m <- exp(spec$meanlog+spec$sdlog^2/2)
    s <- m*sqrt(expm1(spec$sdlog^2))
    r <- function(n) rlnorm(n,spec$meanlog,spec$sdlog)
    p <- function(x,lower.tail=TRUE) plnorm(x,spec$meanlog,spec$sdlog,lower.tail=lower.tail)
    q <- function(u) qlnorm(u,spec$meanlog,spec$sdlog)
    label <- sprintf("Lognormal (log SD %.2f)",spec$sdlog)
  } else if (family == "weibull") {
    m <- spec$scale*gamma(1+1/spec$shape)
    s <- spec$scale*sqrt(gamma(1+2/spec$shape)-gamma(1+1/spec$shape)^2)
    r <- function(n) rweibull(n,spec$shape,spec$scale)
    p <- function(x,lower.tail=TRUE) pweibull(x,spec$shape,spec$scale,lower.tail=lower.tail)
    q <- function(u) qweibull(u,spec$shape,spec$scale)
    label <- sprintf("Weibull (shape %.2f)",spec$shape)
  } else if (family == "lomax") {
    m <- spec$scale/(spec$shape-1)
    s <- spec$scale*sqrt(spec$shape/((spec$shape-1)^2*(spec$shape-2)))
    r <- function(n) actuar::rpareto(n,shape=spec$shape,scale=spec$scale)
    p <- function(x,lower.tail=TRUE) actuar::ppareto(x,shape=spec$shape,scale=spec$scale,lower.tail=lower.tail)
    q <- function(u) actuar::qpareto(u,shape=spec$shape,scale=spec$scale)
    label <- sprintf("Lomax (shape %.2f)",spec$shape)
  } else if (family == "invgauss") {
    m <- spec$mean; s <- sqrt(spec$mean^3/spec$shape)
    r <- function(n) statmod::rinvgauss(n,mean=spec$mean,shape=spec$shape)
    p <- function(x,lower.tail=TRUE) statmod::pinvgauss(x,mean=spec$mean,shape=spec$shape,lower.tail=lower.tail)
    q <- function(u) statmod::qinvgauss(u,mean=spec$mean,shape=spec$shape)
    label <- sprintf("Inverse Gaussian (shape %.2f)",spec$shape)
  } else if (family == "gamma") {
    m <- spec$shape*spec$scale; s <- sqrt(spec$shape)*spec$scale
    r <- function(n) rgamma(n,shape=spec$shape,scale=spec$scale)
    p <- function(x,lower.tail=TRUE) pgamma(x,shape=spec$shape,scale=spec$scale,lower.tail=lower.tail)
    q <- function(u) qgamma(u,shape=spec$shape,scale=spec$scale)
    label <- sprintf("Gamma (shape %.2f)",spec$shape)
  } else if (family == "beta") {
    a <- spec$shape1; b <- spec$shape2
    m <- a/(a+b); s <- sqrt(a*b/((a+b)^2*(a+b+1)))
    r <- function(n) rbeta(n,a,b)
    p <- function(x,lower.tail=TRUE) pbeta(x,a,b,lower.tail=lower.tail)
    q <- function(u) qbeta(u,a,b)
    label <- sprintf("Beta (%.2f, %.0f)",a,b)
  } else stop("Unknown extension distribution: ",family)
  stopifnot(is.finite(m),is.finite(s),s>0)
  list(id=spec$id,spec=spec,label=label,mean=m,sd=s,raw_r=r,raw_p=p,raw_q=q,
    r=function(n) (r(n)-m)/s,
    p=function(z,lower.tail=TRUE) p(m+s*z,lower.tail=lower.tail),
    q=function(u) (q(u)-m)/s, support=c(-m/s,if(family=="beta") (1-m)/s else Inf))
}

msx_content <- function(law,lower,upper) {
  stopifnot(length(lower)==length(upper))
  a <- law$p(lower); b <- law$p(upper)
  value <- b-a
  right <- is.finite(a) & a>.5
  value[right] <- law$p(lower[right],lower.tail=FALSE)-law$p(upper[right],lower.tail=FALSE)
  value
}

# Bound the probability that at least one value collapses onto the lower support
# after the fixed population transformation. This is a numerical admission rule,
# not a calibrated statistical threshold or a bound on attainment error.
msx_admission <- function(cfg) {
  specs <- cfg$laws
  if (is.data.frame(specs)) specs <- lapply(seq_len(nrow(specs)),function(i)as.list(specs[i,]))
  rows <- lapply(specs,function(spec) {
    law <- msx_law(spec)
    u <- c(1e-6,1e-5,1e-4,.001,.005,.025,.05,.25,.5,.75,.95,.975,.995,.999,1-1e-6)
    raw <- law$raw_q(u); z <- (raw-law$mean)/law$sd
    raw_error <- max(abs(law$raw_p(raw)-u))
    coordinate_error <- max(abs(law$p(z)-u))
    boundary <- -law$mean/law$sd
    lo <- 0; hi <- law$mean
    for (i in seq_len(200L)) {
      mid <- (lo+hi)/2
      if ((mid-law$mean)/law$sd == boundary) lo <- mid else hi <- mid
    }
    mass <- law$raw_p(hi)
    probability <- -expm1(max(cfg$design$n)*log1p(-mass))
    raw_pass <- is.finite(raw_error) && raw_error <= cfg$numerical_admission$raw_cdf_quantile_tolerance
    boundary_pass <- probability <= 1/cfg$replications
    admitted <- raw_pass && boundary_pass
    data.frame(law_id=spec$id,label=law$label,conditional=isTRUE(spec$conditional),
      raw_mean=law$mean,raw_sd=law$sd,raw_cdf_error=raw_error,
      standardized_roundtrip_error=coordinate_error,boundary_raw_threshold=hi,
      boundary_probability_mass=mass,maximum_n_boundary_event_probability=probability,
      maximum_n=max(cfg$design$n),numerical_event_budget=1/cfg$replications,
      admitted=admitted,reason=if(!raw_pass)"raw_distribution_accuracy" else if(!boundary_pass)
        "boundary_collapse_risk_requires_higher_precision_fitting" else "numerical_checks_passed")
  })
  result <- do.call(rbind,rows)
  stopifnot(all(result$admitted[!result$conditional]))
  result
}
