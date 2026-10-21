pea_objective <- function(y, lower, upper, q, delta, omega = 1, precision = 1e-4) {
  e <- (y-lower)*(y-upper)
  omega * sum(e*(q-(e < 0)) - q*delta*(lower+upper-2*y)) +
    precision*(lower^2+upper^2)/2
}

pea_stationarity <- function(y, lower, upper, q, delta, omega = 1, precision = 1e-4) {
  if (lower > upper) stop("Ordered endpoints are required.")
  e <- (y-lower)*(y-upper)
  fixed <- e != 0
  psi <- q-(e[fixed] < 0)
  base <- c(-omega*sum((y[fixed]-upper)*psi),
            -omega*sum((y[fixed]-lower)*psi)) - omega*q*length(y)*delta +
    precision*c(lower,upper)
  coefficients <- rbind(-omega*(y[!fixed]-upper), -omega*(y[!fixed]-lower))
  lo <- base + rowSums(pmin(coefficients*(q-1), coefficients*q))
  hi <- base + rowSums(pmax(coefficients*(q-1), coefficients*q))
  distance <- pmax(lo, -hi, 0)
  list(distance = max(distance), midpoint = max(abs((lo+hi)/2)),
       lower_gradient_lo = lo[1], lower_gradient_hi = hi[1],
       upper_gradient_lo = lo[2], upper_gradient_hi = hi[2],
       exact_zero_products = sum(!fixed), minimum_abs_product = min(abs(e)))
}

pea_saved_diagnostic <- function(y, r, omega, precision) {
  lower <- r$lower; upper <- r$upper; q <- r$mti_selected_q; delta <- r$mti_selected_tilt
  raw <- pea_stationarity(y, lower, upper, q, delta, omega, precision)
  snapped <- c(y[which.min(abs(y-lower))], y[which.min(abs(y-upper))])
  snap <- pea_stationarity(y, snapped[1], snapped[2], q, delta, omega, precision)
  objective <- pea_objective(y,lower,upper,q,delta,omega,precision)
  data.frame(objective = objective, midpoint_stationarity = raw$midpoint,
    exact_stationarity_distance = raw$distance, zero_products = raw$exact_zero_products,
    minimum_abs_product = raw$minimum_abs_product,
    nearest_kink_displacement = max(abs(snapped-c(lower,upper))),
    nearest_kink_displacement_sd = max(abs(snapped-c(lower,upper)))/sd(y),
    nearest_kink_objective_change = pea_objective(y,snapped[1],snapped[2],q,delta,omega,precision)-objective,
    nearest_kink_stationarity_distance = snap$distance,
    nearest_kink_midpoint = snap$midpoint)
}

pea_reference <- function(y, q, delta, omega = 1, precision = 1e-4,
                          incumbent = range(y), direct_checks = 32L) {
  if (!length(y) || any(!is.finite(y)) || !is.finite(q) || q <= 0 || q >= 1 ||
      !is.finite(delta) || !is.finite(omega) || omega <= 0 ||
      !is.finite(precision) || precision <= 0) stop("Invalid fixed-target reference inputs.")
  n <- length(y)
  f0 <- pea_objective(y,incumbent[1],incumbent[2],q,delta,omega,precision)
  a <- omega*q*n*delta
  center <- a/precision
  radius2 <- 2*(f0-2*omega*q*delta*sum(y)+a*a/precision)/precision
  radius <- sqrt(max(radius2,0))
  padding <- 128*.Machine$double.eps*(1+abs(center)+radius)
  bound <- center+c(-radius-padding, radius+padding)
  knots <- sort(unique(c(bound, y[y>bound[1] & y<bound[2]])))
  left <- head(knots,-1L); right <- tail(knots,-1L)
  cells <- expand.grid(i=seq_along(left),j=seq_along(left))
  cells <- cells[cells$i<=cells$j,]
  # Each cell uses the same inside set throughout its interior. At a knot,
  # either neighboring polynomial gives the identical objective value.
  ys <- sort(y)
  count <- findInterval(knots,ys)
  s1 <- c(0,cumsum(ys)); s2 <- c(0,cumsum(ys^2))
  lower_count <- count[cells$i]
  upper_count <- count[cells$j]
  k <- upper_count-lower_count
  inside1 <- s1[upper_count+1L]-s1[lower_count+1L]
  inside2 <- s2[upper_count+1L]-s2[lower_count+1L]
  A <- omega*(q*sum(y*y)-inside2+2*q*delta*sum(y))
  B <- omega*(q*sum(y)-inside1+q*n*delta)
  T <- omega*(q*n-k)
  llo <- left[cells$i]; lhi <- right[cells$i]
  ulo <- left[cells$j]; uhi <- right[cells$j]
  clamp <- function(x,lo,hi) pmax(lo,pmin(hi,x))
  candidate <- function(l,u) data.frame(lower=l,upper=u,
    polynomial=A-B*(l+u)+T*l*u+precision*(l*l+u*u)/2)
  candidates <- rbind(candidate(llo,clamp((B-T*llo)/precision,ulo,uhi)),
    candidate(lhi,clamp((B-T*lhi)/precision,ulo,uhi)),
    candidate(clamp((B-T*ulo)/precision,llo,lhi),ulo),
    candidate(clamp((B-T*uhi)/precision,llo,lhi),uhi))
  diagonal <- cells$i==cells$j
  t <- clamp(B/(precision+T),llo,lhi)
  diag_candidates <- candidate(t,t)
  candidates <- rbind(candidates,diag_candidates[diagonal,])
  candidates <- candidates[is.finite(candidates$polynomial) & candidates$lower<=candidates$upper,]
  rank <- order(candidates$polynomial,candidates$lower,candidates$upper)
  best_poly <- candidates$polynomial[rank[1]]
  # Include the entire numerical tie set, not only a fixed top-K truncation.
  eligible <- rank[candidates$polynomial[rank] <= best_poly+
    1e-9*(1+abs(best_poly)) | seq_along(rank)<=direct_checks]
  z <- candidates[eligible,]
  z$objective <- vapply(seq_len(nrow(z)),function(i)
    pea_objective(y,z$lower[i],z$upper[i],q,delta,omega,precision),numeric(1))
  best <- order(z$objective,z$lower,z$upper)[1L]
  ans <- z[best,]
  if (abs(ans$objective-ans$polynomial)>1e-7*(1+abs(ans$objective)) ||
      ans$objective>f0+1e-8*(1+abs(f0))) stop("Independent reference failed its direct-loss check.")
  ties <- z[abs(z$objective-ans$objective)<=1e-10*(1+abs(ans$objective)),]
  data.frame(lower=ans$lower,upper=ans$upper,width=ans$upper-ans$lower,
    objective=ans$objective,cells=nrow(cells),candidate_points=nrow(candidates),
    direct_checked=nrow(z),bound_lower=bound[1],bound_upper=bound[2],
    numerical_tie_width_min=min(ties$upper-ties$lower),
    numerical_tie_width_max=max(ties$upper-ties$lower))
}

pea_select_cases <- function(results, per_stratum=2L) {
  z <- results[results$method_id=="mti_ecm_adaptive_cell",]
  if (anyNA(z$mti_ecm_converged)) stop("Missing selected-fit flags require separate diagnosis.")
  key <- interaction(z$response_id,z$split_scheme,z$mti_ecm_converged,drop=TRUE)
  out <- do.call(rbind,lapply(split(z,key),function(s) head(s[order(s$split_id),],per_stratum)))
  out <- out[order(out$response_id,out$split_scheme,out$mti_ecm_converged,out$split_id),]
  out$case_id <- sprintf("case_%03d",seq_len(nrow(out)))
  attr(out,"out.attrs") <- NULL
  rownames(out) <- NULL
  out
}

pea_replay_case <- function(r,inputs,config,profile,policy) {
  a <- inputs$config$analysis
  membership <- inputs$membership
  s <- membership[membership$split_scheme==r$split_scheme & membership$split_id==r$split_id,]
  yall <- inputs$observed[[r$response_id]]
  y <- yall[s$analysis_row_id[s$training]]
  heldout <- yall[s$analysis_row_id[!s$training]]
  cfg <- profile$config; base <- profile$direct_dp$base
  omega <- as.numeric(cfg$learning_rate %||% 1)
  precision <- 1/as.numeric(cfg$beta_ridge_tau2 %||% 1e4)
  menu <- rqrgibbs::rqr_mti_ecm_adaptive_profile_menu(
    y=y,content=a$content,tolerance_confidence=a$tolerance_confidence,
    scan_target_content=inputs$config$scan_reference$expected_retained_fraction,
    policy=cfg$adaptive_policy %||% "cell",policy_config=cfg,calibration_rule=policy,na_rm=FALSE)
  controls <- cfg$ecm_control
  controls$seed <- as.integer(r$fit_seed)
  controls$store_iteration_trace <- TRUE
  prior <- rqrgibbs::beta_prior("ridge",ridge=list(tau2=1/precision))
  action <- rqrgibbs::rqr_mti_ecm_dp_profile_action(
    y=y,content=a$content,posterior_confidence=menu$posterior_confidence,
    dp_concentration=menu$dp_concentration,
    dp_base_measure=rqrgibbs::rqr_dp_base_normal(mean=base$mean,sd=base$sd),
    strict_bayes=isTRUE(cfg$strict_bayes),
    scan_target_content=inputs$config$scan_reference$expected_retained_fraction,
    q_grid=menu$q_grid,tilt_grid_control=menu$tilt_grid_control,learning_rate=omega,
    beta_prior_obj=prior,ecm_control=controls,expand_if_empty=isTRUE(cfg$expand_if_empty),
    return_candidate_traces=TRUE,na_rm=FALSE)
  selected <- action$selected
  tolerance <- config$comparisons$reproduction_absolute_tolerance
  if (nrow(selected)!=1L || max(abs(c(selected$lower-r$lower,selected$upper-r$upper,
      selected$target_content-r$mti_selected_q,selected$mean_tilt-r$mti_selected_tilt,
      selected$posterior_content_probability-r$mti_posterior_content_probability)))>tolerance ||
      selected$ecm_converged!=r$mti_ecm_converged ||
      selected$ecm_iterations!=r$mti_ecm_iterations ||
      action$candidates_evaluated!=r$mti_candidates_evaluated) {
    stop("Frozen pharmaceutical profile was not reproduced: ",r$case_id)
  }
  candidates <- action$candidates
  references <- do.call(rbind,lapply(seq_len(nrow(candidates)),function(i) {
    z <- candidates[i,]
    if (!isTRUE(z$finite_interval)) stop("Candidate failure requires explicit separate diagnosis.")
    ref <- pea_reference(y,z$target_content,z$mean_tilt,omega,precision,
      incumbent=c(z$lower,z$upper),direct_checks=config$reference$direct_check_candidates)
    ref$candidate_index <- z$candidate_index
    ref$posterior_content_probability <- pea_probability(y,ref$lower,ref$upper,
      a$content,menu$dp_concentration,base)
    ref$passes_screen <- ref$posterior_content_probability>=menu$posterior_confidence
    ref$objective_gap <- z$ecm_objective-ref$objective
    ref$target_content <- z$target_content
    ref$mean_tilt <- z$mean_tilt
    ref$central_tilt <- z$central_tilt
    ref
  }))
  feasible <- references[references$passes_screen,]
  counterfactual <- if(nrow(feasible)) feasible[order(feasible$width,feasible$target_content,
    abs(feasible$mean_tilt-feasible$central_tilt),feasible$lower,feasible$candidate_index)[1L],] else NULL
  reference <- references[references$candidate_index==selected$candidate_index,]
  trace0 <- action$candidate_traces[action$candidate_traces$candidate_selected,]
  init <- trace0[trace0$iteration==0L,]
  if (nrow(init)!=1L) stop("Selected initial roots are missing.")
  rows <- traces <- list()
  for (multiplier in unlist(config$iteration_multipliers)) {
    control <- controls
    control$max_iter <- as.integer(cfg$ecm_control$max_iter*multiplier)
    fit <- rqrgibbs::rqr_ecm_fit(y,matrix(1,length(y),1),
      coverage_level=selected$target_content,learning_rate=omega,mean_tilt=selected$mean_tilt,
      beta_prior_obj=prior,ecm_control=control,init=list(beta1=init$root1,beta2=init$root2))
    lower <- as.numeric(fit$beta_root1); upper <- as.numeric(fit$beta_root2)
    if (multiplier==1 && max(abs(c(lower-r$lower,upper-r$upper)))>tolerance) {
      stop("Selected fixed-target replay differs from the profile.")
    }
    probability <- pea_probability(y,lower,upper,a$content,menu$dp_concentration,base)
    diag <- pea_saved_diagnostic(y,data.frame(lower=lower,upper=upper,
      mti_selected_q=selected$target_content,mti_selected_tilt=selected$mean_tilt),omega,precision)
    endpoint_error <- max(abs(c(lower-reference$lower,upper-reference$upper)))/sd(y)
    objective_gap <- fit$objective-reference$objective
    rows[[length(rows)+1L]] <- cbind(data.frame(case_id=r$case_id,response_id=r$response_id,
      split_scheme=r$split_scheme,split_id=r$split_id,original_flag=r$mti_ecm_converged,
      target_content=selected$target_content,mean_tilt=selected$mean_tilt,
      multiplier=multiplier,iteration_budget=control$max_iter,iterations=fit$iterations,
      converged=fit$converged,termination=fit$convergence_code,lower=lower,upper=upper,
      width=upper-lower,objective_gap=objective_gap,relative_objective_gap=objective_gap/(1+abs(reference$objective)),
      endpoint_difference_sd=endpoint_error,width_difference_sd=(upper-lower-reference$width)/sd(y),
      reference_lower=reference$lower,reference_upper=reference$upper,reference_width=reference$width,
      reference_objective=reference$objective,
      reference_tie_width_min=reference$numerical_tie_width_min,
      reference_tie_width_max=reference$numerical_tie_width_max,
      posterior_content_probability=probability,passes_screen=probability>=menu$posterior_confidence,
      heldout_empirical_content=mean(heldout>=lower & heldout<=upper),
      original_lower=r$lower,original_upper=r$upper,original_width=r$width,
      endpoint_change_sd=max(abs(c(lower-r$lower,upper-r$upper)))/sd(y),
      screen_threshold=menu$posterior_confidence,
      menu_reference_returned=!is.null(counterfactual),
      menu_reference_width=if(is.null(counterfactual)) NA_real_ else counterfactual$width,
      menu_reference_candidate=if(is.null(counterfactual)) NA_integer_ else counterfactual$candidate_index,
      original_candidate=selected$candidate_index),diag)
    trace <- pea_trace_metrics(fit$objective_trace,y,heldout,selected$target_content,selected$mean_tilt,
      omega,precision,reference,a$content,menu$dp_concentration,base)
    trace$case_id <- r$case_id; trace$multiplier <- multiplier
    traces[[length(traces)+1L]] <- trace
  }
  references$case_id <- r$case_id
  candidates$case_id <- r$case_id
  list(summary=do.call(rbind,rows),traces=do.call(rbind,traces),
       references=references,candidates=candidates)
}

pea_probability <- function(y,lower,upper,content,concentration,base) {
  k <- sum(y>=lower & y<=upper)
  h <- pnorm(upper,base$mean,base$sd)-pnorm(lower,base$mean,base$sd)
  pbeta(content,k+concentration*h,length(y)-k+concentration*(1-h),lower.tail=FALSE)
}

pea_trace_metrics <- function(trace,y,heldout,q,delta,omega,precision,reference,
                              content,concentration,base) {
  trace$objective_gap <- trace$objective-reference$objective
  stats <- lapply(seq_len(nrow(trace)),function(i) {
    l <- trace$root1[i]; u <- trace$root2[i]
    s <- pea_stationarity(y,l,u,q,delta,omega,precision)
    c(stationarity_distance=s$distance,
      posterior_content_probability=pea_probability(y,l,u,content,concentration,base),
      heldout_empirical_content=mean(heldout>=l & heldout<=u))
  })
  cbind(trace,as.data.frame(do.call(rbind,stats)))
}
