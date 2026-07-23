#!/usr/bin/env Rscript

options(warn = 1)

`%||%` <- function(x, y) if (is.null(x)) y else x

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(repo_root, "application", "DESCRIPTION"))) {
  stop("Run this script from the RQR-GIBBS repository root.", call. = FALSE)
}
if (!identical(Sys.getenv("RQR_BOUNDED_PILOT_CONFIRM"), "YES")) {
  stop(
    paste(
      "The bounded pilot requires explicit confirmation:",
      "set RQR_BOUNDED_PILOT_CONFIRM=YES."
    ),
    call. = FALSE
  )
}

required_packages <- c("digest", "jsonlite", "pkgload", "pracma", "testthat")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L)
)]
if (length(missing_packages)) {
  stop(
    "Missing bounded-pilot packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

git_value <- function(root, args) {
  out <- suppressWarnings(system2(
    "git", c("-C", shQuote(root), args), stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) {
    stop(
      "Git command failed in ", root, ": ", paste(args, collapse = " "),
      call. = FALSE
    )
  }
  trimws(paste(out, collapse = "\n"))
}

repository_state <- function(name, root, expected_branch, expected_commit = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  branch <- git_value(root, c("rev-parse", "--abbrev-ref", "HEAD"))
  commit <- tolower(git_value(root, c("rev-parse", "HEAD")))
  dirty_text <- git_value(root, c("status", "--porcelain", "--untracked-files=all"))
  clean <- !nzchar(dirty_text)
  expected_match <- is.null(expected_commit) ||
    identical(commit, tolower(expected_commit))
  branch_match <- identical(branch, expected_branch)
  data.frame(
    repository = name,
    root = root,
    expected_branch = expected_branch,
    branch = branch,
    branch_match = branch_match,
    expected_commit = expected_commit %||% commit,
    commit = commit,
    expected_commit_match = expected_match,
    clean = clean,
    dirty_files = if (clean) "" else gsub("\n", " | ", dirty_text),
    stringsAsFactors = FALSE
  )
}

primary_commit <- tolower(git_value(repo_root, c("rev-parse", "HEAD")))
exdqlm_commit <- "dffb71ee70b597d6a716ee74be1cbc99731cd453"
qdesn_commit <- "f9f22804eff3871bb5350c8add04b7c9f4d4957b"
exdqlm_root <- normalizePath(
  Sys.getenv(
    "EXDQLM_RQR_REPO",
    unset = file.path(dirname(repo_root), "exdqlm__wt__qdesn_0p4p0_integration")
  ),
  winslash = "/", mustWork = TRUE
)
qdesn_root <- normalizePath(
  Sys.getenv(
    "QDESN_ARTICLE_REPO",
    unset = file.path(dirname(repo_root), "Article-Q-DESN---Version-2")
  ),
  winslash = "/", mustWork = TRUE
)
source_state <- rbind(
  repository_state("RQR-GIBBS", repo_root, "main", primary_commit),
  repository_state(
    "exdqlm", exdqlm_root, "feature/rqr-desn-readout-20260716",
    exdqlm_commit
  ),
  repository_state("Q-DESN", qdesn_root, "main", qdesn_commit)
)
if (!all(
      source_state$branch_match &
      source_state$expected_commit_match &
      source_state$clean
    )) {
  stop("The frozen source-state gate failed.", call. = FALSE)
}

run_id <- paste0(
  "rqr_bounded_pilot_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
  "_", substr(primary_commit, 1L, 12L)
)
output_dir <- file.path(repo_root, "application", "outputs", run_id)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

failure_path <- file.path(output_dir, "failure_log.csv")
write.csv(
  data.frame(
    recorded_at = character(0), stage = character(0),
    message = character(0), stringsAsFactors = FALSE
  ),
  failure_path, row.names = FALSE
)

current_stage <- "initialization"
pilot_start <- Sys.time()

record_failure <- function(stage, message) {
  existing <- utils::read.csv(failure_path, stringsAsFactors = FALSE)
  existing <- rbind(existing, data.frame(
    recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    stage = stage,
    message = as.character(message),
    stringsAsFactors = FALSE
  ))
  write.csv(existing, failure_path, row.names = FALSE)
}

directory_digest <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- list.files(
    path, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = FALSE, no.. = TRUE
  )
  files <- sort(normalizePath(files, winslash = "/", mustWork = TRUE))
  relative <- substring(files, nchar(path) + 2L)
  hashes <- vapply(
    files,
    function(file) digest::digest(file = file, algo = "sha256", serialize = FALSE),
    character(1L)
  )
  digest::digest(
    paste(relative, hashes, sep = "\t", collapse = "\n"),
    algo = "sha256", serialize = FALSE
  )
}

write_artifact_hashes <- function() {
  files <- list.files(output_dir, full.names = TRUE, recursive = FALSE)
  files <- files[basename(files) != "artifact_hashes.csv"]
  hashes <- data.frame(
    file = basename(files),
    bytes = as.numeric(file.info(files)$size),
    sha256 = vapply(
      files,
      function(file) digest::digest(
        file = file, algo = "sha256", serialize = FALSE
      ),
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  write.csv(
    hashes[order(hashes$file), , drop = FALSE],
    file.path(output_dir, "artifact_hashes.csv"),
    row.names = FALSE
  )
}

split_chains <- function(x) {
  x <- as.matrix(x)
  half <- floor(nrow(x) / 2L)
  if (half < 2L) stop("At least four draws per chain are required.", call. = FALSE)
  cbind(
    x[seq_len(half), , drop = FALSE],
    x[nrow(x) - half + seq_len(half), , drop = FALSE]
  )
}

rank_normalize <- function(x) {
  dims <- dim(x)
  ranks <- rank(as.numeric(x), ties.method = "average")
  matrix(
    stats::qnorm((ranks - 3 / 8) / (length(ranks) + 1 / 4)),
    nrow = dims[1L], ncol = dims[2L]
  )
}

basic_rhat <- function(x) {
  x <- as.matrix(x)
  n <- nrow(x)
  chain_variances <- apply(x, 2L, stats::var)
  within <- mean(chain_variances)
  between <- n * stats::var(colMeans(x))
  if (within == 0) {
    return(if (between == 0) 1 else Inf)
  }
  sqrt(((n - 1) * within / n + between / n) / within)
}

rank_split_rhat <- function(x) {
  split <- split_chains(x)
  location <- basic_rhat(rank_normalize(split))
  folded <- abs(split - stats::median(as.numeric(split)))
  max(location, basic_rhat(rank_normalize(folded)))
}

ess_core <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  n <- nrow(x)
  m <- ncol(x)
  total <- n * m
  chain_variances <- apply(x, 2L, stats::var)
  within <- mean(chain_variances)
  between <- n * stats::var(colMeans(x))
  var_plus <- (n - 1) * within / n + between / n
  if (!is.finite(var_plus) || var_plus == 0) return(total)
  autocovariance <- vapply(seq_len(m), function(jj) {
    stats::acf(
      x[, jj], type = "covariance", plot = FALSE,
      lag.max = n - 1L, demean = TRUE
    )$acf[, 1L, 1L]
  }, numeric(n))
  rho <- 1 - (within - rowMeans(autocovariance)) / var_plus
  rho[1L] <- 1
  pair_count <- floor(length(rho) / 2L)
  pairs <- rho[2L * seq_len(pair_count) - 1L] +
    rho[2L * seq_len(pair_count)]
  first_negative <- which(pairs < 0)[1L]
  if (!is.na(first_negative)) pairs <- pairs[seq_len(first_negative - 1L)]
  if (!length(pairs)) return(total)
  if (length(pairs) > 1L) {
    for (ii in 2:length(pairs)) pairs[ii] <- min(pairs[ii], pairs[ii - 1L])
  }
  tau <- max(1, -1 + 2 * sum(pairs))
  min(total, total / tau)
}

chain_diagnostics <- function(chains, sampler) {
  estimands <- dimnames(chains)[[2L]]
  do.call(rbind, lapply(estimands, function(estimand) {
    x <- chains[, estimand, , drop = TRUE]
    split <- split_chains(x)
    bulk <- ess_core(rank_normalize(split))
    q <- stats::quantile(
      as.numeric(x), probs = c(0.05, 0.95), names = FALSE, type = 8
    )
    lower_indicator <- split_chains(x <= q[1L])
    upper_indicator <- split_chains(x <= q[2L])
    tail <- min(ess_core(lower_indicator), ess_core(upper_indicator))
    data.frame(
      sampler = sampler,
      estimand = estimand,
      rhat = rank_split_rhat(x),
      bulk_ess = bulk,
      tail_ess = tail,
      finite = all(is.finite(x)),
      stringsAsFactors = FALSE
    )
  }))
}

estimands_from_roots <- function(beta1, beta2, lambda, y, alpha, s_L) {
  lower <- pmin(beta1, beta2)
  upper <- pmax(beta1, beta2)
  loss <- vapply(seq_along(beta1), function(ii) {
    residual_product <- (y - beta1[ii]) * (y - beta2[ii])
    sum(residual_product * (alpha - as.numeric(residual_product < 0)))
  }, numeric(1L))
  cbind(
    lambda = lambda,
    effective_learning_rate = lambda / s_L,
    lower_root = lower,
    upper_root = upper,
    width = upper - lower,
    midpoint = 0.5 * (lower + upper),
    total_loss = loss
  )
}

fully_augmented_chain <- function(
    seed, y, X, alpha, s_L, tau2, lambda_shape, lambda_rate,
    n_burn, n_keep) {
  set.seed(seed)
  init <- rqrgibbs:::.rqr_init_roots(y, X, alpha)
  beta1 <- init$beta1
  beta2 <- init$beta2
  lambda <- 1
  v <- rep(1, length(y))
  prior_precision <- rep(1 / tau2, ncol(X))
  precision_control <- list(jitter_ladder = 0)
  draws <- matrix(NA_real_, n_keep, 3L)
  colnames(draws) <- c("beta1", "beta2", "lambda")
  repairs <- 0L
  save_index <- 0L
  total_iterations <- n_burn + n_keep
  for (iteration in seq_len(total_iterations)) {
    constants <- rqr_constants(alpha, lambda / s_L)
    eta1 <- drop(X %*% beta1)
    eta2 <- drop(X %*% beta2)
    residual_product <- rqr_residual_product(y, eta1, eta2)
    augmentation_rate <- lambda_rate + sum(
      v + (residual_product - constants$xi * v)^2 /
        (2 * constants$phi * v)
    ) / s_L
    lambda <- stats::rgamma(
      1L, shape = lambda_shape + 3 * length(y) / 2,
      rate = augmentation_rate
    )
    constants <- rqr_constants(alpha, lambda / s_L)
    parameters <- rqr_gig_params(
      residual_product, coverage_level = alpha,
      learning_rate = constants$omega
    )
    v <- rqr_sample_gig_half(parameters$b, parameters$a)
    update1 <- rqrgibbs:::.rqr_beta_update(
      y, X, beta2, v, constants, prior_precision,
      precision_beta_cfg = precision_control,
      context = list(iteration = iteration, root = "root1")
    )
    beta1 <- update1$draw
    update2 <- rqrgibbs:::.rqr_beta_update(
      y, X, beta1, v, constants, prior_precision,
      precision_beta_cfg = precision_control,
      context = list(iteration = iteration, root = "root2")
    )
    beta2 <- update2$draw
    for (info in list(update1$info, update2$info)) {
      repairs <- repairs +
        as.integer((info$jitter %||% 0) > 0) +
        as.integer((info$clamped_eigenvalues %||% 0L) > 0L)
    }
    if (stats::runif(1L) < 0.5) {
      temporary <- beta1
      beta1 <- beta2
      beta2 <- temporary
    }
    if (iteration > n_burn) {
      save_index <- save_index + 1L
      draws[save_index, ] <- c(beta1, beta2, lambda)
    }
  }
  list(
    estimands = estimands_from_roots(
      draws[, "beta1"], draws[, "beta2"], draws[, "lambda"],
      y, alpha, s_L
    ),
    repairs = repairs
  )
}

quadrature_reference <- function(
    y, alpha, s_L, tau2, lambda_shape, lambda_rate,
    relative_tolerance = 1e-10) {
  n <- length(y)
  prior_sd <- sqrt(tau2)
  cuts <- sort(unique(c(0, stats::pnorm(y / prior_sd), 1)))
  loss_function <- function(roots) {
    residual_product <- (y - roots[1L]) * (y - roots[2L])
    sum(residual_product * (alpha - as.numeric(residual_product < 0)))
  }
  optimizer <- stats::optim(
    stats::quantile(y, c(0.1, 0.9), names = FALSE),
    loss_function, method = "Nelder-Mead",
    control = list(reltol = 1e-14, maxit = 10000L)
  )
  if (optimizer$convergence != 0L) {
    stop("Could not obtain a stable quadrature log-scale.", call. = FALSE)
  }
  marginal_shape <- lambda_shape + n
  log_shift <- -marginal_shape * log(
    lambda_rate + optimizer$value / s_L
  )
  integrand <- function(probability1, probability2, quantity) {
    probability1 <- pmin(
      pmax(probability1, .Machine$double.xmin),
      1 - .Machine$double.eps
    )
    probability2 <- pmin(
      pmax(probability2, .Machine$double.xmin),
      1 - .Machine$double.eps
    )
    root1 <- stats::qnorm(probability1) * prior_sd
    root2 <- stats::qnorm(probability2) * prior_sd
    loss <- numeric(length(root1))
    for (response in y) {
      residual_product <- (response - root1) * (response - root2)
      loss <- loss + residual_product *
        (alpha - as.numeric(residual_product < 0))
    }
    rate <- lambda_rate + loss / s_L
    weight <- exp(-marginal_shape * log(rate) - log_shift)
    value <- switch(
      quantity,
      denominator = 1,
      lambda = marginal_shape / rate,
      effective_learning_rate = marginal_shape / (rate * s_L),
      lower_root = pmin(root1, root2),
      upper_root = pmax(root1, root2),
      width = abs(root1 - root2),
      midpoint = 0.5 * (root1 + root2),
      total_loss = loss,
      stop("Unknown quadrature quantity.", call. = FALSE)
    )
    weight * value
  }
  integrate_quantity <- function(quantity) {
    value <- 0
    error <- 0
    for (ii in seq_len(length(cuts) - 1L)) {
      for (jj in seq_len(length(cuts) - 1L)) {
        result <- pracma::integral2(
          function(probability1, probability2) {
            integrand(probability1, probability2, quantity)
          },
          cuts[ii], cuts[ii + 1L], cuts[jj], cuts[jj + 1L],
          reltol = relative_tolerance, abstol = 1e-16,
          maxlist = 5000L, vectorized = TRUE
        )
        value <- value + result$Q
        error <- error + result$error
      }
    }
    c(value = value, error = error)
  }
  denominator <- integrate_quantity("denominator")
  quantities <- c(
    "lambda", "effective_learning_rate", "lower_root", "upper_root",
    "width", "midpoint", "total_loss"
  )
  results <- do.call(rbind, lapply(quantities, function(quantity) {
    numerator <- integrate_quantity(quantity)
    relative_error_bound <- numerator["error"] /
      max(abs(numerator["value"]), .Machine$double.xmin) +
      denominator["error"] / denominator["value"]
    data.frame(
      estimand = quantity,
      mean = unname(numerator["value"] / denominator["value"]),
      relative_error_bound = unname(relative_error_bound),
      requested_relative_tolerance = relative_tolerance,
      pass = is.finite(relative_error_bound) &&
        relative_error_bound <= 1e-9,
      stringsAsFactors = FALSE
    )
  }))
  list(
    reference = results,
    normalizing_integral = unname(denominator["value"]),
    normalizing_error = unname(denominator["error"]),
    minimum_loss = optimizer$value
  )
}

main <- function() {
  current_stage <<- "runtime binding"
  runtime_root <- normalizePath(
    Sys.getenv(
      "RQR_EXDQLM_RUNTIME_ROOT",
      unset = file.path(repo_root, "application", "cache", "exdqlm_runtime")
    ),
    winslash = "/", mustWork = TRUE
  )
  runtime_library <- normalizePath(
    file.path(runtime_root, "library"), winslash = "/", mustWork = TRUE
  )
  runtime_attestation <- normalizePath(
    file.path(
      runtime_root, "attestations",
      paste0("exdqlm_", substr(exdqlm_commit, 1L, 12L), ".rds")
    ),
    winslash = "/", mustWork = TRUE
  )
  .libPaths(c(runtime_library, .libPaths()))
  if ("exdqlm" %in% loadedNamespaces()) {
    stop("exdqlm was loaded before the isolated library was selected.", call. = FALSE)
  }
  pkgload::load_all(file.path(repo_root, "application"), quiet = TRUE)
  runtime_state <- rqrgibbs:::.rqr_repository_provenance(list(
    repo_root = exdqlm_root,
    expected_git_commit = exdqlm_commit,
    runtime_package = "exdqlm",
    runtime_attestation = runtime_attestation
  ))
  if (!isTRUE(runtime_state$runtime_attestation_match) ||
      !isTRUE(runtime_state$runtime_source_match) ||
      !isTRUE(runtime_state$reproducibility_eligible)) {
    stop("The isolated exdqlm runtime-source gate failed.", call. = FALSE)
  }
  runtime_table <- data.frame(
    package = runtime_state$runtime_package,
    runtime_package_path = runtime_state$runtime_package_path,
    runtime_package_version = runtime_state$runtime_package_version,
    source_commit = runtime_state$git_commit,
    source_tree_digest = runtime_state$source_tree_digest,
    runtime_package_tree_digest = runtime_state$runtime_package_tree_digest,
    runtime_attestation = runtime_state$runtime_attestation,
    runtime_attestation_match = runtime_state$runtime_attestation_match,
    runtime_source_match = runtime_state$runtime_source_match,
    reproducibility_eligible = runtime_state$reproducibility_eligible,
    stringsAsFactors = FALSE
  )
  write.csv(
    runtime_table,
    file.path(output_dir, "runtime_package_provenance.csv"),
    row.names = FALSE
  )
  write.csv(
    source_state, file.path(output_dir, "source_state.csv"), row.names = FALSE
  )

  current_stage <<- "deterministic native tests"
  test_log <- file.path(output_dir, "native_test_log.txt")
  test_expression <- paste0(
    "library(rqrgibbs);",
    "testthat::test_dir(",
    shQuote(file.path(repo_root, "application", "tests", "testthat")),
    ",filter='native',reporter='summary')"
  )
  test_status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(test_expression)),
    stdout = test_log, stderr = test_log
  )
  if (!identical(as.integer(test_status), 0L)) {
    stop("The deterministic native test matrix failed.", call. = FALSE)
  }
  deterministic_fixtures <- data.frame(
    fixture = c(
      "tiny-scale asymmetric C0",
      "tiny-scale indefinite fixed W",
      "machine-relative symmetric roundoff",
      "repaired-parent continuation",
      "dirty or unknown primary source continuation",
      "auto backend resolved-backend change",
      "mismatched exdqlm runtime and checkout",
      "exact isolated exdqlm runtime",
      "p=2 T=4 dense fixed-W FFBS",
      "scalar component forecast H=3",
      "missing pseudo-observation",
      "zero pseudo-design row",
      "exact PSD covariance draw",
      "negative eigenvalue under fail mode"
    ),
    status = "pass",
    evidence = c(
      rep("application/tests/testthat/test-rqr-native-model.R", 3L),
      rep("application/tests/testthat/test-rqr-native-sampler.R", 5L),
      "application/tests/testthat/test-rqr-native-ffbs.R",
      "application/tests/testthat/test-rqr-native-sampler.R",
      rep("application/tests/testthat/test-rqr-native-ffbs.R", 4L)
    ),
    stringsAsFactors = FALSE
  )
  write.csv(
    deterministic_fixtures,
    file.path(output_dir, "deterministic_checks.csv"),
    row.names = FALSE
  )

  y <- c(
    -2.0, -1.3, -0.8, -0.4, -0.1, 0.1,
     0.35, 0.7, 1.1, 1.6, 2.2, 3.0
  )
  X <- matrix(1, 12L, 1L)
  alpha <- 0.80
  s_L <- 1
  tau2 <- 25
  lambda_shape <- 4
  lambda_rate <- 4
  n_burn <- 5000L
  n_keep <- 20000L
  collapsed_seeds <- 73201:73204
  augmented_seeds <- 73301:73304
  estimand_names <- c(
    "lambda", "effective_learning_rate", "lower_root", "upper_root",
    "width", "midpoint", "total_loss"
  )

  manifest <- list(
    schema_version = "rqrgibbs_bounded_pilot/1.0.0",
    run_id = run_id,
    scope = "bounded_validation_only",
    generalized_bayes = TRUE,
    response_likelihood = FALSE,
    production_simulation_authorized = FALSE,
    primary_commit = primary_commit,
    exdqlm_commit = exdqlm_commit,
    qdesn_reference_commit = qdesn_commit,
    isolated_R_library = runtime_library,
    rqrgibbs_namespace_path = getNamespaceInfo(
      asNamespace("rqrgibbs"), "path"
    ),
    exdqlm_runtime_path = runtime_state$runtime_package_path,
    exdqlm_source_tree_digest = runtime_state$source_tree_digest,
    exdqlm_runtime_tree_digest = runtime_state$runtime_package_tree_digest,
    resolved_backend = rqrgibbs:::.rqr_resolve_ffbs_backend("auto"),
    RNGkind = RNGkind(),
    fixture = list(
      y = y, X = unclass(X), coverage_level = alpha,
      loss_reference_scale = s_L, beta_prior_tau2 = tau2,
      lambda_prior_shape = lambda_shape, lambda_prior_rate = lambda_rate
    ),
    mcmc = list(
      collapsed_seeds = collapsed_seeds,
      fully_augmented_seeds = augmented_seeds,
      burn_in = n_burn, retained_per_chain = n_keep, thin = 1L
    ),
    gates = list(
      rhat_max = 1.01, bulk_ess_min = 2000, tail_ess_min = 2000,
      mean_mcse_multiplier = 4, quadrature_relative_tolerance = 1e-10,
      maximum_wall_minutes = 90, maximum_workers = 2,
      maximum_resident_memory_gb = 4, maximum_artifact_mb = 250
    ),
    cdf_thresholds = list(
      lambda = 1.0, lower_root = -1.5, upper_root = 2.5,
      width = 4.0, midpoint = 0.5
    ),
    package_versions = as.list(vapply(
      c("rqrgibbs", "exdqlm", required_packages),
      function(package) as.character(utils::packageVersion(package)),
      character(1L)
    )),
    started_at = format(pilot_start, tz = "UTC", usetz = TRUE)
  )
  jsonlite::write_json(
    manifest, file.path(output_dir, "pilot_manifest.json"),
    auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  current_stage <<- "production collapsed chains"
  collapsed <- array(
    NA_real_, c(n_keep, length(estimand_names), length(collapsed_seeds)),
    dimnames = list(NULL, estimand_names, paste0("chain", seq_along(collapsed_seeds)))
  )
  collapsed_repairs <- integer(length(collapsed_seeds))
  collapsed_promotion <- logical(length(collapsed_seeds))
  for (chain in seq_along(collapsed_seeds)) {
    fit <- rqr_mcmc_fit(
      y = y, X = X, coverage_level = alpha,
      lambda_initial = 1, loss_reference_scale = s_L,
      learning_rate_mode = "learned_pseudoresidual_normalized",
      lambda_prior = list(shape = lambda_shape, rate = lambda_rate),
      beta_prior_obj = beta_prior("ridge", ridge = list(tau2 = tau2)),
      numerical_policy = "fail",
      provenance_control = list(
        repo_root = repo_root, expected_git_commit = primary_commit
      ),
      mcmc_control = list(
        n_burn = n_burn, n_mcmc = n_keep, thin = 1L,
        seed = collapsed_seeds[chain],
        precision_beta = list(jitter_ladder = 0),
        store_latent_draws = FALSE
      )
    )
    collapsed[, , chain] <- estimands_from_roots(
      fit$samp.beta_root1[, 1L], fit$samp.beta_root2[, 1L],
      fit$samp.lambda, y, alpha, s_L
    )
    collapsed_repairs[chain] <- fit$model_spec$numerical_repair_count
    collapsed_promotion[chain] <- isTRUE(fit$model_spec$promotion_eligible)
  }

  current_stage <<- "fully augmented chains"
  augmented <- array(
    NA_real_, c(n_keep, length(estimand_names), length(augmented_seeds)),
    dimnames = list(NULL, estimand_names, paste0("chain", seq_along(augmented_seeds)))
  )
  augmented_repairs <- integer(length(augmented_seeds))
  for (chain in seq_along(augmented_seeds)) {
    fit <- fully_augmented_chain(
      augmented_seeds[chain], y, X, alpha, s_L, tau2,
      lambda_shape, lambda_rate, n_burn, n_keep
    )
    augmented[, , chain] <- fit$estimands
    augmented_repairs[chain] <- fit$repairs
  }

  current_stage <<- "chain diagnostics"
  diagnostics <- rbind(
    chain_diagnostics(collapsed, "collapsed"),
    chain_diagnostics(augmented, "fully_augmented")
  )
  diagnostics$pass <- with(
    diagnostics,
    finite & rhat <= 1.01 & bulk_ess >= 2000 & tail_ess >= 2000
  )
  write.csv(
    diagnostics, file.path(output_dir, "chain_diagnostics.csv"),
    row.names = FALSE
  )
  repair_records <- rbind(
    data.frame(
      sampler = "collapsed", chain = seq_along(collapsed_repairs),
      repair_count = collapsed_repairs
    ),
    data.frame(
      sampler = "fully_augmented", chain = seq_along(augmented_repairs),
      repair_count = augmented_repairs
    )
  )
  repair_records$pass <- repair_records$repair_count == 0L
  write.csv(
    repair_records, file.path(output_dir, "repair_records.csv"),
    row.names = FALSE
  )

  current_stage <<- "adaptive quadrature"
  quadrature <- quadrature_reference(
    y, alpha, s_L, tau2, lambda_shape, lambda_rate,
    relative_tolerance = 1e-10
  )
  write.csv(
    quadrature$reference,
    file.path(output_dir, "quadrature_diagnostics.csv"),
    row.names = FALSE
  )

  current_stage <<- "reference comparisons"
  diagnostic_lookup <- function(sampler, estimand) {
    diagnostics[
      diagnostics$sampler == sampler & diagnostics$estimand == estimand,
      , drop = FALSE
    ]
  }
  mean_rows <- do.call(rbind, lapply(estimand_names, function(estimand) {
    collapsed_values <- as.numeric(collapsed[, estimand, ])
    augmented_values <- as.numeric(augmented[, estimand, ])
    collapsed_diag <- diagnostic_lookup("collapsed", estimand)
    augmented_diag <- diagnostic_lookup("fully_augmented", estimand)
    collapsed_mcse <- stats::sd(collapsed_values) /
      sqrt(collapsed_diag$bulk_ess)
    augmented_mcse <- stats::sd(augmented_values) /
      sqrt(augmented_diag$bulk_ess)
    collapsed_mean <- mean(collapsed_values)
    augmented_mean <- mean(augmented_values)
    quadrature_mean <- quadrature$reference$mean[
      quadrature$reference$estimand == estimand
    ]
    data.frame(
      comparison_type = "mean",
      estimand = estimand,
      threshold = NA_real_,
      collapsed = collapsed_mean,
      fully_augmented = augmented_mean,
      quadrature = quadrature_mean,
      collapsed_mcse = collapsed_mcse,
      fully_augmented_mcse = augmented_mcse,
      difference = abs(collapsed_mean - augmented_mean),
      tolerance = 4 * sqrt(collapsed_mcse^2 + augmented_mcse^2),
      collapsed_vs_augmented_pass =
        abs(collapsed_mean - augmented_mean) <=
          4 * sqrt(collapsed_mcse^2 + augmented_mcse^2),
      collapsed_vs_quadrature_pass =
        abs(collapsed_mean - quadrature_mean) <= 4 * collapsed_mcse,
      augmented_vs_quadrature_pass =
        abs(augmented_mean - quadrature_mean) <= 4 * augmented_mcse,
      stringsAsFactors = FALSE
    )
  }))
  thresholds <- c(
    lambda = 1.0, lower_root = -1.5, upper_root = 2.5,
    width = 4.0, midpoint = 0.5
  )
  cdf_rows <- do.call(rbind, lapply(names(thresholds), function(estimand) {
    threshold <- thresholds[[estimand]]
    collapsed_indicator <- collapsed[, estimand, ] <= threshold
    augmented_indicator <- augmented[, estimand, ] <= threshold
    collapsed_probability <- mean(collapsed_indicator)
    augmented_probability <- mean(augmented_indicator)
    collapsed_ess <- ess_core(split_chains(collapsed_indicator))
    augmented_ess <- ess_core(split_chains(augmented_indicator))
    collapsed_mcse <- sqrt(
      collapsed_probability * (1 - collapsed_probability) / collapsed_ess
    )
    augmented_mcse <- sqrt(
      augmented_probability * (1 - augmented_probability) / augmented_ess
    )
    tolerance <- 4 * sqrt(collapsed_mcse^2 + augmented_mcse^2)
    data.frame(
      comparison_type = "cdf",
      estimand = estimand,
      threshold = threshold,
      collapsed = collapsed_probability,
      fully_augmented = augmented_probability,
      quadrature = NA_real_,
      collapsed_mcse = collapsed_mcse,
      fully_augmented_mcse = augmented_mcse,
      difference = abs(collapsed_probability - augmented_probability),
      tolerance = tolerance,
      collapsed_vs_augmented_pass =
        abs(collapsed_probability - augmented_probability) <= tolerance,
      collapsed_vs_quadrature_pass = NA,
      augmented_vs_quadrature_pass = NA,
      stringsAsFactors = FALSE
    )
  }))
  comparisons <- rbind(mean_rows, cdf_rows)
  write.csv(
    comparisons, file.path(output_dir, "reference_comparisons.csv"),
    row.names = FALSE
  )

  current_stage <<- "continuation and provenance summary"
  continuation_checks <- data.frame(
    check = c(
      "native continuation matrix",
      "collapsed primary provenance eligible",
      "collapsed promotion eligible",
      "isolated exdqlm runtime source match",
      "source repositories clean and exact"
    ),
    pass = c(
      TRUE,
      all(collapsed_promotion),
      all(collapsed_promotion),
      isTRUE(runtime_state$runtime_source_match),
      all(
        source_state$branch_match &
          source_state$expected_commit_match &
          source_state$clean
      )
    ),
    stringsAsFactors = FALSE
  )
  write.csv(
    continuation_checks,
    file.path(output_dir, "continuation_checks.csv"),
    row.names = FALSE
  )

  capture.output(
    utils::sessionInfo(), file = file.path(output_dir, "session_info.txt")
  )
  elapsed_minutes <- as.numeric(
    difftime(Sys.time(), pilot_start, units = "mins")
  )
  artifact_bytes_before_hashes <- sum(
    file.info(list.files(output_dir, full.names = TRUE))$size
  )
  gates <- c(
    diagnostics = all(diagnostics$pass),
    repairs = all(repair_records$pass),
    quadrature = all(quadrature$reference$pass),
    mean_comparisons = all(
      mean_rows$collapsed_vs_augmented_pass &
        mean_rows$collapsed_vs_quadrature_pass &
        mean_rows$augmented_vs_quadrature_pass
    ),
    cdf_comparisons = all(cdf_rows$collapsed_vs_augmented_pass),
    continuation_and_provenance = all(continuation_checks$pass),
    wall_time = elapsed_minutes <= 90,
    artifact_size = artifact_bytes_before_hashes <= 250 * 1024^2
  )
  passed <- all(gates)
  closeout <- c(
    "# RQR bounded-pilot closeout",
    "",
    paste0("- Run: `", run_id, "`"),
    paste0("- Frozen primary commit: `", primary_commit, "`"),
    paste0("- Decision: **", if (passed) "PASS" else "FAIL", "**"),
    sprintf("- Elapsed wall time: %.2f minutes", elapsed_minutes),
    sprintf(
      "- Artifact size before hash manifest: %.3f MB",
      artifact_bytes_before_hashes / 1024^2
    ),
    "",
    "## Gates",
    "",
    paste0("- ", names(gates), ": ", ifelse(gates, "pass", "fail")),
    "",
    "This was a bounded generalized-Bayes validation fixture. It did not run a",
    "production simulation and did not create posterior-predictive response",
    "draws. A pass does not by itself establish empirical interval calibration."
  )
  writeLines(closeout, file.path(output_dir, "closeout.md"))
  write_artifact_hashes()
  if (!passed) stop("One or more bounded-pilot gates failed.", call. = FALSE)
  cat("Bounded pilot passed.\n")
  cat("Artifacts:", output_dir, "\n")
  invisible(output_dir)
}

tryCatch(
  main(),
  error = function(error) {
    record_failure(current_stage, conditionMessage(error))
    if (!file.exists(file.path(output_dir, "closeout.md"))) {
      writeLines(
        c(
          "# RQR bounded-pilot closeout",
          "",
          "- Decision: **FAIL**",
          paste0("- Failed stage: `", current_stage, "`"),
          paste0("- Error: ", conditionMessage(error)),
          "",
          "No production simulation was authorized."
        ),
        file.path(output_dir, "closeout.md")
      )
    }
    write_artifact_hashes()
    message("Bounded pilot failed: ", conditionMessage(error))
    quit(save = "no", status = 1L)
  }
)
