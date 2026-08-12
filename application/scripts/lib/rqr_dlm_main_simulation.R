# Deterministic helpers for the preliminary matched RQR-DLM simulation.
#
# This library constructs design objects and compact reference fixtures. It
# contains no diagnostic-pilot or confirmatory execution authorization.

`%||%` <- function(a, b) if (is.null(a)) b else a

rqr_main_contract_schema <- function() {
  "rqrgibbs_dlm_main_simulation_preliminary/0.2.0"
}

rqr_main_find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "AGENTS.md")) &&
        file.exists(file.path(current, "application", "DESCRIPTION"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the RQR-GIBBS repository root.", call. = FALSE)
    }
    current <- parent
  }
}

rqr_main_read_contract <- function(repo_root = rqr_main_find_repo_root()) {
  config_root <- file.path(repo_root, "application", "config", "rqr_dlm")
  env <- new.env(parent = baseenv())
  sys.source(
    file.path(
      config_root, "rqr_dlm_main_simulation_preliminary_20260724.R"
    ),
    envir = env
  )
  list(
    config = env$rqr_dlm_main_simulation_preliminary,
    scenarios = utils::read.csv(
      file.path(
        config_root,
        "rqr_dlm_main_simulation_preliminary_scenarios_20260724.csv"
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    ),
    methods = utils::read.csv(
      file.path(
        config_root,
        "rqr_dlm_main_simulation_preliminary_methods_20260724.csv"
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
}

rqr_main_validate_contract <- function(contract) {
  if (!is.list(contract) ||
      !all(c("config", "scenarios", "methods") %in% names(contract))) {
    stop("The main-simulation contract is incomplete.", call. = FALSE)
  }
  config <- contract$config
  scenarios <- contract$scenarios
  methods <- contract$methods
  if (!identical(config$schema_version, rqr_main_contract_schema()) ||
      isTRUE(config$diagnostic_pilot_execution_authorized) ||
      isTRUE(config$confirmatory_execution_authorized) ||
      !isTRUE(config$generalized_bayes) ||
      isTRUE(config$response_likelihood) ||
      isTRUE(config$response_prediction_contract)) {
    stop("The main-simulation configuration is not fail closed.",
         call. = FALSE)
  }
  if (anyDuplicated(scenarios$dgp_id) || anyDuplicated(methods$method_id) ||
      !setequal(
        scenarios$dgp_id,
        c(config$design$primary_dgp_ids, config$design$sensitivity_dgp_ids)
      )) {
    stop("Scenario or method IDs are incomplete or duplicated.",
         call. = FALSE)
  }
  if (!all(scenarios$scale_floor > 0) ||
      !all(scenarios$minimum_root_separation > 0) ||
      !all(scenarios$core_T > 0L) ||
      !all(scenarios$core_H > 0L) ||
      !all(scenarios$shared_response_law_across_coverages)) {
    stop("Scenario positivity or shared-response-law gates failed.",
         call. = FALSE)
  }
  seasonal <- scenarios[
    scenarios$matched_pair_id == "trend_seasonal_error_pair", ,
    drop = FALSE
  ]
  matched <- c(
    "state_structure", "initial_state_law", "predictor_law",
    "innovation_covariance", "seasonal_period", "seasonal_amplitude",
    "seasonal_phase", "scale_formula", "scale_floor", "core_T", "core_H"
  )
  if (nrow(seasonal) != 2L ||
      !all(vapply(matched, function(field) {
        identical(seasonal[[field]][[1L]], seasonal[[field]][[2L]])
      }, logical(1L)))) {
    stop("The Gaussian and skewed trend-seasonal controls are not matched.",
         call. = FALSE)
  }
  if (!"rqr_dlm_common_evolution_ablation" %in% methods$method_id ||
      !identical(
        config$methods$external_source$sha256,
        "51bc968f617721c9ab1dcfc6ec14857d30827fcd36659f3de45337cc3c82bd14"
      ) ||
      !identical(
        config$methods$static_external_source$sha256,
        "f42292c5ab25a15f39295b93391deafef192fe09eefde563399a64eba7e0169a"
      ) ||
      isTRUE(config$methods$external_source$protected_checkout_used)) {
    stop("Comparator or component-evolution ablation contracts failed.",
         call. = FALSE)
  }
  invisible(TRUE)
}

rqr_main_seed <- function(config, stream, ...) {
  if (!stream %in% config$seed_contract$streams) {
    stop("Unknown main-simulation seed stream.", call. = FALSE)
  }
  key <- paste(
    config$seed_contract$schema_version,
    config$seed_contract$master_seed,
    stream,
    paste(..., collapse = "::"),
    sep = "|"
  )
  hash <- digest::digest(key, algo = "sha256", serialize = FALSE)
  seed <- strtoi(substr(hash, 1L, 7L), base = 16L)
  if (!is.finite(seed) || seed <= 0L) {
    stop("Could not derive a positive deterministic seed.", call. = FALSE)
  }
  as.integer(seed)
}

rqr_main_error_draw <- function(family, n) {
  if (identical(family, "standard_normal")) {
    return(stats::rnorm(n))
  }
  if (identical(family, "centered_standardized_lognormal")) {
    logsd <- 0.75
    mean_raw <- exp(0.5 * logsd^2)
    sd_raw <- sqrt((exp(logsd^2) - 1) * exp(logsd^2))
    return((stats::rlnorm(n, 0, logsd) - mean_raw) / sd_raw)
  }
  if (identical(family, "standardized_student_t_5")) {
    return(stats::rt(n, df = 5) * sqrt(3 / 5))
  }
  if (identical(family, "standardized_skewed_normal_t_mixture")) {
    component <- stats::runif(n) > 0.90
    raw <- stats::rnorm(n)
    raw[component] <- 2 + stats::rt(sum(component), df = 3)
    raw_mean <- 0.2
    raw_second <- 0.9 * 1 + 0.1 * (3 + 2^2)
    return((raw - raw_mean) / sqrt(raw_second - raw_mean^2))
  }
  stop(sprintf("Unsupported simulation error family: %s.", family),
       call. = FALSE)
}

rqr_main_oracle_family <- function(error_family) {
  switch(
    error_family,
    standard_normal = list(family = "gaussian", params = list()),
    centered_standardized_lognormal = list(
      family = "centered_standardized_lognormal",
      params = list(logmean = 0, logsd = 0.75)
    ),
    standardized_student_t_5 = list(
      family = "student_t",
      params = list(df = 5, scale = sqrt(3 / 5))
    ),
    standardized_skewed_normal_t_mixture = list(
      family = "normal_t_mixture",
      params = list(
        normal_weight = 0.90, t_weight = 0.10,
        t_df = 3, t_shift = 2, t_scale = 1,
        variance_standardized = TRUE
      )
    ),
    stop("No oracle family mapping exists.", call. = FALSE)
  )
}

rqr_main_generate_dgp <- function(
    contract, dgp_id, replication, coverage_level,
    n_time = NULL, horizon = NULL) {
  rqr_main_validate_contract(contract)
  row <- contract$scenarios[
    contract$scenarios$dgp_id == dgp_id, , drop = FALSE
  ]
  if (nrow(row) != 1L) stop("Unknown DGP ID.", call. = FALSE)
  replication <- as.integer(replication)[1L]
  if (!is.finite(replication) || replication < 1L) {
    stop("replication must be a positive integer.", call. = FALSE)
  }
  coverage_level <- as.numeric(coverage_level)[1L]
  if (!coverage_level %in% contract$config$design$coverage_levels) {
    stop("coverage_level is not in the frozen design.", call. = FALSE)
  }
  T <- as.integer(n_time %||% row$core_T[[1L]])
  H <- as.integer(horizon %||% row$core_H[[1L]])
  total <- T + H
  path_key <- if (
      is.na(row$matched_pair_id[[1L]]) ||
      identical(row$matched_pair_id[[1L]], "none")
    ) {
    dgp_id
  } else {
    row$matched_pair_id[[1L]]
  }
  state_seed <- rqr_main_seed(
    contract$config, "data", path_key, replication, "state"
  )
  error_seed <- rqr_main_seed(
    contract$config, "data", dgp_id, replication, "error"
  )
  forecast_seed <- rqr_main_seed(
    contract$config, "forecast", path_key, replication
  )
  set.seed(state_seed)
  mu <- rep(0, total)
  scale <- rep(1, total)
  x <- rep(0, total)
  latent <- list()

  if (identical(dgp_id, "static_gaussian_negative_control")) {
    x <- stats::rnorm(total)
    mu <- 0.5 + 0.75 * x
  } else if (dgp_id %in% c(
      "local_level_gaussian", "local_level_skewed"
    )) {
    mu[[1L]] <- stats::rnorm(1)
    mu[-1L] <- mu[[1L]] + cumsum(stats::rnorm(
      total - 1L, sd = sqrt(0.02)
    ))
  } else if (dgp_id %in% c(
      "trend_seasonal_gaussian", "trend_seasonal_skewed"
    )) {
    level <- slope <- rep(0, total)
    level[[1L]] <- stats::rnorm(1)
    slope[[1L]] <- stats::rnorm(1, sd = sqrt(0.1))
    for (tt in 2:total) {
      slope[[tt]] <- slope[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.0005))
      level[[tt]] <- level[[tt - 1L]] + slope[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.005))
    }
    seasonal <- 0.60 * sin(2 * pi * seq_len(total) / 12)
    mu <- level + seasonal
    scale <- pmax(
      row$scale_floor[[1L]],
      1 + 0.30 * sin(2 * pi * seq_len(total) / 12)
    )
    latent <- list(level = level, slope = slope, seasonal = seasonal)
  } else if (identical(
      dgp_id, "trend_regression_unequal_evolution"
    )) {
    level <- slope <- beta <- x <- rep(0, total)
    level[[1L]] <- stats::rnorm(1)
    slope[[1L]] <- stats::rnorm(1, sd = sqrt(0.1))
    beta[[1L]] <- stats::rnorm(1, 0.5, 0.5)
    x[[1L]] <- stats::rnorm(1)
    for (tt in 2:total) {
      slope[[tt]] <- slope[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.0005))
      level[[tt]] <- level[[tt - 1L]] + slope[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.005))
      beta[[tt]] <- beta[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.05))
      x[[tt]] <- 0.7 * x[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.51))
    }
    mu <- level + beta * x
    latent <- list(level = level, slope = slope, beta = beta)
  } else if (identical(
      dgp_id, "structural_break_heavy_tail_stress"
    )) {
    level <- beta <- rep(0, total)
    level[[1L]] <- stats::rnorm(1)
    beta[[1L]] <- 0.5
    x <- stats::rnorm(total)
    for (tt in 2:total) {
      level[[tt]] <- level[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.02))
      beta[[tt]] <- beta[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.01))
    }
    break_time <- max(2L, floor(0.70 * T))
    beta[break_time:total] <- beta[break_time:total] + 1.5
    mu <- level + beta * x
    latent <- list(level = level, beta = beta, break_time = break_time)
  } else if (identical(
      dgp_id, "heteroscedastic_known_scale_covariate"
    )) {
    mu[[1L]] <- stats::rnorm(1)
    z <- rep(0, total)
    z[[1L]] <- stats::rnorm(1)
    for (tt in 2:total) {
      mu[[tt]] <- mu[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.02))
      z[[tt]] <- 0.8 * z[[tt - 1L]] +
        stats::rnorm(1, sd = sqrt(0.36))
    }
    scale <- pmax(row$scale_floor[[1L]], exp(0.25 * z))
    x <- z
    latent <- list(scale_covariate = z)
  } else if (identical(
      dgp_id, "independent_root_prior_alignment"
    )) {
    oracle_ref <- rqr_oracle_roots(
      "centered_standardized_lognormal", row$reference_coverage[[1L]],
      params = list(logmean = 0, logsd = 0.75)
    )
    lower_ref <- upper_ref <- rep(0, total)
    lower_ref[[1L]] <- -2
    upper_ref[[1L]] <- 2
    lower_ref[-1L] <- lower_ref[[1L]] + cumsum(stats::rnorm(
      total - 1L, sd = sqrt(0.001)
    ))
    upper_ref[-1L] <- upper_ref[[1L]] + cumsum(stats::rnorm(
      total - 1L, sd = sqrt(0.001)
    ))
    separation <- upper_ref - lower_ref
    if (min(separation) <= row$minimum_root_separation[[1L]]) {
      stop("Independent root DGP violated its separation gate.",
           call. = FALSE)
    }
    denominator <-
      oracle_ref$upper_root - oracle_ref$lower_root
    scale <- separation / denominator
    mu <- (
      oracle_ref$upper_root * lower_ref -
        oracle_ref$lower_root * upper_ref
    ) / denominator
    latent <- list(
      reference_lower = lower_ref, reference_upper = upper_ref
    )
  } else {
    stop("The DGP constructor has no implementation for this ID.",
         call. = FALSE)
  }

  set.seed(error_seed)
  error <- rqr_main_error_draw(row$error_family[[1L]], total)
  y <- mu + scale * error
  oracle_spec <- rqr_main_oracle_family(row$error_family[[1L]])
  oracle <- rqr_oracle_roots(
    oracle_spec$family, coverage_level, params = oracle_spec$params
  )
  lower <- mu + scale * oracle$lower_root
  upper <- mu + scale * oracle$upper_root
  if (any(!is.finite(c(y, mu, scale, lower, upper))) ||
      any(scale <= 0) ||
      min(upper - lower) <= row$minimum_root_separation[[1L]]) {
    stop("The generated DGP failed finite, positivity, or separation gates.",
         call. = FALSE)
  }
  list(
    schema_version = "rqrgibbs_dlm_main_simulation_dgp_draw/1.0.0",
    dgp_id = dgp_id,
    replication = replication,
    coverage_level = coverage_level,
    T = T,
    H = H,
    y = y,
    training_y = y[seq_len(T)],
    future_y = y[T + seq_len(H)],
    mu = mu,
    scale = scale,
    realized_root_path = cbind(lower = lower, upper = upper),
    oracle_conditional_mean_root = cbind(
      lower = lower, upper = upper
    ),
    x = x,
    latent = latent,
    seeds = c(
      state = state_seed, error = error_seed, forecast = forecast_seed
    ),
    response_law_shared_across_coverages = TRUE,
    generalized_bayes_target = TRUE,
    response_prediction_contract = FALSE
  )
}

rqr_main_seed_ledger <- function(contract) {
  grid <- expand.grid(
    dgp_id = contract$config$design$primary_dgp_ids,
    coverage_level = contract$config$design$coverage_levels,
    replication = seq_len(
      contract$config$monte_carlo$
        diagnostic_pilot_replications_per_mechanism_coverage_method
    ),
    stream = contract$config$seed_contract$streams,
    stringsAsFactors = FALSE
  )
  grid$seed <- vapply(seq_len(nrow(grid)), function(index) {
    rqr_main_seed(
      contract$config, grid$stream[[index]],
      grid$dgp_id[[index]], grid$coverage_level[[index]],
      grid$replication[[index]]
    )
  }, integer(1L))
  if (anyDuplicated(grid[c(
        "dgp_id", "coverage_level", "replication", "stream"
      )]) || any(grid$seed <= 0L)) {
    stop("The diagnostic seed ledger is not unique and positive.",
         call. = FALSE)
  }
  grid
}

rqr_main_atomic_write_csv <- function(
    value, path, inject_failure = FALSE) {
  directory <- dirname(path)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    paste0(".", basename(path), "-"), tmpdir = directory
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, quote = TRUE)
  if (isTRUE(inject_failure)) {
    stop("Injected atomic-publication failure.", call. = FALSE)
  }
  if (file.exists(path)) {
    stop("Atomic publication refuses to overwrite an existing artifact.",
         call. = FALSE)
  }
  if (!file.rename(temporary, path)) {
    stop("Atomic artifact publication failed.", call. = FALSE)
  }
  invisible(path)
}

rqr_main_recursive_manifest <- function(directory) {
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  paths <- list.files(
    directory, recursive = TRUE, full.names = TRUE,
    all.files = TRUE, no.. = TRUE
  )
  paths <- paths[file.info(paths)$isdir %in% FALSE]
  relative <- substring(paths, nchar(directory) + 2L)
  order_index <- order(relative)
  paths <- paths[order_index]
  relative <- relative[order_index]
  data.frame(
    sha256 = vapply(paths, function(path) {
      digest::digest(
        file = path, algo = "sha256", serialize = FALSE
      )
    }, character(1L)),
    bytes = as.numeric(file.info(paths)$size),
    path = relative,
    stringsAsFactors = FALSE
  )
}

rqr_main_coverage_qualified <- function(
    coverage, standard_error, nominal, margin = 0.02,
    confidence_level = 0.90) {
  values <- c(coverage, standard_error, nominal, margin, confidence_level)
  if (any(!is.finite(values)) || standard_error < 0 || margin <= 0 ||
      confidence_level <= 0 || confidence_level >= 1) {
    stop("Coverage-equivalence inputs are invalid.", call. = FALSE)
  }
  z <- stats::qnorm(1 - (1 - confidence_level) / 2)
  interval <- coverage - nominal + c(-1, 1) * z * standard_error
  list(
    lower = interval[[1L]],
    upper = interval[[2L]],
    margin = margin,
    qualified = interval[[1L]] > -margin && interval[[2L]] < margin
  )
}

rqr_main_validate_exdqlm_adapter <- function(attestation) {
  if (!is.list(attestation) ||
      !identical(attestation$package, "exdqlm") ||
      !identical(attestation$version, "1.1.0") ||
      !dir.exists(attestation$runtime_path)) {
    stop("The exdqlm adapter requires its isolated 1.1.0 runtime.",
         call. = FALSE)
  }
  library_root <- dirname(attestation$runtime_path)
  if ("exdqlm" %in% loadedNamespaces()) {
    loaded_path <- normalizePath(
      getNamespaceInfo(asNamespace("exdqlm"), "path"),
      winslash = "/", mustWork = TRUE
    )
    if (!identical(
          loaded_path,
          normalizePath(
            attestation$runtime_path, winslash = "/", mustWork = TRUE
          )
        )) {
      stop("A different exdqlm namespace is already loaded.", call. = FALSE)
    }
  } else {
    loadNamespace("exdqlm", lib.loc = library_root)
  }
  namespace <- asNamespace("exdqlm")
  if (!identical(
        as.character(utils::packageVersion("exdqlm", lib.loc = library_root)),
        "1.1.0"
      )) {
    stop("The adapter loaded the wrong exdqlm version.", call. = FALSE)
  }
  mcmc <- get("exdqlmMCMC", envir = namespace)
  required_formals <- c(
    "y", "p0", "model", "df", "dim.df", "dqlm.ind",
    "n.burn", "n.mcmc", "init.from.vb"
  )
  if (!all(required_formals %in% names(formals(mcmc)))) {
    stop("The reduced AL/DQLM MCMC interface changed.", call. = FALSE)
  }
  polytrend <- get("polytrendMod", envir = namespace)
  regression <- get("regMod", envir = namespace)
  forecast <- get("exdqlmForecast", envir = namespace)
  trend <- polytrend(2L, m0 = c(0, 0), C0 = diag(2), backend = "R")
  reg <- regression(
    X = matrix(seq_len(6L), ncol = 1L),
    m0 = 0, C0 = matrix(1, 1L, 1L)
  )
  combined <- trend + reg
  if (!identical(dim(combined$FF), c(3L, 6L)) ||
      !identical(dim(combined$GG), c(3L, 3L))) {
    stop("The exdqlm component adapter has the wrong orientation.",
         call. = FALSE)
  }

  fake <- list(
    y = c(0, 0, 0),
    model = list(
      FF = matrix(1, 1L, 3L),
      GG = array(1, c(1L, 1L, 3L))
    ),
    p0 = 0.10,
    df = 0.90,
    dim.df = 1L,
    theta.out = list(
      fm = matrix(c(0, 1, 2), 1L, 3L),
      fC = array(rep(0.5, 3L), c(1L, 1L, 3L))
    ),
    dqlm.ind = TRUE,
    samp.sigma = rep(1, 4L)
  )
  class(fake) <- c("exdqlmMCMC", "exdqlmFit")
  future <- forecast(
    start.t = 3L, k = 2L, m1 = fake,
    fFF = matrix(c(1, 2), 1L, 2L),
    fGG = matrix(1, 1L, 1L),
    plot = FALSE, return.draws = FALSE
  )
  if (!identical(dim(future$fa), c(1L, 2L)) ||
      !identical(length(future$ff), 2L) ||
      !isTRUE(all.equal(as.numeric(future$ff), c(2, 4),
                        tolerance = 1e-14)) ||
      !isTRUE(fake$dqlm.ind)) {
    stop("The exdqlm forecast adapter has the wrong horizon orientation.",
         call. = FALSE)
  }
  data.frame(
    package = "exdqlm",
    version = "1.1.0",
    reduced_AL_flag = "dqlm.ind=TRUE",
    warm_start = "init.from.vb=FALSE",
    combined_state_dimension = nrow(combined$FF),
    combined_time_dimension = ncol(combined$FF),
    forecast_horizon = length(future$ff),
    forecast_orientation = "horizon_vector",
    raw_quantile_forecasts_retained = TRUE,
    response_predictive_draws_used = FALSE,
    protected_checkout_used = FALSE,
    pass = TRUE,
    stringsAsFactors = FALSE
  )
}

rqr_main_validate_quantreg_adapter <- function(attestation) {
  if (!identical(attestation$schema_version,
                 "rqrgibbs_external_cran_runtime/1.0.0") ||
      !identical(attestation$package, "quantreg") ||
      !identical(attestation$version, "6.1")) {
    stop("The quantreg adapter requires its isolated 6.1 runtime.",
         call. = FALSE)
  }
  library_root <- dirname(attestation$runtime_path)
  if ("quantreg" %in% loadedNamespaces()) {
    loaded_path <- normalizePath(
      getNamespaceInfo(asNamespace("quantreg"), "path"),
      winslash = "/", mustWork = TRUE
    )
    expected_path <- normalizePath(
      attestation$runtime_path, winslash = "/", mustWork = TRUE
    )
    if (!identical(loaded_path, expected_path)) {
      stop("A different quantreg namespace is already loaded.",
           call. = FALSE)
    }
  } else {
    loadNamespace("quantreg", lib.loc = library_root)
  }
  namespace <- asNamespace("quantreg")
  if (!identical(
        as.character(utils::packageVersion(
          "quantreg", lib.loc = library_root
        )),
        "6.1"
      )) {
    stop("The adapter loaded the wrong quantreg version.", call. = FALSE)
  }
  rq <- get("rq", envir = namespace)
  if (!all(c("formula", "tau", "data", "method") %in% names(formals(rq)))) {
    stop("The pinned quantreg rq interface is incomplete.", call. = FALSE)
  }
  fixture <- data.frame(
    x = -4:4,
    y = c(-7.8, -5.1, -3.2, -0.7, 1.3, 2.8, 5.4, 6.6, 9.5)
  )
  fit <- rq(y ~ x, tau = c(0.1, 0.9), data = fixture, method = "br")
  prediction <- as.matrix(stats::predict(
    fit, newdata = data.frame(x = c(-1, 1))
  ))
  if (!identical(dim(prediction), c(2L, 2L)) ||
      max(abs(
        prediction -
          matrix(c(-1.62857142857143, 2.48571428571429, -0.7, 3.38),
                 2L, 2L)
      )) >
        1e-8) {
    stop("The quantreg endpoint adapter has the wrong orientation.",
         call. = FALSE)
  }
  data.frame(
    package = "quantreg",
    version = "6.1",
    fitting_function = "rq",
    method = "br",
    raw_quantiles_retained = TRUE,
    ordering_applied_only_for_interval_scoring = TRUE,
    response_predictive_draws_used = FALSE,
    adapter_pass = TRUE,
    stringsAsFactors = FALSE
  )
}

rqr_main_allow_detached_launch_source <- function() {
  identical(Sys.getenv("RQR_ALLOW_DETACHED_LAUNCH_SOURCE", unset = ""),
            "TRUE")
}

rqr_main_source_branch_allowed <- function(branch, allow_detached = NULL) {
  allow_detached <- allow_detached %||%
    rqr_main_allow_detached_launch_source()
  identical(branch, "main") ||
    (isTRUE(allow_detached) && identical(branch, "HEAD"))
}

rqr_main_source_branch_contract <- function(branch, allow_detached = NULL) {
  allow_detached <- allow_detached %||%
    rqr_main_allow_detached_launch_source()
  if (identical(branch, "main")) return("clean_main")
  if (isTRUE(allow_detached) && identical(branch, "HEAD")) {
    return("clean_detached_exact_launch_source")
  }
  "invalid"
}

rqr_main_primary_runtime_binding <- function(
    repo_root, expected_commit, attestation_path,
    allow_detached_launch_source = rqr_main_allow_detached_launch_source()) {
  expected_commit <- tolower(as.character(expected_commit)[1L])
  if (!grepl("^[0-9a-f]{40}$", expected_commit)) {
    stop("A promotion-grade primary commit must be a full SHA.",
         call. = FALSE)
  }
  git <- Sys.which("git")
  git_read <- function(arguments) {
    out <- suppressWarnings(system2(
      git, c("-C", shQuote(repo_root), arguments),
      stdout = TRUE, stderr = TRUE,
      env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
    ))
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    if (!identical(as.integer(status), 0L)) {
      stop("Could not verify the primary Git state.", call. = FALSE)
    }
    trimws(paste(out, collapse = "\n"))
  }
  current_commit <- tolower(git_read(c("rev-parse", "HEAD")))
  branch <- git_read(c("rev-parse", "--abbrev-ref", "HEAD"))
  status <- git_read(c(
    "status", "--porcelain=v2", "--untracked-files=all"
  ))
  if (!identical(current_commit, expected_commit) ||
      !rqr_main_source_branch_allowed(
        branch, allow_detached_launch_source
      ) ||
      nzchar(status)) {
    stop(
      paste(
        "Promotion-grade references require clean main at the",
        "expected SHA, or an explicitly authorized clean detached",
        "launch-source worktree at that SHA."
      ),
      call. = FALSE
    )
  }
  if (!file.exists(attestation_path)) {
    stop("The primary runtime attestation is missing.", call. = FALSE)
  }
  attestation_path <- normalizePath(
    attestation_path, winslash = "/", mustWork = TRUE
  )
  attestation <- readRDS(attestation_path)
  executing_path <- normalizePath(
    getNamespaceInfo(asNamespace("rqrgibbs"), "path"),
    winslash = "/", mustWork = TRUE
  )
  expected_tree <- tolower(git_read(c(
    "rev-parse", paste0(expected_commit, ":application")
  )))
  checks <- c(
    identical(
      attestation$schema_version,
      "rqrgibbs_runtime_attestation/5.0.0"
    ),
    identical(attestation$source_commit, expected_commit),
    identical(attestation$source_tree_digest, expected_tree),
    isTRUE(attestation$source_checkout_unchanged),
    isTRUE(attestation$source_archive_tree_match),
    isTRUE(attestation$source_package_archive_match),
    isTRUE(attestation$runtime_isolated_from_source),
    identical(
      normalizePath(
        attestation$runtime_package_path,
        winslash = "/", mustWork = TRUE
      ),
      executing_path
    ),
    identical(
      rqr_directory_digest(executing_path),
      attestation$runtime_package_tree_digest
    )
  )
  if (!all(checks)) {
    stop("The executing primary runtime is not bound to the expected source.",
         call. = FALSE)
  }
  list(
    expected_commit = expected_commit,
    application_tree = expected_tree,
    runtime_path = executing_path,
    runtime_tree_digest = attestation$runtime_package_tree_digest,
    runtime_attestation_schema = attestation$schema_version,
    runtime_attestation_sha256 = digest::digest(
      file = attestation_path, algo = "sha256", serialize = FALSE
    ),
    package_version = as.character(utils::packageVersion("rqrgibbs")),
    R_version = R.version.string,
    platform = R.version$platform,
    source_branch = branch,
    source_branch_contract = rqr_main_source_branch_contract(
      branch, allow_detached_launch_source
    ),
    match = TRUE
  )
}
