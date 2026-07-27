.rqr_semantic_boundary_cache <- new.env(parent = emptyenv())

.rqr_semantic_boundary_clone <- function(object) {
  unserialize(serialize(object, NULL))
}

.rqr_semantic_boundary_model <- function() {
  rqr_polytrend(
    order = 1L,
    m0 = 0,
    C0 = matrix(2, 1L, 1L),
    name = "level"
  )
}

.rqr_semantic_boundary_y <- function() {
  c(-0.65, -0.20, 0.10, 0.55)
}

.rqr_semantic_boundary_fixed_fit <- function() {
  if (!exists("fixed_dlm", .rqr_semantic_boundary_cache, inherits = FALSE)) {
    fit <- rqr_dlm_fit(
      y = .rqr_semantic_boundary_y(),
      model = .rqr_semantic_boundary_model(),
      coverage_level = 0.8,
      evolution_spec = rqr_evolution_fixed(matrix(0.04, 1L, 1L)),
      learning_rate_mode = "fixed_rate",
      numerical_policy = "fail",
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L, thin = 1L,
        seed = 27101L, backend = "R",
        store_state_draws = TRUE
      )
    )
    assign(
      "fixed_dlm", fit, .rqr_semantic_boundary_cache
    )
  }
  get("fixed_dlm", .rqr_semantic_boundary_cache, inherits = FALSE)
}

.rqr_semantic_boundary_static_fit <- function() {
  if (!exists("fixed_design", .rqr_semantic_boundary_cache, inherits = FALSE)) {
    y <- .rqr_semantic_boundary_y()
    X <- cbind(intercept = 1, time = seq_along(y))
    fit <- rqr_mcmc_fit(
      y = y,
      X = X,
      coverage_level = 0.8,
      learning_rate_mode = "fixed_rate",
      numerical_policy = "fail",
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L, thin = 1L,
        seed = 27102L
      )
    )
    assign(
      "fixed_design", fit, .rqr_semantic_boundary_cache
    )
  }
  get("fixed_design", .rqr_semantic_boundary_cache, inherits = FALSE)
}

.rqr_semantic_boundary_specs <- function() {
  model <- .rqr_semantic_boundary_model()
  list(
    fixed_W = rqr_evolution_fixed(matrix(0.04, 1L, 1L)),
    discount_template = rqr_freeze_discount_template(
      model = model,
      n_time = 4L,
      df = 0.90,
      dim.df = 1L,
      reference_variance = 0.5,
      numerical_policy = "fail"
    ),
    component_scale = rqr_evolution_component_scale(
      templates = list(matrix(1, 1L, 1L)),
      component_dims = 1L,
      component_names = "level",
      prior = list(shape = 2, rate = 1),
      initial = 0.04
    ),
    adaptive_discount = rqr_evolution_adaptive_working(
      df = 0.90,
      component_dims = 1L
    )
  )
}

.rqr_semantic_boundary_expect_bad_evolution <- function(
    evolution, expanded, y, pattern = "evolution|Evolution|discount") {
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_evolution_spec(
      evolution, expanded, y = y
    ),
    pattern
  )
}

test_that("all four evolution constructors have canonical exact schemas", {
  specs <- .rqr_semantic_boundary_specs()
  expected_fields <- list(
    fixed_W = c(
      "schema_version", "mode", "W", "exact_joint_target",
      "frozen_before_mcmc"
    ),
    discount_template = c(
      "schema_version", "mode", "W", "df", "dim.df", "D",
      "reference_variance", "reference_design",
      "reference_variance_source", "reference_design_source",
      "empirical_bayes", "exact_joint_target",
      "frozen_before_mcmc", "construction_contract",
      "construction_audit"
    ),
    component_scale = c(
      "schema_version", "mode", "templates", "component_dims",
      "component_names", "prior", "initial", "exact_joint_target",
      "frozen_before_mcmc", "shared_across_roots"
    ),
    adaptive_discount = c(
      "schema_version", "mode", "df", "dim.df", "D",
      "exact_joint_target", "frozen_before_mcmc",
      "working_sequential"
    )
  )
  expected_flags <- list(
    fixed_W = c(exact = TRUE, frozen = TRUE),
    discount_template = c(exact = TRUE, frozen = TRUE),
    component_scale = c(
      exact = TRUE, frozen = TRUE, shared = TRUE
    ),
    adaptive_discount = c(
      exact = FALSE, frozen = FALSE, working = TRUE
    )
  )
  expanded <- rqrgibbs:::.rqr_expand_model(
    .rqr_semantic_boundary_model(), 4L
  )

  expect_identical(names(specs), names(expected_fields))
  for (mode in names(specs)) {
    spec <- specs[[mode]]
    expect_identical(class(spec), "rqr_evolution", info = mode)
    expect_identical(names(spec), expected_fields[[mode]], info = mode)
    expect_identical(
      spec$schema_version,
      "rqrgibbs_dlm_evolution/1.0.0",
      info = mode
    )
    expect_identical(spec$mode, mode, info = mode)
    expect_identical(
      spec$exact_joint_target,
      unname(expected_flags[[mode]][["exact"]]),
      info = mode
    )
    expect_identical(
      spec$frozen_before_mcmc,
      unname(expected_flags[[mode]][["frozen"]]),
      info = mode
    )
    if ("shared" %in% names(expected_flags[[mode]])) {
      expect_identical(
        spec$shared_across_roots,
        unname(expected_flags[[mode]][["shared"]]),
        info = mode
      )
    }
    if ("working" %in% names(expected_flags[[mode]])) {
      expect_identical(
        spec$working_sequential,
        unname(expected_flags[[mode]][["working"]]),
        info = mode
      )
    }
    expect_invisible(
      rqrgibbs:::.rqr_validate_dlm_evolution_spec(
        spec, expanded, y = .rqr_semantic_boundary_y()
      )
    )
  }
})

test_that("evolution semantic metadata and exact field order fail closed", {
  specs <- .rqr_semantic_boundary_specs()
  expanded <- rqrgibbs:::.rqr_expand_model(
    .rqr_semantic_boundary_model(), 4L
  )
  y <- .rqr_semantic_boundary_y()
  mutations <- list(
    fixed_exact = list("fixed_W", "exact_joint_target", FALSE),
    fixed_frozen = list("fixed_W", "frozen_before_mcmc", FALSE),
    discount_exact = list(
      "discount_template", "exact_joint_target", FALSE
    ),
    discount_frozen = list(
      "discount_template", "frozen_before_mcmc", FALSE
    ),
    component_exact = list(
      "component_scale", "exact_joint_target", FALSE
    ),
    component_frozen = list(
      "component_scale", "frozen_before_mcmc", FALSE
    ),
    component_shared = list(
      "component_scale", "shared_across_roots", FALSE
    ),
    adaptive_exact = list(
      "adaptive_discount", "exact_joint_target", TRUE
    ),
    adaptive_frozen = list(
      "adaptive_discount", "frozen_before_mcmc", TRUE
    ),
    adaptive_working = list(
      "adaptive_discount", "working_sequential", FALSE
    )
  )

  for (label in names(mutations)) {
    mutation <- mutations[[label]]
    bad <- .rqr_semantic_boundary_clone(specs[[mutation[[1L]]]])
    bad[[mutation[[2L]]]] <- mutation[[3L]]
    .rqr_semantic_boundary_expect_bad_evolution(
      bad, expanded, y, "canonical|reconstruct|exactness|working"
    )
  }

  for (mode in names(specs)) {
    bad_schema <- .rqr_semantic_boundary_clone(specs[[mode]])
    bad_schema$schema_version <- "rqrgibbs_dlm_evolution/999.0.0"
    .rqr_semantic_boundary_expect_bad_evolution(
      bad_schema, expanded, y, "exact field schema"
    )

    bad_extra <- .rqr_semantic_boundary_clone(specs[[mode]])
    bad_extra$unreviewed_claim <- TRUE
    .rqr_semantic_boundary_expect_bad_evolution(
      bad_extra, expanded, y, "exact field schema"
    )

    bad_order <- .rqr_semantic_boundary_clone(specs[[mode]])
    bad_order <- bad_order[
      c(2L, 1L, seq.int(3L, length(bad_order)))
    ]
    class(bad_order) <- "rqr_evolution"
    .rqr_semantic_boundary_expect_bad_evolution(
      bad_order, expanded, y, "exact field schema"
    )
  }
})

test_that("adaptive working evolution cannot be forged into an exact fresh fit", {
  y <- .rqr_semantic_boundary_y()
  model <- .rqr_semantic_boundary_model()
  working <- rqr_evolution_adaptive_working(0.90, 1L)
  fit <- suppressWarnings(rqr_dlm_fit(
    y = y,
    model = model,
    coverage_level = 0.8,
    evolution_spec = working,
    learning_rate_mode = "fixed_rate",
    numerical_policy = "fail",
    mcmc_control = list(
      n_burn = 0L, n_mcmc = 1L, seed = 27103L, backend = "R"
    )
  ))

  expect_false(fit$model_spec$exact_joint_target)
  expect_identical(fit$model_spec$target_contract, "working_sequential")
  expect_false(fit$model_spec$ordinary_v1_scope_eligible)
  expect_false(fit$model_spec$continuation_supported)
  expect_false(fit$model_spec$promotion_eligible)

  forged <- .rqr_semantic_boundary_clone(working)
  forged$exact_joint_target <- TRUE
  expect_error(
    rqr_dlm_fit(
      y = y,
      model = model,
      coverage_level = 0.8,
      evolution_spec = forged,
      learning_rate_mode = "fixed_rate",
      numerical_policy = "fail",
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L,
        seed = 27104L, backend = "R"
      )
    ),
    "exactness|canonical"
  )
})

test_that("a public fixed-W matrix spec is stored in canonical fitted form", {
  fit <- .rqr_semantic_boundary_fixed_fit()
  T <- length(.rqr_semantic_boundary_y())
  expanded <- rqrgibbs:::.rqr_expand_model(
    .rqr_semantic_boundary_model(), T
  )

  expect_identical(fit$evolution$mode, "fixed_W")
  expect_identical(dim(fit$evolution$W), c(1L, 1L, T))
  expect_identical(
    as.numeric(fit$evolution$W),
    rep(0.04, T)
  )
  expect_identical(
    names(fit$evolution),
    rqrgibbs:::.rqr_dlm_evolution_field_schema("fixed_W")
  )
  expect_invisible(
    rqrgibbs:::.rqr_validate_dlm_evolution_spec(
      fit$evolution, expanded, y = fit$y
    )
  )
})

test_that("discount-template sources, design, audit, W, and recursion are bound", {
  specs <- .rqr_semantic_boundary_specs()
  template <- specs$discount_template
  expanded <- rqrgibbs:::.rqr_expand_model(
    .rqr_semantic_boundary_model(), 4L
  )
  y <- .rqr_semantic_boundary_y()

  mutations <- list(
    variance_source = function(x) {
      x$reference_variance_source <- "training_response_variance"
      x
    },
    design_source = function(x) {
      x$reference_design_source <- "user_supplied"
      x
    },
    model_design = function(x) {
      x$reference_design[1L, 2L] <-
        x$reference_design[1L, 2L] + 0.25
      x
    },
    audit_count = function(x) {
      x$construction_audit$repair_count <- 1L
      x
    },
    audit_eigenvalue = function(x) {
      x$construction_audit$minimum_eigenvalue[2L] <-
        x$construction_audit$minimum_eigenvalue[2L] + 0.01
      x
    },
    frozen_W = function(x) {
      x$W[1L, 1L, 2L] <- x$W[1L, 1L, 2L] + 0.01
      x
    },
    reconstruction_algorithm = function(x) {
      x$construction_contract$algorithm <- "unreviewed_recursion"
      x
    },
    reconstruction_source = function(x) {
      x$construction_contract$reference_design_source <-
        "user_supplied"
      x
    }
  )

  for (label in names(mutations)) {
    bad <- mutations[[label]](
      .rqr_semantic_boundary_clone(template)
    )
    .rqr_semantic_boundary_expect_bad_evolution(
      bad, expanded, y,
      "source|design|audit|ledger|generated|reconstruct|contract|metadata"
    )
  }
})

test_that("DLM model interpretation, scope, and target claims are reconstructed", {
  fit <- .rqr_semantic_boundary_fixed_fit()
  mutations <- list(
    parameterization = function(x) {
      x$model_spec$parameterization <- "response_likelihood_dlm"
      x
    },
    inferential_target = function(x) {
      x$model_spec$inferential_target <- "posterior predictive density"
      x
    },
    generalized_bayes = function(x) {
      x$model_spec$generalized_bayes <- FALSE
      x
    },
    response_likelihood = function(x) {
      x$model_spec$response_likelihood <- TRUE
      x
    },
    response_prediction = function(x) {
      x$model_spec$response_prediction_contract <- TRUE
      x
    },
    ordinary_scope = function(x) {
      x$model_spec$ordinary_v1_scope_eligible <- FALSE
      x
    },
    continuation_scope = function(x) {
      x$model_spec$continuation_supported <- FALSE
      x
    },
    target_contract = function(x) {
      x$model_spec$target_contract <- "working_sequential"
      x
    },
    exact_target = function(x) {
      x$model_spec$exact_joint_target <- FALSE
      x
    }
  )

  for (label in names(mutations)) {
    bad <- mutations[[label]](
      .rqr_semantic_boundary_clone(fit)
    )
    expect_error(
      rqr_posterior_draws(bad),
      "model specification conflicts",
      info = label
    )
  }
})

test_that("static model interpretation and scope claims are reconstructed", {
  fit <- .rqr_semantic_boundary_static_fit()
  mutations <- list(
    parameterization = function(x) {
      x$model_spec$parameterization <- "ordinary_response_regression"
      x
    },
    tilt = function(x) {
      x$model_spec$tilt <- 0.1
      x
    },
    inferential_target = function(x) {
      x$model_spec$inferential_target <- "response posterior"
      x
    },
    generalized_bayes = function(x) {
      x$model_spec$generalized_bayes <- FALSE
      x
    },
    response_likelihood = function(x) {
      x$model_spec$response_likelihood <- TRUE
      x
    },
    response_prediction = function(x) {
      x$model_spec$response_prediction_contract <- TRUE
      x
    },
    ordinary_scope = function(x) {
      x$model_spec$ordinary_v1_scope_eligible <- FALSE
      x
    },
    continuation_scope = function(x) {
      x$model_spec$continuation_supported <- FALSE
      x
    },
    target_contract = function(x) {
      x$model_spec$target_contract <- "response_likelihood"
      x
    }
  )

  for (label in names(mutations)) {
    bad <- mutations[[label]](
      .rqr_semantic_boundary_clone(fit)
    )
    expect_error(
      rqr_posterior_draws(bad),
      "model specification conflicts",
      info = label
    )
  }
})

test_that("public fitted-time DLM operations reject partial pseudo-fits", {
  partial <- structure(
    list(
      method = "mcmc_ffbs",
      family = "rqr_dlm",
      samp.eta_root1 = matrix(0, 2L, 1L),
      samp.eta_root2 = matrix(1, 2L, 1L),
      samp.lambda = 1,
      y = c(0, 1),
      model_spec = list(learning_rate_mode = "fixed_rate"),
      misc = list(
        thin = 1L,
        store_state_draws = FALSE,
        store_latent_draws = FALSE
      )
    ),
    class = c("rqr_dlm_mcmc", "rqr_fit")
  )

  expect_error(
    rqr_posterior_draws(partial),
    "fitted object requires schema|Expected an rqr_dlm_mcmc"
  )
  expect_error(
    predict_interval(partial),
    "fitted object requires schema|Expected an rqr_dlm_mcmc"
  )
  expect_error(
    print(partial),
    "fitted object requires schema|Expected an rqr_dlm_mcmc"
  )
  expect_error(
    rqr_dlm_continue(
      partial, n_mcmc = 1L, thin = 1L,
      store_state_draws = FALSE,
      store_latent_draws = FALSE
    ),
    "fitted object requires schema|Expected an rqr_dlm_mcmc"
  )
})

test_that("external forecast state fixtures remain unbound and cannot mimic fits", {
  state_only <- structure(
    list(
      samp.theta_terminal_root1 = matrix(c(-0.4, -0.2), 1L, 2L),
      samp.theta_terminal_root2 = matrix(c(0.5, 0.7), 1L, 2L),
      expanded_model = list(p = 1L),
      model_spec = list(
        evolution_mode = "fixed_W",
        numerical_policy = "fail"
      ),
      misc = list(jitter_ladder = 0)
    ),
    class = c("rqr_dlm_mcmc", "rqr_fit")
  )
  args <- list(
    FF_future = matrix(1, 1L, 1L),
    GG_future = matrix(1, 1L, 1L),
    W_future = matrix(0.01, 1L, 1L),
    seed = 27105L
  )
  forecast <- do.call(
    rqr_forecast_roots, c(list(object = state_only), args)
  )
  expect_identical(
    forecast$draw_binding_status,
    "unbound_external_state_fixture"
  )
  expect_identical(
    forecast$diagnostics$draw_binding_status,
    "unbound_external_state_fixture"
  )

  pseudo_fields <- list(
    samp.eta_root1 = matrix(0, 1L, 2L),
    samp.eta_root2 = matrix(1, 1L, 2L),
    samp.lambda = c(1, 1),
    y = c(0)
  )
  for (field in names(pseudo_fields)) {
    bad <- .rqr_semantic_boundary_clone(state_only)
    bad[[field]] <- pseudo_fields[[field]]
    expect_error(
      do.call(rqr_forecast_roots, c(list(object = bad), args)),
      "external forecast-state fixture|pseudo-fitted",
      info = field
    )
  }
})

test_that("stored model and evolution mutations are rejected before continuation", {
  fit <- .rqr_semantic_boundary_fixed_fit()
  continuation_args <- list(
    n_mcmc = 1L,
    thin = 1L,
    store_state_draws = TRUE,
    store_latent_draws = FALSE
  )

  bad_evolution <- .rqr_semantic_boundary_clone(fit)
  bad_evolution$evolution$W[1L, 1L, 2L] <-
    bad_evolution$evolution$W[1L, 1L, 2L] + 0.01
  expect_error(
    do.call(
      rqr_dlm_continue,
      c(list(object = bad_evolution), continuation_args)
    ),
    "matrix digest|evolution digest|model, target, or evolution"
  )

  bad_model <- .rqr_semantic_boundary_clone(fit)
  bad_model$model$FF[1L, 1L] <- bad_model$model$FF[1L, 1L] + 0.25
  expect_error(
    do.call(
      rqr_dlm_continue,
      c(list(object = bad_model), continuation_args)
    ),
    "matrix digest|model, target, or evolution|canonical reconstruction"
  )
})

test_that("classed lambda priors cannot rewrite the generalized target", {
  forged <- structure(
    list(
      shape = 4, rate = 4, power = 99,
      mode = "learned_pseudoresidual_normalized"
    ),
    class = c("rqr_lambda_prior", "list")
  )
  y <- .rqr_semantic_boundary_y()
  X <- cbind(intercept = 1, time = seq_along(y))
  expect_error(
    rqr_mcmc_fit(
      y = y, X = X, coverage_level = 0.8,
      learning_rate_mode =
        "learned_pseudoresidual_normalized",
      lambda_prior = forged,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L, seed = 27106L
      )
    ),
    "mode-derived field and power contract"
  )
  expect_error(
    rqr_dlm_fit(
      y = y, model = .rqr_semantic_boundary_model(),
      coverage_level = 0.8,
      evolution_spec = rqr_evolution_fixed(
        matrix(0.04, 1L, 1L)
      ),
      learning_rate_mode =
        "learned_pseudoresidual_normalized",
      lambda_prior = forged,
      mcmc_control = list(
        n_burn = 0L, n_mcmc = 1L,
        seed = 27107L, backend = "R"
      )
    ),
    "mode-derived field and power contract"
  )

  canonical <- rqrgibbs:::.rqr_lambda_prior(
    list(shape = 4, rate = 4),
    "learned_pseudoresidual_normalized"
  )
  bad_shape <- canonical
  bad_shape$shape <- -1
  expect_error(
    rqrgibbs:::.rqr_lambda_prior(
      bad_shape, "learned_pseudoresidual_normalized"
    ),
    "must lie in"
  )
  bad_extra <- canonical
  bad_extra$unreviewed <- TRUE
  expect_error(
    rqrgibbs:::.rqr_lambda_prior(
      bad_extra, "learned_pseudoresidual_normalized"
    ),
    "mode-derived field and power contract"
  )
})

test_that("stored coefficient-prior semantics are exact and canonical", {
  fit <- .rqr_semantic_boundary_static_fit()
  mutations <- list(
    stateful = function(x) {
      x$beta_prior$stateful <- TRUE
      x
    },
    implementation = function(x) {
      x$beta_prior$implementation <- "external"
      x
    },
    extra = function(x) {
      x$beta_prior$unreviewed <- 123
      x
    },
    design_contract = function(x) {
      x$beta_prior$design_contract$intercept_name <- "fake"
      x
    }
  )
  for (label in names(mutations)) {
    altered <- mutations[[label]](
      .rqr_semantic_boundary_clone(fit)
    )
    expect_error(
      rqrgibbs:::.rqr_validate_static_fit_envelope(altered),
      "coefficient prior|field and semantic|canonical",
      info = label
    )
  }
})

test_that("segment initialization is explicit, semantic, and parent-bound", {
  static <- .rqr_semantic_boundary_static_fit()
  expect_identical(
    static$initialization_contract$schema_version,
    "rqrgibbs_segment_initialization/1.0.0"
  )
  expect_identical(
    static$initialization_contract$rng_source, "explicit_seed"
  )
  expect_true(
    static$initialization_contract$reproducibility_bound
  )
  expect_identical(
    static$misc$seed, static$initialization_contract$seed
  )

  static_child <- rqr_mcmc_continue(
    static, n_mcmc = 1L, allow_environment_mismatch = TRUE
  )
  expect_identical(
    static_child$initialization_contract$rng_source,
    "parent_checkpoint_rng"
  )
  expect_identical(
    static_child$initialization_contract$parent_checkpoint_digest,
    static$checkpoint_digest
  )

  dlm <- .rqr_semantic_boundary_fixed_fit()
  dlm_child <- rqr_dlm_continue(
    dlm, n_mcmc = 1L, store_state_draws = TRUE,
    allow_environment_mismatch = TRUE
  )
  expect_identical(
    dlm_child$initialization_contract$rng_source,
    "parent_checkpoint_rng"
  )
  expect_identical(
    dlm_child$initialization_contract$parent_checkpoint_digest,
    dlm$checkpoint_digest
  )

  ambient_static <- rqr_mcmc_fit(
    y = .rqr_semantic_boundary_y(),
    X = cbind(
      intercept = 1,
      time = seq_along(.rqr_semantic_boundary_y())
    ),
    coverage_level = 0.8,
    mcmc_control = list(n_burn = 0L, n_mcmc = 1L)
  )
  expect_identical(
    ambient_static$initialization_contract$rng_source,
    "ambient_unbound"
  )
  expect_false(
    ambient_static$provenance$rng_initialization_bound
  )
  expect_false(ambient_static$model_spec$reproducibility_eligible)
  expect_false(ambient_static$model_spec$promotion_eligible)

  bad_seed <- .rqr_semantic_boundary_clone(static)
  bad_seed$misc$seed <- bad_seed$misc$seed + 1L
  expect_error(
    rqrgibbs:::.rqr_validate_static_fit_envelope(bad_seed),
    "initialization contract"
  )
  bad_initial <- .rqr_semantic_boundary_clone(dlm)
  bad_initial$initialization_contract$initial_state$theta_root1 <-
    bad_initial$initialization_contract$initial_state$
      theta_root1[-1L, , drop = FALSE]
  bad_initial$initialization_contract$initial_state_digest <-
    digest::digest(
      bad_initial$initialization_contract$initial_state,
      algo = "sha256", serialize = TRUE
    )
  bad_initial$initialization_digest <- digest::digest(
    bad_initial$initialization_contract,
    algo = "sha256", serialize = TRUE
  )
  bad_initial$provenance$initialization_contract_digest <-
    bad_initial$initialization_digest
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_fit_envelope(bad_initial),
    "initialization state|segment schedule"
  )
})

test_that("retained draws and execution evidence are content-bound", {
  static <- .rqr_semantic_boundary_static_fit()
  dlm <- .rqr_semantic_boundary_fixed_fit()
  expect_identical(
    static$retained_draws_contract$schema_version,
    "rqrgibbs_retained_draws/1.0.0"
  )
  expect_identical(
    dlm$retained_evidence_contract$schema_version,
    "rqrgibbs_retained_evidence/1.0.0"
  )

  static_draw <- .rqr_semantic_boundary_clone(static)
  static_draw$samp.beta_root1[1L, 1L] <-
    static_draw$samp.beta_root1[1L, 1L] + 1
  expect_error(
    rqrgibbs:::.rqr_validate_static_fit_envelope(static_draw),
    "retained draws"
  )
  static_summary <- .rqr_semantic_boundary_clone(static)
  static_summary$summary$width_mean[1L] <- 999
  expect_error(
    rqrgibbs:::.rqr_validate_static_fit_envelope(static_summary),
    "retained evidence"
  )
  static_diagnostic <- .rqr_semantic_boundary_clone(static)
  static_diagnostic$diagnostics$loss_trace[1L] <- 999
  expect_error(
    rqrgibbs:::.rqr_validate_static_fit_envelope(
      static_diagnostic
    ),
    "retained evidence"
  )
  dlm_draw <- .rqr_semantic_boundary_clone(dlm)
  dlm_draw$samp.eta_root1[1L, 1L] <-
    dlm_draw$samp.eta_root1[1L, 1L] + 1
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_fit_envelope(dlm_draw),
    "retained draws"
  )
  dlm_summary <- .rqr_semantic_boundary_clone(dlm)
  dlm_summary$model_spec$lambda_summary$mean <- 999
  expect_error(
    rqrgibbs:::.rqr_validate_dlm_fit_envelope(dlm_summary),
    "retained evidence"
  )
})

test_that("nested history, continuation, and DLM checkpoint atoms are plain", {
  rebind_checkpoint <- function(object, mutate) {
    checkpoint <- object$checkpoint_state
    checkpoint <- mutate(checkpoint)
    checkpoint_digest <- rqrgibbs:::.rqr_digest(checkpoint)
    object$checkpoint_state <- checkpoint
    object$checkpoint_digest <- checkpoint_digest
    object$last <- checkpoint
    terminal <- length(object$segment_schedule_contract$segments)
    object$segment_schedule_contract$segments[[terminal]]$
      checkpoint_state <- checkpoint
    object$segment_schedule_contract$segments[[terminal]]$
      checkpoint_digest <- checkpoint_digest
    object$segment_schedule_digest <- rqrgibbs:::.rqr_digest(
      object$segment_schedule_contract
    )
    history_terminal <-
      length(object$continuation_history_contract$segments)
    object$continuation_history_contract$
      segments[[history_terminal]]$checkpoint_digest <-
      checkpoint_digest
    object$continuation_history_digest <- rqrgibbs:::.rqr_digest(
      object$continuation_history_contract
    )
    object
  }

  dlm <- .rqr_semantic_boundary_fixed_fit()
  for (field in c(
      "theta_root1", "theta0_root1", "latent_v",
      "lambda", "evolution_scale", "rng_state"
    )) {
    bad <- rebind_checkpoint(dlm, function(checkpoint) {
      attr(checkpoint[[field]], "unreviewed") <- 1L
      checkpoint
    })
    expect_error(
      rqrgibbs:::.rqr_validate_dlm_fit_envelope(bad),
      "checkpoint|init|RNG|rng_state",
      info = field
    )
  }

  for (family in c("static", "dlm")) {
    fresh <- if (identical(family, "static")) {
      .rqr_semantic_boundary_static_fit()
    } else {
      .rqr_semantic_boundary_fixed_fit()
    }
    bad_history <- .rqr_semantic_boundary_clone(fresh)
    attr(
      bad_history$continuation_history_contract$segments[[1L]],
      "unreviewed"
    ) <- 1L
    bad_history$continuation_history_digest <-
      rqrgibbs:::.rqr_digest(
        bad_history$continuation_history_contract
      )
    validator <- if (identical(family, "static")) {
      rqrgibbs:::.rqr_validate_static_fit_envelope
    } else {
      rqrgibbs:::.rqr_validate_dlm_fit_envelope
    }
    expect_error(
      validator(bad_history),
      "Continuation history|segments",
      info = family
    )
  }

  static_child <- rqr_mcmc_continue(
    .rqr_semantic_boundary_static_fit(), n_mcmc = 1L,
    allow_environment_mismatch = TRUE
  )
  dlm_child <- rqr_dlm_continue(
    .rqr_semantic_boundary_fixed_fit(), n_mcmc = 1L,
    allow_environment_mismatch = TRUE
  )
  for (object in list(static_child, dlm_child)) {
    attr(object$continuation_contract, "unreviewed") <- 1L
    validator <- if (inherits(object, "rqr_mcmc")) {
      rqrgibbs:::.rqr_validate_static_fit_envelope
    } else {
      rqrgibbs:::.rqr_validate_dlm_fit_envelope
    }
    expect_error(
      validator(object),
      "continuation contract"
    )
  }
})

test_that("named public DLM state paths canonicalize to positional state", {
  y <- c(-0.4, 0.1, 0.6, 1.0)
  theta1 <- matrix(
    -0.5, 1L, length(y),
    dimnames = list("level", paste0("t", seq_along(y)))
  )
  theta2 <- matrix(
    1.2, 1L, length(y),
    dimnames = list("level", paste0("t", seq_along(y)))
  )
  fit <- rqr_dlm_fit(
    y = y,
    model = rqr_polytrend(1L, C0 = 2),
    coverage_level = 0.8,
    evolution_mode = "fixed_W",
    W = 0.02,
    init = list(
      state_root1 = theta1,
      state_root2 = theta2
    ),
    mcmc_control = list(
      n_burn = 0L, n_mcmc = 1L, seed = 92741L,
      backend = "R"
    )
  )
  expect_identical(
    names(attributes(
      fit$initialization_contract$initial_state$theta_root1
    )),
    "dim"
  )
  expect_null(dimnames(
    fit$initialization_contract$initial_state$theta_root1
  ))
  expect_silent(
    rqrgibbs:::.rqr_validate_dlm_fit_envelope(fit)
  )
})
