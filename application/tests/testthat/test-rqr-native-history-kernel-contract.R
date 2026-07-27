history_kernel_digest <- function(object) {
  rqrgibbs:::.rqr_digest(object)
}

history_kernel_sha <- function(label) {
  history_kernel_digest(list(label = label))
}

history_kernel_contract <- function() {
  list(
    schema_version = "rqrgibbs_dlm_transition_kernel/test-1.0.0",
    evolution_mode = "component_scale",
    scan_order = c(
      "latent_v_refresh", "component_scale_root1_collapsed",
      "root1_ffbs", "root2_ffbs", "global_root_swap"
    ),
    collapsed_slice_sweeps = 3L
  )
}

make_history_kernel_chain <- function(kernel = history_kernel_contract()) {
  target_digest <- history_kernel_sha("target")
  kernel_digest <- history_kernel_digest(kernel)
  checkpoint0 <- history_kernel_sha("checkpoint-0")
  checkpoint1 <- history_kernel_sha("checkpoint-1")
  history0 <- rqrgibbs:::.rqr_make_continuation_history(
    checkpoint_digest = checkpoint0,
    segment_numerical_repair_count = 0L,
    segment_exact_joint_target = TRUE,
    segment_environment_base_eligible = TRUE,
    segment_target_contract_digest = target_digest,
    backend_requested = "cpp",
    backend_resolved = "cpp",
    segment_transition_kernel_schema = kernel$schema_version,
    segment_transition_kernel_digest = kernel_digest
  )
  history1 <- rqrgibbs:::.rqr_make_continuation_history(
    checkpoint_digest = checkpoint1,
    segment_numerical_repair_count = 0L,
    segment_exact_joint_target = TRUE,
    segment_environment_base_eligible = TRUE,
    segment_target_contract_digest = target_digest,
    backend_requested = "cpp",
    backend_resolved = "cpp",
    parent = history0,
    parent_checkpoint_digest = checkpoint0,
    segment_transition_kernel_schema = kernel$schema_version,
    segment_transition_kernel_digest = kernel_digest
  )
  object <- list(
    continuation_history_contract = history1,
    continuation_history_digest = history_kernel_digest(history1),
    checkpoint_digest = checkpoint1,
    checkpoint_state = list(
      transition_kernel_schema = kernel$schema_version,
      transition_kernel = kernel,
      transition_kernel_digest = kernel_digest
    ),
    model_spec = list(
      cumulative_numerical_repair_count = 0L,
      chain_history_numerically_exact = TRUE,
      target_numerical_eligible = TRUE,
      promotion_eligible = TRUE,
      exact_joint_target = TRUE,
      transition_kernel = kernel,
      transition_kernel_digest = kernel_digest
    ),
    provenance = list(
      reproducibility_eligible = TRUE,
      backend_requested = "cpp",
      backend_resolved = "cpp",
      object_digests = list(target = target_digest)
    )
  )
  list(
    history0 = history0,
    history1 = history1,
    object = object,
    target_digest = target_digest,
    kernel = kernel,
    kernel_digest = kernel_digest,
    checkpoint0 = checkpoint0,
    checkpoint1 = checkpoint1
  )
}

test_that("shared fit and continuation-history schemas bind kernel identity", {
  expect_identical(
    rqrgibbs:::.rqr_schema_version(),
    "rqrgibbs_fit/1.14.0"
  )
  expect_identical(
    rqrgibbs:::.rqr_continuation_history_schema(),
    "rqrgibbs_continuation_history/5.0.0"
  )

  fixture <- make_history_kernel_chain()
  expect_silent(
    rqrgibbs:::.rqr_validate_continuation_history(fixture$object)
  )
  expect_identical(
    fixture$history1$transition_kernel_schema,
    fixture$kernel$schema_version
  )
  expect_identical(
    fixture$history1$transition_kernel_digest,
    fixture$kernel_digest
  )
  expect_true(all(vapply(
    fixture$history1$segments,
    function(segment) {
      identical(
        segment$segment_transition_kernel_schema,
        fixture$kernel$schema_version
      ) && identical(
        segment$segment_transition_kernel_digest,
        fixture$kernel_digest
      )
    },
    logical(1L)
  )))
})

test_that("history construction rejects a changed or implicit DLM kernel", {
  fixture <- make_history_kernel_chain()
  changed_kernel <- fixture$kernel
  changed_kernel$collapsed_slice_sweeps <- 4L
  expect_error(
    rqrgibbs:::.rqr_make_continuation_history(
      checkpoint_digest = history_kernel_sha("checkpoint-2"),
      segment_numerical_repair_count = 0L,
      segment_exact_joint_target = TRUE,
      segment_environment_base_eligible = TRUE,
      segment_target_contract_digest = fixture$target_digest,
      backend_requested = "cpp",
      backend_resolved = "cpp",
      parent = fixture$history1,
      parent_checkpoint_digest = fixture$checkpoint1,
      segment_transition_kernel_schema =
        changed_kernel$schema_version,
      segment_transition_kernel_digest =
        history_kernel_digest(changed_kernel)
    ),
    "cannot change its transition-kernel identity"
  )
  expect_error(
    rqrgibbs:::.rqr_make_continuation_history(
      checkpoint_digest = history_kernel_sha("implicit-dlm"),
      segment_numerical_repair_count = 0L,
      segment_exact_joint_target = TRUE,
      segment_environment_base_eligible = TRUE,
      segment_target_contract_digest = fixture$target_digest,
      backend_requested = "cpp",
      backend_resolved = "cpp"
    ),
    "must explicitly supply"
  )
  expect_error(
    rqrgibbs:::.rqr_make_continuation_history(
      checkpoint_digest = history_kernel_sha("half-kernel"),
      segment_numerical_repair_count = 0L,
      segment_exact_joint_target = TRUE,
      segment_environment_base_eligible = TRUE,
      segment_target_contract_digest = fixture$target_digest,
      backend_requested = "cpp",
      backend_resolved = "cpp",
      segment_transition_kernel_schema =
        fixture$kernel$schema_version
    ),
    "must be supplied together"
  )
  expect_error(
    rqrgibbs:::.rqr_make_continuation_history(
      checkpoint_digest = history_kernel_sha("vector-schema"),
      segment_numerical_repair_count = 0L,
      segment_exact_joint_target = TRUE,
      segment_environment_base_eligible = TRUE,
      segment_target_contract_digest = fixture$target_digest,
      backend_requested = "cpp",
      backend_resolved = "cpp",
      segment_transition_kernel_schema =
        rep(fixture$kernel$schema_version, 2L),
      segment_transition_kernel_digest = fixture$kernel_digest
    ),
    "schema must contain nonempty text"
  )
  expect_error(
    rqrgibbs:::.rqr_make_continuation_history(
      checkpoint_digest = history_kernel_sha("numeric-digest"),
      segment_numerical_repair_count = 0L,
      segment_exact_joint_target = TRUE,
      segment_environment_base_eligible = TRUE,
      segment_target_contract_digest = fixture$target_digest,
      backend_requested = "cpp",
      backend_resolved = "cpp",
      segment_transition_kernel_schema =
        fixture$kernel$schema_version,
      segment_transition_kernel_digest = 1
    ),
    "complete SHA-256"
  )
})

test_that("rehashing cannot hide historical transition-kernel mutations", {
  fixture <- make_history_kernel_chain()
  fabricated_digest <- history_kernel_sha("fabricated-kernel")

  changed_early <- fixture$object
  changed_early$continuation_history_contract$segments[[1L]]$
    segment_transition_kernel_digest <- fabricated_digest
  changed_early$continuation_history_digest <- history_kernel_digest(
    changed_early$continuation_history_contract
  )
  expect_error(
    rqrgibbs:::.rqr_validate_continuation_history(changed_early),
    "derived-status semantics"
  )

  changed_late <- fixture$object
  changed_late$continuation_history_contract$segments[[2L]]$
    segment_transition_kernel_digest <- fabricated_digest
  changed_late$continuation_history_contract$transition_kernel_digest <-
    fabricated_digest
  changed_late$continuation_history_digest <- history_kernel_digest(
    changed_late$continuation_history_contract
  )
  expect_error(
    rqrgibbs:::.rqr_validate_continuation_history(changed_late),
    "derived-status semantics"
  )

  rewritten_assertions <- fixture$object
  for (index in seq_along(
      rewritten_assertions$continuation_history_contract$segments
    )) {
    rewritten_assertions$continuation_history_contract$
      segments[[index]]$segment_transition_kernel_digest <-
        fabricated_digest
  }
  rewritten_assertions$continuation_history_contract$
    transition_kernel_digest <- fabricated_digest
  rewritten_assertions$checkpoint_state$transition_kernel_digest <-
    fabricated_digest
  rewritten_assertions$model_spec$
    transition_kernel_digest <- fabricated_digest
  rewritten_assertions$continuation_history_digest <- history_kernel_digest(
    rewritten_assertions$continuation_history_contract
  )
  expect_error(
    rqrgibbs:::.rqr_validate_continuation_history(
      rewritten_assertions
    ),
    "not internally reconstructible"
  )
})

test_that("static fixed-design callers receive only the scoped default", {
  static <- rqrgibbs:::.rqr_static_history_transition_kernel()
  target_digest <- history_kernel_sha("static-target")
  checkpoint_digest <- history_kernel_sha("static-checkpoint")
  history <- rqrgibbs:::.rqr_make_continuation_history(
    checkpoint_digest = checkpoint_digest,
    segment_numerical_repair_count = 0L,
    segment_exact_joint_target = TRUE,
    segment_environment_base_eligible = TRUE,
    segment_target_contract_digest = target_digest,
    backend_requested = "R_precision_cholesky",
    backend_resolved = "R_precision_cholesky"
  )
  expect_identical(history$transition_kernel_schema, static$schema)
  expect_identical(history$transition_kernel_digest, static$digest)

  object <- list(
    family = "rqr_fixed_design",
    continuation_history_contract = history,
    continuation_history_digest = history_kernel_digest(history),
    checkpoint_digest = checkpoint_digest,
    checkpoint_state = list(transition_version = static$schema),
    misc = list(transition_version = static$schema),
    model_spec = list(
      cumulative_numerical_repair_count = 0L,
      chain_history_numerically_exact = TRUE,
      target_numerical_eligible = TRUE,
      promotion_eligible = TRUE,
      exact_joint_target = TRUE
    ),
    provenance = list(
      reproducibility_eligible = TRUE,
      backend_requested = "R_precision_cholesky",
      backend_resolved = "R_precision_cholesky",
      object_digests = list(target = target_digest)
    )
  )
  expect_silent(rqrgibbs:::.rqr_validate_continuation_history(object))
})

test_that("history validation rejects ambiguous contract and segment schemas", {
  fixture <- make_history_kernel_chain()
  validate <- rqrgibbs:::.rqr_validate_continuation_history

  extra_contract <- fixture$object
  extra_contract$continuation_history_contract$unexpected <- TRUE
  extra_contract$continuation_history_digest <- history_kernel_digest(
    extra_contract$continuation_history_contract
  )
  expect_error(
    validate(extra_contract),
    "history contract or digest"
  )

  duplicate_contract <- fixture$object
  duplicate_contract$continuation_history_contract <- c(
    fixture$history1,
    list(generation = fixture$history1$generation)
  )
  duplicate_contract$continuation_history_digest <- history_kernel_digest(
    duplicate_contract$continuation_history_contract
  )
  expect_error(
    validate(duplicate_contract),
    "history contract or digest"
  )

  extra_segment <- fixture$object
  extra_segment$continuation_history_contract$segments[[1L]]$unexpected <-
    TRUE
  extra_segment$continuation_history_digest <- history_kernel_digest(
    extra_segment$continuation_history_contract
  )
  expect_error(
    validate(extra_segment),
    "segments are incomplete"
  )

  duplicate_segment <- fixture$object
  first_segment <- fixture$history1$segments[[1L]]
  duplicate_segment$continuation_history_contract$segments[[1L]] <- c(
    first_segment,
    list(generation = first_segment$generation)
  )
  duplicate_segment$continuation_history_digest <- history_kernel_digest(
    duplicate_segment$continuation_history_contract
  )
  expect_error(
    validate(duplicate_segment),
    "segments are incomplete"
  )
})
