adoption_env <- function() {
  e <- new.env(parent=globalenv())
  e$repo <- normalizePath(testthat::test_path("..","..",".."))
  for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
    "mti_profile_revision.R","mti_profile_review.R","mti_fixed_policy_evaluation.R",
    "mti_q_grid_study.R","mti_final_reporting.R","mti_profile_adoption.R"))
    sys.source(file.path(e$repo,"application/scripts/lib",file),e)
  e
}

test_that("adopted policy preserves all nine independently assessed settings", {
  e <- adoption_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old))
  a <- e$mpa_read()
  expect_silent(e$mpa_check_runtime(a))
  expect_equal(nrow(a$policy),9L)
  expect_true(all(a$policy$decision=="passed_new_calibration"))
  selected <- read.csv(file.path(a$adoption$evidence_dir,"selected_calibration.csv"))
  expect_equal(nrow(selected),72L)
  expect_true(all(selected$lower_bound>=.95 & selected$returned==24000L))
  for(i in seq_len(nrow(a$policy))) {
    r <- a$policy[i,]; set.seed(20261025+i); y <- rnorm(r$n)
    direct <- e$mpq_frozen_action(y,r$content,r$tolerance_confidence,a$policy,a$cfg)
    cached <- e$mpq_frozen_action(y,r$content,r$tolerance_confidence,a$policy,a$cfg,a$context)
    expect_identical(direct,cached)
    expect_false(direct$universal_frequentist_guarantee)
    expect_false(direct$continuous_optimum_claim)
    expect_equal(direct$computation,"fixed_target_profile")
  }
  altered <- a$policy; altered$decision[1] <- "unresolved"
  expect_error(e$mpq_frozen_action(seq_len(50),.9,.95,altered,a$cfg),"No calibrated policy")
  duplicate <- rbind(a$policy,a$policy[1,])
  expect_error(e$mpq_frozen_action(seq_len(50),.9,.95,duplicate,a$cfg),"No calibrated policy")
  expect_error(e$mpq_frozen_action(seq_len(50),.95,.95,a$policy,a$cfg),"No calibrated policy")
  impossible <- a$policy; impossible$threshold[1] <- 1-1e-14
  absent <- e$mpq_frozen_action(seq(-1,1,length.out=50),.9,.95,impossible,a$cfg)
  expect_false(absent$returned)
})

test_that("profile application records its own diagnostics on unchanged splits", {
  e <- adoption_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old))
  cfg <- e$pma_read_config(e$mpa_application_default)
  context <- e$mpa_application_context(cfg)
  splits <- e$pma_make_splits(context$data,cfg,"confirmatory")
  expect_equal(sum(vapply(splits,function(s)s$scheme=="random",logical(1))),1000L)
  tasks <- lapply(c("fct_tensile","tbl_rsd_weight"),function(response)
    list(split=splits[[1]],response=response,role=if(response=="fct_tensile")"primary" else "supplement"))
  rows <- do.call(rbind,lapply(tasks,e$pma_run_task,context=context))
  expect_silent(e$pma_validate_results(rows))
  mti <- rows[rows$method_id==e$pma_methods[1],]
  expect_true(all(mti$mti_computation=="fixed_target_profile"))
  expect_true(all(is.na(mti$mti_ecm_converged)))
  expect_true(all(mti$mti_profile_converged))
  expect_true(all(abs(mti$mti_relative_objective_gap)<=1e-8))
  bad <- rows; bad$mti_relative_objective_gap[bad$method_id==e$pma_methods[1]] <- .1
  expect_error(e$pma_validate_results(bad),"profiled MTI")
  bad <- rows; bad$mti_ecm_converged[bad$method_id==e$pma_methods[1]] <- TRUE
  expect_error(e$pma_validate_results(bad),"profiled MTI")
  diagnostics <- e$pma_diagnostics(rows)
  expect_true("mti_relative_objective_gap" %in% diagnostics$metric)
  expect_true(all(diagnostics$median[diagnostics$metric=="mti_relative_objective_gap"]>=0))
  expect_false("mti_ecm_final_stationarity" %in% diagnostics$metric)
})

test_that("adopted reporting contains three methods and unpooled widths", {
  e <- adoption_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old))
  out <- tempfile("adopted_tables_"); dir.create(out); on.exit(unlink(out,recursive=TRUE),add=TRUE)
  e$mpa_export_tables(out)
  z <- read.csv(file.path(out,"tolerance_validation_article_dgp_width_ranges.csv"))
  expect_equal(nrow(z),216L)
  expect_setequal(z$method_id,c("mti_profile_mass_cells_v1","young_mathew","wilks_minmax"))
  expect_true(all(z$replications==1000L))
  expect_true(all(z$width_q025<=z$width_q975))
  expect_equal(nrow(read.csv(file.path(out,"tolerance_validation_article_scenario_ranges.csv"))),27L)
  tex <- readLines(file.path(out,"tolerance_validation_article_dgp_delivery.tex"))
  expect_true(any(grepl("pointwise exact",tex,fixed=TRUE)))
  expect_true(any(grepl("tab:supp-primary-dgp-delivery",tex,fixed=TRUE)))
  width <- readLines(file.path(out,"tolerance_validation_article_dgp_width_ranges.tex"))
  expect_true(any(grepl("tab:supp-primary-dgp-width-ranges",width,fixed=TRUE)))
})

test_that("adoption rejects changed policy files and unaudited application publication", {
  e <- adoption_env(); old <- getwd(); setwd(e$repo); on.exit(setwd(old))
  directory <- tempfile("adoption_guard_"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  a <- e$mpr_read_json(e$mpa_default)
  policy <- read.csv(a$policy_csv); policy$threshold[1] <- .5
  a$policy_csv <- file.path(directory,"altered_policy.csv")
  e$pma_write_csv(policy,a$policy_csv)
  path <- file.path(directory,"altered_adoption.json")
  e$pta_atomic_write_json(a,path)
  expect_error(e$mpa_read(path),"hash mismatch")
  # This fixture tests the publication boundary only, not scientific acceptance.
  e$pma_verify_run <- function(...)list(mode="confirmatory",policy_sha256="test_only",
    return_counts=list(mti_ecm_adaptive_cell=2L))
  cfg <- list(mti_profile=list(),paths=list(canonical_dir="published"))
  e$pma_write_csv(data.frame(test_only=TRUE),file.path(directory,"results.csv"))
  expect_error(e$pma_publish(directory,cfg,directory),"Independently validate")
  audit <- list(passed=TRUE,results_sha256="altered",policy_sha256="test_only",
    fixed_target_profiles_recomputed=2L)
  e$pta_atomic_write_json(audit,file.path(directory,"profile_validation.json"))
  expect_error(e$pma_publish(directory,cfg,directory),"does not match")
})
