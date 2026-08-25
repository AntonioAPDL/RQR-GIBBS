#!/usr/bin/env Rscript

script <- normalizePath("figures/generate_tolerance_validation_primary_figure.R",
                        winslash = "/", mustWork = TRUE)
work_dir <- tempfile("tolerance-primary-figure-test-")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

row <- function(dgp, n, content, rep, method, success = TRUE,
                infeasible = FALSE, width = 2) {
  data.frame(
    dgp_id = dgp,
    n = n,
    guaranteed_content = content,
    method_id = method,
    success = success,
    infeasible = infeasible,
    width = width,
    stringsAsFactors = FALSE
  )
}

primary <- do.call(rbind, lapply(c("normal", "student_t3"), function(dgp) {
  do.call(rbind, lapply(c(500L, 1000L), function(n) {
    do.call(rbind, lapply(c(0.90, 0.95, 0.99), function(content) {
      do.call(rbind, lapply(1:4, function(rep) {
        rbind(
          row(dgp, n, content, rep, "tcsp_mc",
              success = !(dgp == "student_t3" && rep == 1L),
              width = 2 + content + rep / 10),
          row(dgp, n, content, rep, "wilks_minmax",
              success = TRUE, width = 4 + content + rep / 10)
        )
      }))
    }))
  }))
}))
young <- do.call(rbind, lapply(c("normal", "student_t3"), function(dgp) {
  do.call(rbind, lapply(c(500L, 1000L), function(n) {
    do.call(rbind, lapply(c(0.90, 0.95, 0.99), function(content) {
      do.call(rbind, lapply(1:4, function(rep) {
        row(dgp, n, content, rep, "young_mathew",
            success = rep != 1L || dgp == "normal",
            width = 1.8 + content + rep / 10)
      }))
    }))
  }))
}))
mti <- do.call(rbind, lapply(c("normal", "student_t3"), function(dgp) {
  do.call(rbind, lapply(c(500L, 1000L), function(n) {
    do.call(rbind, lapply(c(0.90, 0.95, 0.99), function(content) {
      do.call(rbind, lapply(1:4, function(rep) {
        row(dgp, n, content, rep,
            "mti_ecm_adaptive_cell",
            success = rep != 1L || dgp == "normal",
            width = 1.7 + content + rep / 10)
      }))
    }))
  }))
}))

primary_path <- file.path(work_dir, "primary.csv")
young_path <- file.path(work_dir, "young.csv")
mti_path <- file.path(work_dir, "mti.csv")
utils::write.csv(primary, primary_path, row.names = FALSE)
utils::write.csv(young, young_path, row.names = FALSE)
utils::write.csv(mti, mti_path, row.names = FALSE)
out_dir <- file.path(work_dir, "figures")

status <- system2(
  "Rscript",
  c(script, paste0("--primary-results=", primary_path),
    paste0("--mti-ecm-results=", mti_path),
    paste0("--young-mathew-results=", young_path),
    paste0("--output-dir=", out_dir)),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(status, "status"), NULL)) {
  cat(status, sep = "\n")
  stop("Tolerance validation primary figure generator failed.",
       call. = FALSE)
}

png_path <- file.path(out_dir, "fig04_tolerance_validation_primary.png")
manifest_path <- file.path(
  out_dir, "tolerance_validation_primary_figure_manifest.csv"
)
stopifnot(file.exists(png_path), file.exists(manifest_path))
stopifnot(file.info(png_path)$size > 10000)

manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
stopifnot(any(grepl("fig04_tolerance_validation_primary.png",
                    manifest$relative_path, fixed = TRUE)))
stopifnot(any(grepl("primary.csv", manifest$relative_path, fixed = TRUE)))
stopifnot(any(grepl("mti.csv", manifest$relative_path, fixed = TRUE)))
stopifnot(any(grepl("young.csv", manifest$relative_path, fixed = TRUE)))

cat("Tolerance validation primary figure test passed.\n")
