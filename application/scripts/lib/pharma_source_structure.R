psa_default_config <- "application/config/pharma_source_structure_20261026.json"

psa_application_inputs <- function(config) {
  pma_verify_hash(config$application_config, config$application_config_sha256)
  application <- pma_read_config(config$application_config)
  canonical <- application$paths$canonical_dir
  paths <- c(results_sha256 = "results.csv", membership_sha256 = "split_membership.csv.gz",
    observed_sha256 = "observed_batches.csv")
  for (field in names(paths)) pma_verify_hash(file.path(canonical, paths[[field]]), config[[field]])
  application
}

psa_columns <- function(x, columns) {
  if (!all(columns %in% names(x))) pta_stop("Required source columns are missing.")
  if (anyNA(x[columns])) pta_stop("Source identifiers or measurements are missing.")
  invisible(TRUE)
}

psa_campaign_mapping <- function(process, batches, product_code = 23L) {
  psa_columns(process, c("batch", "campaign", "code"))
  if (any(process$code != product_code) || anyDuplicated(batches))
    pta_stop("Unexpected product code or duplicate laboratory batches.")
  mapping <- unique(process[c("batch", "campaign")])
  if (anyDuplicated(mapping$batch) || !setequal(mapping$batch, batches))
    pta_stop("Campaign mapping must match every laboratory batch exactly once.")
  mapping <- mapping[order(mapping$batch), , drop = FALSE]
  rownames(mapping) <- NULL
  mapping
}

psa_measurement_checks <- function(laboratory, product_code = 23L) {
  fields <- c("batch", "code", "fct_tensile", "fct_max_thickness", "fct_max_diameter",
    unlist(lapply(c("fct", "tbl"), function(v)
      paste0(v, c("_min_hardness", "_av_hardness", "_max_hardness")))))
  psa_columns(laboratory, fields)
  if (any(!is.finite(as.matrix(laboratory[fields])))) pta_stop("Nonfinite source measurement.")
  flags <- do.call(rbind, lapply(c("fct", "tbl"), function(v) {
    lo <- laboratory[[paste0(v, "_min_hardness")]]
    av <- laboratory[[paste0(v, "_av_hardness")]]
    hi <- laboratory[[paste0(v, "_max_hardness")]]
    data.frame(batch = laboratory$batch, code = laboratory$code, response = v,
      minimum = lo, average = av, maximum = hi, below = av < lo,
      above = av > hi, inverted = lo > hi)
  }))
  counts <- do.call(rbind, lapply(c("fct", "tbl"), function(v) {
    a <- flags[flags$response == v, ]; b <- a[a$code == product_code, ]
    data.frame(response = v, all_rows = nrow(a), all_questions = sum(a$below | a$above),
      product_rows = nrow(b), product_questions = sum(b$below | b$above),
      product_below = sum(b$below), product_above = sum(b$above))
  }))
  if (any(laboratory$fct_max_thickness <= 0 | laboratory$fct_max_diameter <= 0))
    pta_stop("Nonpositive dimensions in tensile calculation.")
  derived <- with(laboratory, 2 * fct_av_hardness /
    (3.14 * fct_max_thickness * fct_max_diameter))
  list(counts = counts, flags = flags[flags$below | flags$above | flags$inverted, ],
    maximum_tensile_error = max(abs(derived - laboratory$fct_tensile)))
}

psa_partition_sharing <- function(observed, membership, mapping, training_size = 100L) {
  psa_columns(observed, c(".analysis_row", "batch", "api_batch"))
  psa_columns(mapping, c("batch", "campaign"))
  psa_columns(membership, c("analysis_row_id", "split_scheme", "split_id", "training"))
  if (anyDuplicated(observed$.analysis_row) || anyDuplicated(observed$batch) ||
      !is.logical(membership$training) || anyDuplicated(mapping$batch) ||
      !setequal(observed$batch, mapping$batch)) pta_stop("Invalid batch membership or campaign mapping.")
  index <- match(membership$analysis_row_id, observed$.analysis_row)
  if (anyNA(index)) pta_stop("Membership contains an unknown observation.")
  campaign <- mapping$campaign[match(observed$batch, mapping$batch)]
  if (anyNA(campaign)) pta_stop("Missing campaign mapping.")
  keys <- paste(membership$split_scheme, membership$split_id, sep = ":")
  rows <- lapply(split(seq_len(nrow(membership)), keys), function(i) {
    m <- membership[i, ]; j <- index[i]
    if (length(j) != nrow(observed) || anyDuplicated(j) ||
        sum(m$training) != training_size) pta_stop("Incomplete or incorrectly sized partition.")
    sharing <- function(g) {
      common <- intersect(g[m$training], g[!m$training])
      c(shared = length(common), heldout_shared = sum(g[!m$training] %in% common))
    }
    a <- sharing(campaign[j]); b <- sharing(observed$api_batch[j])
    data.frame(split_scheme = m$split_scheme[1], split_id = m$split_id[1],
      campaign_shared = unname(a[1]), campaign_heldout_shared = unname(a[2]),
      api_shared = unname(b[1]), api_heldout_shared = unname(b[2]))
  })
  answer <- do.call(rbind, rows); rownames(answer) <- NULL
  answer[order(answer$split_scheme, answer$split_id), ]
}

psa_group_components <- function(data, variables) {
  psa_columns(data, c("batch", variables))
  if (anyDuplicated(data$batch) || any(as.matrix(data[variables]) == 0))
    pta_stop("Duplicate batch or unknown zero group identifier.")
  pta_require_packages("igraph")
  edges <- do.call(rbind, lapply(variables, function(v)
    data.frame(from = paste0("batch:", data$batch), to = paste0(v, ":", data[[v]]))))
  graph <- igraph::graph_from_data_frame(unique(edges), directed = FALSE)
  membership <- igraph::components(graph)$membership[paste0("batch:", data$batch)]
  labels <- vapply(split(data$batch, membership), function(b) paste(sort(b), collapse = ","), character(1))
  unname(labels[as.character(membership)])
}

psa_separation_design <- function(data, variables, training_size) {
  groups <- psa_group_components(data, variables)
  sizes <- as.integer(table(groups))
  count <- pma_subset_counts(sizes, training_size)[1, training_size + 1L]
  data.frame(separation = paste(variables, collapse = "+"), components = length(sizes),
    component_batch_sizes = paste(sort(sizes), collapse = ","),
    training_size = training_size, feasible_training_sets = count,
    feasible = count > 0)
}

psa_prepare_partitions <- function(data, variables, training_size, requested, seed) {
  groups <- psa_group_components(data, variables)
  partitions <- pma_group_partitions(groups, training_size, requested, seed)
  # The existing sampler weights inclusion by the exact number of feasible suffixes.
  do.call(rbind, lapply(seq_along(partitions), function(i) {
    s <- partitions[[i]]
    data.frame(partition_id = i, batch = data$batch,
      training = seq_len(nrow(data)) %in% s$train)
  }))
}

psa_retrospective_summary <- function(results, sharing) {
  keys <- c("response_id", "split_scheme", "split_id", "method_id")
  psa_columns(results, keys)
  if (anyDuplicated(results[keys]) || anyDuplicated(sharing[c("split_scheme", "split_id")]))
    pta_stop("Duplicate result or partition identifiers.")
  chosen <- sharing$split_id[sharing$split_scheme == "month_grouped" & sharing$campaign_shared == 0]
  if (!length(chosen)) pta_stop("No campaign-disjoint month partitions are available.")
  month <- results[results$split_scheme == "month_grouped", ]
  expected <- expand.grid(response_id = unique(results$response_id), split_id = chosen,
    method_id = pma_methods, stringsAsFactors = FALSE)
  key <- function(z) paste(z$response_id, z$split_id, z$method_id)
  subset <- month[month$split_id %in% chosen, ]
  if (!setequal(key(subset), key(expected)) || nrow(subset) != nrow(expected))
    pta_stop("Retrospective comparisons have missing methods or pairs.")
  a <- pma_summary(month); a$comparison <- "all_month_partitions"
  b <- pma_summary(subset); b$comparison <- "campaign_disjoint_month_subset"
  list(selected = sharing[sharing$split_scheme == "month_grouped" & sharing$split_id %in% chosen, ],
    summary = rbind(a, b), paired = pma_paired_summary(subset))
}

psa_source_file <- function(path, record, download = FALSE) {
  if (!file.exists(path) && download) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(tmpdir = dirname(path))
    on.exit(unlink(temporary), add = TRUE)
    utils::download.file(record$download_url, temporary, mode = "wb", quiet = TRUE)
    psa_source_file(temporary, record, download = FALSE)
    if (!file.rename(temporary, path)) pta_stop("Cannot preserve source download.")
  }
  if (!file.exists(path)) pta_stop("Original input is absent; supply it or explicitly request --download=true: ", path)
  if (file.info(path)$size != record$bytes || pta_file_sha256(path) != record$sha256)
    pta_stop("Original source bytes do not match the versioned source record.")
  path
}

psa_render <- function(evidence_dir, table_dir = "tables") {
  manifest <- pta_read_config(file.path(evidence_dir, "manifest.json"))
  for (a in manifest$artifacts) pma_verify_hash(file.path(evidence_dir, a$path), a$sha256)
  counts <- pta_read_csv(file.path(evidence_dir, "measurement_counts.csv"))
  sharing <- pta_read_csv(file.path(evidence_dir, "partition_sharing.csv"))
  summary <- pta_read_csv(file.path(evidence_dir, "retrospective_summary.csv"))
  selected <- sharing[sharing$split_scheme == "month_grouped" & sharing$campaign_shared == 0, ]
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  values <- c(PharmaCampaigns = manifest$campaigns,
    PharmaFctHardnessQuestions = counts$product_questions[counts$response == "fct"],
    PharmaCoreHardnessQuestions = counts$product_questions[counts$response == "tbl"],
    PharmaRandomSharedHeldout = median(sharing$campaign_heldout_shared[sharing$split_scheme == "random"]),
    PharmaCampaignDisjointMonths = nrow(selected),
    PharmaCampaignDisjointApiShared = sum(selected$api_shared > 0),
    PharmaPreparedGroupPartitions = manifest$prepared_partitions)
  for (response in c("fct_tensile", "tbl_rsd_weight")) {
    z <- summary[summary$response_id == response & summary$method_id == pma_methods[1] &
      summary$comparison == "campaign_disjoint_month_subset", ]
    prefix <- if (response == "fct_tensile") "PharmaCampaignTensile" else "PharmaCampaignRsd"
    values[paste0(prefix, "Content")] <- pta_format_pct(z$heldout_content_median)
    values[paste0(prefix, "Attainment")] <- pta_format_pct(z$heldout_attainment_rate)
  }
  writeLines(sprintf("\\newcommand{\\%s}{%s}", names(values), values),
    file.path(table_dir, "pharma_source_numbers.tex"))
  lines <- c("\\begingroup", "\\footnotesize", "\\setlength{\\tabcolsep}{3pt}",
    "\\begin{tabularx}{\\textwidth}{@{}l>{\\raggedright\\arraybackslash}Xrrr@{}}",
    "\\toprule", "Method & Width median [95\\% range] & Content & $\\ge c$ & Below/above\\\\", "\\midrule")
  for (response in c("fct_tensile", "tbl_rsd_weight")) {
    for (comparison in c("all_month_partitions", "campaign_disjoint_month_subset")) {
      z <- summary[summary$response_id == response & summary$comparison == comparison, ]
      z <- z[match(pma_methods, z$method_id), ]
      label <- if (response == "fct_tensile") "Reported tensile metric" else "Weight RSD"
      description <- if (comparison == "all_month_partitions") "all month partitions" else "no shared campaign"
      lines <- c(lines, sprintf("\\multicolumn{5}{@{}l}{\\textit{%s: %s (%d)}}\\\\",
        label, description, z$splits[1]))
      for (i in seq_len(nrow(z))) lines <- c(lines, sprintf("%s & %s & %s & %s & %s/%s \\\\",
        z$method[i], pma_median_range(z$width_median[i], z$width_q025[i], z$width_q975[i]),
        pta_format_pct(z$heldout_content_median[i]), pta_format_pct(z$heldout_attainment_rate[i]),
        pta_format_pct(z$lower_omitted_median[i]), pta_format_pct(z$upper_omitted_median[i])))
      lines <- c(lines, "\\addlinespace")
    }
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabularx}", "\\endgroup"),
    file.path(table_dir, "pharma_campaign_sensitivity.tex"))
  invisible(values)
}

psa_verify <- function(config) {
  psa_application_inputs(config)
  manifest <- pta_read_config(file.path(config$evidence_dir, "manifest.json"))
  if (manifest$config_sha256 != pta_file_sha256(attr(config, "config_path") %||% psa_default_config) ||
      manifest$new_fits != 0 || isTRUE(manifest$source_clarification_received))
    pta_stop("Source-review record differs from the declared unresolved, no-fit analysis.")
  for (record in manifest$calculation_sources) pma_verify_hash(record$path, record$sha256)
  temporary <- tempfile("pharma-source-tables-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  psa_render(config$evidence_dir, temporary)
  for (name in c("pharma_source_numbers.tex", "pharma_campaign_sensitivity.tex"))
    if (!identical(readLines(file.path(temporary, name)), readLines(file.path("tables", name))))
      pta_stop("Committed source-review table differs from its verified evidence: ", name)
  invisible(TRUE)
}

psa_run <- function(config, laboratory_file, archive_file, download = FALSE) {
  application <- psa_application_inputs(config)
  canonical <- application$paths$canonical_dir
  lab_record <- application$data_source
  lab_record$bytes <- lab_record$expected_bytes; lab_record$sha256 <- lab_record$expected_sha256
  psa_source_file(laboratory_file, lab_record, download)
  psa_source_file(archive_file, config$process_source, download)
  laboratory <- pta_load_raw_data(laboratory_file)
  data <- pta_code23_data(application, laboratory)
  member <- config$process_source$member
  if (!member %in% utils::unzip(archive_file, list = TRUE)$Name) pta_stop("Process member is absent.")
  extracted <- file.path(config$cache_dir, "extracted")
  dir.create(extracted, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(archive_file, files = member, exdir = extracted)
  path <- file.path(extracted, member)
  pma_verify_hash(path, config$process_source$member_sha256)
  header <- names(utils::read.csv(path, sep = ";", nrows = 0))
  classes <- ifelse(header %in% c("batch", "campaign", "code"), "integer", "NULL")
  process <- utils::read.csv(path, sep = ";", colClasses = classes)
  if (nrow(process) != config$process_source$rows) pta_stop("Unexpected process-record count.")
  mapping <- psa_campaign_mapping(process, data$batch, application$analysis$product_code)
  data$campaign <- mapping$campaign[match(data$batch, mapping$batch)]
  measurements <- psa_measurement_checks(laboratory)
  observed <- pta_read_csv(file.path(canonical, "observed_batches.csv"))
  if (!setequal(observed$batch, data$batch)) pta_stop("Observed batch set has changed.")
  for (v in c("fct_tensile", "tbl_rsd_weight")) if (
    max(abs(observed[[v]] - data[[v]][match(observed$batch, data$batch)])) > 1e-12)
      pta_stop("Recorded application response differs from the original source.")
  membership <- pta_read_csv(file.path(canonical, "split_membership.csv.gz"))
  sharing <- psa_partition_sharing(observed, membership, mapping)
  retrospective <- psa_retrospective_summary(pta_read_csv(file.path(canonical, "results.csv")), sharing)
  variables <- list("campaign", c("campaign", "api_batch"),
    c("campaign", "api_batch", "smcc_batch", "lactose_batch", "starch_batch"))
  designs <- do.call(rbind, lapply(variables, function(v) psa_separation_design(data, v, 100L)))
  prepared <- config$prepared_design
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  partitions <- psa_prepare_partitions(data, unlist(prepared$groups), prepared$training_size,
    prepared$partitions, prepared$seed)
  output <- config$evidence_dir
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  artifacts <- list(campaign_mapping = mapping, measurement_counts = measurements$counts,
    measurement_questions = measurements$flags, partition_sharing = sharing,
    retrospective_partitions = retrospective$selected, retrospective_summary = retrospective$summary,
    retrospective_paired = retrospective$paired, separation_designs = designs)
  for (name in names(artifacts)) pma_write_csv(artifacts[[name]], file.path(output, paste0(name, ".csv")))
  pma_write_membership(partitions, file.path(output, "prepared_campaign_api_membership.csv.gz"))
  files <- list.files(output, pattern = "\\.csv(\\.gz)?$", full.names = TRUE)
  manifest <- list(study_id = config$study_id, source_commit = pma_git("rev-parse", "HEAD"),
    original_application_results_sha256 = config$results_sha256,
    original_membership_sha256 = config$membership_sha256,
    config_sha256 = pta_file_sha256(attr(config, "config_path") %||% psa_default_config),
    laboratory_sha256 = pta_file_sha256(laboratory_file), archive_sha256 = pta_file_sha256(archive_file),
    member_sha256 = pta_file_sha256(path), process_doi = config$process_source$doi,
    license = config$process_source$license, campaigns = length(unique(mapping$campaign)),
    maximum_tensile_formula_error = measurements$maximum_tensile_error,
    prepared_partitions = length(unique(partitions$partition_id)), prepared_seed = prepared$seed,
    prepared_status = prepared$status, new_fits = 0L, source_clarification_received = FALSE,
    interpretation = "Retrospective historical partition summaries; no independent tolerance validation.",
    R_version = R.version.string, rng_kind = RNGkind(),
    package_versions = setNames(lapply(c("igraph", "jsonlite", "digest"),
      function(p) as.character(utils::packageVersion(p))), c("igraph", "jsonlite", "digest")),
    calculation_sources = pma_file_record(c("application/scripts/103_review_pharma_source_structure.R",
      file.path("application/scripts/lib", c("pharma_source_structure.R", "pharma_tolerance_application.R",
        "pharma_mti_application.R", "pharma_mti_reporting.R")))),
    artifacts = lapply(files, function(p) list(path = basename(p), sha256 = pta_file_sha256(p))))
  pta_atomic_write_json(manifest, file.path(output, "manifest.json"))
  psa_render(output)
  invisible(manifest)
}
