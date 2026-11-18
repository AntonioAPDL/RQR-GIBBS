mdr_validate <- function(d, cfg) {
  keys <- c("n", "content", "tolerance_confidence", "response_id", "method")
  d <- data.table::as.data.table(d)
  mwd_require(nrow(d) == cfg$cases * 6 && !anyDuplicated(d[, ..keys]) &&
    all(d$samples == cfg$samples_per_case) && all(d$returned == d$samples) &&
    all(d$attainment == d$attained / d$samples) && all(d$width_q025 <= d$width_q975) &&
    all(d$mean_width > 0) && setequal(unique(d$method), c(cfg$methods, "gibbs_baseline", "gibbs_original_radius")),
    "Completed comparison has missing cases, methods, returns, or inconsistent summaries.")
  mwd_require(d[, all(.N == cfg$cases), by = method]$V1 |> all(), "Incomplete method comparison.")
  invisible(TRUE)
}

mdr_prepare <- function(cfg, directory) {
  receipt_path <- file.path(directory, "evidence_receipt.json")
  mwd_require(mwd_file_hash(receipt_path) == cfg$completed_evidence_sha256, "Completed evidence receipt differs.")
  mdc_verify_evidence(directory)
  receipt <- mwd_json(receipt_path)
  mwd_require(receipt$source_commit == cfg$completed_source &&
    mwd_json(file.path(directory, "controller_exit.json"))$exit_code == 0 &&
    mwd_json(file.path(directory, "study_summary.json"))$status == "complete_requires_scientific_review",
    "The source study is incomplete or has a different source.")
  rows <- data.table::fread(file.path(directory, "validation/interval_results.csv"))
  mwd_require(nrow(rows) == cfg$cases * cfg$samples_per_case * 6 &&
    !anyDuplicated(rows[, .(job_id, method)]) && all(rows$returned) &&
    all(rows$attained == (rows$population_content >= rows$content)) &&
    all(abs(rows$width - (rows$upper - rows$lower)) < 1e-9 * pmax(1, rows$width)),
    "Completed interval records are inconsistent.")
  actual <- mdc_summarize(rows)
  expected <- data.table::fread(file.path(directory, "validation/comparison_summary.csv"))
  keys <- c("n", "content", "response_id", "method")
  data.table::setorderv(actual, keys); data.table::setorderv(expected, keys)
  mwd_require(isTRUE(all.equal(as.data.frame(actual), as.data.frame(expected),
    tolerance = 1e-9, check.attributes = FALSE)), "The completed summary does not reproduce interval records.")
  mdr_validate(actual, cfg)
  mwd_require(qbinom(1-cfg$calibration_error/cfg$calibration_comparisons,
    cfg$calibration_samples, unique(actual$tolerance_confidence)) + 1 == cfg$calibration_rank,
    "The calibration rank does not match the declared confidence budget.")
  posterior <- data.table::fread(file.path(directory, "validation/posterior_summary.csv"))
  mwd_require(nrow(posterior) == cfg$cases * cfg$samples_per_case * 2 &&
    all(posterior$returned & posterior$adequate & posterior$mean_precision), "Posterior calculations require review.")
  proposed <- actual[method == "gibbs_proposed"]
  ci <- function(x) if (x == 0) 0 else qbeta(cfg$validation_error/cfg$cases, x, cfg$samples_per_case-x+1)
  proposed[, simultaneous_lower := vapply(attained, ci, 0)]
  pair <- function(method_id) {
    z <- merge(proposed, actual[actual$method == method_id], by = c("n", "content", "response_id"), suffixes = c("_g", "_r"))
    data.frame(method = method_id, cases = nrow(z), smaller = sum(z$mean_width_g < z$mean_width_r),
      minimum_reduction = min(1-z$mean_width_g/z$mean_width_r), maximum_reduction = max(1-z$mean_width_g/z$mean_width_r))
  }
  diagnostics <- list(source_commit = cfg$completed_source, cases = nrow(proposed),
    datasets = data.table::uniqueN(rows$job_id), interval_records = nrow(rows), posterior_records = nrow(posterior),
    maximum_rhat = max(posterior$max_rhat), minimum_bulk_ess = min(posterior$min_bulk_ess),
    minimum_tail_ess = min(posterior$min_tail_ess), maximum_retained_per_chain = max(posterior$retained_per_chain),
    pointwise_lower_above_p = sum(proposed$attainment_lower > proposed$tolerance_confidence),
    simultaneous_lower_above_p = sum(proposed$simultaneous_lower > proposed$tolerance_confidence))
  dir.create(cfg$evidence_root, recursive = TRUE, showWarnings = FALSE)
  files <- c("manifest.json", "evidence_receipt.json", "frozen_selection.csv", "frozen_radius.json", "calibration_policy.csv")
  for (file in files) mwd_require(file.copy(file.path(directory, file),
    file.path(cfg$evidence_root, paste0("completed_", file)), overwrite = TRUE), "Cannot archive completed evidence.")
  mwd_write(actual, file.path(cfg$evidence_root, "comparison_summary.csv"))
  mwd_write(proposed, file.path(cfg$evidence_root, "proposed_summary.csv"))
  mwd_write(data.table::rbindlist(lapply(c("ecm", "young_mathew", "wilks", "gibbs_original_radius"), pair)),
    file.path(cfg$evidence_root, "width_comparison.csv"))
  mgfp_json_write(diagnostics, file.path(cfg$evidence_root, "numerical_summary.json"))
  saved <- list.files(cfg$evidence_root, full.names = TRUE)
  saved <- saved[!basename(saved) %in% c("provenance.json", "rendered.json")]
  inputs <- file.path(directory, c("evidence_receipt.json", "validation/interval_results.csv",
    "validation/posterior_summary.csv", "validation/comparison_summary.csv"))
  mgfp_json_write(list(source_commit = cfg$completed_source, cohort = "independent_decision_validation",
    input_hashes = mgfp_hashes(inputs), artifacts = mgfp_hashes(saved)), file.path(cfg$evidence_root, "provenance.json"))
  invisible(diagnostics)
}

mdr_verify <- function(cfg, rendered = TRUE) {
  p <- mwd_json(file.path(cfg$evidence_root, "provenance.json"))
  mwd_require(p$source_commit == cfg$completed_source &&
    mwd_file_hash(file.path(cfg$evidence_root, "completed_evidence_receipt.json")) == cfg$completed_evidence_sha256,
    "Reporting cohort or completed evidence differs.")
  mgfp_check_hashes(p$artifacts)
  mdr_validate(data.table::fread(file.path(cfg$evidence_root, "comparison_summary.csv")), cfg)
  if (rendered) {
    r <- mwd_json(file.path(cfg$evidence_root, "rendered.json"))
    mgfp_check_hashes(r$inputs); mgfp_check_hashes(r$programs); mgfp_check_hashes(r$artifacts)
  }
  invisible(TRUE)
}

mdr_render <- function(cfg) {
  mdr_verify(cfg, rendered = FALSE)
  d <- data.table::fread(file.path(cfg$evidence_root, "comparison_summary.csv"))
  p <- data.table::fread(file.path(cfg$evidence_root, "proposed_summary.csv"))
  comparison <- data.table::fread(file.path(cfg$evidence_root, "width_comparison.csv"))
  diagnostics <- mwd_json(file.path(cfg$evidence_root, "numerical_summary.json"))
  dir.create(cfg$table_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$figure_root, recursive = TRUE, showWarnings = FALSE)
  old <- comparison[method == "gibbs_original_radius"]
  macros <- c(MTIDecisionAttainmentMin = sprintf("%.1f", 100*min(p$attainment)),
    MTIDecisionAttainmentMax = sprintf("%.1f", 100*max(p$attainment)),
    MTIDecisionPointwise = diagnostics$pointwise_lower_above_p,
    MTIDecisionSimultaneous = diagnostics$simultaneous_lower_above_p,
    MTIDecisionWidthGainMin = sprintf("%.2f",100*old$minimum_reduction),
    MTIDecisionWidthGainMax = sprintf("%.2f",100*old$maximum_reduction),
    MTIDecisionShorterECM = comparison[method == "ecm",smaller],
    MTIDecisionShorterYM = comparison[method == "young_mathew",smaller],
    MTIDecisionShorterWilks = comparison[method == "wilks",smaller],
    MTIDecisionMaxRhat = sprintf("%.4f",diagnostics$maximum_rhat))
  writeLines(sprintf("\\newcommand{\\%s}{%s}",names(macros),macros),file.path(cfg$table_root,"numbers.tex"))
  designs <- unique(p[, .(n, content)]); data.table::setorder(designs,n,content)
  table <- c("\\begin{tabular}{@{}lrrrr@{}}", "\\toprule",
    "Design & Attainment (\\%) & \\multicolumn{3}{c}{Smaller mean width than}\\\\",
    " & & ECM & Young--Mathew & Wilks\\\\", "\\midrule")
  for (i in seq_len(nrow(designs))) {
    n0 <- designs$n[i]; c0 <- designs$content[i]; a <- p[n == n0 & content == c0]
    counts <- vapply(c("ecm","young_mathew","wilks"),function(method_id) {
      r <- d[n == n0 & content == c0 & method == method_id]
      sum(a$mean_width[match(r$response_id,a$response_id)] < r$mean_width)
    },1L)
    table <- c(table,sprintf("\\(n=%d,c=%.2f\\) & %.1f--%.1f & %d/8 & %d/8 & %d/8\\\\",
      n0,c0,100*min(a$attainment),100*max(a$attainment),counts[1],counts[2],counts[3]))
  }
  writeLines(c(table,"\\bottomrule","\\end{tabular}"),file.path(cfg$table_root,"comparison.tex"))
  labels <- c(gibbs_proposed="Calibrated MTI-Gibbs",ecm="MTI-ECM",young_mathew="Young-Mathew",wilks="Wilks")
  colors <- setNames(c("#0072B2","#009E73","#D55E00","black"),names(labels))
  laws <- c(normal="Gaussian",student_t3="Student t (3 df)",exponential="Exponential",
    asym_laplace_tau010="Asymmetric Laplace",two_piece_normal_1_12="Two-piece Normal",
    beta18="Beta (1, 8)",gamma05="Gamma (shape 0.5)",lognormal_hard="Log-normal")
  oracle_path <- "figures/data/tolerance_population_shortest_content_oracle.csv"
  oracle <- data.table::fread(oracle_path)
  for (i in seq_len(nrow(designs))) for (kind in c("width","attainment")) {
    z <- d[n == designs$n[i] & content == designs$content[i] & method %in% names(labels)]
    lo <- if (kind == "width") z$width_q025 else z$attainment_lower
    hi <- if (kind == "width") z$width_q975 else z$attainment_upper
    path <- file.path(cfg$figure_root,sprintf("%s_n%d_c%02d.pdf",kind,designs$n[i],round(100*designs$content[i])))
    grDevices::pdf(path,width=10,height=8,pointsize=14,useDingbats=FALSE)
    layout(matrix(1:2,2,1),heights=c(1,9))
    par(mar=c(0,0,0,0));plot.new()
    legend("center",labels,col=colors,lty=1,lwd=2,ncol=2,bty="n",cex=1)
    par(mar=c(4.5,11,2.8,1),mgp=c(2.5,.7,0),cex.axis=1,cex.lab=1)
    y <- (length(laws)+1-match(z$response_id,names(laws)))*6+2.5-match(z$method,names(labels))
    plot(NA,xlim=if(kind=="width") range(c(lo,hi,oracle[content == designs$content[i],width])) else c(cfg$attainment_axis_minimum,1),
      log=if(kind=="width") "x" else "",ylim=c(3,6*length(laws)+3),axes=FALSE,ylab="",
      xlab=if(kind=="width") "Interval width: empirical 2.5%--97.5% range" else
        "Content attainment: pointwise exact 95% confidence interval",
      main=sprintf("n = %d, c = %.2f, p = 0.95",designs$n[i],designs$content[i]))
    axis(1);axis(2,at=(length(laws):1)*6,labels=laws,las=1,tick=FALSE)
    abline(h=(seq_len(length(laws)-1)+.5)*6,col="gray90")
    segments(lo,y,hi,y,col=colors[z$method],lwd=1.8)
    if (kind == "width") for (law in names(laws)) {
      reference <- oracle[dgp_id == law & content == designs$content[i],width]
      mwd_require(length(reference) == 1 && is.finite(reference) && reference > 0,
        "Missing population shortest-content width.")
      center <- (length(laws)+1-match(law,names(laws)))*6
      segments(reference,center-2,reference,center+2,col="gray45",lty=2,lwd=1.3)
    }
    points(if(kind=="width") z$mean_width else z$attainment,y,
      pch=if(kind=="width") 4 else 16,col=colors[z$method],cex=.9)
    if(kind=="attainment") abline(v=.95,lty=2,col="gray40")
    grDevices::dev.off()
  }
  inputs <- list.files(cfg$evidence_root,full.names=TRUE)
  inputs <- c(inputs[basename(inputs)!="rendered.json"],oracle_path)
  programs <- c("application/scripts/135_report_mti_gibbs_decisions.R",
    "application/scripts/lib/mti_gibbs_decision_reporting.R", "application/config/mti_gibbs_decision_reporting_20261118.json")
  artifacts <- c(list.files(cfg$table_root,full.names=TRUE),list.files(cfg$figure_root,full.names=TRUE))
  mgfp_json_write(list(inputs=mgfp_hashes(inputs),programs=mgfp_hashes(programs),artifacts=mgfp_hashes(artifacts)),
    file.path(cfg$evidence_root,"rendered.json"))
  mdr_verify(cfg)
  invisible(artifacts)
}
