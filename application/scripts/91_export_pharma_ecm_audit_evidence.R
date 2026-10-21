#!/usr/bin/env Rscript

for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
              "pharma_ecm_audit_reporting.R")) source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
directory <- pta_arg_value(args,"--run-dir=","")
destination <- pta_arg_value(args,"--destination=","figures/data/pharma_ecm_numerical_audit_20261020")
if(!nzchar(directory) || file.exists(destination)) stop("Provide a verified run and a fresh destination.")
inputs <- pea_read_audit(directory)
if(inputs$manifest$mode!="run") stop("Smoke evidence cannot supply the completed diagnostic record.")
paths <- c(inputs$manifest$artifact_records$path,"manifest.json","case_decisions.csv","findings.md")
if(!all(file.exists(file.path(directory,paths)))) stop("Generate the verified findings first.")
dir.create(destination,recursive=TRUE)
if(!all(file.copy(file.path(directory,paths),destination,overwrite=FALSE))) stop("Evidence copy failed.")
records <- pma_file_record(file.path(destination,paths)); records$path <- basename(records$path)
evidence <- list(source_run_manifest_sha256=pta_file_sha256(file.path(directory,"manifest.json")),
  source_run=directory,files=records,
  reporting_sources=pma_file_record(c("application/scripts/lib/pharma_ecm_audit_reporting.R",
    "figures/generate_pharma_ecm_numerical_audit_report.R")),article_results_replaced=FALSE)
pta_atomic_write_json(evidence,file.path(destination,"evidence_manifest.json"))
cat("Exported portable diagnostic evidence to",destination,"\n")
