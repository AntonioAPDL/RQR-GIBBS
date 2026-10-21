#!/usr/bin/env Rscript

arguments <- commandArgs(FALSE)
path <- sub("--file=","",arguments[startsWith(arguments,"--file=")][1],fixed=TRUE)
setwd(normalizePath(file.path(dirname(path),".."),mustWork=TRUE))
for(file in c("pharma_tolerance_application.R","pharma_mti_application.R",
               "pharma_ecm_audit_reporting.R")) source(file.path("application/scripts/lib",file))
args <- commandArgs(TRUE)
directory <- pta_arg_value(args,"--run-dir=","")
if (!nzchar(directory)) stop("Provide --run-dir pointing to verified diagnostic evidence.")
inputs <- pea_read_audit(directory)
path <- pta_arg_value(args,"--output-pdf=",file.path(directory,"pharma_ecm_numerical_diagnostics.pdf"))
output_dir <- pta_arg_value(args,"--output-dir=",dirname(path))
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
pea_report_pdf(inputs,path)
pea_write_findings(inputs,output_dir,path)
files <- c(path,file.path(output_dir,c("case_decisions.csv","findings.md")))
manifest <- list(source_manifest_sha256=pta_file_sha256(file.path(directory,"manifest.json")),
  reporting_sources=pma_file_record(c("application/scripts/lib/pharma_ecm_audit_reporting.R",
                                     "figures/generate_pharma_ecm_numerical_audit_report.R")),
  artifacts=pma_file_record(files),article_results_replaced=FALSE)
pta_atomic_write_json(manifest,file.path(output_dir,"report_manifest.json"))
cat("Generated diagnostic PDF and findings:",path,"\n")
