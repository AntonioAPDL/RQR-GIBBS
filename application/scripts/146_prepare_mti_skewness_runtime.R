#!/usr/bin/env Rscript
source_library <- Sys.getenv("MTI_SKEWNESS_SOURCE_LIBRARY", file.path(
  dirname(normalizePath(system2("git", c("rev-parse", "--git-common-dir"), stdout=TRUE))),
  ".codex_work/mti_ecm_adaptive_calibration_28950e67cccd814ecfdedd81f558b80e7472d14c/application/cache/adaptive_calibration_library"))
destination <- Sys.getenv("MTI_SKEWNESS_R_LIBRARY", "application/cache/skewness_library")
dir.create(destination, recursive=TRUE, showWarnings=FALSE)
.libPaths(c(normalizePath(source_library, mustWork=TRUE), .libPaths()))
installed <- installed.packages()
packages <- unique(c("rqrgibbs", "tolerance", "jsonlite", "digest", "data.table",
  unlist(tools::package_dependencies(c("rqrgibbs", "tolerance", "jsonlite", "digest", "data.table"),
    db=installed, which=c("Depends", "Imports", "LinkingTo"), recursive=TRUE))))
for (package in packages) {
  if (!package %in% rownames(installed) || !is.na(installed[package,"Priority"])) next
  from <- find.package(package)
  to <- file.path(destination, package)
  if (!dir.exists(to)) stopifnot(file.copy(from, destination, recursive=TRUE))
}
.libPaths(c(normalizePath(destination), .libPaths()))
sources <- file.path(dirname(destination), "skewness_package_sources")
dir.create(sources, recursive=TRUE, showWarnings=FALSE)
needed <- c("statmod", "actuar", "expint")
missing <- needed[!vapply(needed, function(p) file.exists(file.path(destination,p,"DESCRIPTION")), FALSE)]
if (length(missing)) {
  definitions <- jsonlite::read_json("application/config/mti_skewness_extension_20261121.json")$package_sources
  for (definition in definitions) if (definition$package %in% missing) {
    name <- paste0(definition$package,"_",definition$version,".tar.gz")
    archive <- file.path(sources,name)
    if (!file.exists(archive)) {
      ok <- tryCatch({download.file(paste0("https://cloud.r-project.org/src/contrib/",name),archive,mode="wb");TRUE},error=function(e)FALSE)
      if (!ok) download.file(paste0("https://cloud.r-project.org/src/contrib/Archive/",definition$package,"/",name),archive,mode="wb")
    }
    stopifnot(identical(digest::digest(file=archive,algo="sha256"),definition$sha256))
    install.packages(archive,lib=destination,repos=NULL,type="source",Ncpus=1)
  }
}
stopifnot(all(vapply(c(packages[packages %in% rownames(installed)],needed), requireNamespace, FALSE, quietly=TRUE)),
  as.character(packageVersion("tolerance")) == "3.0.0")
for (definition in jsonlite::read_json("application/config/mti_skewness_extension_20261121.json")$package_sources)
  stopifnot(packageVersion(definition$package)==package_version(definition$version))
files <- list.files(destination, recursive=TRUE, full.names=TRUE)
archives <- list.files(sources, full.names=TRUE)
record <- list(time=format(Sys.time(),tz="UTC",usetz=TRUE), source_library=normalizePath(source_library),
  source_commit=system2("git",c("rev-parse","HEAD"),stdout=TRUE),
  packages=as.list(setNames(vapply(unique(c(packages,needed))[unique(c(packages,needed)) %in% rownames(installed.packages())],
    function(p) as.character(packageVersion(p)), ""), unique(c(packages,needed))[unique(c(packages,needed)) %in% rownames(installed.packages())])),
  archive_hashes=as.list(setNames(vapply(archives,function(p)digest::digest(file=p,algo="sha256"),""),basename(archives))),
  installed_hashes=as.list(setNames(vapply(files,function(p)digest::digest(file=p,algo="sha256"),""),substring(files,nchar(destination)+2))))
jsonlite::write_json(record, file.path(dirname(destination),"skewness_runtime.json"), auto_unbox=TRUE,pretty=TRUE)
cat("Private numerical library prepared:",normalizePath(destination),"\n")
