msp_require <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

msp_inside <- function(path, root) startsWith(path, paste0(root, "/"))
msp_hash <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)

msp_run <- function(command, args, directory, logfile, env = character()) {
  old <- setwd(directory); on.exit(setwd(old))
  status <- system2(command, shQuote(args), stdout = logfile, stderr = logfile, env = env)
  msp_require(identical(as.integer(status), 0L), paste("Command failed; inspect", logfile))
}

msp_git <- function(root, args) {
  output <- suppressWarnings(system2("git", shQuote(c("--no-optional-locks", "-C", root, args)),
    stdout = TRUE, stderr = TRUE))
  msp_require(is.null(attr(output, "status")), paste(output, collapse = "\n"))
  output
}

msp_inputs <- function(recorder, source, build, stem) {
  source <- normalizePath(source, mustWork = TRUE)
  build <- normalizePath(build, mustWork = TRUE)
  lines <- readLines(recorder, warn = FALSE)
  pwd <- unique(substring(lines[startsWith(lines, "PWD ")], 5L))
  msp_require(length(pwd) == 1L, "Recorder must identify one working directory.")
  input <- unique(substring(lines[startsWith(lines, "INPUT ")], 7L))
  files <- character()
  for (item in input) {
    absolute <- if (startsWith(item, "/")) item else file.path(pwd, item)
    msp_require(file.exists(absolute), paste("Recorder input is missing:", item))
    path <- normalizePath(absolute, mustWork = TRUE)
    if (msp_inside(path, source)) {
      relative <- substring(path, nchar(source) + 2L)
      msp_require(tools::file_ext(relative) %in% c("tex", "sty", "cls", "bib", "bst",
        "png", "pdf", "jpg", "jpeg", "eps", "def", "cfg"),
        paste("Unexpected project input:", relative))
      files <- c(files, setNames(path, relative))
    } else if (identical(path, file.path(build, paste0(stem, ".bbl")))) {
      files <- c(files, setNames(path, paste0(stem, ".bbl")))
    }
  }
  files <- files[!duplicated(names(files))]
  msp_require(all(c(paste0(stem, ".tex"), paste0(stem, ".bbl")) %in% names(files)),
    "Recorder is missing the root document or generated bibliography.")
  files
}

msp_build <- function(source, build, stem) {
  msp_require(dir.create(build, recursive = TRUE), paste("Build directory already exists:", build))
  args <- c("-no-shell-escape", "-halt-on-error", "-interaction=nonstopmode", "-recorder",
    paste0("-output-directory=", build), paste0(stem, ".tex"))
  msp_run("pdflatex", args, source, file.path(build, "pdflatex-1.txt"), env = "TEXINPUTS=.:")
  msp_run("bibtex", stem, build, file.path(build, "bibtex.txt"),
    env = paste0("BIBINPUTS=", shQuote(paste0(source, .Platform$path.sep))))
  for (pass in 2:3) msp_run("pdflatex", args, source, file.path(build, paste0("pdflatex-", pass, ".txt")),
    env = "TEXINPUTS=.:")
  log <- paste(readLines(file.path(build, paste0(stem, ".log")), warn = FALSE), collapse = " ")
  msp_require(!grepl("undefined|multiply defined|Label\\(s\\) may have changed|Rerun to get|Missing character|Overfull \\\\[hv]box",
    log, ignore.case = TRUE), paste("Unresolved manuscript diagnostics; inspect", build))
  msp_inputs(file.path(build, paste0(stem, ".fls")), source, build, stem)
}

msp_copy <- function(files, destination) {
  msp_require(length(files) > 0L && !is.null(names(files)) && !anyDuplicated(names(files)) &&
    all(nzchar(names(files))) && all(file.exists(files)), "Invalid source-package file set.")
  msp_require(all(!startsWith(names(files), "/") &
    !vapply(strsplit(names(files), "/", fixed = TRUE), function(x) any(x == ".."), logical(1))),
    "Source-package paths must stay inside their destination.")
  for (relative in names(files)) {
    path <- file.path(destination, relative)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    msp_require(!file.exists(path) && file.copy(files[[relative]], path), paste("Could not copy", relative))
    msp_require(identical(msp_hash(path), msp_hash(files[[relative]])), paste("Copied file differs:", relative))
  }
}

msp_prepare <- function(root, include_supplement = FALSE, stamp = format(Sys.time(),
  "%Y%m%d_%H%M%S", tz = "America/Los_Angeles")) {
  root <- normalizePath(root, mustWork = TRUE)
  msp_require(is.logical(include_supplement) && length(include_supplement) == 1L && !is.na(include_supplement),
    "include_supplement must be TRUE or FALSE.")
  msp_require(length(stamp) == 1L && grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,63}$", stamp), "Invalid package stamp.")
  for (tool in c("git", "pdflatex", "bibtex", "zip"))
    msp_require(nzchar(Sys.which(tool)), paste("Required tool is missing:", tool))
  for (package in c("digest", "jsonlite"))
    msp_require(requireNamespace(package, quietly = TRUE), paste("Required R package is missing:", package))
  msp_require(length(msp_git(root, c("status", "--porcelain", "--untracked-files=no"))) == 0L,
    "Commit tracked changes before preparing an exact-source manuscript package.")
  programs <- c("application/scripts/31_prepare_arxiv_source.sh",
    "application/scripts/115_prepare_mti_manuscript_source.R",
    "application/scripts/lib/manuscript_source_package.R")
  msp_git(root, c("ls-files", "--error-unmatch", "--", programs))
  commit <- msp_git(root, c("rev-parse", "HEAD"))
  kind <- if (include_supplement) "with_supplement" else "main"
  output <- file.path(root, "application/cache", paste0("mti_source_", kind, "_", substr(commit, 1, 12), "_", stamp))
  msp_require(!file.exists(output) && dir.create(output, recursive = TRUE),
    paste("Output directory already exists or cannot be created:", output))
  source <- file.path(output, "committed_source"); dir.create(source)
  archive <- file.path(output, "committed_source.tar")
  reference <- "docs/reproducibility/mti_completed_study_reference_20261113.md"
  msp_run("git", c("--no-optional-locks", "archive", "--format=tar", paste0("--output=", archive), commit,
    "--", "main.tex", "rqr-gibbs-supplement.tex", "refs.bib", "tables", "figures", reference),
    root, file.path(output, "git-archive.txt"))
  utils::untar(archive, exdir = source)
  stems <- c("main", if (include_supplement) "rqr-gibbs-supplement")
  files <- setNames(file.path(source, "refs.bib"), "refs.bib")
  for (stem in stems) {
    build <- file.path(output, paste0("build_", stem))
    found <- msp_build(source, build, stem)
    overlap <- intersect(names(files), names(found))
    msp_require(all(vapply(overlap, function(x) identical(msp_hash(files[[x]]), msp_hash(found[[x]])), logical(1))),
      "Root documents used different versions of a shared source file.")
    files <- c(files, found[setdiff(names(found), names(files))])
    msp_require(file.copy(file.path(build, paste0(stem, ".pdf")), file.path(output, paste0(stem, ".pdf"))),
      "Could not copy the review PDF.")
  }
  package <- file.path(output, "source_upload"); dir.create(package)
  msp_copy(files, package)
  writeLines(c("Mean-Tilted Intervals: Short Tolerance Intervals", paste("Source commit:", commit),
    "Compiler: PDFLaTeX with BibTeX; generated bibliographies are included.",
    paste("Root documents:", paste(paste0(stems, ".tex"), collapse = ", ")),
    "This source upload contains manuscript dependencies only. Review PDFs are separate.",
    "No raw chains, private data, caches, local plans, or raw simulation records are included."),
    file.path(package, "README.txt"))
  package_files <- sort(list.files(package, recursive = TRUE))
  writeLines(vapply(package_files, function(x) paste(msp_hash(file.path(package, x)), x), ""),
    file.path(package, "SOURCE_MANIFEST.txt"))
  zip <- file.path(output, paste0("mti_arxiv_source_", kind, "_", substr(commit, 1, 12), "_", stamp, ".zip"))
  msp_run("zip", c("-X", "-q", zip, sort(list.files(package, recursive = TRUE))), package, file.path(output, "zip.txt"))
  writeLines(paste(msp_hash(zip), basename(zip)), paste0(zip, ".sha256"))
  msp_require(file.copy(file.path(source, reference), file.path(output, "COMPLETED_STUDY_REFERENCE.md")),
    "Could not copy the completed-study reference.")
  evidence <- jsonlite::read_json(file.path(source, "figures/data/mti_gibbs_full_20261113/summary.json"))
  jsonlite::write_json(list(source_commit = commit, sampling_commit = evidence$sampling_commit,
    root_documents = paste0(stems, ".tex"), project_files = as.list(setNames(vapply(files, msp_hash, ""), names(files))),
    archive = basename(zip), archive_sha256 = msp_hash(zip), committed_source_sha256 = msp_hash(archive),
    review_pdfs = as.list(setNames(vapply(stems, function(x) msp_hash(file.path(output, paste0(x, ".pdf"))), ""), stems)),
    draws_required = FALSE, simulations_launched = FALSE, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)),
    file.path(output, "source_receipt.json"), pretty = TRUE, auto_unbox = TRUE)
  list(directory = output, zip = zip, root_documents = stems, source_commit = commit, files = names(files))
}
