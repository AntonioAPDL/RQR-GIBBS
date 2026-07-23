# Shared guards for treating the pinned exdqlm checkout as an immutable source.

rqr_readonly_git <- function(repo_root, args) {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("Git is required.", call. = FALSE)
  out <- suppressWarnings(system2(
    git,
    c("-C", shQuote(repo_root), args),
    stdout = TRUE,
    stderr = TRUE,
    env = c("GIT_OPTIONAL_LOCKS=0", "GIT_TERMINAL_PROMPT=0")
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L)) {
    stop(
      "Read-only Git command failed: git -C ",
      repo_root, " ", paste(args, collapse = " "),
      "\n", paste(out, collapse = "\n"),
      call. = FALSE
    )
  }
  paste(out, collapse = "\n")
}

rqr_path_within <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

rqr_assert_isolated_cache <- function(cache_root, repo_root, external_repo) {
  cache_base <- file.path(repo_root, "application", "cache")
  dir.create(cache_base, recursive = TRUE, showWarnings = FALSE)
  cache_base <- normalizePath(cache_base, winslash = "/", mustWork = TRUE)
  cache_root <- normalizePath(cache_root, winslash = "/", mustWork = FALSE)
  external_repo <- normalizePath(external_repo, winslash = "/", mustWork = TRUE)
  if (!rqr_path_within(cache_root, cache_base)) {
    stop(
      "RQR_EXDQLM_RUNTIME_ROOT must remain under the ignored RQR-owned ",
      "application/cache directory.",
      call. = FALSE
    )
  }
  if (rqr_path_within(cache_root, external_repo) ||
      rqr_path_within(external_repo, cache_root)) {
    stop("The runtime cache and exdqlm source checkout must be disjoint.", call. = FALSE)
  }
  cache_root
}

rqr_checkout_manifest_digest <- function(repo_root) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for the source checkout guard.", call. = FALSE)
  }
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  entries <- list.files(
    repo_root,
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  relative <- substring(entries, nchar(repo_root) + 2L)
  keep <- relative != ".git" & !startsWith(relative, ".git/")
  entries <- entries[keep]
  relative <- relative[keep]
  order_index <- order(relative)
  entries <- entries[order_index]
  relative <- relative[order_index]
  info <- file.info(entries)
  links <- Sys.readlink(entries)
  kinds <- ifelse(
    nzchar(links), "symlink",
    ifelse(info$isdir, "directory", "file")
  )
  hashes <- rep("", length(entries))
  regular <- kinds == "file"
  hashes[regular] <- vapply(
    entries[regular],
    function(path) digest::digest(
      file = path, algo = "sha256", serialize = FALSE
    ),
    character(1L)
  )
  payload <- paste(
    relative,
    kinds,
    sprintf("%o", as.integer(info$mode)),
    as.character(info$size),
    sprintf("%.9f", as.numeric(info$mtime)),
    links,
    hashes,
    sep = "\t",
    collapse = "\n"
  )
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}

rqr_capture_external_checkout <- function(repo_root) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for the source checkout guard.", call. = FALSE)
  }
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  state <- list(
    repo_root = repo_root,
    branch = trimws(rqr_readonly_git(repo_root, c(
      "rev-parse", "--abbrev-ref", "HEAD"
    ))),
    commit = tolower(trimws(rqr_readonly_git(
      repo_root, c("rev-parse", "HEAD")
    ))),
    tree = tolower(trimws(rqr_readonly_git(
      repo_root, c("rev-parse", "HEAD^{tree}")
    ))),
    status = rqr_readonly_git(
      repo_root,
      c("status", "--porcelain=v2", "--untracked-files=all")
    ),
    refs = rqr_readonly_git(
      repo_root, c("show-ref", "--head", "--dereference")
    ),
    local_config = rqr_readonly_git(
      repo_root, c("config", "--local", "--list", "--show-origin")
    ),
    checkout_manifest = rqr_checkout_manifest_digest(repo_root)
  )
  state$guard_digest <- digest::digest(
    paste(
      state$branch,
      state$commit,
      state$tree,
      state$status,
      state$refs,
      state$local_config,
      state$checkout_manifest,
      sep = "\n"
    ),
    algo = "sha256",
    serialize = FALSE
  )
  state
}

rqr_assert_external_checkout_unchanged <- function(before) {
  after <- rqr_capture_external_checkout(before$repo_root)
  fields <- c(
    "branch", "commit", "tree", "status", "refs", "local_config",
    "checkout_manifest", "guard_digest"
  )
  changed <- fields[!vapply(
    fields,
    function(field) identical(before[[field]], after[[field]]),
    logical(1L)
  )]
  if (length(changed)) {
    stop(
      "The protected exdqlm checkout changed during isolated RQR work: ",
      paste(changed, collapse = ", "),
      ". No result from this operation is eligible for use.",
      call. = FALSE
    )
  }
  after
}

rqr_exdqlm_runtime_layout <- function(
    repo_root, exdqlm_repo, pinned_commit, cache_root = NULL) {
  if (is.null(cache_root)) {
    cache_root <- Sys.getenv(
      "RQR_EXDQLM_RUNTIME_ROOT",
      unset = file.path(repo_root, "application", "cache", "exdqlm_runtime")
    )
  }
  cache_root <- rqr_assert_isolated_cache(
    cache_root, repo_root = repo_root, external_repo = exdqlm_repo
  )
  list(
    cache_root = cache_root,
    library_root = file.path(cache_root, "library"),
    git_archive = file.path(
      cache_root,
      paste0("exdqlm_git_", substr(pinned_commit, 1L, 12L), ".tar.gz")
    ),
    attestation_path = file.path(
      cache_root,
      "attestations",
      paste0("exdqlm_", substr(pinned_commit, 1L, 12L), ".rds")
    )
  )
}
