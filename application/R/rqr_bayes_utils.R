.rqr_bayes_schema <- function() {
  "rqrgibbs_bayes_shortest_uq/1.0.0"
}

.rqr_bayes_assert_probability <- function(x, name) {
  x <- as.numeric(x)[1L]
  if (!is.finite(x) || x <= 0 || x >= 1) {
    stop(sprintf("%s must be one finite scalar in (0, 1).", name),
         call. = FALSE)
  }
  x
}

.rqr_bayes_assert_positive <- function(x, name) {
  x <- as.numeric(x)[1L]
  if (!is.finite(x) || x <= 0) {
    stop(sprintf("%s must be one finite positive scalar.", name),
         call. = FALSE)
  }
  x
}

.rqr_bayes_assert_nonnegative <- function(x, name) {
  x <- as.numeric(x)[1L]
  if (!is.finite(x) || x < 0) {
    stop(sprintf("%s must be one finite nonnegative scalar.", name),
         call. = FALSE)
  }
  x
}

.rqr_bayes_clean_y <- function(y, na_rm = FALSE, name = "y") {
  y <- as.numeric(y)
  if (!length(y)) stop(sprintf("%s must be nonempty.", name), call. = FALSE)
  if (any(is.nan(y)) || any(is.infinite(y))) {
    stop(sprintf("%s may contain finite values or NA only.", name),
         call. = FALSE)
  }
  if (anyNA(y)) {
    if (!isTRUE(na_rm)) {
      stop(sprintf("%s contains NA; set na_rm = TRUE to remove missing values.",
                   name), call. = FALSE)
    }
    y <- y[!is.na(y)]
  }
  if (!length(y)) stop(sprintf("%s has no observed values.", name), call. = FALSE)
  y
}

.rqr_bayes_digest <- function(object) {
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(object, algo = "sha256", serialize = TRUE)
  } else {
    NA_character_
  }
}

.rqr_bayes_git_commit <- function() {
  out <- tryCatch(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_
  )
  if (!length(out) || is.na(out[[1L]])) NA_character_ else out[[1L]]
}

.rqr_bayes_provenance <- function(seed = NULL, extra = list()) {
  c(
    list(
      schema_version = .rqr_bayes_schema(),
      software_commit = .rqr_bayes_git_commit(),
      R_version = as.character(getRversion()),
      rng_kind = RNGkind(),
      master_seed = if (is.null(seed)) NA_integer_ else as.integer(seed)
    ),
    extra
  )
}

.rqr_bayes_dirichlet <- function(alpha) {
  alpha <- as.numeric(alpha)
  if (!length(alpha) || any(!is.finite(alpha)) || any(alpha <= 0)) {
    stop("Dirichlet parameters must be finite and positive.", call. = FALSE)
  }
  x <- stats::rgamma(length(alpha), shape = alpha, rate = 1)
  x / sum(x)
}

