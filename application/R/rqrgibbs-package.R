#' rqrgibbs: generalized Bayes interval regression
#'
#' The package implements fixed-design and dynamic samplers for the RQR
#' residual-product loss. Its pseudo-AL representation is a computational
#' augmentation of a loss kernel, not a response likelihood.
#'
#' @keywords internal
#' @useDynLib rqrgibbs, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"

utils::globalVariables(c("qdesn_fit_vb"))
