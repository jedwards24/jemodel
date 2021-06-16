# Place to keep older, unused code that might be useful



# This was used in rang_oob_err() but not now

#########################################################################################
# err_by_class: Error rate by class prediction.
#########################################################################################
#'
#' Error rate by true class
#'
#' Returns the rate of mismatches between entries of \code{pred_vec} that equal \code{class} and the
#' corresponding elements of \code{target}.
#'
#'
#' @param pred_vec Integer vector of class predictions.
#' @param target Integer vector of true classes.
#' @param class Integer indicating which class to measure.
#'
#' @export
err_by_class <- function(pred_vec, target, class = 1) {
  inds <- which(target == class)
  mean(pred_vec[inds] != target[inds], na.rm = T)
}
