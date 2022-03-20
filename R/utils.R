#' Standardise or normalise a numeric vector
#'
#' @description
#'
#' * `standardise()` centres and scales `x` to \eqn{(x - mean(x)) / sd(x)}. Output has mean 0
#'   and standard deviation 1.
#' * `normalise()` normalises `x` to \eqn{(x - min(x)) / (max(x) - min(x))}. Output is
#'   in \[0,1\].
#'
#' Missing values are ignored in all calculations but are included in the returned vector.
#'
#' `base::scale()` is similar to `standardise()` but adds attributes
#' @param x A numeric vector.
#'
#' @export
standardise <- function(x) {
  if (!is.numeric(x) | !is.atomic(x))
    stop("`x` must be an atomic numeric vector.", call. = FALSE)
  if (length(stats::na.omit(x)) < 2)
    stop("`x` must have at least two non-missing values.", call. = FALSE)
  (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
}

#' @rdname standardise
#' @export
normalise <- function(x) {
  if (!is.numeric(x) | !is.atomic(x))
    stop("`x` must be an atomic numeric vector", call. = FALSE)
  if (length(stats::na.omit(x)) < 2)
    stop("`x` must have at least two non-missing values.", call. = FALSE)
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#' Vectorised inverse logit function
#'
#' Calculate the inverse logit function \eqn{exp(x) / (1 + exp(x))}. Modified from Faraway package.
#' I added a check on `x` to avoid `NaN` in the output, which would occur when `x` is greater than
#' about 750. Since \eqn{exp(x) / (1 + exp(x)) = 1} for \eqn{x>=20}, I only check for x > 20 and
#' output 1 for these cases.
#'
#' @param x A numeric vector.
#' @return exp(x)/(1+exp(x))
#'
#' @export
ilogit <- function(x){
  if(any(omit <- is.na(x))){
    lv <- x
    lv[omit] <- NA
    if(any(!omit))
      lv[!omit] <- Recall(x[!omit])
    return(lv)
  }
  ifelse(x > 20, 1, exp(x)/(1 + exp(x)))
}
