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
#' @param x A numeric vector.
#'
#' @export
# base::scale() is a similar option but adds attributes
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

