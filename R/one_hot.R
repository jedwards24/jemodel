# NOTES:
# I need to export `contrast_one_hot()` because of the way it is supplied to `one-hot()` (as a
# string name).

#' Model matrix with one-hot encoding for unordered factors
#'
#' This replicates `model.matrix()` but with one-hot encoding
#' for unordered factors. Ordered factors use the default contrasts (as
#' set in `options("contrasts")`. By default, the first column of the model matrix
#'  (the intercept) is removed.
#'
#' The contrast function used is `contrast_one_hot()`.
#'
#' @param formula A model formula passed to `model.matrix()`.
#' @param data A data frame passed to `model.matrix()`.
#' @param intercept If `FALSE` (the default) then the first column of the matrix is
#' removed before returning/ If `TRUE` then the returned matrix is unaffected.
#' @return A matrix (as from `model.matrix()`).
#' @export
one_hot <- function(formula, data, intercept = FALSE) {
  contr <- options("contrasts")$contrasts
  contr["unordered"] <- "contrast_one_hot"
  rlang::with_options(
    .expr = {
      mm <- model.matrix(formula, data)
    },
    contrasts = contr
  )
  if (intercept) return(mm)
  mm[, -1]
}

#' Contrast function for one-hot encodings
#'
#' This contrast function produces a model matrix that has indicator columns for
#' each level of each factor.
#'
#' Only intended to be used in `one_hot()`. Copy of `hardhat:::contr_one_hot()`.
#'
#' @param n A vector of character factor levels or the number of unique levels.
#' @param contrasts Only the default of `TRUE` is supported.
#' @param sparse Only the default of `FALSE` is supported.
#'
#' @return A diagonal matrix that is `n`-by-`n`.
#' @keywords internal
#' @export
contrast_one_hot <- function(n, contrasts = TRUE, sparse = FALSE) {
  if (sparse) {
    rlang::warn("`sparse = TRUE` not implemented for `contrast_one_hot()`.")
  }
  if (!contrasts) {
    rlang::warn("`contrasts = FALSE` not implemented for `contrast_one_hot()`.")
  }

  if (is.character(n)) {
    names <- n
    n <- length(names)
  } else if (is.numeric(n)) {
    n <- as.integer(n)
    if (length(n) != 1L) {
      rlang::abort("`n` must have length 1 when an integer is provided.")
    }
    names <- as.character(seq_len(n))
  } else {
    rlang::abort("`n` must be a character vector or an integer of size 1.")
  }

  out <- diag(n)
  rownames(out) <- names
  colnames(out) <- names
  out
}
