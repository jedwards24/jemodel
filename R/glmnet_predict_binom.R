#' Consistent formatting for binomial cv.glmnet predictions
#'
#' Gets response predictions from a binomial `cv.glmnet` model in the same form as predictions from
#' ranger models and as used by yardstick.
#'
#' @return A tibble of prediction probabilities with a column for each class.
#' @param fit A model fitted with `glmnet::cv.glmnet()`.
#' @param newx Matrix of new values for `x` at which predictions are to be made. See `glmnet::predict.glmnet()`.
#' @param type Passed to `predict()` with default "response". See `?glmnet::predict.glmnet`.
#' @param ... Other arguments passed to `predict()`.
#' @export
glmnet_predict_binom <- function(fit, newx, type = "response", ...) {
  if (!"cv.glmnet" %in% class(fit)){
    stop("`fit` must be a cv.glmnet object.", call. = FALSE)
  }
  if (!requireNamespace("glmnet", quietly = TRUE)){
    stop("Package \"glmnet\" must be installed to use this function with glmnet `fit`.", call. = FALSE)
  }
  if (!"classnames" %in% names(fit$glmnet.fit) | length(fit$glmnet.fit$classnames) != 2){
    stop("`fit` must be a binomial model.", call. = FALSE)
  }
  preds <- predict(fit, newx, type = "response", ...)[, 1]
  tb <- tibble(x1 = 1 - preds,
               x2 = preds)
  names(tb) <- fit$glmnet.fit$classnames
  tb
}
