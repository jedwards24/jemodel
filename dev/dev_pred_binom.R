#Format glmnet predictions
#(extend to multinomials?)

gmn_predict_binom <- function(fit, ...) {
  classnames <- fit$glmnet.fit$classnames
  pred_event <- predict(fit, ...)[, 1]
  pred_mat <- cbind(1 - pred_event, pred_event)
  colnames(pred_mat) <-  classnames
  pred_mat
}
gmn_predict_binom(glmb, newx = xtest, type = "response") %>% head
predict(glmb, xtest, type = "response")[, 1] %>% head
classnames <- glmb$glmnet.fit$classnames
event <- classnames[2]
other <- classnames[1]
pred_event <- predict(glmb, xtest, type = "response")[, 1]
tibble({{event}} := pred_event, {{other}} := 1 - pred_event)

# Returns tibble.
# Do not pass `type` to ...
# Default arg `type = "response"`?
gmn_predict_binom <- function(fit, newx, newy, ...) {
  classnames <- glmb$glmnet.fit$classnames
  event <- classnames[2]
  other <- classnames[1]
  pred_event <- predict(fit, newx = newx, type = "response", ...)[, 1]
  tibble({{other}} := 1 - pred_event,
         {{event}} := pred_event,
         truth = newy)
}
resg <- gmn_predict_binom(glmb, xtest, test$good)
both <- tibble(rf = resp$Good,
               glm = resg$Good,
               truth = resp$truth)

both %>%
  ggplot(aes(rf, glm, color = truth)) +
  geom_point()



#' Consistent formatting for binomial cv.glmnet predictions
#'
#' Gets response predictions from a binomial `cv.glmnet` model in the same form as predictions from
#' ranger models and as used by yardstick.
#'
#' @return A tibble of prediction probabilities with a column for each class.
#' @param fit A model fitted with `glmnet::cv.glmnet()`.
#' @param newx Matrix of `x` values at which predictions are to be made. See `glmnet::predict.glmnet()`.
glmnet_binomial_predict <- function(fit, newx) {
  if (!"cv.glmnet" %in% class(fit)){
    stop("`fit` must be a cv.glmnet object.", call. = FALSE)
  }
  if (!requireNamespace("glmnet", quietly = TRUE)){
    stop("Package \"glmnet\" must be installed to use this function with glmnet `fit`.", call. = FALSE)
  }
  if (!"classnames" %in% names(fit$glmnet.fit) | length(fit$glmnet.fit$classnames) != 2){
    stop("`fit` must be a binomial model.", call. = FALSE)
  }
  preds <- predict(fit, newx, type = "response")
  tb <- tibble(x1 = 1 - preds[, 1],
               x2 = preds[, 1])
  names(tb) <- fit$glmnet.fit$classnames
  tb
}

