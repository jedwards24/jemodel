#########################################################################################
# roc_cut: Optimal cut for ROC curve.
#########################################################################################
#'
#' Optimal cut for ROC curve
#'
#' Calculates the "optimal" cutoff for a classifier from a ROC curve. This is the cutoff that minimises the
#' distance from the point (FPR = 0, TPR = 1). Also returns sensitivity and specitivity for this cutoff, the AUC
#' of the ROC curve, and optionally plots the curve together with the distance measure.
#'
#' Adapted from <https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/>.
#'
#' @param pred A vector of prediction probabilities.
#' @param actual A vector of outcome classes corresponding to `pred`.
#' @param plot Logical indicating whether to plot a ROC curve together with distance from the optimal corner.
#' @importFrom ggplot2 aes
#' @export
roc_cut <- function(pred, actual, plot = T) {
  roc_pred <- ROCR::prediction(pred, actual)
  perf <- ROCR::performance(roc_pred, measure = "tpr", x.measure = "fpr")
  x <- perf@x.values[[1]]
  y <- perf@y.values[[1]]
  p <- roc_pred@cutoffs[[1]]
  d = x^2 + (y - 1)^2
  ind = which.min(d)

  # Plot
  if(plot){
    nn <- length(y)
    roc_dt <- tibble::tibble(x = rep(x, 2), key = c(rep("tpr", nn), rep("d", nn)), val = c(y, d))
    gg <- ggplot2::ggplot(roc_dt, aes(x = .data$x, y = .data$val)) +
      ggplot2::geom_line(aes(col = .data$key)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
      ggplot2::xlab("False Positive Rate") +
      ggplot2::ylab("") +
      ggplot2::scale_color_discrete(name = "",
                           breaks=c("tpr", "d"),
                           labels=c("TPR", "Distance"))
    print(gg)
  }
  c(sensitivity = y[[ind]],
    specificity = 1 - x[[ind]],
    cutoff = p[[ind]],
    auc = unlist(ROCR::performance(roc_pred, measure = "auc")@y.values))
}

#########################################################################################
# roc_plot: Plots ROC curves for training and test data.
#########################################################################################
#'
#' Plots ROC curves for training and test data.
#'
#' Returns a ggplot of a ROC curve from prediction and target vectors. These are partitioned by the argument
#' `train` into two separate curves so that differences between train and test data can be seen. AUC for each
#' is printed. Functions from ROCR are used.
#'
#' @param pred A vector of prediction probabilities.
#' @param target A vector of outcome classes corresponding to `pred`.
#' @param train A vector of indices indicating which predictions are "training" data.
#' @param test Optional vector of indices indicating which predictions are "test" data. Defaults to `NULL`, in
#'   which case `test` will be set to the data not in `train`.
#'
#' @importFrom ggplot2 aes
#' @export
roc_plot <- function(pred, target, train, test = NULL) {
  if (is.null(test)) {
    test <- setdiff(1 : length(target), train)
  }
  roc_pred_train <- ROCR::prediction(pred[train], target[train])
  roc_pred_test <- ROCR::prediction(pred[test], target[test])

  perf_train <- ROCR::performance(roc_pred_train, measure = "tpr", x.measure = "fpr")
  perf_test <- ROCR::performance(roc_pred_test, measure = "tpr", x.measure = "fpr")

  #auc
  cat("AUC:",
      unlist(ROCR::performance(roc_pred_train, measure = "auc")@y.values),
      "for the training set and",
      unlist(ROCR::performance(roc_pred_test, measure = "auc")@y.values),
      "for the test set\n"
  )

  # Plot
  n_train <- length(perf_train@x.values[[1]])
  n_test <- length(perf_test@x.values[[1]])
  roc_dt <- tibble::tibble(x = c(perf_train@x.values[[1]], perf_test@x.values[[1]]),
                           y = c(perf_train@y.values[[1]], perf_test@y.values[[1]]),
                           set = c(rep("train", n_train), rep("test", n_test))
  )
  ggplot2::ggplot(roc_dt, aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(aes(col = .data$set)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::xlab("False Positive Rate") +
    ggplot2::ylab("True Positive Rate")
}
