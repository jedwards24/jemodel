#########################################################################################
# rang_roc_cut: ROC curve optimal cut for a ranger object.
#########################################################################################
#'
#' ROC curve optimal cut for a ranger object
#'
#' Calls `roc_cut()` for a fitted ranger model. See `roc_cut()` for details. Requires
#'  `probability = TRUE` to be used when the ranger model was fitted.
#'
#' The `rf$predictions` is a matrix with columns for each class. The class corresponding to
#' the predictions in `actual` can be named in `class_name`. Default is to select the "maximum"
#' column name i.e. "TRUE" if classes are TRUE/FALSE and the largest number if numeric.
#'
#' @param rf A ranger fitted model.
#' @param actual A binary class target vector matching `rf`.
#' @param class_name String giving name of the class predicted in `actual`.
#' @param plot Produce a plot. Defaults to `TRUE`.
#'
#' @export
rang_roc_cut <- function(rf, actual, class_name = NULL, plot = TRUE) {
  if (!is.matrix(rf$predictions)){
    stop("'rf$predictions' is not a matrix. When fitting the ranger model 'rf', you must use `probability = TRUE`.",
         call. = FALSE)
  }
  if (is.null(class_name)){
    class_name <- max(colnames(rf$predictions))
    message("Using ", class_name, " as positive class.")
  }else{
    if (!(class_name %in% colnames(rf$predictions))){
      stop("The column '", class_name,  "' does not exist in 'rf$predictions'.", call. = FALSE)
    }
  }
  roc_cut(rf$predictions[, class_name], actual, plot = plot)
}

# Fits a series of random forest models using the ranger package with different values of mtry, as given in m_vec.
# Returns a table of oob and test errors.

#########################################################################################
# rang_mtry: Tune ranger models for mtry.
#########################################################################################
#'
#' Tune ranger models for `mtry`
#'
#' Fits `ranger()` models for a given range of values of `mtry`. Output is a table and graph giving
#' errors for OOB training data and optional validation data.
#'
#' @param data A data frame containing both input and target variables.
#' @param fmla A formula for the model.
#' @param m_vec Integer vector of values of tuning parameter `mtry`.
#' @param train Optional integer vector giving row indices to be used in training set. Remaining rows are used for
#'   validation. If default `train = NULL` is used then all data is used for training and there is no validation.
#' @param seed Integer. Random number seed used for fitting each model.
#' @param importance,num.trees,respect.unordered.factors Optional arguments passed to `ranger()`. Defaults are
#'   `"impurity"`, `500` and `TRUE` respectively.
#'
#' @export
rang_mtry <- function(data, fmla, m_vec, train = NULL, seed = 1, importance = "impurity", num.trees = 500,
                      respect.unordered.factors = TRUE) {
  if (!is.atomic(m_vec) | !is.numeric(m_vec) | length(m_vec) == 0) stop("`m_vec` must be a non-empty numeric vector.",
                                                                        call. = FALSE)
  n_inputs <- length(attributes(stats::terms(fmla, data = data))$term.labels)
  if (max(m_vec) > n_inputs) stop("`mtry` cannot be larger than the number of input variables.\n The max `m_vec` is ",
                                  max(m_vec),
                                  ", but there are ",
                                  n_inputs, " input variables in the model.")
  if (is.null(train)) train <- 1 : nrow(data)
  target_name <- fmla[[2]]
  valid <- dplyr::pull(data, !!target_name)[-train]
  target <- dplyr::pull(data, !!target_name)[train]
  nn <- length(m_vec)
  oob_err <- double(nn)
  valid_err <- double(nn)
  time <- double(nn)
  cat("mtry completed: ")
  for(i in 1 : nn) {
    mtry <- m_vec[i]
    set.seed(seed)
    time[i] <- system.time(rf <- ranger::ranger(fmla, data = data[train, ],
                                        importance = importance, num.trees = num.trees, mtry = mtry,
                                        respect.unordered.factors = respect.unordered.factors))[3]
    oob_err[i] <- rf$prediction.error
    if (length(valid) > 0) {
      pred <- predict(rf, data[-train, ])$predictions
      if(is.factor(target)){
        valid_err[i] <- mean(valid != pred)
      }else{
        valid_err[i] <- mean((valid - pred) ^ 2)
      }
    }
    cat(mtry," ")
  }
  cat("\n")
  res <- tibble::tibble(mtry = m_vec, valid_err, oob_err, time)
  if (length(valid) == 0) res <- dplyr::select(res, -valid_err)
  reslong <- tidyr::gather(res, key = "metric", value = "error", -time, -mtry)
  g <- ggplot2::ggplot(reslong, ggplot2::aes(x = .data$mtry, y = .data$error, colour = .data$metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point()
  print(g)
  res
}

#########################################################################################
# oob_errors: Helper for rang_oob_err().
#########################################################################################
#'
#' Helper for `rang_oob_err()`
#'
#' Returns vector of error indicators chosen by majority vote. Where the vote is split the entry wil be 0.5.
#' The entry will be `NaN` Where there are no out-of-bag predictions for a data point.
#'
#' @param oob_mat Matrix of out-of-bag individual tree class predicitions with an `NA` where the prediciton
#'   was in bag. Rows are observation, columns are trees.
#' @param n_trees Positive integer. Gives the number of trees to be used in the prediction (columns `1:n_trees`
#'   of `oob_mat` will be used).
#' @param target Vector of true classes (either 1 or 2).
#'
#' @keywords internal
oob_errors <- function(oob_mat, n_trees, target) {
  oob_mat2 <- oob_mat[, 1:n_trees, drop = FALSE]
  err_mat <- sweep(oob_mat2, 1, target, FUN = `!=`)
  err_vec <- rowMeans(err_mat, na.rm = TRUE)
  splits <- err_vec == 0.5
  err_vec <- round(err_vec)
  err_vec[splits] <- 0.5
  err_vec
}

#########################################################################################
# rang_oob_err: Out-of-bag error rates by number of trees for a ranger random forest.
#########################################################################################
#'
#' Out-of-bag error rates by number of trees for a ranger random forest
#'
#' Returns a table of out-of-bag error rates for a ranger random forest using number of trees
#' from 5 to all trees in steps of 5. The supplied ranger object must have been created with
#'  `keep.inbag = TRUE` argument.
#'
#' @param rf A ranger random forest object, created with `keep.inbag = TRUE`.
#' @param data A data frame used to fit `rf`. Must contain target variable.
#' @param start,by Error rates are evaluated for number of trees from `start` to maximum number
#'   of trees in steps of `by`.
#' @param plot Optional logical. Output a plot or not.
#'
#' @export
rang_oob_err <- function(rf, data, start = 5L, by = 5L, plot = TRUE) {
  if (!("inbag.counts" %in% names(rf))){
    stop("`inbag.counts` not found in `rf`. Must use `keep.inbag = T` when fitting `rf`.", call. = FALSE)
  }
  nn <- nrow(data)
  ntr <- rf$num.trees
  n_trees_vec <- seq(start, ntr, by = by)
  fmla <- as.character(rf$call)[2]
  target_name <- stringr::str_split(fmla, " ~")[[1]][1]
  target <- data[[target_name]] %>% as.numeric() #use classes in {1,2}

  # Convert inbag counts (list) to matrix
  inbag_mat <- matrix(0, nrow = nn, ncol = ntr)
  for (i in 1 : ntr){
    inbag_mat[, i] <- rf$inbag.counts[[i]]
  }
  oob_mat <- dplyr::if_else(inbag_mat > 0, NA_real_, predict(rf, data, predict.all = T)$predictions) %>%
    matrix(nrow = nrow(inbag_mat))

  len_ntv <- length(n_trees_vec)
  errs <- matrix(0L, nrow = nn, ncol = len_ntv)
  for (i in 1: len_ntv){
    errs[, i] <- oob_errors(oob_mat, n_trees = n_trees_vec[i], target = target)
  }
  res <- tibble::tibble(num.trees = n_trees_vec,
                total = colMeans(errs, na.rm = TRUE),
                class_1 = colMeans(errs[target == 1, ], na.rm = T),
                class_2 = colMeans(errs[target == 2, ], na.rm = T)
  )
  res_long <- tidyr::gather(res, key = "pred", value = "error_rate", -.data$num.trees)
  if (plot){
    g <- ggplot2::ggplot(res_long, ggplot2::aes(x = .data$num.trees, y = .data$error_rate, color = .data$pred)) +
      ggplot2::geom_line() +
      ggplot2::ylab("OOB Error Rate")
    print(g)
  }
  res
}
