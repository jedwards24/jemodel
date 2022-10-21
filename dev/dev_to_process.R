
# Return table of AUC and number of non-zero coefs for each lambda in `fit`.
# `newx` a model matrix of test data and `truth` a logical vector of the corresponding y values
glmnet_summarise <- function(fit, newx, truth) {
  aucs <- map_dbl(fit$lambda, auc_lambda, fit = fit, newx = newx, truth = truth)
  nz <- tibble(s = fit$lambda, log_s = log(fit$lambda), nonzero = fit$nzero, auc = aucs)
}

# Return auc for a glmnet model with given s (lambda)
auc_lambda <- function(s, fit, newx, truth) {
  predg <- predict(fit, newx, s = s)
  auc(predg[, 1], truth)
}

# Returns vector of predictions for cv.glmnet, ranger, or XGB models
# `data` is a data frame of test data.
# `fn` is an optional transform for the predictions.
get_predictions <- function(fit, data = NULL, xmat = NULL, fn = NULL) {
  if (!class(fit) %in% c("ranger", "xgb.Booster", "cv.glmnet")){
    stop("`fit` must be either a ranger, cv.glmnet, or xgb.Booster object. Class is ",
         class(fit), call. = FALSE)
  }
  if("ranger" %in% class(fit)){
    if (is.null(data)) stop("Must supply `data` for ranger model", call. = FALSE)
    preds <- predictions(predict(fit, data))
  }
  if("xgb.Booster" %in% class(fit)){
    if (is.null(xmat)) stop("Must supply `xmat` for xgb.Booster model", call. = FALSE)
    preds <- predict(fit, newdata = xmat)
  }
  if("cv.glmnet" %in% class(fit)){
    if (is.null(xmat)) stop("Must supply `xmat` for cv.glmnet model", call. = FALSE)
    predict(fit, xmat, ...)[, 1]
  }
  if (is.null(fn)) return(preds)
  eval(call(fn, preds))
}

# given output of xgb.cv, this returns the earliest
# iteration where there were `rounds` consecutive iterations with no improvement
# Returns vector of length two of iteration number for training and test sets.
early_stop <- function(x, rounds) {
  diffs <- as_tibble(xgb$evaluation_log) %>%
    select(contains("_mean")) %>%
    mutate(across(contains("_mean"), ~. - lead(.), .names = "{.col}_diff"), .keep = "unused") %>%
    rename_all(~str_remove_all(., "_rmse_mean"))

  mutate(diffs, across(everything(),
                       ~slider::slide_sum(. == 0, before = rounds - 1))) %>%
    summarise(across(everything(), ~{which(. == rounds)[1]})) %>%
    rename_all(~str_remove(., "_diff")) %>%
    unlist()
}

# Line plot of test and train score by iteration.
# Plots from iteration `start` to better scale y axis.
plot_xgb_cv <- function(xgb, start = 100) {
  as_tibble(xgb$evaluation_log) %>%
    select(iter, train_rmse_mean, test_rmse_mean) %>%
    pivot_longer(-iter, names_to = "set", values_to = "rmse") %>%
    mutate(set = as.factor(str_remove(set, "_rmse_mean"))) %>%
    filter(iter > start) %>%
    ggplot(aes(iter, rmse, colour = set, group = set)) +
    geom_line()
}

rmse <- function(x) {
  sqrt(mean(x ^ 2))
}

rmse_data <- function(data, pred, actual) {
  diff <- pull(data, {{pred}}) - pull(data, {{actual}})
  rmse(diff)
}

quantile_diff <- function(data, pred, actual, ...) {
  diff <- pull(data, {{pred}}) - pull(data, {{actual}})
  quantile(diff, ...)
}

# Fit multiple xgb models using parameters in a table

# expers is table with col per param and row per experiment

# This runs a single experiment.
# Loop over exper_row in expers to do all.
run_xgb <- function(target_vec, xmat, expers, exper_row) {
  params <- list(max_depth = expers$max_depth[exper_row],
                 etc,
                 tree_method = "hist",
                 other_fixed_params)
  set.seed(1)
  xgb.cv(params = params) # and other args
  # could return list with runtime too
}

# Summarise results of expers
# res_list is list of model, run times, and expers
exper_summary <- function(res_list) {
  log_list <- map(res_list[["models"]], ~pluck(., "evaluation_log"))
  res_list[["expers"]] %>%
    # map runtimes, nrounds, rmse, rmse_500
    #...
    mutate(rmse_500 = map2_dbl(log_list, nrounds, ~pull(.x, test_rmse_mean)[min(.y, 500)]))
}

# Gives metrics for different seeds which change the CV split in cv.glmnet().
# train_rows and test_rows are integer vectors.
gmn_cv <- function(dt, fmla, in_vars, seed_vec, nfolds = 5, train_rows, test_rows) {
  target_name <- fmla[[2]]
  dt_train <- dt %>%
    slice(train_rows) %>%
    select(!!target_name, all_of(in_vars))
  dt_test <- dt %>%
    slice(test_rows) %>%
    select(!!target_name, all_of(in_vars))
  x_mat <- model.matrix(fmla, dt_train)[, -1]
  n <- length(seed_vec)
  mets_list <- vector('list', n)
  for (j in seq_along(seed_vec)){
    set.seed(seed_vec[j])
    fit <- cv.glmnet(y=pull(dt_train, !!target_name), x=x_mat, family="binomial", nfolds = nfolds)
    mets_list[[j]] <- c(seed = seed_vec[j], gmn_metrics(fit, dt_test, fmla))
    cat(j)
  }
  cat("\n")
  map(mets_list, as_tibble_row) %>% bind_rows()
}

# Model and test metrics for a glmnet model and new data
gmn_metrics <- function(fit, dt_test, fmla) {
  model_mat <- model.matrix(fmla, dt_test)[, -1]
  s_min = "lambda.min"
  s_1se = "lambda.1se"
  scores_min <- gmn_test(fit, dt_test, fmla, model_mat, s = s_min) %>% suffix_name("_min")
  scores_1se <- gmn_test(fit, dt_test, fmla, model_mat, s = s_1se) %>% suffix_name("_1se")
  mets_min <- gmn_model_metrics(fit, s_min) %>% suffix_name("_min")
  mets_1se <- gmn_model_metrics(fit, s_1se) %>% suffix_name("_1se")
  c(mets_min, scores_min, mets_1se, scores_1se)
}

# Returns auc and mse for a given glmnet model and data (specific lambda)
gmn_test <- function(fit, dt_test, fmla, model_mat, s = "lambda.min") {
  preds <- as.vector(predict(fit, newx = model_mat, type = "response", s = s))
  target_name <- fmla[[2]]
  actual <- pull(dt_test, !!target_name)
  mse <- mean((preds - factor_to_numeric(actual)) ^ 2)
  c(auc = roc_auc(preds, actual), mse = mse)
}

# Return AUC for prediction and actual vectors
roc_auc <- function(pred, actual) {
  roc_pred <- ROCR::prediction(pred, actual)
  unlist(ROCR::performance(roc_pred, measure = "auc")@y.values)
}

# nzero and cvm for a glmnet model as a named vector (nvar and dev)
gmn_model_metrics <- function(fit, s = "lambda.min") {
  x <- c(fit$nzero[which(fit$lambda == fit[s])],
         fit$cvm[which(fit$lambda == fit[s])])
  names(x) <- c("nvar", "dev")
  x
}

# Add a suffix to the names of a vector
suffix_name <- function(x, suffix) {
  if (is.null(names(x))) stop("Vector is not named so can't add suffix.", call. = FALSE)
  names(x) <- paste0(names(x), suffix)
  x
}

# variables included in models with different cv seeds.
# Could summarise output or use separate func
gmn_vars <- function(dt, fmla, seed_vec, nfolds = 5, s = "lambda.min") {
  x_mat <- model.matrix(fmla, dt)[, -1]
  n <- length(seed_vec)
  res_vars <- vector('list', n)
  for (j in seed_vec){
    set.seed(j)
    fit <- cv.glmnet(y=dt$target, x=x_mat, family="binomial", nfolds = nfolds)
    res_vars[[j]] <- glmnet_to_table(fit, s = s, min_coef = 0) %>%
      arrange(name) %>%
      mutate(seed = j)
    cat(j)
  }
  cat("\n")
  bind_rows(res_vars)
}

#RF fit and test
#Best to do this over various seeds and average
rf_test <- function(dt_test, ...) {
  fit <- ranger(...)
  preds <- predict(fit, dt_test)$predictions
  preds <- preds[, max(colnames(preds))]
  actual <- dt_test$is_save
  mse <- mean((preds - factor_to_numeric(actual)) ^ 2)
  names(mse) <- "mse"
  roc <- roc_cut(preds, actual, plot = F)
  c(roc, mse, oob.error = fit$prediction.error)
}
