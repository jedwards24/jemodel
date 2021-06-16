# glmnet and pdp copied funcs

##############
# Helper used in glmnet_to_table_mult function.
# Given a table of coef of a glmnet fit, returns a table of only the variables
# with coef > `min_coef`.
##############
coef_to_table <- function(ce, min_coef = 1E-10) {
  coef_mat <- as.matrix(ce)
  var_names <- rownames(coef_mat)
  tibble(vars = rownames(coef_mat), coef = coef_mat[, 1]) %>%
    arrange(desc(coef)) %>%
    filter(abs(coef) >= min_coef)
}

############
# As glmnet_to_table but for multinomial fits. Returns a list with a table for each outcome.
##############
glmnet_to_table_mult <- function(fit, s="lambda.1se", min_coef=1E-10) {
  ce_list <- coef(fit, s=s)
  nn <- length(ce_list)
  table_list <- vector('list', nn)
  for (i in 1 : nn){
    table_list[[i]] <- coef_to_table(ce_list[[i]], min_coef = min_coef)
  }
  names(table_list) <- names(ce_list)
  return(table_list)
}

####################
# Plots a PartialPlot object from randomforest package in ggplot.
# This is needed for categorical features with many levels which don't display well in
# the standard plot.
# Inputs: dt is the input data to the rf fit,
# fit is rf fit, var_name is name of the variable to plot (a string),
# partial_plot is the PartialPlot object created using the same var_name,
# Only levels with n >= min_n are plotted.
# There may be problems with levels with n = 0.
####################
partial_gg <- function(dt, var_name, fit, partial_plot, min_n = 1){
  partial_tbl <- count(dt, !!as.name(var_name)) %>%
    arrange(desc(n)) %>%
    left_join(tibble(!!var_name := as.factor(partial_plot$x), partial = partial_plot$y), by = var_name)
  input_tbl <- filter(partial_tbl, n >= min_n) %>%
    mutate(!!var_name := fct_reorder(!!as.name(var_name), n))
  ggplot(input_tbl, aes(x = !!as.name(var_name), y = partial)) +
    geom_col() +
    coord_flip()
}

####################
# Plots a PartialPlot object from randomforest package in ggplot.
# This is needed for categorical features with many levels which don't display well in
# the standard plot.
# Inputs: dt is the input data to the rf fit,
# fit is rf fit, var_name is name of the variable to plot (a string),
# partial_plot is the PartialPlot object created using the same var_name,
# Only levels with n >= min_n are plotted.
# There may be problems with levels with n = 0.
#
# NEW VERSION - handles non-factors as line plots.
# Issues: might not want to reorder factors by n.
####################
partial_gg <- function(dt, var_name, fit, partial_plot, min_n = 1){
  fac_flag <- is.factor(pull(dt, !!as.name(var_name)))
  if(fac_flag){
    ppx <- as.factor(partial_plot$x)
    partial_tbl <- count(dt, !!as.name(var_name)) %>%
      arrange(desc(n)) %>%
      left_join(tibble(!!var_name := ppx, partial = partial_plot$y), by = var_name)
  }
  if(fac_flag){
    input_tbl <- filter(partial_tbl, n >= min_n) %>%
      mutate(!!var_name := fct_reorder(!!as.name(var_name), n))
    ggplot(input_tbl, aes(x = !!as.name(var_name), y = partial)) +
      geom_col() +
      coord_flip()
  }else{
    input_tbl = tibble(x = partial_plot$x, partial = partial_plot$y)
    ggplot(input_tbl, aes(x = x, y = partial)) +
      geom_line(stat = "identity") +
      xlab(var_name)
  }
}

################
# Plots a partial plot object from the pdp package.
# Designed for factors with many levels. It orders the levels by n,
# optionally filters to show only levels with n >= min_n,
# and flips the plot to more easily display the level names.
# TODO: print table showing n
# - might not want to order by n, esp ordered factors
################
pdp_flip <- function(dt, pp, min_n = 1){
  var_name <- names(pp)[1]
  fac_flag <- is.factor(pull(dt, !!as.name(var_name)))
  if(fac_flag){
    ppx <- as.factor(pp[, 1])
    partial_tbl <- count(dt, !!as.name(var_name)) %>%
      arrange(desc(n)) %>%
      left_join(tibble(!!var_name := ppx, partial = pp$yhat), by = var_name)
  }
  if(fac_flag){
    input_tbl <- filter(partial_tbl, n >= min_n) %>%
      mutate(!!var_name := fct_reorder(!!as.name(var_name), n))
    ggplot(input_tbl, aes(x = !!as.name(var_name), y = partial)) +
      geom_col() +
      coord_flip()
  }else{
    input_tbl = tibble(x = partial_plot[, 1], partial = partial_plot$yhat)
    ggplot(input_tbl, aes(x = x, y = partial)) +
      geom_line(stat = "identity") +
      xlab(var_name)
  }
}

############
# Gets partial dependence plot data for all variables in a fitted ranger model.
# TODO: make variable names an input so it will work with other models.
############
pdp_bulk <- function(fit) {
  var_names <- names(fit$variable.importance)
  nn <- length(var_names)
  cat("Getting partial dependence plots for", nn, "variables\n")
  pdps <- vector('list', nn)
  for (i in 1 : nn){
    vname <- var_names[i]
    pdps[[i]] <- partial(fit, eval(vname))
    cat(i, " ")
  }
  names(pdps) <- var_names
  pdps
}
