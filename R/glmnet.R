#########################################################################################
# match_parent: Match model level names to parent variable names
#########################################################################################
#'
#' Match model level names to parent variable names
#'
#' Helper for `glmnet_to_table()`. Each level starts with a parent plus maybe more text. Returns
#' vector of parents corresponding to `level_names`. Parent is matching parent of greatest length.
#' If no match is found or level is `NA` then the parent is the level name.
#'
#' @param level_names Character vector to find parents for.
#' @param parent_names Character vector of candidate parents.
#' @keywords internal
match_parent <- function(level_names, parent_names) {
  nn <- length(level_names)
  parent <- character(nn)
  for (i in 1 : nn){
    matches <- stringr::str_detect(level_names[i], paste0("^", parent_names))
    if(!any(matches) | any(is.na(matches))){
      parent[i] <- level_names[i]
    }else{
      matches <- parent_names[matches]
      parent[i] <- matches[which.max(stringr::str_length(matches))] #choose longest to avoid substring matching
    }
  }
  parent
}

#############################################################################
# extract_level: Extract a factor level name from a model.matrix level names
#############################################################################
#'
#' Extract a factor level name from a model.matrix level names
#'
#' Helper for `glmnet_to_table()`. The model.matrix names are in `name_vec` and the
#' corresponding entries in `feature_vec` is the name of the first feature. A vector of levels
#' corresponding to the level associated with the feature is returned. If the is no level then the entry is
#' "(none)".
#'
#' @param name_vec Character vector of model.matrix names.
#' @param feature_vec Character vector of feature names corresponding to `name_vec`.
#' @keywords internal
extract_level <- function(name_vec, feature_vec) {
  levels <- stringr::str_remove_all(name_vec, stringr::fixed(feature_vec)) %>%
    stringr::str_remove_all(":.*")
  if_else(stringr::str_length(levels) == 0, "(none)", levels)
}

#########################################################################################
# glmnet_to_table: Summarise coefficients from glmnet in a table
#########################################################################################
#'
#' Summarise coefficients from glmnet in a table
#'
#' Returns a tibble of coefficients for glmnet model `fit` with parameter `s`. Only
#' coefficients with absolute value greater than `min_coef` are included. If `var_names` is
#' supplied then a columns of feature names and level names for each coefficient will be added (first
#' match in the case of interactions). If there is no level involved then the level column will have
#' the entry `none_name`. If there are any interactions in the model then the interacting feature is
#' added in a column.
#'
#' This is still in development. Doesn't handle more than a single interaction.
#'
#' @param fit A fitted glmnet model.
#' @param var_names (optional) A character vector of column names for data used in `fit`.
#' @param s The regularisation parameter determines which model is used from `fit` (as used in glmnet).
#' @param min_coef Coefficients with smaller absolute value than this are excluded from the table.
#' @param none_name String to use when there is no level or interaction in a model name.
#'
#' @export
glmnet_to_table <- function(fit, var_names = NULL, s="lambda.1se", min_coef=1E-10, none_name = "(none)") {
  ce <- coef(fit, s=s)
  coef_mat <- as.matrix(ce)
  level_names <- rownames(ce)
  tbl <- tibble::tibble(name = rownames(coef_mat), coef = coef_mat[, 1])
  if (!is.null(var_names)){
    tbl <- tbl %>%
      dplyr::mutate(feature = match_parent(level_names, var_names)) %>%
      dplyr::mutate(level = extract_level(.data$name, .data$feature))
    tbl <- dplyr::mutate(tbl, interact = if_else(stringr::str_detect(.data$name, ":"),
                                                 stringr::str_remove_all(.data$name, "^.*:"),
                                                 none_name))
    if (all(tbl$interact == none_name)){
      tbl <- dplyr::select(tbl, -.data$interact)
    }
  }
  dplyr::filter(tbl, abs(.data$coef) >= min_coef) %>%
    dplyr::arrange(dplyr::desc(.data$coef))
}

# This handles models with interactions, splitting the interacting variables to get the parent of each.
# Maybe keep the "name" columns.
# Might be neater ways of doing this with the new dplyr::across().
#########################################################################################
# glmnet_to_table: Summarise coefficients from glmnet in a table
#########################################################################################
#'
#' Summarise coefficients from glmnet in a table
#'
#' Experimental variant of `glmnet_to_table()` which splits by interaction when finding parents.
#'
#' @param data The data used to fit the model.
#' @inherit glmnet_to_table
glmnet_to_table2 <- function(fit, data = NULL, var_names = NULL, s="lambda.1se", min_coef=1E-10) {
  ce <- coef(fit, s=s)
  coef_mat <- as.matrix(ce)
  level_names <- rownames(ce)
  tbl <- tibble::tibble(name = rownames(coef_mat), coef = coef_mat[, 1])
  if (!is.null(var_names)){
    n_vars <- max(stringr::str_count(tbl$name, ":")) + 1
    nms <- paste0("name", 1 : n_vars)
    tbl <- tidyr::separate(tbl, .data$name, nms, sep = ":", remove = FALSE, fill = "right")
    for (i in 1 : n_vars){
      nm <- paste0("parent", i)
      new_nm <- paste0("name", i)
      tbl <- dplyr::mutate(tbl, !!rlang::sym(nm) :=  match_parent(!!rlang::sym(new_nm), names(data)))
    }
    tbl <- dplyr::mutate(tbl, is_parent = (.data$name == .data$parent1)) %>%
      dplyr::select(.data$name, .data$coef, dplyr::contains("parent"))
  }
  dplyr::filter(tbl, abs(.data$coef) >= min_coef) %>%
    dplyr::arrange(dplyr::desc(.data$coef))
}
