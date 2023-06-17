#' Summarise model coefficients in a table
#'
#' Returns a table of coefficients, filtered by minimum absolute value and optionally separating
#' into interactions and decomposing dummy variables into parent variable and factor level parts.
#'
#' Only coefficients with absolute value greater than `min_coef` are included. If `var_names` is
#' supplied then extra columns will be added which split the `name` into component parts. Where
#' there are interactions, these are separated, and any dummy variables are split into their
#' original variable and factor level names. See "Value" below for more details.
#'
#' This was designed for glmnet models, but it also often works with other glm-type models.
#'
#' @param fit A fitted model with coefficients (e.g. from glmnet).
#' @param var_names (optional) A character vector of column names for the data used in `fit`.
#' @param ... Parameters passed to `coef()`. For `cv.glmnet` models this will most usually be the
#'   the regularisation parameter `s` of the desired model.
#' @param min_coef Coefficients with smaller absolute value than this are excluded from the table.
#'
#' @return A tibble which will always contain `name` and `coef` columns. If `var_names` is supplied
#'   then there will be extra columns also. If there are no interactions then there will be two
#'   extra columns `var` and `level` which are extracted from `name`. If there are interactions,
#'   there will instead be three columns for each interaction: `interact_dd`, `var_dd`, and
#'   `level_dd`, where each "dd" gives the number of the interaction. The number of interactions
#'   displayed depends on which coefficients are included in the returned table, not the original
#'   model, so `min_coef` can change the number of output columns.
#' @export
coef_to_table <- function(fit, var_names = NULL, ..., min_coef=1E-10) {
  tbl <- coef_to_table_simple(fit = fit, ..., min_coef = min_coef)
  if (is.null(var_names)) return(tbl)
  n_vars <- max(stringr::str_count(tbl$name, ":")) + 1
  if (n_vars == 1){
    tbl <- tbl %>%
      dplyr::mutate(var = match_parent(.data$name, var_names)) %>%
      dplyr::mutate(level = extract_level(.data$name, .data$var))
    return(tbl)
  }
  nms <- sprintf("name%02d", 1 : n_vars)
  tbl2 <- dplyr::select(tbl, "name") %>%
    tidyr::separate(.data$name, nms, sep = ":", remove = FALSE, fill = "right")
  names_list <- purrr::map(2 : ncol(tbl2), ~dplyr::select(tbl2, dplyr::all_of(.)))
  dplyr::bind_cols(tbl, purrr::map_dfc(names_list, ~expand_interaction(., var_names))) %>%
    dplyr::select("name", "coef", dplyr::contains("interact"), dplyr::everything())
}

#' Summarise model coefficients in a table
#'
#' Performs the simpler part of `coef_to_table()` without the name breakdown.
#' @param fit A fitted model with coefficients.
#' @param ... Parameters passed to `coef()`.
#' @param min_coef Coefficients with smaller absolute value than this are excluded from the table.
#' @noRd
coef_to_table_simple <- function(fit, ..., min_coef=1E-10) {
  ce <- coef(fit, ...)
  coef_mat <- as.matrix(ce)
  tibble::tibble(name = rownames(coef_mat), coef = coef_mat[, 1]) %>%
    dplyr::filter(abs(.data$coef) >= min_coef) %>%
    dplyr::arrange(dplyr::desc(.data$coef))
}

#' Match model level names to parent variable names
#'
#' Helper for `coef_to_table()`. Each level starts with a parent plus maybe more text. Returns
#' vector of parents corresponding to `level_names`. Parent is matching parent of greatest length.
#' If no match is found or level is `NA` then the parent is the level name.
#'
#' @param level_names Character vector to find parents for.
#' @param parent_names Character vector of candidate parents.
#' @noRd
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

#' Extract a factor level name from a model.matrix level names
#'
#' Helper for `coef_to_table()`. The model.matrix names are in `name_vec` and the
#' corresponding entries in `feature_vec` is the name of the first feature. A vector of levels
#' corresponding to the level associated with the feature is returned. If there is no level involved
#' then the level column will have the entry `none_name`.
#'
#' @param name_vec Character vector of model.matrix names.
#' @param feature_vec Character vector of feature names corresponding to `name_vec`.
#' @param none_name String to use when there is no level or interaction in a model name.
#' @noRd
extract_level <- function(name_vec, feature_vec, none_name = NA_character_) {
  levels <- stringr::str_remove_all(name_vec, stringr::fixed(feature_vec)) %>%
    stringr::str_remove_all(":.*")
  if_else(stringr::str_length(levels) == 0, none_name, levels)
}

#' Expand dummy names to var and level parts with appropriate column names
#'
#' Helper for `coef_to_table()`. This is used where there are interactions (which will be numbered).
#' Splits supplied name column into variable and level
#' parts. The returned tibble has columns `interact_dd` (input renamed),`var_dd` (variable name
#' part) and `level_dd` (factor level part), where "dd" matches
#' the value in the input column name `name_dd`.
#'
#' @param tbl A tibble with one column called `name_dd` where "dd" are digits.
#' @param var_names A character vector of column names for the data used in `tbl`.
#' @noRd
expand_interaction <- function(tbl, var_names) {
  id <- stringr::str_extract(names(tbl), "\\d+$")
  names(tbl) <- "interact"
  tbl %>%
    dplyr::mutate(var = match_parent(.data$interact, var_names)) %>%
    dplyr::mutate(level = extract_level(.data$interact, .data$var)) %>%
    stats::setNames(., paste0(names(.), "_", id))
}
