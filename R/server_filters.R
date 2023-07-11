#' #' Prepare a dataframe
#' #'
#' #' Prepares a dataframe based on provided filters and naming options
#' #'
#' #' This function collapses values from the naming list into a single string,
#' #' removes null elements from the filter list, transforms filter list into elements
#' #' suitable for filtering, applies filters, adds new columns, and casts var used as color to factor.
#' #' @param lFilters List of filters to be applied on the dataframe.
#' #' @param lValues_for_naming List of values used for naming.
#' #' @param df Dataframe to be processed.
#' #' @param color_var Variable used for coloring.
#' #' @param facet Facet grid side ("left" by default).
#' #' @param facet_var Variable used for facet grid ("VIS_Groep" by default).
#' #' @param facet_name_var Variable used for facet grid naming ("VIS_Groep_naam" by default).
#' #' @return A prepared dataframe with applied filters and new columns.
#' #' @export
#' prep_df <- function(lFilters, lValues_for_naming, df, color_var, facet = "left", facet_var = rlang::sym("VIS_Groep"), facet_name_var = rlang::sym("VIS_Groep_naam")) {
#'   ## Collapses values from the naming list into a single string
#'   sName <- paste(keep_values(lValues_for_naming), collapse = " / ")
#'
#'   ## Removes null elements from the filter list
#'   lFilters <- lFilters %>%
#'     purrr::discard(is.null)
#'
#'   ## Transforms filter list into elements suitable for filtering
#'   lFilter_elements <- purrr::map(lFilters, transform_input)
#'
#'   ## Applies filters, adds new columns, and casts var used as color to factor
#'   dfPrepared <- df %>%
#'     filter_with_lists(lFilter_elements) %>%
#'     dplyr::mutate(
#'       !!facet_var := facet,
#'       !!facet_name_var := paste("VU",
#'         sName,
#'         sep = " - "
#'       )
#'     ) %>%
#'     dplyr::mutate(!!rlang::sym(color_var) := as.factor(!!rlang::sym(color_var)))
#'
#'   return(dfPrepared)
#' }

#' #' Keep only relevant values
#' #'
#' #' Filters out only relevant values based on the provided filters
#' #'
#' #' This function removes null elements from the filter list, transforms filter list into elements
#' #' suitable for filtering, and retrieves relevant values from the data.
#' #' @param lFilters List of filters to be applied on the data.
#' #' @param sVariable The variable for which relevant values are to be retrieved.
#' #' @param dfFilters Dataframe with the possible filters and values for this dataset
#' #' @return A list of relevant values for the specified variable.
#' #' @export
#' keep_only_relevant_values <- function(lFilters, sVariable, dfFilters) {
#'   ## Verifies the input variables are set, if not stop execution
#'   shiny::req(lFilters)
#'
#'   ## Removes null elements from the filter list
#'   lFilters <- lFilters %>%
#'     purrr::discard(is.null)
#'
#'   ## Transforms filter list into elements suitable for filtering
#'   lFilter_elements <- purrr::map(lFilters, transform_input)
#'
#'   ## Retrieves relevant values from the data
#'   lRelevant_values <- dfFilters %>%
#'     filter_with_lists(lFilter_elements) %>%
#'     dplyr::pull(!!rlang::sym(sVariable)) %>%
#'     purrr::set_names(.) %>%
#'     purrr::map(~ paste(.x, sVariable, sep = ";"))
#'
#'   return(lRelevant_values)
#' }



#' Keep values
#'
#' This function extracts values before the semicolon from a ";"-separated string.
#'
#' @param input A character vector with ";"-separated strings
#'
#' @return A list of values before the semicolon in the input
#' @export
keep_values <- function(input) {
  lValues <- purrr::map(input, ~ stringr::str_split(., ";")[[1]][1])

  return(lValues)
}


#' Transform input
#'
#' This function transforms a list of inputs into a column and value for filtering.
#'
#' @param input A list of inputs to be transformed
#'
#' @return A list containing a column and its corresponding value for filtering
#' @export
transform_input <- function(input) {
  ## Splits the string and retrieves the second part as the column name
  sColumn <- stringr::str_split(input[1], ";")[[1]][2]

  ## Retrieves the filter values
  lValues <- keep_values(input)

  ## Combines column and values into a filter element
  lFilter_element <- list(sColumn, lValues)

  return(lFilter_element)
}


#' #' Filter with lists
#' #'
#' #' This function filters a dataframe using a list with column and one or more values.
#' #'
#' #' @param df A dataframe to be filtered
#' #' @param filters A list containing column names and their corresponding values for filtering
#' #'
#' #' @return A dataframe filtered based on the input filters
#' #' @export
#' filter_with_lists <- function(df, filters) {
#'   ## Applies each filter to the dataframe
#'   purrr::walk(filters, function(.x) df <<- df %>% dplyr::filter(!!rlang::sym(.x[[1]]) %in% .x[[2]]))
#'
#'   return(df)
#' }


#' Limit number of values for Gantt chart
#'
#' This function limits the number of values displayed in a Gantt chart. If the number of distinct
#' values in the specified variable is less than the limit, the function returns the original dataframe.
#'
#' @param df A dataframe to be processed
#' @param split_var A character vector specifying the variable to be split
#' @param n_values An integer specifying the maximum number of values (default is 12)
#'
#' @return A dataframe with a limited number of values for the Gantt chart
#' @export
limit_n_values_gantt <- function(df, split_var, n_values = 12) {
  split_var_placeholder <- NULL

  if (n_values >= dplyr::n_distinct(df[[split_var]])) {
    return(df)
  }


  values_to_keep <- df[[split_var]][1:(n_values - 1)]

  ## All variables from manipulation before gantt
  n <- sum(df$n[(n_values):nrow(df)])
  flow_perc <- sum(df$flow_perc[n_values:nrow(df)])
  flow_end_perc <- 1
  flow_start_perc <- df$flow_start_perc[n_values]


  df_mutated <- df %>%
    dplyr::filter(!!rlang::sym(split_var) %in% values_to_keep) %>%
    dplyr::bind_rows(data.frame( # !!rlang::sym(split_var) := "other",
      split_var_placeholder = "Anders",
      n = n,
      flow_perc = flow_perc,
      flow_end_perc = flow_end_perc,
      flow_start_perc = flow_start_perc
    ) %>%
      ## Update the dataframe before binding to avoid multiple columns with same names
      dplyr::rename(!!rlang::sym(split_var) := split_var_placeholder)) %>%
    dplyr::arrange(flow_start_perc)

  return(df_mutated)
}
