#' Get a user-friendly display name
#'
#' This function provides a user-friendly name for a column based on a mapping table, if available.
#'
#' @param col_name A string specifying the name of the column.
#' @param mapping_table An optional named list or other object that maps column names to display names. If not provided, a default is inferred from the 'id' parameter.
#' @return A string containing the user-friendly name for the column.
#' @export
display_name <- function(col_name, mapping_table) {
  if (col_name %in% names(mapping_table)) {
    col_name <- mapping_table[[col_name]]
  }

  return(col_name)
}


#' Prepare a data table for displaying
#'
#' This function prepares a data table for displaying by providing user-friendly names, removing unneeded variables, and formatting percentages.
#'
#' @param y A string specifying the column name to be used as the y-axis variable.
#' @param df A data frame containing the raw data.
#' @param df_summmarized A data frame containing the summarized data.
#' @param id A string specifying the ID associated with the data.
#' @param y_right An optional string specifying the column name to be used as the second y-axis variable. Default is NULL.
#' @param facet_var A symbol specifying the column to be used for faceting. Default is 'VIS_Groep'.
#' @param facet_name_var A symbol specifying the column to be used for faceting names. Default is 'VIS_Groep_naam'.
#' @param ... Further arguments passed on to the 'make_basic_table' function.
#' @return A DT::datatable object ready for displaying.
#' @export
prep_table <- function(y, df, df_summmarized, id, y_right = NULL, facet_var = rlang::sym("VIS_Groep"), facet_name_var = rlang::sym("VIS_Groep_naam"), ...) {
  ## Remove unneeded variables
  dfTabel <- df_summmarized %>%
    dplyr::select(
      -!!facet_name_var,
      -!!facet_var
    )

  ## Set user friendly names
  names(dfTabel) <- purrr::map_chr(names(dfTabel), ~ display_name(.x, id))

  ## Get boolean vars in order to add formatting %
  if (is.logical(df[[y]])) {
    sBoolean_vars <- y
  } else {
    sBoolean_vars <- c()
  }

  if (!is.null(y_right) && is.logical(df[[y_right]])) {
    sBoolean_vars <- c(sBoolean_vars, y_right)
  }

  sBoolean_vars <- sBoolean_vars %>%
    purrr::map_chr(~ display_name(.x, id))

  ## Make datatable object
  dfTabel <- dfTabel %>%
    make_basic_table(
      caption = dplyr::first(df_summmarized[[facet_name_var]]), ...
    ) %>%
    DT::formatPercentage(sBoolean_vars, 2)

  return(dfTabel)
}


#' Quietly run a function
#'
#' This function is a wrapper that allows a function to be run quietly without the need to create a separate quiet function.
#'
#' @param func The function to be run.
#' @param ... Further arguments passed to the 'func' function.
#' @return The result of the 'func' function with messages, warnings, and errors captured.
#' @export
quietly_run <- function(func, ...) {
  func <- func
  quietly_func <- purrr::quietly(func(...))
  quietly_func(...)
}
