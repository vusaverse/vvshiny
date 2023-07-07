#' Bind both
#'
#' This function binds two dataframes row-wise and performs additional manipulations depending on the 'type'.
#' The function also reorders the factor levels of the specified facet variable.
#'
#' @param dfLeft A dataframe to be combined
#' @param dfRight A dataframe to be combined
#' @param id An identifier string specifying the type of operation
#' @param y_left A character vector specifying the column to be used for the left dataframe
#' @param y_right A character vector specifying the column to be used for the right dataframe
#' @param facet_var A symbol specifying the variable to be used for faceting
#' @param facet_name_var A symbol specifying the variable to be used for the facet name
#'
#' @return A dataframe obtained by binding dfLeft and dfRight, with additional transformations applied
#' @export
#'@export
bind_both <- function(dfLeft, dfRight, id = "bench", y_left = NULL, y_right = NULL, facet_var = rlang::sym("VIS_Groep"), facet_name_var = rlang::sym("VIS_Groep_naam")) {

  y <- NULL

  ## Binds the left and right dataframes and reorders factor levels
  dfBoth <- dplyr::bind_rows(dfLeft, dfRight) %>%
    dplyr::mutate(!!facet_name_var := forcats::fct_reorder(!!facet_name_var, !!facet_var, min))
  #dplyr::mutate(VIS_Groep_naam = forcats::fct_reorder(VIS_Groep_naam, VIS_Groep, min))

  ## Mutates y for comparison type
  if (stringr::str_detect(id, "comp")) {
    dfBoth <- dfBoth %>%
      dplyr::mutate(y = dplyr::if_else(!!facet_var == "left",
                                       !!rlang::sym(y_left),
                                       !!rlang::sym(y_right))
      )
  }

  return(dfBoth)
}

#' Bind both table
#'
#' This function joins two summarized dataframes and relocates y_left before y_right.
#' The function also sets the VIS_Groep value to 'left' for the right dataframe.
#'
#' @param dfLeft_summ A summarized dataframe to be joined
#' @param dfRight_summ A summarized dataframe to be joined
#' @param y_left A character vector specifying the column to be relocated before y_right
#' @param y_right A character vector specifying the column after which y_left will be relocated
#'
#' @return A dataframe obtained by joining dfLeft_summ and dfRight_summ, with y_left relocated before y_right
#' @export
bind_both_table <- function(dfLeft_summ, dfRight_summ, y_left, y_right) {

  ## Changes VIS_Groep to 'left' for the right summarized dataframe
  dfRight_summ <- dfRight_summ %>% dplyr::mutate(VIS_Groep = "left")

  ## Joins left and right summarized dataframes, and relocates y_left before y_right
  dfBoth_table <- dplyr::inner_join(dfLeft_summ, dfRight_summ) %>%
    dplyr::relocate(!!rlang::sym(y_left), .before = !!rlang::sym(y_right))

  return(dfBoth_table)
}
