#' @title pickerSankeyVar function
#' @description Function to pick variables for state in a Sankey diagram.
#' @param id A string representing the id of the input element.
#' @param df_sankey A data frame containing the Sankey diagram data.
#' @param df_config_sankey A data frame containing the Sankey configuration.
#' @param state A string representing the state of the variable. Default is "left_var".
#' @return A pickerInput object.
pickerSankeyVar <- function(id, df_sankey, df_config_sankey = NULL, state = "left_var", input_left_var = NULL) {
  Categorie <- NULL


  id_state <- shiny::NS(id, state)

  if (state == "left_var") {
    text <- "left"
  } else if (state == "right_var") {
    text <- "right"
  }

  if (!is.null(df_config_sankey)) {
    choices <- df_config_sankey %>%
      dplyr::filter(!is.na(!!rlang::sym(state))) %>%
      dplyr::mutate(Categorie = factor(Categorie)) %>%
      dplyr::group_split(Categorie) %>%
      purrr::set_names(purrr::map_chr(., ~ .x$Categorie[1] %>% as.character())) %>%
      purrr::map(~ .x %>%
        dplyr::pull(target) %>%
        as.list()) %>%
      ## Iterate over elements
      purrr::map(
        ~ purrr::map(
          ## check if element is present and correctly formed
          .x, ~ purrr::keep(.x, present_and_correct(.x, df = df_sankey)) %>%
            ## set the display name per element
            purrr::set_names(display_name(.x, id))
          ## Remove all empty elements
        ) %>%
          purrr::compact() %>%
          unlist()
      )
    selected <- choices[[1]][[1]]
  } else {
    if (state == "left_var") {
      choices <- names(df_sankey)
    }
    else if (state == "right_var") {
      choices <- names(df_sankey)
      choices <- setdiff(names(df_sankey), input_left_var)
    }
    selected <- choices[1]
  }



  shinyWidgets::pickerInput(
    inputId = id_state,
    label = paste("variabele", text, sep = " "),
    choices = choices,
    selected = selected
  )
}


#' @title pickerSankeyValues function
#' @description Function to pick values for transition of the two Sankey states.
#' @param id A string representing the id of the input element.
#' @param filter_var A string representing the variable to filter.
#' @param df_sankey A data frame containing the Sankey diagram data.
#' @param side A string representing the side of the Sankey diagram.
#' @return A pickerInput object.
pickerSankeyValues <- function(id, filter_var, df_sankey, side) {
  inputId_base <- paste0("filter_", side)
  inputId <- shiny::NS(id, inputId_base)

  values <- unique(df_sankey[[filter_var]])

  shinyWidgets::pickerInput(
    inputId = inputId,
    label = paste0("filter ", side),
    choices = values,
    selected = values[1:5],
    multiple = TRUE
  )
}


#' @title pickerGanttVar function
#' @description Function to pick a variable to show values in a Gantt chart.
#' @param id A string representing the id of the input element.
#' @param element A string representing the element.
#' @param df A dataframe with the data itself
#' @param df_config_gantt A data frame containing the Gantt configuration.
#' @param input_var_value A variable value from the input. Default is NULL.
pickerGanttVar <- function(id, element, df, df_config_gantt = NULL, input_var_value = NULL) {
  input_var <- target_var <- Categorie <- NULL


  id_element <- shiny::NS(id, element)

  if (element == "target_var") {
    label <- "doel variable"


  } else if (element == "input_var") {
    label <- "input variabele"
  }


  ## Set friendly names for choices
  if (!is.null(df_config_gantt)) {

    if (element == "target_var") {
      basic_choices <- df_config_gantt %>%
        dplyr::filter(input_var == input_var_value) %>%
        dplyr::pull(target_var) %>%
        unique()
    } else if (element == "input_var") {
      basic_choices <- df_config_gantt[[element]] %>% unique()
    }

    choices <- df_config_gantt %>%
      ## Keep only earlier selected choices
      dplyr::filter(!!rlang::sym(element) %in% basic_choices) %>%
      ## Split per category
      dplyr::mutate(Categorie = factor(Categorie)) %>%
      dplyr::group_split(Categorie) %>%
      purrr::set_names(purrr::map_chr(., ~ .x$Categorie[1] %>% as.character())) %>%
      purrr::map(~ .x %>%
        # dplyr::filter(!!rlang::sym(element) %in% choices) %>%
        dplyr::pull(!!rlang::sym(element)) %>%
        as.list() %>%
        unique()) %>%
      purrr::map(
        ~ purrr::set_names(.x, ~ purrr::map_chr(.x, ~ display_name(.x, id)))
      )

    selected <- choices[[1]][[1]]

  } else {
    choices <- names(df)
    ## Filter the possibilities for the target_var (exclude the choosen input_var)
    if (element == "target_var") {
      choices <- setdiff(choices, input_var_value)
    }
    selected <- choices[[1]]

  }

  shinyWidgets::pickerInput(
    inputId = id_element,
    label = label,
    choices = choices,
    selected = selected
  )
}


#' @title pickerGanttValues function
#' @description Function to filter the values in the Gantt chart.
#' @param id A string representing the id of the input element.
#' @param filter_var A string representing the variable to filter.
#' @param df_doorstroom_gantt A data frame containing the Gantt chart data.
#' @return A pickerInput object.
pickerGanttValues <- function(id, filter_var, df_doorstroom_gantt) {
  inputId <- shiny::NS(id, "filter")

  shinyWidgets::pickerInput(
    inputId = inputId,
    label = "input filter",
    choices = unique(df_doorstroom_gantt[[filter_var]])
  )
}
