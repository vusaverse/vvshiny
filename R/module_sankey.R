#' Create a Shiny app for Sankey visualization
#'
#' This function initializes and returns a Shiny app, specifically tailored for a Sankey diagram
#' visualization based on given data and configuration.
#'
#' @param df A data frame containing data to be visualized in the Sankey diagram.
#' @param df_config A data frame with configuration settings for the Sankey visualization.
#' @param id Character. A unique identifier for the Shiny app.
#'
#' @return A Shiny app object.
#' @export
#' @examples
#' df <- dplyr::tribble( ~OPL_Onderdeel_CROHO_examen, ~OPL_Onderdeel_CROHO_instroom,
#'  ~OPL_CBS_Label_rubriek_examen, ~OPL_CBS_Label_rubriek_instroom,
#'  "GEDRAG EN MAATSCHAPPIJ", "GEZONDHEIDSZORG", "sociale geografie", "(huis)arts, specialist, geneeskunde",
#'   "GEDRAG EN MAATSCHAPPIJ", "RECHT EN MAATSCHAPPIJ", "sociale geografie", "sociale geografie",
#'   "GEDRAG EN MAATSCHAPPIJ", "RECHT", "sociale geografie", "notariaat",
#'   "RECHT", "GEZONDHEIDSZORG", "notariaat", "notariaat",
#'   "TAAL EN CULTUUR", "RECHT", "niet westerse talen en culturen", "notariaat")
#
#
#' df_config <- dplyr::tribble(
#'   ~Categorie, ~Veldnaam, ~Veldnaam_gebruiker, ~target, ~left_var, ~right_var, ~title_start, ~title_end, ~position_label_y,
#'   "Doorstroom vanuit B", "OPL_Onderdeel_CROHO_examen", "B Croho sector", "OPL_Onderdeel_CROHO_examen", 1, NA, "Waar stromen ", "Bachelor gediplomeerden naar toe?", "right",
#'   "Doorstroom vanuit B", "OPL_CBS_Label_rubriek_examen", "B ISCED-F Rubriek", "OPL_CBS_Label_rubriek_examen", 1, NA, "Waar stromen ", "Bachelor gediplomeerden naar toe?", "right",
#'   "Doorstroom vanuit B", "OPL_CBS_Label_richting_examen", "B ISCED-F Richting", "OPL_CBS_Label_richting_examen", 1, NA, "Waar stromen ", "Bachelor gediplomeerden naar toe?", "right",
#'   "Instroom bij M", "OPL_Onderdeel_CROHO_instroom", "M Croho sector", "OPL_Onderdeel_CROHO_instroom", NA, 1, "Waarvandaan stromen ", "Master studenten in?", "left",
#'   "Instroom bij M", "OPL_CBS_Label_rubriek_instroom", "M ISCED-F Rubriek", "OPL_CBS_Label_rubriek_instroom", NA, 1, "Waarvandaan stromen ", "Master studenten in?", "left",
#'   "Instroom bij M", "OPL_CBS_Label_richting_instroom", "M ISCED-F Richting", "OPL_CBS_Label_richting_instroom", NA, 1, "Waarvandaan stromen ", "Master studenten in?", "left"
#' )
#' sankey_app(df, df_config)
sankey_app <- function(df, df_config = NULL, id = "sankey") {
  if(!exists("request")) {
    request <- NULL
  }

  ui <- single_module_ui(request, id, tab_item = shinydashboard::tabItem(tabName = id, module_sankey_ind_ui(id, df, df_config)))

  server <- function(input, output, session) {
    module_sankey_ind_server(id, df, df_config)
  }

  shiny::shinyApp(ui, server)
}

#' Sankey module server function
#'
#' This function provides the server-side logic for the Sankey visualization module in Shiny.
#' It handles data processing and visualization rendering.
#'
#' @param id Character. A unique identifier for the Shiny module.
#' @param df A data frame containing data to be visualized in the Sankey diagram.
#' @param df_config A data frame with configuration settings for the Sankey visualization.
#'
#' @return None. This function is used for its side effects of generating UI elements.
module_sankey_ind_server <- function(id, df, df_config) {
  shiny::moduleServer(id, function(input, output, session) {

    ## Pas var aan
    output$filter_left <- shiny::renderUI({
      shiny::req(input$left_var)

      shiny::tagList(
        pickerSankeyValues(id, input$left_var, df, "left")
      )
    })

    ## Maak UI om te filteren op geselecteerde variabele
    output$right_var <- shiny::renderUI({

      shiny::req(input$left_var)

      shiny::tagList(
        pickerSankeyVar(id, df, df_config, "right_var", input$left_var)
      )
    })


    ## Pas var aan
    output$filter_right <- shiny::renderUI({
      shiny::req(input$right_var)

      shiny::tagList(
        pickerSankeyValues(id, input$right_var, df, "right")
      )
    })

    ## Maak Sankey ####
    output$sankey <- shiny::renderPlot({

      ## Haal vereiste variables op. De left_var update ook het filter, dus vandaar de
      ## req voor filter en isolate voor de left / right var
      shiny::req(input$filter_left, input$filter_right)


      ## Bepaal variabelen
      left_var <- shiny::isolate(input$left_var)
      right_var <- shiny::isolate(input$right_var)
      multiple_filter_left <- input$filter_left
      multiple_filter_right <- input$filter_right

      ## Prepareer dataframe
      ## TODO Functie
      df_grouped <- df %>%
        dplyr::filter(!!rlang::sym(left_var) != "Onbekend",
                      !!rlang::sym(right_var) != "Onbekend"
        ) %>%
        dplyr::group_by(!!rlang::sym(left_var), !!rlang::sym(right_var)) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(change = !!rlang::sym(left_var) != !!rlang::sym(right_var)) %>%
        dplyr::filter(change == TRUE) %>%
        dplyr::filter(!!rlang::sym(left_var) %in% multiple_filter_left,
                      !!rlang::sym(right_var) %in% multiple_filter_right)

      title <- "Doorstrom van Bachelor naar Master"

      sankey_plot(
        df_grouped,
        left_var,
        right_var,
        ggplot2::xlab("Doorstroom"),
        ggplot2::ylab("Frequentie"),
        "Bachelor",
        "Master",
        title
      )



    })
  })
}


#' UI component for the Sankey module
#'
#' This function creates and returns the user interface components for the Sankey visualization module.
#' It includes both the visualization and the necessary filters and controls.
#'
#' @param id Character. A unique identifier for the Shiny module.
#' @param df A data frame containing data to be visualized in the Sankey diagram.
#' @param df_config A data frame with configuration settings for the Sankey visualization.
#'
#' @return A Shiny UI object containing the Sankey visualization and associated controls.
module_sankey_ind_ui <- function(id, df, df_config = NULL) {

  ns <- shiny::NS(id)

  shiny::fluidPage(
    ## Plots ####
    shiny::column(
      width = 12,
      shiny::fluidRow(
        shinydashboardPlus::box(
          width = 12,
          shinycssloaders::withSpinner(
            shiny::plotOutput(ns("sankey")), type = 4)
        )
      )
    ),
    ## Filters + Bookmark ####
    ## INFO: Sidebar
    shinydashboardPlus::dashboardSidebar(
      ## TODO: Filters op jaar e.d. toevoegen
      shinydashboard::sidebarMenu(
        pickerSankeyVar(id, df, df_config, "left_var"),
        shiny::uiOutput(ns("filter_left")),
        shiny::uiOutput(ns("right_var")),
        shiny::uiOutput(ns("filter_right"))
      )
    )
  )

}
