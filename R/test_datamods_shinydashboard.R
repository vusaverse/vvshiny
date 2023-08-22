
filter_app <- function(df = NULL, df_config = NULL, basic_vars = NULL, id = "filter") {
  if(!exists("request")) {
    request <- NULL
  }
  ns <- shiny::NS(id)

  ui <- single_module_ui(request, id, tab_item = shinydashboard::tabItem(tabName = id, module_filter_ind_ui(id, df, basic_vars)))

  server <- function(input, output, session) {

    vars <- reactive({
      req(input$varPicker)
      vars <- input$varPicker
    })

    res_filter_basic <- filter_data_server(
      id = id,
      data = reactive(df),
      name = reactive("data"),
      vars = vars,
      defaults = reactive(NULL),
      widget_char = "picker",
      widget_num = "slider",
      widget_date = "slider",
      label_na = "Onbekend",
      value_na = TRUE
    )


    output[[ns("code_dplyr")]] <- renderPrint({
      res_filter_basic$code()
    })
    output[[ns("code")]] <- renderPrint({
      res_filter_basic$expr()
    })

    output[[ns("res_str")]] <- renderPrint({
      str(res_filter_basic$filtered())
    })


  }

  shiny::shinyApp(ui, server)
}

module_filter_ind_ui <- function(id, df, basic_vars) {

  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::column(
      width = 12,
      shiny::fluidRow(
        shinydashboardPlus::box(
          width = 12,
          tags$b("Code dplyr:"),
          shinycssloaders::withSpinner(shiny::verbatimTextOutput(outputId = ns("code_dplyr")), type = 4),
          tags$b("Expression:"),
          shinycssloaders::withSpinner(shiny::verbatimTextOutput(outputId = ns("code")), type = 4),
          tags$b("Filtered data:"),
          shinycssloaders::withSpinner(shiny::verbatimTextOutput(outputId = ns("res_str")), type = 4)
        )
      )
    ),
    shinydashboardPlus::dashboardSidebar(
      # TODO add filters
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          startExpanded = TRUE,
          text = "Filters",
          shinyWidgets::pickerInput(
            inputId = "varPicker",
            label = "Choose filters that are available",
            choices = names(df %>%
                              purrr::keep(~ length(unique(.x)) > 1)),  # Let's start with all possible variables from datasets as choices
            selected = basic_vars$filter,   # Default selected variables
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `count-selected-text` = "{0} variables selected"
            )
          ),
          filter_data_ui(id, max_height = "500px")
        )
      )
    )
  )
}

df <- readRDS("C:/Users/chh230/LocalDocuments/repositories/vvshiny/data/Export_Student_Analytics_2023-02-14_NCO_HO_hashed_gefilterd.rds")
basic_vars <- list(filter = c("INS_Inschrijvingsjaar_EOI", "INS_Faculteit", "INS_Opleidingsnaam_2002"),
                   target = c("RES_Aantal_EC_tm_jaar_1"),
                   explanatory = c("DEM_Geslacht", "INS_Hoogste_vooropleiding_soort_cat", "DEM_Nationaliteit_EER_naam"),
                   timeline = "INS_Inschrijvingsjaar_EOI")
all_target <- c("") # "y" = typeof(df[[column_name]]) %in% c("logical", "double", "integer") & class(df[[column_name]]) != "Date",

all_explanatory <- df %>% dplyr::select(where(~ is.logical(.x) | is.character(.x) | is.factor(.x))) %>% names()

all_timeline <- c("INS_Inschrijvingsjaar_EOI", "INS_Studiejaar")

filter_app(df, basic_vars = basic_vars)

