
df <- readRDS("data/Export_Student_Analytics_2023-02-14_NCO_HO_hashed_gefilterd.rds")
df_config <-read.csv2("data/kolom_categorie_individueel.csv")
names(df_config) <- gsub("\\.", "-", names(df_config))

trendline_bench_app <- function(df, df_config = NULL, id = "trendline") {
  if(!exists("request")) {
    request <- NULL
  }

    ui <- single_module_ui(request, id, tab_item = shinydashboard::tabItem(tabName = id, module_trendline_bench_ui(id, df, df_config)))

  server <- function(input, output, session) {
    module_trendline_bench_server(id, df, df_config)
  }

  shiny::shinyApp(ui, server)
}

trendline_app <- function(df, df_config = NULL, id = "trendline", bench = FALSE) {
  if(!exists("request")) {
    request <- NULL
  }

  if (bench == TRUE) {
    ui <- single_module_ui(request, id, tab_item = shinydashboard::tabItem(tabName = id, module_trendline_bench_ui(id, df, df_config)))

    server <- function(input, output, session) {
      module_trendline_bench_server(id, df, df_config)

    }
  } else {
    ui <- single_module_ui(request, id, tab_item = shinydashboard::tabItem(tabName = id, module_trendline_ui(id, df, df_config)))

    server <- function(input, output, session) {
      module_trendline_server(id, df, df_config)

    }
  }

  shiny::shinyApp(ui, server)
}


module_trendline_server <- function(id, df, df_config = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    ## INPUTS ####
    ### Set shiny::reactive variables ####
    kleur <- shiny::reactive({input$color})
    x <- shiny::reactive({input$x})
    y <- shiny::reactive({input$y})


  })
}

module_trendline_bench_server <- function(id, df, df_config = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    ## INPUTS ####
    ### Set shiny::reactive variables ####
    kleur <- shiny::reactive({input$color})
    x <- shiny::reactive({input$x})
    y <- shiny::reactive({input$y})

    faculty_left <- shiny::reactive({input$filter_faculty_left})
    faculty_right <- shiny::reactive({input$filter_faculty_right})

    phase_left <- shiny::reactive({input$phase_left})
    phase_right <- shiny::reactive({input$phase_right})

    programmes_left <- shiny::reactive({input$opleiding_left})
    programmes_right <- shiny::reactive({input$opleiding_right})

    cohorts <- shiny::reactive({input$cohort_both})


    ## Verzamel variables voor latere grouping (muv y omdat deze moet worden geaggregeerd)
    ## TODO: VIS_Groep_naam is variabele die wordt gemaakt obv de filters
    ## en of het left of right is
    variables <- shiny::reactive({c(kleur(), x(), "VIS_Groep", "VIS_Groep_naam")})

    ### Basis dfs ####
    ## Supporting dfs (ook gebruikt voor updaten UI-element)
    lRelevante_opleidingen_left <- shiny::reactive({
      keep_only_relevant_values(list(faculty_left(), phase_left()), "INS_Opleidingsnaam_2002", id)
    })

    ## Maak left df obv dataset en filters
    dfLeft <- shiny::reactive({
      prep_df(
        list(
          faculty_left(),
          phase_left(),
          programmes_left(),
          cohorts()
        ),
        programmes_left(),
        df,
        kleur(),
        "left"
      )
    })

    lRelevante_opleidingen_right <- shiny::reactive({
      keep_only_relevant_values(list(faculty_right(), phase_right()), "INS_Opleidingsnaam_2002", id)
    })

    ## Maak right df obv dataset en filters
    dfRight <- shiny::reactive({
      prep_df(
        list(
          faculty_right(),
          phase_right(),
          programmes_right(),
          cohorts()
        ),
        programmes_right(),
        df,
        kleur(),
        "right"
      )
    })

    # dfBoth <- shiny::reactive({
    #   bind_both(dfLeft(), dfRight(), id)
    # })

    ### Summaries maken adhv selecties ####
    dfLeft_summarized <- shiny::reactive({
      prep_df_summ(dfLeft(), variables(), y())
    })

    dfRight_summarized <- shiny::reactive({
      prep_df_summ(dfRight(), variables(), y())
    })

    ## Maak summary van complete data
    dfBoth_summarized <- shiny::reactive({
      bind_both(dfLeft_summarized(), dfRight_summarized())
    })

    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## PLOTS MAKEN ####
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ### Plot doel geaggregeerd ####
    output$plot_doel_geaggregeerd <-
      plotly::renderPlotly({
        wrapped_chart(dfBoth_summarized(),
                      x(),
                      y(),
                      kleur(),
                      id,
                      df)
      })
  })
}



module_trendline_bench_ui <- function(id, df, df_config = NULL) {
  shiny::fluidPage(
    ## Plots ####
    shiny::column(
      width = 12,
      shiny::fluidRow(
        shinydashboardPlus::box(
          width = 12,
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(shiny::NS(id, "plot_doel_geaggregeerd")), type = 4)
        )
      )
    ),
    ## Filters + Bookmark ####
    ## INFO: Sidebar
    shinydashboardPlus::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "Doel variabele",
          # tabName = "analytics", # childfull menuItems ignore the tabName parameter, they use expandedName ishiny::NStead
          icon = shiny::icon("signal"),
          startExpanded = TRUE,
          pickerVar(id, "y", df_config)
        )
      ),
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "Uitsplitsing",
          startExpanded = TRUE,
          icon = shiny::icon("signal"),
          pickerVar(id, "x", df_config),
          pickerVar(id, "color", df_config, "Kleur")
        )
      ),
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "Filter periode",
          startExpanded = TRUE,
          icon = shiny::icon("signal"),
          pickerValues(
            id = id,
            df = df,
            variable = "cohort",
            role = "both",
            selected = c(
              "2018;INSchrijvingsjaar_EOI",
              "2019;INSchrijvingsjaar_EOI",
              "2020;INSchrijvingsjaar_EOI")
          )
        )
      ),
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "Filters Links",
          startExpanded = FALSE,
          icon = shiny::icon("signal"),
          pickerValues(
            id = id,
            df = df,
            role = "left",
            variable = "phase",
            selected = "B;INS_Opleidingsfase_BPM"
          ),
          pickerValues(
            id = id,
            df = df,
            role = "left",
            variable = "faculty",
            selected = "SBE;INS_Faculteit"
          ),
          pickerValues(
            id = id,
            df = df,
            role = "left",
            variable = "opleiding",
            # TODO Dit wordt overgeschreven door keep_only_relevant_values
            selected = "B International Business Administration;INS_Opleidingsnaam_2002"

          )
        )
      ),
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "Filters Rechts",
          startExpanded = FALSE,
          icon = shiny::icon("signal"),
          pickerValues(
            id = id,
            df = df,
            role = "right",
            variable = "phase",
            selected = "B;INS_Opleidingsfase_BPM"
          ),
          pickerValues(
            id = id,
            df = df,
            role = "right",
            variable = "faculty",
            selected = "SBE;INS_Faculteit"
          ),
          pickerValues(
            id = id,
            df = df,
            role = "right",
            variable = "opleiding",
            # TODO Dit wordt overgeschreven door keep_only_relevant_values
            selected = "Alles"

          )
        )
      )
    )
  )
}
