

filter_app <- function(df, id = "datamods") {

  ui <- datamods::filter_data_ui(id)

  server <- function(input, output, session) {
    datamods::filter_data_server(id, df)
  }

  shiny::shinyApp(ui, server)

}
