library(shiny)
library(shinyWidgets)
library(datamods)
library(MASS)

# Add some NAs to mpg
mtcars_na <- mtcars
mtcars_na[] <- lapply(
  X = mtcars_na,
  FUN = function(x) {
    x[sample.int(n = length(x), size = sample(5:10, 1))] <- NA
    x
  }
)

datetime <- data.frame(
  date = seq(Sys.Date(), by = "day", length.out = 300),
  datetime = seq(Sys.time(), by = "hour", length.out = 300),
  num = sample.int(1e5, 300)
)

one_column_numeric <- data.frame(
  var1 = rnorm(100)
)

basic_vars <- c("INS_Inschrijvingsjaar_EOI", "INS_Faculteit", "INS_Opleidingsnaam_2002")

ui <- function(request) {

  fluidPage(
    tags$h2("Filter data.frame"),
    radioButtons(
      inputId = "dataset",
      label = "Data:",
      choices = c(
        "df",
        "iris",
        "mtcars",
        "mtcars_na",
        "Cars93",
        "datetime",
        "one_column_numeric"
      ),
      inline = TRUE
    ),

    fluidRow(
      column(
        filter_data_ui("filtering", max_height = "500px"),
        width = 3,
        shinyWidgets::pickerInput(
          inputId = "varPicker",
          label = "Choose filters that are available",
          choices = NULL,  # Let's start with all possible variables from datasets as choices
          #selected = ,   # Default selected variables
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `count-selected-text` = "{0} variables selected"
          )
        )
      ),
      column(
        width = 9,
        tags$b("Code dplyr:"),
        verbatimTextOutput(outputId = "code_dplyr"),
        tags$b("Expression:"),
        verbatimTextOutput(outputId = "code"),
        tags$b("Filtered data:"),
        verbatimTextOutput(outputId = "res_str")
      )
    )
  )
}

server <- function(input, output, session) {
  savedFilterValues <- reactiveVal()
  data <- reactive({
    if (input$dataset == "df") {
      df
    } else {
      get(input$dataset)
    }
  })

  vars <- reactive({
    req(input$varPicker)
    # Get selected variables from pickerInput
    vars <- input$varPicker

    # if (any(selected_vars) %in% names(data())) {
    #   vars <- intersect(selected_vars, names(data()))
    # } else {
    #   NULL
    # }
  })

  observeEvent(data(), {

    col_names <- data() %>%
      purrr::keep(~ length(unique(.x)) > 1) %>%
      names()

    basic_selected <- intersect(basic_vars, col_names)
    if (length(basic_selected) == 0) {
      basic_selected <- NULL
    }



    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "varPicker",
      choices = col_names,
      selected = basic_selected
    )
  })

  res_filter_basic <- filter_data_server(
    id = "filtering",
    data = data,
    name = reactive(input$dataset),
    vars = vars,
    defaults = reactive(NULL),
    widget_char = "picker",
    widget_num = "slider",
    widget_date = "slider",
    label_na = "Onbekend",
    value_na = TRUE
  )

  output$code_dplyr <- renderPrint({
    res_filter_basic$code()
  })
  output$code <- renderPrint({
    res_filter_basic$expr()
  })

  output$res_str <- renderPrint({
    str(res_filter_basic$filtered())
  })

}

shinyApp(ui, server)
