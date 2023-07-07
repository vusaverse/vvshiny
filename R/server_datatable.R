#' Generate Javascript for datatable headers.
#'
#' This function generates Javascript code for datatable headers. The script adjusts header tooltips and names.
#' The abbreviations have a minimum length of 7 characters.
#'
#' @param data An optional data.frame. Default is 'data' in the global environment.
#' @return A character vector containing the JavaScript code.
#' @export
header_callback <- function(data = data) {
  ## De r code heeft geen toegang tot het data-object uit de Javascript functie.
  ## Voeg dit daarom toe als optionele variabele
  ## TODO: minLength instelbaar maken obv breedte bovenliggend element
  ## Zie comment bij: https://vustudentanalytics.atlassian.net/browse/VUSASOFT-3541


  c(
    "function(thead, data, start, end, display){",
    "  var ncols = data[0].length;",
    sprintf(
      "  var shortnames = [%s]",
      paste0(paste0(
        "'", abbreviate(names(data), minlength = 7), "'"
      ), collapse = ",")
    ),
    sprintf(
      "  var tooltips = [%s];",
      paste0(paste0(
        "'", names(data), "'"
      ), collapse = ",")
    ),
    "  for(var i=0; i<ncols; i++){",
    "    $('th:eq('+i+')',thead).attr('title', tooltips[i]).text(shortnames[i]);",
    "  }",
    "}"
  )
}

#' Generate Javascript for datatable cell values.
#'
#' This function generates Javascript code for datatable cell values. The script truncates cell values to 11 characters and adds a tooltip with the full value.
#'
#' @param data A data.frame.
#' @return A character vector containing the JavaScript code.
#' @export
value_callback <- function(data) {
  ## TODO: lengte instelbaar maken obv breedte bovenliggend element
  ## Zie comment bij: https://vustudentanalytics.atlassian.net/browse/VUSASOFT-3541

  c(
    "function(data, type, row, meta) {",
    "return type === 'display' && data.length > 11 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 9) + '...</span>' : data;",
    "}"
  )
}


#' Add header-related Javascript to datatable options.
#'
#' This function takes a list of datatable options and adds the header-related Javascript from 'header_callback' to these options.
#'
#' @param options A list of datatable options.
#' @param data A data.frame.
#' @return The updated list of datatable options.
#' @export
add_with_limit_header_JS <- function(options, data) {
  headerJS <- list(headerCallback = htmlwidgets::JS(header_callback(data)))

  ## Add header code
  options <- c(options, headerJS)

  return(options)
}


#' Add value-related Javascript to datatable options.
#'
#' This function takes a list of datatable options and adds the value-related Javascript from 'value_callback' to these options.
#'
#' @param options A list of datatable options.
#' @param data A data.frame.
#' @return The updated list of datatable options.
#' @export
add_width_limit_values_JS <- function(options, data) {
  valueJS <- list(
    targets = "_all",
    render = htmlwidgets::JS(value_callback(data))
  )

  ## Extract current columnDefs internal lists (if set)
  ## Add valueJS to it and set new ColumnDefs
  ## INFO Code is a bit complex, but this method ensures it works also when columnDefs aren't set
  new_columns_options <- append(options["columnDefs"] %>% unname() %>% rlang::flatten(), list(valueJS))
  new_columns_options <- stats::setNames(list(new_columns_options), "columnDefs")

  options["columnDefs"] <- new_columns_options

  return(options)
}


#' Get basic datatable options.
#'
#' This function returns a list of basic datatable options.
#'
#' @return A list of datatable options.
#' @export
basic_options <- function() {
  list(
    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Dutch.json"),
    pagingType = "full",
    deferRender = TRUE,
    # dom = 'lfrti<"table-button"B>p',
    # dom = "<'table-button'B><lf><t><'row'<'col-sm-4'i><'col-sm-8'p>>",
    dom = "<'row'<'col-sm-6'l><'col-sm-6'f>><'row'<'col-sm-12'tr>><'row'<'col-sm-4'i><'col-sm-8'p>><'table-button paginate_button'B>",
    buttons = c("excel", "pdf", "print"),
    # info = TRUE,
    # processing = TRUE,
    columnDefs = list(
      list(
        targets = "_all",
        className = "dt-center"
      )
    )
  )
}


#' Get advanced datatable options.
#'
#' This function returns a list of advanced datatable options.
#'
#' @return A list of datatable options.
#' @export
advanced_options <- function() {
  list(
    pagingType = "full",
    colReorder = TRUE,
    rowReorder = TRUE,
    deferRender = TRUE,
    lengthChange = TRUE,
    dom = 'lfrti<"table-button"B>p',
    scrollX = "362px",
    buttons = c("colvis", "copy", "csv"),
    info = TRUE,
    processing = TRUE,
    columnDefs = list(
      list(
        targets = "_all",
        className = "dt-center"
      )
    )
  )
}



#' Create a basic datatable.
#'
#' This function creates a basic datatable with various configurable options and features.
#'
#' @param data The data.frame to be displayed.
#' @param rownames A logical value indicating whether to display row names.
#' @param extensions A character vector specifying the DataTables extensions to be used.
#' @param options A list of DataTables options.
#' @param limit_width A character string indicating how to limit column width.
#' @param ... Additional arguments passed to DT::datatable().
#' @return A datatable object.
#' @export

make_basic_table <- function(data,
                             rownames = FALSE,
                             extensions = c("Buttons"),
                             options = basic_options(),
                             limit_width = "values",
                             ...) {
  if (limit_width == "both") {
    ## set JS
    options <- add_width_limit_values_JS(options, data)
    options <- add_with_limit_header_JS(options)
  } else if (limit_width == "values") {
    options <- add_width_limit_values_JS(options, data)
  } else if (limit_width == "headers") {
    options <- add_with_limit_header_JS(options)
  }



  ## Add some basic logic
  if ("pageLength" %in% names(options)) {
    # do nothing
  } else {
    if (nrow(data) <= 15) {
      options <- c(options, paging = FALSE)
      options <- c(options, info = FALSE)
    }
  }

  if ("searching" %in% names(options)) {
    # do nothing
  } else {
    if (nrow(data) <= 15) {
      options <- c(options, searching = FALSE)
    }
  }

  if (nrow(data) > 15 & !("lengthChange" %in% options)) {
    options <- c(options, list(lengthMenu = list(
      c(10, -1),
      c("10", "All")
    )))
  } else {
    options <- c(options, lengthChange = FALSE)
  }

  DT::datatable(data,
    rownames = rownames,
    extensions = extensions,
    options = options,
    ## Escape is always true for security reasons, see documentation
    escape = TRUE,
    style = "bootstrap",
    ...
  )
}


#' Create an advanced datatable.
#'
#' This function creates an advanced datatable with various configurable options and features.
#'
#' @param data The data.frame to be displayed.
#' @param rownames A logical value indicating whether to display row names.
#' @param filter A character string indicating where to display the table filter.
#' @param extensions A character vector specifying the DataTables extensions to be used.
#' @param options A list of DataTables options.
#' @param limit_width A logical value indicating whether to limit column width.
#' @param ... Additional arguments passed to DT::datatable().
#' @return A datatable object.
#' @export
make_advanced_table <- function(
    data,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons", "ColReorder", "RowReorder"),
    options = advanced_options(),
    limit_width = "values",
    ...) {
  if (limit_width == "both") {
    ## set JS
    options <- add_width_limit_values_JS(options, data)
    options <- add_with_limit_header_JS(options)
  } else if (limit_width == "values") {
    options <- add_width_limit_values_JS(options, data)
  } else if (limit_width == "headers") {
    options <- add_with_limit_header_JS(options)
  }

  ## Voeg All als optie lengte tabel toe
  if ("lengthMenu" %in% options) {
    ## do nothing
  } else {
    options <- c(options, list(lengthMenu = list(
      c(10, 25, 50, 100, -1),
      c("10", "25", "50", "100", "All")
    )))
  }

  if ("pageLength" %in% names(options)) {
    # do nothing
  } else {
    if (nrow(data) <= 25) {
      options <- c(options, paging = FALSE)
    }
  }

  DT::datatable(data,
    rownames = rownames,
    extensions = extensions,
    filter = filter,
    options = options,
    ## Escape is always true for security reasons, see documentation
    escape = TRUE,
    ...
  )
}


#' Create a basic datatable for HTML rendering.
#'
#' This function creates a basic datatable with options optimized for HTML rendering.
#'
#' @param data The data.frame to be displayed.
#' @param ... Additional arguments passed to 'make_basic_table()'.
#' @return A datatable object.
#' @export
make_basic_table_html <- function(data, ...) {
  make_basic_table(
    data,
    width = "100%",
    height = "auto",
    options = c(basic_options(), scrollX = TRUE),
    limit_width = NULL,
    ...
  )
}


#' Create an advanced datatable for HTML rendering.
#'
#' This function creates an advanced datatable with options optimized for HTML rendering.
#'
#' @param data The data.frame to be displayed.
#' @param ... Additional arguments passed to 'make_advanced_table()'.
#' @return A datatable object.
#' @export
make_advanced_table_html <- function(data, ...) {
  make_advanced_table(
    data,
    width = "100%",
    height = "auto",
    options = c(basic_options(), scrollX = TRUE),
    limit_width = NULL,
    ...
  )
}