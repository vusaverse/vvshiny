#' @title dropdownTabMenu function
#' @description Dropdown that is actually more of a menu with adapted tasks.
#' @param ... additional arguments.
#' @param type A character vector of either "messages", "notifications", "tasks". Default is c("messages", "notifications", "tasks").
#' @param title The title of the dropdown.
#' @param icon The icon to use in the dropdown. If NULL, defaults will be set based on type.
#' @param .list A list of items to add to the dropdown.
#' @param header The header for the dropdown.
#' @return A dropdown menu in the form of an HTML list.
#' @export

dropdownTabMenu <- function(..., type = c("messages", "notifications", "tasks"), title = NULL, icon = NULL, .list = NULL, header = NULL) {
  ## TODO https://stackoverflow.com/questions/43806738/r-shiny-dashboard-custom-dropdown-menu-in-header
  type <- match.arg(type)

  if (is.null(icon)) {
    icon <- switch(type,
                   messages = shiny::icon("envelope"),
                   notifications = shiny::icon("warning"),
                   tasks = shiny::icon("tasks")
    )
  }

  items <- c(list(...), .list)
  ## TODO This line gave warnings, is it needed?
  #map(items, ~shinydashboardPlus::tagAssert(.x, type = "li"))

  dropdownClass <- paste0("dropdown ", type, "-menu")
  htmltools::tags$li(class = dropdownClass, htmltools::a(href = "#", class = "dropdown-toggle",
                                                         `data-toggle` = "dropdown", icon, title), htmltools::tags$ul(class = "dropdown-menu",
                                                                                                                      if (!is.null(header)) htmltools::tags$li(class = "header", header),
                                                                                                                      htmltools::tags$li(htmltools::tags$ul(class = "menu", items))))
}


#' @title dropdownTabDirect function
#' @description Dropdown that is actually a link to a tab.
#' @param type A character vector of either "messages", "notifications", "tasks". Default is c("messages", "notifications", "tasks").
#' @param tab_name The name of the tab to link to.
#' @param title The title of the dropdown.
#' @param icon The icon to use in the dropdown. If NULL, defaults will be set based on type.
#' @param .list A list of items to add to the dropdown.
#' @param header The header for the dropdown.
#' @return A dropdown menu in the form of an HTML list, where clicking the dropdown directs to a specific tab.
#' @export
dropdownTabDirect <- function(type = c("messages", "notifications", "tasks"), tab_name, title, icon = NULL, .list = NULL, header = NULL) {
  type <- match.arg(type)

  if (is.null(icon)) {
    icon <- switch(type,
                   messages = shiny::icon("envelope"),
                   notifications = shiny::icon("warning"),
                   tasks = shiny::icon("tasks")
    )
  }

  tabSelect = TRUE
  dropdownClass <- paste0("dropdown ", type, "-menu")

  htmltools::tags$li(class = dropdownClass,
                     htmltools::a(
                       href = "#",
                       onclick = paste0("shinyjs.tabSelect('", tab_name, "')"),
                       icon,
                       title,
                       `data-tab-name` = tab_name,
                       class = "dropdown-toggle",
                       `data-toggle` = "dropdown"
                     )
  )
}


#' @title taskItemTab function
#' @description Item for above dropdownActionMenu function.
#' @param text The text to display for the item.
#' @param tab_name The name of the tab to link to. Default is NULL.
#' @param href The href link for the item. If NULL, it defaults to "#".
#' @param tabSelect A boolean indicating whether to select the tab. Default is FALSE.
#' @return An HTML list item.
#' @export
taskItemTab <- function(text, tab_name = NULL, href = NULL, tabSelect = FALSE) {
  ##  TODO https://stackoverflow.com/questions/43806738/r-shiny-dashboard-custom-dropdown-menu-in-header

  if (is.null(href)) href <- "#"

  if (tabSelect) {
    htmltools::tags$li(htmltools::a(onclick = paste0("shinyjs.tabSelect('", tab_name, "')"), text, `data-tab-name` = tab_name))
  } else {
    htmltools::tags$li(htmltools::a(href = href, htmltools::h3(text)))
  }
}
