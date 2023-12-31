% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui_shinydashboardplus_adaptions.R
\name{dropdownTabDirect}
\alias{dropdownTabDirect}
\title{dropdownTabDirect function}
\usage{
dropdownTabDirect(
  type = c("messages", "notifications", "tasks"),
  tab_name,
  title,
  icon = NULL,
  .list = NULL,
  header = NULL
)
}
\arguments{
\item{type}{A character vector of either "messages", "notifications", "tasks". Default is c("messages", "notifications", "tasks").}

\item{tab_name}{The name of the tab to link to.}

\item{title}{The title of the dropdown.}

\item{icon}{The icon to use in the dropdown. If NULL, defaults will be set based on type.}

\item{.list}{A list of items to add to the dropdown.}

\item{header}{The header for the dropdown.}
}
\value{
A dropdown menu in the form of an HTML list, where clicking the dropdown directs to a specific tab.
}
\description{
Dropdown that is actually a link to a tab.
}
\examples{
dropdownTabDirect(type = "messages", tab_name = "Tab1", title = "Interesting tab")
}
