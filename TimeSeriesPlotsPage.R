library(shinydashboard)
library(data.table)
library(shinyWidgets)
ui_timeSeriesPlotPage_Ordinary <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: aqua;  color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
  ")),
    tabsetPanel(
      tabPanel(
        "Confirmed",
        box(
          title = "New Cases (during the past 24 hours)", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_confirmed_province"))
        ),
        box(
          title = "Cumulative", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_cumConfirmed_province"))
        )
      ),
      tabPanel(
        "Death",
        box(
          title = "New Cases (during the past 24 hours)", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_death_province"))
        ),
        box(
          title = "Cumulative", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_cumDeath_province"))
        )
      ),
      tabPanel(
        "Recovered",
        box(
          title = "New Cases (during the past 24 hours)", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_recovered_province"))
        ),
        box(
          title = "Cumulative", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSerisPlt_cumRecovered_province"))
        )
      )
    )
   )
}
ui_timeSeriesPlotPage_Proportion <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
  ")),
    tabsetPanel(
      tabPanel(
        "Confirmed",
        box(
          title = "New Cases (during the past 24 hours)", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_confirmed_prop_province"))
        ),
        box(
          title = "Cumulative", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_cumConfirmed_prop_province"))
        )
      ),
      tabPanel(
        "Death",
        box(
          title = "New Cases (during the past 24 hours)", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_death_prop_province"))
        ),
        box(
          title = "Cumulative", status = "warning", solidHeader = TRUE, closable = FALSE, collapsible = TRUE, collapsed = FALSE, width = 12,
          ui_timeSeriesPlot(ns("timeSeriesPlt_cumDeath_prop_province"))
        )
      )
    )
  )
}

server_timeSeriesPlotPage_Ordinary <- function(input, output, session, data) {
  data_prop <- compute_proportion(data, pop = data$Population, n = 1e6)
   data_prop <- compute_cum_ranks(data_prop)
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_confirmed_province",
             data = data,
             var2show = "Cases", "confirmed_province", FALSE, n = 1
  )

  callModule(server_timeSeriesPlot, "timeSeriesPlt_cumConfirmed_province",
             data = data,
             var2show = "cum_cases", "cumConfirmed_province", FALSE, n = 1, showLog = TRUE
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_death_province",
             data = data,
             var2show = "Deaths", "death_province", FALSE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_cumDeath_province",
             data = data,
             var2show = "cum_death", "ordcum_death", FALSE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_recovered_province",
             data = data,
             var2show = "Recovered", "cumDeath_province", FALSE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSerisPlt_cumRecovered_province",
             data = data,
             var2show = "cum_recovered", "cumRecovered_province", FALSE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_confirmed_prop_province",
             data = data_prop,
             var2show = "Cases", "confirmed_prop_province", TRUE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_cumConfirmed_prop_province",
             data = data_prop,
             var2show = "cum_cases", "cumConfirmed_prop_province", TRUE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_death_prop_province",
             data = data_prop,
             var2show = "Deaths", "death_prop_province", TRUE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_cumDeath_prop_province",
             data = data_prop,
             var2show = "cum_death", "cumDeath_prop_province", TRUE, n = 1
  )
}
server_timeSeriesPlotPage_Proportion <- function(input, output, session, data) {
  data_prop <- compute_proportion(data, data$Population, n = 1e6)
  callModule(server_timeSeriesPlot, "timeSeriesPlt_confirmed_prop_province",
             data = data_prop,
             var2show = "Cases", "confirmed_prop_province", TRUE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_cumConfirmed_prop_province",
             data = data_prop,
             var2show = "cum_cases", "cumConfirmed_prop_province", TRUE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_death_prop_province",
             data = data_prop,
             var2show = "Deaths", "death_prop_province", TRUE, n = 1
  )
  
  callModule(server_timeSeriesPlot, "timeSeriesPlt_cumDeath_prop_province",
             data = data_prop,
             var2show = "cum_death", "cumDeath_prop_province", TRUE, n = 1
  )
}

ui_timPage <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(ns("Ord_prop"),
                      choices = c("Absolute counts", "Relative counts"), justified = TRUE, selected = "Absolute counts", status = "primary"
    ),
    conditionalPanel("input.Ord_prop == 'Absolute counts'",
                     ns = ns,
                     h2("Absolute counts"),
                     ui_timeSeriesPlotPage_Ordinary(ns("Ordinary"))
    ),
    conditionalPanel("input.Ord_prop == 'Relative counts'",
                     ns = ns,
                     h2("Relative counts"),
                     ui_timeSeriesPlotPage_Proportion(ns("Proportion"))
    )
  )
}

server_timPage <- function(input, output, session, data) {
  callModule(server_timeSeriesPlotPage_Ordinary, "Ordinary", data)
  callModule(server_timeSeriesPlotPage_Proportion, "Proportion", data)
}

ui <- dashboardPage(header = dashboardHeader(), sidebar = dashboardSidebar(), dashboardBody(
  ui_timPage("f_1")
))
server <- function(input, output) {
  data <- data
  callModule(server_timPage, "f_1", data)
}

shinyApp(ui, server)
