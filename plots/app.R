library(shiny)
library(bs4Dash)
library(tidyverse)

plotApp <- function() {
  shinyApp(
  ui = bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "My dashboard",
        color = "primary"
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(
      width = "280px",
      collapsed = FALSE,
      fixed = TRUE,
      bs4Dash::bs4SidebarMenuItem(
        tabName = "graphs",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::selectInput(
              width = "200px",
              inputId = "plotType", label = "Plot Type",
              c(Scatter = "scatter", 
                Histogram = "hist"),
              selected = NULL
            )
          )
        ),
        # Only show this panel if the plot type is a histogram
        bs4Dash::bs4SidebarMenuItem(
          tabName = "graphs",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              condition = "input.plotType == 'hist'",
              shiny::selectInput(
                width = "200px",
                inputId = "breaks",
                label = "Breaks",
                choices = c("Sturges",
                  "Scott",
                  "Freedman-Diaconis",
                  "[Custom]" = "custom"
                )
              )
            )
          ),
          # Only show this panel if Custom is selected
          bs4Dash::bs4SidebarMenuItem(
            tabName = "graphs",
            condition = "input.breaks == 'custom'",
            shiny::sliderInput(
              width = "200px",
              inputId = "breakCount",
              label = "Break Count",
              min = 1, max = 50, value = 10
            )
          )
        )
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        bs4Dash::tabItem(
          tabName = "graphs",
          shiny::plotOutput("plot")
        )
      )
    ),
    controlbar = bs4Dash::dashboardControlbar(disable = TRUE),
    title = "DashboardPage"
  ),
  server = function(input, output) {
    
    # generate some data
    x <- rnorm(100)
    y <- rnorm(100)

    output$plot <- shiny::renderPlot({
      if (input$plotType == "scatter") {
        plot(x, y)
      } else {
        breaks <- input$breaks
        if (breaks == "custom") {
          breaks <- input$breakCount
        }
        hist(x, breaks = breaks)
      }
    })
  }
)
}
plotApp()