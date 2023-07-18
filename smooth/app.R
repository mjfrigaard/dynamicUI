# packages ----------------------------------------------------------------
library(vroom)
library(shiny)
library(bs4Dash)


# modules  ----------------------------------------------------------------
source("modules.R")


# utility functions -------------------------------------------------------
source("utils.R")

penguins <- vroom::vroom(file = "penguins.csv", delim = ",")

smoothApp <- function() {
  shiny::shinyApp(
  ui = bs4Dash::bs4DashPage(
    bs4Dash::bs4DashNavbar(),
    bs4Dash::bs4DashSidebar(
      disable = FALSE,
      collapsed = FALSE,
      fixed = TRUE,
      bs4Dash::bs4SidebarMenu(
        id = "sidebarMenu",
        bs4Dash::bs4SidebarMenuItem(
          text = "Variables",
          tabName = "tab_01"
        ),
        bs4Dash::bs4SidebarMenuItem(
          condition = "input.show == true",
          text = "Graph",
          tabName = "tab_02"
        )
      )
    ),
    bs4Dash::bs4DashBody(
      bs4Dash::bs4TabItems(
        bs4Dash::bs4TabItem(
          tabName = "tab_01",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::h3(shiny::code("Variable tab")),
              shiny::br(),
              mod_vars_ui("vars"),
              shiny::br(),
              shiny::checkboxInput(
                inputId = "show",
                label = "Show Graph Tab",
                value = FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              bs4Dash::box(
                width = 12,
                title = "Reactive values",
                collapsible = TRUE,
                collapsed = TRUE,
                shiny::verbatimTextOutput(outputId = "tab_01_vals")
              )
            )
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "tab_02",
          width = 12,
          shiny::h3(
            shiny::code("Graph tab")
          ),
          shiny::checkboxInput(
            inputId = "table",
            label = "Show data preview",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.table == true",
            # mod_table_ui -----------------------------------------------
            mod_table_ui(id = "tbl")
          ),
          shiny::checkboxInput(
            inputId = "smooth",
            label = "Show smooth method",
            value = FALSE
          ),
          shiny::conditionalPanel(
            condition = "input.smooth == true",
            shiny::selectInput(
              inputId = "smoothMethod",
              label = "Method",
              choices = list("lm", "glm", "gam", "loess", "rlm")
            )
          ),
          shiny::plotOutput(outputId = "graph"),
          shiny::br(),
          bs4Dash::box(
            width = 12,
            title = "Reactive values",
            collapsible = TRUE,
            collapsed = TRUE,
            shiny::verbatimTextOutput(outputId = "vals")
          )
        )
      )
    )
  ),
  server = function(input, output) {
    output$tab_01_vals <- shiny::renderPrint({
      x <- shiny::reactiveValuesToList(x = input, all.names = TRUE)
      print(x)
    })

    x <- mod_vars_server("vars")
    mod_table_server(id = "tbl", vars = x)

    output$vals <- shiny::renderPrint({
      reactiveValuesToList(input, TRUE)
    })

    output$graph <- shiny::renderPlot({
      if (input$smooth) {
        shiny::req(x())
        smooth_plot(
          df = palmerpenguins::penguins,
          x_var = names(x()[1]),
          y_var = names(x()[2]),
          col_var = names(x()[3]),
          method = input$smoothMethod
        )
      } else {
        point_plot(
          df = palmerpenguins::penguins,
          x_var = names(x()[1]),
          y_var = names(x()[2]),
          col_var = names(x()[3])
        )
      }
    })
  }
)
}

smoothApp()