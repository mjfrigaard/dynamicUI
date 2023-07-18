# packages ----------------------------------------------------------------
library(palmerpenguins)
library(dplyr)
library(purrr)
library(shiny)
library(bs4Dash)


# choices -----------------------------------------------------------------
num_choices <- purrr::set_names(
    colnames(
      dplyr::select(palmerpenguins::penguins, where(is.numeric))))

char_choices <- purrr::set_names(
    colnames(
      dplyr::select(palmerpenguins::penguins, !where(is.numeric))))


# mod_vars_ui ----------------------------------------------------------------
mod_vars_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      inputId = ns("x"), label = "Numeric X Variable",
      choices = num_choices
    ),
    shiny::selectInput(
      inputId = ns("y"), label = "Numeric Y Variable",
      choices = NULL
    ),
    shiny::selectInput(
      inputId = ns("chars"), label = "Character Variables",
      choices = char_choices
    )
  )
}

# mod_vars_server ----------------------------------------------------------
mod_vars_server <- function(id) {
  shiny::moduleServer(id = id, module = function(input, output, session) {
    shiny::observe({
      y_choices <- setdiff(x = num_choices, y = input$x)
      shiny::updateSelectInput(
        inputId = "y", choices = y_choices
      )
    }) |>
      shiny::bindEvent(input$x, ignoreInit = FALSE, ignoreNULL = TRUE)

    return(
      shiny::reactive({
        shiny::req(c(input$x, input$y, input$chars))
        dplyr::select(
          palmerpenguins::penguins,
          dplyr::all_of(c(input$x, input$y, input$chars))
        )
      })
    ) |>
      shiny::bindEvent(input$x, input$y, input$chars)
  })
}

# mod_table_ui ---------------------------------------------------------------
mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tableOutput(outputId = ns("tbl")),
    shiny::verbatimTextOutput(outputId = ns("vals"))
  )
}

# mod_table_server ----------------------------------------------------------
mod_table_server <- function(id, vars) {
  shiny::moduleServer(id = id, module = function(input, output, session) {
    cols <- reactive({
      c(
        x_var = names(vars()[1]),
        y_var = names(vars()[2]),
        col_var = names(vars()[3])
      )
    }) |>
      shiny::bindEvent(vars(),
        ignoreNULL = TRUE
      )

    output$tbl <- shiny::renderTable({
      head(vars())
    })
  })
}


# plots -------------------------------------------------------------------
smooth_plot <- function(df, x_var, y_var, col_var, method) {
  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[col_var]]
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = method)
}

point_plot <- function(df, x_var, y_var, col_var) {
  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[col_var]]
    )
  ) +
    ggplot2::geom_point()
}


# app ---------------------------------------------------------------------
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