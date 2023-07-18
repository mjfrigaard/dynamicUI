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