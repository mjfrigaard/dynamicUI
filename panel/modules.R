# mod_ui <- function(id) {
#   ns <- shiny::NS(id)
#   shiny::tagList(
# 
#     )
# }

# mod_server <- function(id) {
#   shiny::moduleServer(id, function(input, output, session) {
#     ns <- session$ns
# 
#   })
# }


# generate samples ------------------------------------------------------


mod_gen_sample_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 6,
      shiny::h2("Global sample size"),
      shiny::br(),
        style = "text-align: left;",
          shiny::numericInput(inputId = ns("gsamp"), 
            # value = , min = , max = , step = ,
            value = 150, min = 25, max = 300, step = 1,
            label = shiny::p("Global Sample Size (N =) "))
        ),
      shiny::column(width = 6, 
      shiny::h2("Regional sample size"),
      shiny::br(),
        style = "text-align: left;",
        shiny::numericInput(inputId = ns("rsamp"), 
          value = 150, min = 25, max = 300, step = 1,
          label = shiny::p("Regional Sample Size (n =) ")),
        )
      ),
    shiny::br(),
    bs4Dash::box(title = shiny::code("dev box"), 
      collapsible = TRUE, 
      width = 12,
      collapsed = TRUE, 
      closable = TRUE,
    shiny::fluidRow(
      shiny::column(width = 6,
        shiny::code("DEV: Global sample"),
        shiny::verbatimTextOutput(
          placeholder = TRUE,
          outputId = ns("g_out"))
        )),
    shiny::fluidRow(
      shiny::column(width = 6,
        shiny::code("DEV: Regional sample"),
        shiny::verbatimTextOutput(
          placeholder = TRUE,
          outputId = ns("r_out"))
        )
      )
    )
    )
}

mod_gen_sample_server <- function(id) {
  
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$g_out <- shiny::renderPrint({
      input$gsamp
    })
    
    output$r_out <- shiny::renderPrint({
      input$rsamp
    })
    
    return(
      shiny::reactive({
          shiny::req(input$gsamp)
          shiny::req(input$rsamp)
            list('global_sample' = input$gsamp,
                 'regional_sample' = input$rsamp)
      })
    )
  })
}

# binomial distribution ---------------------------------------------------
mod_binomial_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 12, offset = 1,
        style = "text-align: left;",
    bs4Dash::box(
      status = "primary",
      width = 4,
      title = "binomial parameters", 
      collapsible = TRUE, 
      collapsed = FALSE, 
      closable = TRUE, 
      solidHeader = TRUE,
          shiny::numericInput(inputId = ns("er_trt"),
            value = 0.5, min = 0.1, max = 1.0, step = 0.01,
            label = shiny::p("Treatment arm event rate")),
          shiny::numericInput(inputId = ns("er_plac"), 
            value = 0.5, min = 0.1, max = 1.0, step = 0.01,
            label = shiny::p("Placebo arm event rate"))
        )
      )
    ),
      shiny::br(),
   bs4Dash::box(
     title = shiny::code("binomial dev box"), 
      collapsible = TRUE, 
      width = 12,
      collapsed = TRUE, 
      closable = TRUE,
    shiny::fluidRow(
      shiny::column(width = 6,
        shiny::code("DEV: samples"),
        shiny::verbatimTextOutput(
          placeholder = TRUE,
          outputId = ns("samples"))
        )),
    shiny::fluidRow(
      shiny::column(width = 6,
        shiny::code("DEV: distribution parameters"),
        shiny::verbatimTextOutput(
          placeholder = TRUE,
          outputId = ns("parameters"))
        )
      )
   )
  )
}

mod_binomial_server <- function(id, samples = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$samples <- shiny::renderPrint({
      c(samples()$global_sample, samples()$regional_sample)
    })
    
    output$parameters <- shiny::renderPrint({
      c(input$er_trt, input$er_plac)
    })
    
    return(
      shiny::reactive({
          shiny::req(input$er_trt)
          shiny::req(input$er_plac)
            list('event_rate_trt' = input$er_trt,
                 'event_rate_plac' = input$er_plac)
      })
    )
  })
}

# normal distribution ---------------------------------------------------
mod_normal_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 12, offset = 4,
        style = "text-align: left;",
    bs4Dash::box(
      status = "secondary",
      width = 4,
      title = "normal parameters", 
      collapsible = TRUE, 
      collapsed = FALSE, 
      closable = TRUE, 
      solidHeader = TRUE,
          shiny::numericInput(inputId = ns("mean_trt"), 
            value = 200, min = 10, max = 500, step = 1,
            label = shiny::p("Treatment arm mean = ")),
          shiny::numericInput(inputId = ns("sd_trt"), 
            value = 260, min = 10, max = 500, step = 1,
            label = shiny::p("Treatment arm SD = ")),
        shiny::numericInput(inputId = ns("mean_plac"), 
          value = 100, min = 10, max = 500, step = 1,
            label = shiny::p("Placebo arm mean = ")),
        shiny::numericInput(inputId = ns("sd_plac"), 
          value = 260, min = 10, max = 500, step = 1,
            label = shiny::p("Placebo arm SD = "))
          )
        )
      ),
      shiny::br(),
   bs4Dash::box(
     title = shiny::code("normal dev box"), 
      collapsible = TRUE, 
      width = 12,
      collapsed = TRUE, 
      closable = TRUE,
      shiny::fluidRow(
        shiny::column(width = 6,
          shiny::code("DEV: samples"),
          shiny::verbatimTextOutput(
            placeholder = TRUE,
            outputId = ns("samples"))
          )
        ),
      shiny::fluidRow(
        shiny::column(width = 6,
          shiny::code("DEV: distribution parameters"),
          shiny::verbatimTextOutput(
            placeholder = TRUE,
            outputId = ns("parameters"))
        )
      )
    )
  )
}

mod_normal_server <- function(id, samples = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$samples <- shiny::renderPrint({
      c(samples()$global_sample, samples()$regional_sample)
    })
    
    output$parameters <- shiny::renderPrint({
      c(input$mean_trt, input$sd_trt, input$sd_plac, input$sd_plac)
    })
    
    return(
      shiny::reactive({
          shiny::req(input$er_trt)
          shiny::req(input$er_plac)
            list('mean_trt' = input$mean_trt,
                 'sd_trt' = input$sd_trt,
                 'mean_plac' = input$mean_plac,
                 'sd_plac' = input$sd_plac)
      })
    )
  })
}

# negative binomial distribution --------------------------------
mod_neg_binom_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 12, offset = 7,
        style = "text-align: left;",
    bs4Dash::box(
      status = "secondary",
      width = 4,
      title = "negative binomial parameters", 
      collapsible = TRUE, 
      collapsed = FALSE, 
      closable = TRUE, 
      solidHeader = TRUE,
          shiny::textInput(inputId = ns("er_trt"), 
            label = shiny::p("Treatment event rate")),
          shiny::textInput(inputId = ns("disp_trt"), 
            label = shiny::p("Treatment arm dispersion")),
        shiny::textInput(inputId = ns("er_plac"), 
            label = shiny::p("Placebo arm event rate")),
        shiny::textInput(inputId = ns("disp_plac"), 
            label = shiny::p("Placebo arm dispersion"))
          )
        )
      ),
      shiny::br(),
   bs4Dash::box(
     title = shiny::code("negative binomial dev box"), 
      collapsible = TRUE, 
      width = 12,
      collapsed = TRUE, 
      closable = TRUE,
      shiny::fluidRow(
        shiny::column(width = 6,
          shiny::code("DEV: samples"),
          shiny::verbatimTextOutput(
            placeholder = TRUE,
            outputId = ns("samples"))
          )
        ),
      shiny::fluidRow(
        shiny::column(width = 6,
          shiny::code("DEV: distribution"),
          shiny::verbatimTextOutput(
            placeholder = TRUE,
            outputId = ns("parameters"))
        )
      )
    )
  )
}

mod_neg_binom_server <- function(id, samples = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$samples <- shiny::renderPrint({
      c(samples()$global_sample, samples()$regional_sample)
    })
    
    output$parameters <- shiny::renderPrint({
      c(input$er_trt, input$disp_trt, 
        input$er_plac, input$disp_plac)
    })
    
    return(
      shiny::reactive({
          shiny::req(input$er_trt)
          shiny::req(input$disp_trt)
          shiny::req(input$er_plac)
          shiny::req(input$disp_plac)
            list('er_trt' = input$er_trt,
                 'disp_trt' = input$disp_trt,
                 'er_plac' = input$er_plac,
                 'disp_plac' = input$disp_plac)
      })
    )
  })
}

# randomization --------------------------------
mod_randomization_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
  shiny::fluidRow(
        shiny::column(12,
          style = "text-align: left;",
            shiny::numericInput(inputId = ns("rand_cent"),
              label = shiny::p("Centralized randomization: "),
              value = 2, min = 1, max = 3, step = 0.5),
          shiny::helpText(
            shiny::code("code for centralized randomization")),
          ),
        shiny::column(width = 12, 
          style = "text-align: left;",
          shiny::numericInput(inputId = ns("rand_strat"), 
            label = shiny::p("Stratified by country "),
            value = 2, min = 1, max = 3, step = 0.5),
          shiny::code("code for stratified randomization"))
            ),
      shiny::br(),
      shiny::hr(),
   bs4Dash::box(
     title = shiny::code("randomization dev box"), 
      collapsible = TRUE, 
      width = 12,
      collapsed = TRUE, 
      closable = TRUE,
      shiny::fluidRow(
        shiny::column(width = 6,
          shiny::code("DEV: samples"),
          shiny::verbatimTextOutput(
            placeholder = TRUE,
            outputId = ns("samples"))
          )
        ),
      shiny::fluidRow(
        shiny::column(width = 6,
          shiny::code("DEV: parameters"),
          shiny::verbatimTextOutput(
            placeholder = TRUE,
            outputId = ns("parameters"))
        )
      )
    )
  )
}

mod_randomization_server <- function(id, samples = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$samples <- shiny::renderPrint({
      if (!is.null(samples)) {
        c(samples()$global_sample, samples()$regional_sample)
      }
    })
    
    output$parameters <- shiny::renderPrint({
      c(input$rand_cent, input$rand_strat)
    })
  })
}

# regional methods --------------------------------
# mod_regional_methods_ui <- function(id) {
#   ns <- shiny::NS(id)
#   shiny::tagList(
# 
#     )
# }

# mod_regional_methods_server <- function(id) {
#   shiny::moduleServer(id, function(input, output, session) {
#     ns <- session$ns
# 
#   })
# }

# prob of success --------------------------------
# mod_probs_success_ui <- function(id) {
#   ns <- shiny::NS(id)
#   shiny::tagList(
# 
#     )
# }

# mod_probs_success_server <- function(id) {
#   shiny::moduleServer(id, function(input, output, session) {
#     ns <- session$ns
# 
#   })
# }

# header contact --------------------------------
#' header_contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_header_contact_ui <- function(id) {
  ns <- NS(id)
  tags$li(
    class = "dropdown",
    actionLink(
      ns("email_link"),
      label = "Contact Us",
      icon = icon("envelope"),
      onclick = "window.open('mailto:RD.QDM-APP-TEAM@gsk.com?subject=QDM%20App&body=Contact%20us%20with%20any%20issues!')"
    )
  )
}

#' header_contact Server Functions
#'
#' @export
#'
mod_header_contact_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_header_contact_ui("header_contact_ui_1")

## To be copied in the server
# mod_header_contact_server("header_contact_ui_1")