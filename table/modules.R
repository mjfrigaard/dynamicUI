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


# generate samples --------------------------------------------------------


mod_gen_sample_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6,
        style = "text-align: left;",
          shiny::textInput(inputId = ns("gsamp"), 
            label = shiny::p("Global Sample Size: \n N = "))),
      shiny::column(width = 6, 
        style = "text-align: left;",
        shiny::textInput(inputId = ns("rsamp"), 
          label = shiny::p("Regional Sample Size \n n = ")),
        )
      ),
    shiny::br(),
    shiny::hr(),
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
      shiny::column(6,
        style = "text-align: left;",
          shiny::textInput(inputId = ns("er_trt"), 
            label = shiny::p("Event rate of treatment arm = "))
        ),
      shiny::column(width = 6, 
        style = "text-align: left;",
        shiny::textInput(inputId = ns("er_plac"), 
            label = shiny::p("Event rate of placebo arm = ")),
          ),
        ),
      shiny::br(),
      shiny::hr(),
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
      shiny::column(width = 4,
        style = "text-align: left;",
          shiny::textInput(inputId = ns("mean_trt"), 
            label = shiny::p("Mean of treatment arm = "))
        ),
      shiny::column(width = 4, offset = 2,
        style = "text-align: left;",
          shiny::textInput(inputId = ns("sd_trt"), 
            label = shiny::p("SD of treatment arm = "))
        )
      ),
    shiny::fluidRow(
      shiny::column(width = 4, 
        style = "text-align: left;",
        shiny::textInput(inputId = ns("mean_plac"), 
            label = shiny::p("Mean of placebo arm = ")),
          ),
      shiny::column(width = 4, offset = 2, 
        style = "text-align: left;",
        shiny::textInput(inputId = ns("sd_plac"), 
            label = shiny::p("SD of placebo arm = ")),
          ),
      ),
      shiny::br(),
      shiny::hr(),
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
      shiny::column(width = 4,
        style = "text-align: left;",
          shiny::textInput(inputId = ns("er_trt"), 
            label = shiny::p("Event rate of treatment arm = "))
        ),
      shiny::column(width = 4, offset = 2,
        style = "text-align: left;",
          shiny::textInput(inputId = ns("disp_trt"), 
            label = shiny::p("Dispersion of treatment arm = "))
        )
      ),
    shiny::fluidRow(
      shiny::column(width = 4, 
        style = "text-align: left;",
        shiny::textInput(inputId = ns("er_plac"), 
            label = shiny::p("Event rate of placebo arm = ")),
          ),
      shiny::column(width = 4, offset = 2, 
        style = "text-align: left;",
        shiny::textInput(inputId = ns("disp_plac"), 
            label = shiny::p("Dispersion of placebo arm = ")),
          ),
      ),
      shiny::br(),
      shiny::hr(),
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
        shiny::column(6,
          style = "text-align: left;",
            shiny::textInput(inputId = ns("rand_cent"), 
              label = shiny::p("Centralized randomization: "))),
        shiny::column(width = 6, 
          style = "text-align: left;",
          shiny::textInput(inputId = ns("rand_strat"), 
            label = shiny::p("Stratified by country ")),
          )
        ),
      shiny::br(),
      shiny::hr(),
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