# packages ----------------------------------------------------------------
library(golem)
library(shiny)
# library(rsst)
library(bs4Dash)

# golem_add_external_resources() & app_sys()
source("utils.R")

source("modules.R")

app_ui <- function(request) {
  tagList(
    waiter::useWaiter(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bs4DashPage(
      fullscreen = TRUE,
      dark = NULL,
      bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = span("MRCT App (dev)", style = "color:#f36633"),
          color = "white",
          href = "",
          image = "shiny.png",
          opacity = 0.8
        ),
        # mod_header_contact_ui ----
        mod_header_contact_ui("header_contact_ui")
      ),
      bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          id = "side_menu",
          bs4Dash::menuItem("Multi-Regional Clinical Trials",
            tabName = "intro",
            startExpanded = TRUE
          ),
          bs4Dash::menuItem("Overview",
            tabName = "overview"
          ),
          # Step 1: gen_sample ----
          bs4Dash::menuItem("Samples",
            tabName = "gen_sample"
          ),
          # Step 2: Distributions ----
          bs4Dash::menuItem("Distribution (select one)",
            tabName = "dist",
            bs4Dash::menuSubItem("Binomial",
              tabName = "binomial"
            ),
            bs4Dash::menuSubItem("Normal",
              tabName = "normal"
            ),
            bs4Dash::menuSubItem("Negative Binomial",
              tabName = "neg_binom"
            )
          ),
          # Step 3: Randomization ----
          bs4Dash::menuItem("Randomization",
            tabName = "random"
          ),
          # Step 4: Method for Regional Success ----
          bs4Dash::menuItem("Regional Methods",
            tabName = "regional"
          ),
          # Step 5: Probability of Success ----
          bs4Dash::menuItem("Probability of Success",
            tabName = "probs"
          )
        )
      ),
      bs4Dash::dashboardBody(
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(
          script = "jsfunctions.js",
          functions = c(
            "backgroundCol",
            "addTooltip"
          )
        ),
        bs4Dash::tabItems(
          # intro tab ----
          bs4Dash::tabItem(
            tabName = "intro",
            tags$h3(tags$em("<Intro tab>")),
            tags$img(
              src = "dist_options.png",
              height = "100%",
              width = "100%"
            )
          ),
          # intro sub-item (overview) ----
          bs4Dash::tabItem(
            tabName = "overview",
            tags$h2("<Overview tab>"),
            tags$img(
              src = "more_info.png",
              height = "100%",
              width = "100%"
            )
          ),
          # gen_sample the data (gen_sample) ----
          bs4Dash::tabItem(
            tabName = "gen_sample",
            tags$h2("Generate samples"),
            shiny::br(),
            mod_gen_sample_ui("samples")
          ),
          bs4Dash::tabItem(
            tabName = "dist",
            tags$h2("Selecting Distributions"),
            shiny::strong("Please select one of the distributions below"),
            shiny::br(),
          ),
          bs4Dash::tabItem(
            tabName = "binomial",
            tags$h2("Binomial Distribution"),
            shiny::br(),
            mod_binomial_ui(id = "binom")
          ),
          bs4Dash::tabItem(
            tabName = "normal",
            tags$h2("Normal Distribution"),
            shiny::br(),
            mod_normal_ui(id = "norm")
          ),
          bs4Dash::tabItem(
            tabName = "neg_binom",
            tags$h2("Negative Binomial Distribution"),
            shiny::br(),
            mod_neg_binom_ui(id = "neg_binom")
          ),
          bs4Dash::tabItem(
            tabName = "random",
            tags$h2("Randomization"),
            mod_randomization_ui(id = "rand")
          ),
          bs4Dash::tabItem(
            tabName = "regional",
            tags$h2("Method for Regional Success"),
             shiny::fluidRow(
                    shiny::column(6,
                      style = "text-align: left;",
                        shiny::textInput(inputId = "method_1", 
                          label = shiny::p("Method 1: "))),
                    shiny::column(width = 6, 
                      style = "text-align: left;",
                      shiny::textInput(inputId = "method_2", 
                        label = shiny::p("Method 2: ")),
                      )
                    )
          ),
          bs4Dash::tabItem(
            tabName = "probs",
            tags$h2("Probability of Success"),
             shiny::fluidRow(
                    shiny::column(6,
                      style = "text-align: left;",
                        shiny::textInput(inputId = "rand_cent", 
                          label = shiny::p("Unconditional Success "))),
                    shiny::column(width = 6, 
                      style = "text-align: left;",
                      shiny::textInput(inputId = "rand_strat", 
                        label = shiny::p("Conditional Succss (on overall success) ")),
                      )
                    )
          )
        )
      )
    )
  )
}


app_server <- function(input, output, session) {
  
  x <- mod_gen_sample_server(id = "samples")
  
  mod_binomial_server(id = "binom", samples = x)
  
  mod_normal_server(id = "norm", samples = x)
  
  mod_neg_binom_server(id = "neg_binom", samples = x)
  
  mod_randomization_server(id = "rand")
  
}


shiny::shinyApp(
  ui = app_ui,
  server = app_server
)

