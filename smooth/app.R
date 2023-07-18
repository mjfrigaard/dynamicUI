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
          text = "Tab 1",
          tabName = "tab_01"
        ),
        bs4Dash::bs4SidebarMenuItem(
          condition = "input.show == true",
          text = "Tab 2",
          tabName = "tab_02"
        )
      )
    ),
    bs4Dash::bs4DashBody(
      bs4Dash::bs4TabItems(
        bs4Dash::bs4TabItem(
          tabName = "tab_01",
          shiny::h3(shiny::code("Tab 1")),
          shiny::br(),
          shiny::checkboxInput(
            inputId = "show", 
            label = "Show Tab 2",
            value = FALSE
          )
          ),
        bs4Dash::bs4TabItem(
          tabName = "tab_02",
          shiny::h3(shiny::code("Tab 2")),
          shiny::checkboxInput(
            inputId = "smooth",
            label = "Show smooth method", 
            value = FALSE),
      conditionalPanel(
          condition = "input.smooth == true",
        shiny::selectInput(
                inputId = "smoothMethod", 
                label = "Method", 
                choices = list("lm", "glm", "gam", "loess", "rlm"))
            )
          )
        )
      )
    ),
  server = function(input, output) { }
)