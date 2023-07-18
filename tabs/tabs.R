library(shiny)
library(bs4Dash)

tabApp <- function() {
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
          shiny::h1("This is tab 1"),
          shiny::checkboxInput(
            inputId = "show",
            label = "Show Tab 2",
            value = FALSE
          )),
        bs4Dash::bs4TabItem(
          tabName = "tab_02",
          shiny::h1("This is tab 2"))
        )
      )
    ),
  server = function(input, output) { }
)
}
tabApp()