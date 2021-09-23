library('shiny')
library('shinyWidgets')

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
        body {
        background-color: white;
      }"))),
  align='center',
  br(),
  br(),
  br(),
  br(),
  br(),
  uiOutput("fines_plot"))