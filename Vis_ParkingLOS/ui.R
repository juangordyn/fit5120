library('shiny')
library('shinyWidgets')

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
        body {
        background-color: white;
      }"))),
  align='center', 
  prettyRadioButtons(inputId="plot_type", label="What do you want to represent?", choices=c("Avg time to find Parking vs Length of stay","Avg distance from Parking to destination vs Length of stay"), selected ="Avg time to find Parking vs Length of stay"),
  uiOutput("parking_plot"))