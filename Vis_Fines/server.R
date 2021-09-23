library('shiny')
library('plotly')

plotOffence <- function(){
  offence_code <- c(205, 207,167,179, 185,203,'Others')
  offence_desc <- c('Parked for period longer than indicated', 'Meter expired / No valid ticket', 'Stopped in a No Stopping Area', 'Stopped in a Loading Zone', 'Stopped in a Permit Zone', 'Stopped in a Parking Area for people with disabilities', 'Stopping in a Taxi Zone, Parking too close to an intersection, etc')
  offence_perc <- c(39, 39, 7, 8, 2, 2, 3)
  offence_df <- data.frame(offence_code = offence_code, offence_desc = offence_desc, offence_perc = offence_perc)
  
  fig <- offence_df %>% plot_ly(labels = ~offence_code, values = ~offence_perc, textinfo ='offence_perc+percent', hoverinfo = 'text', sort = FALSE,
                                text = ~paste('</br>Offence code:', offence_code,
                                              '</br>Description:', offence_desc))
  fig <- fig %>% add_pie(hole = 0.6)
  fig <- fig %>% layout(title = "Proportion of issued Parking Offences",  showlegend = T, legend = list(title = list(text = "<b>Offence code</b>")),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(fig)
  
}

function(input, output, session){
  
      
      output$fines_plot <- renderUI({fluidRow(column(1,''),column(10,align = 'center', plotlyOutput("offencePlot")), column(1,''))})
      output$offencePlot <- renderPlotly({plotOffence()})
      
      
    }