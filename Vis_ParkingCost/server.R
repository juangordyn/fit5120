library('shiny')
library('plotly')

cost_df <- read.csv('parking_cost.csv')
cost_df$maximum_stay <- as.factor(cost_df$maximum_stay)

plotProportions <- function(){
  
  fig <- cost_df %>% plot_ly(labels = ~maximum_stay, values = ~parking_composition, textinfo ='parking_composition+percent', hoverinfo = 'text', sort = FALSE,
                                text = ~paste('</br>Maximum Stay Time:', maximum_stay, 'min',
                                              '</br>Propotion of Total Parking:', parking_composition, '%'))
  fig <- fig %>% add_pie(hole = 0.6)
  fig <- fig %>% layout(title = "Proportion of Time Restricted Parking Spaces",  showlegend = T, legend = list(title = list(text = "<b>Time Restriction (min)</b>")),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(fig)
  
}

function(input, output, session){

      
      output$cost_plot <- renderUI({fluidRow(column(1,''),column(10,align = 'center', plotlyOutput("proportionPlot")), column(1,''))})
      output$proportionPlot <- renderPlotly({plotProportions()})
      
      
    }