library('shiny')
library('ggplot2')
library('plotly')

parking_df <- read.csv('parking_los.csv')
parking_df$length_of_stay <- as.factor(parking_df$length_of_stay)
hour_range_vector <- c('8:00-11:00', '11:00-15:00', '15:00-19:00', '19:00-22:00')
parking_df$hour_range <- factor(parking_df$hour_range, levels = hour_range_vector)

plotParkingTime <- function(){
  maximum_stay_vector <- parking_df$length_of_stay
  key = row.names(parking_df)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], maximum_stay_vector)
  }
  
  plot_parking_time<- ggplot(data=parking_df, aes(x = length_of_stay, y = parking_time, group = 1, key = key_list, text = paste('</br>Maximum stay:' , length_of_stay, 'min</br>Avg Time to find Parking: ', parking_time, 'min'))) + geom_line()+
    facet_wrap(.~hour_range, ncol=2) + 
    labs(x = 'Length of stay (min)', y='Time to find Parking (min)') + theme(legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle('Time to find Parking-Length of Stay')
  plot_parking_time <- ggplotly(, tooltip = c('text'), source = 'B')
  
  return(plot_parking_time)
}

plotDistance<- function(){
  maximum_stay_vector <- parking_df$length_of_stay
  key = row.names(parking_df)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], maximum_stay_vector)
  }
  
  plot_parking_distance<- ggplot(data=parking_df, aes(x = length_of_stay, y = parking_distance, group = 1, key = key_list, text = paste('</br>Maximum stay:' , length_of_stay, 'min</br>Avg Distance to destination: ', parking_distance, 'min'))) + geom_line()+
    facet_wrap(.~hour_range, ncol=2) +
    labs(x = 'Length of stay (min)', y='Distance from Parking to destination (mts)') + theme(legend.position='none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle('Distance Parking to dest-Length of stay')
  plot_parking_distance <- ggplotly(, tooltip = c('text'), source = 'B')
  
  return(plot_parking_distance)
}



function(input, output, session){
  
  observe({
  if(input$plot_type=='Avg time to find Parking vs Length of stay'){
    output$parking_plot <- renderUI({fluidRow(column(3,''),column(6,align = 'center', plotlyOutput("parkingPlot")), column(3,''))})
    output$parkingPlot <- renderPlotly({plotParkingTime()})
  }
    else{
      output$parking_plot <- renderUI({fluidRow(column(3,''),column(6,align = 'center', plotlyOutput("parkingDistance")), column(3,''))})
      output$parkingDistance <- renderPlotly({plotDistance()})
    }
  })
  
  
  
  
}