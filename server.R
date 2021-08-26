library('jsonlite')
library('request')
library('geosphere')
library('tidyverse')
library('googleway')
library('colorRamps')
library('lubridate')
library('shinybusy')
library('shinyalert')

api_key<-'AIzaSyD36r0dBXmooQ2cSEdI88-U7VOFMYOfLlU'
url_sensor_live <- 'https://data.melbourne.vic.gov.au/resource/vh2v-4nfs.json?$limit=20000'
maximum_stay_cost_df <- read.csv('data/maximum_stay_cost.csv')

retrieve_sensor_live <- function(url){
  request <- GET(url)
  response <- content(request, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame()
  return(df)
}

wrangle_sensor_live_data <- function(max_walk, length_of_stay, end_lng, end_lat){
  sensor_live_df <- retrieve_sensor_live(url_sensor_live)
  names(sensor_live_df)[names(sensor_live_df) == "st_marker_id"] <- "marker_id"
  sensor_live_df <- merge(x = maximum_stay_cost_df, y = sensor_live_df, by = 'marker_id', all.y=TRUE)
  sensor_live_df <- sensor_live_df[!duplicated(sensor_live_df$marker_id), ]
  min_max_stay <- min(sensor_live_df$maximum_stay, na.rm = TRUE)
  max_cost <- max(sensor_live_df$cost_per_hour, na.rm = TRUE)
  sensor_live_df <- sensor_live_df %>% replace_na(list('maximum_stay' = min_max_stay, 'cost_per_hour' = max_cost))
  dist_vector <- c()
  for(i in 1:nrow(sensor_live_df)){
    dist_haversine <- distHaversine(c(end_lng, end_lat), c(as.numeric(sensor_live_df[i, 'lon']),as.numeric(sensor_live_df[i, 'lat'])))
    dist_vector <- c(dist_vector, dist_haversine)
  }
  sensor_live_df$distance <- dist_vector
  sensor_live_df <- sensor_live_df[sensor_live_df$distance <= as.integer(max_walk), ]
  sensor_live_df$restricted <- sensor_live_df$maximum_stay<as.numeric(length_of_stay)
  sensor_live_df$color <- mapply(define_color_parking, sensor_live_df$status, sensor_live_df$restricted)
  sensor_live_df[sensor_live_df$status=='Present', 'status'] = 'Occupied'
  sensor_live_df[sensor_live_df$status=='Unoccupied', 'status'] = 'Free'
  hover_over_vector <- c()
  for(i in 1:nrow(sensor_live_df)){
    bay_id <- sensor_live_df[i, 'bay_id']
    time_restriction <- sensor_live_df[i, 'maximum_stay']
    status <- sensor_live_df[i, 'status']
    hourly_cost <-sensor_live_df[i, 'cost_per_hour']
    distance <- round(sensor_live_df[i, 'distance'])
    if(!is.na(bay_id)){
    hover_over <- paste('Bay ID: ',bay_id,'<br />Dist. to dest (mts): ', distance,"<br />Maximum stay (minutes): ", time_restriction, '<br />Status: ', status,
                        '<br />Hourly Cost ($): ', as.numeric(hourly_cost)/100, sep='')}
    hover_over_vector <- c(hover_over_vector, hover_over)
  }
  sensor_live_df$hover_over <- hover_over_vector
  return(sensor_live_df)
}

define_color_parking <- function(x, y){
  if(y==FALSE){
  if (x=='Present'){
    color = '#FF4545'
  }
  else{
    color = '#4CB27C'
  }}
  else{
    
    color = '#ECC904'
  }
  return(color)
}

directions <- function(input_origin, input_destination, input_mode, departure_time, traffic_model){
  
  directions <- google_directions(
                origin = input_origin,
                destination =  input_destination,
                mode = input_mode,
                units = "metric",
                simplify = TRUE,
                key = api_key,
                departure_time = departure_time,
                traffic_model = traffic_model)
  return(directions)}
  
retrieve_route_public <-function(public_transport_directions){
  df_route_public <- data.frame(route = public_transport_directions$routes$overview_polyline$points)
  if(is.null(public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$line)){
    text_click_polyline_df <- data.frame(
      distance = public_transport_directions$routes$legs[[1]]$steps[[1]]$distance$text,
      duration = public_transport_directions$routes$legs[[1]]$steps[[1]]$duration$text,
      travel_mode = public_transport_directions$routes$legs[[1]]$steps[[1]]$travel_mode)
    
  }
  else{
    text_click_polyline_df <- data.frame(
      distance = public_transport_directions$routes$legs[[1]]$steps[[1]]$distance$text,
      duration = public_transport_directions$routes$legs[[1]]$steps[[1]]$duration$text,
      travel_mode = public_transport_directions$routes$legs[[1]]$steps[[1]]$travel_mode,
      vehicle_name =public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$line$vehicle$name,
      vehicle_line =public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$line$short_name,
      num_stops = public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$num_stops
    )}
  final_string_polyline <- ''
  for(i in 1:nrow(text_click_polyline_df)){
    
    travel_mode <- text_click_polyline_df[i, 'travel_mode']
    distance <- text_click_polyline_df[i, 'distance']
    duration <- text_click_polyline_df[i, 'duration']
    vehicle_name <- text_click_polyline_df[i, 'vehicle_name']
    vehicle_line <- text_click_polyline_df[i, 'vehicle_line']
    num_stops <- text_click_polyline_df[i, 'num_stops']
    
    if(travel_mode != 'WALKING'){
      if(i!=nrow(text_click_polyline_df)){
      
      final_string_polyline <- paste(final_string_polyline, paste(vehicle_name,'<br />Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                  ' | ', duration,'<br /><br />' ,sep=''), sep='')
      
      }
      
      else{
        final_string_polyline <- paste(final_string_polyline, paste(vehicle_name,'<br />Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                    ' | ', duration,'<br />' ,sep=''), sep='')
      }
      
      }
    
    
    else{
      if(i!=nrow(text_click_polyline_df)){
      
      final_string_polyline<- paste(final_string_polyline, paste(travel_mode, '<br />', distance,
                                                    ' | ', duration,'<br /><br />', sep=''))
      }
      else{
        
        final_string_polyline<- paste(final_string_polyline, paste(travel_mode, '<br />', distance,
                                                                   ' | ', duration,'<br />', sep=''))
      }
      
    }
    
  }
  df_route_public$polyline_hover <- final_string_polyline
  return(df_route_public)
}

retrieve_route_private <- function(car_directions, parking_time, parking_distance, walking_time){
  df_route <- data.frame(route = car_directions$routes$overview_polyline$points)
  text_click_polyline_df <- data.frame(duration = car_directions$routes$legs[[1]]$duration$text,
                                       distance = car_directions$routes$legs[[1]]$distance$text)
  text_click_vector <- c()
  for(i in 1:nrow(text_click_polyline_df)){
    distance <- text_click_polyline_df[i, 'distance']
    duration <- text_click_polyline_df[i, 'duration']
    text_click_vector <- c(text_click_vector, paste('Driving<br />', distance,
                                                    ' | ', duration,'<br /><br />Parking<br />', parking_time, ' min<br /><br />Walk CarPark-Dest<br/ >', parking_distance, ' mts | ',walking_time,' min', sep=''))
  }
  df_route$polyline_hover <- text_click_vector
  return(df_route)
}

parking_cost_calculator <- function(day, hour, hourly_cost, length_of_stay){
  if(day!='Sunday' & ((as.POSIXct(hour, format='%H:%M')>as.POSIXct('7:30', format='%H:%M')&as.POSIXct(hour, format ='%H:%M')<as.POSIXct('18:30', format='%H:%M')))){
    parking_cost <- round((hourly_cost)*(as.numeric(length_of_stay)/60), 2)
  }
  else{
    parking_cost = 0
  }
  return(parking_cost)
}

transport_cost_calculator <- function(hour){
  if((as.POSIXct(hour, format='%H:%M')>as.POSIXct('16:00', format='%H:%M')) & (as.POSIXct(hour, format='%H:%M')<as.POSIXct('19:00', format='%H:%M'))){
    public_fare <- 3.15
  }
  else{
    public_fare <- 4.50
  }
  return(public_fare)
}

server <- function(input, output, session){
  destination_reactive <- reactiveVal()
  origin_reactive <- reactiveVal()
  max_walk_reactive <- reactiveVal()
  length_of_stay_reactive <- reactiveVal()
  time_reactive <- reactiveVal()
  day_reactive <- reactiveVal()
  input_live_reactive <- reactiveVal()
  map_to_display_reactive <- reactiveVal()
  parking_data_reactive_complete <- reactiveVal()
  parking_data_reactive_incomplete <- reactiveVal()
  
  output$myMap <- renderGoogle_map({
    google_map(key = api_key,
               location = c(-37.8103, 144.9614),
               zoom = 13,
               scale_control = TRUE, 
               height = 1000)})
  
  observeEvent(input$compare_journeys,{
    
    if(is.null(input$jsValuePretty)){
      destination_reactive(input$destination)
    }
    else{
      destination_reactive(input$jsValuePretty)
    }
    max_walk_reactive(800)
    length_of_stay_reactive(input$length_of_stay)
    time_reactive(input$hour)
    day_reactive(input$day)
    origin_reactive(input$origin)
    
VIRTUALENV_NAME = '/home/ubuntu/env_2'

Sys.setenv(PYTHON_PATH = '/usr/bin/python3.7')
Sys.setenv(VIRTUALENV_NAME = paste0(VIRTUALENV_NAME, '/')) # include '/' => installs into rstudio-connect/apps/
Sys.setenv(RETICULATE_PYTHON = paste0(VIRTUALENV_NAME, '/bin/python3.7'))

server<-function(input, output, session) {
  
  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')
  reticulate::use_python(python_path)
  reticulate::use_virtualenv(virtualenv_dir, required = T)
    
    cbd_distance <- 0
    journey_distance <- 0
    
      if(input$leaving=='Now'){
        
        car_directions <- directions(origin_reactive(), destination_reactive(), 'driving', 'now', 'pessimistic')
        print(car_directions$status)
        if(car_directions$status=='ZERO_RESULTS'| car_directions$status=='NOT_FOUND'){
          shinyalert(title = "Please check your input addresses", type = "error")
        }
        else{
        show_modal_spinner(text = 'This might take a little while...')
        end_lat <- car_directions$routes$legs[[1]]$end_location$lat
        end_lng <- car_directions$routes$legs[[1]]$end_location$lng
        start_lat <- car_directions$routes$legs[[1]]$start_location$lat
        start_lng<- car_directions$routes$legs[[1]]$start_location$lng
        
        print(end_lat)
        print(end_lng)
        cbd_centre_lat <- -37.811871
        cbd_centre_lng <- 144.96478
        cbd_distance <- distHaversine(c(end_lng, end_lat), c(cbd_centre_lng, cbd_centre_lat))
        
        if(cbd_distance>1500){
          
          remove_modal_spinner()
          shinyalert(title = "Your destination address is outside Melbourne CBD", type = "error")
          
        }
          
        else{
          journey_distance <- distHaversine(c(end_lng, end_lat), c(start_lng, start_lat))
          if (journey_distance >20000){
            remove_modal_spinner()
            shinyalert(title = "Your origin address is not within a 20 km radius of the CBD", type = "error")
          }
        else{
        public_transport_directions <- directions(origin_reactive(), destination_reactive(), 'transit', 'now', 'best_guess')
        df_route_public <- retrieve_route_public(public_transport_directions)
        
        df_destination <- cbind(
          car_directions$routes$legs[[1]]$end_location,
          data.frame(address = car_directions$routes$legs[[1]]$end_address))
        
        sensor_live_df <- wrangle_sensor_live_data(max_walk_reactive(), length_of_stay_reactive(), end_lng, end_lat)
        marker_ids <- unique(sensor_live_df$marker_id)
        now <- Sys.time()
        minutes <- as.character(minute(now))
        if(nchar(minutes) < 2){
          minutes <- paste('0',minutes, sep='')
        }
        hour_now <- as.character(hour(now))
        time <- paste(hour_now,':',minutes, sep='')
        print(time)
        day_of_week <- wday(Sys.time())
        days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
        day_of_week <- days_of_week[day_of_week]
        print(day_of_week)
        sensor_live_statistics <- calculate_parking_statistics(end_lat, end_lng, as.integer(length_of_stay_reactive()), as.integer(max_walk_reactive()), time, day_of_week)
        sensor_live_df <- merge(x=sensor_live_df, y=sensor_live_statistics, by = 'marker_id')
        sensor_live_df$avg_vacancy <- round(sensor_live_df$avg_vacancy)
        sensor_live_df$avg_occupation <- round(sensor_live_df$avg_occupation)
        
        for(i in 1:nrow(sensor_live_df)){
          vehiclepresent <- sensor_live_df[i, 'status']
          hover_text <- sensor_live_df[i, 'hover_over']
          if(vehiclepresent=='Occupied'){
            occupation_time <- sensor_live_df[i, 'avg_occupation']
            sensor_live_df[i, 'hover_over'] <- paste(hover_text, '<br />Avg. occupation (minutes): ', occupation_time, sep='')
          }
          else{
            vacancy_time <- sensor_live_df[i, 'avg_vacancy']
            sensor_live_df[i, 'hover_over'] <- paste(hover_text, '<br />Avg. vacancy (minutes): ', vacancy_time, sep='')
          }
          
        }
        
        parking_data_reactive_complete(sensor_live_df)
        parking_data_reactive_incomplete(sensor_live_df[sensor_live_df$color!='#ECC904', ])
        
        # statistics to print in Dashboard
        parking_occupation <- round(mean(sensor_live_df[, 'occupation_ratio' ]))
        time_restriction_ratio <- round((nrow(sensor_live_df[sensor_live_df$color=='#ECC904', ])/nrow(sensor_live_df))*100)
        parking_hourly_cost <- sensor_live_df[, 'parking_cost'][1]
        if(parking_hourly_cost>10){
          parking_hourly_cost <- parking_hourly_cost/100
        }
        parking_cost <- parking_cost_calculator(day_of_week, time, parking_hourly_cost, length_of_stay_reactive())
        parking_time <- sensor_live_df[, 'parking_time'][1]
        parking_distance <- sensor_live_df[, 'parking_distance'][1]
        walking_time <- sensor_live_df[, 'walking_time'][1]
        car_duration = round((car_directions$routes$legs[[1]]$duration$value)/60)
        total_time_private <- car_duration + parking_time + walking_time
        total_time_public <- round(sum(public_transport_directions$routes$legs[[1]]$steps[[1]]$duration$value)/60)
        
        # private vehicle costs
        car_distance = car_directions$routes$legs[[1]]$distance$value
        petrol_cost_per_litre = 1.27
        avg_fuel_consumption_l_per_km = 0.5
        driving_cost = round((car_distance/1000)*avg_fuel_consumption_l_per_km*petrol_cost_per_litre)
        total_private_cost = driving_cost + parking_cost
        
        # public transport costs
        public_transport_cost <- transport_cost_calculator(time_reactive())
        
        df_route <- retrieve_route_private(car_directions, parking_time, parking_distance, walking_time)
        time_steps_private <- df_route[, 'polyline_hover'][1]
        time_steps_public <- df_route_public[, 'polyline_hover'][1]
        
      google_map_update(map_id = "myMap") %>% 
        clear_polylines() %>% clear_circles %>% clear_markers %>% 
              add_polylines(data = df_route,
                                      polyline = "route",
                                      stroke_colour = '#F95E1B',
                                      stroke_weight = 7,
                                      stroke_opacity = 0.7,
                                      info_window = "polyline_hover",
                                      mouse_over = 'Private car journey',
                                      load_interval = 100)%>% 
              add_polylines(data = df_route_public,
                                      polyline = "route",
                                      stroke_colour = '#54C785',
                                      stroke_weight = 7,
                                      stroke_opacity = 0.7,
                                      info_window = "polyline_hover",
                                      mouse_over = 'Public transport journey',
                                      load_interval = 100) %>% 
              add_circles(data=parking_data_reactive_complete(), lat='lat', lon='lon', 
                                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window='hover_over') %>% 
        add_markers(data=df_destination, info_window = "address")
      map_title <- 'Public vs Private Journey including Real-Time Parking Availability'
      remove_modal_spinner()
        }
      }
      }
      }
    else{
        
          days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
          today_dow <- wday(Sys.time())
          input_dow <- match(input$day, days_of_week)
          difference <- abs(input_dow - today_dow)
          if (difference!=0){
            departure_hour <- as.POSIXct(input$hour, format ='%H:%M') + days(difference)
          }
          else{
            if(Sys.time()>as.POSIXct(input$hour, format ='%H:%M')){
            departure_hour <- as.POSIXct(input$hour, format ='%H:%M') + days(7)}
            else{
              departure_time <- as.POSIXct(input$hour, format ='%H:%M')
            }
          }
        car_directions <- directions(origin_reactive(), destination_reactive(), 'driving', departure_hour, 'pessimistic')
        
        if(car_directions$status=='ZERO_RESULTS'|car_directions$status=='NOT FOUND'){
          shinyalert(title = "Please check your input adresses", type = "error")
        }
        else{
        
        show_modal_spinner(text = 'This might take a little while...')
        end_lat <- car_directions$routes$legs[[1]]$end_location$lat
        end_lng <- car_directions$routes$legs[[1]]$end_location$lng
        start_lat <- car_directions$routes$legs[[1]]$start_location$lat
        start_lng <- car_directions$routes$legs[[1]]$start_location$lng
        
        print(end_lat)
        print(end_lng)
        cbd_centre_lat <- -37.811871
        cbd_centre_lng <- 144.963494
        cbd_distance <- distHaversine(c(end_lng, end_lat), c(cbd_centre_lng, cbd_centre_lat))
        
        if(cbd_distance>1500){
          
          remove_modal_spinner()
          shinyalert(title = "Your destination address is outside Melbourne CBD", type = "error")
          
          
        }
        else{
        journey_distance <- distHaversine(c(end_lng, end_lat), c(start_lng, start_lat))
        if (journey_distance >20000){
          remove_modal_spinner()
          shinyalert(title = "Your origin address is not within a 20 km radius of the CBD", type = "error")
        }
        else{
        public_transport_directions <- directions(origin_reactive(), destination_reactive(), 'transit', departure_hour, 'best_guess')
        df_route_public <- retrieve_route_public(public_transport_directions)
        df_destination <- cbind(
          car_directions$routes$legs[[1]]$end_location,
          data.frame(address = car_directions$routes$legs[[1]]$end_address))
        start_time <- Sys.time()
        parking_statistics_df <- calculate_parking_statistics(end_lat, end_lng, as.integer(length_of_stay_reactive()), as.integer(max_walk_reactive()), time_reactive(), day_reactive())
        end_time <- Sys.time()
        print(end_time - start_time)
        svals <- parking_statistics_df$occupation_ratio/100
        f <- colorRamp(c("#FBF8F8", "#FA4A39"))
        parking_statistics_df$color <- rgb(f(svals)/255)
        parking_statistics_df[parking_statistics_df$maximum_stay<as.numeric(length_of_stay_reactive()),'color']<-'#ECC904'
        parking_data_reactive_complete(parking_statistics_df)
        parking_data_reactive_incomplete(parking_statistics_df[parking_statistics_df$color!='#ECC904', ])
        
        # statistics to print in Dashboard
        parking_occupation <- round(mean(parking_statistics_df[, 'occupation_ratio' ]))
        time_restriction_ratio <- round((nrow(parking_statistics_df[parking_statistics_df$color=='#ECC904', ])/nrow(parking_statistics_df))*100)
        parking_hourly_cost <- parking_statistics_df[, 'parking_cost'][1]
        if(parking_hourly_cost>10){
          parking_hourly_cost <- parking_hourly_cost/100
        }
        parking_cost <- parking_cost_calculator(day_reactive(), time_reactive(), parking_hourly_cost, length_of_stay_reactive())
        parking_time <- parking_statistics_df[, 'parking_time'][1]
        parking_distance <- parking_statistics_df[, 'parking_distance'][1]
        walking_time <- parking_statistics_df[, 'walking_time'][1]
        car_duration = round((car_directions$routes$legs[[1]]$duration$value)/60)
        total_time_private <- car_duration + parking_time + walking_time
        total_time_public <- round(sum(public_transport_directions$routes$legs[[1]]$steps[[1]]$duration$value)/60)
        
        # private vehicle costs
        car_distance = car_directions$routes$legs[[1]]$distance$value
        petrol_cost_per_litre = 1.27
        avg_fuel_consumption_l_per_km = 0.5
        driving_cost = round((car_distance/1000)*avg_fuel_consumption_l_per_km*petrol_cost_per_litre)
        total_private_cost = driving_cost + parking_cost
        
        # public transport costs
        public_transport_cost <- transport_cost_calculator(time_reactive())
        
        df_route <- retrieve_route_private(car_directions, parking_time, parking_distance, walking_time)
        time_steps_private <- df_route[, 'polyline_hover'][1]
        time_steps_public <- df_route_public[, 'polyline_hover'][1]
        google_map_update(map_id = "myMap") %>% 
          clear_polylines() %>% clear_circles %>% clear_markers %>% 
          add_polylines(data = df_route,
                        polyline = "route",
                        stroke_colour = '#F95E1B',
                        stroke_weight = 7,
                        stroke_opacity = 0.7,
                        info_window = "polyline_hover",
                        mouse_over = 'Private car journey',
                        load_interval = 100)%>% 
          add_polylines(data = df_route_public,
                        polyline = "route",
                        stroke_colour = '#54C785',
                        stroke_weight = 7,
                        stroke_opacity = 0.7,
                        info_window = "polyline_hover",
                        mouse_over = 'Public transport journey',
                        load_interval = 100) %>% 
          add_circles(data=parking_data_reactive_complete(), lat='mean_lat', lon='mean_long', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information') %>% 
          add_markers(data=df_destination, info_window = "address")
        map_title <- 'Public vs Private Journey including Historical Parking Availability'
        remove_modal_spinner()
        }
    }
        }
    }
    if(car_directions$status!='ZERO_RESULTS'& car_directions$status!='NOT_FOUND' & cbd_distance<=1500 & journey_distance <=20000){
    output$map_title <- renderText(map_title)
    output$show_non_restricted <- renderUI({
      fluidRow(column(6,style="border-radius:8px; background-color: #7E8BFA; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px", div(prettyCheckbox('restrictions_checkbox', 'Show only parkings within time restriction', FALSE), style = "color:white;")))
      })
    
    output$show_time_statistics <- renderUI({
    fluidRow(width = 12, valueBoxOutput("time_private")  , valueBoxOutput("time_public"), valueBoxOutput("parking_stats"))})
    
    output$show_cost_statistics <- renderUI({
      fluidRow(width = 12, valueBoxOutput("cost_private")  , valueBoxOutput("cost_public"))})
    
    output$time_private <- renderValueBox({valueBox(paste(formatC(total_time_private, format="d", big.mark=','),'min') , HTML(paste('<b>Total Journey Time Private</b><br /><br />', time_steps_private), sep=''), icon = icon("clock"), color = "orange")})
    output$time_public <- renderValueBox({valueBox(paste(formatC(total_time_public, format="d", big.mark=','),'min') , HTML(paste('<b>Total Journey Time Public</b><br /><br />', time_steps_public), sep=''), icon = icon("clock"),color = "green")})
    output$parking_stats <- renderValueBox({valueBox(paste(formatC(parking_occupation, format="d", big.mark=','),'%') , HTML(paste('<b>Occupation ratio</b><br /><br />Parking Time: ', parking_time, 'min<br /><br />Parking Cost: ', parking_cost, '$<br /><br />Time restriction non-availability: ', time_restriction_ratio), '%', sep=''), icon = icon("parking"),color = "purple")})
    output$cost_private <- renderValueBox({valueBox(paste('$', total_private_cost) , HTML(paste('<b>Total Journey Cost Private</b><br /><br />Driving Cost: $', driving_cost,'<br />Parking Cost: ', parking_cost), sep=''), icon = icon("dollar-sign"), color = "orange")})
    output$cost_public <- renderValueBox({valueBox(paste('$', public_transport_cost) , HTML(paste('<b>Total Journey Cost Public</b><br /><br />', sep='')), icon = icon("dollar-sign"),color = "green")})
    }
    })
  
  observeEvent(input$restrictions_checkbox,{
    
    if(input$restrictions_checkbox==TRUE){
      if(nrow(parking_data_reactive_incomplete())>0){
        if(input$leaving=='Now'){
          google_map_update(map_id = "myMap") %>% clear_circles %>%
            add_circles(data=parking_data_reactive_incomplete(), lat='lat', lon='lon', 
                        fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over')}
        else{
          google_map_update(map_id = "myMap") %>% clear_circles %>%
            add_circles(data=parking_data_reactive_incomplete(), lat='mean_lat', lon='mean_long', 
                        fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information')
        }
      }
      
      else{
        google_map_update(map_id = "myMap") %>% clear_circles
      }
      
      }
    
    else{
      if(input$leaving=='Now'){
        google_map_update(map_id = "myMap") %>% clear_circles %>%
          add_circles(data=parking_data_reactive_complete(), lat='lat', lon='lon', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over')}
      else{
        google_map_update(map_id = "myMap") %>% clear_circles %>%
          add_circles(data=parking_data_reactive_complete(), lat='mean_lat', lon='mean_long', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information')
      }
    }
    
  })
  
}
