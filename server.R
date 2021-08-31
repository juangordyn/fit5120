library('jsonlite')
library('request')
library('geosphere')
library('tidyverse')
library('googleway')
library('colorRamps')
library('lubridate')
library('shinybusy')
library('shinyalert')

# server part of the Shiny app, where all the logic is handled


api_key<-'AIzaSyD36r0dBXmooQ2cSEdI88-U7VOFMYOfLlU'
# to retrieve sensor data live
url_sensor_live <- 'https://data.melbourne.vic.gov.au/resource/vh2v-4nfs.json?$limit=20000'
maximum_stay_cost_df <- read.csv('maximum_stay_cost.csv')

retrieve_sensor_live <- function(url){
  request <- GET(url)
  response <- content(request, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame()
  return(df)
}

# wrangle the sensor data to get only the parking spaces relevant to the users input

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
  if(nrow(sensor_live_df)==0){
    return('No results')
  }
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
# color function to represent the parking spaces on the map
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

# function to calculate directions using google's api
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

# retrieve route for Public Transport
retrieve_route_public <-function(public_transport_directions){
  df_route_public <- data.frame(route = public_transport_directions$routes$overview_polyline$points)
  if(is.null(public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$line)){
    text_click_polyline_df <- data.frame(
      distance = public_transport_directions$routes$legs[[1]]$steps[[1]]$distance$text,
      duration = public_transport_directions$routes$legs[[1]]$steps[[1]]$duration$text,
      travel_mode = public_transport_directions$routes$legs[[1]]$steps[[1]]$travel_mode)
      instructions = public_transport_directions$routes$legs[[1]]$steps[[1]]$html_instructions
    
  }
  else{
    text_click_polyline_df <- data.frame(
      distance = public_transport_directions$routes$legs[[1]]$steps[[1]]$distance$text,
      duration = public_transport_directions$routes$legs[[1]]$steps[[1]]$duration$text,
      travel_mode = public_transport_directions$routes$legs[[1]]$steps[[1]]$travel_mode,
      vehicle_name =public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$line$vehicle$name,
      vehicle_line =public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$line$short_name,
      num_stops = public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$num_stops,
      arrival_stop_name = public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$arrival_stop$name,
      departure_stop_name = public_transport_directions$routes$legs[[1]]$steps[[1]]$transit_details$departure_stop$name,
      instructions = public_transport_directions$routes$legs[[1]]$steps[[1]]$html_instructions
      
    )}
  final_string_polyline <- ''
  final_string_polyline_short <- ''
  
  for(i in 1:nrow(text_click_polyline_df)){
    
    travel_mode <- text_click_polyline_df[i, 'travel_mode']
    distance <- text_click_polyline_df[i, 'distance']
    duration <- text_click_polyline_df[i, 'duration']
    vehicle_name <- text_click_polyline_df[i, 'vehicle_name']
    vehicle_line <- text_click_polyline_df[i, 'vehicle_line']
    num_stops <- text_click_polyline_df[i, 'num_stops']
    arrival_name <- text_click_polyline_df[i, 'arrival_stop_name']
    departure_name <- text_click_polyline_df[i, 'departure_stop_name']
    instructions <- text_click_polyline_df[i, 'instructions']
      
    if(travel_mode != 'WALKING'){
      if(i!=nrow(text_click_polyline_df)){
      
      final_string_polyline <- paste(final_string_polyline, paste(vehicle_name,'<br />Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                  ' | ', duration,'<br />From ', departure_name, ' to ', arrival_name, '<br /><br />' ,sep=''), sep='')
      
      
      final_string_polyline_short <-paste(final_string_polyline_short, paste(vehicle_name,'<br />Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                       ' | ', duration, '<br /><br />' ,sep=''), sep='')
      }
      
      else{
        final_string_polyline <- paste(final_string_polyline, paste(vehicle_name,'<br />Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                    ' | ', duration,'<br />From ', departure_name, ' to ', arrival_name, '<br />' ,sep=''), sep='')
        
        final_string_polyline_short <- paste(final_string_polyline_short, paste(vehicle_name,'<br />Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                    ' | ', duration, '<br />' ,sep=''), sep='')
      }
      
      }
    
    
    else{
      if(i!=nrow(text_click_polyline_df)){
      
      final_string_polyline <- paste(final_string_polyline, paste(travel_mode, '<br />', distance,
                                                    ' | ', duration,'<br />',instructions, '<br /><br />', sep=''))
      final_string_polyline_short <- paste(final_string_polyline_short, paste(travel_mode, '<br />', distance,
                                                                 ' | ', duration, '<br /><br />', sep=''))
      }
      else{
        
        final_string_polyline<- paste(final_string_polyline, paste(travel_mode, '<br />', distance,
                                                                   ' | ', duration,'<br />', instructions,'<br />', sep=''))
        final_string_polyline_short<- paste(final_string_polyline_short, paste(travel_mode, '<br />', distance,
                                                                   ' | ', duration,'<br />', sep=''))
      }
      
    }
    
  }
  df_route_public$polyline_hover <- final_string_polyline
  df_route_public$polyline_hover_short <- final_string_polyline_short
  
  return(df_route_public)
}

# retrieve route for private transport
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

# to calculate parking cost
parking_cost_calculator <- function(day, hour, hourly_cost, length_of_stay){
  if(day!='Sunday' & ((as.POSIXct(hour, format='%H:%M')>as.POSIXct('7:30', format='%H:%M')&as.POSIXct(hour, format ='%H:%M')<as.POSIXct('18:30', format='%H:%M')))){
    parking_cost <- round((hourly_cost)*(as.numeric(length_of_stay)/60), 2)
  }
  else{
    parking_cost = 0
  }
  return(parking_cost)
}

# to calculate public transport cost
transport_cost_calculator <- function(hour){
  if((as.POSIXct(hour, format='%H:%M')>as.POSIXct('16:00', format='%H:%M')) & (as.POSIXct(hour, format='%H:%M')<as.POSIXct('19:00', format='%H:%M'))){
    public_fare <- 3.15
  }
  else{
    public_fare <- 4.50
  }
  return(public_fare)
}

# defining env variables to make Reticulate package work (to connect Python with Shiny)
# VIRTUALENV_NAME = '/home/ubuntu/env_yes'

# Sys.setenv(PYTHON_PATH = '/usr/bin/python3')
# Sys.setenv(VIRTUALENV_NAME = paste0(VIRTUALENV_NAME, '/'))
# Sys.setenv(RETICULATE_PYTHON = paste0(VIRTUALENV_NAME, '/bin/python3'))

server <- function(input, output, session){
  # env variables
  # virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  # python_path = Sys.getenv('PYTHON_PATH')
  # reticulate::use_python(python_path)
  # reticulate::use_virtualenv(virtualenv_dir, required = T)
  
  # reactive values
  destination_reactive <- reactiveVal('')
  origin_reactive <- reactiveVal('')
  max_walk_reactive <- reactiveVal('')
  length_of_stay_reactive <- reactiveVal('')
  time_reactive <- reactiveVal('')
  day_reactive <- reactiveVal('')
  input_live_reactive <- reactiveVal('')
  map_to_display_reactive <- reactiveVal('')
  parking_data_reactive_complete <- reactiveVal('')
  parking_data_reactive_incomplete <- reactiveVal('')
  public_steps_long <- reactiveVal()
  public_steps_short <- reactiveVal()
  total_time_public_reactive <- reactiveVal()
  sensor_live_df_reactive <- reactiveVal('')
  parking_statistics_df_reactive <- reactiveVal('No results')
  car_direction_status_reactive <- reactiveVal('NOT FOUND')
  cbd_distance_reactive <- reactiveVal(3500)
  journey_distance_reactive <- reactiveVal(21000)
  total_time_private_reactive <- reactiveVal()
  time_steps_private_reactive <- reactiveVal()
  parking_occupation_reactive <- reactiveVal()
  parking_time_reactive <- reactiveVal()
  parking_cost_reactive <- reactiveVal()
  parking_distance_reactive <- reactiveVal()
  walking_time_reactive <- reactiveVal()
  driving_cost_reactive <- reactiveVal()
  time_restriction_ratio_reactive <- reactiveVal()
  public_transport_cost_reactive <- reactiveVal()
  total_private_cost_reactive <- reactiveVal()
  
  # google map
  output$myMap <- renderGoogle_map({
    google_map(key = api_key,
               location = c(-37.8103, 144.9614),
               zoom = 14,
               scale_control = TRUE, 
               height = 1000)})
  
  # what happens when we click on Compare Journeys
  observeEvent(input$compare_journeys,{
    
    # initializing all the reactive values
    max_walk_reactive(400)
    length_of_stay_reactive(input$length_of_stay)
    time_reactive(input$hour)
    day_reactive(input$day)
    
    if(!is.null(input$jsorigincoords)){
    origin_reactive(c(input$jsorigincoords$lat, input$jsorigincoords$lng))}
    else{
      origin_reactive(input$origin)
    }
    if(!is.null(input$jsdestcoords)){
    destination_reactive(c(input$jsdestcoords$lat, input$jsdestcoords$lng))}
    else{
      destination_reactive(input$destination)
    }
    
    # we will use the functions in this python script
    python_path = '/Users/jgordyn/opt/anaconda3/envs/nlp_new/bin/python3.7'
    reticulate::use_virtualenv('/Users/jgordyn/opt/anaconda3/envs/nlp_new', required = T)
    reticulate::source_python("python_helper_functions.py")
    
    cbd_distance <- 0
    journey_distance <- 0
    
    # show warning if inputs are blank
    if(origin_reactive()== ''| destination_reactive()==''){
      shinyalert(title = "Origin and destination cannot be blank", type = "error")
    }
    else{
      
      # if the person has chosen leaving now
      if(input$leaving=='Now'){
        # googles car directions
        car_directions <- directions(origin_reactive(), destination_reactive(), 'driving', 'now', 'pessimistic')
        # if no result ask user to input address again
        car_direction_status_reactive(car_directions$status)
        if(car_directions$status=='ZERO_RESULTS'| car_directions$status=='NOT_FOUND'){
          shinyalert(title = "Please check your input addresses", type = "error")
        }
        else{
        # lats and longs
        show_modal_spinner(text = 'This might take a little while...')
        end_lat <- car_directions$routes$legs[[1]]$end_location$lat
        end_lng <- car_directions$routes$legs[[1]]$end_location$lng
        start_lat <- car_directions$routes$legs[[1]]$start_location$lat
        start_lng<- car_directions$routes$legs[[1]]$start_location$lng
        
        print(end_lat)
        print(end_lng)
        cbd_centre_lat <- -37.811871
        cbd_centre_lng <- 144.96478
         # check if destination is within cbd
        cbd_distance <- distHaversine(c(end_lng, end_lat), c(cbd_centre_lng, cbd_centre_lat))
        cbd_distance_reactive(cbd_distance)
          # cbd limit is 1.5 km from the center
        if(cbd_distance>2500){
          
          remove_modal_spinner()
          shinyalert(title = "Your destination address is outside Melbourne CBD", type = "error")
          
        }
          
        else{
          journey_distance <- distHaversine(c(end_lng, end_lat), c(start_lng, start_lat))
          journey_distance_reactive(journey_distance)
          
          # we only allow journeys of 20 km or less from the CBD
          if (journey_distance >20000){
            remove_modal_spinner()
            shinyalert(title = "Your origin address is not within a 20 km radius of the CBD", type = "error")
          }
        else{
        # public trasnport journey calculation
        public_transport_directions <- directions(origin_reactive(), destination_reactive(), 'transit', 'now', 'best_guess')
        df_route_public <- retrieve_route_public(public_transport_directions)
        
        df_destination <- cbind(
          car_directions$routes$legs[[1]]$end_location,
          data.frame(address = car_directions$routes$legs[[1]]$end_address))
        
         # mixing parking data with journey results
        print('tuvieja1')
        sensor_live_df <- wrangle_sensor_live_data(max_walk_reactive(), length_of_stay_reactive(), end_lng, end_lat)
        print(sensor_live_df)
        if(sensor_live_df=='No results'){
          remove_modal_spinner()
          shinyalert(title = "We don't have Parking Data for this particular destination. Please select another destination.", type = "error")
        }
        else{
        marker_ids <- unique(sensor_live_df$marker_id)
        now <- with_tz(Sys.time(), 'Australia/Melbourne')
        minutes <- as.character(minute(now))
        if(nchar(minutes) < 2){
          minutes <- paste('0',minutes, sep='')
        }
        hour_now <- as.character(hour(now))
        time <- paste(hour_now,':',minutes, sep='')
        day_of_week <- wday(with_tz(Sys.time(), 'Australia/Melbourne'))
        days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
        day_of_week <- days_of_week[day_of_week]
        
        start_time <- with_tz(Sys.time(), 'Australia/Melbourne')
        parking_statistics_df <- calculate_parking_statistics(end_lat, end_lng, as.integer(length_of_stay_reactive()), as.integer(max_walk_reactive()), time, day_of_week)
        parking_statistics_df_reactive(parking_statistics_df)
        if (parking_statistics_df == 'No results'){
          remove_modal_spinner()
          shinyalert(title = "We don't have Parking Data for this particular destination. Please select another destination.", type = "error")
        }
        else{
          print('tu vieja2')
        print(parking_statistics_df)
        sensor_live_df <- merge(x=sensor_live_df, y=parking_statistics_df, by = 'marker_id')
        
        if(nrow(sensor_live_df)==0){
          sensor_live_df_reactive('No results')
          sensor_live_df_reactive('No results')
          remove_modal_spinner()
          shinyalert(title = "We don't have Parking Data for this particular destination. Please select another destination.", type = "error")
        }
        else{
        sensor_live_df$avg_vacancy <- round(sensor_live_df$avg_vacancy)
        sensor_live_df$avg_occupation <- round(sensor_live_df$avg_occupation)
        print(sensor_live_df)
        
        for(i in 1:nrow(sensor_live_df)){
          vehiclepresent <- sensor_live_df[i, 'status']
          hover_text <- sensor_live_df[i, 'hover_over']
          if(vehiclepresent=='Occupied'){
            print(vehiclepresent)
            occupation_time <- sensor_live_df[i, 'avg_occupation']
            sensor_live_df[i, 'hover_over'] <- paste(hover_text, '<br />Avg. occupation (minutes): ', occupation_time, sep='')
          }
          else{
            vacancy_time <- sensor_live_df[i, 'avg_vacancy']
            sensor_live_df[i, 'hover_over'] <- paste(hover_text, '<br />Avg. vacancy (minutes): ', vacancy_time, sep='')
          }
          
        }
        
        print('tuvieja3')
        parking_data_reactive_complete(sensor_live_df)
        parking_data_reactive_incomplete(sensor_live_df[sensor_live_df$color!='#ECC904', ])
        print('tuvieja4')
        # statistics to print in Dashboard
        parking_occupation <- round(mean(sensor_live_df[, 'occupation_ratio' ]))
        parking_occupation_reactive(parking_occupation)
        time_restriction_ratio <- round((nrow(sensor_live_df[sensor_live_df$color=='#ECC904', ])/nrow(sensor_live_df))*100)
        time_restriction_ratio_reactive(time_restriction_ratio)
        parking_hourly_cost <- sensor_live_df[, 'parking_cost'][1]
        if(parking_hourly_cost>10){
          parking_hourly_cost <- parking_hourly_cost/100
        }
        parking_cost <- parking_cost_calculator(day_of_week, time, parking_hourly_cost, length_of_stay_reactive())
        parking_cost_reactive(parking_cost)
        parking_time <- sensor_live_df[, 'parking_time'][1]
        parking_time_reactive(parking_time)
        parking_distance <- sensor_live_df[, 'parking_distance'][1]
        parking_distance_reactive(parking_distance)
        walking_time <- sensor_live_df[, 'walking_time'][1]
        walking_time_reactive(walking_time)
        car_duration = round((car_directions$routes$legs[[1]]$duration$value)/60)
        total_time_private <- car_duration + parking_time + walking_time
        total_time_private_reactive(total_time_private)
        total_time_public <- round(sum(public_transport_directions$routes$legs[[1]]$steps[[1]]$duration$value)/60)
        total_time_public_reactive(total_time_public)
        print('tuvieja5')
        # private vehicle costs
        car_distance = car_directions$routes$legs[[1]]$distance$value
        petrol_cost_per_litre = 1.27
        avg_fuel_consumption_l_per_km = 0.5
        driving_cost = round((car_distance/1000)*avg_fuel_consumption_l_per_km*petrol_cost_per_litre)
        driving_cost_reactive(driving_cost)
        total_private_cost = driving_cost + parking_cost
        total_private_cost_reactive(total_private_cost)
        
        # public transport costs
        public_transport_cost <- transport_cost_calculator(time_reactive())
        public_transport_cost_reactive(public_transport_cost)
        
         # what to display when clicking on routes
        df_route <- retrieve_route_private(car_directions, parking_time, parking_distance, walking_time)
        time_steps_private <- df_route[, 'polyline_hover'][1]
        time_steps_private_reactive(time_steps_private)
        time_steps_public <- df_route_public[, 'polyline_hover'][1]
        time_steps_public_short <-df_route_public[, 'polyline_hover_short'][1]
        public_steps_long(time_steps_public)
        public_steps_short(time_steps_public_short)
       print('tuvieja6')
       # google map displaying live parking data and routes
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
                                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window='hover_over', mouse_over = 'hover_over') %>% 
        add_markers(data=df_destination, info_window = "address")
      map_title <- 'Public vs Private Journey including Real-Time Parking Availability'
      end_time <- with_tz(Sys.time(), 'Australia/Melbourne')
      print(end_time-start_time)
      remove_modal_spinner()
      print('tuvieja7')
        }
      }
      }
      }
        }
        }
      }
    else{
        # ALL ANALOGOUS TO ABOVE BUT FOR DISPLAYING HISTORICAL PARKING DATA INSTEAD OF LIVE, AND JOURNEYS IN SELECTED TIMES AND DAYS
        # EXCEPT OF RIGHT NOW
        car_directions <- directions(origin_reactive(), destination_reactive(), 'driving', "now", 'pessimistic')
        
        car_direction_status_reactive(car_directions$status)
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
        cbd_distance_reactive(cbd_distance)
        
        if(cbd_distance>2500){
          
          remove_modal_spinner()
          shinyalert(title = "Your destination address is outside Melbourne CBD", type = "error")
          
          
        }
        else{
        journey_distance <- distHaversine(c(end_lng, end_lat), c(start_lng, start_lat))
        journey_distance_reactive(journey_distance)
        
        if (journey_distance >20000){
          remove_modal_spinner()
          shinyalert(title = "Your origin address is not within a 20 km radius of the CBD", type = "error")
        }
        else{
        public_transport_directions <- directions(origin_reactive(), destination_reactive(), 'transit', "now", 'best_guess')
        df_route_public <- retrieve_route_public(public_transport_directions)
        df_destination <- cbind(
          car_directions$routes$legs[[1]]$end_location,
          data.frame(address = car_directions$routes$legs[[1]]$end_address))
        start_time <- with_tz(Sys.time(), 'Australia/Melbourne')
        parking_statistics_df <- calculate_parking_statistics(end_lat, end_lng, as.integer(length_of_stay_reactive()), as.integer(max_walk_reactive()), time_reactive(), day_reactive())
        parking_statistics_df_reactive(parking_statistics_df)
        
        if (parking_statistics_df == 'No results'){
          remove_modal_spinner()
          shinyalert(title = "We don't have Parking Data for this particular destination. Please select another destination.", type = "error")
        }
        else{
        end_time <- with_tz(Sys.time(), 'Australia/Melbourne')
        print(end_time - start_time)
        svals <- parking_statistics_df$occupation_ratio/100
        
        for(i in 1:length(svals)){
          if(svals[i]>1 | svals[i]<0){
            svals[i] <- 1
          }
        }
        f <- colorRamp(c("#FBF8F8", "#FA4A39"))
        parking_statistics_df$color <- rgb(f(svals)/255)
        
        if(day_reactive()!='Sunday' & ((as.POSIXct(time_reactive(), format='%H:%M')>as.POSIXct('7:30', format='%H:%M')&as.POSIXct(time_reactive(), format ='%H:%M')<as.POSIXct('18:30', format='%H:%M')))){
        parking_statistics_df[parking_statistics_df$maximum_stay<as.numeric(length_of_stay_reactive()),'color']<-'#ECC904'}
        
        parking_data_reactive_complete(parking_statistics_df)
        parking_data_reactive_incomplete(parking_statistics_df[parking_statistics_df$color!='#ECC904', ])
        
        # statistics to print in Dashboard
        parking_occupation <- round(mean(parking_statistics_df[, 'occupation_ratio' ]))
        parking_occupation_reactive(parking_occupation)
        time_restriction_ratio <- round((nrow(parking_statistics_df[parking_statistics_df$color=='#ECC904', ])/nrow(parking_statistics_df))*100)
        time_restriction_ratio_reactive(time_restriction_ratio)
        parking_hourly_cost <- parking_statistics_df[, 'parking_cost'][1]
        if(parking_hourly_cost>10){
          parking_hourly_cost <- parking_hourly_cost/100
        }
        parking_cost <- parking_cost_calculator(day_reactive(), time_reactive(), parking_hourly_cost, length_of_stay_reactive())
        parking_cost_reactive(parking_cost)
        parking_time <- parking_statistics_df[, 'parking_time'][1]
        parking_time_reactive(parking_time)
        parking_distance <- parking_statistics_df[, 'parking_distance'][1]
        parking_distance_reactive(parking_distance)
        walking_time <- parking_statistics_df[, 'walking_time'][1]
        walking_time_reactive(walking_time)
        car_duration = round((car_directions$routes$legs[[1]]$duration$value)/60)
        total_time_private <- car_duration + parking_time + walking_time
        total_time_private_reactive(total_time_private)
        total_time_public <- round(sum(public_transport_directions$routes$legs[[1]]$steps[[1]]$duration$value)/60)
        total_time_public_reactive(total_time_public)
        
        # private vehicle costs
        car_distance = car_directions$routes$legs[[1]]$distance$value
        petrol_cost_per_litre = 1.27
        avg_fuel_consumption_l_per_km = 0.5
        driving_cost = round((car_distance/1000)*avg_fuel_consumption_l_per_km*petrol_cost_per_litre)
        driving_cost_reactive(driving_cost)
        total_private_cost = driving_cost + parking_cost
        total_private_cost_reactive(total_private_cost)
        
        # public transport costs
        public_transport_cost <- transport_cost_calculator(time_reactive())
        public_transport_cost_reactive(public_transport_cost)
         
        df_route <- retrieve_route_private(car_directions, parking_time, parking_distance, walking_time)
        time_steps_private <- df_route[, 'polyline_hover'][1]
        time_steps_private_reactive(time_steps_private)
        time_steps_public <- df_route_public[, 'polyline_hover'][1]
        time_steps_public_short <-df_route_public[, 'polyline_hover_short'][1]
        public_steps_long(time_steps_public)
        public_steps_short(time_steps_public_short)
        
        # map showing historical parking data
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
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over = 'hover_information') %>% 
          add_markers(data=df_destination, info_window = "address")
        map_title <- 'Public vs Private Journey including Historical Parking Availability'
        remove_modal_spinner()
        }
        }
        }
        }
        }
        }
    
    if(car_direction_status_reactive()!='ZERO_RESULTS'& car_direction_status_reactive()!='NOT_FOUND' & cbd_distance_reactive()<=2500 & journey_distance_reactive() <=20000 & parking_statistics_df_reactive()!='No results' & (sensor_live_df_reactive()!='No results')){
    output$map_title <- renderText(map_title)
    output$show_non_restricted <- renderUI({
      fluidRow(column(6,style="border-radius:8px; background-color: #7E8BFA; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px", div(prettyCheckbox('restrictions_checkbox', 'Show only parkings within time restriction', FALSE), style = "color:white;")))
      })
    
    output$show_time_statistics <- renderUI({
    fluidRow(width = 12, valueBoxOutput("time_private")  , valueBoxOutput("time_public"), valueBoxOutput("parking_stats"))})
    
    output$show_cost_statistics <- renderUI({
      fluidRow(width = 12, valueBoxOutput("cost_private")  , valueBoxOutput("cost_public"))})
    
     # dashboard stats output
    output$time_private <- renderValueBox({valueBox(paste(formatC(total_time_private_reactive(), format="d", big.mark=','),'min') , HTML(paste('<b>Total Journey Time Private</b><br /><br />', time_steps_private_reactive()), sep=''), icon = icon("clock"), color = "orange")})
    output$parking_stats <- renderValueBox({valueBox(paste(formatC(parking_occupation_reactive(), format="d", big.mark=','),'%') , HTML(paste('<b>Occupation ratio</b><br /><br />Parking Time: ', parking_time_reactive(), 'min<br /><br />Parking Cost: ', parking_cost_reactive(), '$<br /><br />Time restriction non-availability: ', time_restriction_ratio_reactive()), '%', sep=''), icon = icon("parking"),color = "purple")})
    output$cost_private <- renderValueBox({valueBox(paste('$', total_private_cost_reactive()) , HTML(paste('<b>Total Journey Cost Private</b><br /><br />Driving Cost: $', driving_cost_reactive(),'<br />Parking Cost: ', parking_cost_reactive()), sep=''), icon = icon("dollar-sign"), color = "orange")})
    output$cost_public <- renderValueBox({valueBox(paste('$', public_transport_cost_reactive()) , HTML(paste('<b>Total Journey Cost Public</b><br /><br />', sep='')), icon = icon("dollar-sign"),color = "green")})
    
    output$time_public <- renderValueBox({valueBox(paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'min') , HTML(paste('<b>Total Journey Time Public</b><br /><br />', public_steps_short(), '<br /><button class="btn action-button" type="button" id="buttonSeeMore" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><b>See more</b></button>'), sep=''), icon = icon("clock"),color = "green")})
    }
    })
  
  observeEvent(input$buttonSeeLess, {
    output$time_public <- renderValueBox({valueBox(paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'min') , HTML(paste('<b>Total Journey Time Public</b><br /><br />', public_steps_short(), '<br /><button class="btn action-button" type="button" id="buttonSeeMore" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><b>See more</b></button>'), sep=''), icon = icon("clock"),color = "green")})
  })
  
  observeEvent(input$buttonSeeMore, {
    output$time_public <- renderValueBox({valueBox(paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'min') , HTML(paste('<b>Total Journey Time Public</b><br /><br />', public_steps_long(), '<br /><button class="btn action-button" type="button" id="buttonSeeLess" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><b>See less</b></button>'), sep=''), icon = icon("clock"),color = "green")})
  })
  
  # checkbox to make the time restricted parking spots appear/disappear
  observeEvent(input$restrictions_checkbox,{
    if(input$restrictions_checkbox==TRUE){
      if(nrow(parking_data_reactive_incomplete())>0){
        if(input$leaving=='Now'){
          google_map_update(map_id = "myMap") %>% clear_circles %>%
            add_circles(data=parking_data_reactive_incomplete(), lat='lat', lon='lon', 
                        fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over', mouse_over= 'hover_over', update_map_view=FALSE)}
        else{
          google_map_update(map_id = "myMap") %>% clear_circles %>%
            add_circles(data=parking_data_reactive_incomplete(), lat='mean_lat', lon='mean_long', 
                        fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over= 'hover_information', update_map_view=FALSE)
        }
      }
      
      else{
        google_map_update(map_id = "myMap") %>% clear_circles
      }
      
      }
    
    else{
      if(input$leaving=='Now'){
        print('tuvieja3')
        google_map_update(map_id = "myMap") %>% clear_circles %>%
          add_circles(data=parking_data_reactive_complete(), lat='lat', lon='lon', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over', mouse_over = 'hover_over',
                      update_map_view=FALSE)
        }
      else{
        google_map_update(map_id = "myMap") %>% clear_circles %>%
          add_circles(data=parking_data_reactive_complete(), lat='mean_lat', lon='mean_long', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over= 'hover_information',
                      update_map_view=FALSE)
      }
    }
    
  })
  
}
