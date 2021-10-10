library('jsonlite')
library('request')
library('geosphere')
library('googleway')
library('colorRamps')
library('lubridate')
library('shinybusy') 
library('shinyalert')
library('shinyBS')

# server part of the Shiny app, where all the logic is handled


api_key<-'AIzaSyD36r0dBXmooQ2cSEdI88-U7VOFMYOfLlU'
# to retrieve sensor data live
url_sensor_live <- 'https://data.melbourne.vic.gov.au/resource/vh2v-4nfs.json?$limit=20000'
maximum_stay_cost_df <- read.csv('maximum_stay_cost.csv')
disabled_data_df <- read.csv('disabled_parking.csv')

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
  sensor_live_df[is.na(sensor_live_df$maximum_stay), 'maximum_stay'] <- min_max_stay
  sensor_live_df[is.na(sensor_live_df$cost_per_hour), 'cost_per_hour'] <- max_cost
  # sensor_live_df <- sensor_live_df %>% replace_na(list('maximum_stay' = min_max_stay, 'cost_per_hour' = max_cost))
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
  sensor_live_df[sensor_live_df$status=='Present', 'status'] = 'Occupied'
  sensor_live_df[sensor_live_df$status=='Unoccupied', 'status'] = 'Free'
  sensor_live_df$color <- mapply(define_color_parking, sensor_live_df$status, sensor_live_df$restricted)
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
  if (x=='Occupied'){
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
      
      final_string_polyline <- paste(final_string_polyline, paste('<img src="bus_32_white.png"><br />',vehicle_name,' Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                  ' | ', duration,'<br />From ', departure_name, ' to ', arrival_name, '<br /><br />' ,sep=''), sep='')
      
      
      final_string_polyline_short <-paste(final_string_polyline_short, paste('<img src="bus_32_white.png"><br />',vehicle_name,' Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                       ' | ', duration, '<br /><br />' ,sep=''), sep='')
      }
      
      else{
        final_string_polyline <- paste(final_string_polyline, paste('<img src="bus_32_white.png"><br />',vehicle_name,' Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                    ' | ', duration,'<br />From ', departure_name, ' to ', arrival_name, '<br />' ,sep=''), sep='')
        
        final_string_polyline_short <- paste(final_string_polyline_short, paste('<img src="bus_32_white.png"><br />',vehicle_name,' Line ',vehicle_line,' | ',num_stops, ' stops | ', distance,
                                                                    ' | ', duration, '<br />' ,sep=''), sep='')
      }
      
      }
    
    
    else{
      if(i!=nrow(text_click_polyline_df)){
      
      final_string_polyline <- paste(final_string_polyline, paste('<img src="pedestrian_32_white.png"><br />', distance,
                                                    ' | ', duration,'<br />',instructions, '<br /><br />', sep=''))
      final_string_polyline_short <- paste(final_string_polyline_short, paste('<img src="pedestrian_32_white.png"><br />', distance,
                                                                 ' | ', duration, '<br /><br />', sep=''))
      }
      else{
        
        final_string_polyline<- paste(final_string_polyline, paste('<img src="pedestrian_32_white.png"><br />', distance,
                                                                   ' | ', duration,'<br />', instructions,'<br />', sep=''))
        final_string_polyline_short<- paste(final_string_polyline_short, paste('<img src="pedestrian_32_white.png"><br />', distance,
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
    text_click_vector <- c(text_click_vector, paste('<img src="car_32_white.png"><br />', distance,
                                                    ' | ', duration,'<br /><br /><img src="parking_32_white.png"><br />Find Parking Time<br />', parking_time, ' mins<br /><br /><img src="pedestrian_32_white.png"><br />From car park to destination<br/ >', parking_distance, ' mts | ',walking_time,' mins', sep=''))
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

has_toll_funct <- function(private_transport_route){
  html_instructions <- private_transport_route$routes$legs[[1]]$steps[[1]]$html_instructions
  for(i in 1:length(html_instructions)){
    has_toll <- html_instructions[i][[1]]
    if(grepl( 'Toll road', has_toll, fixed = TRUE)){
      return('Has tolls')
      break
    }
    if(i==length(html_instructions)){
      return('No tolls')
    }
  }
}

show_disabled <- function(disabled_data_df, dest_lat, dest_lng, max_dist){
  for(i in 1:nrow(disabled_data_df)){
    disabled_lat <- as.numeric(disabled_data_df[i, 'mean_lat'])
    disabled_long <- as.numeric(disabled_data_df[i, 'mean_long'])
  dist <- distHaversine(c(disabled_long, disabled_lat), c(dest_lng, dest_lat))
  disabled_data_df[i,'dist'] <- dist
  address <- disabled_data_df[i, 'rd_seg_dsc']
  disabled_data_df[i, 'hover_over'] <- paste(address, '<br />', 'Distance from destination: ', round(dist), ' mts')
  
  }
  disabled_data_subset <- disabled_data_df[disabled_data_df$dist<=max_dist, ]
  return(disabled_data_subset)
}

public_benefits <- function(total_time_public, total_time_private, total_cost_public, total_cost_private,
                             has_tolls, fine_prob){
  
  if(round(total_time_public)<round(total_time_private)){
    total_time_benefit <- paste('<br /><br /><img src="checked_32.png">   <img src="clock_benefits_32.png"><font size="4"> Save around ', round(total_time_private-total_time_public), ' minutes by using Public Transport.</font>', sep='')
  }
  else{
    total_time_benefit=''
  }
  
  if(total_cost_public<total_cost_private){
    total_cost_benefit <- paste('<br /><br /><img src="checked_32.png"> <img src="money_bag_32.png"><font size="4">Save around $', round(total_cost_private-total_cost_public), ' by using Public Transport excluding tolls and fines.</font>', sep='')
  }
  else{
    total_cost_benefit=''
  }
  
  #if(round(walked_distance_public)<round(walked_distance_private)){
    #total_walk_benefit <- paste('<br />You will be walking around $ ', round(walked_distance_private-walked_distance_public), ' metres less by choosing pubic transport.', sep='')
  #}
  #else{
    #total_walk_benefit=''
  #}
  
  if(has_tolls == 'Has tolls'){
    
    tolls_benefit <- ('<br /><br /><img src="checked_32.png">   <img src="toll_jose_32.png"><font size="4">Your current private vehicle journey includes tolls which can cost you up to $10.27 extra on your total cost of the journey.</font>')
    
  }
  
  else{
    tolls_benefit <- ''
  }
  
  if(fine_prob > 10){
    
    fines_benefit <- paste('<br /><br /><img src="checked_32.png"> <img src="police_jose_32.png"><font size="4"> The current fine probability is ', fine_prob, '% which is considerably high. Parking fines in Melbourne range from $91 to $182 depending on the offence.</font>', sep='')
    
  }
  else{
    fines_benefit <- ''
  }
  emissions_benefit <- '<br /><br /><img src="checked_32.png">   <img src="no_pollution_32.png"><font size="4"> If only 10% of the private car owners shift to using bus as Public Transport, greenhouse gas emission will be reduced by more than 400,000 tonnes a year.</font><br /><br /><img src="checked_32.png">  <img src="no_pollution_32.png"><font size="4"> If only 10% of the private car owners shift to using train and trams, greenhouse gas emission will be reduced by more than 4 Million tonnes a year.</font>'
  
  final_benefits_string <- paste(total_time_benefit, total_cost_benefit, tolls_benefit, fines_benefit, emissions_benefit, sep='')
  return(final_benefits_string)
}

stat_1 <- 'Australia was ranked second-worst in transport energy efficiency in 2019.'
stat_2 <- "Transport is Australia's second largest source of greenhouse gas emissions."
stat_3 <- "Cars are responsible for more than half of Australia's transport emissions."
stat_4 <- "After lockdown, car trips to Melbourne's CBD could increase by 33%."
stat_5 <- "Daily, more than 300,000 car trips are made to Melbourne's CBD."
stat_6 <- "Spencer St. has recorded a 20% increase in Traffic congestion levels since 2018."
stat_7 <- "Australian cars have emitted in 2019 the same amount of gas than the entire Queensland's electricity supply."
stat_8 <- "Demand for public transport is set to increase by 89% in Australia by 2031."

stats_while_waiting <- c(stat_1, stat_2, stat_3, stat_4, stat_5, stat_6, stat_7, stat_8)


# defining env variables to make Reticulate package work (to connect Python with Shiny)
VIRTUALENV_NAME = '/home/ubuntu/env_yes'

Sys.setenv(PYTHON_PATH = '/usr/bin/python3')
Sys.setenv(VIRTUALENV_NAME = paste0(VIRTUALENV_NAME, '/'))
Sys.setenv(RETICULATE_PYTHON = paste0(VIRTUALENV_NAME, '/bin/python3'))

server <- function(input, output, session){
  # env variables
  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')
  reticulate::use_python(python_path)
  reticulate::use_virtualenv(virtualenv_dir, required = T)
  
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
  has_tolls_reactive <- reactiveVal('')
  map_title_reactive <- reactiveVal()
  dest_lat_reactive <- reactiveVal()
  dest_lon_reactive <- reactiveVal()
  svals_reactive <- reactiveVal()
  min_distance_disabled_reactive <- reactiveVal()
  count_disabled_parkings_reactive <- reactiveVal()
  disabled_max_distance_reactive <- reactiveVal(0)
  df_route_private_reactive <- reactiveVal()
  df_route_public_reactive <- reactiveVal()
  leaving_reactive <- reactiveVal()
  display_map_reactive <- reactiveVal(0)
  first_time_map_reactive <- reactiveVal(0)
  
  # google map
  output$myInitialMap <- renderGoogle_map({
    google_map(key = api_key,
               location = c(-37.8103, 144.9614),
               zoom = 15,
               scale_control = TRUE, 
               height = 1000)})
  
  observe({
    if(display_map_reactive()==0){
    output$maps <- renderUI({google_mapOutput("myInitialMap")})
  }
  else{
    output$maps <- renderUI({google_mapOutput("myMap")})
  }
  })
  
  # what happens when we click on Compare Journeys
  observeEvent(input$compare_journeys,{
    leaving_reactive(input$leaving)
    random_stat <- sample(stats_while_waiting, 1)
    show_modal_spinner(text = HTML(paste('<br />While you wait, did you know that...<br /><br/><b>', random_stat, '</b>', sep='')))
    
    # initializing all the reactive values
    max_walk_reactive(600)
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
    # python_path = '/Users/jgordyn/opt/anaconda3/envs/nlp_new/bin/python3.7'
    # reticulate::use_virtualenv('/Users/jgordyn/opt/anaconda3/envs/nlp_new', required = T)
    reticulate::source_python("python_helper_functions.py")
    
    cbd_distance <- 0
    journey_distance <- 0
    
    # show warning if inputs are blank
    if(origin_reactive()== ''| destination_reactive()==''){
      remove_modal_spinner()
      shinyalert(title = "Origin and destination cannot be blank", type = "error")
      
    }
    else{
      
      # if the person has chosen leaving now
      if(leaving_reactive()=='Now'){
        # googles car directions
        car_directions <- directions(origin_reactive(), destination_reactive(), 'driving', 'now', 'pessimistic')
        # if no result ask user to input address again
        car_direction_status_reactive(car_directions$status)
        if(car_directions$status=='ZERO_RESULTS'| car_directions$status=='NOT_FOUND'){
          remove_modal_spinner()
          shinyalert(title = "Please check your input addresses", type = "error")
          
        }
        else{
        # checking if the journey has tolls
        has_tolls <- has_toll_funct(car_directions)
        has_tolls_reactive(has_tolls)
        # lats and longs
        # show_modal_spinner(text = 'This might take a little while...')
        end_lat <- car_directions$routes$legs[[1]]$end_location$lat
        end_lng <- car_directions$routes$legs[[1]]$end_location$lng
        dest_lat_reactive(end_lat)
        dest_lon_reactive(end_lng)
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
        sensor_live_df_map <- wrangle_sensor_live_data(max_walk_reactive(), length_of_stay_reactive(), end_lng, end_lat)
        if(sensor_live_df_map=='No results'){
          remove_modal_spinner()
          shinyalert(title = "We don't have Parking Data for this particular destination. Please select another destination.", type = "error")
          
        }
        else{
        marker_ids <- unique(sensor_live_df_map$marker_id)
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
        sensor_live_df <- merge(x=sensor_live_df_map, y=parking_statistics_df, by = 'marker_id')
        
        if(nrow(sensor_live_df)==0){
          sensor_live_df_reactive('No results')
          sensor_live_df_reactive('No results')
          remove_modal_spinner()
          shinyalert(title = "We don't have Parking Data for this particular destination. Please select another destination.", type = "error")
          
        }
        else{
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
        parking_data_reactive_complete(sensor_live_df_map)
        parking_data_reactive_incomplete(sensor_live_df_map[sensor_live_df_map$color!='#ECC904', ])
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
        df_route_public$polyline_hover <- gsub('bus_32_white', 'bus_32', df_route_public$polyline_hover)
        df_route_public$polyline_hover <- gsub('pedestrian_32_white', 'pedestrian_32', df_route_public$polyline_hover)
        df_route$polyline_hover <- gsub('car_32_white', 'car_32', df_route$polyline_hover)
        df_route$polyline_hover <- gsub('parking_32_white', 'parking_32', df_route$polyline_hover)
        df_route$polyline_hover <- gsub('pedestrian_32_white', 'pedestrian_32', df_route$polyline_hover)
        df_route_private_reactive(df_route)
        df_route_public_reactive(df_route_public)
        
       # google map displaying live parking data and routes
      display_map_reactive(1)
      if(first_time_map_reactive() == 0){
      output$myMap <- renderGoogle_map({
      google_map(key = api_key,
                 location = c(-37.8103, 144.9614),
                 zoom = 15,
                 scale_control = TRUE, 
                 height = 1000) %>%
              add_polylines(data = df_route,
                                      polyline = "route",
                                      stroke_colour = '#F95E1B',
                                      stroke_weight = 7,
                                      stroke_opacity = 0.7,
                                      info_window = "polyline_hover",
                                      mouse_over = '<b>Private vehicle journey</b> <br />Click to see detail',
                                      load_interval = 100, update_map_view = FALSE)%>% 
              add_polylines(data = df_route_public,
                                      polyline = "route",
                                      stroke_colour = '#54C785',
                                      stroke_weight = 7,
                                      stroke_opacity = 0.7,
                                      info_window = "polyline_hover",
                                      mouse_over = '<b>Public transport journey</b> <br />Click to see detail',
                                      load_interval = 100, update_map_view = FALSE) %>% 
              add_circles(data=parking_data_reactive_complete(), lat='lat', lon='lon', 
                                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window='hover_over', mouse_over = 'hover_over', update_map_view = TRUE) %>% 
        add_markers(data=df_destination, info_window = "address")
        })
      first_time_map_reactive(1)
      }
      else{
        google_map_update(map_id = "myMap") %>% 
        clear_polylines() %>% clear_circles() %>% clear_markers() %>% 
          add_polylines(data = df_route,
                        polyline = "route",
                        stroke_colour = '#F95E1B',
                        stroke_weight = 7,
                        stroke_opacity = 0.7,
                        info_window = "polyline_hover",
                        mouse_over = '<b>Private vehicle journey</b> <br />Click to see detail',
                        load_interval = 100, update_map_view = FALSE)%>% 
          add_polylines(data = df_route_public,
                        polyline = "route",
                        stroke_colour = '#54C785',
                        stroke_weight = 7,
                        stroke_opacity = 0.7,
                        info_window = "polyline_hover",
                        mouse_over = '<b>Public transport journey</b> <br />Click to see detail',
                        load_interval = 100, update_map_view = FALSE) %>% 
          add_circles(data=parking_data_reactive_complete(), lat='lat', lon='lon', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window='hover_over', mouse_over = 'hover_over', update_map_view = TRUE) %>% 
          add_markers(data=df_destination, info_window = "address")
      }
      map_title <- 'Public vs Private Journey including Real-Time Parking Availability'
      map_title_reactive(map_title)
      end_time <- with_tz(Sys.time(), 'Australia/Melbourne')
      print(end_time-start_time)
      remove_modal_spinner()
      
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
      
      days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
      today_dow <- wday(with_tz(Sys.time(), 'Australia/Melbourne'))
      input_dow <- match(input$day, days_of_week)
      difference <- abs(input_dow - today_dow)
      if (difference!=0)
      {
        departure_hour <- with_tz(as.POSIXct(input$hour, format ='%H:%M') + days(difference), 'Australia/Melbourne')
      } else
      {
        if(with_tz(Sys.time(), 'Australia/Melbourne')>with_tz(as.POSIXct(input$hour, format ='%H:%M'), 'Australia/Melbourne')){
          departure_hour <- with_tz(as.POSIXct(input$hour, format ='%H:%M') + days(7), 'Australia/Melbourne')
        } else
        {
          departure_hour <- with_tz(as.POSIXct(input$hour, format ='%H:%M'), 'Australia/Melbourne')
        }
      }
        car_directions <- directions(origin_reactive(), destination_reactive(), 'driving', departure_hour, 'pessimistic')
        print('here1')
        car_direction_status_reactive(car_directions$status)
        if(car_directions$status=='ZERO_RESULTS'|car_directions$status=='NOT FOUND'){
          remove_modal_spinner()
          shinyalert(title = "Please check your input adresses", type = "error")
          
        }
        else{
        # checking if the private route has tolls
        has_tolls <- has_toll_funct(car_directions)
        has_tolls_reactive(has_tolls)
        # show_modal_spinner(text = 'This might take a little while...')
        end_lat <- car_directions$routes$legs[[1]]$end_location$lat
        end_lng <- car_directions$routes$legs[[1]]$end_location$lng
        dest_lat_reactive(end_lat)
        dest_lon_reactive(end_lng)
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
          print('here2')
        public_transport_directions <- directions(origin_reactive(), destination_reactive(), 'transit', departure_hour, 'best_guess')
        print('here3')
        df_route_public <- retrieve_route_public(public_transport_directions)
        df_destination <- cbind(
          car_directions$routes$legs[[1]]$end_location,
          data.frame(address = car_directions$routes$legs[[1]]$end_address))
        print('here4')
        start_time <- with_tz(Sys.time(), 'Australia/Melbourne')
        print('here5')
        parking_statistics_df <- calculate_parking_statistics(end_lat, end_lng, as.integer(length_of_stay_reactive()), as.integer(max_walk_reactive()), time_reactive(), day_reactive())
        parking_statistics_df_reactive(parking_statistics_df)
        print('here6')
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
        svals_reactive(svals)
        f <- colorRamp(c("#FACAC4", "#FA4A39"))
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
        df_route_public$polyline_hover <- gsub('bus_32_white', 'bus_32', df_route_public$polyline_hover)
        df_route_public$polyline_hover <- gsub('pedestrian_32_white', 'pedestrian_32', df_route_public$polyline_hover)
        df_route$polyline_hover <- gsub('car_32_white', 'car_32', df_route$polyline_hover)
        df_route$polyline_hover <- gsub('parking_32_white', 'parking_32', df_route$polyline_hover)
        df_route$polyline_hover <- gsub('pedestrian_32_white', 'pedestrian_32', df_route$polyline_hover)
        df_route_private_reactive(df_route)
        df_route_public_reactive(df_route_public)
        
        display_map_reactive(1)
        if(first_time_map_reactive() == 0){
        output$myMap <- renderGoogle_map({
          google_map(key = api_key,
                     location = c(-37.8103, 144.9614),
                     zoom = 15,
                     scale_control = TRUE, 
                     height = 1000) %>% 
          add_polylines(data = df_route,
                        polyline = "route",
                        stroke_colour = '#F95E1B',
                        stroke_weight = 7,
                        stroke_opacity = 0.7,
                        info_window = "polyline_hover",
                        mouse_over = '<b>Private vehicle journey</b> <br />Click to see detail',
                        load_interval = 100,
                        update_map_view = FALSE)%>% 
          add_polylines(data = df_route_public,
                        polyline = "route",
                        stroke_colour = '#54C785',
                        stroke_weight = 7,
                        stroke_opacity = 0.7,
                        info_window = "polyline_hover",
                        mouse_over = '<b>Public transport journey</b> <br />Click to see detail',
                        load_interval = 100,
                        update_map_view = FALSE) %>% 
          add_circles(data=parking_data_reactive_complete(), lat='mean_lat', lon='mean_long', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over = 'hover_information', update_map_view = TRUE) %>%
          add_markers(data=df_destination, info_window = "address", update_map_view = FALSE)})
          first_time_map_reactive(1)
        }
        else{
          google_map_update(map_id = "myMap") %>%
          clear_polylines() %>% clear_circles() %>% clear_markers() %>% 
            add_polylines(data = df_route,
                          polyline = "route",
                          stroke_colour = '#F95E1B',
                          stroke_weight = 7,
                          stroke_opacity = 0.7,
                          info_window = "polyline_hover",
                          mouse_over = '<b>Private vehicle journey</b> <br />Click to see detail',
                          load_interval = 100,
                          update_map_view = FALSE)%>% 
            add_polylines(data = df_route_public,
                          polyline = "route",
                          stroke_colour = '#54C785',
                          stroke_weight = 7,
                          stroke_opacity = 0.7,
                          info_window = "polyline_hover",
                          mouse_over = '<b>Public transport journey</b> <br />Click to see detail',
                          load_interval = 100,
                          update_map_view = FALSE) %>% 
            add_circles(data=parking_data_reactive_complete(), lat='mean_lat', lon='mean_long', 
                        fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over = 'hover_information', update_map_view = TRUE) %>%
            add_markers(data=df_destination, info_window = "address", update_map_view = FALSE)
        }
        map_title <- 'Public vs Private Journey including Historical Parking Availability'
        map_title_reactive(map_title)
        remove_modal_spinner()
        
        }
        }
        }
        }
        }
        }
    
    if(car_direction_status_reactive()!='ZERO_RESULTS'& car_direction_status_reactive()!='NOT_FOUND' & cbd_distance_reactive()<=2500 & journey_distance_reactive() <=20000 & parking_statistics_df_reactive()!='No results' & (sensor_live_df_reactive()!='No results')){
      output$map_title <- renderUI({
      strong(htmlOutput("titles"))
      })
     output$titles <- renderText(paste('<p style = "color:#7E8BFA;font-family:verdana;">', map_title_reactive(), '</p', sep=''))
     if(map_title_reactive()=='Public vs Private Journey including Real-Time Parking Availability'){
       output$map_legend <- renderUI({
         fluidRow(column(6, img(src = 'map_legend_live.png'), bsTooltip("map_legend", "Time Restriction Non-Compliant: Parking spaces for which your length of stay exceeds their time restrictions.", placement = "top", trigger = "hover",options = NULL)))
       })}
     else{
       output$map_legend <- renderUI({
         fluidRow(column(5, img(src = 'map_legend_historical_2.jpeg'), bsTooltip("map_legend", "Time Restriction Non-Compliant: Parking spaces for which your length of stay exceeds their time restrictions.", placement = "top", trigger = "hover",options = NULL)))
       }) 
     }
    output$map_sliders <- renderUI({
      fluidRow(
        column(4, align='center', sliderInput(inputId = "max_stay_map", label = HTML('<img src= "clock_32.png"><br /><br />Length of stay in minutes'), min = 30, max=240, value = length_of_stay_reactive(), step=30)),   bsTooltip("max_stay_map", HTML("Increase the time you will be staying at the CBD and see how the Parkings that are not within your time limits turn yellow. <br />Please note that this only has an effect on the map and not the statistics below. To update the statistics, click again on Compare Journeys."), placement = "top", trigger = "hover",options = NULL),
               column(2, align = 'center', actionButton("routes_zoom_out", "Routes View", style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76")),bsTooltip("routes_zoom_out", "Click here to have full view of the routes.", placement = "top", trigger = "hover",options = NULL), column(2, align = 'center', actionButton("parking_zoom_in", "Parking View", style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76")), bsTooltip("parking_zoom_in", "Click here to set the zoom at Parking level.", placement = "top", trigger = "hover",options = NULL), column(4, align = 'center', sliderInput(inputId = "disabled_max_distance", label = HTML('<img src="disabled_32.png"><br /><br />Distance in metres'), min = 0, max=1000, value =0, step=250)),   bsTooltip("disabled_max_distance", "Retrieve all disability parking spaces on the map within the selected distance.", placement = "top", trigger = "hover",options = NULL))
                                                                                                                                                                                
    })
    output$show_non_restricted <- renderUI({fluidRow(column(6,div(prettyCheckbox('restrictions_checkbox', 'Show only parkings within time restriction', FALSE), style = "color:#7E8BFA;font-family:verdana;")))})
    output$sep_line <- renderUI({hr(style="border-top: 6px solid #7E8BFA;")})
    output$journey_icons <- renderUI({
      fluidRow(width = 12, column(4,align = "center", img(src = "bus_128.png", width = "25%", height = "25%")), column(4, align = "center", img(src = "car_128.png", width = "25%", height = "25%")), column(4, align = "center", img(src = "parking_128.png", width = "25%", height = "25%")))})
    output$show_time_statistics <- renderUI({
    fluidRow(width = 12, valueBoxOutput("time_public")  , valueBoxOutput("time_private"), valueBoxOutput("parking_stats"))})
    
    if(disabled_max_distance_reactive() !=0){
      output$show_cost_statistics <- renderUI({
      fluidRow(width = 12, valueBoxOutput("cost_public")  , valueBoxOutput("cost_private"), valueBoxOutput("parking_disabled"))})}
    else{
      output$show_cost_statistics <- renderUI({fluidRow(width = 12, valueBoxOutput("cost_public")  , valueBoxOutput("cost_private"))})
    }
    
    output$show_public_benefits <- renderUI({
      fluidRow(width = 12, column(1,''), valueBoxOutput("benefits_public_output", width=10),column(1, ''))
    })
    
    output$expand_contract_all <- renderUI({
      
      fluidRow(width = 12, column(2, align='center', actionButton("expand_all", "Expand all", style=" border-radius: 8px; color: white; background-color: #4472C4; border: 2px solid #4472C4")), column(8, ''), column(2, align='center',actionButton("contract_all", "Contract all", style=" border-radius: 8px; color: white; background-color: #4472C4; border: 2px solid #4472C4")))
      
    })
     # dashboard stats output
    output$time_private <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_private_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandTimePrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>'), sep=''), icon = icon("clock"), color = "orange")})
    output$parking_stats <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(parking_occupation_reactive(), format="d", big.mark=','),'%'),'</center>', sep='')) , HTML(paste('<center><b>Parking Occupation</b></center><br /><center><button class="btn action-button" type="button" id="ExpandParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_down" class="triangle_down"></div></button></center>', sep='')), icon = icon("parking"),color = "purple")})
    output$parking_disabled <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(min_distance_disabled_reactive(), format="d", big.mark=','),'mts'),'</center>', sep='')), HTML(paste('<center><b>to the closest parking space for people with disabilities</center><br /><center><button class="btn action-button" type="button" id="ExpandDisabledParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_up" class="triangle_down"></div></button></center>'), sep=''), icon = icon("wheelchair"),color = "purple")})
    if(has_tolls_reactive()=='No tolls'){
      output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Cost Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandCostPrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>', sep='')), icon = icon("dollar-sign"), color = "orange")})}
    else{
      output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<b><center>+ ADDITIONAL COST OF TOLLS</b></center><center><b>Total Journey Cost Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandCostPrivateTolls" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>', sep='')), icon = icon("dollar-sign"), color = "orange")})}
    
    output$cost_public <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', public_transport_cost_reactive()), '</center>', sep='')) , HTML(paste('<center><b>Total Journey Cost Public</b></center><br /><br />', sep='')), icon = icon("dollar-sign"),color = "green")})
    
    output$time_public <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Public</b></center><br /><center><button class="btn action-button" type="button" id="ExpandTimePublic" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_public" class="triangle_down"></div></button></center>'), sep=''), icon = icon("clock"),color = "green")})
    
    output$benefits_public_output <- renderValueBox({valueBox(HTML('<center>Public Journey Benefits</center>'), HTML((public_benefits(total_time_public_reactive(), total_time_private_reactive(), public_transport_cost_reactive(), total_private_cost_reactive(),
                                                                                                                 has_tolls_reactive(), round(mean(parking_statistics_df_reactive()$fine_prob, na.rm=TRUE))))), icon = icon("smile"), color = "teal")})
    
    }
    })
  
  observeEvent(input$disabled_max_distance, {
    disabled_max_distance_reactive(input$disabled_max_distance)
    if(input$disabled_max_distance!=0){
    disabled_data_df <- show_disabled(disabled_data_df, dest_lat_reactive(), dest_lon_reactive(), input$disabled_max_distance)
    min_distance_disabled <- round(min(disabled_data_df$dist))
    min_distance_disabled_reactive(min_distance_disabled)
    count_disabled_parkings <- nrow(disabled_data_df)
    count_disabled_parkings_reactive(count_disabled_parkings)
    output$show_cost_statistics <- renderUI({
      fluidRow(width = 12, valueBoxOutput("cost_public")  , valueBoxOutput("cost_private"), valueBoxOutput("parking_disabled"))})
    if(nrow(disabled_data_df)>0){
      output$parking_disabled <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(min_distance_disabled_reactive(), format="d", big.mark=','),'mts'),'</center>', sep='')), HTML(paste('<center><b>to the closest parking space for people with disabilities</center><br /><center><button class="btn action-button" type="button" id="ExpandDisabledParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_up" class="triangle_down"></div></button></center>'), sep=''), icon = icon("wheelchair"),color = "purple")})
      if(nrow(disabled_data_df)>4){
    google_map_update(map_id = "myMap") %>% clear_markers(layer_id='disabled_markers') %>%  add_markers(disabled_data_df, layer_id='disabled_markers', lat= 'mean_lat', lon= 'mean_long', marker_icon= 'disabled0.75.png', focus_layer=TRUE, mouse_over='hover_over')}
      else{
        google_map_update(map_id = "myMap") %>% clear_markers(layer_id='disabled_markers') %>%  add_markers(disabled_data_df, layer_id='disabled_markers', lat= 'mean_lat', lon= 'mean_long', marker_icon= 'disabled0.75.png', update_map_view=FALSE, mouse_over='hover_over')
      }
    }
    
    else{
      google_map_update(map_id = "myMap") %>% clear_markers(layer_id='disabled_markers')
      count_disabled_parkings_reactive(0)
      click('parking_zoom_in')
      output$parking_disabled <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(0, format="d", big.mark=',')),'</center>', sep='')), HTML(paste('<center><b>exclusive Parking spaces within ', input$disabled_max_distance, ' mts of the destination<b/></center>' ), sep=''), icon = icon("wheelchair"),color = "purple")})
    }
    
    }
    else{
      google_map_update(map_id = "myMap") %>% clear_markers(layer_id='disabled_markers')
      count_disabled_parkings_reactive(0)
      click('parking_zoom_in')
      output$parking_disabled <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(0, format="d", big.mark=',')),'</center>', sep='')), HTML(paste('<center><b>exclusive Parking spaces within ', input$disabled_max_distance, ' mts of the destination<b/></center>' ), sep=''), icon = icon("wheelchair"),color = "purple")})
    }
    })
  
  observeEvent(input$expand_all, {
    output$parking_stats <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(parking_occupation_reactive(), format="d", big.mark=','),'%'),'</center>', sep='')) , HTML(paste('<center><b>Parking Occupation</b></center><br />Find Parking Time: ', parking_time_reactive(), ' mins<br /><br />Parking Cost: $ ', parking_cost_reactive(), '<br /><br />Time restriction non-availability: ', time_restriction_ratio_reactive(),'%<br /><br />Parking fine probability*: ', round(mean(parking_statistics_df_reactive()$fine_prob, na.rm=TRUE)), ' %<br /><br /><br /><font size="-2">*Parking fine probability has been calculated as the proportion of vehicles that committed a parking infraction over the total vehicles that parked in that area for the selected time and day of the week, according to Historical data.</font><br /><br /><center><button class="btn action-button" type="button" id="ContractParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_up" class="arrow-up"></div></button></center>', sep='')), icon = icon("parking"),color = "purple")})
    output$time_public <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Public</b></center><br />',public_steps_long(),'<br /><center><button class="btn action-button" type="button" id="ContractTimePublic" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_public" class="arrow-up"></div></button></center>'), sep=''), icon = icon("clock"),color = "green")})
    output$time_private <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_private_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Private Vehicle</b></center><br />',  time_steps_private_reactive(), '<br /><br /><center><button class="btn action-button" type="button" id="ContractTimePrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private_up" class="arrow-up"></div></button></center>'), sep=''), icon = icon("clock"), color = "orange")})
    if(has_tolls_reactive()=='No tolls'){output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Cost Private Vehicle</b></center><br />Driving Cost*: $', driving_cost_reactive(),'<br />Parking Cost: $', parking_cost_reactive()), '<br />Tolls: ', has_tolls_reactive(), '<br /><br /><font size="-2">*Driving cost is estimated by multiplying driven distance by 2020 average fuel consumption per km for vehicles in Australia by average price of petrol Litre in Melbourne.</font><br /><br /><center><button class="btn action-button" type="button" id="ContractCostPrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private_up" class="arrow-up"></div></button></center>', sep=''), icon = icon("dollar-sign"), color = "orange")})}
    else{output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<b><center>+ ADDITIONAL COST OF TOLLS</center></b><center><b>Total Journey Cost Private Vehicle</b></center><br />Driving Cost*: $', driving_cost_reactive(),'<br />Parking Cost: $', parking_cost_reactive()), '<br />Tolls: Yes<br /><br /><font size="-2">*Driving cost is estimated by multiplying driven distance by 2020 average fuel consumption per km for vehicles in Australia by average price of petrol Litre in Melbourne.</font><br /><br /><center><button class="btn action-button" type="button" id="ContractCostPrivateTolls" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private_up" class="arrow-up"></div></button></center>', sep=''), icon = icon("dollar-sign"), color = "orange")})}
    output$parking_disabled <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(min_distance_disabled_reactive(), format="d", big.mark=','),'mts'),'</center>', sep='')), HTML(paste('<center><b>to the closest parking space for people with disabilities</b></center><br /><br /><b>',  count_disabled_parkings_reactive(), '</b> exclusive Parking spaces within ', input$disabled_max_distance, ' mts of the destination<br /><br /><center><button class="btn action-button" type="button" id="ContractDisabledParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_up" class="arrow-up"></div></button></center>'), sep=''), icon = icon("wheelchair"),color = "purple")})
  })
  
  observeEvent(input$contract_all, {
    output$parking_stats <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(parking_occupation_reactive(), format="d", big.mark=','),'%'),'</center>', sep='')) , HTML(paste('<center><b>Parking Occupation</b></center><br /><center><button class="btn action-button" type="button" id="ExpandParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_down" class="triangle_down"></div></button></center>', sep='')), icon = icon("parking"),color = "purple")})
    output$time_public <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Public</b></center><br /><center><button class="btn action-button" type="button" id="ExpandTimePublic" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_public" class="triangle_down"></div></button></center>'), sep=''), icon = icon("clock"),color = "green")})
    output$time_private <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_private_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandTimePrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>'), sep=''), icon = icon("clock"), color = "orange")})
    if(has_tolls_reactive()=='No tolls'){output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Cost Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandCostPrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>', sep='')), icon = icon("dollar-sign"), color = "orange")})}
    else{output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<b><center>+ ADDITIONAL COST OF TOLLS</b></center><center><b>Total Journey Cost Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandCostPrivateTolls" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>', sep='')), icon = icon("dollar-sign"), color = "orange")})}
    output$parking_disabled <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(min_distance_disabled_reactive(), format="d", big.mark=','),'mts'),'</center>', sep='')), HTML(paste('<center><b>to the closest parking space for people with disabilities</center><br /><center><button class="btn action-button" type="button" id="ExpandDisabledParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_up" class="triangle_down"></div></button></center>'), sep=''), icon = icon("wheelchair"),color = "purple")})
    })
  
  
  observeEvent(input$ExpandDisabledParking, {
    
    output$parking_disabled <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(min_distance_disabled_reactive(), format="d", big.mark=','),'mts'),'</center>', sep='')), HTML(paste('<center><b>to the closest parking space for people with disabilities</b></center><br /><br /><b>',  count_disabled_parkings_reactive(), '</b> exclusive Parking spaces within ', input$disabled_max_distance, ' mts of the destination<br /><br /><center><button class="btn action-button" type="button" id="ContractDisabledParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_up" class="arrow-up"></div></button></center>'), sep=''), icon = icon("wheelchair"),color = "purple")})
    
  })
  
  observeEvent(input$ContractDisabledParking, {
  
  output$parking_disabled <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(min_distance_disabled_reactive(), format="d", big.mark=','),'mts'),'</center>', sep='')), HTML(paste('<center><b>to the closest parking space for people with disabilities</center><br /><center><button class="btn action-button" type="button" id="ExpandDisabledParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_up" class="triangle_down"></div></button></center>'), sep=''), icon = icon("wheelchair"),color = "purple")})
  
  })
  
  observeEvent(input$ExpandParking, {
    
    output$parking_stats <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(parking_occupation_reactive(), format="d", big.mark=','),'%'),'</center>', sep='')) , HTML(paste('<center><b>Parking Occupation</b></center><br />Find Parking Time: ', parking_time_reactive(), ' mins<br /><br />Parking Cost: $ ', parking_cost_reactive(), '<br /><br />Time restriction non-availability: ', time_restriction_ratio_reactive(),'%<br /><br />Parking fine probability*: ', round(mean(parking_statistics_df_reactive()$fine_prob, na.rm=TRUE)), ' %<br /><br /><br /><font size="-2">*Parking fine probability has been calculated as the proportion of vehicles that committed a parking infraction over the total vehicles that parked in that area for the selected time and day of the week, according to Historical data.</font><br /><br /><center><button class="btn action-button" type="button" id="ContractParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_up" class="arrow-up"></div></button></center>', sep='')), icon = icon("parking"),color = "purple")})
    
  })
  
  observeEvent(input$ContractParking, {
    
    output$parking_stats <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(parking_occupation_reactive(), format="d", big.mark=','),'%'),'</center>', sep='')) , HTML(paste('<center><b>Parking Occupation</b></center><br /><center><button class="btn action-button" type="button" id="ExpandParking" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_parking_down" class="triangle_down"></div></button></center>', sep='')), icon = icon("parking"),color = "purple")})
    
  })
  
  
  observeEvent(input$ContractTimePublic, {
    
    output$time_public <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Public</b></center><br /><center><button class="btn action-button" type="button" id="ExpandTimePublic" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_public" class="triangle_down"></div></button></center>'), sep=''), icon = icon("clock"),color = "green")})
    
  })
  
  observeEvent(input$ExpandTimePublic, {
    
    output$time_public <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Public</b></center><br />',public_steps_long(),'<br /><center><button class="btn action-button" type="button" id="ContractTimePublic" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_time_public" class="arrow-up"></div></button></center>'), sep=''), icon = icon("clock"),color = "green")})
    
  })
  
  observeEvent(input$ExpandTimePrivate, {
    
    output$time_private <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_private_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Private Vehicle</b></center><br />',  time_steps_private_reactive(), '<br /><br /><center><button class="btn action-button" type="button" id="ContractTimePrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private_up" class="arrow-up"></div></button></center>'), sep=''), icon = icon("clock"), color = "orange")})
    
  })
  
  observeEvent(input$ContractTimePrivate, {
    
    output$time_private <- renderValueBox({valueBox(HTML(paste('<center>',paste(formatC(total_time_private_reactive(), format="d", big.mark=','),'mins'),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Time Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandTimePrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>'), sep=''), icon = icon("clock"), color = "orange")})
    
  })
  
  observeEvent(input$ExpandCostPrivateTolls, {
  output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<b><center>+ ADDITIONAL COST OF TOLLS</center></b><center><b>Total Journey Cost Private Vehicle</b></center><br />Driving Cost*: $', driving_cost_reactive(),'<br />Parking Cost: $', parking_cost_reactive()), '<br />Tolls: Yes<br /><br /><font size="-2">*Driving cost is estimated by multiplying driven distance by 2020 average fuel consumption per km for vehicles in Australia by average price of petrol Litre in Melbourne.</font><br /><br /><center><button class="btn action-button" type="button" id="ContractCostPrivateTolls" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private_up" class="arrow-up"></div></button></center>', sep=''), icon = icon("dollar-sign"), color = "orange")})})
  
  observeEvent(input$ContractCostPrivateTolls, {
  output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<b><center>+ ADDITIONAL COST OF TOLLS</b></center><center><b>Total Journey Cost Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandCostPrivateTolls" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>', sep='')), icon = icon("dollar-sign"), color = "orange")})})
  
  observeEvent(input$ExpandCostPrivate, {
    output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Cost Private Vehicle</b></center><br />Driving Cost*: $', driving_cost_reactive(),'<br />Parking Cost: $', parking_cost_reactive()), '<br />Tolls: ', has_tolls_reactive(), '<br /><br /><font size="-2">*Driving cost is estimated by multiplying driven distance by 2020 average fuel consumption per km for vehicles in Australia by average price of petrol Litre in Melbourne.</font><br /><br /><center><button class="btn action-button" type="button" id="ContractCostPrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private_up" class="arrow-up"></div></button></center>', sep=''), icon = icon("dollar-sign"), color = "orange")})})
  
  observeEvent(input$ContractCostPrivate, {
    output$cost_private <- renderValueBox({valueBox(HTML(paste('<center>',paste('$', total_private_cost_reactive()),'</center>', sep='')) , HTML(paste('<center><b>Total Journey Cost Private Vehicle</b></center><br /><center><button class="btn action-button" type="button" id="ExpandCostPrivate" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><div id="arrow_cost_private" class="triangle_down"></div></button></center>', sep='')), icon = icon("dollar-sign"), color = "orange")})})
  
observeEvent(input$buttonSeeLess, {
    output$time_public <- renderValueBox({valueBox(paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'mins') , HTML(paste('<b>Total Journey Time Public</b><br /><br />', public_steps_short(), '<br /><button class="btn action-button" type="button" id="buttonSeeMore" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><b>See more</b></button>'), sep=''), icon = icon("clock"),color = "green")})
  })
  
  observeEvent(input$buttonSeeMore, {
    output$time_public <- renderValueBox({valueBox(paste(formatC(total_time_public_reactive(), format="d", big.mark=','),'mins') , HTML(paste('<b>Total Journey Time Public</b><br /><br />', public_steps_long(), '<br /><button class="btn action-button" type="button" id="buttonSeeLess" style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"><b>See less</b></button>'), sep=''), icon = icon("clock"),color = "green")})
  })
  
  observeEvent(input$max_stay_map,{
    updateSliderInput(session, inputId = "length_of_stay", label = "Length of stay (minutes)", min = 30, max=240, value =input$max_stay_map, step=30)
      if(input$restrictions_checkbox==TRUE){
        updatePrettyCheckbox(session, 'restrictions_checkbox', 'Show only parkings within time restriction', FALSE)
      }
      if(leaving_reactive()=='Now'){
        data <- parking_data_reactive_complete()
        data$restricted <- data$maximum_stay<as.numeric(input$max_stay_map)
        # print(data)
        data$color <- mapply(define_color_parking, data$status, data$restricted)
        parking_data_reactive_complete(data)
        parking_data_reactive_incomplete(data[data$color!='#ECC904', ])
        google_map_update(map_id = "myMap") %>% clear_circles() %>%
          add_circles(data=data, lat='lat', lon='lon', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over', mouse_over = 'hover_over',
                      update_map_view=FALSE)
      }
      else{
        parking_statistics_df_map <- parking_data_reactive_complete()
        f <- colorRamp(c("#FACAC4", "#FA4A39"))
        parking_statistics_df_map$color <- rgb(f(svals_reactive())/255)
        if(day_reactive()!='Sunday' & ((as.POSIXct(time_reactive(), format='%H:%M')>as.POSIXct('7:30', format='%H:%M')&as.POSIXct(time_reactive(), format ='%H:%M')<as.POSIXct('18:30', format='%H:%M')))){
          parking_statistics_df_map[parking_statistics_df_map$maximum_stay<as.numeric(input$max_stay_map),'color']<-'#ECC904'}
        parking_data_reactive_complete(parking_statistics_df_map)
        parking_data_reactive_incomplete(parking_statistics_df_map[parking_statistics_df_map$color!='#ECC904',])
        google_map_update(map_id = "myMap") %>% clear_circles() %>%
          add_circles(data=parking_statistics_df_map, lat='mean_lat', lon='mean_long', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over= 'hover_information',
                      update_map_view=FALSE)
      }
    
  })
  observeEvent(input$routes_zoom_out, {
    google_map_update(map_id = "myMap") %>% 
      clear_polylines()  %>% 
      add_polylines(data = df_route_private_reactive(),
                    polyline = "route",
                    stroke_colour = '#F95E1B',
                    stroke_weight = 7,
                    stroke_opacity = 0.7,
                    info_window = "polyline_hover",
                    mouse_over = '<b>Private vehicle journey</b> <br />Click to see detail',
                    load_interval = 100)%>% 
      add_polylines(data = df_route_public_reactive(),
                    polyline = "route",
                    stroke_colour = '#54C785',
                    stroke_weight = 7,
                    stroke_opacity = 0.7,
                    info_window = "polyline_hover",
                    mouse_over = '<b>Public transport journey</b> <br />Click to see detail',
                    load_interval = 100)
  })
  
  observeEvent(input$parking_zoom_in,{
    if(nrow(parking_data_reactive_incomplete())==0){
      updatePrettyCheckbox(session, 'restrictions_checkbox', 'Show only parkings within time restriction', FALSE)
      parking_data_reactive_incomplete(parking_data_reactive_complete())
    }
    if(leaving_reactive() !='Now'){
      if(input$restrictions_checkbox == FALSE){
    google_map_update(map_id = "myMap") %>% 
      clear_circles()  %>% add_circles(data=parking_data_reactive_complete(), lat='mean_lat', lon='mean_long', 
                                                 fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over = 'hover_information', focus_layer = TRUE)}
      else{
        google_map_update(map_id = "myMap") %>% 
          clear_circles()  %>% add_circles(data=parking_data_reactive_incomplete(), lat='mean_lat', lon='mean_long', 
                                           fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over = 'hover_information', focus_layer = TRUE)
      }
        }
    else{
      if(input$restrictions_checkbox == FALSE){
      google_map_update(map_id = "myMap") %>% 
        clear_circles()  %>% add_circles(data=parking_data_reactive_complete(), lat='lat', lon='lon', 
                                         fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over', mouse_over = 'hover_over', focus_layer = TRUE)}
      else{
        google_map_update(map_id = "myMap") %>% 
          clear_circles()  %>% add_circles(data=parking_data_reactive_incomplete(), lat='lat', lon='lon', 
                                           fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over', mouse_over = 'hover_over', focus_layer = TRUE)
      }
      
    }
    
  })
  
  # checkbox to make the time restricted parking spots appear/disappear
  observeEvent(input$restrictions_checkbox,{
    if(input$restrictions_checkbox==TRUE){
      if(nrow(parking_data_reactive_incomplete())>0){
        if(leaving_reactive()=='Now'){
          google_map_update(map_id = "myMap") %>% clear_circles() %>%
            add_circles(data=parking_data_reactive_incomplete(), lat='lat', lon='lon', 
                        fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over', mouse_over= 'hover_over', update_map_view=FALSE)}
        else{
          if(day_reactive()!='Sunday' & ((as.POSIXct(time_reactive(), format='%H:%M')>as.POSIXct('7:30', format='%H:%M')&as.POSIXct(time_reactive(), format ='%H:%M')<as.POSIXct('18:30', format='%H:%M')))){
          google_map_update(map_id = "myMap") %>% clear_circles() %>%
            add_circles(data=parking_data_reactive_incomplete(), lat='mean_lat', lon='mean_long', 
                        fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over= 'hover_information', update_map_view=FALSE)
        }}
      }
      
      else{
        google_map_update(map_id = "myMap") %>% clear_circles()
      }
      
      }
    
    else{
      if(leaving_reactive()=='Now'){
        google_map_update(map_id = "myMap") %>% clear_circles() %>%
          add_circles(data=parking_data_reactive_complete(), lat='lat', lon='lon', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_over', mouse_over = 'hover_over',
                      update_map_view=FALSE)
        }
      else{
        if(day_reactive()!='Sunday' & ((as.POSIXct(time_reactive(), format='%H:%M')>as.POSIXct('7:30', format='%H:%M')&as.POSIXct(time_reactive(), format ='%H:%M')<as.POSIXct('18:30', format='%H:%M')))){
        google_map_update(map_id = "myMap") %>% clear_circles() %>%
          add_circles(data=parking_data_reactive_complete(), lat='mean_lat', lon='mean_long', 
                      fill_colour='color', radius = 20, stroke_colour= 'color', info_window = 'hover_information', mouse_over= 'hover_information',
                      update_map_view=FALSE)
      }}
    }
    
  })
  
  }
