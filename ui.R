library(shiny)
library(shinydashboard)
library(googleway)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(lubridate)
library(bsplus)
library(shinyjs)
library(shinyBS)

options(shiny.sanitize.errors = FALSE)

key <- 'AIzaSyD36r0dBXmooQ2cSEdI88-U7VOFMYOfLlU'

# Script showing the User Interface part of the Shiny app

# to display days an hour in input

day_of_week <- wday(with_tz(Sys.time(), 'Australia/Melbourne'))
days_of_week <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
day_of_week <- days_of_week[day_of_week]

hours <- c()
for(i in 0:23){
  hour <- paste(i, ':00', sep='')
  hours<-c(hours, hour)
}

hour_now <- paste(as.character(hour(with_tz(Sys.time(), 'Australia/Melbourne'))),":00", sep='')

# Initializing dashboard panels and styling them with CSS

ui <- dashboardPage(
  dashboardHeader(      title = "Compare public transport and private vehicles journeys",
                        titleWidth = 450),
  dashboardSidebar(      tags$head(tags$style(HTML("
                                .skin-blue .main-header .logo {
                                background-color: #7E8BFA;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #7E8BFA;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #7E8BFA;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #7E8BFA;
                                }
                                .content-wrapper, .right-side {
                                background-color: #FAF4F4;
                                }
                                .wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}
                                .small-box .icon-large {top: 5px;
                                }
                                .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #E56B76}
                                .tooltip .tooltip-inner {background-color: #E56B76 ; }
                                .box-body {
                                padding-bottom: 0px;}
                                
                                .notbold{
                                font-weight:normal
                                }
                                .triangle_down {
                                width: 0;
                                height: 0;
                                border-left: 15px solid transparent;
                                border-right: 15px solid transparent;
                                border-top: 15px solid white;
                                font-size: 0;
                                line-height: 0;
                                float: left;
                                }
                                .arrow-up {
                                  width: 0; 
                                  height: 0; 
                                  border-left: 15px solid transparent;
                                  border-right: 15px solid transparent;
                              
                                  border-bottom: 15px solid white;
                                }
                                #arrow_cost_private {
                                margin: 0 auto;
                                text-align: center;
                                }
                                                   
                                "))),width=325,
                                
                      br(),
                      textInput(inputId = "origin", label = "Origin", value = '', placeholder='Input a location within 20 km of the CBD...'),
                      textInput(inputId = "destination", label = "Destination", value = '', placeholder = 'Input a location in the CBD...'),
                      sliderInput(inputId = "length_of_stay", label = "Length of stay (minutes)", min = 30, max=240, value =90, step=30),
                      bsTooltip("length_of_stay", HTML("How long are you planning on staying at the CBD? <br />This information is relevant to calculate the estimated time to find a Parking Space as well as its cost."), placement = "top", trigger = "hover",options = NULL),
                      prettyRadioButtons(inputId="leaving", label="Leaving", choices=c("Now","Selected Time & Day"), selected ="Selected Time & Day"),
                      bsTooltip("leaving", HTML('Select one of the 2 options and click "Compare Journeys". Both options will display a map with the Private Vehicle and Public Transport optimal routes.<br />If "Now" is selected, the map will also show live Parking availability near your destination while if "Selected Time & Day" is selected, the map will show historical Parking availability for the selected time and day.'), placement = "top", trigger = "hover",options = NULL),
                      conditionalPanel(condition = "input.leaving == 'Selected Time & Day'",
                      selectInput(inputId = "day", label = "Day", choices = c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'), selected = day_of_week),
                      selectInput(inputId = "hour", label = "Hour", choices = hours, selected= hour_now)),
                      br(),
                      actionButton("compare_journeys", "Compare Journeys", style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76"),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br()),
  
  # Custom JavaScript to allow places autocomplete
  
  dashboardBody(HTML(paste0("
                <script>
                function initAutocomplete() {
                                var defaultBounds = new google.maps.LatLngBounds(
                                new google.maps.LatLng(-38.180008 , 144.365954),
                                new google.maps.LatLng(-37.575154, 145.662341)
                );
                var restriction_options ={
                bounds: defaultBounds,
                strictBounds: true
                };
                var autocomplete = new google.maps.places.Autocomplete(document.getElementById('origin'),restriction_options);
                autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                autocomplete.addListener('place_changed', function() {
                var place = autocomplete.getPlace();

                var addressorigin = place.formatted_address;
                var coordsOrigin = place.geometry.location;
                Shiny.onInputChange('jsorigin', addressorigin);
                Shiny.onInputChange('jsorigincoords', coordsOrigin);
                
                });
                
                var autocomplete2 = new google.maps.places.Autocomplete(document.getElementById('destination'),restriction_options);
                autocomplete2.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                autocomplete2.addListener('place_changed', function() {
                var place2 = autocomplete2.getPlace();

                var addressdestination = place2.formatted_address;
                var coordsDest = place2.geometry.location;
                Shiny.onInputChange('jsdestcoords', coordsDest);
                Shiny.onInputChange('jsdestination', addressdestination);});}
                
                </script> 
                <script src='https://maps.googleapis.com/maps/api/js?key=", key,"&libraries=places&callback=initAutocomplete' async defer></script>")),
                
    # all the output elements
    useShinyjs(),
    useShinyalert(),
    uiOutput("map_title"),
    uiOutput("map_legend"),
    # google_mapOutput("myMap"),
    # google_mapOutput("myMapo"),
    uiOutput("maps"),
    br(),
    uiOutput("map_sliders"),
    uiOutput("show_non_restricted"),
    uiOutput("sep_line"),
    uiOutput("expand_contract_all"),
    br(),
    uiOutput("journey_icons"),
    br(),
    uiOutput("show_time_statistics"),
    br(),
    uiOutput("show_cost_statistics"),
    br(),
    br(),
    uiOutput("show_public_benefits"),
    br(),
    br(),
    br(),
    br(),
    br()
  )
)
