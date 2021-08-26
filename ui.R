library(shiny)
library(shinydashboard)
library(googleway)
library(shinyTime)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)

key <- ''

ui <- dashboardPage(
  dashboardHeader(      title = "Compare public transport and car journeys",
                        titleWidth = 450),
  dashboardSidebar(      tags$head(tags$style(HTML("
                                .skin-blue .main-header .logo {
                                background-color: #6553F7;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #7E8BFA;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #6553F7;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #7E8BFA;
                                }
                                .content-wrapper, .right-side {
                                background-color: #FAF4F4;
                                }
                                .small-box .icon-large {top: 5px;
                                }
                                .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #E56B76}
                                                  "))),width=300,
                                
                      br(),
                      img(src = 'jamsnot_logo.png'),
                      br(),
                      br(),
                      textInput(inputId = "origin", label = "Origin", value = ''),
                       textInput(inputId = "destination", label = "Destination", value = 'Department of Agriculture, La Trobe St, Melbourne VIC 3000'),
                       sliderInput(inputId = "length_of_stay", label = "Length of stay (minutes)", min = 30, max=240, value =30, step=30),
                      prettyRadioButtons(inputId="leaving", label="Leaving", choices=c("Now","Selected Time & Day")),
                       conditionalPanel(condition = "input.leaving == 'Selected Time & Day'",
                       selectInput(inputId = "day", label = "Day", choices = c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
                       textInput(inputId = "hour", label = "Hour", value = '19:00')),
                        br(),
                       actionButton("compare_journeys", "Compare Journeys", style=" border-radius: 8px; color: white; background-color: #E56B76; border: 2px solid #E56B76")),
  dashboardBody(HTML(paste0(" <script> 
                function initAutocomplete() {

                var autocomplete = new google.maps.places.Autocomplete(document.getElementById('origin'),{types: ['geocode']});
                autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                autocomplete.addListener('place_changed', function() {
                var place = autocomplete.getPlace();
                if (!place.geometry) {
                return;
                }

                var addressPretty = place.formatted_address;
                var address = '';
                if (place.address_components) {
                address = [
                (place.address_components[0] && place.address_components[0].short_name || ''),
                (place.address_components[1] && place.address_components[1].short_name || ''),
                (place.address_components[2] && place.address_components[2].short_name || ''),
                (place.address_components[3] && place.address_components[3].short_name || ''),
                (place.address_components[4] && place.address_components[4].short_name || ''),
                (place.address_components[5] && place.address_components[5].short_name || ''),
                (place.address_components[6] && place.address_components[6].short_name || ''),
                (place.address_components[7] && place.address_components[7].short_name || '')
                ].join(' ');
                }
                var address_number =''
                address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                var coords = place.geometry.location;
                //console.log(address);
                Shiny.onInputChange('jsValue', address);
                Shiny.onInputChange('jsValueAddressNumber', address_number);
                Shiny.onInputChange('jsValuePretty', addressPretty);
                Shiny.onInputChange('jsValueCoords', coords);});}
                </script> 
                <script src='https://maps.googleapis.com/maps/api/js?key=", key,"&libraries=places&callback=initAutocomplete' async defer></script>")),
    
                
                HTML(paste0(" <script> 
                function initAutocomplete2() {

                var autocomplete2 = new google.maps.places.Autocomplete(document.getElementById('destination'),{types: ['geocode']});
                autocomplete2.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                autocomplete2.addListener('place_changed', function() {
                var place = autocomplete2.getPlace();
                if (!place.geometry) {
                return;
                }

                var addressPretty = place.formatted_address;
                var address = '';
                if (place.address_components) {
                address = [
                (place.address_components[0] && place.address_components[0].short_name || ''),
                (place.address_components[1] && place.address_components[1].short_name || ''),
                (place.address_components[2] && place.address_components[2].short_name || ''),
                (place.address_components[3] && place.address_components[3].short_name || ''),
                (place.address_components[4] && place.address_components[4].short_name || ''),
                (place.address_components[5] && place.address_components[5].short_name || ''),
                (place.address_components[6] && place.address_components[6].short_name || ''),
                (place.address_components[7] && place.address_components[7].short_name || '')
                ].join(' ');
                }
                var address_number =''
                address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                var coords = place.geometry.location;
                //console.log(address);
                Shiny.onInputChange('jsValue', address);
                Shiny.onInputChange('jsValueAddressNumber', address_number);
                Shiny.onInputChange('jsValuePretty', addressPretty);
                Shiny.onInputChange('jsValueCoords', coords);});}
                </script> 
                <script src='https://maps.googleapis.com/maps/api/js?key=", key,"&libraries=places&callback=initAutocomplete2' async defer></script>")),
    
    useShinyalert(),           
    strong(htmlOutput("map_title")),           
    google_mapOutput("myMap"),
    uiOutput("show_non_restricted"),
    br(),
    br(),
    uiOutput("show_time_statistics"),
    br(),
    uiOutput("show_cost_statistics")
  )
)
