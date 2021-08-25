library(shiny)
library(shinydashboard)
library(googleway)
library(shinyTime)
library(shinythemes)
library(shinyWidgets)

key <- 'AIzaSyD36r0dBXmooQ2cSEdI88-U7VOFMYOfLlU'

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
                                                  "))),width=300,    
                      textInput(inputId = "origin", label = "Origin", value = ''),
                       textInput(inputId = "destination", label = "Destination", value = 'Department of Agriculture, La Trobe St, Melbourne VIC 3000'),
                       textInput(inputId = "length_of_stay", label = "Length of stay (minutes)", value = '45'),
                       textInput(inputId = "max_walk", label = "Max walk (mts)", '800'),
                       # fluidRow(column(3,'Leaving'), column(2, checkboxInput(inputId = "live", label = "Now")), column(1,''), column(2,checkboxInput(inputId = "selected_hd", label = "Select Time & Day"))),
                      prettyRadioButtons(inputId="leaving", label="Leaving", choices=c("Now","Selected Time & Day")),
                       conditionalPanel(condition = "input.leaving == 'Selected Time & Day'",
                       textInput(inputId = "day", label = "Day", value = 'Friday'),
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
                
    google_mapOutput("myMap"),
    uiOutput("show_non_restricted"),
    br(),
    br(),
    uiOutput("show_private_statistics")
  )
)