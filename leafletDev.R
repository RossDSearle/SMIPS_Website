library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  
  tags$style(' .leaflet-bar button,
.leaflet-bar button:hover {
  background-color: #fff;
  border: none;
  border-bottom: 1px solid #ccc;
  width: 120px;
  height: 30px;
  line-height: 30px;
  display: block;
  text-align: left;
  text-decoration: none;
  color: black;
}

.leaflet-bar button {
  background-position: 50% 50%;
  background-repeat: no-repeat;
  overflow: hidden;
  display: block;
}

.leaflet-bar button:hover {
  background-color: #f4f4f4;
}

.leaflet-bar button:first-of-type {
  border-top-left-radius: 10px;
  border-top-right-radius: 10px;
}

.leaflet-bar button:last-of-type {
  border-bottom-left-radius: 10px;
  border-bottom-right-radius: 10px;
  border-bottom: none;
}

.leaflet-bar.disabled,
.leaflet-bar button.disabled {
  cursor: default;
  pointer-events: none;
  opacity: .4;
}

.easy-button-button .button-state{
  display: block;
  width: 100%;
  height: 100%;
  position: relative;
}


.leaflet-touch .leaflet-bar button {
  width: 120px;
  height: 30px;
  line-height: 30px;
  
  .leaflet-container.crosshair-cursor-enabled {
    cursor:crosshair;
  }

}'
  ),
  leafletOutput("moistureMap")
)

server <- function(input, output, session) {
  
  observeEvent(input$SMIPSDrillbtn, {
    print(input$SMIPSDrillbtn)
    
  })
  
  output$moistureMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addMarkers(data=quakes,
                 clusterOptions = markerClusterOptions(),
                 clusterId = "quakesCluster") %>%
      
      addEasyButtonBar(
      easyButton(
        states = list(
          easyButtonState(
            stateName="sOff",
            #icon="fa-check",
            #icon=htmltools::span(class = "star", htmltools::HTML('<img src="Buttons/drill.png"></font>')),
            icon=htmltools::span(class = "star", htmltools::HTML('<font color="gray">Get&nbspTimeseries&nbsp<b>Off</b></font>')),
            title="Click to turn on drilling of a pixel on the map to return a timeseries of soil moisture values",
            onClick = JS("
              function(btn, map) {
                btn.state('sOn');
                Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOn');
               $('.leaflet-container').css('cursor','crosshair');
              }")
          ),
          easyButtonState(
            stateName="sOn",
            icon=htmltools::span(class = "star", htmltools::HTML('<font color="green">Get&nbspTimeseries&nbsp<b>On</b></font>')),
            title="Click to turn off drilling of a pixel on the map to return a timeseries of soil moisture values",
            onClick = JS("
              function(btn, map) {
                btn.state('sOff');
                Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOff');
                $('.leaflet-container').css('cursor','');
              }")
          )
        )
      )

      
      )
  })
}

shinyApp(ui, server)