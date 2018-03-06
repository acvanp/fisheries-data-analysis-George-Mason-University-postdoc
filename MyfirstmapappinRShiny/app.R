#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("C:\\Users\\avanplan\\Dropbox\\Personal projects\\R scrips")
redsnapper = read.csv("redsnapper.csv")

redsnapper = data.frame(redsnapper$lat, redsnapper$lon, redsnapper$cpue, redsnapper$year)
colnames(redsnapper) = c("lat",  "long", "cpue", "year")

redsnapper = redsnapper[which(!is.infinite(redsnapper$cpue)),]
redsnapper = redsnapper[order(redsnapper$cpue, decreasing = TRUE),]
redsnapper = redsnapper[which(redsnapper$cpue < 1),]

redsnapper = na.omit(redsnapper)



library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "year", sep = "", format = "####", min(redsnapper$year), max(redsnapper$year),
                            value = range(redsnapper$year), step = 1
                ),
                selectInput("colors", "Color Scheme", 
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )

)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    redsnapper[redsnapper$year >= input$range[1] & redsnapper$year <= input$range[2]
               ,]
  })

  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, redsnapper$cpue)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(redsnapper) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~(100*cpue/mean(cpue))^2/400, weight = 2, color = "#777777",
                 fillColor = ~pal(cpue), fillOpacity = 0.7, popup = ~paste(cpue)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = redsnapper)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~cpue
      )
    }
  })

  
  }


shinyApp(ui, server)
