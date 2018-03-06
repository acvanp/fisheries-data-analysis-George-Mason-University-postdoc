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




library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("obs", min(redsnapper$year), 
                            max(redsnapper$year),
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
    redsnapper[redsnapper$year >= input$range[1] & redsnapper$year <= input$range[2],]
  })
  
  
  
  points <- eventReactive(input$recalc, {
    cbind(redsnapper$long, redsnapper$lat)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
  

}

shinyApp(ui, server)
















library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(redsnapper$long, redsnapper$lat)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)











library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Depth", min(quakes$depth), max(quakes$depth),
                            value = range(quakes$depth), step = 50
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
    quakes[quakes$depth >= input$range[1] & quakes$depth <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$depth)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
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
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(depth), fillOpacity = 0.7, popup = ~paste(depth)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~depth
      )
    }
  })
}

shinyApp(ui, server)


# https://bhaskarvk.github.io/leaflet-talk-rstudioconf-2017/RstudioConf2017.html#13

leaflet(data) %>%
  addMarkers(
    lat = ~latitude, lon = ~longitude,
    options = markerOptions(),
    label=~label, labelOptions = labelOptions(),
    popup=~popup, popupOptions = popupOptions(),
    clusterOptions = clusterOptions(),
    group = 'Group-A')
# Similarly 
addCircleMarkers()  # Fixed scale Circles
addAwesomeMarkers() # More choices for icons
addLabelOnlyMarkers() # No icon

quakes.df <- quakes %>% dplyr::mutate(
  mag.level = cut(mag,c(3.5,4.5,5.5,6.5),
                  labels = c('> 3.5 & <=4.5', '>4.5 & <=5.5', '>5.5 & <=6.5'))) %>%
  split(.$mag.level)
l <- leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap)
names(quakes.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addMarkers(data=quakes.df[[df]], lng=~long, lat=~lat,
                 label=~as.character(mag), popup=~as.character(mag),
                 group = df,
                 clusterOptions = markerClusterOptions())
  })
l <- l %>%
  addLayersControl(
    overlayGroups = names(quakes.df),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(tiles = providers$Esri.OceanBasemap, width = 120, height=80)

# spdf is a sp::SpatialPolygonsDataFrame
qpal <- colorQuantile(rev(viridis::viridis(5)),
                      spdf$POPDENSITY, n=5)
l <- leaflet(spdf, options =
               leafletOptions(attributionControl = FALSE, minzoom=1.5)) %>%
  addPolygons(
    label=~stringr::str_c(
      NAME, ' ',
      formatC(POPDENSITY, big.mark = ',', format='d')),
    labelOptions= labelOptions(direction = 'auto'),
    weight=1,color='#333333', opacity=1,
    fillColor = ~qpal(POPDENSITY), fillOpacity = 1,
    highlightOptions = highlightOptions(
      color='#000000', weight = 2,
      bringToFront = TRUE, sendToBack = TRUE)
  ) %>%
  addLegend(
    "topright", pal = qpal, values = ~POPDENSITY,
    title = htmltools::HTML("Population Density<br/>(2005)"),
    opacity = 1 )


crs.molvidde <- leafletCRS(
  crsClass="L.Proj.CRS", code='ESRI:53009',
  proj4def= '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs',
  resolutions = c(65536, 32768, 16384, 8192, 4096, 2048))
l <- leaflet(
  spdf,
  options = leafletOptions(
    maxZoom = 5, crs= crs.molvidde, attributionControl = FALSE)) %>%
  addGraticule(style= list(color= '#999', weight= 0.5, opacity= 1)) %>%
  addGraticule(sphere = TRUE,
               style= list(color= '#777', weight= 1, opacity= 0.25)) %>%
  addPolygons(
    label=~stringr::str_c(
      NAME, ' ', formatC(POPDENSITY, big.mark = ',', format='d')),
    labelOptions= labelOptions(direction = 'auto'),
    weight=1,color='#ffffff', opacity=1,
    fillColor = ~qpal(POPDENSITY), fillOpacity = 1,
    highlightOptions = highlightOptions(
      color='#000000', weight = 2,
      bringToFront = TRUE, sendToBack = TRUE))