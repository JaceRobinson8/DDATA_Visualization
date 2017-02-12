# Jace Robinson
# Senior Project Demo
# Spring 2016

### See comment 'server' function on how to switch between local test data and data
# pulled from server. You should just have to comment/uncomment the test data or the
# getXML() line.

# You can uncomment these to install necessary packages
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("XML")
# install.packages("RColorBrewer")

library(shiny)
library(leaflet)
library(XML)
library(RColorBrewer)

ui <- fluidPage(
  leafletOutput("mymap", height=800),
  p(),
  actionButton("recalc", "Refresh")
)

server <- function(input, output, session) {
  
  incidentIcons <- iconList(
    incident = makeIcon(iconUrl = "https://cdn1.iconfinder.com/data/icons/Map-Markers-Icons-Demo-PNG/256/Map-Marker-Marker-Outside-Azure.png",iconWidth = 40,iconHeight = 40, iconAnchorX = 0, iconAnchorY = 250),
    incidentReport = makeIcon(iconUrl = "https://cdn1.iconfinder.com/data/icons/Map-Markers-Icons-Demo-PNG/256/Map-Marker-Marker-Outside-Chartreuse.png",iconWidth = 40,iconHeight = 40, iconAnchorX = 0, iconAnchorY = 250)
  )
  getXML <- function() {
    dataIR <- xmlParse("http://localhost:8080/ddata_rest/GetAllIncidentReports")
    xml_dataIR <- xmlToList(dataIR)
    dataI <- xmlParse("http://localhost:8080/ddata_rest/GetAllIncidents")
    xml_dataI <- xmlToList(dataI)
    
    lat <- c()
    lon <- c()
    tag <- c()
    type <- c()
    
    for (i in 1:length(xml_dataIR)) {
      lat <- append(lat,as.double(xml_dataIR[[i]][['Point']][['Latitude']]))
      lon <- append(lon,as.double(xml_dataIR[[i]][['Point']][['Longitude']]))
      tag <- append(tag,value = xml_dataIR[[i]][['IncidentReportType']])
      type <- append(type, value = "incidentReport")
    }
    
    for (i in 1:length(xml_dataI)) {
      lat <- append(lat,as.double(xml_dataI[[i]][['Point']][['Latitude']]))
      lon <- append(lon,as.double(xml_dataI[[i]][['Point']][['Longitude']]))
      
      tempTags <- c()
      for (j in 1:length(xml_dataI[[i]][['IncidentTypes']]))
      {
        tempTags <- append(tempTags, value = xml_dataI[[i]][['IncidentTypes']][[j]])
      }
      tag <- append(tag,value = toString(tempTags))
      type <- append(type, value = "incident")
    }
    
    return (cbind(lon,lat,tag,type))
  }
  
  points <- eventReactive(input$recalc, {
    ###### To run local data instead of from server, uncomment the line directly below and comment out getXML()
    # cbind(c(-84.0636,-84.09,-84.07,-84.06), c(39.7815,39.79,39.77,39.8), c("Cogan is on Fire OMG!!!!","Jace rules","tag3","tag4"),c("incident","incident","incidentReport","incidentReport"))
    getXML()
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lat = 39.7815, lng = -84.0636, zoom = 13) %>%
      addTiles(
      ) %>%
      addMarkers(lng = points()[,1], lat= points()[,2], popup = points()[,3], icon = incidentIcons[points()[,4]])
  })
}

shinyApp(ui, server)

############## Junk below here, do not want to delete old thoughts

# ui <- bootstrapPage(
#   
#     leafletOutput("mymap", width = "100%", height = "100%"),
#     p(),
#     actionButton("recalc", "Refresh"),
#   tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#   leafletOutput("map", width = "100%", height = "100%"),
#   absolutePanel(top = 10, right = 10,
#                 sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
#                             value = range(quakes$mag), step = 0.1
#                 ),
#                 selectInput("colors", "Color Scheme",
#                             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
#                 ),
#                 checkboxInput("legend", "Show legend", TRUE)
#   )
# )

# library(maps)
# library(ggmap)
# library(ggplot2)
# 
# map <- get_map(location=c(lon = -84.0636, lat = 39.7815), zoom = 14)
# 
# coords <- data.frame(c(-84.0636,-84.09,-84.07,-84.06), c(39.7815,39.79,39.77,39.8),c("tag1","tag2","tag3","tag4"))
# colnames(coords) <- c("lon","lat","tag")
# 
# ggmap(map, extend = "normal") + geom_point(data = coords, aes(x=lon,y=lat,color=tag), size = 8)


# library(XML)
# data <- xmlParse("http://www.w3schools.com/xml/cd_catalog.xml")
# xml_data <- xmlToList(data)
# 
# lat <- c()
# lon <- c()
# tag <- c()
# 
# for (i in 1:length(xml_data)) {
#   # lat <- append(lon,value = xml_data[[i]][['TITLE']])
#   # lon <- append(lon,value = xml_data[[i]][['TITLE']])
#   tag <- append(lon,value = xml_data[[i]][['TITLE']])
# }
# library(leaflet)
#  
# m <- leaflet()
# m <- addTiles(m)
# m <- addMarkers(m, lng=c(-84.0636,-84.09,-84.07,-84.06), lat=c(39.7815,39.79,39.77,39.8),popup=c("Cogan is on Fire OMG!!!!","Jace rules","tag3","tag4"))
# m
# 
# 
# ########
# library(shiny)
# setwd("Senior Project/")
# runApp("Demo")

# 
# data <- xmlParse("http://localhost:8080/ddata_rest/GetAllIncidentReports")
# xml_data <- xmlToList(data)
# as.double(xml_data[[2]][['Point']][['Latitude']])


