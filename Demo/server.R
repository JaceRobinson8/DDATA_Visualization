library(shiny)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
#     x    <- faithful[, 2]  # Old Faithful Geyser data
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    m <- leaflet()
    m <- addTiles(m)
    m <- addMarkers(m, lng=c(-84.0636,-84.09,-84.07,-84.06), lat=c(39.7815,39.79,39.77,39.8),popup=c("Cogan is on Fire OMG!!!!","Jace rules","tag3","tag4"))
    m
  })
})


