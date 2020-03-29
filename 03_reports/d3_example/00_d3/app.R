# This Example Comes from:
# https://deck.gl/showcases/gallery/viewport-transition

library(tidyverse)
library(shiny)
library(r2d3)

ui <- fluidPage(
    tags$head(HTML('<script src="https://unpkg.com/deck.gl@^7.0.0/dist.min.js"></script>')),
    tags$head(HTML('<script src="https://d3js.org/d3.v5.min.js"></script>')),
    tags$head(HTML('<script src="https://api.tiles.mapbox.com/mapbox-gl-js/v0.50.0/mapbox-gl.js"></script>')),
    inputPanel(
        selectInput(
            "select", 
            label = h3("Select City"),
            choices = list(
                "San Francisco", 
                "New York", 
                "Los Angeles",
                "London",
                "Hyderabad"
                ),
            selected = "San Francisco"
            )
        ),
    d3Output("d3")
)

server <- function(input, output) {
    
    cities <- tibble(
        name = c("San Francisco", "New York", "Los Angeles", "London", "Hyderabad"),
        latitude = c(37.7751, 40.6643, 34.051597, 51.5074, 17.3850),               
        longitude = c(-122.4193, -73.9385, -118.244263, -0.1278, 78.4867)
    )
    
    output$d3 <- renderD3({
        r2d3(
            filter(cities, name == input$select),
            # cities,
            script = here::here("03_reports/d3_example/", "00_d3/zoom.js")
            )
        
    })
}

shinyApp(ui = ui, server = server)