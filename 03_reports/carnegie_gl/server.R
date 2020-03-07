#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$arc_map <- renderDeckgl({

        properties = list(
            pickable = TRUE,
            getStrokeWidth = 2,
            cellSize = 200,
            elevationScale = 4,
            getSourcePosition = get_position("from_lat", "from_lon"),
            getTargetPosition = get_position("to_lat", "to_lon"),
            getTargetColor = get_color_to_rgb_array("ch_color"),
            getSourceColor = get_color_to_rgb_array("from_color"),
            getTooltip = get_property("tooltip")
        )
        
        deckgl(
            latitude = 40.7,
            longitude = -74,
            zoom = 11, 
            pitch = 0
        ) %>% 
            add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9") %>%   
            add_arc_layer(
                data = dat,
                id = 'arc-layer',
                properties = properties
            )
    })
    
    output$scatter_map <- renderDeckgl({
        
        properties <- list(
            getPosition = get_position("from_lat", "from_lon"),
            getRadius = JS("data => Math.sqrt(data.exits)"),
            radiusScale = 1000,
            getColor = get_color_to_rgb_array("from_color"),
            getTooltip = get_property("tooltip")
        )
        
        deckgl(
            latitude = 40.7,
            longitude = -74,
            zoom = 11, 
            pitch = 0
        ) %>% 
            add_scatterplot_layer(
                data = dat, 
                properties = properties
            ) %>% 
            add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9")
        
    })
    
    output$hex_map <- renderDeckgl({

        properties <- list(
            extruded = TRUE,
            radius = 10000,
            elevationScale = 4,
            getPosition = get_position("from_lat", "from_lon"),
            getTooltip = JS("object => `${object.centroid.join(', ')}<br/>Count: ${object.points.length}`"),
            fixedTooltip = TRUE
        )

        deckgl() %>%
            add_hexagon_layer(data = dat, properties = properties) %>%
            add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9")
    })

})
