observe({
    centroids_city <- filter(centroids, sigla_muni == input$city)

    mapdeck_update(map_id = "map") %>%
      mapdeck_view(location = c(centroids_city$lon, centroids_city$lat), zoom = 10,
                   duration = 3000,
                   transition = "fly")
  })

  observeEvent({c(input$time, input$activity, input$city)},{

    print(" -- changing -- ")
    sf <- time_filtered()
    print( unique( sf$sigla_muni ) )
    print( unique( sf$time_threshold ) )
    print( unique( sf$activity )  )

    mapdeck_update(map_id = "map") %>%
      add_polygon(
        data = sf,
        fill_colour = "value",
        fill_opacity = 200,
        layer_id = "acess",
        palette = "inferno",
        update_view = FALSE,
        focus_layer = FALSE,
      )
  })


  ####################


  # Complete App that Works

  
# * mapdeck reactive example ----------------------------------------------
## From:
## https://stackoverflow.com/questions/59702062/keep-map-zoom-inside-same-city-while-changing-attributes-in-shinymapdeck

# library(tidyverse)
# library(shiny)
# library(r2d3)
# library(mapdeck)
# #
key <- 'pk.eyJ1Ijoia3BpdmVydCIsImEiOiJjazc2dWc4YTUwMHp6M2tvNWIyYTQyaXNnIn0.MmXD8-ud_HmuDffvJMotVA'
mapdeck(token = key, style = 'mapbox://styles/mapbox/dark-v9')
# 
# ui <- fluidPage(
#         inputPanel(
#         selectInput(
#             "city", 
#             label = h3("Select City"),
#             choices = list(
#                 "San Francisco", 
#                 "New York", 
#                 "Los Angeles",
#                 "London",
#                 "Hyderabad"
#             ),
#             selected = "San Francisco"
#         )
#     ),
#     mapdeckOutput(outputId = 'myMap')
# )
# 
# server <- function(input, output) {
#     
#     cities <- tibble(
#         name = c("San Francisco", "New York", "Los Angeles", "London", "Hyderabad"),
#         latitude = c(37.7751, 40.6643, 34.051597, 51.5074, 17.3850),               
#         longitude = c(-122.4193, -73.9385, -118.244263, -0.1278, 78.4867)
#     )
#     
#     # reactive for the city
#     city <- reactive({
#         cities %>% filter(name == input$city)
#     })
#     
#     
#     output$myMap <- renderMapdeck({
#         mapdeck(style = mapdeck_style('dark'), token = key)
#     })
#     
#     observe({
#         
#         city <- filter(cities, name == input$city)
#     
#         mapdeck_update(map_id = "map") %>%
#             mapdeck_view(
#                 location = c(
#                     city$longitude, 
#                     city$latitude), 
#                 zoom = 10,
#                 duration = 3000,
#                 transition = "fly"
#                 )
#     })
#     
# # observeEvent(input$city,{
# # 
# #     print(" -- changing -- ")
# #     sf <- city
# #     
# #     mapdeck_update(map_id = "map") %>%
# #         add_polygon(
# #             data = sf,
# #             fill_colour = "value",
# #             fill_opacity = 200,
# #             layer_id = "acess",
# #             palette = "inferno",
# #             update_view = FALSE,
# #             focus_layer = FALSE,
# #         )
# # })
#  
# }
# 
# 
# # # Run the application
# shinyApp(ui = ui, server = server)


# library(shiny)
# library(shinydashboard)
# library(jsonify)
# 
# ui <- dashboardPage(
#     dashboardHeader()
#     , dashboardSidebar()
#     , dashboardBody(
#         mapdeckOutput(
#             outputId = 'myMap'
#         ),
#         sliderInput(
#             inputId = "longitudes"
#             , label = "Longitudes"
#             , min = -180
#             , max = 180
#             , value = c(-180,-90)
#         )
#         , verbatimTextOutput(
#             outputId = "observed_click"
#         )
#     )
# )
# server <- function(input, output) {
#     
#     #set_token('abc') ## set your access token
#     
#     origin <- capitals[capitals$country == "Australia", ]
#     destination <- capitals[capitals$country != "Australia", ]
#     origin$key <- 1L
#     destination$key <- 1L
#     
#     df <- merge(origin, destination, by = 'key', all = T)
#     
#     output$myMap <- renderMapdeck({
#         mapdeck(style = mapdeck_style('dark'), token = key) 
#     })
#     
#     ## plot points & lines according to the selected longitudes
#     df_reactive <- reactive({
#         if(is.null(input$longitudes)) return(NULL)
#         lons <- input$longitudes
#         return(
#             df[df$lon.y >= lons[1] & df$lon.y <= lons[2], ]
#         )
#     })
#     
#     observeEvent({input$longitudes}, {
#         if(is.null(input$longitudes)) return()
#         
#         mapdeck_update(map_id = 'myMap') %>%
#             add_scatterplot(
#                 data = df_reactive()
#                 , lon = "lon.y"
#                 , lat = "lat.y"
#                 , fill_colour = "country.y"
#                 , radius = 100000
#                 , layer_id = "myScatterLayer"
#                 , update_view = FALSE
#             ) %>%
#             add_arc(
#                 data = df_reactive()
#                 , origin = c("lon.x", "lat.x")
#                 , destination = c("lon.y", "lat.y")
#                 , stroke_from = "country.x"
#                 , stroke_to = "country.y"
#                 , layer_id = "myArcLayer"
#                 , id = "country.x"
#                 , stroke_width = 4
#                 , update_view = FALSE
#             )
#     })
#     
#     ## observe clicking on a line and return the text
#     observeEvent(input$myMap_arc_click, {
#         
#         event <- input$myMap_arc_click
#         output$observed_click <- renderText({
#             jsonify::pretty_json( event )
#         })
#     })
# }
# 
# shinyApp(ui, server)




library(shiny)
library(dplyr)
library(mapdeck)
library(sf)


ui <- shinyUI(fluidPage(
    selectInput(inputId = "city",
                label = h1("Pick city:"),
                choices = c("Belo Horizonte" = "bho",
                            "Fortaleza" = "for"),
                selected = "bho"),
    selectInput(inputId = "activity",
                label = h1("Pick activity:"),
                choices = c("TT", "ST"),
                selected = "TT"),
    sliderInput(inputId = "time",
                label = h1("Pick time threshold:"),
                min = 30, max = 120,
                step = 30, value = 30,
                animate = TRUE),
    mapdeckOutput("map")
)
)



# SERVER --------------------------------------------------------------------------------------

# Define a server for the Shiny app
server <- shinyServer(function(input, output) {
    
    
    data <- readRDS(url("https://github.com/kauebraga/misc/raw/master/data.rds"), "rb")
    
    centroids <- data.frame(sigla_muni = c("for", "bho"),
                            lon = c(-38.52770, -43.95988),
                            lat = c( -3.785656, -19.902739))
    
    # register mapbox api key
    # mapdeck::set_token("YOUR_API")
    key <- 'pk.eyJ1Ijoia3BpdmVydCIsImEiOiJjazc2dWc4YTUwMHp6M2tvNWIyYTQyaXNnIn0.MmXD8-ud_HmuDffvJMotVA'
   
    # reactive for the city
    city_filtered <- reactive({
        data %>% filter(sigla_muni == input$city)
    })
    
    # reactive for the activity
    activity_filtered <- reactive({
        city_filtered() %>% dplyr::filter(activity == input$activity)
    })
    
    
    # Reactive for time threshold
    time_filtered <- reactive({
        
        activity_filtered() %>% dplyr::filter(time_threshold == input$time)
        
    })
    
    # initialize baseMap
    output$map <- renderMapdeck({
        
        mapdeck(
            token = key, 
            style = 'mapbox://styles/mapbox/dark-v9',
            location = c(-43.95988, -19.902739), 
            zoom = 0
            )
        
    })
    

    
    
   
        
        
        observe({
            centroids_city <- filter(centroids, sigla_muni == input$city)
            
            mapdeck_update(map_id = "map") %>%
                mapdeck_view(location = c(centroids_city$lon, centroids_city$lat), zoom = 10,
                             duration = 3000,
                             transition = "fly")
        })
        
        observeEvent({c(input$time, input$activity, input$city)},{
            
            print(" -- changing -- ")
            sf <- time_filtered()
            print( unique( sf$sigla_muni ) )
            print( unique( sf$time_threshold ) )
            print( unique( sf$activity )  )
            
            mapdeck_update(map_id = "map") %>%
                add_polygon(
                    data = sf,
                    fill_colour = "value",
                    fill_opacity = 200,
                    layer_id = "acess",
                    palette = "inferno",
                    update_view = FALSE,
                    focus_layer = FALSE,
                )
        })
        
        
    })
    
    
    
    shinyApp(ui = ui, server = server)
    
