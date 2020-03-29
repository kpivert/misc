# * mapdeck reactive example ----------------------------------------------

key <- 'pk.eyJ1Ijoia3BpdmVydCIsImEiOiJjazc2dWc4YTUwMHp6M2tvNWIyYTQyaXNnIn0.MmXD8-ud_HmuDffvJMotVA'
mapdeck(token = key, style = 'mapbox://styles/mapbox/dark-v9')
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
library(jsonify)
library(geosphere)
library(tidyverse)

ui <- shinyUI(fluidPage(
    selectInput(inputId = "continent",
                label = h1("Pick Continent:"),
                choices = c(
                    "Africa" = "AF",
                    "Asia" = "AS",
                    "Europe"= "EU",
                    "North America" = "NA",
                    "Oceania" = "OC",
                    "South America" = "SA"
                ),
                selected = "NA"),
    verbatimTextOutput(
        outputId = "observed_click"
        ),
    mapdeckOutput("map")
    )
)

# SERVER --------------------------------------------------------------------------------------

# Define a server for the Shiny app
server <- shinyServer(function(input, output) {
    

# * Data ------------------------------------------------------------------

    dat <- feather::read_feather(
        "/Users/kurtispivert/Documents/00_gh/Carnegie-Hall-App/data/geolocated_performers_dt.feather"
    ) 
    
    dat <- dat %>% 
        mutate(
            from_lon = lon,
            from_lat = lat, 
            from_name = birthPlaceName,
            to_lon = ch_lon,
            to_lat = ch_lat
        ) 
    
    dat <- dat %>%   
        mutate(
            distance_miles = distGeo(
                dat %>% 
                    select(starts_with("from_l")) %>% 
                    as.matrix(),
                dat %>% 
                    select(starts_with("to_l")) %>% 
                    as.matrix()
            ) / 1609.344
        ) %>% 
        mutate(
            to_name = "Carnegie Hall",
            tooltip = str_c(
                name, 
                ": Born in ", 
                from_name,
                ", ",
                round(distance_miles),
                " miles from Carnegie Hall"
            ),
            ch_color = "#F7002B",
            from_color = case_when(
                `continent code` == "AF" ~ "#8F9DCB",
                `continent code` == "AS" ~ "#DBA8AF",
                `continent code` == "EU" ~ "#f9f6f7",
                `continent code` == "NA" ~ "#1DA3CA",
                `continent code` == "OC" ~ "#BF346B",
                `continent code` == "SA" ~ "#767969"
            ),
            cont_lon = case_when(
                `continent code` == "AF" ~ 18.77,
                `continent code` == "AS" ~ 100.16,
                `continent code` == "EU" ~ 11.61,
                `continent code` == "NA" ~ -101,
                `continent code` == "OC" ~ 133.7,
                `continent code` == "SA" ~ -59.4
            ),
            cont_lat = case_when(
                `continent code` == "AF" ~ 10.86,
                `continent code` == "AS" ~ 39.39,
                `continent code` == "EU" ~ 48.8,
                `continent code` == "NA" ~ 41.86,
                `continent code` == "OC" ~ -20.9,
                `continent code` == "SA" ~ -14
            )
        )

    centroids <- tibble(
        continent = c(
            "AF", "AS", "EU", "NA", "OC", "SA"
            ),
        lon = c(
            26.17, 87.331, 23.106111, -99.99611, 133.4166, -56.1004
            ),
        lat = c(
            5.65, 43.681, 53.5775, 48.367222222222225, -24.25, -15.6006
            )
        )
        

# * Key -------------------------------------------------------------------

    # register mapbox api key
    # mapdeck::set_token("YOUR_API")
    key <- 'pk.eyJ1Ijoia3BpdmVydCIsImEiOiJjazc2dWc4YTUwMHp6M2tvNWIyYTQyaXNnIn0.MmXD8-ud_HmuDffvJMotVA'
    

# * App -------------------------------------------------------------------

    
    # reactive for the city
    df_filtered <- reactive({
        dat %>% filter(continent == input$continent)
    })
    
    
    # initialize baseMap
    output$map <- renderMapdeck({
        
        mapdeck(
            token = key, 
            style = 'mapbox://styles/mapbox/dark-v9',
            location = c(-43.95988, -19.902739), 
            zoom = 0
        ) %>% 
            add_arc(
                data = dat, 
                layer_id = "arc_layer", 
                origin = c("lon", "lat"), 
                destination = c("ch_lon", "ch_lat"),
                stroke_from = "from_color", 
                stroke_to = "ch_color", 
                tooltip = "tooltip"
            )
        
    })
    
    
    observe({
        centroids_cont <- filter(centroids, continent == input$continent)
        
        mapdeck_update(map_id = "map") %>%
            mapdeck_view(
                location = c(centroids_cont$lon, centroids_cont$lat), 
                zoom = 2, 
                duration = 3000,
                transition = "fly"
                ) 
    })
    
    # observeEvent({input$continent}, {
    # 
    #     print(" -- changing -- ")
    #     # df <- filter(dat, continent == input$continent)
    #     # print( unique( df$continent ) )
    #    
    #     mapdeck_update(map_id = "map") %>%
    #         add_arc(
    #             data = df_filtered(),
    #             layer_id = "arc_layer",
    #             origin = c("lon", "lat"),
    #             destination = c("ch_lon", "ch_lat"),
    #             stroke_from = "from_color",
    #             stroke_to = "ch_color",
    #             tooltip = "tooltip",
    #             update_view = FALSE,
    #             focus_layer = FALSE
    #         )
    # 
    # 
    #         # add_polygon(
    #         #     data = df_cont,
    #         #     fill_colour = "value",
    #         #     fill_opacity = 200,
    #         #     layer_id = "acess",
    #         #     palette = "inferno",
    #         #     update_view = FALSE,
    #         #     focus_layer = FALSE,
    #         # )
    # })
    # 
    # observe clicking on a line and return the text
        observeEvent(input$myMap_arc_click, {

            event <- input$myMap_arc_click
            output$observed_click <- renderText({
                jsonify::pretty_json( event )
            })
        })

    
})



shinyApp(ui = ui, server = server)



