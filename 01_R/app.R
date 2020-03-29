# * mapdeck reactive example ----------------------------------------------
library(shiny)
library(tidyverse)
library(mapdeck)
library(jsonify)

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
        "Carnegie-Hall-App/data/geolocated_performers_dt.feather"
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
    key <- 'your_key_here'
    

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



