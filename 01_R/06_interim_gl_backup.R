## Backup of interim_gl as of 2020-03-27

## src.r ---------------------------------------------------

# Library calls -----------------------------------------------------------

# app
library(htmltools)
library(glue)
library(shinydashboard)
library(shiny)
library(DT)
library(shinythemes)
library(sp)

# viz
library(packcircles)
# library(hrbrthemes)
library(plotly)
library(leaflet)
library(sf)
require(geosphere)
require(deckgl)
library(feather) # prolly not needed
library(tidyverse)

# Data sets ---------------------------------------------------------------

dat <- read_feather(here::here("data", "geolocated_performers_dt.feather"))

# Continent Shapefiles
m <- readRDS("data/continent_sf.RDS")

# Country Shapefiles
countries <- readRDS("data/country_sf.RDS")

world <- read_sf(
  dsn = here::here("data", "gis"),
  layer = "ne_110m_admin_0_countries"
) %>%
  mutate(
    ISO_A2 = replace(ISO_A2, NAME == "France", "FR")
  )

# Instrumental Performers Dataset
instruments <- read_feather("data/name_instrument.feather")

# Performer Roles Dataset
roles <- read_feather("data/name_role.feather")

# Join Datasets for App Use

dat <- left_join(
  dat, 
  instruments
) %>% 
  mutate(
    inst = str_to_title(inst),
    role = str_to_title(role)
  )

# * Add Mapbox API Token for Session --------------------------------------

# Sys.setenv(MAPBOX_API_TOKEN = "your_super-secret_token")

# * Add Variables for DeckGL Vizes and Tooltip ----------------------------

# Edit Names
dat <- dat %>% 
  mutate(
    from_lon = lon,
    from_lat = lat, 
    from_name = birthPlaceName,
    to_lon = ch_lon,
    to_lat = ch_lat
  ) 

# Add Distances, Tooltip, and Continental Color Scheme  
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

# App functions -----------------------------------------------------------

# a wrapper for ggplot_circlepack %>% ggplotly
gg_circlepack <- function(dat, label) {
  packing <- circleProgressiveLayout(dat$n, sizetype = "area")
  layout <- circleLayoutVertices(packing, npoints = 6)
  
  dat <- bind_cols(dat, packing)
  dat$text <- paste0(dat[[1]], " (", dat[["n"]], ")")
  co <- quantile(dat[["n"]], .95)
  print(co)
  print(100 < co)
  dat[[label]] <- if_else(dat[["n"]] < co, "", dat[[label]])
  
  print(head(dat))
  
  kvm <- set_names(dat$text, 1:nrow(dat))
  layout$text <- kvm[layout$id]
  
  ggplot(dat, aes(x, y, text = text)) +
    geom_text(aes_(size = ~n, label = as.name(label))) +
    geom_polygon(data = layout, aes(color = as.factor(id), fill = as.factor(id), text = text), size = 3, alpha = .5) +
    scale_size_continuous(range = c(3,5)) +
    theme_void() +
    theme(legend.position = 'none') +
    coord_equal()
}

# build a vector for leaflet::fitBounds
fitBounds_bbox <- function(dat) {
  x <- st_bbox(dat) %>% unname()
  # meh but it's better
  if ("Europe" %in% unique(dat$region)) x[1] <- -10; x[3] <- 100
  x
}

## server.R ---------------------------------------------



# * Source Functions and Data ---------------------------------------------

source("src.R")


# * Continent Colors ------------------------------------------------------

pal <- colorFactor(
  c("#8F9DCB", 
    "#DBA8AF", 
    "#BF346B", 
    "#f9f6f7", 
    "#1DA3CA", 
    "#767969"
  ), 
  m$region
)


# * Server  ---------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  rv <- reactiveValues()
  
# * Tab 1: Arc Map of Entire Dataset --------------------------------------

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
      zoom = 2, 
      pitch = 0
    ) %>% 
      add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9") %>%   
      add_arc_layer(
        data = dat,
        id = 'arc-layer',
        properties = properties
      )
  })
  

# * Tab 2: Scatterplot ----------------------------------------------------
  
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
      zoom = 3, 
      pitch = 0
    ) %>% 
      add_scatterplot_layer(
        data = dat, 
        properties = properties
      ) %>% 
      add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9")
    
  })
  

# * Tab 3: Hex Map --------------------------------------------------------

  output$hex_map <- renderDeckgl({
    
    properties <- list(
      extruded = TRUE,
      radius = 10000,
      elevationScale = 4,
      getPosition = get_position("from_lat", "from_lon"),
      getTooltip = JS("object => `${object.centroid.join(', ')}<br/>Count: ${object.points.length}`"),
      fixedTooltip = TRUE
    )
    
    deckgl(
      latitude = 40.7,
      longitude = -74,
      zoom = 2, 
      pitch = 0
    ) %>%
      add_hexagon_layer(data = dat, properties = properties) %>%
      add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9")
  })
  

# * Tab 4: Filtered by Continent ------------------------------------------

  # Input Map ---------------------------------------------
  
  output$selectmap <- renderLeaflet({
    
    leaflet(m,
            options = leafletOptions(
              zoomControl = FALSE,
              dragging = FALSE,
              minZoom = 0,
              maxZoom = 0)
    ) %>%
      addPolygons(layerId = ~region,
                  fillColor = ~pal(region),
                  fillOpacity = 1,
                  color = "black",
                  stroke = F,
                  highlight = highlightOptions(
                    fillOpacity = .5,
                    bringToFront = TRUE))
  })
  
  
  observe({
    click <- input$selectmap_shape_click
    
    if (is.null(click)) return()
    
    updateSelectizeInput(session, "continent",
                         selected = click$id)
  })
  
  # Respond to Input -------------------------------------------------
  
  observeEvent(c(input$continent, input$main_tabs), {
    print("triggered Observe")
    leafletProxy("selectmap", session) %>%
      removeShape("selected") %>% 
      addPolylines(data = filter(m, region == input$continent),
                   layerId = "selected",
                   color = "black",
                   weight = 3)
    
    
    
  })
  
  # Output DeckGL Continent Specific  -------------------------------
  
  
  observeEvent(input$continent, {
    
    rv$map_dat <- filter(dat, region %in% input$continent) 
    
    output$`filtered-map` <- renderDeckgl({
      
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
        # latitude = 40.7,
        # longitude = -74,
        latitude = 38,
        longitude = -105,
        # latitude = rv$map_dat$cont_lat,
        # longitude = rv$map_dat$cont_lon,
        zoom = 1,
        pitch = 3
      ) %>%
        add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9") %>%
        add_arc_layer(
          data = rv$map_dat,
          id = 'arc-layer',
          properties = properties
        )
    })
    
    output$top_instruments <- renderPlot(
      ggplot(
        rv$map_dat %>% 
          filter(!is.na(inst)) %>% 
          count(inst) %>% 
          arrange(desc(n)) %>% 
          slice(1:10), 
        aes(
          x = reorder(inst, n),
          y = n
          # fill = rv$map_dat$from_color
        )
      ) +
        geom_col(
          width = .5,
          fill = "#F7002B"
        ) +
        coord_flip() +
        # theme_ft_rc()
        theme_dark()
    )
    
    
  })

# * Tab 5: Detail Table ---------------------------------------------------

  # output$table_1 <- DT::renderDataTable(DT::datatable({
  #   data <- dat %>% 
  #     select(
  #       Name = name, 
  #       `Birth Place` = birthPlaceName,
  #       `Birth Date` = birthDate,
  #       `Online Resource`,
  #       Instruments = inst
  #     )
  #   if (input$performer != "All") {
  #     data <- data[data$Name == input$performer, ]
  #   } 
  #   
  #   # 
  #   # if (input$role != "All") {
  #   #   data <- data[data$role == input$role, ]
  #   # }
  #   # if (input$instrument != "All") {
  #   #   data <- data[data$instrument == input$instrument, ]
  #   # }
  #   data  
  #   }, 
  #   options = list(
  #     initComplete = JS(
  #       "function(settings, json) {",
  #       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #       "}")
  #   ),
  #   rownames = FALSE,
  #   escape = FALSE
  #   # fillContainer = TRUE
  # ))
  
  output$table_1 <- renderDT({
    data <- dat %>% 
      select(
        Name = name, 
        `Birth Place` = birthPlaceName,
        `Birth Date` = birthDate,
        `Online Resource`,
        Instruments = inst, 
        Role = role
      )
    if (input$performer != "All") {
      data <- data[data$Name == input$performer, ]
    } 
    
    # 
    # if (input$role != "All") {
    #   data <- data[data$role == input$role, ]
    # }
    # if (input$instrument != "All") {
    #   data <- data[data$instrument == input$instrument, ]
    # }
    data  
  }, 
  options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ),
  rownames = FALSE,
  escape = FALSE
  # fillContainer = TRUE
  )
  

# * Tab 6: Filtered by Performer ------------------------------------------

  observeEvent(input$name, {
    
    rv$name_dat <- filter(dat, name %in% input$name) 
    
    output$`name_map` <- renderDeckgl({
      
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
        # latitude = 40.7,
        # longitude = -74,
        latitude = rv$name_dat$from_lat,
        longitude = rv$name_dat$from_lon,
        zoom = 5,
        pitch = 0
      ) %>%
        add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9") %>%
        add_arc_layer(
          data = rv$name_dat,
          id = 'arc-layer',
          properties = properties
        )
    
  })
  
  # output$text2 <- renderUI({
  #     HTML(paste("hello", "world", sep="<br/>"))
  #   })
        
  output$`performer-info` <- renderText({
    
      glue::glue("
               
               Birthplace: {rv$name_dat$birthPlaceName} 
               
               Birthdate: {rv$name_dat$birthDate} 
               
               Instrument(s): {rv$name_dat$inst} 
               
               Role: {rv$name_dat$role}
               
               ") 
        
    })
  
  # output$`performer-info` <- renderText(
  #   glue::glue("
  #              
  #              Birthplace: {rv$name_dat$birthPlaceName} \n
  #              
  #              Birthdate: {rv$name_dat$birthDate}
  #              
  #              Instrument(s): {rv$name_dat$inst} 
  #              
  #              Role: {rv$name_dat$role}
  #              
  #              ") 
  # )
  
  performer_birth_place <- unique(rv$name_dat$birthPlaceName)
  
  output$`performer-birthplace` <- renderText(
    glue::glue("
               
               Also from: {performer_birth_place}
               
               ")
  )
  
  output$table_2 <- renderDT({
    dat %>%
      filter(
        birthPlaceName %in% rv$name_dat$birthPlaceName,
        name != rv$name_dat$name
        ) %>%
      distinct(name, .keep_all = TRUE) %>% 
      select(
        Name = name,
        `Online Resource`,
        Instruments = inst
      )
    },
      filter = "none",
    options = list(
      dom = "t"
      # initComplete = JS(
      #   "function(settings, json) {",
      #   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      #   "}")
      ),
    rownames = FALSE,
    escape = FALSE
    # fillContainer = TRUE

  )
  
})
  
})


## ui.R ----------------------------------------



source("src.R")

shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Neuton&display=swap" rel="stylesheet">')),
  # tags$head(HTML("<style>* {font-size: 100%; font-family: 'Neuton', serif;}</style>")),
  # Application title
  # titlePanel("How did they get to Carnegie Hall?"),
  HTML("<h1>How did they get to <span style='color:#F7002B; font-family:Neuton; font-size:140%'>Carnegie Hall</span>?</h1>"),
  h1("Practice, Practice, Practice"),
  HTML(
    paste(
      "<p>So goes <a href = 'https://www.carnegiehall.org/Blog/2016/04/The-Joke'>'The Joke'</a>",
      "familiar to musicians across the world.",
      "Attributed to multiple people, its definitive origin story remains a mystery.</p>"
    )
  ),
  HTML(
    paste(
      "<p>This <a href = 'https://shiny.rstudio.com'>Shiny application</a>",
      "demonstrates how far each of the >8000 individual performers have traveled to grace",
      "the stage at <a href = 'https://www.carnegiehall.org'>Carnegie Hall</a>"
      
    )
  ),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Routes To Carnegie Hall",
        deckglOutput(
          "arc_map", 
          width = "100%", 
          height = "800px"
        )
      ),
      # tabPanel(
      #   "Scatter",
      #   deckglOutput(
      #     "scatter_map",
      #     width = "100%",
      #     height = "800px"
      #   )
      # ),
      tabPanel(
        "Performer Density",
        deckglOutput(
          "hex_map",
          width = "100%",
          height = "800px"
        )
      ),
      tabPanel(
        "Continents",
        fluidRow(
          column(
            4,
            wellPanel(
              h4("Continent:"),
              leafletOutput("selectmap", height = 200),
              selectizeInput(
                inputId = "continent",
                label = NULL,
                choices = m$region,
                selected = NULL
              ),
              plotOutput(
                "top_instruments"
              )
            )
          ),
          column(
            8,
            deckglOutput(
              "filtered-map",
              width = "auto",
              height = "800px"
            )
          )
        )
      ), 
      tabPanel(
        "Performers",
        fluidRow(
          column(
            5,
            wellPanel(
              h4("Performer"),
              selectizeInput(
                inputId = "name",
                label = NULL,
                choices = sort(dat$name),
                selected = NULL
              ),
              verbatimTextOutput(
                outputId = "performer-info"
              ),
              textOutput(
                outputId = "performer-birthplace"
              ),
              DT::dataTableOutput("table_2")
            )
          ),
          column(
            7,
            deckglOutput(
              "name_map",
              width = "auto",
              height = "800px"
            )
          )
        )
      ),
      tabPanel(
        "Detail Table",
        fluidPage(
          wellPanel(
          h4("Performer:"),
          selectInput(
                inputId = "performer",
                label = "Performer",
                choices = c(
                  "All",
                  unique(dat$name)
                  )
                )
          ),
            # selectInput(
            #     inputId = "role",
            #     label = "Role",
            #     choices = c(
            #       # "All",
            #       unique(dat$role) %>% str_to_title()
            #     )
            #   ),
            # selectInput(
            #     inputId = "instrument",
            #     label = "Instrument",
            #     choices = c(
            #       # "All",
            #       unique(dat$inst) %>% str_to_title()
            #     )
            # ),
          
            DT::dataTableOutput("table_1")
          )
        )
      )
    )
  )
)


