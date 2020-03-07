
# * DeckGL Vignette -------------------------------------------------------

library(deckgl)
library(feather)
library(tidyverse)

yourSuperSecretApiToken <- 
does_it_work()

#> You should see a text layer telling you that it works.

# Or in case you do have an api token from mapbox ...
does_it_work(token = "")
does_it_work(yourSuperSecretApiToken)

#> Output should be the same as above but rendered on top of a base map from mapbox.


# Pass 'data = NULL' to load some sample data for the hexagon layer
deckgl() %>%
  add_hexagon_layer(data = NULL) %>%
  add_mapbox_basemap(yourSuperSecretApiToken) # optional

# This Will Set MB Token for One Session
Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoia3BpdmVydCIsImEiOiJjazc2dWc4YTUwMHp6M2tvNWIyYTQyaXNnIn0.MmXD8-ud_HmuDffvJMotVA")

# This is the Style we need. 

deckgl() %>%
  add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9")

# Grid layer example

sample_data <- paste0(
  "https://raw.githubusercontent.com/",
  "uber-common/deck.gl-data/",
  "master/website/sf-bike-parking.json"
)

properties <- list(
  pickable = TRUE,
  extruded = TRUE,
  cellSize = 200,
  elevationScale = 4,
  getPosition = JS("data => data.COORDINATES"),
  getTooltip = JS("object => object.count")
)

deckgl(zoom = 11, pitch = 45) %>%
  add_grid_layer(data = sample_data, properties = properties) %>%
  add_mapbox_basemap() # optional


# Generic function
deckgl() %>% add_layer("ArcLayer", "arc-layer", data, properties)

# Shortcut function
deckgl() %>% add_arc_layer("arc-layer", data, properties)

example(add_icon_layer)


# Corresponding R code

deck <- deckgl() %>%
  add_grid_layer(
    id = "grid-layer",
    data = data,
    extruded = TRUE,
    cellSize = 200,
    elevationScale = 4,
    getPosition = JS("d => d.COORDINATES")
  )

properties <- list(
  extruded = TRUE,
  cellSize = 200,
  elevationScale = 4,
  getPosition = get_property("COORDINATES")
)

htmlwidgets:::toJSON2(properties, pretty = TRUE)
#> {
#>   "extruded": true,
#>   "cellSize": 200,
#>   "elevationScale": 4,
#>   "getPosition": "data => data.COORDINATES"
#> }


library(geojsonio)

val_max <- apply(us_cities[, c("lat", "long")], 2, max)
val_min <- apply(us_cities[, c("lat", "long")], 2, min)
center <- ((val_max + val_min) / 2) %>%
  as.list()

us_cities$marker <- ifelse(us_cities$capital > 0, "marker2", "marker1")

icon_mapping <- list(
  marker1 = icon_definition(), # default mapping
  marker2 = icon_definition(x = 128, mask = FALSE) # use second icon on the default atlas image
)

deckgl(longitude = center$long, latitude = center$lat, zoom = 3) %>%
  add_icon_layer(
    data = us_cities,
    iconMapping = icon_mapping,
    getIcon = JS("d => d.marker"),
    getPosition = get_position("lat", "long"),
    pickable = TRUE,
    onClick = JS("info => deckglWidget.tooltipElement.innerHTML = info.object.name"),
    getSize = 3.5,
    sizeScale = 15
  ) %>%
  add_mapbox_basemap(token = yourSuperSecretApiToken)


dat <- read_feather(here::here("00_data", "geolocated_performers_dt.feather"))

dat <- dat %>% 
  rename(
    from_lon = lon,
    from_lat = lat, 
    from_name = birthPlaceName,
    to_lon = ch_lon,
    to_lat = ch_lat
  ) %>% 
  mutate(
    to_name = "Carnegie Hall"
  )

dat <-  dat %>% 
  mutate(
    to_name = rep("CH", nrow(.))
  )

dat <- dat %>% 
  mutate(
    tooltip = str_c(name, " Born in: ", from_name)
  ) %>% 
  mutate(
    ch_color = "#F7002B",
    from_color = case_when(
      `continent code` == "AF" ~ "#8F9DCB",
      `continent code` == "AS" ~ "#DBA8AF",
      `continent code` == "EU" ~ "#f9f6f7",
      `continent code` == "NA" ~ "#1DA3CA",
      `continent code` == "OC" ~ "#BF346B",
      `continent code` == "SA" ~ "#767969"
    )
  )


properties = list(
      pickable = TRUE,
      getStrokeWidth = 2,
      cellSize = 200,
      elevationScale = 4,
      getSourcePosition = get_position("from_lat", "from_lon"),
      getTargetPosition = get_position("to_lat", "to_lon"),
      # getSourceColor = JS("d => [Math.sqrt(d.inbound), 140, 0]"),
      # getTargetColor = JS("d => [Math.sqrt(d.outbound), 140, 0]"),
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

hex_dat <- dat %>% 
  group_by(from_name) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  left_join(., dat, by = "from_name") %>% 
  distinct(from_name, .keep_all = TRUE) %>% 
  select(from_name, n, from_lat, from_lon, from_color, ch_color)


deckgl(
  latitude = 40.7,
  longitude = -74,
  zoom = 11, 
  pitch = 0
) %>% 
  add_mapbox_basemap() %>%   
  add_arc_layer(
    data = dat,
    id = 'arc-layer',
    properties = properties
  )

# const layer = new ArcLayer({
#   id: 'arc-layer',
#   data,
#   pickable: true,
#   getWidth: 12,
#   getSourcePosition: d => d.from.coordinates,
#   getTargetPosition: d => d.to.coordinates,
#   getSourceColor: d => [Math.sqrt(d.inbound), 140, 0],
#   getTargetColor: d => [Math.sqrt(d.outbound), 140, 0],
#   onHover: ({object, x, y}) => {
#     const tooltip = `${object.from.name} to ${object.to.name}`;
#     /* Update tooltip
#     http://deck.gl/#/documentation/developer-guide/adding-interactivity?section=example-display-a-tooltip-for-hovered-object
#       */
#   }
# });

sample_data <- paste0(
  "https://raw.githubusercontent.com/",
  "uber-common/deck.gl-data/master/",
  "website/bart-segments.json"
)

properties <- list(
  pickable = TRUE,
  getStrokeWidth = 12,
  getSourcePosition = get_property("from.coordinates"),
  getTargetPosition = get_property("to.coordinates"),
  getSourceColor = JS("d => [Math.sqrt(d.inbound), 140, 0]"),
  getTargetColor = JS("d => [Math.sqrt(d.outbound), 140, 0]"),
  getTooltip = JS("object => `${object.from.name} to ${object.to.name}`")
)


deck <- deckgl(zoom = 10, pitch = 35) %>%
  add_arc_layer(data = sample_data, properties = properties) %>%
  add_mapbox_basemap()

if (interactive()) deck

deckgl(zoom = 10, pitch = 35) %>%
  add_arc_layer(data = sample_data, properties = properties) %>%
  add_mapbox_basemap(token = yourSuperSecretApiToken)

deckgl(zoom = 10, pitch = 35) %>%
  add_arc_layer(data = dat, properties = properties) %>%
  add_mapbox_basemap(token = yourSuperSecretApiToken)



source("src.R")

df <- readRDS(sample_data)

require(jsonlite)


test <- toJSON(dat)

dat %>% slice(1) %>% toJSON()


# * Hex Example -----------------------------------------------------------

sample_data <- paste0(
  "https://raw.githubusercontent.com/",
  "uber-common/deck.gl-data/",
  "master/website/sf-bike-parking.json"
)

properties <- list(
  extruded = TRUE,
  radius = 200,
  elevationScale = 4,
  getPosition = get_property("COORDINATES"),
  getTooltip = JS("object => `${object.centroid.join(', ')}<br/>Count: ${object.points.length}`"),
  fixedTooltip = TRUE
)

deck <- deckgl(zoom = 11, pitch = 45, bearing = 35) %>%
  add_hexagon_layer(data = sample_data, properties = properties) %>%
  add_mapbox_basemap()

if (interactive()) deck


properties <- list(
  extruded = TRUE,
  radius = 10000,
  elevationScale = 4,
  getPosition = get_position("from_lat", "from_lon"),
  getTooltip = JS("object => `${object.centroid.join(', ')}<br/>Count: ${object.points.length}`"),
  fixedTooltip = TRUE
)

deckgl(zoom = 11, bearing = 35) %>%
  add_hexagon_layer(data = dat, properties = properties) %>%
  add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9")

deckgl() %>%
  add_hexagon_layer(data = dat, properties = properties) %>%
  add_mapbox_basemap(style = "mapbox://styles/mapbox/dark-v9")



# * Scatterplot Example ---------------------------------------------------
bart_stations <- paste0(
  "https://raw.githubusercontent.com/",
  "uber-common/deck.gl-data/",
  "master/website/bart-stations.json"
)

properties <- list(
  getPosition = get_property("coordinates"),
  getRadius = JS("data => Math.sqrt(data.exits)"),
  radiusScale = 6,
  getColor = c(255, 140, 20)
)

deck <- deckgl(zoom = 10.5, pitch = 35) %>%
  add_scatterplot_layer(data = bart_stations, properties = properties) %>%
  add_mapbox_basemap()

if (interactive()) deck

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


