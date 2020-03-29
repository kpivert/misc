library(geosphere)
library(mapdeck)
library(tidyverse)
library(sf)

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
  

mapdeck( token = key, style = 'mapbox://styles/mapbox/dark-v9',
         location = c(
           -99.9961111111111,
           48.3672
           ) ,
         # pitch = 45 ,
         zoom = 5
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


df <- read.csv(paste0(
  'https://raw.githubusercontent.com/uber-common/deck.gl-data/master/',
  'examples/3d-heatmap/heatmap-data.csv'
))

mapdeck( token = key, style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_grid(
    data = dat, 
    lat = "lat", 
    lon = "lon", 
    cell_size = 50000, 
    elevation_scale = 50, 
    layer_id = "grid_layer"
  )
mapdeck( token = key, style = mapdeck_style("dark"), pitch = 45 ) %>%
add_hexagon(
  data = dat
  , lat = "lat"
  , lon = "lon"
  , layer_id = "hex_layer"
 # , elevation_scale = 1000
  #, colour_range = colourvalues::colour_values(1:6, palette = colourvalues::get_palette("viridis")[70:256,])
)


library(sf)
library(geojsonsf)

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

choro_dat <- dat %>% 
  count(ISO_Country) %>%
  mutate(n = n * 1000) %>% 
  right_join(world, ., by = c("iso_a2" = "ISO_Country")) %>% 
  mutate(
    tooltip = str_c(
      formal_en, 
      "\u2013",
      n / 1000,
      " Performers"
    )
  )

mapdeck(
  token = key, 
  style = mapdeck_style("dark"),
  pitch = 45
) %>% 
  add_polygon(
    data = choro_dat,
    layer = "polygon_layer",
    fill_colour = "mapcolor13",
    elevation = "n",
    tooltip = "tooltip"
  )

sf <- geojson_sf("https://symbolixau.github.io/data/geojson/SA2_2016_VIC.json")
sf$e <- sf$AREASQKM16 * 10

mapdeck(token = key, style = mapdeck_style("dark"), pitch = 45) %>%
  add_polygon(
    # data = sf[ data.table::`%like%`(sf$SA4_NAME16, "Melbourne"), ]
    data = sf
    , layer = "polygon_layer"
    , fill_colour = "SA2_NAME16"
    , elevation = "e"
  )



