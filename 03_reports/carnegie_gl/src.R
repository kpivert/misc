# Library calls -----------------------------------------------------------

# app
library(htmltools)
library(shinydashboard)
library(shiny)
library(DT)
require(shinythemes)

# viz
library(packcircles)
library(plotly)
library(leaflet)
library(sf)
require(geosphere)
require(deckgl)
library(feather) # prolly not needed
library(tidyverse)

# Data sets ---------------------------------------------------------------

dat <- read_feather(
  here::here(
    "03_reports/carnegie_gl",
    "geolocated_performers_dt.feather"
    )
  )

dat <- dat %>% 
  mutate(
    from_lon = lon,
    from_lat = lat, 
    from_name = birthPlaceName,
    to_lon = ch_lon,
    to_lat = ch_lat
  ) %>% 
  mutate(
    to_name = "Carnegie Hall",
    tooltip = str_c(name, " Born in: ", from_name),
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

m <- readRDS(
  here::here(
    "03_reports/carnegie_gl",
    "continent_sf.RDS"
    )
  )
countries <- readRDS(
  here::here(
    "03_reports/carnegie_gl",
    "country_sf.RDS")
  )

instruments <- read_feather(
  here::here(
    "03_reports/carnegie_gl",
    "name_instrument.feather"
    )
  )
roles <- read_feather(
  here::here(
    "03_reports/carnegie_gl",
    "name_role.feather"
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


