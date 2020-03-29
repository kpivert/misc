

library(shiny)
library(r2d3)
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


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
