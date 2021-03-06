#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("src.R")
library(shiny)

 
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("darkly"),

    # Application title
    titlePanel("How did they get to Carnegie Hall?"),


        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Arcs",
                    deckglOutput(
                        "arc_map", 
                        width = "100%", 
                        height = "800px"
                    )
                ),
            tabPanel(
                "Scatter",
                deckglOutput(
                    "scatter_map",
                    width = "100%",
                    height = "800px"
                )
            ),
            tabPanel(
                "Hex",
                deckglOutput(
                    "hex_map",
                    width = "100%",
                    height = "800px"
                )
            ),
            tabPanel(
                "Filtered by Continent",
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
        )
        )
    )
)
)
