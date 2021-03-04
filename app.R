#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

pacman::p_load(shiny, tidyverse, ggplot2, magrittr, gapminder, plotly,
               shinythemes, shinyjs, DT, leaflet, knitr, Stat2Data, 
               dplyr, patchwork, ggpubr, htmlwidgets)

data("ThreeCars")

dataPanel = tabPanel(title = "Stat2Data Package", 
                     sidebarLayout(
                         sidebarPanel(),
                         mainPanel(
                             p("In the following presentation we will do a simple analysis of the ``ThreeCars`` data set from the ``Stat2Data`` package, which compare prices for Porsche, Jaguar, and BMW cars offered for sale at an internet site.
                         Student project data collected from autotrader.com in Spring 2007.")
                         ))
                     )
                     
# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 dataPanel)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
