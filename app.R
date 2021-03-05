#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

pacman::p_load(shiny, tidyverse, ggplot2, magrittr, gapminder, plotly,
               shinythemes, shinyjs, DT, leaflet, knitr, Stat2Data, 
               dplyr, patchwork, ggpubr, htmlwidgets)

data("ThreeCars")

dataPanel = tabPanel(title = "Stat2Data Package", 
                     sidebarLayout(
                         sidebarPanel(selectInput(inputId = "vars", label = "Select the variable", choices = c("CarType"=1, "Price"=2,
                                                                                                               "Age"=3, "Mileage"=4,
                                                                                                               "Car"=5, "Porsche"=6,
                                                                                                               "Jaguar"=7, "BMW"=8)),
                                      HTML(paste0("<ul><li>", code("CarType"), ": BMW, Jaguar, or Porsche.</li><li>", code("Price"), ": Asking price (in $1,000's).</li><li>", code("Age"), ": Age of the car (in years).</li><li>", code("Mileage"), ": Previous miles driven (in 1,000's).</li><li>", code("Car"), ": 0=Porsche, 1=Jaguar and 2=BMW. </li><li>", code("Porsche"), ": Indicator with 1=Porsche and 0=otherwise.</li><li>", code("Jaguar"), ": Indicator with 1=Jaguar and 0=otherwise.</li><li>", code("BMW"), ": Indicator with 1=BMW and 0=otherwise.</li>
</ul>")),
                                      verbatimTextOutput("info")),
                         mainPanel(
                             p("In the following presentation we will do a simple analysis of the", code("ThreeCars"), "data set from the", code("Stat2Data"), "package, which compare prices for Porsche, Jaguar, and BMW cars offered for sale at an internet site."),
                             p("Student project data collected from autotrader.com in Spring 2007."),
                             dataTableOutput("data")
                         ))
                     )


                     
# Define UI for application that draws a histogram
ui <- navbarPage("shiny App",
                 dataPanel)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$info <- renderPrint({
    summary(ThreeCars[, as.numeric(input$vars)])
  })
  
  output$data <- renderDataTable({
    ThreeCars
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
