#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

pacman::p_load(shiny, tidyverse, ggplot2, magrittr, gapminder, plotly,
               shinythemes, shinyjs, DT, leaflet, knitr, Stat2Data, 
               dplyr, patchwork, ggpubr, htmlwidgets, shinythemes)

data("ThreeCars")

dataPanel = tabPanel(title = "Stat2Data Package", 
                     sidebarLayout(
                         sidebarPanel(
                           h3(strong("Description of the variables: ")),
                           HTML(paste0("<ul><li>", 
                                       code("CarType"), ": BMW, Jaguar, or Porsche.</li><li>", 
                                       code("Price"), ": Asking price (in $1,000's).</li><li>", 
                                       code("Age"), ": Age of the car (in years).</li><li>", 
                                       code("Mileage"), ": Previous miles driven (in 1,000's).</li><li>",
                                       code("Car"), ": 0=Porsche, 1=Jaguar and 2=BMW. </li><li>",
                                       code("Porsche"), ": Indicator with 1=Porsche and 0=otherwise.</li><li>", 
                                       code("Jaguar"), ": Indicator with 1=Jaguar and 0=otherwise.</li><li>", 
                                       code("BMW"), ": Indicator with 1=BMW and 0=otherwise.</li></ul>")),
                           selectInput(inputId = "vars", label = "Select the variable: ", choices = c("CarType"=1, "Price"=2,
                                                                                                      "Age"=3, "Mileage"=4,
                                                                                                      "Car"=5, "Porsche"=6,
                                                                                                      "Jaguar"=7, "BMW"=8)),
                           verbatimTextOutput("info")),
                         mainPanel(
                             p("In the following presentation we will do a simple analysis of the",
                               code("ThreeCars"), 
                               "data set from the", 
                               code("Stat2Data"), 
                               "package, which compare prices for Porsche, Jaguar, 
                               and BMW cars offered for sale at an internet site. 
                               Student project data collected from autotrader.com in Spring 2007."),
                             
                             p("On the left, we have the
                               description of the variables, and we can select each of them to see their 
                               summary. Below, we show a data set description, where we are able to 
                               search anything in the table, as long as sort the variables or show more
                               entries."),
                             
                             h3(strong("Data frame description")),
                             dataTableOutput("data")
                         ))
                     )

plotPanel <- tabPanel(title = "Plots of the variables",
                      useShinyjs(),
                      sidebarLayout(
                        position = "right",
                        sidebarPanel(
                          p("In the following panel, we are able to plot two graphics: a ",
                            strong("boxplot"), " and a ", strong("histogram."), "For the 
                            boxplot, we can select the variables ", code("Price"), ", ", 
                            code("Age"), " or ", code("Mileage"), " and see the difference 
                            between each manufacturer. Next to it, we can select again these
                            three variables with a determined number of bins to see their 
                            histograms."),
                          h4("Select the variable to be plotted in the boxplot"),
                          selectInput("box_var", label = "", choices = c("Price", "Age", "Mileage")),
                          h4("Select the variable and the number of bins to be plotted in the histogram"),
                          selectInput("hist_var", label = "", choices = c("Price", "Age", "Mileage")),
                          sliderInput("n_bins", label = NULL, min = 2, max = 30, value = 10)
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                        )
                      )

regPanel <- tabPanel(title = "A simple regression",
                     useShinyjs(),
                     sidebarLayout(
                       sidebarPanel(
                         p("Finally, in this last panel we can see the relationship between the variables",
                           code("Price"), ", ", code("Age"), " and ", code("Mileage"), " by selecting the
                           target variable and the predictor variable. Moreover, each point has a different
                           color depending on the car type. Finally, we can see the Pearson's correlation 
                           coefficient between the two selected variables."),
                         radioButtons("target", label = "Select the target variable:",  choices = c("Price", "Age", "Mileage")),
                         radioButtons("pred", label = "Select the predictor variable:", choices = c("Price", "Age", "Mileage")),
                         withMathJax("$$\\text{Whose Pearson's correlation coefficient } R^2 \\text{ is: }$$"),
                         verbatimTextOutput("rsquared")
                       ),
                       mainPanel(
                         plotlyOutput("plotly")
                       )
                     )
                     )

refPanel <- tabPanel("References",
                     p(tags$button(
                       class="btn btn-default", 
                       `data-toggle`="collapse", 
                       `data-target`="#collapseExample",
                       "References")),
                     div(class="collapse", id="collapseExample",
                         div(class="card card-body",
                             includeMarkdown("References.md")
                             )
                         )
                     )

                     
# Define UI for application that draws a histogram
ui <- navbarPage("Roberto J. Alcaraz Molina",
                 theme = shinytheme("sandstone"),
                 dataPanel,
                 plotPanel,
                 regPanel,
                 refPanel
                 )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$info <- renderPrint({
    summary(ThreeCars[, as.numeric(input$vars)])
  })
  
  output$data <- renderDataTable({
    ThreeCars
  })
  
  
  output$plot <- renderPlot({
    p1 <- ggplot(data = ThreeCars, aes_string(x = "CarType", y = input$box_var, color = "CarType")) +
      geom_boxplot() +
      ggtitle("Boxplot for the CarType") +
      theme_bw()
    
    p2 <- ggplot(data = ThreeCars, aes_string(x = input$hist_var)) +
      geom_histogram(bins = input$n_bins, fill = "orange", color = "black") +
      ggtitle("Histogram") +
      theme_bw()
    
    ggarrange(p1, p2)
  })
  
  output$plotly <- renderPlotly({
    form <- as.formula(paste(input$target, " ~ ", input$pred))
    fit <- lm(form, data = ThreeCars)
    
    ggplot(data = ThreeCars, aes_string(x = input$pred, y = input$target)) +
      geom_point(aes(colour = CarType)) + 
      geom_smooth(method = "lm") +
      theme_bw()
  })
  
  cmd = reactive(eval(parse(text=paste(round(cor(ThreeCars[,input$target],
                                                 ThreeCars[,input$pred]),4),sep=""))));
  
  output$rsquared <- renderPrint({
    cmd()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
