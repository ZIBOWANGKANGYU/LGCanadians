#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)

setwd("~/Documents/rshiny/example1/MyApp/")
source("data_cleaning.R")
source("create_chart.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lesbian and gay Canadians"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
       selectInput("var", "Summary variable", summary_vars),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        summary_var <- input$var

        if (summary_var == "Race"){
          ggplotly(chart_race) %>% config(displayModeBar = FALSE)
        } else if (summary_var == "Education"){
          ggplotly(chart_education) %>% config(displayModeBar = FALSE)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
