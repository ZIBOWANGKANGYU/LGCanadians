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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lesbian and gay Canadians"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
       selectInput("demo", "Summary variable", summary_vars),
       checkboxGroupInput("geo", "Please select regions", regions)),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"),
           textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        
        data_cleaned <- reactive(filter_data(data, input$geo, input$demo))

        data_cleaned() %>%
          ggplot(aes(y = GEO, x = thousands)) +
          geom_col(aes(fill = `Sexual orientation`), position = position_dodge()) +
          facet_grid(cols = vars(Demographic), scales = "free_x") + 
          labs(title = "Lesbian and Gay Canadians") +
          theme(axis.title.y = element_blank(),
                legend.title = element_blank())
    })
    
    output$text <- renderText({
      region_var <- input$geo
      return(region_var)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
