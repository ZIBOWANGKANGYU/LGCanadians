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
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        
        data_cleaned <- reactive(filter_data(data, input$geo, input$demo))
        chart_title <- reactive(set_title(input$demo))

        ggplot(data = data_cleaned()[["data_bars"]], aes(y = GEO)) +
          geom_col(aes(x = thousands, fill = `Sexual orientation`), position = position_dodge()) +
          geom_text(data = data_cleaned()[["data_percent"]], aes(x = 0.98 * data_cleaned()[["data_x_lim_dict"]][Demographic], label = lg_percent)) +
          facet_grid(cols = vars(Demographic), scales = "free_x") + 
          labs(title = chart_title()) +
          theme(axis.title.y = element_blank(),
                legend.title = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
