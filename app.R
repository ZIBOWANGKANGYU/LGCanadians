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
  theme = bslib::bs_theme(bootswatch = "united"),
  tabsetPanel(
  id = "panel1",
  tabPanel(
    "Lesbian and gay Canadians",
    
    titlePanel("Lesbian and gay Canadians"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("demo", "Summary variable", summary_vars),
        checkboxGroupInput("geo", "Please select regions", regions),
        uiOutput("showage")
      ),
      
      mainPanel(plotlyOutput("distPlot"),
                plotlyOutput("agePlot"))
    )
  ),
  tabPanel("Canadians in the GTA")
))

server <- function(input, output, session) {

    thematic::thematic_shiny()
    output$distPlot <- renderPlotly({

        data_cleaned <- reactive(filter_data(data, input$geo, input$demo))
        chart_title <- reactive(set_title(input$demo))
        

        (ggplot(data = data_cleaned()[["data_bars"]], aes(y = GEO)) +
          geom_col(aes(x = thousands, fill = `Sexual orientation`, text = Population), position = position_dodge()) +
          geom_text(data = data_cleaned()[["data_percent"]], aes(x = 0.98 * data_cleaned()[["data_x_lim_dict"]][Demographic], label = lg_percent), size = 8, fontface = "bold") +
          facet_grid(cols = vars(Demographic), scales = "free_x") + 
          labs(title = chart_title()) +
          theme(axis.title.y = element_blank(),
                legend.title = element_blank()))|> ggplotly(tooltip = "text")
    })
    
    output$showage <- renderUI({
      if (input$demo == "Race") {
        selectInput("showage_race", "Show age distribution?", c("Yes", "No"))
      }
    })
    
    output$agePlot <- renderPlotly({
      req(input$showage_race == "Yes" & input$demo == "Race")
      data_cleaned_by_age <- reactive(filter_data_by_age(data_by_age, input$demo))
      
      (ggplot(data = data_cleaned_by_age(), aes(x = `Age group`)) +
        geom_col(aes(y = age_prop, fill = `Sexual orientation`, text = Population), position = position_dodge()) +
        scale_y_continuous(labels = scales::percent) +
        facet_grid(cols = vars(Demographic)) + 
        labs(title = "Age distribution of lesbian and gay Canadians") +
        theme(axis.title.y = element_blank(),
              legend.title = element_blank())) |> ggplotly(tooltip = "text")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
