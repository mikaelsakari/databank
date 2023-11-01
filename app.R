#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(tidyverse)
library(here)
library(readxl)
library(ggplot2)
library(shinydashboard)
library(ggthemes)
library(janitor)
library(plotly)
library(rsconnect)
library(rmarkdown)


wbnew <- here("wbnew.xlsx") %>% 
  read_excel() %>%
  pivot_longer(cols = -c("Country Name", "Country Code", "Series Name", "Series Code"),
               names_to = "Year",
               values_to = "Value") %>% 
  mutate(Year = gsub("\\[.*\\]", "", Year)) %>% 
  rename(country = "Country Name", serie = "Series Name", year = "Year", value = "Value") %>% 
  mutate(value = as.numeric(value))

# Set the options to prevent scientific notation
options(scipen = 999)

# Assuming your data frame is called 'wbnew'
wbnew <- wbnew %>%
  mutate(value = as.numeric(value))
# Clean the year column by removing non-numeric characters
wbnew$year <- as.numeric(gsub("[^0-9]", "", wbnew$year))

# Convert the "value" column to numeric
wbnew$value <- as.numeric(wbnew$value)

ui <- fluidPage(
  titlePanel("wbDATA"),
  
  # Create a select input for choosing multiple countries
  selectInput(inputId = "country",
              label = "Select Countries",
              choices = unique(wbnew$country),
              selected = unique(wbnew$country)[1],
              multiple = TRUE),  # Allow multiple selections,
  
  # Create a select input for choosing the y-axis variable
  selectInput(inputId = "y_variable",
              label = "Select Y-Axis Variable",
              choices = unique(wbnew$serie),
              selected = unique(wbnew$serie)[1]),
  
  # Create a slider input for selecting the year range
  sliderInput(inputId = "year_range",
              label = "Select Year Range",
              min = min(wbnew$year, na.rm = TRUE),  # Use the minimum year from your dataset
              max = max(wbnew$year, na.rm = TRUE),  # Use the maximum year from your dataset
              value = c(min(wbnew$year, na.rm = TRUE), max(wbnew$year, na.rm = TRUE)),  # Set default range
              step = 1,
              pre = "",  # Add an empty prefix
              post = "",  # Add an empty suffix
              animate = FALSE,  # Allow animation for slider
              sep = ""  # Remove thousands separator
  ),
  
  # Create a plot output
  plotlyOutput(outputId = "wbnewPlot", height = "50vw")
)

server <- function(input, output) {
  # Define a reactive expression for filtered data based on the selected countries, year, and y-variable
  filteredData <- reactive({
    subset(wbnew, country %in% input$country & serie == input$y_variable & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  # Render a plotly plot based on the selected data
  output$wbnewPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = year, y = value, color = country, group = country)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      labs(x = "", y = input$y_variable, color = "")
    
    # Adjust the x-axis labels to display as whole numbers
    p <- p + scale_x_continuous(breaks = seq(min(filteredData()$year), max(filteredData()$year), by = 5)) +
      ylim(0, max(filteredData()$value))
    
    # Convert the ggplot object to plotly
    p <- ggplotly(p, tooltip = c("value", "year"))
    
    p
  })
  
  # Generate the dynamic headline
  #output$graph_headline <- renderText({
  # Create a headline based on selected "serie" and "country"
  # paste("", input$y_variable, "in", paste(input$country, collapse = ", "))
  #})
}

shinyApp(ui, server)
