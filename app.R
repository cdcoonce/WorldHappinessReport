
library(plotly)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)
library(readr)
library(tidyr)
library(rlang)
library(viridis)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)


  ##Read Data Set
Yearly_data <- read.csv("Yearly_data.csv")

  ## Define a function to normalize data to a 0-10 scale
normalize_to_10 <- function(x) {
  
  10 * (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
}

  ## Apply the normalization function to all columns except 'Country name' and 'year'
Yearly_data <- Yearly_data %>%
  mutate(across(-c(Country.name, year), normalize_to_10))

  ## Create lists for display names.
new_names <- c("Life.Ladder" = "Life Ladder",
                "Log.GDP.per.capita" = "GDP Per Capita",
                "Social.support" = "Social Support",
                "Healthy.life.expectancy.at.birth" = "Healthy Life Expectancy",
                "Freedom.to.make.life.choices" = "Life Choice Freedom",
                "Generosity" = "Generosity",
                "Perceptions.of.corruption" = "Perception of Corruption",
                "Positive.affect" = "Positive Affect",
                "Negative.affect" = "Negative Affect")

re_name <- c("Life Ladder" = "Life.Ladder",
             "GDP Per Capita" = "Log.GDP.per.capita",
             "Social Support" = "Social.support",
             "Healthy Life Expectancy" = "Healthy.life.expectancy.at.birth",
             "Life Choice Freedom" = "Freedom.to.make.life.choices",
             "Generosity" = "Generosity",
             "Perceptions of Corruption" = "Perceptions.of.corruption",
             "Positive Affect" = "Positive.affect",
             "Negative Affect" = "Negative.affect")



ui <- dashboardPage(
  
  dashboardHeader(title = "World Happiness Report Trends"),
  
  dashboardSidebar( 
    
    # Add checkboxes for selecting which indicators to plot
    checkboxGroupInput("selected_indicators", "Select Indicators:",
                       choices = re_name,
                       selected = "Life.Ladder")
  ),
    
  dashboardBody(
    box(width = 12,
      title = "Options",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      
      column( width = 3,
      
          ## Drop-down menu to select a country
        selectizeInput("selected_country", 
                       "Choose Countries:", 
                       choices = unique(Yearly_data$Country.name),
                       multiple = TRUE,
                       selected = c("United States", "Sweden"))
      ),
      
      column( width = 3,
              
            ## Slider for selecting the year range
          sliderInput("yearRange", "Select Year Range:",
                      min = min(as.integer(Yearly_data$year)),
                      max = max(as.integer(Yearly_data$year)),
                      value = c(min(as.integer(Yearly_data$year)),
                                max(as.integer(Yearly_data$year))),
                      step = 1, sep = ""),
          
                
      ),
    
      column( width = 3,

            ## slider for selecting plot height
          sliderInput("plotHeight", 
                      "Plot Height (in pixels)", 
                      value = 500, min = 300, max = 700)

    ),
    ),
  
  box( width = 12,
       title = "Trends for Selected Countries and Indicators  (scale normalized to 0 - 10)",
       status = "primary",
       solidHeader = TRUE,
      ## Plot output
    plotOutput("happinessTrend", width = "100%", height = "auto")
  )
  ),
        
)
  
  
server <- function(input, output) {
  
    ## Used to properly select multiple indicators
  filtered_data <- reactive({
    
      ## Manipulating the data set to handle facet wrapping with multiple indicators
    long_data <- Yearly_data %>%
      filter(Country.name %in% input$selected_country,
             year >= input$yearRange[1],
             year <= input$yearRange[2]) %>%
      pivot_longer(cols = input$selected_indicators, names_to = "indicator", values_to = "value") %>%
      drop_na(value)
    
    long_data
    
  })
  
  
    ## Output Plot
  output$happinessTrend <- renderPlot({
    
    country_data <- filtered_data()
        ## Create color palette
      unique_countries <- unique(country_data$Country.name)
      color_palette <- viridis(length(unique_countries), option = "D")
      
        ## Initialize ggplot
      p <- ggplot(country_data, aes(x = year, y = value, 
                                    color = Country.name)) +
        geom_line() + 
        geom_point() + 
        theme_minimal() +
        theme(text = element_text(size = 16, face = "bold"),
              strip.text = element_text(size = 20, face = "bold"),
              axis.line = element_line(color = "grey", size = .25),
              axis.ticks = element_line(),
              panel.grid.major = element_line(size = .3),  
              panel.grid.minor.y = element_blank()) +
        scale_x_continuous(
          breaks = seq(min(input$yearRange), max(input$yearRange), by = 4),  # Change as needed
          limits = c(min(input$yearRange), max(input$yearRange)))
      
        ## Apply color scale and facet wrap
      p <- p + scale_color_manual(values = color_palette, name = "Country",) +
        facet_wrap(~factor(indicator, 
                            labels = new_names[names(new_names) %in% 
                            input$selected_indicators]), scales = "free_y") +
        labs(x = NULL, y = "Indicator Values")
      
      p
    
  }, 
  
  height = function() {
    
    input$plotHeight
    
  })
  
}

shinyApp(ui, server)
