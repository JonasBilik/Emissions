
#-----------------#
# 0. Settings ####
#-----------------#

rm(list = ls())

# packages
library(readxl)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(rsconnect)


# path
#path.data <- paste0(getwd(),"/app/")


#--------------------------------#
# 1. load & prepare datasers ####
#--------------------------------#

#data <- read_xlsx(paste0(path.data,"owid-co2-data.xlsx"))

data <- read_xlsx("owid-co2-data.xlsx")

data <- data %>%
  select(country, year, population, gdp, co2, co2_per_capita, cement_co2, coal_co2, gas_co2, oil_co2, 
         flaring_co2, cement_co2_per_capita, coal_co2_per_capita, co2_growth_abs,
         co2_growth_prct, share_global_co2, share_of_temperature_change_from_ghg) %>%
  filter(year >= 1950)



#------------------#
# 2. Shiny app ####
#------------------#

# UI
ui <- fluidPage(
  titlePanel("Enhanced CO₂ Emission Trends Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("countries", "Select Countries", choices = unique(data$country), multiple = TRUE, 
                     selected = "Germany", options = list(placeholder = 'Type to search...', maxItems = 10)),
      selectInput("emission_type", "Emission Type", 
                  choices = c("Total CO2" = "co2", "CO2 per Capita" = "co2_per_capita", 
                              "Cement CO2" = "cement_co2", "Coal CO2" = "coal_co2", 
                              "Oil CO2" = "oil_co2", "Gas CO2" = "gas_co2")),
      sliderInput("year_range", "Year Range", min = 1950, max = 2020, value = c(1950, 2020), step = 1, sep = ""),
      checkboxInput("showTrend", "Show Trendline", value = TRUE),
      downloadButton("downloadData", "Download Data"),
      br(),
      tags$p("Data source: Our World in Data. Licensed under CC BY 4.0.", 
             style = "font-size: 0.8em; color: grey;")
    ),
    mainPanel(
      plotlyOutput("emissionPlot"),
      div(style = "margin-top: 10px; color: #2C3E50;",
          textOutput("emissionExplanation"),
          p("Note: The trendline represents the linear regression of emissions over the selected period.", style = "font-size: 0.9em; color: grey;")
      ),
      h4("Key Metrics"),
      div(style = "display: flex; gap: 15px;",
          verbatimTextOutput("avgEmissions"),
          verbatimTextOutput("minEmissions"),
          verbatimTextOutput("maxEmissions")
      ),
      DTOutput("dataTable", height = "300px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive Expression to filter data based on user inputs
  filtered_data <- reactive({
    data %>%
      filter(country %in% input$countries,
             year >= input$year_range[1],
             year <= input$year_range[2]) %>%
      mutate(year = as.integer(year)) %>%
      arrange(country, year) %>%
      select(Year = year, Country = country, `Emission value` = all_of(input$emission_type))
  })
  
  # Plot with optional trendline and improved color scheme
  output$emissionPlot <- renderPlotly({
    ggplot_object <- ggplot(filtered_data(), aes(x = Year, y = `Emission value`, color = Country)) +
      geom_line() +
      scale_color_viridis_d() +
      labs(title = "Emission Trends", x = "Year", y = "Emissions") +
      theme_minimal()
    
    if (input$showTrend) {
      ggplot_object <- ggplot_object + 
        geom_smooth(aes(x = Year, y = `Emission value`), method = "lm", se = FALSE, linetype = "dashed", color = "blue", inherit.aes = FALSE)
    }
    
    ggplotly(ggplot_object, tooltip = c("Year", "Emission value", "Country")) %>%
      layout(legend = list(title = list(text = "Legend"))) %>%
      style(texttemplate = "Year: %{x}<br>Emission value: %{y:.2f}<br>Country: %{text}")
  })
  
  # Dynamische Erklärung für den Emissionstyp
  output$emissionExplanation <- renderText({
    explanation <- switch(input$emission_type,
                          "co2" = "Total CO₂ emissions in million tons.",
                          "co2_per_capita" = "CO₂ emissions per capita in tons.",
                          "cement_co2" = "CO₂ emissions from cement production in million tons.",
                          "coal_co2" = "CO₂ emissions from coal use in million tons.",
                          "oil_co2" = "CO₂ emissions from oil use in million tons.",
                          "gas_co2" = "CO₂ emissions from natural gas in million tons.")
    paste("Explanation:", explanation)
  })
  
  # Key metrics: Average, minimum, and maximum emissions
  output$avgEmissions <- renderText({
    avg_emissions <- mean(filtered_data()$`Emission value`, na.rm = TRUE)
    paste("Average Emissions:", round(avg_emissions, 2))
  })
  
  output$minEmissions <- renderText({
    min_emissions <- min(filtered_data()$`Emission value`, na.rm = TRUE)
    paste("Minimum Emissions:", round(min_emissions, 2))
  })
  
  output$maxEmissions <- renderText({
    max_emissions <- max(filtered_data()$`Emission value`, na.rm = TRUE)
    paste("Maximum Emissions:", round(max_emissions, 2))
  })
  
  # Data Table with fixed height
  output$dataTable <- renderDT({
    datatable(filtered_data() %>% setNames(c("Year", "Country", "Emission value")),
              options = list(
                pageLength = 10,
                dom = 'tip',
                autoWidth = TRUE,
                rownames = FALSE,
                stripeClasses = c('table-striped', 'table-hover')
              )) %>%
      formatRound(columns = 'Emission value', digits = 2)
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() { paste("emission_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}



# Run the Shiny app
shinyApp(ui = ui, server = server)

rsconnect::deployApp(appDir = "C:/GIT/Emissions/app", appName = "emissions_dashboard")
