# Load all required packages
library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)

# 1. THE USER INTERFACE (UI)
# We use bslib for styling
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera"),
  
  titlePanel("NEON Water Quality Explorer"),
  
  # Use a sidebar layout
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      p("Use these controls to filter the data. The plots and table will update automatically."),
      
      # Interactive Input 1: Select sites
      selectizeInput(
        inputId = "siteSelect",
        label = "Select Site(s):",
        # We load the data here just to get the site names
        choices = unique(neonWaterQuality::neon_water_quality$siteName),
        selected = "Arikaree River",
        multiple = TRUE
      ),
      
      # Interactive Input 2: Select date range
      dateRangeInput(
        inputId = "dateSelect",
        label = "Select Date Range:",
        start = min(neonWaterQuality::neon_water_quality$datetime),
        end = max(neonWaterQuality::neon_water_quality$datetime)
      ),
      
      h5("About the Data"),
      p("This app visualizes high-frequency water quality data from Kermorvant et al. (2023).")
    ),
    
    mainPanel(
      # We use tabs to organize the output
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Nitrate vs. Time",
          h4("Nitrate Concentration over Time"),
          p("Shows the 15-minute nitrate measurements for the selected sites."),
          plotlyOutput("nitratePlot") # Interactive plot
        ),
        tabPanel(
          "Variable Relationships",
          h4("Explore Relationships"),
          p("Select two variables to see how they relate to each other."),
          
          # More interactivity
          fluidRow(
            column(6, selectInput("var_x", "X-Axis Variable:",
                                  choices = c("temperature", "dissolved_oxygen", "specific_conductance", "log_turbidity", "elevation"),
                                  selected = "log_turbidity")),
            column(6, selectInput("var_y", "Y-Axis Variable:",
                                  choices = c("nitrate"),
                                  selected = "nitrate"))
          ),
          plotlyOutput("scatterPlot") # Interactive plot
        ),
        tabPanel(
          "Raw Data",
          h4("Filtered Data Table"),
          p("The raw data, based on your selections."),
          DT::dataTableOutput("dataTable") # Interactive table
        )
      )
    )
  )
)

# 2. THE SERVER (LOGIC)
server <- function(input, output, session) {
  
  # Load the package data ONCE
  data <- neonWaterQuality::neon_water_quality
  
  # Create a SHARED REACTIVE dataset
  # This filters the data based on user input and is fast because it only re-runs when an input changes.
  filtered_data <- reactive({
    
    # Use req() to prevent errors before inputs are ready
    req(input$siteSelect, input$dateSelect)
    
    data %>%
      filter(
        siteName %in% input$siteSelect,
        datetime >= input$dateSelect[1],
        datetime <= input$dateSelect[2]
      )
  })
  
  # Output 1: Nitrate Time Series Plot
  output$nitratePlot <- renderPlotly({
    plot_ly(
      filtered_data(),
      x = ~datetime,
      y = ~nitrate,
      color = ~siteName,
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(
        title = "Nitrate Concentration",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Nitrate (µmol/L)")
      )
  })
  
  # Output 2: Scatter Plot
  output$scatterPlot <- renderPlotly({
    plot_ly(
      filtered_data(),
      # Use .data[[]] to access inputs as column names
      x = ~.data[[input$var_x]],
      y = ~.data[[input$var_y]],
      color = ~siteName,
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(
        title = paste(input$var_y, "vs.", input$var_x),
        xaxis = list(title = input$var_x),
        yaxis = list(title = input$var_y)
      )
  })
  
  # Output 3: Data Table
  output$dataTable <- DT::renderDataTable({
    filtered_data()
  })
  
}

# 3. RUN THE APP
shinyApp(ui, server)