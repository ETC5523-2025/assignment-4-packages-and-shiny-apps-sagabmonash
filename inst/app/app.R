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
  includeCSS("www/styles.css"),
  titlePanel("NEON Water Quality Explorer"),
  
  
  # Use a sidebar layout
  sidebarLayout(
    sidebarPanel(
      bslib::card(
        bslib::card_header("Controls & Info"),
        bslib::card_body(
          h4("Instructions"),
          p("Use these controls to filter the data. The plots and table will update automatically."),
          
          # Input 1: Site Selector
          selectizeInput(inputId = "siteSelect", label = "Select Site(s):",
                         choices = unique(neonWaterQuality::neon_water_quality$siteName),
                         selected = "Arikaree River", multiple = TRUE),
          helpText("Choose one or more NEON sites (Arikaree River, Caribou Creek, Lewis Run) to display."), # Description for siteSelect
          
          # Input 2: Date Selector
          dateRangeInput(inputId = "dateSelect",
                         label = "Select Date Range:", start = min(neonWaterQuality::neon_water_quality$datetime),
                         end = max(neonWaterQuality::neon_water_quality$datetime)),
          hr(), # Added separator for clarity
          h5("About the Data"),
          p("This app visualizes high-frequency water quality data from Kermorvant et al. (2023).")
        )
      ) # End of bslib::card
    ), # End of sidebarPanel - THIS IS THE KEY FIX
    
    mainPanel( # Start of mainPanel as the second argument
      tabsetPanel(
        type = "tabs",
        
        # Tab 1: Nitrate vs. Time
        tabPanel("Nitrate vs. Time",
                 h4("Nitrate Concentration over Time"),
                 p("This plot shows the 15-minute nitrate measurements (µmol/L) 
                   for the selected sites and date range."), # What it shows
                 p(strong("Interpretation:")), # Add interpretation heading
                 p("Observe the overall levels, daily fluctuations 
                   (diel patterns), seasonal trends, and any potential event-driven spikes 
                   (e.g., after rainfall, though precipitation data isn't shown here) in nitrate concentration. 
                   Compare patterns across different sites if multiple are selected."),
                 plotlyOutput("nitratePlot")),
        
        # Tab 2: Variable Relationships
        tabPanel("Variable Relationships",
                 h4("Explore Relationships Between Variables"),
                 p("This scatter plot shows the relationship between two selected water quality variables 
                   for the chosen sites and dates. Use the dropdown menus below 
                   to change the variables on the X and Y axes."),
                 fluidRow(
                   column(6, selectInput("var_x", "X-Axis Variable:",
                                         choices = c("temperature", "dissolved_oxygen", "specific_conductance",
                                                     "log_turbidity", "elevation"), selected = "log_turbidity")),
                   column(6, selectInput("var_y", "Y-Axis Variable:",
                                         choices = c("nitrate"), selected = "nitrate"))
                 ),
                 p(strong("Interpretation:")), # Add interpretation heading
                 p("Look for patterns, correlations (positive or negative), 
                   or distinct clusters in the scatter plot. The relationship between variables 
                   (e.g., nitrate vs. specific conductance) can differ significantly between sites, 
                   reflecting different underlying environmental processes as discussed in Kermorvant et al. (2023)."),
                 plotlyOutput("scatterPlot")),
        tabPanel("Raw Data",
                 h4("Filtered Data Table"),
                 p("The raw data, based on your selections."),
                 DT::dataTableOutput("dataTable"))
      ) # End of tabsetPanel
    ) # End of mainPanel
  )) # End of sidebarLayout

# 2. THE SERVER (LOGIC)
server <- function(input, output, session) {
  
  # Load the package data ONCE
  data <- neonWaterQuality::neon_water_quality
  
  # Check if data loaded (add this temporarily for debugging)
  print(paste("Data dimensions:", paste(dim(data), collapse=" x ")))
  
  # Create a SHARED REACTIVE dataset
  filtered_data <- reactive({
    
    # Use req() to prevent errors before inputs are ready
    req(input$siteSelect, input$dateSelect)
    
    # Convert input Date objects to POSIXct datetimes
    # Start date should be the beginning of the selected day
    start_dt <- as.POSIXct(input$dateSelect[1], tz = "UTC")
    
    # End date should be the beginning of the *next* day
    # So we filter for datetime < end_dt
    end_dt <- as.POSIXct(input$dateSelect[2] + 1, tz = "UTC")
                           
    data %>%
      filter(
        siteName %in% input$siteSelect,
        datetime >= start_dt,
        datetime < end_dt
      )
  })
  
  # Output 1: Nitrate Time Series Plot
  output$nitratePlot <- renderPlotly({
    site_names <- paste(unique(filtered_data()$siteName), collapse = ", ")
    plot_title <- paste("Nitrate Concentration for", site_names)
    
    plot_ly(
      filtered_data(),
      x = ~datetime,
      y = ~nitrate,
      color = ~siteName,
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(
        title = plot_title,
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
    # Add another check here (add this temporarily)
    df_to_render <- filtered_data()
    print(paste("Filtered data dimensions:", paste(dim(df_to_render), collapse=" x ")))
    df_to_render
  })
  
}

# 3. RUN THE APP
shinyApp(ui, server)