library(shiny)
library(shinydashboard)
library(leaflet)
library(here)
library(dplyr)
library(vegan)


# UI 
ui <- dashboardPage(
  dashboardHeader(title = "Biodiversity Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Moorea Coral Reef LTER", tabName = "example", icon = icon("lightbulb")),
      menuItem("Moorea LTER Site Map", tabName = "map", icon = icon("globe-americas")),
      menuItem("Biodiversity Measures", tabName = "biodiversity", icon = icon("leaf")),
      menuItem("Taxonomic Breakdown", tabName = "taxonomic", icon = icon("list")),
      menuItem("Time Series Analysis", tabName = "time_series", icon = icon("calendar-alt")), 
      menuItem("Data Input", tabName = "data_input", icon = icon("upload")),
      menuItem("User Guidance", tabName = "guidance", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", fluidRow(
        column(12, 
               h3("Welcome to Biodiversity Explorer!"),
               p("This application allows you to explore biodiversity data with various analysis tools."),
               p("The example dataset used here is the Moorrea Coral Reef LTER Fish data, collected by A. Brooks. The dataset is a part of the MCR LTER Coral Reef Long-term Population and Community Dynamics study, ongoing since 2005."),
               p("Reference: Moorea Coral Reef LTER and A. Brooks. 2023. MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Fishes, ongoing since 2005 ver 62. Environmental Data Initiative. https://doi.org/10.6073/pasta/75644add7e7f90c568bf5045264d359a (Accessed 2024-02-17)."),
               p("The example dataset showcases the various functions of the app, including biodiversity measures such as Shannon's index and more."),
               p("It includes a time series analysis and a map visualization for the Moorea data, as well as taxonomic breakdown."),
               p("You can also upload your own biodiversity data, and the app will run biodiversity indices for you."),
               p("Additionally, you can perform a time series analysis on a given measure of your data.")
        )
      )),
      tabItem(tabName = "data_input", fluidRow(
        column(12, 
               h3("Data Input"),
               fileInput("file", "Upload Data", accept = c(".csv", ".txt"))
        )
      )),
      # Biodiversity Measures tab
      tabItem(tabName = "biodiversity", fluidRow(
        column(12, 
               h3("Biodiversity Measures"),
               fluidRow(
                 column(4, actionButton("shannon_button", "Calculate Shannon's Diversity Index")),
                 column(4, actionButton("simpson_button", "Calculate Simpson's Diversity Index")),
                 column(4, actionButton("other_button", "Calculate Other Index"))  # Add more buttons as needed
               ),
               br(),
               textOutput("biodiversity_info")
        )
      )),
      tabItem(tabName = "taxonomic", fluidRow(
        column(12, 
               h3("Taxonomic Breakdown"),
               dataTableOutput("taxonomic_table")
        )
      )),
      tabItem(tabName = "map", fluidRow(
        column(12, 
               h3("Moorea LTER Site Map Here"),
               leafletOutput("map")
        )
      )),
      tabItem(tabName = "time_series", fluidRow(
        column(12, 
               h3("Time Series Analysis"),
               actionButton("plot_button", "Plot Time Series"),  
               plotOutput("time_series_plot")
        )
      )),
      tabItem(tabName = "example", fluidRow(
        column(12, 
               h3("Moorea LTER Data"),
               tableOutput("example_table")
        )
      )),
      tabItem(tabName = "guidance", fluidRow(
        column(12, 
               h3("User Guidance"),
               p("This tab provides guidance on using the application.")
        )
      ))
    )
  )
)

server <- function(input, output, session) {
  
  # Define reactive expression to load the dataset
  dataset <- reactive({
    # Load the dataset specifically within the "example" tab
    read.csv(here::here("MCR_LTER_Annual_Fish_Survey_20230615.csv"))
  })
  
  # Reactive variable to track button (on/off)
  plot_toggle <- reactiveVal(FALSE)
  
  # Shannon's Diversity Index calculation
  observeEvent(input$shannon_button, {
    output$biodiversity_info <- renderText({
      # Calculate Shannon's diversity index
      # Read dataset
      lter <- dataset()
      # Drop NA values in the count column
      lter <- lter[!is.na(lter$Count),]
      # Summarize counts by species
      species_counts <- lter %>%
        group_by(Taxonomy) %>%
        summarize(total_count = sum(Count))
      # Calculate Shannon's diversity index directly from counts
      shannons_index <- diversity(species_counts$total_count, index = "shannon")
      # Print Shannon's diversity index
      shannons_index
    })
  })
  
  # Simpson's Diversity Index calculation
  observeEvent(input$simpson_button, {
    output$biodiversity_info <- renderText({
      # Calculate Simpson's diversity index
      # Read dataset
      lter2 <- dataset()
      # Drop NA values in the count column
      lter2 <- lter2[!is.na(lter2$Count),]
      # Summarize counts by species
      species_counts <- lter2 %>%
        group_by(Taxonomy) %>%
        summarize(total_count = sum(Count))
      # Calculate Simpson's diversity index directly from counts
      simpsons_index <- diversity(species_counts$total_count, index = "simpson")
      # Print Simpson's diversity index
      simpsons_index
    })
  })
  
  # Other Biodiversity Index (Berger-Parker) calculation
  observeEvent(input$other_button, {
    output$biodiversity_info <- renderText({
      # Calculate Berger-Parker index
      # Read dataset
      lter3 <- dataset()
      # Drop NA values in the count column
      lter3 <- lter3[!is.na(lter3$Count),]
      # Summarize counts by species
      species_counts <- lter3 %>%
        group_by(Taxonomy) %>%
        summarize(total_count = sum(Count))
      # Calculate Berger-Parker index directly from counts
      berger_parker_index <- max(species_counts$total_count) / sum(species_counts$total_count)
      # Print Berger-Parker index
      berger_parker_index
    })
  })
  
  
  # Taxonomic breakdown using the dataset
  output$taxonomic_table <- renderDataTable({
    # Access the dataset using the reactive expression
    data <- dataset()
    
    # Group the data by 'Taxonomy' and summarize to get total count for each species
    taxonomic_breakdown <- data %>%
      group_by(Taxonomy) %>%
      summarize(Total_Count = sum(Count))
    
    # Return the taxonomic breakdown data frame
    taxonomic_breakdown
  })
  
  # Map 
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -122.43, lat = 37.77, zoom = 12)
  })
  
  # Time series analysis using the dataset
  output$time_series_plot <- renderPlot({
    req(plot_toggle())  # Wait for the button to be toggled on
    
    data <- dataset()  # Access dataset with reactive expression
    
    # Convert year to numeric to make sure
    data$Year <- as.numeric(data$Year)
    
    # Count the number of unique species for each year
    species_count <- aggregate(Taxonomy ~ Year, data = data, FUN = function(x) length(unique(x)))
    
    # Plot the species richness over time
    plot(species_count$Year, species_count$Taxonomy, type = "l", 
         xlab = "Year", ylab = "Species Richness",
         main = "Species Richness Over Time")
  })
  
  # Toggle button event handler
  observeEvent(input$plot_button, {
    # Toggle the state of the button
    plot_toggle(!plot_toggle())
  })
  
  # Display the example Moorea dataset within the "Example" tab
  output$example_table <- renderTable({
    # Display first few rows of the dataset
    head(dataset())
  })
}

# Run the app
shinyApp(ui = ui, server = server)