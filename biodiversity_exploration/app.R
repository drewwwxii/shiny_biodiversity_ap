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
               fileInput("file", "Upload Data", accept = c(".csv", ".txt")),
               actionButton("generate_taxonomic", "Generate Taxonomic Breakdown"),
               actionButton("generate_shannon", "Calculate Shannon's Index"),
               actionButton("generate_simpson", "Calculate Simpson's Index"),
               actionButton("generate_berger_parker", "Calculate Berger-Parker Index"),
               br(),
               textOutput("uploaded_biodiversity_info"), # Display biodiversity info here
               dataTableOutput("uploaded_taxonomic_table") # Display taxonomic breakdown here
        )
      )),
      # UI modification
      tabItem(tabName = "biodiversity", fluidRow(
        column(12, 
               h3("Biodiversity Measures"),
               fluidRow(
                 column(4, actionButton("shannon_button", "Calculate Shannon's Diversity Index")),
                 column(4, actionButton("simpson_button", "Calculate Simpson's Diversity Index")),
                 column(4, actionButton("berger_button", "Calculate Berger-Parker Index"))
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

# Server logic
server <- function(input, output, session) {
  
options(shiny.maxRequestSize = 20 * 1024^2) # Set maximum request size to 20 MB (20 * 1024^2 bytes)
  
  
# Reactive expression to read the uploaded dataset
uploaded_dataset <- reactive({
  req(input$file)  # Require a file to be uploaded
  
  # Read the uploaded file
  read.csv(input$file$datapath)
})

observeEvent(input$generate_shannon, {
# Uploaded Shannon's Diversity Index calculation
  output$uploaded_biodiversity_info <- renderText({
    
    uploaded_lter <- uploaded_dataset()
    # Drop NA values in the count column
    uploaded_lter <- uploaded_lter[!is.na(uploaded_lter$Count),]
    # Summarize counts by species
    uploaded_species_counts <- uploaded_lter %>%
      group_by(Taxonomy) %>%
      summarize(total_count = sum(Count))
    # Shannon's diversity index directly from counts
    uploaded_shannons_index <- diversity(uploaded_species_counts$total_count, index = "shannon")
    paste("Shannon's diversity index:", uploaded_shannons_index)
  })
})
  
  observeEvent(input$generate_simpson, {
# Uploaded Simpson's Diversity Index calculation
    output$uploaded_biodiversity_info <- renderText({
    uploaded_lter2 <- uploaded_dataset()
    # Drop NA values in the count column
    uploaded_lter2 <- uploaded_lter2[!is.na(uploaded_lter2$Count),]
    # Summarize counts by species
    uploaded_species_counts2 <- uploaded_lter2 %>%
      group_by(Taxonomy) %>%
      summarize(total_count = sum(Count))
    # Simpson's diversity index directly from counts
    uploaded_simpsons_index <- diversity(uploaded_species_counts2$total_count, index = "simpson")
    # Print 
    paste("Simpson's diversity index:", uploaded_simpsons_index)
})
  })
  
# Uploaded Berger-Parker index calculation
  observeEvent(input$generate_berger_parker, {
    output$uploaded_biodiversity_info <- renderText({
    uploaded_lter3 <- uploaded_dataset()
    # Drop NA values in the count column
    uploaded_lter3 <- uploaded_lter3[!is.na(uploaded_lter3$Count),]
    # Summarize counts by species
    uploaded_species_counts3 <- uploaded_lter3 %>%
      group_by(Taxonomy) %>%
      summarize(total_count = sum(Count))
    # Calculate Berger-Parker index
    uploaded_berger_parker_index <- max(uploaded_species_counts3$total_count) / sum(uploaded_species_counts3$total_count)
    paste("Berger-Parker index:", uploaded_berger_parker_index)
  })
  })

# Taxonomic breakdown using the uploaded dataset
observeEvent(input$generate_taxonomic, {
output$uploaded_taxonomic_table <- renderDataTable({
  req(uploaded_dataset())  # Ensure dataset is available
  
  data <- uploaded_dataset()  # Access the uploaded dataset
  
  # Group the data by 'Taxonomy' and summarize to get total count for each species
  taxonomic_breakdown <- data %>%
    group_by(Taxonomy) %>%
    summarize(Total_Count = sum(Count))
  
  # Return the taxonomic breakdown data frame
  taxonomic_breakdown
})
})
  
  # Define reactive expression to load the original dataset
  original_dataset <- reactive({
    # Load the dataset
    read.csv(here::here("MCR_LTER_Annual_Fish_Survey_20230615.csv"))
  })
  
  
  # Shannon's Diversity Index calculation
  observeEvent(input$shannon_button, {
    output$biodiversity_info <- renderText({
  
      lter <- original_dataset()
      # Drop NA values in the count column
      lter <- lter[!is.na(lter$Count),]
      # Summarize counts by species
      species_counts <- lter %>%
        group_by(Taxonomy) %>%
        summarize(total_count = sum(Count))
      # Shannon's diversity index directly from counts
      shannons_index <- diversity(species_counts$total_count, index = "shannon")
      # Print
      shannons_index
    })
  })
  
  # Simpson's Diversity Index calculation
  observeEvent(input$simpson_button, {
    output$biodiversity_info <- renderText({
      lter2 <- original_dataset()
      # Drop NA values in the count column
      lter2 <- lter2[!is.na(lter2$Count),]
      # Summarize counts by species
      species_counts <- lter2 %>%
        group_by(Taxonomy) %>%
        summarize(total_count = sum(Count))
      # Calculate Simpson's diversity index directly from counts
      simpsons_index <- diversity(species_counts$total_count, index = "simpson")
      # Print 
      simpsons_index
    })
  })
  
  # Berger-Parker index calculation
  observeEvent(input$berger_button, {
    output$biodiversity_info <- renderText({
      lter3 <- original_dataset()
      # Drop NA values in the count column
      lter3 <- lter3[!is.na(lter3$Count),]
      # Summarize counts by species
      species_counts <- lter3 %>%
        group_by(Taxonomy) %>%
        summarize(total_count = sum(Count))
      # Calculate Berger-Parker index directly from counts
      berger_parker_index <- max(species_counts$total_count) / sum(species_counts$total_count)
      # Print 
      berger_parker_index
    })
  })
  
  
  # Taxonomic breakdown using the dataset
  output$taxonomic_table <- renderDataTable({
    
    tax_data <- original_dataset()
    
    # Group the data by 'Taxonomy' and summarize to get total count for each species
    taxonomic_breakdown <- tax_data %>%
      group_by(Taxonomy) %>%
      summarize(Total_Count = sum(Count))
    
    # Return the taxonomic breakdown data frame
    taxonomic_breakdown
  })
  
  # Map 
  output$map <- renderLeaflet({
    leaflet() %>%
      # Set initial map view to Moorea, French Polynesia
      setView(lng = -149.8366, lat = -17.5364, zoom = 11) %>%
      addTiles() %>%
      # Add marker for Moorea
      addMarkers(lng = -149.8366, lat = -17.5364, popup = "Moorea, French Polynesia") %>%
      # Add polygons for square transects
      addPolygons(
        lng = c(-149.67, -150.0, -150.0, -149.67, -149.67), # East to West
        lat = c(-17.45, -17.45, -17.62, -17.62, -17.45), # South to North
      ) %>%
      addPolygons(
        lng = c(-149.8455917, -149.829821, -149.829821, -149.8455917, -149.8455917), # East to West
        lat = c(-17.47185366, -17.47185366, -17.48641792, -17.48641792, -17.47185366), # South to North
      ) %>%
      addPolygons(
        lng = c(-149.8116849, -149.7961685, -149.7961685, -149.8116849, -149.8116849), # East to West
        lat = c(-17.46576169, -17.46576169, -17.48131958, -17.48131958, -17.46576169), # South to North
      ) %>%
      addPolygons(
        lng = c(-149.7708619, -149.7519968, -149.7519968, -149.7708619, -149.7708619), # East to West
        lat = c(-17.50382025, -17.50382025, -17.52087158, -17.52087158, -17.50382025), # South to North
      ) %>%
      addPolygons(
        lng = c(-149.7772857, -149.7566866, -149.7566866, -149.7772857, -149.7772857), # East to West
        lat = c(-17.53305021, -17.53305021, -17.55064263, -17.55064263, -17.53305021), # South to North
      ) %>%
      addPolygons(
        lng = c(-149.8869755, -149.8561009, -149.8561009, -149.8869755, -149.8869755), # East to West
        lat = c(-17.56818162, -17.56818162, -17.59182383, -17.59182383, -17.56818162), # South to North
      ) %>%
      addPolygons(
        lng = c(-149.934537, -149.9115336, -149.9115336, -149.934537, -149.934537), # East to West
        lat = c(-17.50735955, -17.50735955, -17.52839766, -17.52839766, -17.50735955), # South to North
      )
  })
  
  # Time series analysis using the dataset
  
  # Reactive variable to track button (on/off)
  plot_toggle <- reactiveVal(FALSE)
  
  output$time_series_plot <- renderPlot({
    req(plot_toggle())  # Wait for the button to be toggled on
    
    time_data <- original_dataset()  # Access dataset with reactive expression
    
    # Convert year to numeric to make sure
    time_data$Year <- as.numeric(time_data$Year)
    
    # Count the number of unique species for each year
    species_count <- aggregate(Taxonomy ~ Year, data = time_data, FUN = function(x) length(unique(x)))
    
    # Plot the species richness over time
    plot(species_count$Year, species_count$Taxonomy, type = "l", 
         xlab = "Year", ylab = "Species Richness",
         main = "Species Richness Over Time")
  })
  
  # Toggle button event 
  observeEvent(input$plot_button, {
    # Toggle the state of the button
    plot_toggle(!plot_toggle())
  })
  
  # Display the example Moorea dataset within the Example tab
  output$example_table <- renderTable({
    # Display first few rows
    head(original_dataset())
  })
}

# Run the app
shinyApp(ui = ui, server = server)