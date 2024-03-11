library(shiny)
library(shinydashboard)
library(leaflet)
library(here)
library(dplyr)
library(vegan)
library(tidyverse)


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
               helpText("Upload your biodiversity data file and perform various analyses."),
               fileInput("file", "Upload Data", accept = c(".csv", ".txt")),
               actionButton("generate_taxonomic", "Generate Taxonomic Breakdown"),
               actionButton("generate_shannon", "Calculate Shannon's Index"),
               actionButton("generate_simpson", "Calculate Simpson's Index"),
               actionButton("generate_berger_parker", "Calculate Berger-Parker Index"),
               br(),
               textOutput("uploaded_biodiversity_info"), # Display uploaded biodiversity info here
               dataTableOutput("uploaded_taxonomic_table") # Display uploaded taxonomic breakdown here
        )
      )),
      # UI modification
      tabItem(tabName = "biodiversity", fluidRow(
        column(12, 
               h3("Biodiversity Measures"),
               helpText("Calculate various biodiversity measures using the example data."),
               checkboxGroupInput("site_selection", "Select Site(s):", choices = NULL),
               actionButton("calculate_biodiversity", "Calculate Biodiversity Measures"),
               br(),
               dataTableOutput("biodiversity_table")
        )
      )),
      tabItem(tabName = "taxonomic", fluidRow(
        column(12, 
               h3("Taxonomic Breakdown"),
               helpText("Generate a taxonomic breakdown table from the example dataset."),
               checkboxGroupInput("site_selector", "Select Sites", choices = NULL),
               dataTableOutput("taxonomic_table")
        )
      )),
      tabItem(tabName = "map", fluidRow(
        column(12, 
               h3("Moorea LTER Site Map Here"),
               helpText("A site map of the locations of the various sites that data was collected from."),
               leafletOutput("map")
        )
      )),
      tabItem(tabName = "time_series", fluidRow(
        column(12, 
               h3("Time Series Analysis"),
               helpText("Generate a time series analysis of species richness over time from the example dataset."),
               actionButton("plot_button", "Plot Time Series"),  
               plotOutput("time_series_plot")
        )
      )),
      tabItem(tabName = "example", fluidRow(
        column(12, 
               h3("Moorea LTER Data"),
               helpText("A preview of what the example dataset contains."), 
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
  
options(shiny.maxRequestSize = 20 * 1024^2) # Set maximum request size to 20 MB 
  
  
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
  uploaded_taxonomic_breakdown <- data %>%
    group_by(Taxonomy) %>%
    summarize(Total_Count = sum(Count))
  
  # Return the taxonomic breakdown data frame
  uploaded_taxonomic_breakdown
})
})
  
  # Define reactive expression to load the original dataset
  original_dataset <- reactive({
    # Load the dataset
    read.csv(here::here("MCR_LTER_Annual_Fish_Survey_20230615.csv"))
  })
  
  
  observe({
    # Filter out NA values from site choices
    site_choices <- unique(original_dataset()$Site)
    site_choices <- site_choices[!is.na(site_choices)]
    updateCheckboxGroupInput(session, "site_selection", choices = site_choices)
  })
  
  observeEvent(input$calculate_biodiversity, {
    output$biodiversity_table <- renderDataTable({
      lter <- original_dataset()
      
      # Filter the dataset based on selected site(s)
      selected_sites <- input$site_selection
      if (!is.null(selected_sites)) {
        lter <- lter[lter$Site %in% selected_sites, ]
      }
      
      # Drop NA values in the count column
      lter <- lter[!is.na(lter$Count),]
      # Summarize counts by species
      species_counts <- lter %>%
        group_by(Taxonomy) %>%
        summarize(total_count = sum(Count))
      
      # Calculate all three biodiversity measures
      shannons_index <- diversity(species_counts$total_count, index = "shannon")
      simpsons_index <- diversity(species_counts$total_count, index = "simpson")
      berger_parker_index <- max(species_counts$total_count) / sum(species_counts$total_count)
      
      # Create a data frame to store the results
      biodiversity_data <- data.frame(
        Measure = c("Shannon's Diversity Index", "Simpson's Diversity Index", "Berger-Parker Index"),
        Value = c(shannons_index, simpsons_index, berger_parker_index)
      )
      
      # Return the data frame
      biodiversity_data
    })
  })
  
  observe({
    # Get unique site names from the original dataset and remove NA values
    site_choices <- na.omit(unique(original_dataset()$Site))
    
    # Update checkbox input choices
    updateCheckboxGroupInput(session, "site_selector", choices = site_choices)
  })
  
  output$taxonomic_table <- renderDataTable({
    
    tax_data <- original_dataset()
    
    # Filter the data based on selected sites
    chosen_sites <- input$site_selector
    if (!is.null(chosen_sites) && length(chosen_sites) > 0) {
      tax_data <- tax_data %>% filter(Site %in% chosen_sites)
    } else {
      # If no sites are selected, don't filter the data
      tax_data <- tax_data
    }
    
    # Group the filtered data by 'Taxonomy' and summarize to get total count for each species
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
    ggplot(species_count, aes(x = Year, y = Taxonomy)) +
      geom_line() + # Add line plot
      labs(x = "Year", y = "Species Richness", caption = "Figure 1: Species richness over time") +
      theme_minimal()
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