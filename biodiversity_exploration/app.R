library(shiny)
library(shinydashboard)
library(leaflet)  # Add this line

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Biodiversity Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Morrea Coral Reef LTER", tabName = "example", icon = icon("lightbulb")),
      menuItem("Morrea LTER Site Map", tabName = "map", icon = icon("globe-americas")),
      menuItem("Biodiversity Measures", tabName = "biodiversity", icon = icon("leaf")),
      menuItem("Taxonomic Breakdown", tabName = "taxonomic", icon = icon("list")),
      menuItem("Time Series Analysis", tabName = "time_series", icon = icon("calendar-alt")), 
      menuItem("Data Input", tabName = "data_input", icon = icon("upload")),
      menuItem("User Guidance", tabName = "guidance", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              fluidRow(
                column(12, h3("Welcome to Biodiversity Explorer!")),
                p("This application allows you to explore biodiversity data with various analysis tools."),
                p("The example dataset used here is the Morrea Coral Reef LTER Fish data, collected by A. Brooks. The dataset is a part of the MCR LTER Coral Reef Long-term Population and Community Dynamics study, ongoing since 2005."),
                p("Reference: Moorea Coral Reef LTER and A. Brooks. 2023. MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Fishes, ongoing since 2005 ver 62. Environmental Data Initiative. https://doi.org/10.6073/pasta/75644add7e7f90c568bf5045264d359a (Accessed 2024-02-17)."),
                p("The example dataset showcases the various functions of the app, including biodiversity measures such as Shannon's index and more."),
                p("It includes a time series analysis and a map visualization for the Morrea data, as well as taxonomic breakdown."),
                p("You can also upload your own biodiversity data, and the app will run biodiversity indices for you."),
                p("Additionally, you can perform a time series analysis on a given measure of your data.")
              )
      ),
      
      # Data Input Tab
      tabItem(tabName = "data_input",
              # Your data input widgets go here
              fluidRow(
                column(12, h3("Data Input")),
                fileInput("file", "Upload Data", accept = c(".csv", ".txt"))
              )
      ),
      
      
      # Biodiversity Measures Tab
      tabItem(tabName = "biodiversity",
              # Your biodiversity measures widgets go here
              fluidRow(
                column(12, h3("Biodiversity Measures")),
                textOutput("biodiversity_info")
              )
      ),
      
      # Taxonomic Breakdown Tab
      tabItem(tabName = "taxonomic",
              # Your taxonomic breakdown widgets go here
              fluidRow(
                column(12, h3("Taxonomic Breakdown")),
                dataTableOutput("taxonomic_table")
              )
      ),
      
      # Morrea LTER Map
      tabItem(tabName = "map",
              fluidRow(
                column(12, h3("Morrea LTER Site Map Here")),
                leafletOutput("map")
              )
      ),
      
      # Time Series Analysis Tab
      tabItem(tabName = "time_series",
              # Your time series analysis widgets go here
              fluidRow(
                column(12, h3("Time Series Analysis")),
                plotOutput("time_series_plot")
              )
      ),
      
      # Example Dataset Tab
      tabItem(tabName = "example",
              # Your example dataset widgets go here
              fluidRow(
                column(12, h3("Morrea LTER Data Here")),
                tableOutput("example_table")
              )
      ),
      
      # User Guidance Tab
      tabItem(tabName = "guidance",
              # Your user guidance content goes here
              fluidRow(
                column(12, h3("User Guidance")),
                p("This tab provides guidance on using the application.")
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Your server logic goes here
  
  # Placeholder outputs for demonstration
  output$eda_plot <- renderPlot({
    plot(1:10, main = "Placeholder EDA Plot")
  })
  
  output$biodiversity_info <- renderText({
    "Placeholder Biodiversity Info"
  })
  
  output$taxonomic_table <- renderDataTable({
    data.frame(Species = letters[1:5], Count = 1:5)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -122.43, lat = 37.77, zoom = 12)
  })
  
  output$time_series_plot <- renderPlot({
    plot(1:10, main = "Placeholder Time Series Plot")
  })
  
  output$example_table <- renderTable({
    data.frame(Example_Column = letters[1:5], Another_Column = 1:5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
