# ==================================================
# Packages
# ==================================================
library(shiny)
library(tidyverse)      # for syntactic manipulation of tables
library(sf)             # provides classes and functions for vector data
library(rnaturalearth)  # map data sets from Natural Earth

# ==================================================
# Load and Prepare Data
# ==================================================
# Assuming 'storms' is a dataframe with columns: year, month, day, long, lat, wind, category, name, pressure
# You need to load your 'storms' data here. Below is an example line to load a dataset named 'storms' from R's datasets.
# data("storms", package = "dplyr")

# Add a new column to identify major hurricanes
storms <- storms %>%
  group_by(name) %>%
  mutate(is_major_hurricane = any(max(wind) > 96)) %>%
  ungroup()

# ==================================================
# Auxiliary objects (don't depend on input widgets)
# ==================================================
# world country polygons (from natural earth)
world_countries <- ne_countries(returnclass = "sf")

# ggplot object for North Atlantic map
atlantic_map <- ggplot() +
  geom_sf(data = world_countries, fill = "gray95") +
  coord_sf(xlim = c(-110, 0), ylim = c(5, 65)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(), # hide tick marks
        axis.text = element_blank()) + # hide degree values of lat & lon
  labs(x = "", y = "") # hide axis labels

# ===========================================================
# Define UI for application that graphs a map of storms
# ===========================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("North Atlantic Tropical Cyclones"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Select a Year:",
                  sep = "", # no separator between thousands places
                  min = 1975,
                  max = 2021,
                  value = 2000),
      sliderInput("wind_speed", "Select Minimum Wind Speed:", min = 0, max = 200, value = 0),
      checkboxInput("facet_month", "Facet by Month"),
      checkboxInput("major_hurricanes", "Show Major Hurricanes Only"),
      actionButton("toggle_legend", "Toggle Legend")
    ),
    
    # ----------------------------------------------------------
    # Main Panel with output: plot map of storms
    # ----------------------------------------------------------
    mainPanel(
      plotOutput(outputId = "plot_map"),
      hr(),
      dataTableOutput(outputId = "summary_table")
    )
  ) # closes sidebarLayout
) # closes fluidPage

# ======================================================
# Define server to graph the map, and display table
# ======================================================
server <- function(input, output) {
  
  # Reactive value to track the state of the legend visibility
  show_legend <- reactiveVal(TRUE)
  
  # Toggle the visibility of the legend when the button is clicked
  observeEvent(input$toggle_legend, {
    show_legend(!show_legend())
  })
  
  # ----------------------------------------------------------
  # table of filtered storms (this is a reactive conductor!)
  # ----------------------------------------------------------
  tbl <- reactive({
    # Start with all storms from the selected year
    storms_filtered <- storms %>%
      filter(year == input$year) %>%
      filter(wind >= input$wind_speed)
    
    # If "Show Major Hurricanes Only" is selected, filter based on major hurricane status
    if (input$major_hurricanes) {
      storms_filtered <- storms_filtered %>%
        filter(is_major_hurricane)
    }
    
    storms_filtered
  })
  
  # ----------------------------------------------------------
  # output: map of storms
  # ----------------------------------------------------------
  output$plot_map <- renderPlot({
    map <- atlantic_map +
      geom_point(data = tbl(), aes(x = long, y = lat, group = name, color = name)) +
      geom_path(data = tbl(), aes(x = long, y = lat, group = name, color = name))
    
    if (!show_legend()) {
      map <- map + theme(legend.position = "none")
    }
    
    if (input$facet_month) {
      map <- map + facet_wrap(~ month)
    }
    
    map
  })
  
  # ----------------------------------------------------------
  # output: summary table
  # ----------------------------------------------------------
  output$summary_table <- renderDataTable({
    tbl() %>%
      group_by(name) %>%
      summarise(
        start_date = paste0(first(month), "-", first(day)),
        end_date = paste0(last(month), "-", last(day)),
        maximum_wind = max(wind),
        minimum_pressure = min(pressure)
      )
  })
  
} # closes server

# Run the application 
shinyApp(ui = ui, server = server)
