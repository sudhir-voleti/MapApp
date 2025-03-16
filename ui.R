# ui.R

# Source dependencies
source("dependencies.R")

# Define UI
ui <- fluidPage(
  titlePanel("Leaflet Map from CSV"),
  sidebarLayout(
    sidebarPanel(width = 4,
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
      ),
      tags$hr(),
      p("Select the appropriate columns from your CSV file:"),
      uiOutput("place_select"),
      uiOutput("lat_select"),
      uiOutput("lon_select"),
      tags$hr(),
      p("Select variables to display in the tooltip (optional):"),
      uiOutput("tooltip_vars_select"),
      tags$hr(),
      actionButton("render_map", "Render Map"),
      tags$hr(),
      p("Click two markers to calculate the geodesic distance between them."),
      actionButton("reset_selection", "Reset Selection")
    ),
    mainPanel(width = 8,
      tabsetPanel(
        tabPanel("Data",
                 br(),
                 p("First 10 rows of the data:"),
                 dataTableOutput("data_preview")
        ),
        tabPanel("Map",
                 leafletOutput("map", height = 600)
        ),
        tabPanel("Distance",
                 br(),
                 h4("Geodesic Distance Between Selected Markers"),
                 verbatimTextOutput("distance_output"),
                 p("Click two markers on the map to calculate the distance. Use 'Reset Selection' to start over.")
        )
      )
    )
  )
)
