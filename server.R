# server.R

# Source dependencies
source("dependencies.R")

# Define server logic
server <- function(input, output, session) {
  
  # Default dataset: Top 20 shopping malls in Hyderabad
  default_data <- reactive({
    data.frame(
      place = c(
        "Sarath City Capital Mall", "Inorbit Mall", "GVK One Mall", "Forum Sujana Mall", "Lulu Mall Hyderabad",
        "Nexus Mall", "Manjeera Mall", "City Centre Mall", "Next Galleria Mall", "Hyderabad Central Mall",
        "Big Bazaar Supercentre", "South India Shopping Mall", "GSM Mall", "DSL Virtue Mall", "IKEA Hyderabad",
        "FMG Mall", "Lanco Mall", "Shoppers Stop Mall", "Nexus Westend Mall", "Next Galleria Metro Mall"
      ),
      latitude = c(
        17.4345, 17.4340, 17.4160, 17.4840, 17.4800,
        17.4845, 17.4890, 17.4130, 17.4265, 17.4280,
        17.4060, 17.4895, 17.4940, 17.4045, 17.4400,
        17.3900, 17.4000, 17.4100, 17.4850, 17.4270
      ),
      longitude = c(
        78.3855, 78.3860, 78.4465, 78.3900, 78.3900,
        78.3910, 78.3950, 78.4410, 78.4520, 78.4530,
        78.4770, 78.3955, 78.3240, 78.5630, 78.3820,
        78.4750, 78.4800, 78.4650, 78.3920, 78.4530
      ),
      details = c(
        "Largest mall in Hyderabad (2.7M sq ft), with over 400 stores, AMB Cinemas, and food courts.",
        "Over 1M sq ft, known for brands like Tommy Hilfiger, Levi’s, and a 6-screen PVR.",
        "1.5M sq ft, luxurious with international brands, INOX cinema, and a food court.",
        "850,000 sq ft, features Zara, Forever 21, PVR cinema, and a vibrant food court.",
        "450,000 sq ft, opened in 2023, largest Lulu Mall in India, with extended hours and a hypermarket.",
        "800,000 sq ft, 14,493 daily visitors, known for brands and entertainment options.",
        "450,000 sq ft, features HyperCity, Cinepolis, and a bowling alley.",
        "600,000 sq ft, one of the first malls in Hyderabad, with brands like Adidas and SMAAASH gaming.",
        "Known as the 'Shipbuilding Mall,' connected to Panjagutta Metro Station, with PVR and gaming.",
        "250,000 sq ft, one of the oldest malls, with PVR Cinemas and branded stores like Guess.",
        "Dedicated to Big Bazaar, offers groceries, electronics, and clothing, with food outlets.",
        "Specializes in traditional and contemporary clothing, with branches in Gachibowli, Kothapet.",
        "New-generation mall with modern facilities, good connectivity, and dining options.",
        "Hyderabad’s first mall in the eastern region, near Uppal Metro Station, top retail center.",
        "Not a traditional mall but a major retail destination for home furnishings, with dining.",
        "Ideal for window shopping, located in the business district, close to historical monuments.",
        "Mentioned in lists but lacks specific location details; approximated to central Hyderabad.",
        "A one-stop retail destination for trendy clothing, with brands like Levi’s and Lavie.",
        "Another Nexus property, smaller but popular, approximated near Nexus Mall.",
        "Another Next Galleria property, with dining and entertainment, near Panjagutta Metro."
      )
    )
  })

  # Reactive data: Use uploaded CSV if available, otherwise use default data
  data <- reactive({
    file <- input$file1
    if (is.null(file)) {
      return(default_data())
    }
    tryCatch(
      read.csv(file$datapath,
               header = TRUE,
               sep = ',',
               quote = '"',
               stringsAsFactors = FALSE),
      error = function(e) {
        showNotification(paste("Error reading CSV:", e$message), type = "error")
        return(default_data())
      }
    )
  })

  # Render data preview
  output$data_preview <- renderDataTable({
    req(data())
    head(data(), n = 10)
  }, options = list(pageLength = 5))

  # Dynamic UI for column selection
  output$place_select <- renderUI({
    df <- data()
    if (is.null(df)) return(NULL)
    selectInput("place_col", "Select Place Column:", choices = names(df), selected = "place")
  })

  output$lat_select <- renderUI({
    df <- data()
    if (is.null(df)) return(NULL)
    selectInput("lat_col", "Select Latitude Column:", choices = names(df), selected = "latitude")
  })

  output$lon_select <- renderUI({
    df <- data()
    if (is.null(df)) return(NULL)
    selectInput("lon_col", "Select Longitude Column:", choices = names(df), selected = "longitude")
  })

  output$tooltip_vars_select <- renderUI({
    df <- data()
    if (is.null(df)) return(NULL)
    selectInput("tooltip_vars", "Select Variables for Tooltip:",
                choices = names(df),
                multiple = TRUE,
                selected = "details")
  })

  # Prepare map data when render button is clicked
  map_data <- eventReactive(input$render_map, {
    df <- data()
    place_col <- input$place_col
    lat_col <- input$lat_col
    lon_col <- input$lon_col
    tooltip_vars <- input$tooltip_vars

    req(df, place_col, lat_col, lon_col)

    if (!place_col %in% names(df) || !lat_col %in% names(df) || !lon_col %in% names(df)) {
      showNotification("Selected columns not found in data.", type = "error")
      return(NULL)
    }

    # Ensure latitude and longitude are numeric
    df[[lat_col]] <- as.numeric(as.character(df[[lat_col]]))
    df[[lon_col]] <- as.numeric(as.character(df[[lon_col]]))

    # Identify rows with NA values after conversion
    invalid_rows <- which(is.na(df[[lat_col]]) | is.na(df[[lon_col]]))
    if (length(invalid_rows) > 0) {
      showNotification(
        paste("Latitude or longitude contains non-numeric values or NA in",
              length(invalid_rows), "rows. These rows will be skipped."),
        type = "warning"
      )
    }

    # Select and rename columns
    selected_cols <- unique(c(place_col, lat_col, lon_col, tooltip_vars))
    selected_data <- df %>%
      select(any_of(selected_cols)) %>%
      rename(place = !!sym(place_col),
             latitude = !!sym(lat_col),
             longitude = !!sym(lon_col)) %>%
      filter(!is.na(latitude), !is.na(longitude))

    # Print for debugging
    print("Map data structure after filtering:")
    print(str(selected_data))
    print("First few rows of map data:")
    print(head(selected_data))

    if (nrow(selected_data) == 0) {
      showNotification("No valid data after filtering NA values.", type = "error")
      return(NULL)
    }

    selected_data
  })

  # Render the initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 78.4867, lat = 17.3850, zoom = 11) # Center on Hyderabad by default
  })

  # Update map with markers when render button is clicked
  observeEvent(input$render_map, {
    df <- map_data()
    if (is.null(df) || nrow(df) == 0) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        setView(lng = 78.4867, lat = 17.3850, zoom = 11) # Center on Hyderabad
      return()
    }

    tooltip_vars <- input$tooltip_vars

    leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      fitBounds(min(df$longitude, na.rm = TRUE), min(df$latitude, na.rm = TRUE),
                max(df$longitude, na.rm = TRUE), max(df$latitude, na.rm = TRUE)) %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = lapply(1:nrow(df), function(i) {
          tooltip_content <- paste0("<b>", htmltools::htmlEscape(df$place[i]), "</b><br>")
          if (!is.null(tooltip_vars) && length(tooltip_vars) > 0) {
            tooltip_content <- paste0(tooltip_content,
                                     paste(sapply(tooltip_vars, function(var) {
                                       if (var %in% names(df)) {
                                         paste0(htmltools::htmlEscape(var), ": ",
                                                htmltools::htmlEscape(as.character(df[[var]][i])))
                                       } else ""
                                     }), collapse = "<br>"))
          }
          HTML(tooltip_content)
        }),
        labelOptions = labelOptions(
          style = list("font-size" = "12px", "padding" = "5px 10px"),
          direction = "auto",
          html = TRUE
        )
      )
  })

  # Track selected markers
  selected_markers <- reactiveVal(list())

  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    df <- map_data()
    if (is.null(click) || is.null(df) || nrow(df) == 0) return()

    # Find the closest marker based on click coordinates
    distances <- sqrt((df$latitude - click$lat)^2 + (df$longitude - click$lng)^2)
    closest_idx <- which.min(distances)
    selected_row <- df[closest_idx, ]

    current_markers <- selected_markers()
    new_marker <- list(
      place = selected_row$place,
      lat = selected_row$latitude,
      lon = selected_row$longitude
    )

    if (length(current_markers) < 2) {
      current_markers <- append(current_markers, list(new_marker))
      selected_markers(current_markers)
      if (length(current_markers) == 1) {
        showNotification("Select the second marker.", type = "message")
      } else if (length(current_markers) == 2) {
        showNotification("Two markers selected—distance calculated.", type = "message")
      }
    }
  })

  # Reset selection
  observeEvent(input$reset_selection, {
    selected_markers(list())
    showNotification("Selection reset. Click two new markers to calculate distance.", type = "message")
  })

  # Calculate and display distance
  output$distance_output <- renderPrint({
    markers <- selected_markers()
    if (length(markers) == 2) {
      point1 <- c(markers[[1]]$lon, markers[[1]]$lat)
      point2 <- c(markers[[2]]$lon, markers[[2]]$lat)
      distance_m <- distGeo(point1, point2)
      distance_km <- distance_m / 1000
      cat(sprintf("Distance between %s and %s: %.2f km\n",
                  markers[[1]]$place, markers[[2]]$place, distance_km))
    } else if (length(markers) > 2) {
      selected_markers(list(markers[[1]], markers[[2]])) # Limit to first two
      showNotification("Only the first two selected markers are considered.", type = "warning")
    } else {
      cat("Select two markers on the map to calculate the geodesic distance.\n")
    }
  })
}
