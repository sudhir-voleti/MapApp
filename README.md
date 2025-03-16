# Leaflet Map Shiny App

This Shiny app allows users to upload a CSV file with geocoded locations, display them on a Leaflet map, and calculate the geodesic distance between two selected markers.

## How to Run Locally
1. Clone the repository: `git clone <repository-url>`
2. Open RStudio and set the working directory to the app folder.
3. Run the app using `shiny::runApp()`.

## Deployment
- Deploy on ShinyApps.io by following the instructions at [shinyapps.io](https://www.shinyapps.io).

## Dependencies
- shiny
- leaflet
- dplyr
- DT
- geosphere
