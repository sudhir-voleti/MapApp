# dependencies.R

# Check and install required libraries
list.of.packages <- c("shiny", "leaflet", "dplyr", "DT", "geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(geosphere)
