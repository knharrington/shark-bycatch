################################################################################
# This script is the global environment for the CFEMM app
# It stores objects needed in the server and ui

################################################################################

# load packages
{
  library(tidyverse)
  library(data.table)
  library(mgcv)
  library(janitor)
  library(sf)
  library(leaflet)
  library(plotly)
  library(shiny)
  library(shinyWidgets)
  library(htmlwidgets)
  library(bslib)
  library(shinycssloaders)
  library(leaflet.extras2)
  library(thematic)
}

load("data/preprocess.RData")
