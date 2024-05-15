#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(raster)
library(leaflet.extras)
library(shinyWidgets)
library(sf)
library(raster)
library(rgdal)

# Define UI for application that draws a map
ui <- fluidPage(
  titlePanel("Draw Polygons and Add Labels"),
  mainPanel(
    leafletOutput("map"),
    uiOutput("label_input")  # Add this line to include the dynamic UI element
  )
)