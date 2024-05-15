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
  titlePanel("Mapping the Pontal"),
  sidebarLayout(
    sidebarPanel(
      # Add text to the sidebar
      HTML("<h3> Draw Polygons and Add Labels </h3>"),
      p("This tool is designed to draw polygon into the map and adding labels to them."),
      tags$hr(),  # Add a horizontal line
      p("With the 'Draw a polygon' tool, add polygons to the map."),
      tags$hr(),  # Add a horizontal line
      p("Finish the polygon and enter the label in the text box."),
      tags$hr(),  # Add a horizontal line
      p("After finishing the text, click Ádd Label' button to add it to the centroid of the polygon")
    ),
    mainPanel(
      leafletOutput("map"),
      uiOutput("label_input")  # Add this line to include the dynamic UI element
    )
  )
)