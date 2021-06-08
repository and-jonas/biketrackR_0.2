rm(list = ls())
library(shiny)
library(shinyFiles)
library(leaflet)
library(OpenStreetMap)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(stringr)
options(shiny.maxRequestSize = 30*1024^2)
source("funcs.R")
library(rdrop2)
library(httpuv)
drop_auth()
drop_acc() %>% data.frame()
drop_dir()

# Tweaks to the checkboxgroupinput
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 750px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count: 5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")),
                 tags$style("#mymap {height: calc(100vh - 90px) !important;}"),
                 tags$style("#select {
                    font-size:10px;
                    height:10px;
           }")
  ))

# ================================================================================================= -
# User Interface ----
# ================================================================================================= -

ui <- fluidPage(
  
  tweaks,
  
  setBackgroundColor(color = "ghostwhite"),
  useShinydashboard(),
  
  titlePanel("Wähle Tour(en)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("GPXfile", 
                "Wähle .gpx file(s)", 
                accept=c("text/xml", ".gpx"), 
                multiple = TRUE),
      
      sliderInput("Slider1", 
                  "Wähle den Zeitraum:",
                  min = as.Date("2013-01-01", "%Y-%m-%d"),
                  max = as.Date("2013-01-01", "%Y-%m-%d"),
                  value = c(as.Date("2013-01-01", "%Y-%m-%d"), 
                            as.Date("2013-01-01", "%Y-%m-%d")),
                  timeFormat = "%b %Y"),
      
      verbatimTextOutput("Slider1"),
      
      tags$div(align = 'left', 
               class = 'multicol', 
               checkboxGroupInput(inputId  = "select", 
                                  label    = "Wähle Tour(en)", 
                                  choices  = NULL,
                                  selected = NULL,
                                  inline   = FALSE)),
      
      actionLink("selectall","Select All"),
      verbatimTextOutput("select")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Höhenprofil", plotOutput("Streckenprofil")), 
        tabPanel("Karte", leafletOutput("mymap")),
        tabPanel("Statistiken", 
                 column(width = 12, id = "main-panel", 
                        valueBoxOutput("distbox", width = 6)),
                 column(width = 12, id = "main-panel", 
                        valueBoxOutput("uphillbox", width = 6)))
      )
    )
  )
)