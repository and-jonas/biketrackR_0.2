rm(list = ls())

.libPaths("C:/Users/anjonas/RLibs")

library(shiny)
library(shinyFiles)
library(leaflet)
library(OpenStreetMap)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(stringr)
options(shiny.maxRequestSize = 30*1024^2)
source("C:/Users/anjonas/Rprojects/biketrackR/R/funcs.R")
# source("R/funcs.R")

library(rdrop2)
library(httpuv)
drop_auth()
drop_acc() %>% data.frame()
drop_dir()

# ================================================================================================= -
# Server ----
# ================================================================================================= -

server <- function(input, output, session) {
  
  
  readGPX <- eventReactive(input$GPXfile, {
    FILE0 <- input$GPXfile
    if(is.null(FILE0)) return(NULL)
    # get input file path(s)
    FILE <- FILE0$datapath
    # get input file name(s)
    FILENAME <- FILE0$name
    # load GPS tracks
    
    #===
    
    # get prepared data from repository
    predat <- drop_read_csv("Appdata.csv")
    predat <- predat %>% dplyr::select(-X) %>% 
      mutate(date = as.Date(date, "%Y-%m-%d"))
    pre_dates <- unique(predat$date)
    
    dates_process <- strsplit(FILENAME, "_") %>% lapply(., "[[", 1) %>% unlist()
    
    # redefine which files to read and process
    date_load_idx <- which(!as.character(dates_process) %in% as.character(pre_dates))
    if(!identical(date_load_idx, integer(0))){
      FILE <- FILE[date_load_idx]
      FILENAME <- FILENAME[date_load_idx]
    }
    
    if(!identical(date_load_idx, integer(0))){
      
      #===
      
      data <- lapply(FILE, plotKML::readGPX)
      # get tour date(s)
      date <- basename(FILENAME) %>% strsplit(., "_") %>% 
        lapply(., "[[", 1) %>% unlist() %>% as.Date(format = "%Y-%m-%d")
      # reshape gps data
      geodat <- lapply(data, get_geodata)
      # add tour date
      for (i in 1:length(geodat)){
        geodat[[i]]$date <- date[i]
      }
      # check for duplicate dates and merge files, if necessary
      # identify dates with more than one track
      date <- list()
      for (i in 1:length(geodat)){
        date[[i]] <- geodat[[i]]$date[1]
      }
      dd <- date[which(duplicated(date))]
      dateindex <- which(duplicated(date))
      # iterate over dates with more than one track
      for (d in dd){ 
        ids <- which(unlist(date) %in% d)
        # total distance of first part
        final_dist1 <- max(geodat[[ids[1]]]$dist_tot, na.rm = TRUE)
        # add distance of first part to the distances in the second part
        geodat[[ids[2]]]$dist_tot <- geodat[[ids[2]]]$dist_tot + final_dist1
        # bind elements
        geodat[[ids[1]]] <- rbind(geodat[[ids[1]]], geodat[[ids[2]]])
      }
      geodat[c(dateindex[1], dateindex[2])] <- NULL
      # to single df
      alldat <- bind_rows(geodat)
      alldat <- alldat %>% mutate(ele = as.numeric(ele),
                                  time = as.factor(time),
                                  ele.p1 = as.numeric(ele.p1))
      #===
      
      # bind rows of previously and newly loaded data
      alldat <- bind_rows(predat, alldat)
      # update processed data on repository
      str(alldat)
      write.csv(alldat, 'AppData.csv', col.names = FALSE)
      drop_upload('AppData.csv')
    }
    else{
      alldat <- predat
    }
    
    #===
    
  })
  
  text_message <- reactiveVal('')
  output$print_action <- renderText({text_message()})
  output$range <- renderText({input$Slider1})
  output$value <- renderText({input$select})
  
  observe({
    alldat <- readGPX()
    if(is.null(alldat)) return(NULL)
    choices <- unique(alldat$date) %>% as.Date(., format = "%Y-%m-%d")
    updateSliderInput(session, "Slider1", 
                      "Wähle den Zeitraum:",
                      min = min(choices),
                      max = max(choices),
                      timeFormat = "%b %Y")
    early <- input$Slider1[1]
    late <- input$Slider1[2]
    selected_dates <- choices[which(choices >= early & choices <= late)]
    
    if (length(selected_dates)>0){
      updateCheckboxGroupInput(session, "select","Wähle Tour(en)",
                               choices = selected_dates)
    }
    if(input$selectall == 0) return(NULL)
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session, "select","Wähle Tour(en)",
                               choices = selected_dates)
    }
    else
    {
      updateCheckboxGroupInput(session, "select",
                               label = "Wähle Tour(en)",
                               choices = selected_dates,
                               selected = selected_dates)
    }
  })
  
  output$SliderText <- renderText({sliderMonth$Month})
  
  output$mymap <- renderLeaflet({
    alldat <- readGPX()
    # initiate object
    mymap <- leaflet() %>% 
      addTiles()
    # color vector
    colors <- rainbow(n = length(input$select))
    # read input file
    alldat <- readGPX()
    if(is.null(alldat)) return(NULL)
    # add all selected tours
    if (length(input$select) > 0){
      for (i in 1:length(input$select)){
        df <- alldat[alldat$date == as.Date(input$select[i], format = "%Y-%m-%d"), 
                     c("lat", "lon")]
        mymap <- mymap %>%
          addProviderTiles(provider = providers$Stamen.Terrain, layerId = 0) %>%
          addPolylines(data = df, 
                       lng = ~lon, 
                       lat = ~lat,
                       col = colors[i],
                       layerId = i,
                       group = "base")
      }
      # define map limits
      mymap <- mymap %>% clearBounds()
    }
  })
  
  # Observe the polylines
  observe({
    leafletProxy("mymap") %>% clearPopups() %>% clearGroup(group = "selected")
    event <- input$mymap_shape_click
    if (is.null(event))
      return(NULL)
    # selected data for the clicked polyline
    alldat <- readGPX()
    df <- alldat[alldat$date == as.Date(input$select[event$id], format = "%Y-%m-%d"),]  
    # summary data to display on click
    date <- unique(df$date)
    dist <- max(df$dist_tot, na.rm = T)
    up <- round(max(df$cum.uphill, na.rm = T), 0)
    highest <- round(max(as.numeric(df$ele), na.rm = T), 0)
    # Popup on click
    leafletProxy("mymap") %>% 
      addPopups(
        event$lng, event$lat, 
        paste(paste0("\uF4C5", date), "\n",
              paste0("\u2192", dist, "km"), 
              paste0("\u2191", up, "m"), 
              paste0("\u25B2", highest, "M.ü.M"), 
              sep = " ")
      ) %>%
      addPolylines(data = df, 
                   lng = ~lon, 
                   lat = ~lat,
                   layerId = event$id,
                   group = "selected",
                   dashArray = 3, 
                   color = "red", weight = 5, opacity = 1)
  })
  
  output$Streckenprofil <- renderPlot({
    alldat <- readGPX()
    profilplot(alldat[alldat$date %in% as.Date(input$select, format = "%Y-%m-%d"),])
  })
  
  output$distbox <- renderValueBox({
    alldat <- readGPX()
    # subset data according to selected dates
    df <- alldat[alldat$date %in% as.Date(input$select, format = "%Y-%m-%d"), ]
    # extract daily distance
    data_list <- split(df, df$date)
    daily_dist <- lapply(data_list, function(df) max(df$dist_tot, na.rm = TRUE))
    # sum up
    sum_dd <- daily_dist %>% unlist() %>% unname() %>% sum()
    valueBox(paste0(sum_dd, "km"), "Distanz", 
             icon = icon("arrow-alt-circle-right", lib = "font-awesome"), 
             width = 7.5, color = "green")
  })
  
  output$uphillbox <- renderValueBox({
    alldat <- readGPX()
    # subset data according to selected dates
    df <- alldat[alldat$date %in% as.Date(input$select, format = "%Y-%m-%d"), ]
    # extract daily uphill
    data_list <- split(df, df$date)
    daily_uphill <- lapply(data_list, function(d) max(d$cum.uphill, na.rm = TRUE))
    # sum up
    sum_dup <- daily_uphill %>% unlist() %>% unname() %>% sum() %>% round(., 0)
    valueBox(paste0(sum_dup, "m"), "Aufwärts", 
             icon = icon("arrow-alt-circle-up", lib = "font-awesome"), 
             width = 7.5, color = "red")
  })
  
}