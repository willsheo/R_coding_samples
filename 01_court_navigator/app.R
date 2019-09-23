####################################################
# 
# Philippine Supreme Court Justice Reform Project
#
# Title:        Court Navigator
# Host:         Office of the Court Administrator
# Created by:   Will Seonmin Heo
# Last update:  September 23, 2019
# Version:      1.0F
#
# Note:
# This application is originally created to assist
# the Court Administrator of the Philippine Supreme
# Court in identifying lower-level courts heavily
# burdened with caseflow. This version is to only
# demonstrate how the application works and does not 
# include confidential information; all the values,
# including the branch numbers of stations, are
# randomly generated.
#
####################################################

library(DT)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(rmarkdown)
library(sf)
library(shiny)
library(stringi)
library(tidyverse)
library(viridis)

load("generated_data.RData")

pal <- colorNumeric(palette = "plasma", domain = c(400, 550))
color <- colorFactor(brewer.pal(n = 5, name = "Dark2"), data_final$crttype)


ui <- navbarPage("Court Navigator", id="nav",
  
  ##### Interactive map #####
  tabPanel("Interactive Map",
           div(class="outer",
               
               tags$head(
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),          
               
               leafletOutput(outputId = "philippine_map", height = 650),
               
               absolutePanel(id = "controls", fixed = FALSE, class = "outer", 
                             draggable = TRUE, top = 10, left = 150, right = "auto", bottom = "auto",
                             width = 350, height = "630",
                             
                             sliderInput(inputId = "pendingnum",
                                         label = "Number of pending cases (greater than):",
                                         min = 0,
                                         max = 2000,
                                         step = 100,
                                         value = 0),
                             
                             selectInput(inputId = "timescale",
                                         label = "Period of interest:",
                                         choices = c("Latest", "2018", "2017", 
                                                     "2016", "2015", "2014", "2013")),
                             
                             checkboxGroupInput(inputId = "crttype", "Type of Courts",
                                                choices = list("MTC" = "MTC",
                                                               "METC" = "METC",
                                                               "MCTC" = "MCTC",
                                                               "MTCC" = "MTCC",
                                                               "RTC" = "RTC"),
                                                selected = c("MTC", "METC", "MCTC", 
                                                             "MTCC", "RTC")),
                             
                             actionButton(inputId = "go_caseload", label = "Update"),
                             
                             plotOutput("tsplot_pe", height = 200),
                             
                             downloadButton(outputId = "report_summary", "Generate Report"),
                             downloadButton(outputId = "report_detail", "Detailed Report")
               ),
               
               hr(),
               hr(),
               
               helpText("This version uses randomly generated numbers of caseflow 
                        to simply demonstrate how the application is run. 
                        All the values, including the branch numbers of stations, 
                        are randomly generated.")
               
           )),
  
  tabPanel("Court Data",
           DT::dataTableOutput("courtdt")
  ),
  
  conditionalPanel("false", icon("balance-scale"))
  
)

server <- function(input, output, session) {
  
  # Create a map
  output$philippine_map <- renderLeaflet({
    leaflet(data = data_final) %>% 
      # addProviderTiles(providers$Esri.OceanBasemap) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
      # addWMSTiles(
      #   "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
      #   attribution = NULL,group = 'Graticules') %>%
      addLayersControl(overlayGroups = c('Place names'),
                       options = layersControlOptions(collapsed = FALSE),
                       position = 'topright') %>%
      addScaleBar(position = 'topright') %>% 
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE) %>% 
      hideGroup(c("Place names", "Graticules")) %>% 
      addLegend(
        "bottomleft", # Legend position
        pal = color, # color palette
        values = ~data_final$crttype, # legend values
        opacity = 1,
        title = "Type of Trial Court") %>%
      setView(lng = 122, lat = 12, zoom = 6) 

  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  courtsInBounds <- reactive({
    if (is.null(input$philippine_map_bounds))
      return(data[FALSE,])
    bounds <- input$philippine_map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(data_caseload(),
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2])
  })
  
  # Whenever the "Update" button is clicked, filter the courts meeting the criteria
  data_caseload <- eventReactive(input$go_caseload, {
    if(input$timescale == "Latest") {
      data_final %>% filter(`LAST_PENDING_VALUE` > input$pendingnum) %>% 
        filter(crttype %in% input$crttype)  
    }
    else if(input$timescale == "2018") {
      data_final %>% filter(PENDING_ENDING_12_2018 > input$pendingnum) %>% 
        filter(crttype %in% input$crttype)  
    }
    else if(input$timescale == "2017") {
      data_final %>% filter(PENDING_ENDING_12_2017 > input$pendingnum) %>% 
        filter(crttype %in% input$crttype)  
    }
    else if(input$timescale == "2016") {
      data_final %>% filter(PENDING_ENDING_12_2016 > input$pendingnum) %>% 
        filter(crttype %in% input$crttype)  
    }
    else if(input$timescale == "2015") {
      data_final %>% filter(PENDING_ENDING_12_2015 > input$pendingnum) %>% 
        filter(crttype %in% input$crttype)  
    }
    else if(input$timescale == "2014") {
      data_final %>% filter(PENDING_ENDING_12_2014 > input$pendingnum) %>% 
        filter(crttype %in% input$crttype)  
    }
    else if(input$timescale == "2013") {
      data_final %>% filter(PENDING_ENDING_12_2013 > input$pendingnum) %>% 
        filter(crttype %in% input$crttype)  
    }
    
    ## add ignoreNULL=FALSE to also update it when the app starts 
  }, ignoreNULL = FALSE)
  
  ## Create an observer that is executed whenever its dependencies change, 
  ## such as when the Update button is clicked or selected courts change
  observe({
    # If no court is selected, then simply clear all the markers
    if (nrow(data_caseload()) == 0) {
      return( leafletProxy("philippine_map", data = data_final) %>% 
                clearMarkers() )
    }
    
    leafletProxy("philippine_map", data = data_final) %>% 
      clearMarkers() %>% 
      addCircleMarkers(lng = ~data_caseload()$lon, 
                       lat = ~data_caseload()$lat, 
                       #radius = ~ 0.5*sqrt(data$`CASE INFLOW`),
                       radius = 9,
                       # clusterOptions = markerClusterOptions(
                       #   showCoverageOnHover = F,
                       #   freezeAtZoom = 10),
                       stroke = TRUE,
                       fillOpacity = 0.4,
                       color = ~color(data_caseload()$crttype),
                       label = ~data_caseload()$station_name,
                       layerId = ~data_caseload()$station_name)
  })

  # This is activated when an Action button on the data table is clicked
  observe({
    if (is.null(input$goto))
      return()
    print(input$goto)
    isolate({
      map <- leafletProxy("philippine_map")
      map %>% clearPopups()
      dist <- 0.5
      lat <- input$goto$lat
      lon <- input$goto$lon
      station_name <- input$goto$station_name
      courtPopup(lat, lon, station_name)
      map %>% fitBounds(lon - dist, lat - dist, lon + dist, lat + dist)
      showTSPlot(input$goto$station_name)
    })
  })
  
  # Choose only the necessary variables to show in the summary report
  selectedDF <- reactive({
    
    if(input$timescale == "Latest") {
      data_caseload() %>% 
        mutate(LAST_DISPOSITION = sprintf("%.1f%%", 100*LAST_DISPOSITION)) %>% 
        select(Province = PROVINCE, REGION, Station = station_name,
               `Pending Cases` = LAST_PENDING_VALUE,
               `Case Inflow` = LAST_INFLOW_SUM, 
               `Case Outflow` = LAST_OUTFLOW_SUM,
               `Disposition Rate` = LAST_DISPOSITION)  
    }
    else if(input$timescale == "2018") {
      data_caseload() %>% 
        mutate(DISPOSITION_2018 = sprintf("%.1f%%", 100*DISPOSITION_2018)) %>% 
        select(Province = PROVINCE, REGION, Station = station_name,
               `Pending Cases` = PENDING_ENDING_12_2018,
               `Case Inflow` = CASE_INFLOW_TOTAL_2018, 
               `Case Outflow` = CASE_OUTFLOW_TOTAL_2018,
               `Disposition Rate` = DISPOSITION_2018)  
    }
    else if(input$timescale == "2017") {
      data_caseload() %>% 
        mutate(DISPOSITION_2017 = sprintf("%.1f%%", 100*DISPOSITION_2017)) %>% 
        select(Province = PROVINCE, REGION, Station = station_name,
               `Pending Cases` = PENDING_ENDING_12_2017,
               `Case Inflow` = CASE_INFLOW_TOTAL_2017, 
               `Case Outflow` = CASE_OUTFLOW_TOTAL_2017,
               `Disposition Rate` = DISPOSITION_2017)  
    }
    else if(input$timescale == "2016") {
      data_caseload() %>% 
        mutate(DISPOSITION_2016 = sprintf("%.1f%%", 100*DISPOSITION_2016)) %>% 
        select(Province = PROVINCE, REGION, Station = station_name,
               `Pending Cases` = PENDING_ENDING_12_2016,
               `Case Inflow` = CASE_INFLOW_TOTAL_2016, 
               `Case Outflow` = CASE_OUTFLOW_TOTAL_2016,
               `Disposition Rate` = DISPOSITION_2016)  
    }
    else if(input$timescale == "2015") {
      data_caseload() %>% 
        mutate(DISPOSITION_2015 = sprintf("%.1f%%", 100*DISPOSITION_2015)) %>% 
        select(Province = PROVINCE, REGION, Station = station_name,
               `Pending Cases` = PENDING_ENDING_12_2015,
               `Case Inflow` = CASE_INFLOW_TOTAL_2015, 
               `Case Outflow` = CASE_OUTFLOW_TOTAL_2015,
               `Disposition Rate` = DISPOSITION_2015)  
    }
    else if(input$timescale == "2014") {
      data_caseload() %>% 
        mutate(DISPOSITION_2014 = sprintf("%.1f%%", 100*DISPOSITION_2014)) %>% 
        select(Province = PROVINCE, REGION, Station = station_name,
               `Pending Cases` = PENDING_ENDING_12_2014,
               `Case Inflow` = CASE_INFLOW_TOTAL_2014, 
               `Case Outflow` = CASE_OUTFLOW_TOTAL_2014,
               `Disposition Rate` = DISPOSITION_2014)  
    }
    else if(input$timescale == "2013") {
      data_caseload() %>% 
        mutate(DISPOSITION_2013 = sprintf("%.1f%%", 100*DISPOSITION_2013)) %>% 
        select(Province = PROVINCE, REGION, Station = station_name,
               `Pending Cases` = PENDING_ENDING_12_2013,
               `Case Inflow` = CASE_INFLOW_TOTAL_2013, 
               `Case Outflow` = CASE_OUTFLOW_TOTAL_2013,
               `Disposition Rate` = DISPOSITION_2013)  
    }

  })
  
  # This creates the data table in the Court Data Tab
  output$courtdt <- renderDataTable({
    if (input$timescale == "Latest") {
      table <- data_caseload() %>% 
        mutate(Action = paste0('<a class="go-map" href="" data-lat="', lat, 
                               '" data-lon="', lon, '"data-station_name="', station_name,
                               '"><i class="fa fa-balance-scale"></i></a>')) %>% 
        mutate(LAST_DISPOSITION = sprintf("%.1f%%", 100*LAST_DISPOSITION),
               LAST_CLEARANCE = sprintf("%.1f%%", 100*LAST_CLEARANCE),
               LAST_SUBMISSION = paste0("As of ", 
                                        LAST_SUBMIT_MONTH, "/", LAST_SUBMIT_YEAR)) %>% 
        select(`Station Name` = station_name, Region = REGION, 
               `Pending Cases (Latest)` = LAST_PENDING_VALUE, 
               `Clearance Rate` = LAST_CLEARANCE,
               `Disposition Rate` = LAST_DISPOSITION, 
               `Last Submitted` = LAST_SUBMISSION,
               crttype, Action)
    }
    else if (input$timescale == "2018") {
      table <- data_caseload() %>% 
        mutate(Action = paste0('<a class="go-map" href="" data-lat="', lat, 
                               '" data-lon="', lon, '"data-station_name="', station_name,
                               '"><i class="fa fa-balance-scale"></i></a>')) %>% 
        mutate(DISPOSITION_2018 = sprintf("%.1f%%", 100*DISPOSITION_2018),
               CLEARANCE_2018 = sprintf("%.1f%%", 100*CLEARANCE_2018),
               LAST_SUBMISSION = paste0("As of ", 
                                        LAST_SUBMIT_MONTH, "/", LAST_SUBMIT_YEAR)) %>% 
        select(`Station Name` = station_name, Region = REGION, 
               `Pending Cases (2018)` = PENDING_ENDING_12_2018, 
               `Clearance Rate` = CLEARANCE_2018,
               `Disposition Rate` = DISPOSITION_2018, 
               `Last Submitted` = LAST_SUBMISSION,
               crttype, Action)
    }
    else if (input$timescale == "2017") {
      table <- data_caseload() %>% 
        mutate(Action = paste0('<a class="go-map" href="" data-lat="', lat, 
                               '" data-lon="', lon, '"data-station_name="', station_name,
                               '"><i class="fa fa-balance-scale"></i></a>')) %>% 
        mutate(DISPOSITION_2017 = sprintf("%.1f%%", 100*DISPOSITION_2017),
               CLEARANCE_2017 = sprintf("%.1f%%", 100*CLEARANCE_2017),
               LAST_SUBMISSION = paste0("As of ", 
                                        LAST_SUBMIT_MONTH, "/", LAST_SUBMIT_YEAR)) %>% 
        select(`Station Name` = station_name, Region = REGION, 
               `Pending Cases (2017)` = PENDING_ENDING_12_2017, 
               `Clearance Rate` = CLEARANCE_2017,
               `Disposition Rate` = DISPOSITION_2017, 
               `Last Submitted` = LAST_SUBMISSION,
               crttype, Action)
    }
    else if (input$timescale == "2016") {
      table <- data_caseload() %>% 
        mutate(Action = paste0('<a class="go-map" href="" data-lat="', lat, 
                               '" data-lon="', lon, '"data-station_name="', station_name,
                               '"><i class="fa fa-balance-scale"></i></a>')) %>% 
        mutate(DISPOSITION_2016 = sprintf("%.1f%%", 100*DISPOSITION_2016),
               CLEARANCE_2016 = sprintf("%.1f%%", 100*CLEARANCE_2016),
               LAST_SUBMISSION = paste0("As of ", 
                                        LAST_SUBMIT_MONTH, "/", LAST_SUBMIT_YEAR)) %>% 
        select(`Station Name` = station_name, Region = REGION, 
               `Pending Cases (2016)` = PENDING_ENDING_12_2016, 
               `Clearance Rate` = CLEARANCE_2016,
               `Disposition Rate` = DISPOSITION_2016, 
               `Last Submitted` = LAST_SUBMISSION,
               crttype, Action)
    }
    else if (input$timescale == "2015") {
      table <- data_caseload() %>% 
        mutate(Action = paste0('<a class="go-map" href="" data-lat="', lat, 
                               '" data-lon="', lon, '"data-station_name="', station_name,
                               '"><i class="fa fa-balance-scale"></i></a>')) %>% 
        mutate(DISPOSITION_2015 = sprintf("%.1f%%", 100*DISPOSITION_2015),
               CLEARANCE_2015 = sprintf("%.1f%%", 100*CLEARANCE_2015),
               LAST_SUBMISSION = paste0("As of ", 
                                        LAST_SUBMIT_MONTH, "/", LAST_SUBMIT_YEAR)) %>% 
        select(`Station Name` = station_name, Region = REGION, 
               `Pending Cases (2015)` = PENDING_ENDING_12_2015, 
               `Clearance Rate` = CLEARANCE_2015,
               `Disposition Rate` = DISPOSITION_2015, 
               `Last Submitted` = LAST_SUBMISSION,
               crttype, Action)
    }
    else if (input$timescale == "2014") {
      table <- data_caseload() %>% 
        mutate(Action = paste0('<a class="go-map" href="" data-lat="', lat, 
                               '" data-lon="', lon, '"data-station_name="', station_name,
                               '"><i class="fa fa-balance-scale"></i></a>')) %>% 
        mutate(DISPOSITION_2014 = sprintf("%.1f%%", 100*DISPOSITION_2014),
               CLEARANCE_2014 = sprintf("%.1f%%", 100*CLEARANCE_2014),
               LAST_SUBMISSION = paste0("As of ", 
                                        LAST_SUBMIT_MONTH, "/", LAST_SUBMIT_YEAR)) %>% 
        select(`Station Name` = station_name, Region = REGION, 
               `Pending Cases (2014)` = PENDING_ENDING_12_2014, 
               `Clearance Rate` = CLEARANCE_2014,
               `Disposition Rate` = DISPOSITION_2014, 
               `Last Submitted` = LAST_SUBMISSION,
               crttype, Action)
    }
    else if (input$timescale == "2013") {
      table <- data_caseload() %>% 
        mutate(Action = paste0('<a class="go-map" href="" data-lat="', lat, 
                               '" data-lon="', lon, '"data-station_name="', station_name,
                               '"><i class="fa fa-balance-scale"></i></a>')) %>% 
        mutate(DISPOSITION_2013 = sprintf("%.1f%%", 100*DISPOSITION_2013),
               CLEARANCE_2013 = sprintf("%.1f%%", 100*CLEARANCE_2013),
               LAST_SUBMISSION = paste0("As of ", 
                                        LAST_SUBMIT_MONTH, "/", LAST_SUBMIT_YEAR)) %>% 
        select(`Station Name` = station_name, Region = REGION, 
               `Pending Cases (2013)` = PENDING_ENDING_12_2013, 
               `Clearance Rate` = CLEARANCE_2013,
               `Disposition Rate` = DISPOSITION_2013, 
               `Last Submitted` = LAST_SUBMISSION,
               crttype, Action)
    }
    
    action <- DT::dataTableAjax(session, table)
    DT::datatable(table, options = list(ajax = list(url = action), 
                                        columnDefs = list(list(className='dt-right', targets=3:7))),
                  escape = FALSE)
  })

  # Show a pop-up at a given location
  courtPopup <- function(lat, lon, station_name) {
    selectedCourt <- data_caseload()[data_caseload()$station_name == station_name, ]
    if(input$timescale == "Latest") {
      content <- as.character(tagList(
        tags$h4(paste0(selectedCourt$station_name, ", ", selectedCourt$PROVINCE)),
        sprintf("%-40s: %d", "Case Inflow", 
                as.integer(selectedCourt$LAST_INFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Cases Outflow", 
                as.integer(selectedCourt$LAST_OUTFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Pending Cases (Ending)", 
                as.integer(selectedCourt$LAST_PENDING_VALUE)), tags$br()
      ))      
    }
    else if (input$timescale == "2018") {
      content <- as.character(tagList(
        tags$h4(paste0(selectedCourt$station_name, ", ", selectedCourt$PROVINCE)),
        sprintf("%-40s: %d", "Case Inflow", 
                as.integer(selectedCourt$LAST_INFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Cases Outflow", 
                as.integer(selectedCourt$LAST_OUTFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Pending Cases (Ending)", 
                as.integer(selectedCourt$PENDING_ENDING_12_2018)), tags$br()
      ))    
    }
    else if (input$timescale == "2017") {
      content <- as.character(tagList(
        tags$h4(paste0(selectedCourt$station_name, ", ", selectedCourt$PROVINCE)),
        sprintf("%-40s: %d", "Case Inflow", 
                as.integer(selectedCourt$LAST_INFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Cases Outflow", 
                as.integer(selectedCourt$LAST_OUTFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Pending Cases (Ending)", 
                as.integer(selectedCourt$PENDING_ENDING_12_2017)), tags$br()
      ))    
    }
    else if (input$timescale == "2016") {
      content <- as.character(tagList(
        tags$h4(paste0(selectedCourt$station_name, ", ", selectedCourt$PROVINCE)),
        sprintf("%-40s: %d", "Case Inflow", 
                as.integer(selectedCourt$LAST_INFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Cases Outflow", 
                as.integer(selectedCourt$LAST_OUTFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Pending Cases (Ending)", 
                as.integer(selectedCourt$PENDING_ENDING_12_2016)), tags$br()
      ))    
    }
    else if (input$timescale == "2015") {
      content <- as.character(tagList(
        tags$h4(paste0(selectedCourt$station_name, ", ", selectedCourt$PROVINCE)),
        sprintf("%-40s: %d", "Case Inflow", 
                as.integer(selectedCourt$LAST_INFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Cases Outflow", 
                as.integer(selectedCourt$LAST_OUTFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Pending Cases (Ending)", 
                as.integer(selectedCourt$PENDING_ENDING_12_2015)), tags$br()
      ))    
    }
    else if (input$timescale == "2014") {
      content <- as.character(tagList(
        tags$h4(paste0(selectedCourt$station_name, ", ", selectedCourt$PROVINCE)),
        sprintf("%-40s: %d", "Case Inflow", 
                as.integer(selectedCourt$LAST_INFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Cases Outflow", 
                as.integer(selectedCourt$LAST_OUTFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Pending Cases (Ending)", 
                as.integer(selectedCourt$PENDING_ENDING_12_2014)), tags$br()
      ))    
    }
    else if (input$timescale == "2013") {
      content <- as.character(tagList(
        tags$h4(paste0(selectedCourt$station_name, ", ", selectedCourt$PROVINCE)),
        sprintf("%-40s: %d", "Case Inflow", 
                as.integer(selectedCourt$LAST_INFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Cases Outflow", 
                as.integer(selectedCourt$LAST_OUTFLOW_SUM)), tags$br(),
        sprintf("%-40s: %d", "Pending Cases (Ending)", 
                as.integer(selectedCourt$PENDING_ENDING_12_2013)), tags$br()
      ))    
    }
    
    leafletProxy("philippine_map") %>% 
      addPopups(lon, lat, content, layerId = station_name)
  }
  
  # Shows a time-series plot of pending cases at the end of the last twelve months from the last submitted date
  showTSPlot <- function(station_name) {
    
    selectedCourt <- data_caseload()[data_caseload()$station_name == station_name, ]
    
    temp_pe <- selectedCourt %>% 
      select(starts_with("PENDING_ENDING"), -contains("TOTAL")) %>% 
      select((which(names(.) == selectedCourt$LAST_PENDING) - 11):which(names(.) == selectedCourt$LAST_PENDING)) %>% 
      t() %>% 
      as.data.frame() %>% 
      rowid_to_column("month") %>%
      # mutate(month = rownames(.)) %>% 
      rename(`Pending Cases` = V1)
    
    beginning_label <- case_when(
      selectedCourt$LAST_SUBMIT_MONTH == 12 ~ 
        paste0("1/", selectedCourt$LAST_SUBMIT_YEAR),
      selectedCourt$LAST_SUBMIT_MONTH == 11 ~ 
        paste0("12/", as.numeric(selectedCourt$LAST_SUBMIT_YEAR)-1),
      selectedCourt$LAST_SUBMIT_MONTH < 11 ~ 
        paste0((selectedCourt$LAST_SUBMIT_MONTH-11)%%12,
               "/", as.numeric(selectedCourt$LAST_SUBMIT_YEAR)-1) )
      
    
    temp_pe %>% ggplot(aes(x = month, y = `Pending Cases`)) + 
      geom_line(size = 2, color = "steelblue") + 
      labs(x = "month", y = "Pending Cases (Ending)") +
      scale_x_continuous(breaks = c(1, 12),
                         labels = c(beginning_label, 
                                    paste0(selectedCourt$LAST_SUBMIT_MONTH, "/", selectedCourt$LAST_SUBMIT_YEAR))) +
      theme(axis.text.x = element_text(angle = 0, vjust=0.5, size = 8),
            legend.position = "bottom")
  }
  
  output$tsplot_pe <- renderPlot({
    event_marker <- input$philippine_map_marker_click
    event_action <- input$goto
    if (is.null(event_marker) & is.null(event_action))
      return(NULL)
    
    isolate({
      # showTSPlot(event_action$station_name)
      # print(event_marker$id)
      showTSPlot(event_marker$id)
    })
    
  })
  
  # When the map is clicked, initiate courtPopup() above
  observe({
    leafletProxy("philippine_map") %>% clearPopups()
    event <- input$philippine_map_marker_click

    if (is.null(event))
      return()

    isolate({
      # The station names are passed from marker_click as "id"
      courtPopup(event$lat, event$lng, event$id)
      showTSPlot(event$id)
      
    })
  })
  
  # Generate a summary report of the data table shown in the application
  output$report_summary <- downloadHandler(
    filename = "report_summary.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "OCA_report.Rmd")
      file.copy("OCA_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(df = selectedDF())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
   
  # Generate a detailed report on the selected rows of the data table
  output$report_detail <- downloadHandler(
    filename = "report_detail.html",
    content = function(file) {
      selected_courts <- data_caseload()[input$courtdt_rows_selected, ]
      tempReport <- file.path(tempdir(), "OCA_report_detail.Rmd")
      file.copy("OCA_report_detail.Rmd", tempReport, overwrite = TRUE)
      params <- list(df = selected_courts)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
}


shinyApp(ui = ui, server = server)
