library(shiny)
library(tidyverse)
library(pryr)
library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(DT)
library(tools)

# Define UI for data upload app ----
ui <- shinyUI(fluidPage(
  
  # App title ----
  titlePanel("Movia - Costing by Zone"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3, 
      
      # Input: Upload GTFS ----
      fileInput("zip", "Upload GTFS",
                multiple = FALSE,
                accept = c(".zip")),
      
      # Input: Upload Excel with regions ----
      fileInput("regions", "Excel with Regions",
                multiple = FALSE,
                accept = c(".xlsx")),
      
      # Horizontal line ----
      tags$hr(),
      
      #Hourly cost
      numericInput(inputId = "cost", label = "Hourly cost:", value = 100),
      
      tags$hr(),
      
      #Group by Zone, Line or Day type
      selectInput(inputId = "selected_group",
                  label = "Group by:",
                  choices = c("Zone" = 'zone',
                              "Route" = 'route',
                              "Day type" = 'day_type'
                    
                  ),
                  selected = "zone",
                  multiple = TRUE,
                  selectize = TRUE),
      
      # Line filter
      uiOutput("routes_selected_dt"),
      
      tags$hr(),
      
      # Select which types of day types to see
      checkboxGroupInput(inputId = "selected_service",
                         label = "Select day type(s):",
                         choices = c("Weekday" = 1, 
                                     "Saturday" = 2, 
                                     "Sunday" = 3),
                         selected = 1),
      
      #Link to Remix
      tags$a("Go to Remix", href = "https://www.remix.com/")
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      #Create different tabs
      tabsetPanel(type = "tabs",
                  id = "tabspanel",
                  tabPanel(title = "Yearly values", 
                           br(),
                           fluidRow(
                             column(2,
                                    numericInput(inputId = "weekdays", label = "Weekdays", value = 255) 
                             ),
                             column(2,
                                    numericInput(inputId = "saturdays", label = "Saturdays", value = 55) 
                             ),
                             column(2,
                                    numericInput(inputId = "sundays", label = "Sundays", value = 55) 
                             ),
                             column(1, offset = 4,
                                    # Download bUtton
                                    downloadButton("downloadYearlyData", "Download") 
                             )
                           ),
                           
                           fluidRow(
                             tags$hr(),
                             DT:: dataTableOutput(outputId = "group_selected_yearly_dt")
                           )
                           
                           ),
                  
                  tabPanel(title = "Daily values", 
                           br(),
                           fluidRow(
                             column(1, offset = 10,
                                         downloadButton("downloadData", "Download")
                                       )
                           ),
                            tags$hr(),
                            DT:: dataTableOutput(outputId = "group_selected_dt")
                          )
      )
      
      
      
      
    )
    
  )
))

# Define server logic to read selected file ----
options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output, session) {
  
  #get the GTFS path to use it later
  gtfs <- reactive({
    req(input$zip)
    input$zip$datapath
  })
  
  #Create a temp directory
  exdir <- reactive({
    req(gtfs())
    substring(gtfs(), 1, nchar(gtfs())-4)
  })
  
  #Get the list of files from the GTFS
  files <- reactive({
    req(exdir())
    unzip(gtfs(), list = FALSE, exdir = exdir())
  })
  
  #Define all the data frames I need
  #Create the path to find the files
  routes <- reactive({
    req(files())
    routes_path <- paste(exdir(), 'routes.txt', sep = '/')
    read_csv(routes_path)
  }) 
  
  all_routes <- reactive({
    req(routes())
    sort(unique(routes()$route_long_name))
  }) 
  
  trips <- reactive({
    req(files())
    trips_path <- paste(exdir(), 'trips.txt', sep = '/')
    read_csv(trips_path)
  }) 
  
  stops <- reactive({
    req(files())
    stops_path <- paste(exdir(), 'stops.txt', sep = '/')
    read_csv(stops_path)
  }) 
  
  #Get the Excel file
  regions <- reactive({
    req(input$regions)
    read_excel(input$regions$datapath) %>% 
      select(#TariffZoneKey,
             TariffZoneDisplayName,
             TariffZoneNumber, 
             #MunicipalityDisplayName,
             #MunicipalityCode, 
             #StopPointId, 
             #StopPointDisplayName, 
             StopPointNumber,
             #StopPointName, 
             StopPointTypeCode, 
             IsCurrent) %>% 
      filter(IsCurrent == 1) %>% 
      filter(StopPointTypeCode == 'BUSSTOP') %>% 
      select(-StopPointTypeCode, - IsCurrent)
  })
  
  #Now that I have all the files I read and built stop_times
  stop_times <- reactive({
    req(files())
    stop_times_path <- paste(exdir(), 'stop_times.txt', sep = '/')
    read_csv(stop_times_path, col_types= cols(arrival_time = col_character(), departure_time = col_character())) %>% 
      left_join(trips()) %>%
      left_join(stops()) %>% 
      mutate(StopPointNumber = as.numeric(stop_id)) %>% 
      left_join(regions()) %>% 
      select(-block_id) 
  }) 
  
  #Start building the data frama to find the breaks
  #See when we change trip and tarif zone
  filtered <- reactive({
    req(regions(), stop_times())
    stop_times() %>%
      select(trip_id, TariffZoneNumber)
  })
  
  trips_ids <- reactive({
    req(regions(), stop_times())
    filtered() %>% 
      #trip_id from the previous stop
      mutate(prev_trip = ifelse(is.na(lag(trip_id)), 
                                filtered()[nrow(filtered()), 1], 
                                lag(trip_id))) %>% 
      #trip_id from the next stop
      mutate(next_trip = ifelse(is.na(lead(trip_id)), 
                                filtered()[1, 1], 
                                lead(trip_id))) %>% 
      #TariffZoneNumber of the previous stop
      mutate(prev_zone = ifelse(is.na(lag(TariffZoneNumber)),
                                TariffZoneNumber,
                                lag(TariffZoneNumber)) ) %>% 
      #TariffZoneNumber of the next stop
      mutate(next_zone = ifelse(is.na(lead(TariffZoneNumber)),
                                TariffZoneNumber,
                                lead(TariffZoneNumber)) )
  })
  
  #Find the breaks
  breaks_1 <- reactive({
    req(trips_ids())
    stop_times()[(
      trips_ids()$trip_id != trips_ids()$prev_trip | 
        trips_ids()$trip_id != trips_ids()$next_trip |
        trips_ids()$TariffZoneNumber != trips_ids()$prev_zone |
        trips_ids()$TariffZoneNumber != trips_ids()$next_zone 
    ), ]
    
  })
  
  #We'll only keep the ones that are not NA.
  breaks_2 <- reactive({
    req(breaks_1())
    breaks_1() %>% 
      filter(is.na(TariffZoneNumber) != 'TRUE')
  })
  
  breaks <-  reactive({
    req(breaks_2())
    #calculate running time for each
    hours <- as.integer(substr(breaks_2()$arrival_time, 1, 2))
    minutes <- as.integer(substr(breaks_2()$arrival_time, 4, 5))/60
    breaks_2() %>% 
      mutate(hours_minutes = hours + minutes) %>% 
      mutate(travel_hours = ifelse(trip_id == lead(trip_id),round((lead(hours_minutes) - hours_minutes),digits = 2), 0)) %>% 
      select(trip_id, route_id, TariffZoneDisplayName, travel_hours,-timepoint, - direction_id, -hours_minutes) %>% 
      left_join(trips()) %>% 
      select(trip_id, route_id, service_id, TariffZoneDisplayName, travel_hours) %>% 
      left_join(routes()) %>% 
      select('route' = route_long_name,
              'trip' = trip_id,
              'zone' = TariffZoneDisplayName,
              'hours' = travel_hours,
              'day_type' = service_id)
  })
  
  hourly_cost <- reactive({
    input$cost
  })
  
  #Daily table - Group by selectd columns
  group_selected <-  reactive({
    req(input$selected_group, breaks()) # ensure availablity of value before proceeding
    if(!is.null(input$selected_route)){
      breaks() %>% 
        filter(day_type %in% input$selected_service) %>% 
        filter(route %in% input$selected_route) %>% 
        mutate(cost = hours * hourly_cost()) %>% 
        group_by_at(vars(input$selected_group)) %>% 
        summarise(hours = sum(hours), cost = sum(cost))
    } else {
      breaks() %>% 
        filter(day_type %in% input$selected_service) %>% 
        mutate(cost = hours * hourly_cost()) %>% 
        group_by_at(vars(input$selected_group)) %>% 
        summarise(hours = sum(hours), cost = sum(cost))
    }
    
  })
  
  weekday <- reactive({
    input$weekdays
  })
  
  saturdays <- reactive({
    input$saturdays
  })
  
  sundays <- reactive({
    input$sundays
  })
  
  #Yearly reference table
  yearly_table <- reactive({
    req(breaks())
    breaks() %>% 
      mutate(hours = case_when(
        day_type == 1 ~ hours * weekday(),
        day_type == 2 ~ hours * saturdays(),
        day_type == 3 ~ hours * sundays()
      )) %>% 
      group_by(route, zone, day_type) %>% 
      summarize(hours = sum(hours))
    
  })
  
  #Yearly table - Group by selectd columns
  group_selected_yearly <-  reactive({
    req(yearly_table()) # ensure availablity of value before proceeding
    if(!is.null(input$selected_route)){
      yearly_table() %>% 
        filter(day_type %in% input$selected_service) %>% 
        filter(route %in% input$selected_route) %>% 
        mutate(cost = hours * hourly_cost()) %>% 
        group_by_at(vars(input$selected_group)) %>% 
        summarise(hours = sum(hours), cost = sum(cost))
    } else {
      yearly_table() %>% 
        filter(day_type %in% input$selected_service) %>% 
        mutate(cost = hours * hourly_cost()) %>% 
        group_by_at(vars(input$selected_group)) %>% 
        summarise(hours = sum(hours), cost = sum(cost))
    }
    
  })
  
  
  #Render list of routes
  output$routes_selected_dt <- renderUI({
      selectInput(inputId = "selected_route",
                  label = "Filter lines:",
                  choices = all_routes() ,
                  selected = '',
                  multiple = TRUE,
                  selectize = TRUE)
  })
  
  # Daily Information output
  output$group_selected_dt <- renderDataTable({
    if(is.null(gtfs()))
      return ()
    datatable(data =  group_selected(),
              options = list(pageLength = 10, lengthMenu = c(10, 20, 40)), 
              rownames = FALSE)
  })
  
  # Yearaly Information output
  output$group_selected_yearly_dt <- renderDataTable({
    if(is.null(gtfs()))
      return ()
    datatable(data =  group_selected_yearly(),
              options = list(pageLength = 10, lengthMenu = c(10, 20, 40)), 
              rownames = FALSE)
  })
  
  # Downloadable daily csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(paste("Daily_Costing", date(),sep = "-"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(group_selected(), file, row.names = TRUE)
    }
  )
  
  # Downloadable yearly csv of selected dataset
  output$downloadYearlyData <- downloadHandler(
    filename = function() {
      paste(paste("Yearly_Costing", date(),sep = "-"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(group_selected_yearly(), file, row.names = TRUE)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)