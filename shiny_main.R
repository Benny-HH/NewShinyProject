library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(magrittr)
library(readr)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(DT)
library(vroom)
library(shinyWidgets)


dt <- vroom("Map_data.csv", locale = locale(encoding = "UTF-16"))
dt$`Date Announced (MM/DD/YYYY)1` <- as.Date(dt$`Date Announced (MM/DD/YYYY)1`, format = "%m/%d/%Y")
min_date <- min(dt$`Date Announced (MM/DD/YYYY)1`, na.rm = TRUE)
max_date <- max(dt$`Date Announced (MM/DD/YYYY)1`, na.rm = TRUE)


ui <- fluidPage(
  titlePanel(
    tagList(
      h1("MAJOR CLEAN ENERGY PROJECTS ANNOUNCED BY PRIVATE SECTOR"),
      tags$h5("This map began tracking projects announced by the private sector after the Inflation Reduction Act (IRA) was passed in August 2022. Zoom in for more detail and explore using dropdown navigation, filters, and interactive charts.")
    )
  ),
  
  
  fluidRow(
    column(
      width = 2,
      wellPanel(
        pickerInput("map_view", "Map View", choices = c("Sector", "Disadvantaged Communities", "Energy Communities", "Congressional Districts"), multiple = FALSE),
        pickerInput("attribute", "Project Size by Attribute", choices = c("By Investment", "By Job"), multiple = FALSE),
        pickerInput("show_hide", "Show/Hide Project Type", choices = c("Show", "Hide"), multiple = FALSE)
      )
    ),
    
    column(
      width = 8,
      leafletOutput("map", height = 500)
    ),
    
    column(
      width = 2,
      wellPanel(
        pickerInput("representative", "Representative:", choices = unique(dt$Representative), multiple = TRUE),
        pickerInput("state", "State:", choices = unique(dt$State), multiple = TRUE),
        pickerInput("project_type", "Project Type:", choices = unique(dt$`Project Type`), multiple = TRUE),
        pickerInput("sector", "Sector:", choices = unique(dt$Sector), multiple = TRUE),
        sliderInput("date_announced", "Date Announced:", 
                    min = min_date, max = max_date, 
                    value = c(min_date, max_date), timeFormat = "%b %d, %Y"),
        actionButton("reset_filters", "Reset Filters", icon = icon("redo"))
      )
    )
  ),
  
  fluidRow(
    column(
      width = 4,
      plotOutput("pie_chart", height = 300),
      br(),
      uiOutput("pie_legend")
    ),
    
    column(
      width = 4,
      DTOutput("summary_table")
    ),
    
    column(
      width = 4,
      DTOutput("project_list")
    )
  )
  
)

server <- function(input, output, session) {
  
  
  filtered_data <- reactive({
    data <- dt

    if (!is.null(input$representative) && input$representative != "") {
      data <- data[data$Representative == input$representative, ]
    }
    if (!is.null(input$state) && input$state != "") {
      data <- data[data$State == input$state, ]
    }
    if (!is.null(input$project_type) && input$project_type != "") {
      data <- data[data$`Project Type` == input$project_type, ]
    }
    if (!is.null(input$sector) && input$sector != "") {
      data <- data[data$Sector == input$sector, ]
    }
    if (!is.null(input$date_announced)) {
      data <- data[
        data$`Date Announced (MM/DD/YYYY)1` >= input$date_announced[1] &
          data$`Date Announced (MM/DD/YYYY)1` <= input$date_announced[2], 
      ]
    }
  
    return(data)
  })
  

  
  output$project_list <- renderDataTable({
    dt_projects <- filtered_data()[, c("Representative", "Developer", "City", "Project Type", "Announced Jobs", "Announced Investment")]
    datatable(dt_projects)
  })
  
  output$summary_table <- renderDataTable({
    data <- filtered_data()
    
    summary_df <- data.frame(
      Metric = c("Announced Projects", "Announced Investment", "Announced Jobs"),
      Value = c(
        nrow(data),
        paste0("$", formatC(sum(data$`Announced Investment`, na.rm = TRUE), format = "f", big.mark = ",", digits = 0)),
        paste0("$", formatC(sum(data$`Announced Jobs`, na.rm = TRUE), format = "f", big.mark = ",", digits = 0))
      )
    )
    
    datatable(summary_df, rownames = FALSE, options = list(dom = 't'))
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl = FALSE,
      zoomControl = TRUE,
      scrollWheelZoom = TRUE,
      dragging = TRUE,
      touchZoom = TRUE,
      doubleClickZoom = TRUE,
      zoomSnap = 0.25, 
      zoomDelta = 0.25,
      minZoom = 4,
      maxZoom = 8
    )) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)
  })

  observe({
    data <- filtered_data()
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6,
        color = "#2A81CB",
        stroke = TRUE,
        weight = 3,
        fill = FALSE,
        fillOpacity = 0
      )
  })
  
}

shinyApp(ui, server)
