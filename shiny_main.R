library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(magrittr)
library(readr)
library(stringr)
library(dplyr)
library(RColorBrewer)
# library(rnaturalearth)
# library(rnaturalearthdata)
library(DT)
library(vroom)
library(shinyWidgets)


dt <- vroom("Map_data.csv", locale = locale(encoding = "UTF-16"))
dt$`Date Announced (MM/DD/YYYY)1` <- as.Date(dt$`Date Announced (MM/DD/YYYY)1`, format = "%m/%d/%Y")
dt <- dt[!is.na(dt$Latitude) & !is.na(dt$Longitude), ]
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
        pickerInput("map_view", "Map View", choices = c("Sector", "Disadvantaged Communities", "Congressional Districts"), multiple = FALSE),
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
  
  
  ffiltered_data <- reactive({
    data <- dt
    
    if (!is.null(input$representative) && length(input$representative) > 0) {
      data <- data[data$Representative %in% input$representative, ]
    }
    if (!is.null(input$state) && length(input$state) > 0) {
      data <- data[data$State %in% input$state, ]
    }
    if (!is.null(input$project_type) && length(input$project_type) > 0) {
      data <- data[data$`Project Type` %in% input$project_type, ]
    }
    if (!is.null(input$sector) && length(input$sector) > 0) {
      data <- data[data$Sector %in% input$sector, ]
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
        paste0(formatC(sum(data$`Announced Jobs`, na.rm = TRUE), format = "f", big.mark = ",", digits = 0))
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

  observeEvent(input$reset_filters, {
    updatePickerInput(session, "representative", selected = character(0))
    updatePickerInput(session, "state", selected = character(0))
    updatePickerInput(session, "project_type", selected = character(0))
    updatePickerInput(session, "sector", selected = character(0))
    updateSliderInput(session, "date_announced", value = c(min_date, max_date))
  })

  observe({
    data <- filtered_data()
    
    size_attr <- if (input$attribute == "By Investment") {
      data$`Announced Investment`
    } else {
      data$`Announced Jobs`
    }
    
    size_attr[is.na(size_attr)] <- 0
    radius <- scales::rescale(size_attr, to = c(8, 35), na.rm = TRUE)
    
    color_data <- if (input$map_view == "Congressional Districts") {
      data$`Rep Party`
    } else if (input$map_view == "Disadvantaged Communities") {
      data$Disadvantaged
    } else if (input$map_view == "Sector") {
      data$Sector
    } else {
      rep("#2A81CB", nrow(data))
    }
    
    colors <- if (input$map_view == "Congressional Districts") {
      case_when(
        color_data == "Democrat" ~ "blue",
        color_data == "Republican" ~ "red",
        color_data == "TBD" ~ "gray",
        color_data == "Vacant" ~ "black",
        TRUE ~ "white"
      )
    } else if (input$map_view == "Disadvantaged Communities") {
      case_when(
        color_data == "Yes" ~ "lightblue",
        color_data == "No" ~ "black",
        color_data == "Location TBD" ~ "gray",
        TRUE ~ "white"
      )
    } else if (input$map_view == "Sector") {
      case_when(
        color_data == "Battery/Storage" ~ "darkblue",
        color_data == "Clean Vehicles" ~ "skyblue",
        color_data == "Grid/Electrification" ~ "darkorange",
        color_data == "Solar" ~ "tan",
        color_data == "Wind" ~ "darkgreen",
        color_data == "Other" ~ "gray",
        TRUE ~ "white"
      )
    } else {
      rep("#2A81CB", nrow(data))
    }
    
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = radius,
        color = colors,
        stroke = TRUE,
        weight = 1.75,
        fillColor = colors,
        fillOpacity = 0,
        opacity = 1
      )
  })

  output$pie_chart <- renderPlot({
    data <- filtered_data()
    
    category_col <- switch(input$map_view,
                           "Sector" = "Sector",
                           "Disadvantaged Communities" = "Disadvantaged",
                           "Congressional Districts" = "Rep Party"
    )
    
    category_data <- data[[category_col]]
    category_data[is.na(category_data) | category_data == ""] <- "Unknown"
    
    pie_data <- as.data.frame(table(category_data))
    names(pie_data) <- c("Category", "Count")
    pie_data$Percent <- round(pie_data$Count / sum(pie_data$Count) * 100, 1)
    
    pie_labels <- paste0(pie_data$Category, " (", pie_data$Percent, "%)")
    
    pie_colors <- if (input$map_view == "Congressional Districts") {
      sapply(pie_data$Category, function(x) {
        if (x == "Democrat") "blue"
        else if (x == "Republican") "red"
        else if (x == "TBD") "gray"
        else if (x == "Vacant") "black"
        else "white"
      })
    } else if (input$map_view == "Disadvantaged Communities") {
      sapply(pie_data$Category, function(x) {
        if (x == "Yes") "lightblue"
        else if (x == "No") "black"
        else if (x == "Location TBD") "gray"
        else "white"
      })
    } else if (input$map_view == "Sector") {
      sapply(pie_data$Category, function(x) {
        if (x == "Battery/Storage") "darkblue"
        else if (x == "Clean Vehicles") "skyblue"
        else if (x == "Grid/Electrification") "darkorange"
        else if (x == "Solar") "tan"
        else if (x == "Wind") "darkgreen"
        else if (x == "Other") "gray"
        else "white"
      })
    }
    
    pie(pie_data$Count, labels = pie_labels, col = pie_colors,
        main = paste("Distribution by", input$map_view))
  })
  
}

shinyApp(ui, server)
