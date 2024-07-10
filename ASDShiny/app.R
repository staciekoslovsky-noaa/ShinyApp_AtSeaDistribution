library(shiny) #add install.packages above later?
library(shinyjs)
library(leaflet)
library(sf)
library(tidyverse)
library(shinydashboard)
library(leaflet.extras)

source("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/ASDShiny/helper_functions.R")
#load_all_filest("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data")

clicks <- data.frame(lat = numeric(), lng = numeric(), .nonce = numeric())



# UI
ui <- dashboardPage(
  title = "At Sea Densities of Marine Mammals",
  dashboardHeader(title = "NOAA "),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
      ),
    
    sidebarMenu(
      menuItem("About NOAA", tabName = "aboutpg", icon = icon("about")),
      menuItem("How to Use", tabName = "widgets", icon = icon("th")),
      menuItem("Species", tabName = "specmap", icon = icon("otter", lib = "font-awesome")),
      menuItem("Datasets for Download", tabname = "dfd"),
      menuItem("Licenses", tabname = "lic")
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'aboutpg',
            h2("About NOAA/Alaska Fisheries/This app"),
            p("Cool things here.")
                ),
        tabItem(tabName = 'widgets',
                h2("How to Use the Tool"),
                p("To use this tool...")
        ),
        tabItem(tabName = "specmap",
          selectInput("mapselect", "Select Marine Mammal", choices = c("Fur Seals", "Bearded Seals", "Steller Sea Lion"), downloadButton("downloadData")),
          leafletOutput(outputId = "map", width="100%"),
          actionButton("use_clik_loc", "Check loc")        
        )
    )))
  


# Define server logic
server <- function(input, output, session) {
    #BS_grid_sf <- Sample_data[["BS_grid_sf"]]
    
    #lines below taken from harbor seal app
    abund_bins <- c(0, 10, 100, 250, 500, 1000, 2500, 5000, 12000) 
    pal <- colorBin(
      palette = "inferno",
      reverse = TRUE,
      #domain = na.omit(survey_polygons$abund_est),
      bins = abund_bins,
      pretty = FALSE,
      na.color = "#00000000"
    )
    #end of this code
  

    for (species in names(Sample_data)){
      #print(species)
      species_data <- Sample_data[[species]]
      if (inherits(species_data, "matrix") || inherits(species_data, "array")){
        next
      }
      if (!inherits(species_data, "sf")) #&& !(inherits(species_data, "matrix"))){
        {species_data <- st_as_sf(species_data)
      }
      #print(st_crs(Sample_data[[species]]))
      current_crs <- st_crs(species_data)
      species_data <- st_transform(species_data, 4326)
        
      if (!all(st_is_valid(species_data))) {
          species_data <- st_make_valid(species_data)
        }
      assign(species, species_data)
    }
      
    # Move BS_grid_sf polygons across dateline - did not work 
    #BS_grid_sf <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
    BS_grid_sf$geom <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
    POP_hexagons_sf$geometry <- (sf::st_geometry(POP_hexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)
    SSL_grid_sf$x <- (sf::st_geometry(SSL_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)

    
    map_data <- reactive({
      switch(input$mapselect, 
             'Fur Seals' = list(data = POP_hexagons_sf, fillColor = ~colorNumeric('RdYlBu', CU)(CU), fillOpacity = 0.8, color = "black", weight = 0.5),
             'Bearded Seals' = list(data =BS_grid_sf, fillColor = ~colorNumeric('RdYlBu', BS2)(BS2), fillOpacity = 0.8, color = "black", weight = 1), # Basic color to ensure visibility
             'Steller Sea Lion' = list(data = SSL_grid_sf, fillColor = ~colorNumeric('RdYlBu', SSL_POP_ests)(SSL_POP_ests), fillOpacity = 0.8, color = "black", weight = 0.5))
    })
    
    output$map <- renderLeaflet({
      map_info <- map_data()
      leaflet(map_info$data) %>%
        addTiles() %>%
        addPolygons(fillColor = map_info$fillColor, fillOpacity = 0.8, opacity = 0, color = map_info$color, weight = 1) %>%
        addDrawToolbar(
          polylineOptions = drawPolylineOptions(),
          polygonOptions = drawPolygonOptions(),
          circleOptions = drawCircleOptions(),
          rectangleOptions = drawRectangleOptions(),
          markerOptions = drawMarkerOptions(),
          editOptions = editToolbarOptions(edit = FALSE, 
                                           selectedPathOptions = FALSE, 
                                           remove = TRUE)
        ) %>%
        addLayersControl(
          overlayGroups = c("draw"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(208, 64, 3) %>%
        #fitBounds(-179, 48, -140, 73)
        addLegend("bottomleft",
                  pal = pal,
                  values = abund_bins,
                  title = "Abundance:") 
    })

    observeEvent(input$use_clik_loc, {
      last_click <- isolate(as.data.frame(input$map_click))
      clicks <<- clicks |>
        bind_rows(last_click)
      print(clicks)
    })}

# Run the application 
shinyApp(ui = ui, server = server)
