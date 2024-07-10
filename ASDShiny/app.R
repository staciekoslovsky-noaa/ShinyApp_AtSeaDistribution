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
          actionButton("getshape", "Generate Shapes"),
          fileInput('drawfile', "Input Drawing CSV", accept = '.csv')
        )
    )))
  


# Define server logic
server <- function(input, output, session) {
  
  #lines below taken from harbor seal app
  abund_bins <- c(0.1, 0.08, 0.06, 0.04, 0.02, 0.01, 0.001, 0) #not accurate, need to change later
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
           'Fur Seals' = list(data = POP_hexagons_sf, fillColor = ~colorNumeric('inferno', CU)(CU), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Bearded Seals' = list(data =BS_grid_sf, fillColor = ~colorNumeric('inferno', BS2)(BS2), fillOpacity = 0.8, color = "black", weight = 1), # Basic color to ensure visibility
           'Steller Sea Lion' = list(data = SSL_grid_sf, fillColor = ~colorNumeric('inferno', SSL_POP_ests)(SSL_POP_ests), fillOpacity = 0.8, color = "black", weight = 0.5))
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
        rectangleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, 
                                         selectedPathOptions = FALSE, 
                                         remove = TRUE)
      ) %>%
      addLayersControl(
        overlayGroups = c("draw"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(208, 64, 3) %>%
      addLegend("bottomright",
                pal = pal,
                values = abund_bins,
                title = "Abundance:") %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 250))
  })
  

  # Provides coordinates for markers when you place them on the map. 
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$properties$feature_type == "marker") {
      leafletProxy("map") %>%
        addLabelOnlyMarkers(
          lng <- feature$geometry$coordinates[[1]],
          lat <- feature$geometry$coordinates[[2]],
          label = sprintf("Lat: %0.5f, Lng: %0.5f", lat, lng),
          labelOptions = labelOptions(noHide = TRUE, direction = 'top', offset = c(0, -10))
        )
    }})

  # Eventually provides data for selected region, not yet(taken from Harbor Seal App)
  observeEvent(input$getshape, {
    drawn <- input$map_draw_new_feature
    polygon_coordinates <- do.call(rbind, lapply(drawn$geometry$coordinates[[1]], function(x){c(x[[1]][1],x[[2]][1])}))
    drawn_polygon <- data.frame(lat = polygon_coordinates[, 2], long = polygon_coordinates[, 1]) %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
    #found_in_bounds <- st_join(sf::st_set_crs(drawn_polygon, 4326), sf::st_set_crs(survey_polygons, 4326))
    
    #poly_filter <- found_in_bounds$polyid
    print('blahblah')
  })

  observeEvent(input$drawfile, {
    drawfile <- input$drawfile
    validate(need(ext == 'csv', 'Please upload a .csv file.'))
  })
}


# Run the application 
#shinyApp(ui = ui, server = server)
#temporary due to bug
shinyApp(ui = ui, server = server,options=c(launch.browser = .rs.invokeShinyPaneViewer))

