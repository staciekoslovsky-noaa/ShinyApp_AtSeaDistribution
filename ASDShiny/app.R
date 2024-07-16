library(shiny) #add install.packages above later?
library(shinyjs)
library(leaflet)
library(sf)
library(tidyverse)
library(shinydashboard)
library(leaflet.extras)
library(leaflet.mapboxgl)
library(shinySearchbar)
library(fresh)
library(shinyWidgets)
library(htmltools)

#May need to change pathway, sorry!
source("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/ASDShiny/helper_functions.R")
load_all_filest("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data")

load("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data/POPhexagons_sf.rda")
load("../data/Sample_data_for_portal.RData")

# species_list <- c("Northern Minke Whale", "Fin Whale", "Northern Fur Seal", "Bearded Seal", "Steller Sea Lion", "Sea Otter", 
#                    "Gray Whale", "Pacific White-Sided Dolphin", "Humpback Whale", "Killer Whale", "Walrus", "Dall's Porpoise",
#                  "Sperm Whale", "Harbor Porpoise", "Harbor Seal")

species_list <- list("Northern Minke Whale" = BA_MCMC,
                  "Fin Whale" = BP_MCMC,
                  "Northern Fur Seal" = CU_MCMC,
                  #"Bearded Seal" = EB_MCMC,
                  "Steller Sea Lion" = EJ_MCMC,
                  "Sea Otter" = EL_MCMC,
                  "Gray Whale" = ER_MCMC,
                  "Pacific White-Sided Dolphin" = LO_MCMC,
                  "Humpback Whale" = MN_MCMC,
                  "Killer Whale" = OO_MCMC,
                  "Walrus" = OR_MCMC,
                  "Dall's Porpoise" = PD_MCMC,
                  "Sperm Whale" = PM_MCMC,
                  "Harbor Porpoise" = PP_MCMC,
                  "Harbor Seal" = PV_MCMC)

# UI
ui <- dashboardPage(
  title = "At Sea Densities of Marine Mammals",
  dashboardHeader(title = "NOAA "),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
      ),
    sidebarMenu(
      menuItem("About This Tool", tabName = "aboutpg", icon = icon("about")),
      menuItem("How to Use", tabName = "widgets", icon = icon("th")),
      menuItem("Species", tabName = "specmap", icon = icon("otter", lib = "font-awesome")),
      menuItem("Other", tabName = "othr"),
      menuItem("Licenses", tabName = "lic")
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'aboutpg',
            wellPanel(
              h2(strong(div("About This Tool", style = 'color: #011f4b')))
              ),
            wellPanel(
                h3(purp, width = "100%"),
                p(information),
                tags$figure(
                  class = "centerFigure",
                  tags$img(
                    src = "sampleseal.jpg",
                    width = '100%',
                    alt = "Picture of a male ribbon seal"
                  ),
                  tags$figcaption("NOAA Fisheries/Josh M London")
                )
                
                )),
        tabItem(tabName = 'widgets',
            wellPanel(
              (h2(strong(div("How to Use", style = 'color: #011f4b'))))),
            wellPanel(
              p(toolinfo),
              p(toolinfo2)
            )),
        tabItem(tabName = "specmap",
          fluidRow(
            column(9, 
                   leafletOutput(outputId = "map", width="100%")
            ),
            column(3,
              wellPanel(
                h3('Other Information (estimates, etc'),
                selectizeInput("mapselect", "Select Marine Mammal", choices = names(species_list)),
                sliderInput('ci', 'Cells of Interest', min = 1, max = 200, value = 1)
              )
            )
          ),
          fluidRow(
            wellPanel(
              h3('Download or Upload Shape Data'),
              
              downloadButton(outputID = "downloadData", "Generate Shapes"),
              fileInput('drawfile', "Input Shapefile", accept = '.shp', multiple = TRUE)
            )
          )
        ),
        tabItem(tabName = "othr",
          wellPanel(
            h2('For more information, contact: etc')
          )
    )))
)

# Define server logic
server <- function(input, output, session) {

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
  
  POPhexagons_sf <- st_transform(POPhexagons_sf, 4326)
    
    
  # Move BS_grid_sf polygons across dateline - did not work 
  #BS_grid_sf <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
  BS_grid_sf$geom <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
  POP_hexagons_sf$geometry <- (sf::st_geometry(POP_hexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)
  SSL_grid_sf$x <- (sf::st_geometry(SSL_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
  POPhexagons_sf$geometry <- (sf::st_geometry(POPhexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)
  
  #Sample for one species - Northern Minke Whale - hopefully a function can do this easily instead.
  filtered_sf <- POPhexagons_sf %>% filter(!is.na(CU))
  
  #Columns containing abundance of species ONLY. 
  col_to_filter <- POPhexagons_sf[,5:18]
  #filtered_list <- filter_by_col(col_to_filter)
    
  map_data <- reactive({
    switch(input$mapselect, 
           'Northern Minke Whale' = list(data = (filtered_sf), fillColor = ~colorNumeric('inferno', BA_MCMC[,1])(BA_MCMC[,1]), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Fin Whale' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', BP_MCMC[,1])(BP_MCMC[,1]), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Northern Fur Seal' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', CU_MCMC[,1])(CU_MCMC[,1]), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Bearded Seal' = list(data = BS_grid_sf, fillColor = ~colorNumeric('inferno', BS2)(BS2), fillOpacity = 0.8, color = "black", weight = 1), # Basic color to ensure visibility
           'Steller Sea Lion' = list(data = SSL_grid_sf, fillColor = ~colorNumeric('inferno', SSL_POP_ests)(SSL_POP_ests), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Sea Otter' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', )),
           'Gray Whale' = list(),
           'Pacific White-Sided Dolphin' = list()
           
           )
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
      # addLegend("bottomright",
      #           pal = reactive_pal(),
      #           values = reactive_species_data()$V1,
      #           title = "Abundance:") %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 250))
  })

  
  observeEvent(input$mapselect, {
    selected_species <- input$mapselect
    species_data <- species_list[[selected_species]]
    
    # debugging for matrix.
    if (is.null(species_data) || !is.matrix(species_data)) {
      print("Not a matrix")
    }
    
    pal <- colorBin(
      palette = "inferno",
      domain = species_data[,1],
      bins = 7,
      pretty = FALSE,
      na.color = "#00000000"
    )
    
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend("bottomright", pal = pal, values = species_data[,1], title = selected_species)
  })

  # Provides coordinates for markers when you place them on the map. 
  #Currently does not delete together - FIX
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$properties$feature_type == "marker") {
      leafletProxy("map") %>%
        #clearControls() %>%
        addLabelOnlyMarkers(
          lng <- feature$geometry$coordinates[[1]],
          lat <- feature$geometry$coordinates[[2]],
          label = sprintf("Lat: %0.5f, Lng: %0.5f", lat, lng),
          labelOptions = labelOptions(noHide = TRUE, direction = 'top', offset = c(0, -10))
        )
    }})

  data <- reactive({
    map_data()$data
  })
  
  output$downloadData <- downloadHandler(
    filename = 'map.png',
    #content = function(file) {
      
    #}
  )
  
  observeEvent(input$drawfile, {
    drawfile <- input$drawfile
    #validate(need(ext == 'shp', 'Please upload a valid shapefile in one of the following formats: .shp, ...etc.'))
  })
}


# Run the application 
#shinyApp(ui = ui, server = server)
#temporary due to bug
shinyApp(ui = ui, server = server,options=c(launch.browser = .rs.invokeShinyPaneViewer))

