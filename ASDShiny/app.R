# Load all library packages
library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(tidyverse)
library(shinydashboard)
library(leaflet.extras)
library(leaflet.mapboxgl) #mapbox extension 
library(shinySearchbar) #nu
library(fresh) 
library(shinyWidgets)
library(htmltools)
library(htmlwidgets)
library(mapview)


#Currently local accessing files
source("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/ASDShiny/helper_functions.R")
load_all_filest("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data")

load("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data/POPhexagons_sf.rda")
load("../data/Sample_data_for_portal.RData")


#Add default map POPhexagons_sf 
species_list <- list("Northern Minke Whale" = BA_MCMC,
                  "Fin Whale" = BP_MCMC,
                  "Northern Fur Seal" = CU_MCMC,
                  #"Bearded Seal" = EB_MCMC, #currently using grid data 
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
ui <- shinydashboard::dashboardPage(
  #title = "At Sea Densities of Marine Mammals",
  
  # Hides warnings and errors in app
  
  # htmltools::tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }",),
  # 
  # Dashboard/sidebar visible on left side of screen. 
  dashboardHeader(title = "At Sea Densities of Marine Mammals"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
      ),
    sidebarMenu(
      menuItem("About This Tool", tabName = "aboutpg", icon = icon("about")),
      menuItem("How to Use", tabName = "widgets", icon = icon("th")),
      menuItem("Species", tabName = "specmap", icon = icon("otter", lib = "font-awesome")),
      menuItem("Methods", tabName = "metd"),
      menuItem("Licenses", tabName = "lic")
    )),
    dashboardBody(
      tabItems(
        
        # About the tool tab 
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
        
        # How to use/instructional tab
        tabItem(tabName = 'widgets',
            wellPanel(
              (h2(strong(div("How to Use", style = 'color: #011f4b'))))),
            wellPanel(
              p(tool_info), # Separated texts to allow for appropriate spacing.
              p(tool_info2),
              p(tool_info3),
              p(tool_info4)
            )),
        
        # Species density map
        tabItem(tabName = "specmap",
          fluidRow(
            column(9, 
                   leafletOutput(outputId = "map", width="100%")
            ),
            column(3,
              wellPanel(
                
                # Customization features in map
                h3('Customize Map'),
                selectizeInput("mapselect", "Select Marine Mammal", choices = sort(names(species_list)),
                sliderInput('ci', 'Cells of Interest', min = 1, max = 200, value = 1)
              )
            )
          )),
          fluidRow(
            wellPanel(
              
              #Shapefile upload/download UI 
              h3('Download or Upload Shapefile'),
              #textInput('downloadShp', 'Filename:', value = 'Shapes.zip'),
              downloadButton('downloadData', "Download Shapefile"),
              fileInput('drawfile', "Upload Shapefile", accept = '.zip', multiple = TRUE)
            )
          )
        ),
        tabItem(tabName = "metd",
          wellPanel(
            h2(strong(methods_title))
          ),
          wellPanel(
            p(methods_info),
            h3('For more information, contact: etc')
          )
        ),
        tabItem(tabName = 'lic',
          wellPanel(
            h2('licenses')
          ))
    ))
)

# Define server logic
server <- function(input, output, session) {

  #remove and change   
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
  
  # Converts starting projection to EPSG 4326 to be displayed onto base map.
  POPhexagons_sf <- sf::st_transform(POPhexagons_sf, 4326)
    
    
  # As Alaska is split by the international dateline, the following lines move 
  # the data across the dateline for a unified view. 
  
  BS_grid_sf$geom <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
  POP_hexagons_sf$geometry <- (sf::st_geometry(POP_hexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)
  SSL_grid_sf$x <- (sf::st_geometry(SSL_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
  POPhexagons_sf$geometry <- (sf::st_geometry(POPhexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)
  
  # Sample for one species (Northern Minke Whale) which filters the dataframe for
  # areas in which data is available fpr the selected species
  filtered_sf <- POPhexagons_sf %>% filter(!is.na(CU))
  
  # Columns containing only abundance of species.
  col_to_filter <- POPhexagons_sf[,5:18]
  #filtered_list <- filter_by_col(col_to_filter)
    
  # Reactive expression that updates map_data when a marine mammal species is
  # selected from "selectize" dropdown/searchbar.
  map_data <- shiny::reactive({
    switch(input$mapselect, 
           'Northern Minke Whale' = list(data = (filtered_sf), fillColor = ~colorNumeric('inferno', BA_MCMC[,1])(BA_MCMC[,1]), fillOpacity = 0.8, color = "black", weight = 0.5),
            #use the actual values in POPhexagons_sf 
           'Fin Whale' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', BP_MCMC[,1])(BP_MCMC[,1]), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Northern Fur Seal' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', CU_MCMC[,1])(CU_MCMC[,1]), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Bearded Seal' = list(data = BS_grid_sf, fillColor = ~colorNumeric('inferno', BS2)(BS2), fillOpacity = 0.8, color = "black", weight = 1), # Basic color to ensure visibility
           #'Steller Sea Lion' = list(data = SSL_grid_sf, fillColor = ~colorNumeric('inferno', SSL_POP_ests)(SSL_POP_ests), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Steller Sea Lion' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', EJ_MCMC[,1])(EJ_MCMC[,1]), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Sea Otter' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', )),
           'Gray Whale' = list(),
           'Pacific White-Sided Dolphin' = list()
    )
  })
  
  # Output leaflet map
  output$map <- leaflet::renderLeaflet({
    map_info <- map_data()
    
    # Data layer obtained from selected species
    leaflet(map_info$data) %>%
      addTiles() %>%
      addPolygons(fillColor = map_info$fillColor, fillOpacity = 0.8, opacity = 0, color = map_info$color, weight = 1) %>%
      addDrawToolbar(
        polygonOptions = drawPolygonOptions(),
        circleOptions = drawCircleOptions(),
        rectangleOptions = drawRectangleOptions(),
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, 
                                         selectedPathOptions = FALSE, 
                                         remove = TRUE),
        targetGroup = "Shapes"
      ) %>%
      addLayersControl(
        
        overlayGroups = c("Shapes"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(208, 64, 3) %>%
      addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 250))
  })

  # Obtain corresponding MCMC matrix for selected species 
  shiny::observeEvent(input$mapselect, {
    selected_species <- input$mapselect
    species_data <- species_list[[selected_species]]
    
    # debugging for matrix.
    if (is.null(species_data) || !is.matrix(species_data)) {
      print("Not a matrix")
    }
    
    # Color palette, bincount, and other customizations for reactive legend
    pal <- leaflet::colorBin(
      palette = "inferno",
      domain = species_data[,1],
      bins = 7,
      pretty = FALSE,
      na.color = "#00000000"
    )
    
    # Clear controls every time a new species is selected with updated legend
    leaflet::leafletProxy("map") %>%
      clearControls() %>%
      addLegend("bottomright", pal = pal, values = species_data[,1], title = 'Abundance:')
  })

  # Provides coordinates for markers when you place them on the map. 
  # Currently does not delete together - FIX
  shiny::observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$properties$feature_type == "marker") {
      leaflet::leafletProxy("map") %>%
        #clearControls() %>%
        addLabelOnlyMarkers(
          lng <- feature$geometry$coordinates[[1]],
          lat <- feature$geometry$coordinates[[2]],
          label = sprintf("Lat: %0.5f, Lng: %0.5f", lat, lng),
          labelOptions = labelOptions(noHide = TRUE, direction = 'top', offset = c(0, -10))
        )
    }})
  
  shiny::observeEvent(input$drawfile, {
    drawfile <- input$drawfile
    files <- unzip(drawfile, list = TRUE)
    #zipped only
    validate(need(ext == ('.shp' || '.kmz'), 'Please upload a valid shapefile in one of the following formats: .shp, ...etc.'))
    print('upload succesful')
  })
  
  dl.y <- callModule(dlmodule, 'dlmodule1')

  
  data <- reactive({
    get(input$mapselect)
  })

}


dlmodule <- function(input, output, session){
  output$downloadData <- shiny::downloadHandler(
    filename = 'Shape.pdf',
    content = function(file){
      print('download triggered')
      #line of code map_shot/st_write - neither succesful so far
      print('file saved to', file)
    }
  )
}

# Run the application 
#shinyApp(ui = ui, server = server)
#temporary due to bug
shinyApp(ui = ui, server = server,options=c(launch.browser = .rs.invokeShinyPaneViewer))

