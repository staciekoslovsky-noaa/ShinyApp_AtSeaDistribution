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
library(tools)


#Currently local accessing files
source("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/ASDShiny/helper_functions.R")
source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/helper_functions.R')
#load_all_filest("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data")
# load("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data/POPhexagons_sf.rda")
 load(url('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/POPhex_MCMC.rda'))


# load("../data/Sample_data_for_portal.RData")


# Access via GitHub
load(url('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/POPhexagons_sf.rda'))
#load_all_files(urls)

species_list2 <- list("Northern Minke Whale" = POPhex_MCMC$Northern.Minke.Whale,
                      "Fin Whale" = POPhex_MCMC$Fin.Whale,
                      "Northern Fur Seal" = POPhex_MCMC$Northern.Fur.Seal,
                      #"Bearded Seal" = EB_MCMC, #currently using grid data
                      "Steller Sea Lion" = POPhex_MCMC$Steller.Sea.Lion,
                      "Sea Otter" = POPhex_MCMC$Sea.Otter,
                      "Gray Whale" = POPhex_MCMC$Gray.Whale,
                      "Pacific White-Sided Dolphin" = POPhex_MCMC$Pacific.White.Sided.Dolphin,
                      "Humpback Whale" = POPhex_MCMC$Humpback.Whale,
                      "Killer Whale" = POPhex_MCMC$Killer.Whale,
                      "Walrus" = POPhex_MCMC$Walrus,
                      "Dall's Porpoise" = POPhex_MCMC$Dall.s.Porpoise,
                      "Sperm Whale" = POPhex_MCMC$Sperm.Whale,
                      "Harbor Porpoise" = POPhex_MCMC$Harbor.Porpoise,
                      "Harbor Seal" = POPhex_MCMC$Harbor.Seal
                )

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
                selectizeInput("mapselect", "Select Marine Mammal", choices = c("Select", sort(names(species_list2)))),
                selectizeInput("legendselect", "Select Legend", choices = c("Quintiles",
                                                                           "Low and High Density Emphasis 1",
                                                                           "Low and High Density Emphasis 2", 
                                                                           "Low Density Emphasis",
                                                                           "High Density Emphasis")),
                textInput("abs_abund", "Abundance Estimate", width = NULL, placeholder = "e.g. 5,000"),
                sliderInput('ci', 'Cells of Interest', min = 1, max = 200, value = 1)
              
            )
          )),
          fluidRow(
            wellPanel(
              #Shapefile upload/download UI 
              h3('Download or Upload Shapefile'),
              downloadButton('downloadData', "Download Shapefile"),
              # File input only accepts zipped files. 
              # Server below contains further code on validating content within
              # unzipped file
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
  
  # Converts starting projection to EPSG 4326 to be displayed onto base map.
  POPhexagons_sf <- sf::st_transform(POPhexagons_sf, 4326)
  POPhex_MCMC <- sf::st_transform(POPhex_MCMC, 4326)
    
  # As Alaska is split by the international dateline, the following lines move 
  # the data across the dateline for a unified view. 
  POPhexagons_sf$geometry <- (sf::st_geometry(POPhexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)
  POPhex_MCMC$geometry <- (sf::st_geometry(POPhex_MCMC) + c(360, 90)) %% c(360) - c(0, 90)
  
  # Sample for one species (Northern Minke Whale) which filters the dataframe for
  # areas in which data is available fpr the selected species
  filtered_sf <- POPhexagons_sf %>% filter(!is.na(CU))
  
  
  
  # Reactive value to hold palette data and calculate quartiles for legend
  species_pal <- shiny::reactive({
    selected_species <- input$mapselect
    selected_abund <- as.numeric(input$abs_abund)
    
    if (is.na(selected_abund) || selected_abund <= 0) { selected_abund <- 1 }
    
    #This accesses the POPhex_MCMC$species column of values
    species_data <- species_list2[[selected_species]]
  
    scaled_species_data <- species_data * selected_abund
    
    quartile_vals <- shiny::reactive({
      switch(input$legendselect,
               "Quintiles" = quantile(scaled_species_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
                  "Low and High Density Emphasis 1" = quantile(scaled_species_data, probs = c(0, 0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99, 1)),
                  "Low and High Density Emphasis 2" = quantile(scaled_species_data, probs = c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1)),
                  "Low Density Emphasis" = quantile(scaled_species_data, probs = c(0, 0.01, 0.05, 0.6, 0.8, 1)),
                  "High Density Emphasis" = quantile(scaled_species_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1))) 
      })
    quartile_opt <- quartile_vals()
    
    #another option is I could create a button or smth where we can offer different divisions to select from
    
    # Color palette, bincount, and other customizations for reactive legend
    pal <- leaflet::colorBin(
      palette = "YlGnBu", #BuPu
      reverse = TRUE,
      domain = scaled_species_data,
      bins = quartile_opt,
      pretty = FALSE,
      na.color = "#FFFFFF80"
    )
    
    list(
      selected_species = selected_species,
      data = POPhex_MCMC,
      column = scaled_species_data,
      quartile_opt = quartile_opt,
      fillColor = ~pal(species_data),
      pal = pal,
      selected_abund = selected_abund)#ifelse(is.na(selected_abund), 1, selected_abund))
  })
  
  # Output leaflet map
  output$map <- leaflet::renderLeaflet({
    #map_info <- map_data()
    species_info <- species_pal()
    
    # Data layer obtained from selected species
    leaflet(species_info$data) %>%
      addTiles(
      ) %>%
      addPolygons(fillColor = ~species_info$pal(species_info$column),
                  fillOpacity = 0.8,
                  opacity = 0.7,
                  color = ~species_info$pal(species_info$column),
                  weight = 1,
                  # smoothFactor helps control the level of smoothing
                  # In practice, this setting lowers resolution at a smaller
                  # zoom and returns to better resolution with larger zoom levels.
                  smoothFactor = 0.5,
                  options = pathOptions(zIndex = 5000),
                  group = 'Hexagons') %>%
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
      
      # Adding layers to turn coordinates or shapes on and off.
      addLayersControl(
        overlayGroups = c("Shapes", "Coordinates", "Legend", "Hexagons", "shp"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      # Static set view of Alaska 
      setView(208, 64, 3) %>%
      addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 250)) %>%
      addLegend(
        "bottomright",
        pal = species_info$pal,
        values = species_info$column,
        title = 'Relative Abundance:',
        labFormat = leaflet::labelFormat(digits = 6),
        group = 'Legend'
                  )
  })


  # Provides coordinates for markers when you place them on the map. 
  # Currently does not delete together - FIX
  shiny::observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$properties$feature_type == "marker") {
      leaflet::leafletProxy("map") %>%
        addLabelOnlyMarkers(
          lng <- feature$geometry$coordinates[[1]],
          lat <- feature$geometry$coordinates[[2]],
          label = sprintf("Lat: %0.5f, Lng: %0.5f", lat, lng),
          labelOptions = labelOptions(noHide = TRUE, direction = 'top', offset = c(0, -10)),
          group = "Coordinates"
        )
    }})
  
  # Observe when shapefile is uploaded. 
  shiny::observeEvent(input$drawfile, {
    drawfile <- input$drawfile
    # Create temp directory
    temp_direc2 <- tempdir()
    
    # Unzips the file
    unzip(input$drawfile$datapath, exdir = temp_direc2)
    all_files <- list.files(temp_direc2, full.names = TRUE)
    shp_file <- all_files[grepl("\\.shp$", all_files)]
    print('upload succesful')
   
    if (length(shp_file) > 0) {
      # Read the shapefile
      shapefile_data <- sf::st_read(shp_file)
      
      # Transform the projection to EPSG 4326 in case it is different
      shapefile_data <- sf::st_transform(shapefile_data, 4326)
      shifted_geometry <- (st_geometry(shapefile_data) + c(360, 90)) %% c(360) - c(0, 90)
      st_geometry(shapefile_data) <- shifted_geometry
      
      # Display the shapefile on the map
      leaflet::leafletProxy("map", session) %>%
        addPolygons(data = shapefile_data, color = "red", weight = 1, group = "shp")
      print("shown on map")

    } 
    else {
      showNotification("No .shp file found in the uploaded zip file.", type = "error")
    }
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
      #line of code map_shot/st_write - neither successful so far
      print('file saved to', file)
    }
  )
}

# Run the application 
#shinyApp(ui = ui, server = server)
#temporary due to bug
shinyApp(ui = ui, server = server,options=c(launch.browser = .rs.invokeShinyPaneViewer))

