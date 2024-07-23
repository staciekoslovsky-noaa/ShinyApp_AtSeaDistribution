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
load_all_filest("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data")
 # load("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data/POPhexagons_sf.rda")
 # load(url('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/POPhex_MCMC.rda'))


# load("../data/Sample_data_for_portal.RData")


# Access via GitHub
#load(url('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/POPhexagons_sf.rda'))
#load_all_files(urls)



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

species_list2 <- list(
  "Northern Minke Whale" = POPhex_MCMC$Northern.Minke.Whale,
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
                sliderInput('ci', 'Cells of Interest', min = 1, max = 200, value = 1)
              
            )
          )),
          fluidRow(
            wellPanel(
              
              #Shapefile upload/download UI 
              h3('Download or Upload Shapefile'),
              #textInput('downloadShp', 'Filename:', value = 'Shapes.zip'),
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
  
    
  # Reactive expression that updates map_data when a marine mammal species is
  # selected from "selectize" dropdown/searchbar.
  # Finish
  
  map_data <- shiny::reactive({
    switch(input$mapselect,
           'Select' = list(data = POPhex_MCMC, text = "Surveyed areas shown in Blue"),
           'Northern Minke Whale' = list(data = POPhex_MCMC, fillColor = ~colorNumeric('inferno', POPhex_MCMC$Northern.Minke.Whale)(POPhex_MCMC$Northern.Minke.Whale), fillOpacity = 0.8, color = "black", weight = 0.5),
            #use the actual values in POPhexagons_sf 
           'Fin Whale' = list(data = POPhex_MCMC, fillColor = ~colorNumeric('inferno', POPhex_MCMC$Fin.Whale)(POPhex_MCMC$Fin.Whale), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Northern Fur Seal' = list(data = filtered_sf, fillColor = ~colorNumeric('inferno', POPhexagons_sf$CU)(CU), fillOpacity = 0.8, color = "black", weight = 0.5),
           #'Bearded Seal' = list(data = BS_grid_sf, fillColor = ~colorNumeric('inferno', BS2)(BS2), fillOpacity = 0.8, color = "black", weight = 1), # Basic color to ensure visibility
           #'Steller Sea Lion' = list(data = SSL_grid_sf, fillColor = ~colorNumeric('inferno', SSL_POP_ests)(SSL_POP_ests), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Steller Sea Lion' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', POPhexagons_sf$EJ)(EJ), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Sea Otter' = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', POPhexagons_sf$EL)(EL), fillOpacity = 0.8, color = "black", weight = 0.5),
           'Gray Whale' = list(),
           'Pacific White-Sided Dolphin' = list(),
           "Dall's Porpoise" = list(data = POPhexagons_sf, fillColor = ~colorNumeric('inferno', POPhexagons_sf$PV)(PV), fillOpacity = 0.8, color = "black", weight = 0.5)
    )
  })
  
  # Reactive value to hold palette data and calculate quartiles for legend
  species_pal <- shiny::reactive({
    selected_species <- input$mapselect
    
    #This accesses the POPhex_MCMC$species column of values
    species_data <- species_list2[[selected_species]]
    
    quartile_vals <- quantile(species_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    
    # Color palette, bincount, and other customizations for reactive legend
    pal <- leaflet::colorBin(
      palette = "inferno",
      domain = species_data,
      bins = quartile_vals,
      pretty = FALSE,
      na.color = "#FFFFFF80"
    )
    
    list(
      selected_species = selected_species,
      data = POPhex_MCMC,
      column = species_data,
      quartile_vals = quartile_vals,
      fillColor = ~pal(species_data),
      pal = pal)
  })
  
  # Output leaflet map
  output$map <- leaflet::renderLeaflet({
    map_info <- map_data()
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
                  smoothFactor = 0.3) %>%
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
        overlayGroups = c("Shapes", "Coordinates", "Legend"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      # Static set view of Alaska 
      setView(208, 64, 3) %>%
      addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 250)) %>%
      addLegend(
        "bottomright",
        pal = species_info$pal,
        values = species_info$species_data,
        title = 'Relative Abundance:',
        labFormat = leaflet::labelFormat(digits = 6),
        group = 'Legend'
                  )
  })

  # Obtain corresponding MCMC matrix for selected species 
  # Code moved upwards; leafletproxy substituted for legend within leafet output
  
    # Clear controls every time a new species is selected with updated legend
  #   leaflet::leafletProxy("map") %>%
  #     clearControls() %>%
  #     addLegend("bottomright",
  #               pal = pal,
  #               values = species_data,
  #               title = 'Relative Abundance:',
  #               labFormat = leaflet::labelFormat(digits = 6),
  #               group = 'Legend')
  # })

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
    unzip(input$drawfile$datapath, exdir = temp_dir)
    # Validates that the files are zipped
    validate(need(ext == ('.shp' || '.kmz'), 'Please upload a valid shapefile in one of the following formats: .shp, ...etc.'))
    #files <- st_transform(files, 4326)
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
      #line of code map_shot/st_write - neither successful so far
      print('file saved to', file)
    }
  )
}

# Run the application 
#shinyApp(ui = ui, server = server)
#temporary due to bug
shinyApp(ui = ui, server = server,options=c(launch.browser = .rs.invokeShinyPaneViewer))

