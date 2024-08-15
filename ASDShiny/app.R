# Load all library packages
library(shiny)
library(shinyjs)
library(leaflet)
library(shinyBS)
library(sf)
library(tidyverse)
library(shinydashboard)
library(leaflet.extras)
library(leaflet.mapboxgl) #mapbox extension 
library(shinyWidgets)
library(htmltools)
library(htmlwidgets)
library(mapview)
library(tools)
library(raster)
library(RColorBrewer)


#Access files
source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/helper_functions.R')
source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/custom_area_analysis.R')
#load_all_filest("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data")
# load("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data/POPhexagons_sf.rda")



# load("../data/Sample_data_for_portal.RData")

# Access via GitHub

#load_all_files(urls)

# species_list2 <- list("Northern Minke Whale" = POPhex_MCMC$Northern.Minke.Whale,
#                       "Fin Whale" = POPhex_MCMC$Fin.Whale,
#                       "Northern Fur Seal" = POPhex_MCMC$Northern.Fur.Seal,
#                       #"Bearded Seal" = EB_MCMC, #currently using grid data
#                       "Steller Sea Lion" = POPhex_MCMC$Steller.Sea.Lion,
#                       "Sea Otter" = POPhex_MCMC$Sea.Otter,
#                       "Gray Whale" = POPhex_MCMC$Gray.Whale,
#                       "Pacific White-Sided Dolphin" = POPhex_MCMC$Pacific.White.Sided.Dolphin,
#                       "Humpback Whale" = POPhex_MCMC$Humpback.Whale,
#                       "Killer Whale" = POPhex_MCMC$Killer.Whale,
#                       "Walrus" = POPhex_MCMC$Walrus,
#                       "Dall's Porpoise" = POPhex_MCMC$Dall.s.Porpoise,
#                       "Sperm Whale" = POPhex_MCMC$Sperm.Whale,
#                       "Harbor Porpoise" = POPhex_MCMC$Harbor.Porpoise,
#                       "Harbor Seal" = POPhex_MCMC$Harbor.Seal
# )

species_list2 <- list(
  "Northern Minke Whale" = list(
    data = POPhex_MCMC$Northern.Minke.Whale,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BA_MCMC.RData'
  ),
  "Fin Whale" = list(
    data = POPhex_MCMC$Fin.Whale,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BP_MCMC.RData'
  ),
  "Northern Fur Seal" = list(
    data = POPhex_MCMC$Northern.Fur.Seal,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/CU_MCMC.RData'
  ),
  "Steller Sea Lion" = list(
    data = POPhex_MCMC$Steller.Sea.Lion,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EJ_MCMC.RData'
  ),
  "Sea Otter" = list(
    data = POPhex_MCMC$Sea.Otter,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EL_MCMC.RData'
  ),
  "Gray Whale" = list(
    data = POPhex_MCMC$Gray.Whale,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/ER_MCMC.RData'
  ),
  "Pacific White-Sided Dolphin" = list(
    data = POPhex_MCMC$Pacific.White.Sided.Dolphin,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/LO_MCMC.RData'
  ),
  "Humpback Whale" = list(
    data = POPhex_MCMC$Humpback.Whale,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/MN_MCMC.RData'
  ),
  "Killer Whale" = list(
    data = POPhex_MCMC$Killer.Whale,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OO_MCMC.RData'
  ),
  "Walrus" = list(
    data = POPhex_MCMC$Walrus,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OR_MCMC.RData'
  ),
  "Dall's Porpoise" = list(
    data = POPhex_MCMC$Dall.s.Porpoise,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PD_MCMC.RData'
  ),
  "Sperm Whale" = list(
    data = POPhex_MCMC$Sperm.Whale,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PM_MCMC.RData'
  ),
  "Harbor Porpoise" = list(
    data = POPhex_MCMC$Harbor.Porpoise,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PP_MCMC.RData'
  ),
  "Harbor Seal" = list(
    data = POPhex_MCMC$Harbor.Seal,
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PV_MCMC.RData'
  )
)

RelAbund_MCMC <- NULL

palettes <- list(
  "Viridis" = "viridis",
  "Plasma" = "plasma",
  "Blue-Purple" = "BuPu",
  "Yellow-Green-Blue" = "YlGnBu",
  "Greyscale" = "Greys"
)

# UI
ui <- shinydashboard::dashboardPage(
  dashboardHeader(title = "At Sea Densities of Marine Mammals"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    ),
    sidebarMenu(
      menuItem("About This Tool", tabName = "aboutpg", icon = icon("about")),
      menuItem("How to Use", tabName = "widgets", icon = icon("th")),
      menuItem("Species Maps", tabName = "specmap", icon = icon("otter", lib = "font-awesome")),
      menuItem("Methods", tabName = "metd"),
      menuItem("Licenses", tabName = "lic")
    )),
  dashboardBody(
    shinydashboard::tabItems(
      
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
                    width = '50%',
                    align = "center",
                    alt = "Picture of a male ribbon seal"
                  ),
                  tags$figcaption("NOAA Fisheries/Josh M London"),
                  p(info_two)
                )
              )),
      
      # How to use/instructional tab
      tabItem(tabName = 'widgets',
              wellPanel(
                (h2(strong(div("How to Use", style = 'color: #011f4b'))))),
              wellPanel(
                p(tool_info),
                p(tool_info1),# Separated texts to allow for appropriate spacing.
                p(tool_description),
                uiOutput("palettePlots"),
                p(tool_info3),
                p(tool_info4)
              )),
      
      # Species density map
      tabItem(tabName = "specmap",
              fluidRow(
                column(8, 
                       leafletOutput(outputId = "map", width="100%")
                ),
                column(4,
                       wellPanel(
                         
                         # Customization features in map
                         h3('Customize Map'),
                         selectizeInput("mapselect", "Select Marine Mammal", choices = c("Select", sort(names(species_list2)))),
                         selectizeInput("legendselect", "Select Legend", choices = c("Quintiles",
                                                                                     "Low and High Density Emphasis 1",
                                                                                     "Low and High Density Emphasis 2", 
                                                                                     "Low Density Emphasis",
                                                                                     "High Density Emphasis")),
                         fluidRow(column(6, selectizeInput("palselect", "Select Palette", choices =  c("Viridis",
                                                                                    "Plasma",
                                                                                    "Blue-Purple",
                                                                                    "Yellow-Green-Blue",
                                                                                    "Greyscale"
                                                                                    ), width = NULL)),
                                  column(6, checkboxInput("rev_pal", "Reverse Palette", value = FALSE, width = NULL))),
                         # textInput("abs_abund", "Total Abundance", width = NULL, placeholder = "e.g. 5,000"),
                         shinyBS::bsCollapse(id = "collapse_1", open = "Panel 1",
                           shinyBS::bsCollapsePanel("Additional Options", style = 'success',
                           bsCollapse(id = "collapseExample", open = "Panel 2",
                                      bsCollapsePanel("Abundance Estimate", 
                                                      textInput("abs_abund", "Total Abundance", width = NULL, placeholder = "e.g. 5,000"),
                                                      "Enter total abundance to get an updated abundance estimate.",
                                                      style = "info"),
                                      bsCollapsePanel("Custom Area Analysis",
                                                      "Upload a shapefile for custom area analysis.",
                                                      "Only zipped files will be accepted.",
                                                      br(),
                                                      br(),
                                                      fileInput('drawfile', "Upload Shapefile", accept = '.zip', multiple = TRUE),
                                                      textInput("std_er", "Standard Error", placeholder = "Optional?", width = NULL),
                                                      br(),
                                                      actionButton("do", "Generate"),
                                                      style = "primary")
                           ))
                         )
                       )
                )),
              fluidRow(
                wellPanel(
                  bsCollapse(id = "collapseanalysis", open = "Panel 3",
                             bsCollapsePanel("Generated Custom Area Analysis", 
                                             'Small Area Analysis will be provided once a shapefile is uploaded
                                              and the button "Generate Shapes" is pressed in the Custom Area Analysis
                                              section within Additional Options.',
                                             #tableOutput('coords_table'),
                                             br(),
                                             h4(textOutput('small_area_abund')),
                                             h4(textOutput('medmode')),
                                             h4(textOutput('overall_variance_sum')),
                                             h4(textOutput('overall_variance_mean')),
                                             br(),
                                             br(),
                                             plotOutput('small_area_hist'),
                                             style = "primary")
                )
              )),
              fluidRow(
                wellPanel(
                  #Shapefile upload/download UI 
                  h3('Download Shapefile'),
                  downloadButton('downloadData', "Download Shapefile"),
                  h4('Download Analysis (possibility later)')
                  
                  # File input only accepts zipped files. 
                  # Server below contains further code on validating content within
                  # unzipped file
                  
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
  
  output$palettePlots <- renderUI({
    plot_list <- lapply(names(palettes), function(palette_name) {
      plotOutput(outputId = paste0("plot_", palette_name), height = "100px", width = "100%")
    })
    do.call(tagList, plot_list)
  })
  
  lapply(names(palettes), function(palette_name) {
    output[[paste0("plot_", palette_name)]] <- renderPlot({
      palette_colors <- brewer.pal(9, palettes[[palette_name]])
      ggplot(data.frame(x = 1:9, y = 1, fill = factor(1:9)), aes(x, y, fill = fill)) +
        geom_tile() +
        scale_fill_manual(values = palette_colors) +
        theme_void() +
        theme(legend.position = "none",
              plot.title = element_text(size = 10, hjust = 0.5, family = "Helvetica Neue")) +
        ggtitle(palette_name)
    })
  })
  
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
    rev_selection <- input$rev_pal
    selected_abund <- as.numeric(input$abs_abund)
    
    if (is.na(selected_abund) || selected_abund <= 0) { selected_abund <- 1 }
    
    #This accesses the POPhex_MCMC$species column of values
    species_data <- species_list2[[selected_species]]$data
    
    scaled_species_data <- species_data * selected_abund
    
    palette_choices <- shiny::reactive({
      switch(input$palselect,
             "Viridis" = "viridis",
             "Plasma" = "plasma",
             "Blue-Purple" = "BuPu",
             "Yellow-Green-Blue" = "YlGnBu",
             "Greyscale" = "Greys"
      )
    })
    selected_pal <- palette_choices()
    
    quartile_vals <- shiny::reactive({
      switch(input$legendselect,
             "Quintiles" = raster::quantile(scaled_species_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
             "Low and High Density Emphasis 1" = raster::quantile(scaled_species_data, probs = c(0, 0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99, 1)),
             "Low and High Density Emphasis 2" = raster::quantile(scaled_species_data, probs = c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1)),
             "Low Density Emphasis" = raster::quantile(scaled_species_data, probs = c(0, 0.01, 0.05, 0.6, 0.8, 1)),
             "High Density Emphasis" = raster::quantile(scaled_species_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1))) 
    })
    quartile_opt <- quartile_vals()
    
    # Color palette, bincount, and other customizations for reactive legend
    pal <- leaflet::colorBin(
      palette = selected_pal, 
      reverse = rev_selection,
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
      selected_abund = selected_abund)
  })
  
  # Output leaflet map
  output$map <- renderLeaflet({
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
      leaflet.extras::addDrawToolbar(
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
        title = ifelse(species_info$selected_abund == 1, 'Relative Abundance:', 'Abundance Estimate'),
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
        leaflet::addLabelOnlyMarkers(
          lng <- feature$geometry$coordinates[[1]],
          lat <- feature$geometry$coordinates[[2]],
          label = sprintf("Lat: %0.5f, Lng: %0.5f", lat, lng),
          labelOptions = labelOptions(noHide = TRUE, direction = 'top', offset = c(0, -10)),
          group = "Coordinates"
        )
    }})
  
  
  #will change to observeEvent later if not used globally
  generate_analysis <- shiny::observeEvent(input$do, {
    shapefile_data <- uploaded_shapes()
    species_info <- species_pal()
    selected_species <- input$mapselect
    load(url(species_list2[[selected_species]]$url))
    
    if (is.na(st_crs(shapefile_data))) {
      st_crs(shapefile_data) <- 4326 # Assign a default CRS (EPSG:4326)
    }
    shapefile_data <- sf::st_transform(shapefile_data, 4326)
    shifted_geometry <- (sf::st_geometry(shapefile_data) + c(360, 90)) %% c(360) - c(0, 90)
    sf::st_geometry(shapefile_data) <- shifted_geometry
    
    coords <- st_coordinates(shapefile_data)
    coords_df <- data.frame(coords)
    
    output$coords_table <- renderTable({
      coords_df})
    
    print(coords_df)
    max_x <- max(coords_df$X)
    max_y <- max(coords_df$Y)
    min_x <- min(coords_df$X)
    min_y <- min(coords_df$Y)
    
    # Max X and Y value currently for debugging purposes 
    print(paste("Max X:", max_x, "Min X:", min_x))
    print(paste("Max Y:", max_y, "Min Y:", min_y))
    
    # Calculates variance for RelAbund_MCMC for 1 (MCMC rows)
    row_variances <- apply(RelAbund_MCMC, 1, var)
    
    row_stdev <- apply(RelAbund_MCMC, 1, sd)
    
    POPdata_with_MCMC <- cbind(POPhex_MCMC, RelAbund_MCMC, row_variances)
    
    POPdata_with_MCMC$centroid.x <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,1]
    POPdata_with_MCMC$centroid.y <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,2]
    
    # Filter only those in the shapefile coordinates
    POPdata_with_MCMC <- POPdata_with_MCMC %>%
      dplyr::filter(centroid.x >= min_x & centroid.x <= max_x & centroid.y >= min_y & centroid.y <= max_y)
    #print(POPdata_with_MCMC$Fin.Whale)
  
    # POPdata_with_MCMC$t_value <- POPdata_with_MCMC$Fin.Whale / POPdata_with_MCMC$row_stdev
    # 
    # tval_pal <- colorNumeric(palette = "RdYlBu", domain = POPdata_with_MCMC$t_value, reverse = TRUE)
    # 
    # # Add polygons to the map with colors based on the t-values
    # leafletProxy("map", session) %>%
    #   addPolygons(data = shapefile_data,
    #               fillColor = ~tval_pal(POPdata_with_MCMC$t_value),
    #               fillOpacity = 0.8,
    #               color = "black",
    #               weight = 1,
    #               group = "tvals") %>%
    #   addLegend(pal = tval_pal, 
    #             values = POPdata_with_MCMC$t_value, 
    #             title = "T-Values",
    #             position = "bottomright")
    
    

    total_abundance_sums <- colSums(st_drop_geometry(POPdata_with_MCMC)[, paste0("X", 1:1000)], na.rm = TRUE)
    
    # Calculate the variance of these summed values
    overall_variance <- var(total_abundance_sums)
    
    print(overall_variance)
    output$overall_variance_sum <- renderText("Variance for Selected Area: ", overall_variance)
  
    # Mean Variance for potential use later 
    n_hexagons <- nrow(POPdata_with_MCMC)
    var_mean <- overall_variance / (n_hexagons^2)
    print(var_mean)
    output$overall_variance_mean <- renderText("Mean Variance per Hexagon: ", var_mean)
  
    selected_abund <- species_info$selected_abund
    
    if (is.na(selected_abund) || selected_abund <= 0) { 
      selected_abund <- 1 }
    
    total_abundance_sums <- colSums(st_drop_geometry(POPdata_with_MCMC)[, paste0("X", 1:1000)], na.rm = TRUE)*selected_abund
    print(total_abundance_sums)
    
    output$medmode <- renderText({paste0('Median Abundance Estimate: ', median(total_abundance_sums))})
    
    if (selected_abund == 1 || is.na(selected_abund) || selected_abund <= 0){
      output$small_area_abund <- renderText({paste0("Relative Abundance Estimate for Selected Area: ", sum(species_list2[[selected_species]]$data))})
    }
      
    
    else{
      output$small_area_abund <- renderText({paste0("Absolute Abundance Estimate for Selected Area: ", selected_abund*sum(species_list2[[selected_species]]$data))})
      
      p <- ggplot(data.frame(TotalAbundance = total_abundance_sums), aes(x = TotalAbundance)) +
        geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
        ggtitle("Histogram of Abundance Estimates") +
        xlab("Total Abundance") +
        ylab("Frequency") +
        theme_minimal() +
        theme(plot.title = element_text(size = 15, hjust = 0.5),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12))
      
      output$small_area_hist <- renderPlot({p})
    }
    
    })
  
  
  
  uploaded_shapes <- reactiveVal(NULL)
  # Observe when shapefile is uploaded. 
  shiny::observeEvent(input$drawfile, {
    drawfile <- input$drawfile
    # Create temp directory
    temp_direc2 <- tempdir()
    uploaded_shapes(NULL)
    # unlink(temp_direc2, recursive = TRUE)
    # dir.create(temp_direc2)
    
    all_shapefiles <- list()
    
    # Unzips the file
    for (i in 1:nrow(drawfile)) {
    utils::unzip(input$drawfile$datapath, exdir = temp_direc2)
    all_files <- list.files(temp_direc2, full.names = TRUE)
    shape_file <- all_files[grepl("\\.shp$", all_files)]
    print('upload succesful')
    
    # if (length(shape_file) > 1) {
      # Read the shapefile
      shapefile_data <- sf::st_read(shape_file)
      
      # Transform the projection to EPSG 4326 in case it is different
      shapefile_data <- sf::st_transform(shapefile_data, 4326)
      shifted_geometry <- (sf::st_geometry(shapefile_data) + c(360, 90)) %% c(360) - c(0, 90)
      sf::st_geometry(shapefile_data) <- shifted_geometry
      
      uploaded_shapes(shapefile_data)
      # existing_shapes <- drawn_shapes()
      # if (is.null(existing_shapes)) {
      #   drawn_shapes(shapefile_data)
      # } else {
      #   drawn_shapes(rbind(existing_shapes, shapefile_data))
      # }
      
      all_shapefiles[[length(all_shapefiles) + 1]] <- shapefile_data
      combined_shapefiles <- do.call(rbind, all_shapefiles)
      # Display the shapefile on the map
      leaflet::leafletProxy("map", session) %>%
        leaflet::addPolygons(data = shapefile_data, color = "red", weight = 1, group = "shp")
      print("shown on map")
       
    # } 
    # else {
    #   shiny::showNotification("No .shp file found in the uploaded zip file.", type = "error")
    # }
    }
  })
  
  drawn_shapes <- reactiveVal(NULL)
  
  # Update reactive value when a new shape is drawn
  observeEvent(input$map_draw_new_feature, {
    new_shape <- input$map_draw_new_feature
    new_shape_sf <- sf::st_as_sf(st_sfc(st_polygon(list(matrix(unlist(new_shape$geometry$coordinates[[1]]), ncol = 2, byrow = TRUE)))), crs = 4326)
    
    # set/update new shape to reactive value
    existing_shapes <- drawn_shapes()
    if (is.null(existing_shapes)) {
      drawn_shapes(new_shape_sf)
    } else {
      drawn_shapes(rbind(existing_shapes, new_shape_sf))
    }
  })
  
  # Download handler for shapefiles
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$mapselect,"drawn_shapes_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      shp_file <- file.path(temp_dir, "drawn_shapes.shp")
      
      
      unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE)
      sf::st_write(drawn_shapes(), shp_file)
      
      zip_file <- file
      zip(zip_file, files = list.files(temp_dir, full.names = TRUE, pattern = "drawn_shapes"))
    }
  )
  
  #POPdata_with_MCMC
  
  
}



# Run the application 
#shinyApp(ui = ui, server = server)
#temporary due to bug
shinyApp(ui = ui, server = server)
#options=c(launch.browser = .rs.invokeShinyPaneViewer)

