
###Decided to keep a file for old code I may come back to - easier than revisiting old commits.


# ui <- fluidPage(
  #     # Theme: Morph CSS (bootswatch)
  #     tags$head(
  #       tags$link(rel = "stylesheet", href = "https://bootswatch.com/5/morph/bootstrap.min.css")
  #     ),
  #     # Application title
  #     titlePanel("NOAA   At Sea Densities of Marine Mammals"),
  #     selectInput("mapselect", "Select Marine Mammal", choices = c("Fur Seals", "Bearded Seals", "Steller Sea Lion"), downloadButton("downloadData")),
  #     
  #     # Sidebar (non functional buttons as of now)
  #     sidebarLayout(
  #         sidebarPanel(
  #           selectInput("dataset", "Select Data to display", choices = c("scatterplot", "heatmap")),
  #           selectInput("basemap", "Select Base Map Style", choices = c("sample", "sample2")),
  #           actionButton("update", "Update Map") #some may have addtl optional buttons
  #          ),
  # 
  #         # Show generated map in main panel
  #         mainPanel(
  #            leafletOutput(outputId = "map", width="100%")
  #         )))


observeEvent(input$map_draw_new_feature, {
  feature <- input$map_draw_new_feature
  if (feature$properties$feature_type == 'marker'){
    new_marker <- data.frame(
      id = feature$properties$`_leaflet_id`,
      lat = feature$geometry$coordinates[2],
      lng = feature$geometry$coordinates[1]
      # lat <- input$lat,
      # lng <- input$long
    )
    markers$data <- rbind(markers$data, new_marker)
    
    leafletProxy("map") %>%
      addLabelOnlyMarkers(
        lng = new_marker$lng,
        lat = new_marker$lat,
        layerId = new_marker$id,
        label = sprintf("Lat: %0.5f, Lng: %0.5f", new_marker$lat, new_marker$lng),
        labelOptions = labelOptions(noHide = TRUE, direction = 'top', offset = c(0, -10))
      )
  }
})




# Eventually provides data for selected region, not yet(taken from Harbor Seal App)
drawn_poly_reac <- reactiveVal(NULL)
observeEvent(input$map_draw_new_feature, {
  drawn <- input$map_draw_new_feature
  polygon_coordinates <- do.call(rbind, lapply(drawn$geometry$coordinates[[1]], function(x){c(x[[1]][1],x[[2]][1])}))
  drawn_polygon <- data.frame(lat = polygon_coordinates[, 2], long = polygon_coordinates[, 1]) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  #found_in_bounds <- st_join(sf::st_set_crs(drawn_polygon, 4326), sf::st_set_crs(survey_polygons, 4326))
  
  drawn_poly_reac(drawn_polygon)
  print('new reac val w poly')})


output$getshape <- downloadHandler(
  filename = function(){
    paste('Shapefile', ".shp", sep = "")
  },
  content = function(file){
    st_write(drawn_polygon, 'data/shapefile.shp')})



# DOES NOT INCLUDE POPhexagons_sf file!!!
# Previous local data download function
load_all_filest <- function(directory) {
  # Loads all files from data folder 
  
  # Inputs: directory[str]: pathway to files
  
  # Returns: None, sets file name to loaded data
  
  
  # List all .RData files in the directory
  files <- list.files(directory, pattern = "\\.RData$", full.names = TRUE)
  #print(files) debugging
  if (length(files) == 0) {
    stop("No .RData files found in the directory.")
  }
  
  for (file in files) {
    # Get base name of the file (without directory and extension)
    file_base_name <- tools::file_path_sans_ext(basename(file))
    
    temp_env <- new.env()
    load(file, envir = temp_env)
    
    # Assuming the file contains only one object, get its name and data
    # Explore further this globalenv/tempenv issue 
    object_name <- ls(temp_env)[1]
    data <- get(object_name, envir = temp_env)
    
    # Assign the data to a variable named after the file
    assign(file_base_name, data, envir = .GlobalEnv)
    #print(file_base_name)
  }
  
  print("Finished loading.")
}

# BS_grid_sf$geom <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
# POP_hexagons_sf$geometry <- (sf::st_geometry(POP_hexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)
# SSL_grid_sf$x <- (sf::st_geometry(SSL_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)

#remove and change
for (species in names(Sample_data)){
  #print(species)
  species_data <- Sample_data[[species]]
  if (inherits(species_data, "matrix") || inherits(species_data, "array")){
    next
  }
  if (!inherits(species_data, "sf")) #&& !(inherits(species_data, "matrix"))){
  {species_data <- sf::st_as_sf(species_data)
  }
  #print(st_crs(Sample_data[[species]]))
  current_crs <- sf::st_crs(species_data)
  species_data <- sf::st_transform(species_data, 4326)
  
  if (!all(st_is_valid(species_data))) {
    species_data <- sf::st_make_valid(species_data)
  }
  assign(species, species_data)
}

updated_spec_list <- list("Northern Minke Whale" = POPhexagons_sf$BA,
                          "Fin Whale" = POPhexagons_sf$BP,
                          "Northern Fur Seal" = POPhexagons_sf$CU,
                          #"Bearded Seal" = EB_MCMC, #currently using grid data 
                          "Steller Sea Lion" = POPhexagons_sf$EJ,
                          "Sea Otter" = POPhexagons_sf$EL,
                          "Gray Whale" = POPhexagons_sf$ER,
                          "Pacific White-Sided Dolphin" = POPhexagons_sf$LO,
                          "Humpback Whale" = POPhexagons_sf$MN,
                          "Killer Whale" = POPhexagons_sf$OO,
                          "Walrus" = POPhexagons_sf$OR,
                          "Dall's Porpoise" = POPhexagons_sf$PD,
                          "Sperm Whale" = POPhexagons_sf$PM,
                          "Harbor Porpoise" = POPhexagons_sf$PP,
                          "Harbor Seal" = POPhexagons_sf$PV)


shiny::observeEvent(input$mapselect, {
  selected_species <- input$mapselect
  species_data <- species_list2[[selected_species]]
  
  map_info2 <- map_data()
  #CHANGE TO POPHEX_MCMC 
  quartile_vals <- quantile(species_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  
  # Color palette, bincount, and other customizations for reactive legend
  pal <- leaflet::colorBin(
    palette = "inferno",
    reverse = TRUE,
    domain = species_data,
    bins = quartile_vals,
    pretty = FALSE,
    na.color = "#FFFFFF80"
  )
})