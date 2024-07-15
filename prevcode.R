
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