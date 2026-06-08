# Define server logic
server <- function(input, output, session) {
  
  # Palette plots for how to use tab.
  output$palettePlots <- renderUI({
    plot_list <- lapply(names(palettes), function(palette_name) {
      plotOutput(outputId = paste0("plot_", palette_name), height = "100px", width = "100%")
    })
    do.call(tagList, plot_list)
  })
  
  lapply(names(palettes), function(palette_name) {
    output[[paste0("plot_", palette_name)]] <- renderPlot({
      if (palette_name == "Viridis") {
        # For viridis-like palettes
        palette_colors <- viridis::viridis(9, option = "viridis")}
      else if (palette_name == "Plasma"){
        palette_colors <- viridis::viridis(9, option = "plasma")
      }
      else {
        # Default to RColorBrewer for other palettes
        palette_colors <- brewer.pal(9, palettes[[palette_name]])
      }
      
      #palette_colors <- brewer.pal(9, palettes[[palette_name]])
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
    
    # Code for map title on top of page
    output$selected_species_name <- renderText({
      selected_species <- input$mapselect
      if (selected_species == "Select" || is.null(selected_species)) {
        "Base Map"  # Default text when no species is selected
      } else {
        selected_species  # The selected species name
      }
    })
    
    # Takes input on whether reverse is selected or not for the palette on map
    rev_selection <- input$rev_pal
    
    # Turns inputted value for selected abundance as a number (otherwise remains a string)
    selected_abund <- as.numeric(input$abs_abund)
    
    # If not inputted, set it to 1
    if (is.na(selected_abund) || selected_abund <= 0) { selected_abund <- 1 }
    
    #This accesses the POPhex_MCMC$species column of values
    species_data <- species_list2[[selected_species]]$data
    
    # Scales data with selected abundance
    scaled_species_data <- species_data * selected_abund
    
    # Palette choices to select from
    palette_choices <- shiny::reactive({
      switch(input$palselect,
             "Viridis" = "viridis",
             "Plasma" = "plasma",
             "Blue-Purple" = "BuPu",
             "Yellow-Green-Blue" = "YlGnBu",
             "Greyscale" = "Greys"
      )
    })
    
    # Updates selected_pal with any changes in palette choice in UI 
    selected_pal <- palette_choices()
    
    # Manually selected quartile divisions to display different variations of the data 
    quartile_vals <- shiny::reactive({
      switch(input$legendselect,
             "Quintiles" = raster::quantile(scaled_species_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
             "Low and High Density Emphasis 1" = raster::quantile(scaled_species_data, probs = c(0, 0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99, 1)),
             "Low and High Density Emphasis 2" = raster::quantile(scaled_species_data, probs = c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1)),
             "Low Density Emphasis" = raster::quantile(scaled_species_data, probs = c(0, 0.01, 0.05, 0.6, 0.8, 1)),
             "High Density Emphasis" = raster::quantile(scaled_species_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1))) 
    })
    
    # Set that reeactive value to quartile_opt var
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
      addTiles() %>%
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
      
      # Options to draw polygons and circles onto the map for download or analysis 
      leaflet.extras::addDrawToolbar(
        polygonOptions = drawPolygonOptions(),
        circleOptions = drawCircleOptions(),
        rectangleOptions = drawRectangleOptions(),
        markerOptions = FALSE, # Turned off by SMK; to re-add, remove this line and uncomment "# Provides coordinates for markers when you place them on the map." section ~30 lines below here
        polylineOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, 
                                         selectedPathOptions = FALSE, 
                                         remove = TRUE),
        targetGroup = "Shapes"
      ) %>%
      
      # Adding layers to turn coordinates or shapes on and off.
      addLayersControl(
        overlayGroups = c("Shapes", #"Coordinates", 
                          "Legend", "Hexagons", "Shapefile"),
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
  # shiny::observeEvent(input$map_draw_new_feature, {
  #   feature <- input$map_draw_new_feature
  #   if (feature$properties$feature_type == "marker") {
  #     leaflet::leafletProxy("map") %>%
  #       leaflet::addLabelOnlyMarkers(
  #         lng <- feature$geometry$coordinates[[1]],
  #         lat <- feature$geometry$coordinates[[2]],
  #         label = sprintf("Lat: %0.5f, Lng: %0.5f", lat, lng),
  #         labelOptions = labelOptions(noHide = TRUE, direction = 'top', offset = c(0, -10)),
  #         group = "Coordinates"
  #       )
  #   }})
  
  # Generates custom area analysis when the "generate" button is pressed. 
  generate_analysis <- shiny::observeEvent(input$generate_button, {
    
    # Sets the following variables to uploaded shape/species_pal data/species input 
    shapefile_data <- uploaded_shapes()
    species_info <- species_pal()
    selected_species <- input$mapselect
    species_name <- species_list2[[input$mapselect]]$popdata
    
    # Coefficient of variation input 
    cv_input <- input$coeff_var
    
    if (is.null(selected_species)) {
      print("selected_species is NULL")
    }
    
    # This loads the url correlating to the selected species from species_list2 above
    # and the resulting matrix will be named RelAbund_MCMC
    load(url(species_list2[[selected_species]]$url))
    
    # Processing shapefile data if crs is not provided or different
    if (is.na(st_crs(shapefile_data))) {
      st_crs(shapefile_data) <- 4326 # Assign a default CRS (EPSG:4326)
    }
    
    # Creates dataframe `coords_df` based on coordinates within the shapefile data 
    coords <- st_coordinates(shapefile_data)
    coords_df <- data.frame(coords)
    
    # use for debugging purposees (coordinates of shapefile area)
    # output$coords_table <- renderTable({
    #   coords_df})
    
    # Gather the maximum and minimum values for the coordinates in the shapefile area 
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
    
    
    # Puts all the data together into one POPdata_with_MCMC (initialized prior as POPhex_MCMC)
    POPdata_with_MCMC <- cbind(POPhex_MCMC, RelAbund_MCMC, row_variances)
    
    # Gather centroids of each hexagon in POP data 
    POPdata_with_MCMC$centroid.x <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,1]
    POPdata_with_MCMC$centroid.y <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,2]
    
    # Filter only those within the shapefile coordinates
    POPdata_with_MCMC <- POPdata_with_MCMC %>%
      dplyr::filter(centroid.x >= !!min_x & centroid.x <= !!max_x & centroid.y >= !!min_y & centroid.y <= !!max_y)
    
    # Gather total abundance sums by summing those columns after filter
    total_abundance_sums <- colSums(st_drop_geometry(POPdata_with_MCMC)[, paste0("X", 1:1000)], na.rm = TRUE)
    
    # Calculate the variance of these summed values
    overall_variance <- var(total_abundance_sums)
    print(overall_variance)
    
    # Placed again for debugging (ignore) 
    selected_abund <- species_info$selected_abund
    
    if (is.na(selected_abund) || selected_abund <= 0) { 
      selected_abund <- 1 }
    
    # Turns coefficient of variation input (originally character/string class) to numeric/number
    cv_input <- as.numeric(cv_input)
    
    # For histogram
    #total_abundance_sums <- total_abundance_sums*selected_abund
    
    # Simulating a log normal sampling distribution with`rlnorm` (1000 values for 1000 columns in MCMC chains)
    N_sim <- rlnorm(1000, meanlog = log(selected_abund), sdlog = sqrt(log(1 + (cv_input**2))))
    
    # Multiplying generated log-normal samples with previously computed total abundance sums from MCMC chains - line 531
    total_abundance_sums <- N_sim * total_abundance_sums 
    
    ### GOODMAN'S FORMULA
    # selected_abund (inputted user abundance: mu X) 
    # overall_var (calculated by just getting variance from MCMC chains only: var Y)
    # summed POP data column (containing mean for each hexagon (from MCMC) for the selected species in filtered area: mu Y)
    # (selected_abund*cv_input) squared (standard error is abund*CV, now square to get variance: var X)
    updated_var <- ((selected_abund)**2)*overall_variance + (sum(POPdata_with_MCMC[[species_name]])**2)*((selected_abund*cv_input)**2) + overall_variance*((selected_abund*cv_input)**2)  # *
    print(updated_var)
    
    stderror <- sqrt(updated_var)
    
    # Obtaining resulting CV using CV = (stderror / mean) formula
    cv_result <- stderror/(selected_abund*(sum(POPdata_with_MCMC[[species_name]])))
    
    #browser()  
    
    relative_abundance <- reactive({
      species_name <- species_list2[[input$mapselect]]$popdata  # Example: "Fin.Whale"
      
      # Access the corresponding column dynamically
      abundance_data <- POPdata_with_MCMC[[species_name]]
      
      # Calculate the relative abundance estimate for the selected area
      round(sum(abundance_data, na.rm = TRUE), digits = 3)
    })
    
    # If case when selected_abundance not inputted (default is 1) or is some nonsensical value 
    if (selected_abund == 1 || is.na(selected_abund) || selected_abund <= 0){
      
      # currently not outputted as it has been substituted into table, but can be modified to show if renderText in UI added 
      output$small_area_abund <- renderText({paste0("Relative Abundance Estimate for Selected Area: ", relative_abundance())})
      output$overall_variance_sum <- renderText({paste0("Variance for Selected Area: ", round(overall_variance, digits = 5))})   
      
      # Posterior indicates Bayesian appraoch - include in output name
      output$medmode <- renderText({paste0('Posterior Median Abundance Estimate: ', round(median(total_abundance_sums), digits = 3))})
      
      # Summary data frame 
      summary_data <- data.frame(
        Species = species_info$selected_species,
        'Relative Abundance Estimate' = format(sum(POPdata_with_MCMC[[species_name]]), digits = 6, scientific = FALSE),
        'Variance' = format(round(overall_variance, digits = 7), scientific = TRUE),
        check.names = FALSE
      )
      
      # This just renders as regular table, not transposed, because there is no histogram
      output$stat_result <- renderTable(summary_data)
      
      # This line is necessary if switching between the two analyses - for example if you have a histogram outputted before
      # when an abundance was previously inputted,
      # this removes/nulls the histogram when it is reverted back to relative abundance 
      output$small_area_hist <- renderPlot(NULL)
    }
    
    
    else{
      # Same approach as above if statement
      output$small_area_abund <- renderText({paste0("Posterior Mean Estimate for Selected Area: ", round(selected_abund*sum(POPdata_with_MCMC[[species_name]]), digits = 0))})  # *
      #output$overall_variance_sum <- renderText({paste0("Variance for Selected Area: ", updated_var)})    
      output$medmode <- renderText({paste0('Posterior Median Abundance Estimate: ', round(median(total_abundance_sums), digits = 0))})
      output$overall_cv <- renderText({paste0("Coefficient of Variation for Selected Area: ", cv_result)})
      
      # Summary data frame
      summary_data <- data.frame(
        Species = species_info$selected_species,
        'Selected Abundance' = format(selected_abund, big.mark = ",", scientific = FALSE),
        'Posterior Mean Estimate' = round(selected_abund*sum(POPdata_with_MCMC[[species_name]])),
        'Posterior Median Abundance Estimate' = round(median(total_abundance_sums)),
        'Coefficient of Variation' = round(cv_result, digits = 2),
        check.names = FALSE
      )
      
      # Turn the data into a data frame and then 
      # Transpose data / turn it around so that it is better aligned and can include histogram on same plot
      transposed_data <- as.data.frame(t(summary_data))
      transposed_data <- tibble::rownames_to_column(transposed_data, var = "Metrics")
      transposed_data$V1 <- format(transposed_data$V1, scientific = FALSE)
      
      # Histogram that shows the possible abundance estimate simulations using MCMC chains and CV input uncertainty
      p <- ggplot2::ggplot(data.frame(TotalAbundance = total_abundance_sums), aes(x = TotalAbundance)) +
        geom_histogram(bins = 10, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
        ggtitle("Histogram of Abundance Estimates") +
        xlab("Total Abundance") +
        ylab("Frequency") +
        theme_minimal() +
        theme(plot.title = element_text(size = 20, hjust = 0.5),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),  # Adjusts axis numbers (the actual values)
              axis.text.y = element_text(size = 12))
      
      output$small_area_hist <- renderPlot({
        
        p})
      
      # Set col and row names to false, or unnecessary matrix titles will appear 
      output$stat_result <- renderTable(transposed_data, 
                                        colnames = FALSE,
                                        rownames = FALSE)
    }
    
    
    
  })
  
  
  uploaded_shapes <- reactiveVal(NULL)
  
  # Observe when shapefile is uploaded. 
  shiny::observeEvent(input$drawfile, {
    enable("generate_button")
    
    # Null (clear) everything again in case there is something preexisting
    uploaded_shapes(NULL)
    shapefile_data <- NULL
    temp_direc2 <- tempfile(pattern = "shapefile_temp_")
    dir.create(temp_direc2)
    
    # Take in inputted shapefile and set to variable
    drawfile <- input$drawfile
    
    # Added list for later possibility if there are >1 shapefiles
    all_shapefiles <- list()
    
    # Unzips the file
    for (i in 1:nrow(drawfile)) {
      utils::unzip(input$drawfile$datapath, exdir = temp_direc2)
      all_files <- list.files(temp_direc2, full.names = TRUE)
      shape_file <- all_files[grepl("\\.shp$", all_files)]
      message('upload succesful')
      
      if (length(shape_file) >= 1) {
        # Read the shapefile
        shapefile_data <- sf::st_read(shape_file)
        
        # Transform the projection to EPSG 4326 in case it is different
        shapefile_data <- sf::st_transform(shapefile_data, 4326)
        
        shifted_geometry <- (sf::st_geometry(shapefile_data) + c(360, 90)) %% c(360) - c(0, 90)
        
        # Shifts the geometry taking the dateline into account
        sf::st_geometry(shapefile_data) <- shifted_geometry
        
        # Sets the shapefile to uploaded shapes() reactive value
        uploaded_shapes(shapefile_data)
        
        # Previously for when there would be more than one shape in the file (no longer option, should only 
        # contain one shape only)
        # existing_shapes <- drawn_shapes()
        # if (is.null(existing_shapes)) {
        #   drawn_shapes(shapefile_data)
        # } else {
        #   drawn_shapes(rbind(existing_shapes, shapefile_data))
        # }

        # Display the shapefile on the map
        leaflet::leafletProxy("map", session) %>%
        leaflet::clearGroup("Shapefile") %>%
        leaflet::clearGroup("Shapes") %>%
        leaflet::addPolygons(data = shapefile_data, color = "red", weight = 1, group = "Shapefile")
        print("shown on map")
      } else {
        shiny::showNotification("Uploaded zip file does not contain a valid .shp file.", type = "error")
        shinyjs::reset("drawfile")
        drawfile <- NULL
        disable("generate_button")
      }
      # all_shapefiles[[length(all_shapefiles) + 1]] <- shapefile_data
      # combined_shapefiles <- do.call(rbind, all_shapefiles)
    }
  })
  
  drawn_shapes <- reactiveVal(NULL)
  
  # Update reactive value when a new shape is drawn
  observeEvent(input$map_draw_new_feature, {
    drawn_shapes(NULL)
    uploaded_shapes(NULL)
    # Takes in new shape and sets it to variable 
    new_shape <- input$map_draw_new_feature
    
    if (new_shape$properties$feature_type == "circle") {
      # Extract the center and radius of the circle
      center <- c(new_shape$geometry$coordinates[[1]], new_shape$geometry$coordinates[[2]])
      radius <- new_shape$properties$radius
      
      # Convert the circle to a polygon (sf)
      new_shape_sf <- sf::st_as_sf(
        sf::st_buffer(
          sf::st_sfc(sf::st_point(center), crs = 4326) %>%
            sf::st_transform(32632), radius
        ) %>%
          sf::st_transform(4326)
      )
    } else {
      # Assuming it's a polygon or similar
      new_shape_sf <- sf::st_as_sf(st_sfc(st_polygon(list(matrix(unlist(new_shape$geometry$coordinates[[1]]), ncol = 2, byrow = TRUE)))), crs = 4326)
    }
    
    # set/update new shape to reactive value
    existing_shapes <- drawn_shapes()
    if (is.null(existing_shapes)) {
      drawn_shapes(new_shape_sf)
    } else {
      # While they should only draw one shape, this takes into the possibility that 
      # more than one shape exists on map
      drawn_shapes(rbind(existing_shapes, new_shape_sf))
    }
    
    # Set existing_shapes to files in drawn shapes, then pass it onto uploaded_shapes()
    # For analysis use 
    existing_shapes <- drawn_shapes()
    uploaded_shapes(existing_shapes)
  })
  
  # Download handler for shapefiles
  output$downloadData <- downloadHandler(
    filename = function() {
      # Set the title for the shapefile 
      paste0(input$mapselect,"drawn_shapes_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Manually create all the files to include in zipped file
      
      # This helps address previous issue where they came in a nested folder (folder in folder)
      shp_file <- file.path(temp_dir, "drawn_shapes.shp")
      shx_file <- file.path(temp_dir, "drawn_shapes.shx")
      dbf_file <- file.path(temp_dir, "drawn_shapes.dbf")
      prj_file <- file.path(temp_dir, "drawn_shapes.prj")
      
      # Unlinks all files in temp_dir for clean up 
      unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE)
      sf::st_write(drawn_shapes(), shp_file)
      
      # -j Means no directory paths, just files only!
      zip(zipfile = file, files = c(shp_file, shx_file, dbf_file, prj_file), flags = "-j")
    }
  )
  
}