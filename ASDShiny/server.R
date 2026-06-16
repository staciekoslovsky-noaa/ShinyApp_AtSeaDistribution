# Define server logic
server <- function(input, output, session) {

  # ============ setup ============

  # Converts starting projection to EPSG 4326 to be displayed onto base map.
  hexagons_sf <- sf::st_transform(POPhexagons_sf, 4326)

  # As Alaska is split by the international dateline, the following lines move
  # the data across the dateline for a unified view.
  hexagons_sf$geometry <- (sf::st_geometry(hexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)

  uploaded_shapes <- shiny::reactiveVal(NULL)

  drawn_shapes <- shiny::reactiveVal(NULL)

  # ============ reactives =============


  selected_species <- shiny::reactive({
    shiny::req(input$mapselect)
    shiny::req(input$mapselect != "Select" && input$mapselect != "")
    input$mapselect
  })

  selected_abund <- shiny::reactive({
    selected_abund <- as.numeric(input$abs_abund)

    # If not inputted, set it to 1
    if (is.na(selected_abund) || selected_abund <= 0) {
      selected_abund <- 1
    } else {
      selected_abund
    }

  })

  scaled_species_data <- shiny::reactive({
    current_species <- selected_species()

  
    code <- species_codes$code[tolower(trimws(species_codes$species)) == tolower(trimws(current_species))]

    # File read fallback if not cached yet
    filename <- paste0("data/", code, "_MCMC.RData")

    
    if (!file.exists(filename)) {
      shiny::showNotification(paste("File not found:", filename), type = "error")
      return(NULL)
    }
    
    load(filename)
          
    spec_data <- rowMeans(RelAbund_MCMC)
    spec_data * selected_abund()
  })

  color_palette <- shiny::reactive({
    if (input$greyscale) {
      "Greys"
    } else {
      "turbo"
    }
  })

  quartiles <- shiny::reactive({
    s_data <- scaled_species_data()

    switch(input$legendselect,
                            "Quintiles" = raster::quantile(s_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
                            "Low and High Density Emphasis 1" = raster::quantile(s_data, probs = c(0, 0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99, 1)),
                            "Low and High Density Emphasis 2" = raster::quantile(s_data, probs = c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1)),
                            "Low Density Emphasis" = raster::quantile(s_data, probs = c(0, 0.01, 0.05, 0.6, 0.8, 1)),
                            "High Density Emphasis" = raster::quantile(s_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1)))
  })

  pal <- shiny::reactive({
    s_data <- scaled_species_data()

    leaflet::colorBin(
      palette = color_palette(),
      domain = s_data,
      bins = quartiles(),
      pretty = FALSE,
      na.color = "#FFFFFF80"
    )
  })


  # ============ ui/output ============

  output$selected_species_name <- shiny::renderUI({
    current_species <- selected_species()
    
    latin <- species_codes$latin[tolower(trimws(species_codes$species)) == tolower(trimws(current_species))]

    div(current_species, tags$i(paste0(" (", latin, ")")))
  })

  # Output leaflet map
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(hexagons_sf, options = leafletOptions(attributionControl = FALSE)) |>
      leaflet::addTiles() |>
      leaflet.extras::addDrawToolbar(
        polygonOptions = leaflet.extras::drawPolygonOptions(),
        circleOptions = leaflet.extras::drawCircleOptions(),
        rectangleOptions = leaflet.extras::drawRectangleOptions(),
        markerOptions = FALSE, 
        polylineOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(edit = FALSE,
                                                         selectedPathOptions = FALSE,
                                                         remove = TRUE),
        targetGroup = "Shapes"
      ) |>
      leaflet::setView(208, 64, 3) |>
      leaflet::addScaleBar(position = "bottomleft",
                           options = leaflet::scaleBarOptions(maxWidth = 250))

  })
  # Download handler for shapefiles
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      # Set the title for the shapefile
      paste0(selected_species(), "drawn_shapes_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()

      # Manually create all the files to include in zipped file
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

  # ============ observers ==============

  shiny::observeEvent(c(input$mapselect, input$legendselect, input$greyscale), {
    species_values <- scaled_species_data()
    shiny::req(species_values)

    color_func <- pal()
    polygon_colors <- color_func(species_values)
    
    proxy <- leaflet::leafletProxy("map", data = hexagons_sf)
    
    proxy |> 
      leaflet::clearGroup(group = "Hexagons") |>
      leaflet::addPolygons(
        fillColor = polygon_colors,
        fillOpacity = 0.8,
        opacity = 0.7,
        color = polygon_colors,
        weight = 1,
        smoothFactor = 0.5,
        options = leaflet::pathOptions(zIndex = 5000),
        group = "Hexagons"
      )
      
    proxy |> 
      leaflet::addLegend(
        position = "bottomright",
        pal = color_func,
        values = species_values,
        title = ifelse(selected_abund() == 1, "Relative Abundance:", "Abundance Estimate"),
        labFormat = leaflet::labelFormat(digits = 6),
        group = "Legend",
        layerId = "dynamic"
      )
  })

  shiny::observeEvent(input$abs_abund, {
    species_values <- scaled_species_data()
    shiny::req(species_values)

    color_func <- pal()
    
    proxy <- leaflet::leafletProxy("map", data = hexagons_sf)

    proxy |> leaflet::removeControl(layerId = "my_dynamic_legend")

    proxy |> leaflet::clearGroup(group = "Legend")

    proxy |> 
      leaflet::addLegend(
        position = "bottomright",
        pal = color_func,
        values = species_values,
        title = ifelse(selected_abund() == 1, "Relative Abundance:", "Abundance Estimate"),
        labFormat = leaflet::labelFormat(digits = 6),
        group = "Legend",
        layerId = "dynamic"
      )
  })

  # Update reactive value when a new shape is drawn
  shiny::observeEvent(input$map_draw_new_feature, {
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
          sf::st_sfc(sf::st_point(center), crs = 4326) |>
            sf::st_transform(32632), radius
        ) |>
          sf::st_transform(4326)
      )
    } else {
      # Assuming it's a polygon or similar
      new_shape_sf <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(matrix(unlist(new_shape$geometry$coordinates[[1]]), ncol = 2, byrow = TRUE)))), crs = 4326)
    }

    # set/update new shape to reactive value
    existing_shapes <- drawn_shapes()
    if (is.null(existing_shapes)) {
      drawn_shapes(new_shape_sf)
    } else {
      drawn_shapes(rbind(existing_shapes, new_shape_sf))
    }

    uploaded_shapes(drawn_shapes())
  })

  # when a drawfile is uploaded
  shiny::observeEvent(input$drawfile, {
    shinyjs::enable("generate_button")

    # Null (clear) everything again in case there is something preexisting
    uploaded_shapes(NULL)
    shapefile_data <- NULL
    temp_direc2 <- tempfile(pattern = "shapefile_temp_")
    dir.create(temp_direc2)

    # Take in inputted shapefile and set to variable
    drawfile <- input$drawfile

    # Unzips the file
    for (i in seq_len(nrow(drawfile))) {
      utils::unzip(input$drawfile$datapath, exdir = temp_direc2)
      all_files <- list.files(temp_direc2, full.names = TRUE)
      shape_file <- all_files[grepl("\\.shp$", all_files)]
      message("upload succesful")

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

        # Display the shapefile on the map
        leaflet::leafletProxy("map", session) |>
          leaflet::clearGroup("Shapefile") |>
          leaflet::clearGroup("Shapes") |>
          leaflet::addPolygons(data = shapefile_data, color = "red", weight = 1, group = "Shapefile")
        print("shown on map")
      } else {
        shiny::showNotification("Uploaded zip file does not contain a valid .shp file.", type = "error")
        shinyjs::reset("drawfile")
        drawfile <- NULL
        shinyjs::disable("generate_button")
      }
    }
  })

  # Generates custom area analysis when the "generate" button is pressed.
  shiny::observeEvent(input$generate_button, {

    shapefile_data <- uploaded_shapes()
    species_name <- species_list2[[selected_species()]]$popdata

    # Coefficient of variation input
    cv_input <- input$coeff_var

    if (is.null(selected_species())) {
      print("selected_species is NULL")
    }

    # the resulting matrix will be named RelAbund_MCMC
    load(url(species_list2[[selected_species()]]$url))

    # Processing shapefile data if crs is not provided or different
    if (is.na(sf::st_crs(shapefile_data))) {
      sf::st_crs(shapefile_data) <- 4326 # Assign a default CRS (EPSG:4326)
    }

    # Creates dataframe based on coordinates within the shapefile data
    coords_df <- data.frame(sf::st_coordinates(shapefile_data))

    # use for debugging purposees (coordinates of shapefile area)
    # output$coords_table <- renderTable({
    #   coords_df})

    max_x <- max(coords_df$X)
    max_y <- max(coords_df$Y)
    min_x <- min(coords_df$X)
    min_y <- min(coords_df$Y)

    # Max X and Y value currently for debugging purposes
    print(paste("Max X:", max_x, "Min X:", min_x))
    print(paste("Max Y:", max_y, "Min Y:", min_y))

    # Calculates variance for RelAbund_MCMC for 1 (MCMC rows)
    row_variances <- apply(RelAbund_MCMC, 1, var)

    bound_mcmc <- cbind(hex_mcmc, RelAbund_MCMC, row_variances)

    # Gather centroids of each hexagon in POP data
    bound_mcmc$centroid.x <- sf::st_coordinates(sf::st_centroid(bound_mcmc))[, 1]
    bound_mcmc$centroid.y <- sf::st_coordinates(sf::st_centroid(bound_mcmc))[, 2]

    # Filter only those within the shapefile coordinates
    bound_mcmc <- bound_mcmc |>
      dplyr::filter(centroid.x >= !!min_x & centroid.x <= !!max_x & centroid.y >= !!min_y & centroid.y <= !!max_y)

    # Gather total abundance sums by summing those columns after filter
    total_abundance_sums <- colSums(sf::st_drop_geometry(bound_mcmc)[, paste0("X", 1:1000)], na.rm = TRUE)

    # Calculate the variance of these summed values
    overall_variance <- var(total_abundance_sums)
    print(overall_variance)

    # Turns coefficient of variation input to numeric/number
    cv_input <- as.numeric(cv_input)

    # Simulating a log normal sampling distribution with`rlnorm`
    n_sim <- rlnorm(1000, meanlog = log(selected_abund()), sdlog = sqrt(log(1 + (cv_input**2))))

    # previously computed total abundance sums from MCMC chains
    total_abundance_sums <- n_sim * total_abundance_sums

    ### GOODMAN'S FORMULA
    # selected_abund (inputted user abundance: mu X)
    # overall_var (calculated by just getting variance from MCMC chains only
    # summed POP data column (containing mean for each hexagon
    # (selected_abund*cv_input) squared
    updated_var <- ((selected_abund())**2) * overall_variance + (sum(bound_mcmc[[species_name]])**2) * ((selected_abund() * cv_input)**2) + overall_variance * ((selected_abund() * cv_input)**2) 
    print(updated_var)

    stderror <- sqrt(updated_var)

    # Obtaining resulting CV using CV = (stderror / mean) formula
    cv_result <- stderror / (selected_abund() * (sum(bound_mcmc[[species_name]])))

    relative_abundance <- round(sum(bound_mcmc[[species_name]], na.rm = TRUE), digits = 3)

    if (selected_abund() == 1 || is.na(selected_abund()) || selected_abund() <= 0) {

      # currently not outputted, but can be modified if renderText in UI added
      output$small_area_abund <- shiny::renderText({
        paste0("Relative Abundance Estimate for Selected Area: ", relative_abundance)
      })
      output$overall_variance_sum <- shiny::renderText({
        paste0("Variance for Selected Area: ", round(overall_variance, digits = 5))
      })

      # Posterior indicates Bayesian appraoch - include in output name
      output$medmode <- shiny::renderText({
        paste0("Posterior Median Abundance Estimate: ", round(median(total_abundance_sums), digits = 3))
      })

      # Summary data frame
      summary_data <- data.frame(
        Species = selected_species(),
        "Relative Abundance Estimate" = format(sum(bound_mcmc[[species_name]]), digits = 6, scientific = FALSE),
        "Variance" = format(round(overall_variance, digits = 7), scientific = TRUE),
        check.names = FALSE
      )

      # This just renders as regular table, because there is no histogram
      output$stat_result <- shiny::renderTable(summary_data)

      # This line is necessary if switching between the two analyses
      # this nulls the histogram when it is reverted to relative abundance
      output$small_area_hist <- shiny::renderPlot(NULL)
    } else {
      # Same approach as above if statement
      output$small_area_abund <- shiny::renderText({
        paste0("Posterior Mean Estimate for Selected Area: ", round(selected_abund() * sum(bound_mcmc[[species_name]]), digits = 0))
      })
      output$medmode <- shiny::renderText({
        paste0("Posterior Median Abundance Estimate: ", round(median(total_abundance_sums), digits = 0))
      })
      output$overall_cv <- shiny::renderText({
        paste0("Coefficient of Variation for Selected Area: ", cv_result)
      })

      # Summary data frame
      summary_data <- data.frame(
        Species = selected_species(),
        "Selected Abundance" = format(selected_abund(), big.mark = ",", scientific = FALSE),
        "Posterior Mean Estimate" = round(selected_abund() * sum(bound_mcmc[[species_name]])),
        "Posterior Median Abundance Estimate" = round(median(total_abundance_sums)),
        "Coefficient of Variation" = round(cv_result, digits = 2),
        check.names = FALSE
      )

      # Turn the data into a data frame and then
      # Transpose data so it is aligned and can include histogram on same plot
      transposed_data <- as.data.frame(t(summary_data))
      transposed_data <- tibble::rownames_to_column(transposed_data, var = "Metrics")
      transposed_data$V1 <- format(transposed_data$V1, scientific = FALSE)

      # Histogram that shows the possible abundance estimate simulations
      p <- ggplot2::ggplot(data.frame(TotalAbundance = total_abundance_sums), ggplot2::aes(x = TotalAbundance)) +
        ggplot2::geom_histogram(bins = 10, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
        ggplot2::ggtitle("Histogram of Abundance Estimates") +
        ggplot2::xlab("Total Abundance") +
        ggplot2::ylab("Frequency") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = element_text(size = 20, hjust = 0.5),
                       axis.title.x = element_text(size = 16),
                       axis.title.y = element_text(size = 16),
                       axis.text.x = element_text(size = 12),
                       axis.text.y = element_text(size = 12))
      output$small_area_hist <- shiny::renderPlot({
        p
      })

      # Set col and row names to false, or unnecessary matrix titles will appear
      output$stat_result <- shiny::renderTable(transposed_data,
                                               colnames = FALSE,
                                               rownames = FALSE)
    }
  })
}