# Define server logic
server <- function(input, output, session) {

  # ============ setup ============

  # Converts starting projection to EPSG 4326 to be displayed onto base map.
  hexagons_sf <- sf::st_transform(POPhexagons_sf, 4326)

  # As Alaska is split by the international dateline, the following lines move
  # the data across the dateline for a unified view.
  hexagons_sf$geometry <- (sf::st_geometry(hexagons_sf) + c(360, 90)) %% c(360) - c(0, 90)

  uploaded_shape <- shiny::reactiveVal(NULL)

  drawn_shape <- shiny::reactiveVal(NULL)

  download_shape <- NULL


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

  selected_species_code <- shiny::reactive({
    current_species <- selected_species()

    species_codes$code[tolower(trimws(species_codes$species)) == tolower(trimws(current_species))]
  })

  species_data <- shiny::reactive({

    code <- selected_species_code()

    # File read fallback if not cached yet
    filename <- paste0("data/", code, "_MCMC.RData")
    
    if (!file.exists(filename)) {
      shiny::showNotification(paste("File not found:", filename), type = "error")
      return(NULL)
    }
    
    names <- load(filename)

    get(names[1])
  })

  scaled_species_data <- shiny::reactive({
    spec_data <- rowMeans(species_data())

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
      leaflet::addMapPane("hexagon_pane", zIndex = 350) |>

      leaflet.extras::addDrawToolbar(
        polygonOptions = leaflet.extras::drawPolygonOptions(),
        circleOptions = leaflet.extras::drawCircleOptions(),
        rectangleOptions = leaflet.extras::drawRectangleOptions(),
        markerOptions = FALSE, 
        polylineOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions(edit = FALSE,
                                                         selectedPathOptions = FALSE,
                                                         remove = TRUE,
                                                         ),
        targetGroup = "Shapes",
        singleFeature = TRUE
      ) |>
      leaflet::setView(208, 64, 3) |>
      leaflet::addScaleBar(position = "bottomleft",
                           options = leaflet::scaleBarOptions(maxWidth = 250))

  })
  # Download handler for shapefiles
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      # Set the title for the shapefile
      paste0(selected_species_code(), "_drawn_shape_", Sys.Date(), ".zip")
    },
    content = function(file) {
      export_dir <- file.path(tempdir(), "vscode_shape_export")
      dir.create(export_dir, showWarnings = FALSE)

      shp_file <- file.path(export_dir, "drawn_shape.shp")
      sf::st_write(download_shape, shp_file, delete_layer = TRUE, quiet = TRUE)      

      all_files <- list.files(export_dir, full.names = TRUE)

      zip(zipfile = file, files = all_files, flags = "-j")
      
      unlink(export_dir, recursive = TRUE)
    }
  )

  # ============ observers ==============

  shiny::observeEvent(c(input$mapselect, input$legendselect, input$greyscale), {
    proxy <- leaflet::leafletProxy("map", data = hexagons_sf)

    species_values <- scaled_species_data()

    color_func <- pal()
    polygon_colors <- color_func(species_values)
    
    proxy |> 
      leaflet::clearGroup(group = "Hexagons") |>
      leaflet::addPolygons(
        fillColor = polygon_colors,
        fillOpacity = 0.8,
        opacity = 0.7,
        color = polygon_colors,
        weight = 1,
        smoothFactor = 0.5,
        options = leaflet::pathOptions(pane = "hexagon_pane", pointerEvents = "none"),
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

      if (!is.null(drawn_shape())) {
        shinyjs::enable("downloadData")
      }

      if (!is.null(uploaded_shape())) {
        generate_custom_analysis(uploaded_shape())
      } else {
        generate_custom_analysis(drawn_shape())
      }
  })

  shiny::observeEvent(input$abs_abund, {
    species_values <- scaled_species_data()
    shiny::req(species_values)

    color_func <- pal()
    
    proxy <- leaflet::leafletProxy("map", data = hexagons_sf)

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

      if (!is.null(uploaded_shape())) {
        generate_custom_analysis(uploaded_shape())
      } else {
        generate_custom_analysis(drawn_shape())
      }
  })

  # Update reactive value when a new shape is drawn
  shiny::observeEvent(input$map_draw_new_feature, {
    # Takes in new shape and sets it to variable

    req(input$map_draw_new_feature)

    proxy <- leaflet::leafletProxy("map")

    proxy |> clearGroup("Shapefile")
    shinyjs::reset("drawfile")
    shinyjs::disable("generate_button")
    shinyjs::disable("remove_button")
    
    if (!is.null(input$mapselect) && input$mapselect != "Select" && input$mapselect != "") {
      shinyjs::enable("downloadData")
    }
    
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

    drawn_shape(new_shape_sf)

    generate_custom_analysis(new_shape_sf)
  })

  shiny::observeEvent(input$map_draw_deleted_features, {
    drawn_shape(NULL)

    shinyjs::disable("downloadData")

    if (!is.null(uploaded_shape())) {
      generate_custom_analysis(uploaded_shape())
    } else {
      generate_custom_analysis(drawn_shape())
    }
  })

  # when a drawfile is uploaded
  shiny::observeEvent(input$drawfile, {
    # Null (clear) everything again in case there is something preexisting
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

      if (length(shape_file) == 1) {
        # Read the shapefile
        shapefile_data <- sf::st_read(shape_file, quiet = TRUE)

        # Transform the projection to EPSG 4326 in case it is different
        shapefile_data <- sf::st_transform(shapefile_data, 4326)

        shifted_geometry <- (sf::st_geometry(shapefile_data) + c(360, 90)) %% c(360) - c(0, 90)

        # Shifts the geometry taking the dateline into account
        sf::st_geometry(shapefile_data) <- shifted_geometry

        # Sets the shapefile to uploaded shapes() reactive value
        uploaded_shape(shapefile_data)
        proxy <- leaflet::leafletProxy("map")

        session$sendCustomMessage("clearDrawnShapes", list())

        # Display the shapefile on the map
        proxy |>
          leaflet::clearGroup("Shapefile") |>
          leaflet::addPolygons(data = shapefile_data, color = "red", weight = 1, group = "Shapefile")
        
        shinyjs::enable("generate_button")
        shinyjs::enable("remove_button")
        shinyjs::disable("downloadData")
      } else {
        shiny::showNotification("Uploaded zip file does not contain a valid .shp file.", type = "error")
        shinyjs::reset("drawfile")
        drawfile <- NULL
      }
    }
  })

  # Generates custom area analysis when the "generate" button is pressed.
  shiny::observeEvent(input$generate_button, {
    generate_custom_analysis(uploaded_shape())
  })

  shiny::observeEvent(input$remove_button, {
    proxy <- leaflet::leafletProxy("map")

    proxy |> clearGroup("Shapefile")
    shinyjs::reset("drawfile")
    shinyjs::disable("generate_button")
    shinyjs::disable("remove_button")
  })

  generate_custom_analysis <- function(shape_data) {

    if (is.null(shape_data)) {
      output$stat_result    <- shiny::renderTable(NULL)
      output$small_area_hist <- shiny::renderPlot({ plot.new() })
      output$small_area_abund <- shiny::renderText({ "" })
      output$medmode         <- shiny::renderText({ "" })
      output$overall_cv      <- shiny::renderText({ "" })
      output$overall_variance_sum <- shiny::renderText({ "" })
      return(NULL)
    }
    
    species_name <- selected_species_code()

    # Coefficient of variation input
    cv_input <- as.numeric(input$coeff_var)

    if (is.null(selected_species())) {
      print("selected_species is NULL")
    }

    if (is.na(sf::st_crs(shape_data))) {
      sf::st_crs(shape_data) <- 4326 # Assign a default CRS (EPSG:4326)
    }

    if (is.na(sf::st_crs(hexagons_sf))) { 
      sf::st_crs(hexagons_sf) <- 4326 
    }
    
    row_variances <- apply(species_data(), 1, var)

    bound_mcmc <- cbind(hexagons_sf, species_data(), row_variances)

    centroids <- sf::st_centroid(bound_mcmc)

    inside <- lengths(
      sf::st_intersects(
        centroids,
        shape_data
      )
    ) > 0

    bound_mcmc <- bound_mcmc[inside, ]

    relative_draws <- colSums(
      sf::st_drop_geometry(bound_mcmc)[, paste0("X", 1:1000)],
      na.rm = TRUE
    )

    relative_mean <- mean(relative_draws)
    relative_variance <- var(relative_draws)

    if (selected_abund() == 1 ||
        is.na(selected_abund()) ||
        selected_abund() <= 0) {

      posterior_draws <- relative_draws

    } else {

      sigma2 <- log(1 + cv_input^2)
      sigma <- sqrt(sigma2)

      meanlog <- log(selected_abund()) - sigma2 / 2

      abundance_draws <- rlnorm(
        n = length(relative_draws),
        meanlog = meanlog,
        sdlog = sigma
      )

      posterior_draws <- relative_draws * abundance_draws
    }

    posterior_mean <- mean(posterior_draws)
    posterior_median <- median(posterior_draws)
    posterior_variance <- var(posterior_draws)

    posterior_sd <- sqrt(posterior_variance)
    posterior_cv <- posterior_sd / posterior_mean

    if (selected_abund() == 1 || is.na(selected_abund()) || selected_abund() <= 0) {

      download_shape <<- drawn_shape() |>
        mutate(
          "RlAbndEs" = relative_mean,
          "Var" = relative_variance
        )

      # currently not outputted, but can be modified if renderText in UI added
      output$small_area_abund <- shiny::renderText({
        paste0("Relative Abundance Estimate for Selected Area: ", relative_mean)
      })
      output$overall_variance_sum <- shiny::renderText({
        paste0("Variance for Selected Area: ", round(relative_variance, digits = 5))
      })
      # Posterior indicates Bayesian appraoch - include in output name
      output$medmode <- shiny::renderText({
        paste0("Posterior Median Abundance Estimate: ", round(posterior_median, digits = 3))
      })

      # Summary data frame
      summary_data <- data.frame(
        Species = selected_species(),
        "Relative Abundance Estimate" = format(
          relative_mean,
          digits = 6,
          scientific = FALSE
        ),
        "Variance" = format(
          round(relative_variance, digits = 7),
          scientific = TRUE
        ),
        check.names = FALSE
      )

      # This just renders as regular table, because there is no histogram
      output$stat_result <- shiny::renderTable(summary_data)

      # This line is necessary if switching between the two analyses
      # this nulls the histogram when it is reverted to relative abundance
      output$small_area_hist <- shiny::renderPlot(NULL)
    } else {
      download_shape <<- drawn_shape() |>
        mutate(
          "PstMenEs" = posterior_mean,
          "PstMedEs" = posterior_median,
          "CoeffVar" = posterior_cv
        )

      # Same approach as above if statement
      output$small_area_abund <- shiny::renderText({
        paste0(
          "Posterior Mean Estimate for Selected Area: ",
          format(round(posterior_mean), big.mark = ",")
        )
      })
      output$medmode <- shiny::renderText({
        paste0(
          "Posterior Median Abundance Estimate: ",
          format(round(posterior_median), big.mark = ",")
        )
      })
      output$overall_cv <- shiny::renderText({
        paste0(
          "Coefficient of Variation for Selected Area: ",
          round(posterior_cv, 3)
        )
      })

      # Summary data frame
      summary_data <- data.frame(
        Species = selected_species(),
        "Selected Abundance" = format(
          round(selected_abund()),
          big.mark = ",",
          scientific = FALSE
        ),
        "Posterior Mean Estimate" = format(
          round(posterior_mean),
          big.mark = ",",
          scientific = FALSE
        ),
        "Posterior Median Abundance Estimate" = format(
          round(posterior_median),
          big.mark = ",",
          scientific = FALSE
        ),
        "Coefficient of Variation" = round(
          posterior_cv,
          digits = 2
        ),
        check.names = FALSE
      )

      # Turn the data into a data frame and then
      # Transpose data so it is aligned and can include histogram on same plot
      transposed_data <- as.data.frame(t(summary_data))
      transposed_data <- tibble::rownames_to_column(transposed_data, var = "Metrics")
      transposed_data$V1 <- format(transposed_data$V1, scientific = FALSE)

      # Histogram that shows the possible abundance estimate simulations
      p <- ggplot2::ggplot(data.frame(TotalAbundance = posterior_draws), ggplot2::aes(x = TotalAbundance)) +
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
  }
}