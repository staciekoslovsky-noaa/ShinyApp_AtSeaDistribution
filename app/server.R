server <- function(input, output, session) {

  # ============ setup ============

  uploaded_shape <- shiny::reactiveVal(NULL)

  drawn_shape <- shiny::reactiveVal(NULL)

  download_shape <- NULL

  active_shapefile <- shiny::reactiveVal(NULL)

  has_temporal <- shiny::reactiveVal(FALSE)

  is_relative <- shiny::reactiveVal(TRUE)

  # ============ reactives =============

  debounced_index <- shiny::reactive({ input$selected_index }) %>% shiny::debounce(250)

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

  latitude <- shiny::reactive({
    latitude <- as.numeric(input$latitude)

    if (is.na(latitude)) {
      latitude <- 60
    } else {
      max(-90, min(90, latitude))
    }
  })

  longitude <- shiny::reactive({
    longitude <- as.numeric(input$longitude)

    if (is.na(longitude) || length(longitude) == 0) {
      return(205)
    }
    
    transformed_lng <- (longitude + 360) %% 360
  
    return(transformed_lng)
  })

  selected_species_code <- shiny::reactive({
    current_species <- selected_species()

    idx <- match(tolower(trimws(current_species)), tolower(trimws(species_codes$species)))

    has_temporal(species_codes$has_temporal[idx] == "TRUE")
    is_relative(species_codes$absolute_relative[idx] == "relative")

    if (!is_relative()) {
      updateTextInput(session, "abs_abund", value = "")
    }

    species_codes$code[tolower(trimws(species_codes$species)) == tolower(trimws(current_species))]
  })

  species_data <- shiny::reactive({
    shiny::req(input$mapselect)
    
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
    abundance <- species_data()

    if (!is_relative()) {

      debounced_index <- debounced_index()

      if (!is.null(debounced_index) && (debounced_index %in% colnames(abundance))) {
        spec_data <- abundance[, debounced_index]
      } else {
        spec_data <- abundance[, ncol(abundance)]
      }
    } else {
      spec_data <- abundance
    }

    if (is.matrix(spec_data) || is.array(spec_data)) {
      final_data <- rowMeans(spec_data)
    } else {
      final_data <- spec_data
    }

    final_data * selected_abund()
  })

  quartiles <- shiny::reactive({ 
    s_data <- scaled_species_data() 
    
    raw_breaks <- switch(input$legendselect,
                        "Quintiles" = raster::quantile(s_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
                        "Low and High Density Emphasis 1" = raster::quantile(s_data, probs = c(0, 0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99, 1)),
                        "Low and High Density Emphasis 2" = raster::quantile(s_data, probs = c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1)),
                        "Low Density Emphasis" = raster::quantile(s_data, probs = c(0, 0.01, 0.05, 0.6, 0.8, 1)),
                        "High Density Emphasis" = raster::quantile(s_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1)))

    if (any(duplicated(raw_breaks))) {
      
      sorted_breaks <- sort(raw_breaks)
      
      for (i in 2:length(sorted_breaks)) {
        if (sorted_breaks[i] <= sorted_breaks[i - 1]) {
          sorted_breaks[i] <- sorted_breaks[i - 1] + 0.1
        }
      }
      return(sorted_breaks)
      
    } else {
      return(raw_breaks)
    }
  })

  pal <- shiny::reactive({
    s_data <- scaled_species_data()

    leaflet::colorBin(
      palette = "Blues",
      domain = s_data,
      bins = quartiles(),
      pretty = FALSE,
      na.color = "#FFFFFF80"
    )
  })

  base_data <- shiny::reactive({
    current_species <- selected_species()
    file_name <- species_codes$base_file[tolower(trimws(species_codes$species)) == tolower(trimws(current_species))]

    data_file_name <- paste0("data/", file_name)

    env <- new.env()

    loaded_names <- load(data_file_name, envir = env)

    base_data <- env[[loaded_names[1]]]

    base_data <- sf::st_transform(base_data, 4326)

    base_data <- sf::st_shift_longitude(base_data)
  })

  # ============ ui/output ============

  output$selected_species_name <- shiny::renderUI({
    current_species <- selected_species()
    
    latin <- species_codes$latin[tolower(trimws(species_codes$species)) == tolower(trimws(current_species))]

    div(current_species, tags$i(paste0(" (", latin, ")")))
  })

  output$is_temporal <- shiny::reactive({
    has_temporal()
  })

  shiny::outputOptions(output, "is_temporal", suspendWhenHidden = FALSE)
  
  output$is_relative <- shiny::renderText({
    if (isTRUE(is_relative())) "true" else "false"
  })

  shiny::outputOptions(output, "is_relative", suspendWhenHidden = FALSE)

  # Output leaflet map
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leafletOptions(attributionControl = FALSE, worldCopyJump = FALSE, preferCanvas = TRUE)) |>
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
                                                         remove = TRUE
                                                         ),
        targetGroup = "Shapes",
        singleFeature = TRUE
      ) |>
      leaflet::setView(lat = 60, lng = 205, zoom = 4) |>
      leaflet::addScaleBar(position = "bottomleft",
                           options = leaflet::scaleBarOptions(maxWidth = 250))
  })

  # Download handler for shapefiles
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      # Set the title for the shapefile
      paste0(selected_species_code(), "_drawn_shape_", Sys.time(), ".zip")
    },
    content = function(file) {
      export_dir <- file.path(tempdir(), "vscode_shape_export")
      dir.create(export_dir, showWarnings = FALSE)

      shp_file <- file.path(export_dir, "drawn_shape.shp")
      sf::st_write(download_shape, shp_file, delete_layer = TRUE, quiet = TRUE)      

      all_files <- list.files(export_dir)

      old_wd <- setwd(export_dir)
      on.exit(setwd(old_wd), add = TRUE)

      zip::zipr(zipfile = file, files = all_files)
      
      unlink(export_dir, recursive = TRUE)
    }
  )

  # ============ observers ==============

  shiny::observeEvent(c(input$mapselect, input$legendselect, input$selected_index), {
    proxy <- leaflet::leafletProxy("map", data = base_data())

    color_func <- pal()
    polygon_colors <- color_func(scaled_species_data())
    
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
    
    if (!is_relative()) {
      label <- "Abundance Estimate"
      digits <- 2
    } else {
      if (selected_abund() == 1) {
        label = "Relative Abundance"
        digits <- 6
      } else {
        label = "Abundance Estimate"
        digits <- 2
      }
    }
      
    proxy |> 
      leaflet::addLegend(
        position = "bottomright",
        pal = color_func,
        values = scaled_species_data(),
        title = label,
        labFormat = leaflet::labelFormat(digits = digits),
        group = "Legend",
        layerId = "dynamic"
      )

      area <- as.numeric(sf::st_area(base_data())) / 1e6

      output$area <- shiny::renderUI({
        formatted_area <- paste0(format(round(area[1], 2), big.mark = ","), " km²")

        tags$span(
          style = "font-weight: bold; color: #555555;", 
          paste0("Cell Area: ", formatted_area)
        )

      })

      if (!is.null(uploaded_shape()) || !is.null(drawn_shape())) {
        shinyjs::enable("generate_button")
      } 
  })

  shiny::observeEvent(species_data(), {
     s_data <- species_data()
  
    if (has_temporal()) {
      column_names <- colnames(s_data)

      shinyWidgets::updateSliderTextInput(
        session = session,
        inputId = "selected_index",
        choices = column_names,       
        selected = tail(column_names, 1)
      )
    
    }
  })


  shiny::observeEvent(input$abs_abund, {
    species_values <- scaled_species_data()
    shiny::req(species_values)

    abund <- suppressWarnings(as.numeric(input$abs_abund))
    
    if (is.na(abund)) {
      showNotification("Warning: Absolute abundace input must be numeric", type = "warning", duration = 5)
      shinyjs::reset("abs_abund")
      return(NULL)
    }

    color_func <- pal()
    
    proxy <- leaflet::leafletProxy("map")

    proxy |> leaflet::clearGroup(group = "Legend")

     if (!is_relative()) {
      label <- "Abundance Estimate"
      digits <- 2
    } else {
      if (selected_abund() == 1) {
        label = "Relative Abundance"
        digits <- 6
      } else {
        label = "Abundance Estimate"
        digits <- 2
      }
    }

    proxy |> 
      leaflet::addLegend(
        position = "bottomright",
        pal = color_func,
        values = species_values,
        title = label,
        labFormat = leaflet::labelFormat(digits = digits),
        group = "Legend",
        layerId = "dynamic"
      )
  })

  shiny::observeEvent(input$zoom, {

    long <- suppressWarnings(as.numeric(input$longitude))
    
    if (is.na(long)) {
      showNotification("Warning: Longitude input must be numeric", type = "warning", duration = 5)
      shinyjs::reset("longitude")
      return(NULL)
    }

    lat <- suppressWarnings(as.numeric(input$latitude))
    
    if (is.na(lat)) {
      showNotification("Warning: Latitude input must be numeric", type = "warning", duration = 5)
      shinyjs::reset("latitude")
      return(NULL)
    }

    proxy <- leaflet::leafletProxy("map")

    proxy |> leaflet::flyTo(lat = latitude(), lng = longitude(), zoom = 8) |>
      addMarkers(lat = latitude(), lng = longitude(), group = "manual_markers")
    
    shinyjs::enable("remove_marker")
  })

  shiny::observeEvent(input$remove_marker, {
    proxy <- leaflet::leafletProxy("map")

    proxy |> clearGroup("manual_markers")
    shinyjs::disable("remove_marker")
  })

  # Update reactive value when a new shape is drawn
  shiny::observeEvent(input$map_draw_new_feature, {
    # Takes in new shape and sets it to variable

    req(input$map_draw_new_feature)
    generate_custom_analysis(NULL)

    proxy <- leaflet::leafletProxy("map")

    proxy |> clearGroup("Shapefile")
    shinyjs::reset("drawfile")
    uploaded_shape(NULL)
    shiny::updateSelectInput(session, "shapefile_select", selected = "Select")
    shinyjs::enable("generate_button")
    shinyjs::disable("remove_button")
    
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
  })

  shiny::observeEvent(input$map_draw_deleted_features, {
    download_shape <<- NULL
    generate_custom_analysis(NULL)
    drawn_shape(NULL)
    shinyjs::disable("generate_button")
    shinyjs::disable("downloadData")
  })

  # when a drawfile is uploaded
  shiny::observeEvent(input$drawfile, {
    active_shapefile(input$drawfile$datapath)

    shiny::updateSelectInput(session, "shapefile_select", selected = "Select")
    uploaded_shape(NULL)
    generate_custom_analysis(NULL)

    show_shapefile(active_shapefile())
  })

  shiny::observeEvent(input$shapefile_select, {
    if (input$shapefile_select == "Select") {
      return(NULL)
    }

    shinyjs::reset("drawfile")

    shape_name <- loaded_shapefiles$filename[tolower(trimws(loaded_shapefiles$name)) == tolower(trimws(input$shapefile_select))]

    shapefile_name <- paste0("shapefiles/", shape_name)

    active_shapefile(shapefile_name)

    show_shapefile(shapefile_name)

  }, ignoreInit = TRUE)

  # Generates custom area analysis when the "generate" button is pressed.
  shiny::observeEvent(input$generate_button, {
    if (!is.null(uploaded_shape())) {
        generate_custom_analysis(uploaded_shape())
      } else {
        generate_custom_analysis(drawn_shape())
      }
  })

  shiny::observeEvent(input$remove_button, {
    proxy <- leaflet::leafletProxy("map")
    proxy |> clearGroup("Shapefile")

    active_shapefile(NULL)
    uploaded_shape(NULL)
    download_shape <<- NULL

    shiny::updateSelectInput(session, "shapefile_select", selected = "Select")
    
    shinyjs::reset("drawfile")
    shinyjs::disable("generate_button")
    shinyjs::disable("remove_button")
    shinyjs::disable("downloadData")
    generate_custom_analysis(NULL)
  })

  show_shapefile <- function(drawfile) {
    shapefile_data <- NULL
    temp_direc2 <- tempfile(pattern = "shapefile_temp_")
    dir.create(temp_direc2)

    utils::unzip(active_shapefile(), exdir = temp_direc2)
    shape_file <- list.files(temp_direc2, pattern = "\\.shp$", full.names = TRUE)

    shapefile_data <- sf::st_read(shape_file, quiet = TRUE)
  
    # Transform the projection to EPSG 4326 in case it is different
    shapefile_data <- sf::st_transform(shapefile_data, 4326)

    # shifted_geometry <- (sf::st_geometry(shapefile_data) + c(360, 90)) %% c(360) - c(0, 90)
    shapefile_data <- sf::st_shift_longitude(shapefile_data)

    # Shifts the geometry taking the dateline into account
    #sf::st_geometry(shapefile_data) <- shifted_geometry

    # Sets the shapefile to uploaded shapes() reactive value
    uploaded_shape(shapefile_data)

    proxy <- leaflet::leafletProxy("map")

    session$sendCustomMessage("clearDrawnShapes", list())

    # Display the shapefile on the map
    proxy |>
      leaflet::clearGroup("Shapefile") |>
      leaflet::addPolygons(data = shapefile_data, color = "red", weight = 3, group = "Shapefile")

    shinyjs::enable("remove_button")

    shiny::req(selected_species())
    shinyjs::enable("generate_button")
  }

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
    cv_input <- as.numeric(input$coeff_var)

    if (is.na(sf::st_crs(shape_data))) {
      sf::st_crs(shape_data) <- 4326 
    }
    
    raw_abundance <- species_data()
    
    if (!is_relative()) {
      target_layer <- debounced_index()
      if (!is.null(target_layer) && (target_layer %in% colnames(raw_abundance))) {
        active_data <- raw_abundance[, target_layer, drop = FALSE]
      } else {
        active_data <- raw_abundance[, 1, drop = FALSE]
      }
      row_variances <- rep(0, nrow(raw_abundance))
    } else {
      active_data   <- raw_abundance
      row_variances <- apply(raw_abundance, 1, var)
    }

    bound_mcmc <- cbind(base_data(), active_data, row_variances)
    centroids  <- sf::st_centroid(bound_mcmc)

    inside <- lengths(sf::st_intersects(centroids, shape_data)) > 0
    bound_mcmc <- bound_mcmc[inside, ]

    if (nrow(bound_mcmc) == 0) {
      shiny::showNotification("No data cells found inside the selected area.", type = "warning")
      return(NULL)
    }

    df_no_geom <- sf::st_drop_geometry(bound_mcmc)
    total_cols <- ncol(df_no_geom)
    last_data_idx  <- total_cols - 1 
    first_data_idx <- ncol(sf::st_drop_geometry(base_data())) + 1

    relative_draws <- df_no_geom |> 
      dplyr::select(all_of(first_data_idx:last_data_idx)) |>
      dplyr::summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
      as.numeric()

    if (!is_relative()) {
      relative_mean <- relative_draws[1] 
      relative_variance <- NA
    } else {
      relative_mean     <- mean(relative_draws)
      relative_variance <- var(relative_draws)
    }

    if (selected_abund() == 1 || is.na(selected_abund()) || selected_abund() <= 0) {
      posterior_draws <- relative_draws
    } else {
      sigma2 <- log(1 + cv_input^2)
      sigma <- sqrt(sigma2)
      meanlog <- log(selected_abund()) - sigma2 / 2
      abundance_draws <- rlnorm(n = length(relative_draws), meanlog = meanlog, sdlog = sigma)
      posterior_draws <- relative_draws * abundance_draws
    }

    posterior_mean <- mean(posterior_draws)
    posterior_median <- median(posterior_draws)
    posterior_variance <- var(posterior_draws)
    posterior_sd <- sqrt(posterior_variance)
    posterior_cv <- posterior_sd / posterior_mean

    if (selected_abund() == 1 || is.na(selected_abund()) || selected_abund() <= 0) {
      if (!is.null(drawn_shape())) {
        col_name <- if (is_relative()) "rel_abund" else "est_abund"
        download_shape <<- drawn_shape() |>
          dplyr::mutate(!!col_name := relative_mean, "variance" = relative_variance)
      } else {
        col_name <- if (is_relative()) "rel_abund" else "est_abund"
        download_shape <<- uploaded_shape() |>
          dplyr::mutate(!!col_name := relative_mean, "variance" = relative_variance)
      }

      output$small_area_abund <- shiny::renderText({
        label_prefix <- if (is_relative()) "Relative Abundance Estimate" else "Absolute Abundance Sum"
        paste0(label_prefix, " for Selected Area: ", format(round(relative_mean, 2), big.mark = ","))
      })
      
      output$overall_variance_sum <- shiny::renderText({
        if (is.na(relative_variance)) "" else paste0("Variance for Selected Area: ", round(relative_variance, digits = 5))
      })
      
      output$medmode <- shiny::renderText({
        if (!is_relative()) "" else paste0("Posterior Median Abundance Estimate: ", round(posterior_median, digits = 3))
      })

      summary_data <- data.frame(
        Species = selected_species(),
        "Abundance Value" = format(relative_mean, digits = 6, big.mark = ",", scientific = FALSE),
        "Variance" = if (is.na(relative_variance)) "N/A" else format(round(relative_variance, digits = 7), scientific = TRUE),
        check.names = FALSE
      )
      colnames(summary_data)[2] <- if (is_relative()) "Relative Abundance Estimate" else "Total Abundance (Sum)"

      output$stat_result <- shiny::renderTable(summary_data)
      output$small_area_hist <- shiny::renderPlot(NULL)
      
    } else {
      # Log-normal scaled pipeline configuration blocks
      if (!is.null(drawn_shape())) {
        download_shape <<- drawn_shape() |>
          dplyr::mutate("post_mean" = posterior_mean, "post_med" = posterior_median, "cv" = posterior_cv)
      } else {
        download_shape <<- uploaded_shape() |>
          dplyr::mutate("post_mean" = posterior_mean, "post_med" = posterior_median, "cv" = posterior_cv)
      }

      output$small_area_abund <- shiny::renderText({
        paste0("Posterior Mean Estimate for Selected Area: ", format(round(posterior_mean), big.mark = ","))
      })
      output$medmode <- shiny::renderText({
        paste0("Posterior Median Abundance Estimate: ", format(round(posterior_median), big.mark = ","))
      })
      output$overall_cv <- shiny::renderText({
        paste0("Coefficient of Variation for Selected Area: ", round(posterior_cv, 3))
      })

      summary_data <- data.frame(
        Species = selected_species(),
        "Selected Abundance" = format(round(selected_abund()), big.mark = ",", scientific = FALSE),
        "Posterior Mean Estimate" = format(round(posterior_mean), big.mark = ",", scientific = FALSE),
        "Posterior Median Abundance Estimate" = format(round(posterior_median), big.mark = ",", scientific = FALSE),
        "Coefficient of Variation" = round(posterior_cv, digits = 2),
        check.names = FALSE
      )

      transposed_data <- as.data.frame(t(summary_data))
      transposed_data <- tibble::rownames_to_column(transposed_data, var = "Metrics")
      transposed_data$V1 <- format(transposed_data$V1, scientific = FALSE)

      p <- ggplot2::ggplot(data.frame(TotalAbundance = posterior_draws), ggplot2::aes(x = TotalAbundance)) +
        ggplot2::geom_histogram(bins = 10, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
        ggplot2::ggtitle("Histogram of Abundance Estimates") +
        ggplot2::xlab("Total Abundance") +
        ggplot2::ylab("Frequency") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = element_text(size = 12, hjust = 0.5),
                       axis.title.x = element_text(size = 8),
                       axis.title.y = element_text(size = 8),
                       axis.text.x = element_text(size = 6),
                       axis.text.y = element_text(size = 6))
                       
      output$small_area_hist <- shiny::renderPlot({ p })
      output$stat_result <- shiny::renderTable(transposed_data, colnames = FALSE, rownames = FALSE)
    }
    shinyjs::enable("downloadData")
    shiny::showNotification("Custom Analysis Results shown in Analysis results tab.", type = "message", duration = 5)
  }
}