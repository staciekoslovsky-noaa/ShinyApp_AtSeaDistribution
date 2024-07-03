library(shiny) #add install.packages above later?
library(leaflet)
library(sf)
library(tidyverse)

load("../data/Sample_data_for_portal.RData")


# UI
ui <- fluidPage(
    # Theme: Morph CSS (bootswatch)
    tags$head(
      tags$link(rel = "stylesheet", href = "https://bootswatch.com/5/morph/bootstrap.min.css")
    ),
    # Application title
    titlePanel("NOAA   At Sea Densities of Marine Mammals"),
    selectInput("mapselect", "Select Marine Mammal", choices = c("Fur Seals", "Bearded Seals", "Steller Sea Lion"), downloadButton("downloadData")),
    
    # Sidebar (non functional buttons as of now)
    sidebarLayout(
        sidebarPanel(
          selectInput("dataset", "Select Data to display", choices = c("scatterplot", "heatmap")),
          selectInput("basemap", "Select Base Map Style", choices = c("sample", "sample2")),
          actionButton("update", "Update Map") #some may have addtl optional buttons
         ),

        # Show generated map in main panel
        mainPanel(
           leafletOutput(outputId = "map", width="100%")
        )))

# Define server logic
server <- function(input, output, session) {
    #BS_grid_sf <- Sample_data[["BS_grid_sf"]]

    specieslist <- list()
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
      
    # Move BS_grid_sf polygons across dateline - did not work 
    #BS_grid_sf <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
    #BS_grid_sf$geom <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
    
    map_data <- reactive({
      switch(input$mapselect, 
             'Fur Seals' = list(data = st_wrap_dateline(POP_hexagons_sf), fillColor = ~colorNumeric('RdYlBu', CU)(CU), fillOpacity = 0.8, color = "black", weight = 0.5),
             'Bearded Seals' = list(data = st_wrap_dateline(BS_grid_sf), fillColor = ~colorNumeric('RdYlBu', BS2)(BS2), fillOpacity = 0.8, color = "black", weight = 1), # Basic color to ensure visibility
             'Steller Sea Lion' = list(data = st_wrap_dateline(SSL_grid_sf), fillColor = ~colorNumeric('RdYlBu', SSL_POP_ests)(SSL_POP_ests), fillOpacity = 0.8, color = "black", weight = 0.5))
    })
    
    output$map <- renderLeaflet({
      map_info <- map_data()
      leaflet(map_info$data) %>%
        addTiles() %>%
        addPolygons(fillColor = map_info$fillColor, fillOpacity = 0.8, opacity = 0, color = map_info$color, weight = 1) %>%
        flyToBounds(-179, 48, -140, 73)
    })}
  
    
#     output$map <- renderLeaflet({ replaced
#       leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>% #best i can do for now
#         addProviderTiles("CartoDB.Positron") %>%
#         addPolygons(
#           data = BS_grid_sf,
#           fillColor = ~colorNumeric('RdYlBu', BS2)(BS2),  # Basic color to ensure visibility
#           fillOpacity = 0.8,
#           color = "black",
#           weight = 1
#         ) %>%
#         flyToBounds(-179, 48, -140, 73)
#     })
# }


# Run the application 
shinyApp(ui = ui, server = server)
