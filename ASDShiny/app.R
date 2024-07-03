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
    # Application title3
    titlePanel("At Sea Densities of Marine Mammals"),
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
server <- function(input, output) {

    BS_grid_sf <- Sample_data[["BS_grid_sf"]]
  
    if (!inherits(BS_grid_sf, "sf")) {
      BS_grid_sf <- st_as_sf(BS_grid_sf)
    }
    # Transform to 4326 
    if (st_crs(BS_grid_sf)$epsg != 4326) {
      BS_grid_sf <- st_transform(BS_grid_sf, 4326)
    }
    # valid geom
    if (!all(st_is_valid(BS_grid_sf))) {
      BS_grid_sf <- st_make_valid(BS_grid_sf)
      
    # Move BS_grid_sf polygons across dateline - did not work 
    #BS_grid_sf <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
    #BS_grid_sf$geom <- (sf::st_geometry(BS_grid_sf) + c(360, 90)) %% c(360) - c(0, 90)
    }
    # print(str(BS_grid_sf))
    # print(head(BS_grid_sf))
  
    BS_grid_sf <- st_wrap_dateline(BS_grid_sf, options = c("WRAPDATELINE=YES"), quiet = TRUE)
    
    #BS_grid_sf$geom <- st_wrap_dateline(BS_grid_sf$geom, options = c("WRAPDATELINE=YES"), quiet = TRU
     #should put not in server put somewhere else
    
    
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>% #best i can do for now
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          data = BS_grid_sf,
          fillColor = ~colorNumeric('RdYlBu', BS2)(BS2),  # Basic color to ensure visibility
          fillOpacity = 0.8,
          color = "black",
          weight = 1
        ) %>%
        flyToBounds(-179, 48, -140, 73)
        #fitBounds(lng1 = bbox["xmin"], lat1 = bbox["ymin"], lng2 = bbox["xmax"], lat2 = bbox["ymax"])
    })
}

  output

# Run the application 
shinyApp(ui = ui, server = server)
