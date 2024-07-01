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
    
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          selectInput("dataset", "Select Data to display", choices = c("scatterplot", "heatmap")),
          selectInput("basemap", "Select Base Map Style", choices = c("Streets", "Satellite", "Dark")),
          actionButton("update", "Update Map")
         ),

        # Show generated map in main panel
        mainPanel(
           leafletOutput(outputId = "map", width="100%")
           
        )))


# Define server logic
server <- function(input, output) {
    # Random sample output map
  
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
    }
    # print(str(BS_grid_sf))
    # print(head(BS_grid_sf))
  
    BS_grid_sf <- st_wrap_dateline(BS_grid_sf, options = c("WRAPDATELINE=YES"), quiet = TRUE)
    #POP_hexagons_sf <- st_transform(POP_hexagons_sf, 3857)
    # output$map <- renderLeaflet({
    #     leaflet(data = Sample_data[['POP_hexagons_sf']]) %>% addTiles() %>% addPolygons(
    #     fillColor = ~colorNumeric("YlOrRd", POP_hexagons_sf$CU)(POP_hexagons_sf$CU),
    #     fillOpacity = 0.8,
    #     color = "black",
    #     weight = 1,
    #     popup = ~paste("CU:", POP_hexagons_sf$CU)
    #   ) %>%
    #       flyToBounds(-179, 48, -140, 73)
    #     })
    # }
    # output$map <- renderLeaflet({
    #   leaflet(data = BS_grid_sf) %>%
    #     addProviderTiles("OpenStreetMap") %>%
    #     addPolygons(
    #       fillColor = ~colorNumeric("YlOrRd", BS2)(BS2),
    #       fillOpacity = 0.8,
    #       color = "black",
    #       weight = 1,
    #       popup = ~paste("BS2:", BS2)
    #     ) %>%
    #     
    #     flyToBounds(-179, 48, -140, 73)
    # })
    output$map <- renderLeaflet({
      leaflet(BS_grid_sf) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~colorNumeric('RdYlBu', BS2)(BS2),  # Basic color to ensure visibility
          fillOpacity = 0.8,
          color = "black",
          weight = 1
        ) %>%
        flyToBounds(-179, 48, -140, 73)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
