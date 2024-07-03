
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