ui <- shinydashboard::dashboardPage(

  skin = "black",

  # Dashboard based Shiny set up (collapsible sidebar)
  dashboardHeader(
                  title = "At Sea Densities of Marine Mammals",
                  titleWidth = 400,
                  tags$li(class = "dropdown",
                          tags$a(
                            tags$img(
                              src = "fisheries_header_logo_jul2019.png",
                              height = "50px",
                              width = "125px"
                            ),
                            style = "padding: 0;"
                          ))),
  dashboardSidebar(
                   tags$head(
                     tags$link(rel = "stylesheet",
                               href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
                   ),

                   # Various tabs inclued in sidebar menu
                   sidebarMenu(
                     shinydashboard::menuItem("About This Tool",
                              tabName = "aboutpg",
                              icon = icon("info-circle")),
                     shinydashboard::menuItem("How to Use",
                              tabName = "how_to",
                              icon = icon("th")),
                     shinydashboard::menuItem("Explore Data",
                              tabName = "specmap",
                              icon = icon("map")),
                     shinydashboard::menuItem("Methods",
                              tabName = "method",
                              icon = icon("clipboard")),
                     shinydashboard::menuItem("Reference Information",
                              tabName = "reference",
                              icon = icon("book"))
                   )),

  dashboardBody(
    useShinyjs(),
    shinydashboard::tabItems(
      # About the tool tab
      tabItem(tabName = "aboutpg",
              wellPanel(
                h2(strong("About This Tool"),
                   style = "color: #011f4b"),
                h3(about_title, width = "100%"),
                p(about_info1),
                p(about_info2),
                tags$figure(
                  class = "centerFigure",
                  tags$img(
                    src = "sampleseal.jpg",
                    width = "50%",
                    align = "center",
                    alt = "Picture of a male ribbon seal"
                  ),
                  tags$figcaption("NOAA Fisheries/Josh M London"), 
                  p(about_info3),
                  p(about_info4)
                )
              )),

      # How to use/instructional tab
      tabItem(tabName = "how_to",
        wellPanel(
          h2(strong("How to Use"),
             style = "color: #011f4b"),
          p(tool_info1),
          p(tool_info2),
          p(tool_descript1),
          p(tool_descript2)
        )
      ),

      # Species density map
      tabItem(tabName = "specmap",
        wellPanel(
          tags$div(
            # Map title with species on top of page
            uiOutput("selected_species_name"),
            style = "color: #2c3e50; font-size: 20px; font-weight: bold;"
          ),
          br(),
          fluidRow(
            column(8,
              leafletOutput(outputId = "map", width = "100%", height = "75vh"),
              tags$script(HTML("
                Shiny.addCustomMessageHandler('clearDrawnShapes', function(message) {
                  var map = HTMLWidgets.find('#map').getMap();
                  map.eachLayer(function(layer) {
                    if (layer instanceof L.FeatureGroup && layer.groupname === 'Shapes') {
                      layer.clearLayers();
                    }
                  });
                });
              ")),
              br(),
              tags$div(
                uiOutput("area"),
                style = "color: #2c3e50; font-size: 20px; font-weight: bold;"
              )
            ),
            
            column(4,
              tabsetPanel(
                id = "sidebar_toggle",
                type = "tabs", 
                
                tabPanel("Customize Map",
                  wellPanel(
                  disabled(actionButton("generate_button", "Generate", class = "btn-primary", style = "width: 100%; margin-bottom: 15px;")),

                      bsCollapse(id = "species", open = "Select Species", multiple = FALSE,
                        bsCollapsePanel("Select Species",
                          wellPanel(
                            selectizeInput("mapselect", "Select Marine Mammal",
                                           choices = c("Select", as.list(sort(species_codes$species)))),
                            selectizeInput("legendselect", "Select Legend",
                                           choices = c(
                                             "Quintiles",
                                             "Low and High Density Emphasis 1",
                                             "Low and High Density Emphasis 2",
                                             "Low Density Emphasis",
                                             "High Density Emphasis"
                                           )),
                            
                            conditionalPanel(
                              condition = "typeof output.is_temporal !== 'undefined' && output.is_temporal == true",
                              sliderTextInput(
                                inputId = "selected_index",
                                label = "Selected Season:",
                                choices = "Loading...",   
                                grid = FALSE,
                                width = "100%",
                                force_edges = TRUE
                              )
                            )
                          ), style = "info"
                        ),
                        bsCollapsePanel("Abundance Estimate", style = "info",
                          conditionalPanel( 
                            condition = "typeof output.is_relative !== 'undefined' && output.is_relative == 'true'", 
                            textInput("abs_abund", "Total Abundance", width = NULL, placeholder = "e.g. 5000"), 
                            "Enter total abundance to generate an abundance estimate for each grid cell.",
                            br(), br(), 
                            textInput("coeff_var", "Coefficient of Variation", value = 0.2, placeholder = "e.g. = 0.2", width = NULL), 
                            "Enter a coefficient of variation value. The default value is 0.2.",
                            style = "info"
                          ),
                          conditionalPanel(
                            condition = "typeof output.is_relative !== 'undefined' && output.is_relative != 'true'",
                            tags$em("This option is only available for relative abundance datasets.")
                          )
                        ), 
                        bsCollapsePanel("Custom Area Analysis",
                          fileInput("drawfile", "Upload Shapefile", accept = ".zip", multiple = TRUE),
                          selectizeInput("shapefile_select", "Select Shapefile", choices = c("Select", as.list(loaded_shapefiles$name))),
                          disabled(actionButton("remove_button", "Remove Shapefile")),
                          style = "info"
                        ),
                        bsCollapsePanel("Zoom To",
                          "Enter latitude and longitude to zoom",
                          br(), br(),
                          textInput("latitude", "Latitude", placeholder = "e.g. 60"),
                          textInput("longitude", "Longitude", placeholder = "e.g. -155"),
                          actionButton("zoom", "Zoom"),
                          disabled(actionButton("remove_marker", "Remove Markers")),
                          style = "info"
                        )
                    
                        )  
                  )
                ),
                
                tabPanel("Analysis Results",
                  wellPanel(
                      disabled(downloadButton("downloadData", "Download Results", class = "btn-primary", style = "width: 100%; margin-bottom: 15px;")),
                      hr(),
                      
                      h5("Summary Statistics", style = "font-weight: bold;"),
                      tableOutput("stat_result"),
                      
                      br(), hr(),
                      h5("Abundance Distribution", style = "font-weight: bold;"),
                      conditionalPanel(
                      condition = "input.abs_abund !== ''",
                      plotOutput("small_area_hist", height = "250px")
                    ),

                    conditionalPanel(
                      condition = "input.abs_abund === ''",
                      tags$div(
                        tags$em("Abundance distribution plot will be available once a Total Abundance value is entered in the Customize Map settings.")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # Methods tab detailing POP data and how estimates were calculated
      tabItem(tabName = "method",
        wellPanel(
          h2(strong(methods_title), style = "color: #011f4b"),

          withMathJax(), # LaTex-like equation formatting
          p(methods_info1),
          br(),
          p(methods_info2),
          br(),
          p(methods_info3),
          br(),
          p(methods_info4),
          br(),
          p(methods_info5)
        )
      ),

      # Licenses and How to Cite tab // Needs to be completed
      tabItem(tabName = "reference",
        wellPanel(
          h2(strong("Reference Information", style = "color: #011f4b")),
          wellPanel(
            h3(strong("How to Cite the Data/Application"), style = "color: #011f4b"),
            p("Authors: P.B. Conn, H.L. Faucher, S.M. Koslovsky, C. Kwon (alphabetical; order TBD)", style = "color: #005b96"),
            p("Page Title: At Sea Densities of Marine Mammals", style = "color: #005b96")
          ),
          wellPanel(
            h3(strong("License", style = "color: #011f4b")),
            p(licenses)
          ),
          wellPanel(
            h3(strong("References"), style = "color: #011f4b"),
            p("Goodman, L. A. (1960). On the exact variance of products. Journal of the American Statistical Association, 55, 708-713.", style = "color: #005b96"),
            p("Conn, P. B., Shelden, K. E. W., Brower, A. A., Christman, C. L., & Goetz, K. T. (2026). Abundance and distribution of eastern Bering Sea belugas from 2024 aerial line-transect surveys. NOAA Technical Memorandum NMFS-AFSC-510.", style = "color: #005b96"),
            p("Ferguson, M. C., Conn, P. B., & Thorson, J. T. (2025). Spatially explicit models of density improve estimates of Eastern Bering Sea beluga (Delphinapterus leucas) abundance and distribution from line-transect surveys. PeerJ, 13, e20077.", style = "color: #005b96"),
            p("Ver Hoef, J. M., Johnson, D., Angliss, R., & Higham, M. (2021). Species density models from opportunistic citizen science data. Methods in Ecology and Evolution, 12, 1911-1925.", style = "color: #005b96")
          ),
          wellPanel(
            h3(strong("Additional Questions?", style = "color: #011f4b")),
            p("For any additional questions on code maintenance, contact Stacie Koslovsky (stacie.koslovsky [at] noaa.gov). For additional questions regarding  statistical analysis, contact Paul Conn (paul.conn [at] noaa.gov).", style = "color: #005b96"),
            p("For further reference, the code base can be found on GitHub, at the following link: https://github.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution.", style = "color: #005b96")
          ),
        )
      )
    )
  )
)
