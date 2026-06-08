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
                               href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"), # nolint: line_length_linter.
                   ),

                   # Various tabs inclued in sidebar menu
                   sidebarMenu(
                     menuItem("About This Tool",
                              tabName = "aboutpg",
                              icon = icon("info-circle")),
                     menuItem("How to Use",
                              tabName = "how_to",
                              icon = icon("th")),
                     menuItem("Explore Data",
                              tabName = "specmap",
                              icon = icon("map")),
                     menuItem("Methods",
                              tabName = "method",
                              icon = icon("clipboard")),
                     menuItem("Reference Information",
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
                h3(purp, width = "100%"),
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
                  tags$figcaption("NOAA Fisheries/Josh M London"),  # nolint: line_length_linter.
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
          uiOutput("palettePlots"),
          p(tool_descript2)
        )
      ),

      # Species density map
      tabItem(tabName = "specmap",
        wellPanel(
          tags$div(
            # Map title with species on top of page
            textOutput("selected_species_name"),
            style = "color: #2c3e50;
                      font-size: 20px; 
                      font-weight: bold;"
          ),
          br(),
          fluidRow(
            column(8,
              leafletOutput(outputId = "map", width = "100%")
            ),
            column(4,
              bsCollapse(id = "customize_map", open = "Customize Map",
                bsCollapsePanel("Customize Map", style = "success",
                  bsCollapse(id = "species", open = "Select Species", # nolint: line_length_linter.
                             bsCollapsePanel("Select Species",
                                             wellPanel(
                                               selectizeInput("mapselect",
                                                              "Select Marine Mammal", # nolint: line_length_linter.
                                                              choices = c("Select", sort(names(species_list2)))), # nolint: line_length_linter.
                                               selectizeInput("legendselect",
                                                              "Select Legend",
                                                              choices = c(
                                                                "Quintiles",
                                                                "Low and High Density Emphasis 1", # nolint: line_length_linter.
                                                                "Low and High Density Empasis 2", # nolint: line_length_linter.
                                                                "Low Density Emphasis", # nolint: line_length_linter.
                                                                "High Density Emphasis" # nolint: line_length_linter.
                                                              )),
                                               selectizeInput("palselect",
                                                              "Select Palette",
                                                              choices =  c(
                                                                "Viridis",
                                                                "Plasma",
                                                                "Blue-Purple",
                                                                "Yellow-Green-Blue", # nolint: line_length_linter.
                                                                "Greyscale"
                                                              ),
                                                              width = NULL),
                                               checkboxInput("rev_pal", "Reverse Palette", value = FALSE, width = NULL), # nolint: line_length_linter.
                                             ))),
                  bsCollapsePanel("Abundance Estimate",
                                  textInput("abs_abund", "Total Abundance", width = NULL, placeholder = "e.g. 5000"), # nolint: line_length_linter.
                                  "Enter total abundance to get an updated abundance estimate.", # nolint: line_length_linter.
                                  br(),
                                  br(),
                                  textInput("coeff_var", "Coefficient of Variation", value = 0.2, placeholder = "e.g. = 0.2", width = NULL), # nolint: line_length_linter.
                                  "Enter a coefficient of variation value. The default value is 0.2.", # nolint: line_length_linter.
                                  style = "info"),
                  bsCollapsePanel("Custom Area Analysis",
                                  "Upload a shapefile for custom area analysis.", # nolint: line_length_linter.
                                  "Only single zipped files will be accepted.", # nolint: line_length_linter.
                                  br(),
                                  br(),
                                  fileInput("drawfile", "Upload Shapefile", accept = ".zip", multiple = TRUE), # nolint: line_length_linter.
                                  br(),
                                  disabled(actionButton("generate_button", "Generate")),
                                  style = "primary")
                )
              )
            )
          ),
          wellPanel(
                    bsCollapse(id = "collapseanalysis", open = "Panel 3",
                      bsCollapsePanel("Generated Custom Area Analysis",
                                      "Small Area Analysis will be provided once a shapefile is uploaded and the button 'Generate Shapes' is pressed in the Custom Area Analysis section within Additional Options.", # nolint: line_length_linter.
                                      br(),
                                      fluidRow(
                                               column(5, h4(tableOutput("stat_result"))), # nolint: line_length_linter.
                                               column(7, plotOutput("small_area_hist"))), # nolint: line_length_linter.
                                      style = "primary")
                    )),
          wellPanel(
            h3("Download Shapefile"),
            downloadButton("downloadData", "Download Shapefile"),
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
          p(methods_info2)
        )
      ),

      # Licenses and How to Cite tab // Needs to be completed
      tabItem(tabName = "reference",
        wellPanel(
          h2(strong("Reference Information", style = "color: #011f4b")),
          wellPanel(
            h3(strong("How to Cite the Data/Application"), style = "color: #011f4b"), # nolint: line_length_linter.
            p("Authors: P.B. Conn, H.L. Faucher, S.M. Koslovsky, C. Kwon (alphabetical; order TBD)", style = "color: #005b96"), # nolint: line_length_linter.
            p("Page Title: At Sea Densities of Marine Mammals", style = "color: #005b96") # nolint: line_length_linter.
          ),
          wellPanel(
            h3(strong("License", style = "color: #011f4b")),
            p(licenses)
          ),
          wellPanel(
            h3(strong("References"), style = "color: #011f4b"),
            p("Goodman, L. A. (1960). On the exact variance of products. Journal of the American Statistical Association, 55, 708-713.", style = "color: #005b96"), # nolint: line_length_linter.
            p("Ver Hoef, J. M., Johnson, D., Angliss, R., & Higham, M. (2021). Species density models from opportunistic citizen science data. Methods in Ecology and Evolution, 12, 1911-1925.", style = "color: #005b96") # nolint: line_length_linter.
          ),
          wellPanel(
            h3(strong("Additional Questions?", style = "color: #011f4b")), # nolint: line_length_linter.
            p("For any additional questions on code maintenance, contact Stacie Koslovsky (stacie.koslovsky [at] noaa.gov). For additional questions regarding  statistical analysis, contact Paul Conn (paul.conn [at] noaa.gov).", style = "color: #005b96"), # nolint: line_length_linter.
            p("For further reference, the code base can be found on GitHub, at the following link: https://github.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution.", style = "color: #005b96") # nolint: line_length_linter.
          ),
        )
      )
    )
  )
)
