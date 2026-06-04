# Load all library packages
library(shiny)
library(shinyjs)
library(leaflet)
library(shinyBS)
library(sf)
library(tidyverse)
library(shinydashboard)
library(leaflet.extras)
library(shinyWidgets)
library(htmltools)
library(mapview)
library(tools)
library(RColorBrewer)
library(viridis)


# Access files
source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/helper_functions.R')
source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/custom_area_analysis.R')


# Access via GitHub

# Initialize POPdata_with_MCMC (used for later custom area analysis)
POPdata_with_MCMC <- POPhex_MCMC

# List containing the species and its respective POP data column in dataframe (data), its name (as a string) that contains 
# the MCMC posterior means (popdata), and url of the MCMC chains from Git (url)
species_list2 <- list(
  "Northern Minke Whale" = list(
    data = POPhex_MCMC$Northern.Minke.Whale,
    popdata = "Northern.Minke.Whale",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BA_MCMC.RData'
  ),
  "Fin Whale" = list(
    data = POPhex_MCMC$Fin.Whale,
    popdata = "Fin.Whale",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BP_MCMC.RData'
  ),
  "Northern Fur Seal" = list(
    data = POPhex_MCMC$Northern.Fur.Seal,
    popdata = "Northern.Fur.Seal",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/CU_MCMC.RData'
  ),
  "Steller Sea Lion" = list(
    data = POPhex_MCMC$Steller.Sea.Lion,
    popdata = "Steller.Sea.Lion",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EJ_MCMC.RData'
  ),
  "Sea Otter" = list(
    data = POPhex_MCMC$Sea.Otter,
    popdata = "Sea.Otter",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EL_MCMC.RData'
  ),
  "Gray Whale" = list(
    data = POPhex_MCMC$Gray.Whale,
    popdata = "Gray.Whale",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/ER_MCMC.RData'
  ),
  "Pacific White-Sided Dolphin" = list(
    data = POPhex_MCMC$Pacific.White.Sided.Dolphin,
    popdata = "Pacific.White.Sided.Dolphin",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/LO_MCMC.RData'
  ),
  "Humpback Whale" = list(
    data = POPhex_MCMC$Humpback.Whale,
    popdata = "Humpback.Whale",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/MN_MCMC.RData'
  ),
  "Killer Whale" = list(
    data = POPhex_MCMC$Killer.Whale,
    popdata = "Killer.Whale",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OO_MCMC.RData'
  ),
  "Walrus" = list(
    data = POPhex_MCMC$Walrus,
    popdata = "Walrus",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OR_MCMC.RData'
  ),
  "Dall's Porpoise" = list(
    data = POPhex_MCMC$Dall.s.Porpoise,
    popdata = "Dall.s.Porpoise",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PD_MCMC.RData'
  ),
  "Sperm Whale" = list(
    data = POPhex_MCMC$Sperm.Whale,
    popdata = "Sperm.Whale",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PM_MCMC.RData'
  ),
  "Harbor Porpoise" = list(
    data = POPhex_MCMC$Harbor.Porpoise,
    popdata = "Harbor.Porpoise",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PP_MCMC.RData'
  ),
  "Harbor Seal" = list(
    data = POPhex_MCMC$Harbor.Seal,
    popdata = "Harbor.Seal",
    url = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PV_MCMC.RData'
  )
)


# Initialize RelAbund_MCMC as object
# NULL is later replaced by loaded MCMC file from Git
RelAbund_MCMC <- NULL

palettes <- list(
  "Viridis" = "viridis",
  "Plasma" = "plasma",
  "Blue-Purple" = "BuPu",
  "Yellow-Green-Blue" = "YlGnBu",
  "Greyscale" = "Greys"
)

# ========================= helper_functions =========================

# Load data
urls <- c('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BA_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BP_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/CU_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EJ_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EL_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/ER_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/LO_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/MN_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OO_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OR_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PD_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PM_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PP_MCMC.RData',
          'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PV_MCMC.RData'
)

load(url('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/POPhex_MCMC.rda'))
load(url('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/POPhexagons_sf.rda'))

analysis_title <- reactiveVal(NULL)

load_all_files <- function(directory_urls){
  # Loads all files from data folder. 
  
  # Inputs: directory_urls: vector of raw GitHub file links
  
  # Returns: None, just loads all files
  for (url_input in directory_urls){
    # Get the name of the file (ex: BA_MCMC)
    file_base_name <- tools::file_path_sans_ext(basename(url_input))
    
    # Load into temporary environment
    temp_env <- new.env()
    load(url(url_input), envir = temp_env)
    
    # Get the names of objects
    obj_name <- ls(temp_env)
    
    # Assign object to the global environment (for easy access) with the file base name
    assign(file_base_name, get(obj_name, envir = temp_env), envir = .GlobalEnv)
    
  }
  
  # Final check 
  print('Done loading')
}

# Optional function for filtering only areas where species is not NA
# Not used as to show areas that have all been surveyed and not found
filter_by_col <- function(sffile, col_name){
  sffile %>% filter(!is.na(.[[col_name]]))
}



# Text for the sidebar info pages - from Project Proposal /metadata?
purp <- div('Integrating Diverse Datasets to Understand the Seasonal Distributions
        and Densities of Marine Mammals', style = 'color: #03396c')

# About tab 
about_info1 <- div('Understanding seasonal distributions and densities of marine mammals remains a high priority for NMFS science centers and regional offices. 
                   In addition to answering basic ecological questions, such information is frequently requested of science center staff to help in calculation 
                   of “takes” under the Marine Mammal Protection Act (MMPA) and the Endangered Species Act (ESA) (e.g., due to offshore energy development, 
                   fisheries interactions, naval exercises, aquaculture permitting, pier construction, or hazard remediation).', style = 'color: #005b96')

about_info2 <- div('Calculation of at-sea densities remains challenging, however, as many Arctic marine mammal surveys are timed to occur during spring or summer 
                   months when animals are the most accessible (and often on land in the case of many pinniped species). In practice,there is a large amount of 
                   data that may be leveraged to provide estimates of seasonal densities, including scientific surveys, satellite telemetry, acoustic detections, 
                   Alaska Native subsistence harvests, and platform-of-opportunity (POP) observations. Seasonal species distribution and density maps inevitably 
                   require fitting a statistical model to one or more of these data sources, and to use the fitted model to make spatially referenced predictions. 
                   Our goal with this tool is to share such predictive maps with interested stakeholders. We anticipate more maps being added over time as 
                   increasingly sophisticated models are applied to individual species.', style = 'color: #005b96')

about_info3 <- div('Marine spatial planning requires knowledge of the timing and location of marine mammal distribution, migrations, density in local areas, and 
                   movements to mitigate anthropogenic impacts on protected species. The ability to combine data from surveys, POP, telemetry, and passive acoustic 
                   recorders will shrink spatial-temporal data gaps and provide managers with a product that represents the best scientific information available. 
                   This more complete information can then be used to assist in NEPA analyses, ESA Section 7 consultations, MMPA Incidental Harassment Authorizations, 
                   and other permit and management needs. Further, because the gaps present in the data proposed for development of this toolbox are not unique to the 
                   Alaska Region, the framework developed here will have broad applicability beyond Arctic ecosystems and for all NMFS science centers and regional 
                   offices.', style = 'color: #005b96')

about_info4 <- div('The potential benefits of these analytical approaches are expected to reach well beyond just NMFS. Other federal and state agencies, tribal 
                   governments and Alaska Native organizations, non-governmental organizations, and a variety of industry and community stakeholders seek the type 
                   of information products that this project is targeting. Information about seasonal patterns of marine mammal density distributions and habitat 
                   use will help these groups to better assess potential impacts, implement mitigation actions where appropriate, and develop plans that are more 
                   fully informed by science. Examples of specific federal agencies that have expressed a desire for access to better information on the seasonal 
                   locations and densities of marine mammals include BOEM and the U.S. Navy.', style = 'color: #005b96')


# How to Use Tab 
tool_info1 <- div('This tool was developed using Shiny, a package that facilitates web app development directly from coding languages, such as R.', style = 'color: #005b96')
tool_info2 <- div(p("To access species density maps, click the Species button in the sidebar. Use the right panel to toggle between different marine mammal species."), style = 'color: #005b96')

tool_descript1 <- div(h3("Using the Draw Toolbar"),
                      p("The toolbar on the left of the map contains various tools to work with select data shown on the map (for instance to perform small area calculations, as when the user is 
                      interested in the number of animals or fraction of a population that are in a specific area of interest). 
                        The polygon, rectangle, and circle options on the toolbar can be used to draw shapes corresponding to areas of interest; 
                        The marker can be used to obtain coordinates of the selected location; 
                        The trash bin will delete any shapes or lines that are no longer necessary; 
                        The bottom panel contains buttons to download a shapefile of the drawn polygons and upload the user's own shape data for analysis in one of the following formats: 
                          zipped .kmz or .shp file."),
                      
                      h3("Customizing the Legend"),
                      'The legend in the map can be customized using the "Select Legend" option in the sidebar.  This can be useful in visualizing changes in abundance across a map, 
                    particularly when species are clustered in small areas. The options are named:',
                      tags$ul(
                        tags$li('"Quintiles" divides them into the following percentiles: 0, 0.2, 0.4, 0.6, 0.8, 1'),
                        tags$li('"Low and High Density Emphasis 1" divides them into the following: 0, 0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99, 1'),
                        tags$li('"Low and High Density Emphasis 2" divides them into the following: 0, 0.05, 0.1, 0.5, 0.9, 0.95, 1'),
                        tags$li('"Low Density Emphasis" divides them into the following: 0, 0.01, 0.05, 0.6, 0.8, 1'),
                        tags$li('"High Density Emphasis" divides them into the following: 0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1')), 
                      h3("Customizing the Color Palette"),
                      p('There are also additional options for viewing the map with various color palettes. Possible palettes to select between include Viridis, Plasma, Blue-Purple, 
                      Yellow-Green-Blue, and Greyscale, as shown below. The reverse option can be toggled on and off to reverse the palette on the map.'),
                      style = 'color: #005b96')

tool_descript2 <- div(h3('Generating Analysis'),
                      p('Additional options exist for analysis and abundance estimates for a specific area. For example, all of the initial maps we have made available only 
                              provide relative abundance (defined as the proportion of the population in a specific region). In order to make inference about absolute abundance, 
                              one must specify the total abundance of the population. For instance, such estimates could be taken from NMFS stock assessment reports.'),
                      p('Within the panel "Abundance Estimate", the user can input a population-wide abundance estimate, along with a coefficient of variation value (CV), 
                              to get an updated abundance estimate and legend. If no CV value is input, the default value is 0.2.'),
                      p('If the user has a shapefile to upload containing a specific area of interest, the panel "Custom Area Analysis" provides an upload button for the shapefile. 
                              The shapefile must contain a single polygon and must be provided in a zipped format. The user can also designate the polygon manually using the toolbar on 
                              the left of the map'),
                      p('Once the shape is uploaded or drawn, the button "Generate Analysis" in the "Custom Area Analysis" panel can be pressed, at which the the bottom tab below
                              the map, "Generated Custom Area Analysis", will output summary statistics, as well as a histogram that simulates possible abundances with the included uncertainty.'),
                      p(' If no abundance estimate value is inputted by the user, or an invalid value is inputted, it will default to the relative abundance estimates (abundance = 1), 
                              and a histogram will not be provided in the generated analysis.'),
                      style = 'color: #005b96')

# Methods tab
methods_title <- div('Methods', style = '#011f4b')

methods_info1 <- div('The statistical approaches for different data integration sub-projects differ depending on species and the types of data available. For select cetacean species, 
                     POP constitute the only data available and existing models developed with previous NMFS Protected Resources Toolbox funding can be applied directly to estimate species 
                     distributions (Ver Hoef et al., 2021). For species with more data sources (e.g., bearded seals, Cook Inlet beluga whales), partially or fully integrated species 
                     distribution models are needed, as described in Conn et al. (In prep) and presented at the 2023 PSAW conference. These models work similarly to fisheries stock-assessment 
                     models with inference conducted using a product log-likelihood, assuming that each data source is attempting to “sample” the underlying species distribution. At present, 
                     only POP-derived surfaces are available but more maps are expected to be added in the future. Notably, the POP estimates are produced using observations obtained 
                     between May and September, so only represents spatial distributions for the “ice free” period.', style = 'color: #005b96')

methods_info2 <- div(h3("How the POP Estimates are Generated"),
                     p('The estimates are generated using platform-of-opportunity (POP) analyses from Jay Ver Hoef.'),
                     p('Posterior means are taken from the resulting Bayesian analysis for each hexagon. The sum of these cells within the selected area is taken as  a relative abundance 
                       estimate, ranging between 0 and 1.  Uncertainty in this quantity is propagated by taking quantiles or calculating coefficient of variation (CV) from posterior (MCMC) 
                       samples.'),
                     p("When an abundance estimate and CV value is inputted by the user, a more in-depth estimate can be generated. The relative abundance estimate (a proportion) is 
                       multiplied by the absolute abundance estimate to generate an absolute abundance estimate for the selected area. Additionally, the user-input CV, which indicates the 
                       uncertainty in the user's inputted abundance estimate of the entire area, can be propagated into the small area estimate using Goodman's exact formula:"),
                     h5("$$Var(XY) = \\mu_x^2 Var(Y) + \\mu_y^2 Var(X) + Var(X) Var(Y)$$"),
                     p("Where mu_x and mu_y are expected values (E[X] and E[Y]) of the random variables. The following values would replace each of the components of the Goodman's 
                       exact formula (1960):"),
                     tags$ul(
                       tags$li("Mu (x): inputted user abundance"),
                       tags$li('Var(Y): variance from the MCMC chains in the filtered area'),
                       tags$li('Mu (y): sum of the posterior means of selected hexagons (between 0 and 1) in the filtered area'),
                       tags$li('Var(X): calculated by multiplying the user inputted abundance and the coefficient of variance, which yields the 
                                      standard error. Squared to then obtain variance.')),
                     p('This will then provide the new variance that takes into account both the uncertainty in the user inputted data and the POP analyses. It is converted to a coefficient 
                       of variation value for interpretability. Note that if a value is NOT provided, the CV will default to 0.2.'),
                     p('A histogram is also displayed, conveying uncertainty in the small area abundance estimate. In particular, we assume a log normal sampling distribution.'),
                     style = 'color: #005b96')

# Reference tab
licenses <- div(p("NOAA data is available under the CC-BY-4.0 license, which allows for unrestricted use. User must read and fully comprehend the metadata prior to use. 
                  Applications or inferences derived from the data should be carefully considered for accuracy. While every effort has been made to ensure that these data 
                  are accurate and reliable within the limits of the current state of the art, NOAA cannot assume liability for any damages caused by any errors or omissions 
                  in the data, nor as a result of the failure of the data to function on a particular system. NOAA makes no warranty, expressed or implied, nor does the fact 
                  of distribution constitute such a warranty. Acknowledge NOAA/NMFS/AFSC or the specified citation as the source from which these data were obtained in any 
                  publications and/or other representations of these data. Communication and collaboration with dataset authors is strongly encouraged."),
                style = 'color: #005b96')


load_all_filest <- function(directory) {
  # Loads all files from data folder 
  
  # Inputs: directory[str]: pathway to files
  
  # Returns: None, sets file name to loaded data
  
  
  # List all .RData files in the directory
  files <- list.files(directory, pattern = "\\.RData$", full.names = TRUE)
  #print(files) debugging
  if (length(files) == 0) {
    stop("No .RData files found in the directory.")
  }
  
  for (file in files) {
    # Get base name of the file (without directory and extension)
    file_base_name <- tools::file_path_sans_ext(basename(file))
    
    temp_env <- new.env()
    load(file, envir = temp_env)
    
    # Assuming the file contains only one object, get its name and data
    # Explore further this globalenv/tempenv issue 
    object_name <- ls(temp_env)[1]
    data <- get(object_name, envir = temp_env)
    
    # Assign the data to a variable named after the file
    assign(file_base_name, data, envir = .GlobalEnv)
    #print(file_base_name)
  }
  
  print("Finished loading.")
}


# Previously used code for adding MCMC data means to POPhexagons_sf data with
# MCMC means. "Commented out" as new datafile saved, but modify as necessary
# CANNOT be saved as .csv file or sf geom not preserved - .rda or .RData works I believe 

# POPhex_MCMC <- POPhexagons_sf
# 
# for (name in names(species_list)) {
#   data <- species_list[[name]]
#   rel_abund <- rowMeans(data, na.rm = TRUE)
#   POPhex_MCMC <- cbind(POPhex_MCMC, setNames(data.frame(rel_abund), name))
# }

# ============================ custom_area_analysis ============================

source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/helper_functions.R')
# source('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/ASDShiny/app.R')


#load_all_files(urls)

species_links <- list(
  "Northern Minke Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BA_MCMC.RData',
  "Fin Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BP_MCMC.RData',
  "Northern Fur Seal" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/CU_MCMC.RData',
  "Steller Sea Lion" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EJ_MCMC.RData',
  "Sea Otter" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/EL_MCMC.RData',
  "Gray Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/ER_MCMC.RData',
  "Pacific White-Sided Dolphin" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/LO_MCMC.RData',
  "Humpback Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/MN_MCMC.RData',
  "Killer Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OO_MCMC.RData',
  "Walrus" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/OR_MCMC.RData',
  "Dall's Porpoise" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PD_MCMC.RData',
  "Sperm Whale" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PM_MCMC.RData',
  "Harbor Porpoise" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PP_MCMC.RData',
  "Harbor Seal" = 'https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/PV_MCMC.RData'
)

# New dataframe containing the selected MCMC data (abundance calculations source)
# with the POPhexagons data (spatial data source) 

# row_variances <- apply(RelAbund_MCMC, 1, var)
# 
# POPdata_with_MCMC <- cbind(POPhex_MCMC, RelAbund_MCMC, row_variances)
#  
# 
# # Move across dateline
# POPdata_with_MCMC$geometry <- (sf::st_geometry(POPdata_with_MCMC) + c(360, 90)) %% c(360) - c(0, 90)
# 
# # # Set centroid values
# POPdata_with_MCMC$centroid.x <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,1]
# POPdata_with_MCMC$centroid.y <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,2]
# 
# # Get
# POPdata_with_MCMC$centroid.x <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,1]
#POPdata_with_MCMC$centroid.y <- st_coordinates(sf::st_centroid(POPdata_with_MCMC))[,2]

load(url('https://raw.githubusercontent.com/staciekoslovsky-noaa/ShinyApp_AtSeaDistribution/main/data/BP_MCMC.RData'))

# total_abundance_samples <- colSums(RelAbund_MCMC)
# 
# ggplot(data.frame(TotalAbundance = total_abundance_samples), aes(x = TotalAbundance)) +
#   geom_histogram(binwidth = 10, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
#   ggtitle("Histogram of MCMC Samples for Total Abundance") +
#   xlab("Total Abundance") +
#   ylab("Frequency") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 15, hjust = 0.5),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12))