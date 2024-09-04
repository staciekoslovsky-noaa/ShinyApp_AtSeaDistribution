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
information <- div('Understanding seasonal distributions and densities of marine mammals remains a high priority for
                    NMFS science centers and regional offices. In addition to answering basic ecological questions, such
                    information is frequently requested of science center staff to help in calculation of “takes” under the
                    Marine Mammal Protection Act (MMPA) and the Endangered Species Act (ESA) (e.g., due to offshore
                    energy development, naval exercises, aquaculture permitting, pier construction, or hazard remediation). 
                    Calculation of at-sea densities remains challenging however, as many Arctic marine mammal surveys
                    are timed to occur during spring or summer months when animals are the most accessible (and often on
                    land in the case of many pinniped species). But the continuing need for such density estimates for
                    management purposes has led to approximating distributions based on suboptimal approaches such as
                    redistributing animals from haul-out and rookery locations, or applying spring and summer distributions
                    to other seasons in order to calculate takes. However, we continue to believe that there are better
                    approaches. In particular, there is a large amount of data that may be leveraged to provide more refined
                    estimates of seasonal densities, including scientific surveys, satellite telemetry, acoustic detections,
                    Alaska Native subsistence harvests, and platform-of-opportunity (POP) observations.', style = 'color: #005b96')

info_two <- div(p('Thus, this tool serves as a data portal to share species densities maps.'), 
                    p("Marine spatial planning requires knowledge of the timing and location of marine mammal distribution,
                      migrations, density in local areas, and movements to mitigate anthropogenic impacts on protected
                      species. The ability to combine data from surveys, POP, telemetry, and passive acoustic recorders will
                      shrink spatial-temporal data gaps and provide managers with a product that represents the best scientific
                      information available. This more complete information can then be used to assist in NEPA analyses,
                      ESA Section 7 consultations, MMPA Incidental Harassment Authorizations, and other permit and
                      management needs. Further, because the gaps present in the data proposed for development of this
                      toolbox are not unique to the Alaska Region, the framework developed here will have broad
                      applicability beyond Arctic ecosystems and for all NMFS science centers and regional offices.
                      The potential benefits of these analytical approaches are expected to reach well beyond just NMFS.
                      Other federal and state agencies, tribal governments and Alaska Native organizations, non-governmental
                      organizations, and a variety of industry and community stakeholders seek the type of information
                      products that this project is targeting. Information about seasonal patterns of marine mammal density
                      distributions and habitat use will help these groups to better assess potential impacts, implement
                      mitigation actions where appropriate, and develop plans that are more fully informed by science.
                      Examples of specific federal agencies that have expressed a desire for access to better information on the
                      seasonal locations and densities of marine mammals include BOEM and the U.S. Navy."), style = 'color: #005b96')


# How to Use Tab 
tool_info <- div('This tool was developed using Shiny, a package that facilitates web app development directly 
                  from coding languages such as R.', style = 'color: #005b96')
tool_info1 <- div(p("To first access species density maps, click the Species button in the sidebar. 
                  Use the right panel to toggle between different marine mammal species."), style = 'color: #005b96')
                  
tool_description <- div(h3("Using the Draw Toolbar"),
                      p("The toolbar on the left of the map contains various tools to work with select data shown on the map. 
                        The polygon, rectangle, and circle options on the toolbar can be used to draw shapes
                        that can be downloaded as a shapefile. 
                        The marker can be used to obtain coordinates of the selected location. 
                        The trash bin will delete any shapes or lines that are no longer necessary.
                        The bottom panel contains buttons to download a shapefile of the drawn polygons and upload the user's own shape data
                        for analysis in one of the following formats: zipped .kmz or .shp file."),
  
                    h3("Customizing the Legend"),
                      'The legend in the map can be customized using the "Select Legend" option in the sidebar.
                       The options are named:',
                      tags$ul(
                              tags$li('"Quintiles" divides them into the following percentiles: 0, 0.2, 0.4, 0.6, 0.8, 1'),
                              tags$li('"Low and High Density Emphasis 1" divides them into the following: 0, 0.01, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.99, 1'),
                              tags$li('"Low and High Density Emphasis 2" divides them into the following: 0, 0.05, 0.1, 0.5, 0.9, 0.95, 1'),
                              tags$li('"Low Density Emphasis" divides them into the following: 0, 0.01, 0.05, 0.6, 0.8, 1'),
                              tags$li('"High Density Emphasis" divides them into the following: 0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1')), 
                    h3("Customizing the Color Palette"),
                    p('There are also additional options for viewing the map with various color palettes. 
                      Possible palettes to select between include Viridis, Plasma, Blue-Purple, Yellow-Green-Blue, and Greyscale, as shown below.
                      The reverse option can be toggled on and off to reverse the palette on the map.'),
                    style = 'color: #005b96')

tool_description_cont <- div(h3('Generating Analysis'),
                            p('Additional options exist for analysis and abundance estimates for a specific area.'),
                  
                            p('Within the panel "Abundance Estimate", the user can input a total abundance for the area, along with a coefficient of variation value (CV),
                              to get an updated abundance estimate and legend. If no CV value is inputted, the default value is 0.2.'),
                            p('If the user has a shapefile to upload containing a specific area, the panel "Custom Area Analysis" provides an upload button for
                              the shapefile. 
                              The shapefile must contain a single area, and must be provided in a zipped format.
                              The user can also designate the area manually using the toolbar on the left of the map.'),
                            p('One the shape is uploaded or drawn, the button "Generate Analysis" in the "Custom Area Analysis" panel can be pressed, at which the
                              the bottom tab below the map, "Generated Custom Area Analysis", will output summary statistics, as well as a histogram that simulates
                              possible abundances with the included uncertainty.
                              If no abundance estimate value is inputted by the user, or an invalid value is inputted, it will default to the relative abundance estimates (abundance = 1), and 
                              a histogram will not be provided in the generated analysis.
                              '),
                             style = 'color: #005b96')
                      
tool_info3 <- div(h3('Additional Questions?'),
                  p('For any additional questions on code maintenance, contact Stacie Koslovsky. For additional questions
                  on the the statistical analysis, contact ___.'), style = 'color: #005b96')
tool_info4 <- div('For further reference, the code base can be found on GitHub, at the following link:____', style = 'color: #005b96')


# Methods tab
methods_title <- div('Methods and Approaches', style = '#011f4b')

methods_info <- div('The statistical approaches for different data integration sub-projects differ depending on species and the
                    types of data available. For select cetacean species, POP provide the only data available and
                    existing models developed with previous Toolbox funding can be applied directly to estimate species
                    distributions (Ver Hoef et al., 2021). For species with more data sources (e.g., bearded seals, Cook Inlet
                    beluga whales), partially or fully integrated species distribution models are needed, as described in Conn
                    et al. (In prep) and presented at the 2023 PSAW conference. These models work similarly to fisheries
                    stock-assessment models with inference conducted using a product log-likelihood, assuming that each
                    data source is attempting to “sample” the underlying species distribution. These models will use
                    existing data (i.e., no new data collection is anticipated, only processing and analysis of existing data).',
                    style = 'color: #005b96')

methods_info2 <- div(h3("How the Estimates are Generated"),
                     p('The estimates are generated using platform-of-opportunity (POP) analyses from Jay Ver Hoef.'),
                     p('Posterior means are taken from the MCMC chain analysis, and this leads to a posterior mean estimate
                     for each individual cell/hexagon. The sum of these cells within the selected area obtains a relative abundance estimate, 
                       ranging between 0 and 1.'),
                     p("When an abundance estimate and coefficient of variation value is inputted by the user, 
                        a more in-depth estimate can be generated.
                        The relative abundance estimate is multipied by the abundance estimate to provide a posterior mean estimate
                        within the same selected area. 
                        Additionally, the coefficient of variation, which indicates the uncertainty in the user's inputted abundance estimate of
                        the entire area, can be used to then estimate the uncertainty in the posterior mean estimate.
                        This is found using the Goodman's formula:"),
                     h5("$$Var(XY) = \\mu_x^2 Var(Y) + \\mu_y^2 Var(X) + Var(X) Var(Y)$$"),
                     p("Where $\\mu_x$ and $\\mu_y$ are expected values (E[X] and E[Y]) of the random variables.
                       The following values would replace each of the components of the Goodman's Formula:"),
                       tags$ul(
                              tags$li("$\\mu_x$: inputted user abundance d$\\mu_y$"),
                              tags$li('Var(Y): variance from the MCMC chains in the filtered area'),
                              tags$li('$\\mu_y$: sum of the posterior means of selected hexagons (between 0 and 1) in the filtered area'),
                              tags$li('Var(X): calculated by multiplying the user inputted abundance and the coefficient of variance, which yields the 
                                      standard error. Squared to then obtain variance.')),
                     p('This will then provide the new variance that takes into account both the uncertainty in the
                        user inputted data and the POP analyses. It is converted to a coefficient of variation value
                        for interpretability. Note that if a value is NOT provided, the CV will default to 0.2.'),
                     p('A histogram is also displayed, which uses the relative abundance proportions in the MCMC chains with the user inputted 
                     abundance estimate. However, it also takes in the uncertainty in the abundance estimate. A log normal sampling distribution
                       is simulated, taking into account both the estimate and the CV value, and this is then multipled along with the MCMC chains (1000 columns/1000 simulations
                       to result in a histogram propoagates uncertainity as shown in its spread.'),
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
  