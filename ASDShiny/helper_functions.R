load_all_files <- function(directory){
  # Loads all files from data folder. 
  
  # Inputs: directory[str]: pathway to files
  
  # Returns: None, just loads all files
  files <- list.files(directory, pattern = "\\.RData$", full.names = TRUE)
  
  for (file in files) {
    load(file, envir = .GlobalEnv) 
  }
}


load_data <- function(filepath) {
  # Loads the .RData file into distinct variables - otherwise all named "RelAbund_MCMC"
  
  # Inputs: filepath[str]
  
  # Returns: None, just laods all files 
  loaded_objects <- load(filepath)
  data <- get(loaded_objects[1])
  
  return(data)
}

#load_all_files("/Users/christinekwon/NOAAproject-CK-s24/ShinyApp_AtSeaDistribution/data")
#class(RelAbund_MCMC)


# DOES NOT INCLUDE POPhexagons_sf file!!!
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


# Optional function for filtering only areas where species is not NA
# Not used as to show areas that have all been surveyed and not found
filter_by_col <- function(sffile, col_name){
  sffile %>% filter(!is.na(.[[col_name]]))
}



# Text for the sidebar info pages - from Project Proposal 
purp <- div('Integrating Diverse Datasets to Understand the Seasonal Distributions
        and Densities of Marine Mammals', style = 'color: #03396c')

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

information2 <- div('Thus, this tool serves as a data portal to share species densities maps.')

tool_info <- 'This tool was developed using Shiny, a package that facilitates web app development directly 
from R and Python.'
tool_info2 <- 'To first access species density maps, click the Species button in the sidebar. Use the right panel
to toggle between different marine mammal species, and (more options later). The toolbar on the left of the map
contains various tools to work with select data shown on the map. The polyline, polygon and circle options on the toolbar 
can be used to draw polygons or lines that can be downloaded as a shapefile. The marker can be used to obtain coordinates
of the selected location. The bin will delete any shapes or lines that are no longer necessary.
The bottom panel contains buttons to download a shapefile and upload own shape data for analysis in one of the following
formats: zipped .kmz or .shp file.'

methods_title <- div('Methods and Approaches', style = 'color: #03396c')

methods_info <- div('The statistical approaches for different data integration sub-projects differ depending on species and the
types of data available. For select cetacean species, POP provide the only data available and
existing models developed with previous Toolbox funding can be applied directly to estimate species
distributions (Ver Hoef et al., 2021). For species with more data sources (e.g., bearded seals, Cook Inlet
beluga whales), partially or fully integrated species distribution models are needed, as described in Conn
et al. (In prep) and presented at the 2023 PSAW conference. These models work similarly to fisheries
stock-assessment models with inference conducted using a product log-likelihood, assuming that each
data source is attempting to “sample” the underlying species distribution. These models will use
existing data (i.e., no new data collection is anticipated, only processing and analysis of existing data).', style = 'color: #005b96')
  

  