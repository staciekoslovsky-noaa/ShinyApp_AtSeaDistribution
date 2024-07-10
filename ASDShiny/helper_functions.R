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



load_all_filest <- function(directory) {
  # Loads all files from data folder 
  
  # Inputs: directory[str]: pathway to files
  
  # Returns: None, sets file name to loaded data
  
  
  # List all .RData files in the directory
  files <- list.files(directory, pattern = "\\.RData$", full.names = TRUE)
  print(files)
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
