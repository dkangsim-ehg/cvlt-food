
load_and_join_csv_files <- function(directory, demo) {
  # Get the list of CSV files in the directory
  csv_files <- list.files(directory, 
                          pattern = "*.csv", 
                          full.names = TRUE)
  
  # Loop through the CSV files
  for (file in csv_files) {
    # Extract the file name without extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Read the CSV file
    data<- read_csv(file)%>%
      left_join(demo, by="ID")#%>%
      #na.exclude()
    
    assign(file_name, data, envir = .GlobalEnv)
  }
}