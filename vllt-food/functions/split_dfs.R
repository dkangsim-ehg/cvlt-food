
parse_by_age_male <- function(data, age_groups) {
  output <- list()
  
  for (age_group in names(age_groups)) {
    age_range <- age_groups[[age_group]]
    
    data_name <- sub("^cvlt_", "", deparse(substitute(data)))
    data_name <- gsub("_\\d+_\\d+_\\d+", "", data_name)  # Remove the numeric part
    correct_column <- paste0(data_name, "_correct")
    
      filtered_data <- data %>%
        filter(age >= age_range[1] & age < age_range[2]) %>%
        filter(sex==1)%>%
        arrange(ID, timepts) %>%
        select(ID, timepts, correct_column)
    
    #output_name <- paste(correct_column, 
    #                     if (sex == 1) "m" else "f", 
    #                     age_group, sep = ".")
    output_name <- paste(correct_column, 
                         "m", 
                         age_group, 
                         sep = ".")
    
    output[[output_name]] <- filtered_data

    
    # Save data frame into Global Environment
    assign(output_name, filtered_data, envir = .GlobalEnv)
  }
}

##
parse_by_age_female <- function(data, age_groups) {
  output <- list()
  
  for (age_group in names(age_groups)) {
    age_range <- age_groups[[age_group]]
    
    data_name <- sub("^cvlt_", "", deparse(substitute(data)))
    data_name <- gsub("_\\d+_\\d+_\\d+", "", data_name)  # Remove the numeric part
    correct_column <- paste0(data_name, "_correct")
    
    filtered_data <- data %>%
      filter(age >= age_range[1] & age < age_range[2]) %>%
      filter(sex==2)%>%
      arrange(ID, timepts) %>%
      select(ID, timepts, correct_column)
    
    #output_name <- paste(correct_column, 
    #                     if (sex == 1) "m" else "f", 
    #                     age_group, sep = ".")
    output_name <- paste(correct_column, 
                         "f", 
                         age_group, 
                         sep = ".")
    
    output[[output_name]] <- filtered_data
    
    #output_name <- paste(correct_column, "m", age_group, sep = ".")
    #data_output[[output_name]] <- filtered_data
    # Save data frame into Global Environment
    assign(output_name, filtered_data, envir = .GlobalEnv)
    
  }
}



