
parse_cvlt_columns <- function(df, folder) {
  
  #define output folder
  out_path<- folder
  
  if (!dir.exists(out_path)){
    dir.create(out_path)
  }
  
  #create a new folder to place surveys in
  if (!dir.exists(here(out_path,
                       Sys.Date()))){
    dir.create(here(out_path,
                 Sys.Date()))}
  
  #split trials
  cvlt.trials1_5<- df%>%
    dplyr::select("ID", "Event Name", 
                  matches("^Trial \\d+ Word") &
                    !matches("Intrusion Raw Word"))%>%
    rename_with(~str_replace_all(.x,
                                 "^Trial (\\d+) Word (\\d+)",
                                 "t\\1_w\\2"))%>%
    mutate(across(everything(), 
                  ~ if_else(. %in% 
                              c("BLANK. (Previous word wast last for this trial)"), 
                            NA,
                            .)))%>%
    write_csv( # save survey results 
      file = here(
        out_path,
        Sys.Date(),
        glue("cvlt_trials1_5.csv")))
  

  cvlt.list_b<- df%>%
    dplyr::select("ID", "Event Name", 
                  matches("^Trial B Word") &
                    !matches("Intrusion"))%>%
    rename_with(~str_replace_all(.x,
                                 "^Trial B Word (\\d+)",
                                 "t\\b_w\\1"))%>%
    mutate(across(everything(), 
                  ~ if_else(. %in% 
                              c("BLANK. (Previous word wast last for this trial)"), 
                            NA,
                            .)))%>%
    write_csv( # save survey results 
      file = here(
        out_path,
        Sys.Date(),
        glue("cvlt_list_b.csv")))
  
  ##
  cvlt.sd_free<- df%>%
    dplyr::select("ID", "Event Name", 
                  matches("^Short-Delay Free") &
                    !matches("Intrusion"))%>%
    rename_with(~str_replace_all(.x,
                                 "^Short-Delay Free Recall Word (\\d+)",
                                 "sdfr_w\\1"))%>%
    mutate(across(everything(), 
                  ~ if_else(. %in% 
                              c("BLANK. (Previous word wast last for this trial)"), 
                            NA,
                            .)))%>%
    write_csv( # save survey results 
      file = here(
        out_path,
        Sys.Date(),
        glue("cvlt_sd_free.csv")))
  
  ###
  cvlt.sd_cued<- df%>%
    dplyr::select("ID", "Event Name", 
                  matches("^Short-Delay Cued") &
                    !matches("Intrusion"))%>%
    rename_with(~str_replace_all(.x,
                                 "Short-Delay Cued Recall Furniture Word (\\d+)",
                                 "sdcue_fu\\1"))%>%
    rename_with(~str_replace_all(.x,
                                 "Short-Delay Cued Recall Vegetables Word (\\d+)",
                                 "sdcue_ve\\1"))%>%
    rename_with(~str_replace_all(.x,
                                 "Short-Delay Cued Recall Traveling Word (\\d+)",
                                 "sdcue_tr\\1"))%>%
    rename_with(~str_replace_all(.x,
                                 "Short-Delay Cued Recall Snack Food Word (\\d+)",
                                 "sdcue_sn\\1"))%>%
    mutate(across(everything(), 
                  ~ if_else(. %in% 
                              c("BLANK. (Previous word wast last for this trial)"), 
                            NA,
                            .)))%>%
    dplyr::select(-c("Short-Delay Cued Recall END TIME"))%>%
    write_csv( # save survey results 
      file = here(
        out_path,
        Sys.Date(),
        glue("cvlt_sd_cued.csv")))
  
  ###
  cvlt.ld_free<- df%>%
    dplyr::select("ID", "Event Name", 
                  matches("^Long-Delay Free") &
                    !matches("Intrusion"))%>%
    rename_with(~str_replace_all(.x,
                                 "^Long-Delay Free Recall Word (\\d+)",
                                 "ldfr_w\\1"))%>%
    mutate(across(everything(), 
                  ~ if_else(. %in% 
                              c("BLANK. (Previous word wast last for this trial)"), 
                            NA,
                            .)))%>%
    write_csv( # save survey results 
      file = here(
        out_path,
        Sys.Date(),
        glue("cvlt_ld_free.csv")))
  
  ##
  cvlt.ld_cued<- df%>%
    dplyr::select("ID", "Event Name", 
                  matches("^Long-Delay Cued") &
                    !matches("Intrusion"))%>%
    rename_with(~str_replace_all(.x,
                                 "Long-Delay Cued Recall Furniture Word (\\d+)",
                                 "ldcue_fu\\1"))%>%
    rename_with(~str_replace_all(.x,
                                 "Long-Delay Cued Recall Vegetables Word (\\d+)",
                                 "ldcue_ve\\1"))%>%
    rename_with(~str_replace_all(.x,
                                 "Long-Delay Cued Recall Traveling Word (\\d+)",
                                 "ldcue_tr\\1"))%>%
    rename_with(~str_replace_all(.x,
                                 "Long-Delay Cued Recall Snack Food Word (\\d+)",
                                 "ldcue_sn\\1"))%>%
    mutate(across(everything(), 
                  ~ if_else(. %in% 
                              c("BLANK. (Previous word wast last for this trial)"), 
                            NA,
                            .)))%>%
    dplyr::select(-c("Long-Delay Cued Recall START TIME"))%>%
    write_csv( # save survey results 
      file = here(
        out_path,
        Sys.Date(),
        glue("cvlt_ld_cued.csv")))
  ###
  cvlt.recog<- df%>%
    dplyr::select("ID", "Event Name", 
                  matches("^\\d+."))%>%
    rename_with(~str_replace_all(.x,
                                 "^(\\d+). (.+)",
                                 "recog.\\1"), 
                everything())%>%
    write_csv( # save survey results 
      file = here(
        out_path,
        Sys.Date(),
        glue("cvlt_recog.csv")))
  
}


load_csv_files <- function(directory) {
  # Get the list of CSV files in the directory
  csv_files <- list.files(directory, 
                          pattern = "*.csv", 
                          full.names = T)
  
  # Loop through the CSV files and read them
  for (file in csv_files) {
    # Extract the file name without extension
    file_name <- tools::file_path_sans_ext(basename(file))
    
    # Read the CSV file and assign it to a variable
    assign(file_name, 
           read_csv(file))
  }
}



