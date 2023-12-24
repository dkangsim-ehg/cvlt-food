# CVLT-Food merging demographic and matching by standardized values

merge_match_data <- function(df,
                            outcome_name,
                            rename_col, 
                            start_col,  
                            list_name){
  
  # Rename "Event Name" column to "tp"
  df <- df %>% dplyr::rename(tp = {{rename_col}})
  # Gather columns from 3:end of last column number
  df <- df %>% gather(time, t, {{start_col}}:ncol(df))
  
  # Add a column indicating the list (A or B)
  df<- df%>% 
    mutate(list = ifelse(grepl("A", list_name), "ListA", "ListB"))
  
  # Define matching list based on the given list name
  matching_list <- switch(list_name,
                          "ListA" = ListA,
                          "ListB" = ListB)
  
  # Mutate t1r column to find the matching word in the matching_list
  df <- df %>% 
    arrange(ID, tp) %>% 
    mutate(t1r = map_chr(t, ~ {
      x <- .x
      matching_list <- matching_list[which(sapply(matching_list, 
                                                  function(y) any(x %in% y)))]
      if (length(matching_list) == 0) {
        NA_character_
      } else {
        matching_list[[1]][1]
      }
    }))%>%
    # Separate time column into "trials" and "words"
    separate(time, into = c("trials", "words"), sep="_")
  
  # Add a column "t.dup" to mark the duplicates in "t" column
  df <- df %>% 
    group_by(ID, tp, trials, t) %>%
    mutate(t.dup = ifelse(duplicated(t) | duplicated(t, fromLast = F),'r', 'c'))%>%
    # Add a column "t1r.dup" by combining "t1r" and "t.dup"
    mutate(t1r.dup = paste0(t1r, t.dup, sep=" "))
  
  # Create dynamic outcome variable name
  outcome_col1 <- paste0({{outcome_name}}, "_recall")
  outcome_col2 <- paste0({{outcome_name}}, "_recall_n")
  outcome_col3 <- paste0({{outcome_name}}, "_recall_cfh")
  outcome_col4 <- paste0({{outcome_name}}, "_recall_cfl")
  outcome_col5 <- paste0({{outcome_name}}, "_recall_cn")
  outcome_col6 <- paste0({{outcome_name}}, "_recall_ct")
  outcome_col6b <- paste0({{outcome_name}}, "_recall_cm")
  outcome_col7 <- paste0({{outcome_name}}, "_recall_ifh")
  outcome_col8 <- paste0({{outcome_name}}, "_recall_ifl")
  outcome_col9 <- paste0({{outcome_name}}, "_recall_in")
  outcome_col10 <- paste0({{outcome_name}}, "_recall_r")
  
  # Add a column for immediate free recall using the rec() function from sjmisc package
  df <- df %>% 
    mutate({{outcome_col1}} := 
             case_when(
               list == "ListA" ~ case_when(
                 str_trim(t1r.dup) == "cfhc" ~ "cfh",
                 str_trim(t1r.dup) %in% c("cfhr","cflr", "cnr", "ctr") ~ "r",
                 str_trim(t1r.dup) == "cflc" ~ "cfl",
                 str_trim(t1r.dup) == "cnc" ~ "cn",
                 str_trim(t1r.dup) == "ctc" ~ "ct",
                 str_trim(t1r.dup) %in% c("IFHc", "IFHr")~"ifh",
                 str_trim(t1r.dup) %in% c("IFLc", "IFLr") ~ "ifl",
                 str_trim(t1r.dup) %in% c("INc", "INr") ~ "in",
                 TRUE ~ NA_character_
               ), 
               list=="ListB" ~ case_when(
                 str_trim(t1r.dup) == "cfhc" ~ "cfh",
                 str_trim(t1r.dup) %in% c("cfhr","cflr", "cnr", "ctr") ~ "r",
                 str_trim(t1r.dup) == "cflc" ~ "cfl",
                 str_trim(t1r.dup) == "cnc" ~ "cn",
                 str_trim(t1r.dup) == "cmc" ~ "cm",
                 str_trim(t1r.dup) %in% c("IFHc", "IFHr")~"ifh",
                 str_trim(t1r.dup) %in% c("IFLc", "IFLr") ~ "ifl",
                 str_trim(t1r.dup) %in% c("INc", "INr") ~ "in",
                 TRUE ~ NA_character_
               )
             ))%>%
    group_by(ID, tp, trials, list, !!sym(outcome_col1)) %>% 
    summarize({{outcome_col2}} := n())%>% 
    spread(!!sym(outcome_col1), !!sym(outcome_col2))%>%
    mutate(ifh = if (!("ifh" %in% names(.))) NA else ifh,
           ifl = if (!("ifl" %in% names(.))) NA else ifl, 
           `in` = if (!("in" %in% names(.))) NA else `in`,
           r = if (!("r" %in% names(.))) NA else r)
  
#conditional select based on the ListA or ListB
  if(list_name == "ListA"){
    df<- df%>%dplyr::select(
      "ID", "tp", "trials", "list",
      "cfh", "cfl", "cn", "ct", 
      "ifh", "ifl", "in",
      "r")%>%
      dplyr::rename(
        "ID" = "ID",
        "timepts" = "tp",
        "trials" = "trials",
        !!sym(outcome_col3) := "cfh",
        !!sym(outcome_col4) := "cfl",
        !!sym(outcome_col5) := "cn",
        !!sym(outcome_col6) := "ct",
        !!sym(outcome_col7) := "ifh",
        !!sym(outcome_col8) := "ifl",
        !!sym(outcome_col9) := "in",
        !!sym(outcome_col10) := "r")%>%
      mutate(
        !!sym(outcome_col7) := ifelse(is.na(!!sym(outcome_col7)), 
                                      0, 
                                      !!sym(outcome_col7)),
        !!sym(outcome_col8) := ifelse(is.na(!!sym(outcome_col8)), 
                                      0, 
                                      !!sym(outcome_col8)),
        !!sym(outcome_col9) := ifelse(is.na(!!sym(outcome_col9)), 
                                      0, 
                                      !!sym(outcome_col9)),
        !!sym(outcome_col10) := ifelse(is.na(!!sym(outcome_col10)), 
                                       0, !!sym(outcome_col10))
      ) %>%
      ungroup()
  }else{df<- df%>% dplyr::select(
    "ID", "tp", "trials", "list",
    "cfh", "cfl", "cn", "cm", 
    "ifh", "ifl", "in",
    "r")%>%
    dplyr::rename(
      "ID" = "ID",
      "timepts" = "tp",
      "trials" = "trials",
      !!sym(outcome_col3) := "cfh",
      !!sym(outcome_col4) := "cfl",
      !!sym(outcome_col5) := "cn",
      !!sym(outcome_col6b) := "cm",
      !!sym(outcome_col7) := "ifh",
      !!sym(outcome_col8) := "ifl",
      !!sym(outcome_col9) := "in",
      !!sym(outcome_col10) := "r")%>%
    mutate(
      !!sym(outcome_col7) := ifelse(is.na(!!sym(outcome_col7)), 
                                    0, 
                                    !!sym(outcome_col7)),
      !!sym(outcome_col8) := ifelse(is.na(!!sym(outcome_col8)), 
                                    0, 
                                    !!sym(outcome_col8)),
      !!sym(outcome_col9) := ifelse(is.na(!!sym(outcome_col9)), 
                                    0, 
                                    !!sym(outcome_col9)),
      !!sym(outcome_col10) := ifelse(is.na(!!sym(outcome_col10)), 
                                     0, !!sym(outcome_col10))
    ) %>%
    ungroup()%>%
    arrange(ID, timepts)}

  #$ #scoring
  
  output<- "output"
  
  #define output folder
  out_path<- output
  
  if (!dir.exists(out_path)){
    dir.create(out_path)
  }
  
  #create a new folder to place surveys in
  if (!dir.exists(here(out_path,
                       Sys.Date()))){
    dir.create(here(out_path,
                    Sys.Date()))}
  
  correct_col<- paste0(outcome_name, "_correct")
  intrusion_col<- paste0(outcome_name, "_intrusion")
  repetition_col <- paste0(outcome_name, "_repetition")
  
  if(list_name == "ListA"){
    df<- df%>%
      mutate(!!correct_col := rowSums(select(.,
                                             !!sym(outcome_col3), 
                                        !!sym(outcome_col4),
                                        !!sym(outcome_col5),
                                        !!sym(outcome_col6)),
                                      na.rm=T),
             !!correct_col := ifelse(!!sym(correct_col) == 0,
                                     NA, 
                                     !!sym(correct_col)),
             !!intrusion_col := rowSums(select(., 
                                               !!sym(outcome_col7), 
                                               !!sym(outcome_col8),
                                               !!sym(outcome_col9)),
                                        na.rm=T),
             !!intrusion_col := ifelse(is.na(!!sym(correct_col)),
                                       NA, 
                                       !!sym(intrusion_col)),
             !!repetition_col := !!sym(outcome_col10),
             !!repetition_col := ifelse(is.na(!!sym(correct_col)),
                                        NA, 
                                        !!sym(repetition_col)))%>%
      write_csv( # save survey results 
        file = here(
          output,
          Sys.Date(),
          glue("cvlt_{outcome_name}_{Sys.Date()}.csv")))
  }else{#replace outcome_col6 to outcome_col6b 
    df<- df%>%
      mutate(!!correct_col := rowSums(select(.,
                                             !!sym(outcome_col3), 
                                             !!sym(outcome_col4),
                                             !!sym(outcome_col5),
                                             !!sym(outcome_col6b)),
                                      na.rm=T),
             !!correct_col := ifelse(!!sym(correct_col) == 0,
                                     NA, 
                                     !!sym(correct_col)),
             !!intrusion_col := rowSums(select(., 
                                               !!sym(outcome_col7), 
                                               !!sym(outcome_col8),
                                               !!sym(outcome_col9)),
                                        na.rm=T),
             !!intrusion_col := ifelse(is.na(!!sym(correct_col)),
                                       NA, 
                                       !!sym(intrusion_col)),
             !!repetition_col := !!sym(outcome_col10),
             !!repetition_col := ifelse(is.na(!!sym(correct_col)),
                                        NA, 
                                        !!sym(repetition_col)))%>%
      write_csv( # save survey results 
        file = here(
          output,
          Sys.Date(),
          glue("cvlt_{outcome_name}_{Sys.Date()}.csv")))
  }
  
  
  #only for immediate recall task where there are trials are repeated 5 times
  if(outcome_name == "trials1_5"){
    df<- df%>%
      group_by(ID, timepts)%>%
      summarise(across(c(!!sym(correct_col),
                         !!sym(intrusion_col),
                         !!sym(repetition_col)), 
                       ~sum(.x, na.rm=F)))%>%
      ungroup()%>%
      write_csv( # save survey results 
        file = here(
          output,
          Sys.Date(),
          glue("cvlt_scored_{outcome_name}_{Sys.Date()}.csv")))
  }
}
  

