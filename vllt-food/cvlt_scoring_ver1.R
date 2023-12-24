##################
# Prepared by D. Eastern Kang Sim, PhD
# 07/17/2023
# This script does: 
# Wrangles to output for trials1_5, list_b, short-delay_free, short-delay_cued,
# long-delay_free, long-delay_cued

# load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(glue, here, tidyverse, 
               foreach, future , tictoc)

# setup stage
# check your working directory (CHEAR/workingfolder/PACIFIC FIT)
getwd() #if working directory is not properly set up, use the setwd() to set the path

cvlt.df<- read_csv(here("input",
  "test_input.csv"))

########################################################################
# 1. Parse CVLT data by tasks (trials1_5, list_b, sd_free, sd_cued,
# ld_free, ld_cued, recog)

#load the custom function
source(here("functions",
            "parse_cvlt_columns.R"))

source(here("functions",
            "cvlt_list_keys.R"))

parse_cvlt_columns(df= cvlt.df,
                   folder = "input")

#load all parsed files into the environment
directory<- here("input", 
                 Sys.Date()) #change the directory as needed

#get the list of CSV files in the directory
csv_files<- list.files(directory, 
                       pattern="*.csv",
                       full.names = T) #1:7 file paths
#full.names=T include the directory path; "False" if only file names are needed


# Loop through the CSV files and read them
for (file in csv_files) {
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the CSV file and assign it to a variable
  assign(file_name, read_csv(file))
}

########################################################################
# 2. Count by semantic categories (correct, intrusion, and repetition)

#load the custom function
source(here("functions",
            "cvlt_cleaning_counting.R"))

#scored files are saved in the "output/Sys.date()" folder
clean_score_data(df=cvlt_trials1_5, 
                 outcome_name= "trials1_5_free",
                 rename_col="Event Name", 
                 start_col=t1_w1, 
                 list_name = "ListA")

clean_score_data(df=cvlt_list_b, 
                 outcome_name= "list_b",
                 rename_col="Event Name", 
                 start_col=tb_w1, 
                 list_name = "ListB")

clean_score_data(df=cvlt_sd_free, 
                 outcome_name= "sd_free",
                 rename_col="Event Name", 
                 start_col=sdfr_w1, 
                 list_name = "ListA")

clean_score_data(df=cvlt_ld_free, 
                 outcome_name= "ld_free",
                 rename_col="Event Name", 
                 start_col=ldfr_w1, 
                 list_name = "ListA")

clean_score_data(df=cvlt_sd_cued, 
                 outcome_name= "sd_cued",
                 rename_col="Event Name", 
                 start_col=c("sdcue_fu1", "sdcue_ve1",
                             "sdcue_tr1", "sdcue_sn1"), 
                 list_name = "ListA")

clean_score_data(df=cvlt_ld_cued, 
                 outcome_name= "ld_cued",
                 rename_col="Event Name", 
                 start_col=c("ldcue_fu1", "ldcue_ve1",
                             "ldcue_tr1", "ldcue_sn1"), 
                 list_name = "ListA")

########################################################################
# 3. Merging demographics to the scored data
# keys are saved in the "cvlt_list_keys.R" (loaded above)

# load demographic varaibles 
# change directory as needed
demo<- read_csv(file=here("input", 
                          "pacific_fit_redcap_input_demographics.csv"))%>%
  dplyr::select(ID, age, sex)#%>%
  #na.exclude()

source(here("functions",
            "load_join.R"))

#load all scored files into the environment
directory_scored<- here("output", 
                        Sys.Date()) #change the directory as needed

load_and_join_csv_files(directory_scored, demo)

########################################################################
# 4. Parse scored data by age and sex
source(here("functions",
            "split_dfs.R"))

parse_by_age_male(data=cvlt_ld_cued,
                  age_groups = age_groups)

parse_by_age_male(data=cvlt_sd_cued,
                  age_groups = age_groups)

parse_by_age_male(data=cvlt_ld_free,
                  age_groups = age_groups)

parse_by_age_male(data=cvlt_sd_free,
                  age_groups = age_groups)

parse_by_age_male(data=cvlt_list_b,
                  age_groups = age_groups)

parse_by_age_male(data=cvlt_trials1_5_free,
                  age_groups = age_groups)

#female
parse_by_age_female(data=cvlt_ld_cued,
                  age_groups = age_groups)

parse_by_age_female(data=cvlt_sd_cued,
                  age_groups = age_groups)

parse_by_age_female(data=cvlt_ld_free,
                  age_groups = age_groups)

parse_by_age_female(data=cvlt_sd_free,
                  age_groups = age_groups)

parse_by_age_female(data=cvlt_list_b,
                  age_groups = age_groups)

parse_by_age_female(data=cvlt_trials1_5_free,
                  age_groups = age_groups)


########################################################################
# 5. Match by age and gender, then merge rows