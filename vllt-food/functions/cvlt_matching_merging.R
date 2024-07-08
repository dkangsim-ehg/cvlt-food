# CVLT-Food merging demographic and matching by standardized values

# Due to copyrights of the CVLT-II normative standards, the matching and merging function is not included.
# Interested researchers are encouraged to refer to the normative standard tables by age and sex as listed in the appendix of the manual.

#General strategy:

# step 1. parse count data by age groups (age groups: 20_29; 30_44, etc.)

# step 2. make a list of keys
# list_sex_age_grp<- list (c("5", "0", "1"),
#                           c("6", "2", "3") ...)
  
# step 3. match count data by list of keys
# lappy(list_sex_age_grp, function(x) {df_sex_age_grp$tscore_free[df_sex_age_grp$imm_free_correct %in% x[-1]] <<- x[[1]]

# step 4. merge data
# merged_df<- bind_rows (df_sex_age_grp1, df_sex_age_grp2, etc.)


