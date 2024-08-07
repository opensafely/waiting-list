###############################################################
# This script creates summary statistics for all key variables
# for all people with a closed RTT pathway (May21-Apr22)
###############################################################

# For running locally only #
#setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/waiting-list")
#getwd()


## Import libraries
library('tidyverse')
library('lubridate')
library('here')
library('dplyr')
library('ggplot2')
library('zoo')
library('reshape2')
library('fs')
library('rlang')

## Rounding function
source(here("analysis", "custom_functions.R"))

## Create directories if needed
dir_create(here::here("output", "clockstops"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)


## Load data ##
ortho_routine_final <- read_csv(here::here("output", "data", "cohort_ortho_routine_clockstops.csv.gz"),
                        col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                                         rtt_end_date = col_date(format="%Y-%m-%d"),
                                         reg_end_date = col_date(format="%Y-%m-%d"),
                                         dod = col_date(format="%Y-%m-%d"),
                                         end_date = col_date(format="%Y-%m-%d"),
                                         rtt_start_month =  col_date(format="%Y-%m-%d"),
                                         rtt_end_month =  col_date(format="%Y-%m-%d"))) %>%
  
  mutate(oa = ifelse(oa == TRUE, "Yes", "No"),
         hip_hrg = ifelse(hip_hrg == TRUE, "Yes", "No"),
         knee_hrg = ifelse(knee_hrg == TRUE, "Yes", "No")) %>%
  
  dplyr::select(c(patient_id, starts_with(c("opioid_", "short_opioid", "long_opioid", 
                                            "moderate_opioid","weak_opioid", "strong_opioid")), 
                  age_group, sex, imd10, ethnicity6, region, prior_opioid_rx, 
                  long_term_opioid, wait_gp,censor_before_study_end, num_weeks, oa, hip_hrg, knee_hrg)) %>%
  
  # Only keep count variables
  dplyr::select(!c(ends_with(c("_any1","_any2","_any")))) 


# Transpose data for prescribing in past 6months only
ortho_routine_final_6mos <- ortho_routine_final %>%
  
  dplyr::select(!c(ends_with(c("_count2","_count")))) %>%
  
  # Transpose to long
  reshape2::melt(id = c("patient_id","age_group","sex","imd10","ethnicity6", "region",
                        "prior_opioid_rx","long_term_opioid","wait_gp","censor_before_study_end",
                        "num_weeks", "oa", "hip_hrg", "knee_hrg")) %>%
  
    # Variables for any prescribing, and >=3 prescriptions
  mutate(med_any_6mos = ifelse(value >= 1, 1, 0),
         med_3more_6mos = ifelse(value >= 3, 1, 0),
         
        period = case_when(
          grepl("pre_", variable) ~ "Pre-WL",
          grepl("wait_", variable) ~ "During WL", 
          grepl("post_", variable) ~ "Post WL"),
        
        measure = case_when(
          grepl("short_opioid", variable) ~ "Short-acting opioid",
          grepl("long_opioid", variable) ~ "Long-acting opioid",
          grepl("weak_opioid", variable) ~ "Weak opioid",
          grepl("strong_opioid1", variable) ~ "Strong opioid",
          grepl("strong_opioid2", variable) ~ "Strong opioid 2",
          grepl("moderate_opioid", variable) ~ "Moderate opioid",
          TRUE ~ "Any opioid")) %>%
  
  dplyr::select(!c("value", "variable"))
  

# Transpose data for prescribing in past 3months only
ortho_routine_final_3mos <- ortho_routine_final %>%
  
  dplyr::select(!c(ends_with(c("_count1","_count")))) %>%
  
  # Transpose to long
  reshape2::melt(id = c("patient_id","age_group","sex","imd10","ethnicity6", "region",
                        "prior_opioid_rx","long_term_opioid","wait_gp","censor_before_study_end",
                        "num_weeks", "oa", "hip_hrg", "knee_hrg")) %>%
  
  # Variables for any prescribing, and >=3 prescriptions
  mutate(med_any_3mos = ifelse(value >= 1, 1, 0),
         med_3more_3mos = ifelse(value >= 3, 1, 0),

         period = case_when(
           grepl("pre_", variable) ~ "Pre-WL",
           grepl("wait_", variable) ~ "During WL", 
           grepl("post_", variable) ~ "Post WL"),
         measure = case_when(
           grepl("short_opioid", variable) ~ "Short-acting opioid",
           grepl("long_opioid", variable) ~ "Long-acting opioid",
           grepl("weak_opioid", variable) ~ "Weak opioid",
           grepl("strong_opioid1", variable) ~ "Strong opioid",
           grepl("strong_opioid2", variable) ~ "Strong opioid 2",
           grepl("moderate_opioid", variable) ~ "Moderate opioid",
           TRUE ~ "Any opioid")) %>%
  
  dplyr::select(!c("value", "variable"))
  

# Combine both
ortho_routine_final_both <- merge(ortho_routine_final_6mos, ortho_routine_final_3mos,
                                  by = c("patient_id","age_group","sex","imd10","ethnicity6", "region",
                                         "prior_opioid_rx","long_term_opioid","wait_gp","censor_before_study_end",
                                         "num_weeks", "oa", "hip_hrg", "knee_hrg","period","measure"))

# Only keep pre- and post-WL time periods
ortho_routine_final_2 <- ortho_routine_final_both %>%
  subset(period != "During WL")


######### Medicine prescribing frequencies - overall and stratified ############

dat <- ortho_routine_final_2 

meds <- rbind(
    meds_dist(full, "Full cohort"),
    meds_dist(age_group, "Age"),
    meds_dist(imd10, "IMD"),
    meds_dist(region, "Region"),
    meds_dist(ethnicity6, "Ethnicity"),
    meds_dist(sex, "Sex"),
    meds_dist(long_term_opioid, "Long-term opioid"),
    meds_dist(wait_gp, "Time on waiting list"),
    meds_dist(oa, "OA diagnosis"),
    meds_dist(hip_hrg, "Hip HRG"),
    meds_dist(knee_hrg, "Knee HRG")
    ) 
  
meds <- meds[,c("cohort", "variable", "category", "period", "measure", 
                "total",
                "count_any_6mos", "count_any_3mos", 
                "count_3more_6mos", "count_3more_3mos") ] 

write.csv(meds, here::here("output", "clockstops", "med_by_period.csv"), row.names = FALSE)



#########################################################################
############ By prior opioid prescribing and wait time ##################
#########################################################################

####### Medicine prescribing frequencies - 
####### by long-term opioid use and wait time ####


# FUll cohort 
dat <- ortho_routine_final_2 

prior_wait <- rbind(
  meds_dist_wait(full, "Full cohort"),
  meds_dist_wait(oa, "Osteoarthritis diagnosis") ,
  meds_dist_wait(hip_hrg, "Hip procedure"),  
  meds_dist_wait(knee_hrg, "Knee procedure")
)


prior_wait <- prior_wait[,c("cohort", "long_term_opioid", 
                            "wait_gp", "period", "measure", "variable",
                            "category", "count_any_6mos","count_any_3mos",
                            "count_3more_6mos", "count_3more_3mos", "total") ]

write.csv(prior_wait, here::here("output", "clockstops", "med_by_period_wait.csv"),
          row.names = FALSE)


