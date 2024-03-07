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
  
  dplyr::select(c(patient_id, ends_with(c("_count")), post_time_adj, 
                  wait_time_adj, pre_time, age_group, sex, imd10, 
                  ethnicity6, region, prior_opioid_rx, wait_gp,
                  censor_before_study_end)) %>%
  
  # Transpose to long
  reshape2::melt(id = c("patient_id","age_group","sex","imd10","ethnicity6", "region",
                        "prior_opioid_rx", "wait_gp", "censor_before_study_end")) %>%
  
  # Create new variables for time period and medicine type
  mutate(period = case_when(
                      grepl("pre_", variable) ~ "Pre-WL",
                      grepl("wait_", variable) ~ "During WL", 
                      grepl("post_", variable) ~ "Post WL"),
         measure = case_when(
                      grepl("time", variable) ~ "Person time",
                      grepl("short_opioid", variable) ~ "Short-acting opioid",
                      grepl("long_opioid", variable) ~ "Long-acting opioid",
                      grepl("weak_opioid", variable) ~ "Weak opioid",
                      grepl("strong_opioid", variable) ~ "Strong opioid",
                      grepl("gabapentinoid", variable) ~ "Gabapentinoid",
                      grepl("antidepressant", variable) ~ "Antidepressant",
                      grepl("nsaid", variable) ~ "NSAID",
                      grepl("tca", variable) ~ "TCA",
                      TRUE ~ "Any opioid"),
         
         # Variables for any prescribing, and >=3 prescriptions
         med_any = ifelse(value >= 1, 1, 0),
         med_3plus = ifelse(value >= 3, 1, 0)) 

# Split out person-time and merge it back in
person_time <- ortho_routine_final %>%
    subset(measure == "Person time") %>%
    dplyr::select(c(patient_id, period, value, censor_before_study_end)) %>%
    rename(person_time = value)

ortho_routine_final_2 <- merge(subset(ortho_routine_final, measure != "Person time"), person_time, 
                     by = c("patient_id", "period", "censor_before_study_end"), all.x = TRUE) 


######### Medicine prescribing frequencies - overall and stratified ############

# Number of people with any prescription and 3+ prescriptions
# during each time period
cat_dist_meds <- function() {
  cat_dist <- function(var, name) {
    
    dat %>% 
      mutate(full = "Full cohort", 
             prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
      subset(period %in% c("Pre-WL","Post WL")) %>%
      group_by({{var}}, measure, period) %>%
      summarise(count_any = rounding(sum(med_any)),
                count_3plus = rounding(sum(med_3plus)),
                total = rounding(n()),
                total_post = rounding(sum(censor_before_study_end == FALSE))) %>%
      ungroup() %>%
      mutate(cohort = "Orthopaedic - Routine/Admitted") %>%
      rename(category = {{var}}) %>%
      mutate(category = as.character(category),
             variable = name,
             total = ifelse(period == "Pre-WL", total, total_post))
    
  }
  
  rbind(
    cat_dist(full, "Full cohort"),
    cat_dist(age_group, "Age"),
    cat_dist(imd10, "IMD"),
    cat_dist(ethnicity6, "Ethnicity"),
    cat_dist(region, "Region"),
    cat_dist(sex, "Sex"),
    cat_dist(prior_opioid_rx, "Prior opioid Rx"),
    cat_dist(wait_gp, "Time on waiting list")
  ) 
  
}


dat <- ortho_routine_final_2

meds <- cat_dist_meds() 

meds <- meds[,c("cohort", "variable", "category", "period", "measure", "count_any", "count_3plus", "total") ]

write.csv(meds, here::here("output", "clockstops",  "meds_dist_ortho.csv"),
           row.names = FALSE)


################## Medicine Rx count variables - overall and stratified ####################

# Count total number of Rx, total person-days
#   for each period and each medicine group
summ <- function(gp, var){

    ortho_routine_final_2 %>%
      mutate(full = "Full cohort",
             prior_opioid_gp = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
      subset(!(measure %in% c("Gabapentinoid", "NSAID", "Antidepressant", "TCA"))) %>%
      group_by(period, measure, {{gp}}) %>%
      summarise(person_days = rounding(sum(person_time)),
                count_rx = rounding(sum(value))) %>%
      rename(category = {{gp}}) %>%
      mutate(variable = var,  cohort = "Orthopaedic - Routine/Admitted")

}

meds_ptime <- rbind(
    summ(full, "Full cohort"),
    summ(prior_opioid_gp, "Prior opioid Rx"),
    summ(wait_gp, "Time on waiting list")) %>%
  arrange(cohort, variable, category, period, measure)

meds_ptime <- meds_ptime[,c("cohort", "category","period","person_days",
                                          "measure", "count_rx")]



## Combine into one file
all_meds <- merge(meds, meds_ptime, by = c("period", "measure", "category", "cohort"))

write.csv(all_meds, here::here("output", "clockstops", "med_by_period_ortho.csv"),
          row.names = FALSE)

