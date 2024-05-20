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
  
  dplyr::select(c(patient_id, starts_with(c("opioid_", "short_opioid", "long_opioid",
                                            "weak_opioid", "strong_opioid")), post_time_adj, 
                  wait_time_adj, pre_time, age_group, sex, imd10, 
                  ethnicity6, region, prior_opioid_rx, wait_gp,
                  censor_before_study_end, num_weeks)) %>%
  
  dplyr::select(!c(ends_with(c("_any")))) %>%
  
  # Transpose to long
  reshape2::melt(id = c("patient_id","age_group","sex","imd10","ethnicity6", "region",
                        "prior_opioid_rx", "wait_gp", "censor_before_study_end",
                        "num_weeks")) %>%
  
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
                      TRUE ~ "Any opioid"),
         
         # Variables for any prescribing, and >=3 prescriptions
         med_any = ifelse(value >= 1, 1, 0),
         med_3plus = ifelse(value >= 3, 1, 0)) 

# Split out person-time and merge it back in
person_time <- ortho_routine_final %>%
    subset(measure == "Person time") %>%
    dplyr::select(c(patient_id, period, value, censor_before_study_end, num_weeks)) %>%
    rename(person_time = value)

ortho_routine_final_2 <- merge(subset(ortho_routine_final, measure != "Person time"), person_time, 
                     by = c("patient_id", "period",  "censor_before_study_end"), all.x = TRUE) 


######### Medicine prescribing frequencies - overall and stratified ############

dat <- ortho_routine_final_2

meds <- rbind(
    meds_dist(full, "Full cohort"),
    meds_dist(age_group, "Age"),
    meds_dist(imd10, "IMD"),
    meds_dist(region, "Region"),
    meds_dist(ethnicity6, "Ethnicity"),
    meds_dist(sex, "Sex"),
    meds_dist(prior_opioid_rx, "Prior opioid Rx"),
    meds_dist(wait_gp, "Time on waiting list"),
    meds_dist(oa, "OA diagnosis"),
    meds_dist(hip_hrg, "Hip HRG"),
    meds_dist(knee_hrg, "Knee HRG")
    ) 
  
meds <- meds[,c("cohort", "variable", "category", "period", "measure", "count_any", "count_3plus", "total") ]


########### Medicine Rx counts and person time - overall and stratified ########

meds_ptime <- rbind(
    ptime(full, "Full cohort"),
    ptime(age_group, "Age"),
    ptime(imd10, "IMD"),
    ptime(region, "Region"),
    ptime(ethnicity6, "Ethnicity"),
    ptime(sex, "Sex"),
    ptime(prior_opioid_rx, "Prior opioid Rx"),
    ptime(wait_gp, "Time on waiting list"),
    ptime(oa, "OA diagnosis"),
    ptime(hip_hrg, "Hip HRG"),
    ptime(knee_hrg, "Knee HRG")) %>%
  arrange(cohort, variable, category, period, measure) %>%
  subset(!(variable == "Region" & is.na(category))) %>%
  subset(!(variable == "IMD" & category == "Unknown"))

meds_ptime <- meds_ptime[,c("cohort","period","measure","variable","category","person_days",
                                           "count_rx")]


## Combine into one file
all_meds <- merge(meds, meds_ptime, by = c( "cohort", "period", "variable","category", "measure")) %>%
  arrange(cohort, variable, category, period, measure) 

write.csv(all_meds, here::here("output", "clockstops", "med_by_period.csv"),
          row.names = FALSE)



#########################################################################
############ By prior opioid prescribing and wait time ##################
#########################################################################

####### Medicine prescribing frequencies - by prior opioid Rx and wait time ####

prior_wait <- ortho_routine_final_2 %>% 
  mutate(prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
  group_by(wait_gp, prior_opioid_rx, measure, period) %>%
  summarise(count_any = rounding(sum(med_any)),
            count_3plus = rounding(sum(med_3plus)),
            total = rounding(n()),
            total_post = rounding(sum(censor_before_study_end == FALSE))) %>%
  ungroup() %>%
  mutate(cohort = "Orthopaedic - Routine/Admitted") %>%
  mutate(total = ifelse(period == "Pre-WL", total, total_post))

prior_wait <- prior_wait[,c("cohort", "prior_opioid_rx", "wait_gp", "period", "measure", "count_any", "count_3plus", "total") ]


#########  Medicine Rx counts and person time - by prior opioid Rx and wait time ####################

# Count total number of Rx, total person-days
#   for each period and each medicine group

prior_wait_ptime <- ortho_routine_final_2 %>%
    mutate(prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
    group_by(period, measure, wait_gp, prior_opioid_rx) %>%
    summarise(person_days = rounding(sum(person_time)),
              count_rx = rounding(sum(value))) %>%
    mutate( cohort = "Orthopaedic - Routine/Admitted")
  
prior_wait_ptime <- prior_wait_ptime[,c("cohort","period","measure","prior_opioid_rx","wait_gp","person_days",
                            "count_rx")]


## Combine into one file
all_prior_wait <- merge(prior_wait, prior_wait_ptime, by = c( "cohort", "period", "prior_opioid_rx", "wait_gp", "measure")) %>%
  arrange(cohort, prior_opioid_rx, wait_gp, period, measure) 

write.csv(all_prior_wait, here::here("output", "clockstops", "med_by_period_wait.csv"),
          row.names = FALSE)



##############


week_tot_rx <- ortho_routine_final %>%
  subset(period == "During WL" & num_weeks <= 52 & measure!= "Person time" & num_weeks > 0) %>%
  group_by(num_weeks, measure, prior_opioid_rx) %>%
  summarise(opioid_rx = rounding(sum(value)),
         denominator = rounding(n())) %>%
  rename(weeks_on_wait_list = num_weeks) %>%
  ungroup() %>% 
  arrange(prior_opioid_rx, weeks_on_wait_list, measure, opioid_rx, denominator) %>%
  subset(prior_opioid_rx == TRUE | (prior_opioid_rx == FALSE & measure == "Any opioid"))

week_tot_rx <- week_tot_rx[,c("prior_opioid_rx", "measure", "weeks_on_wait_list", "opioid_rx", "denominator")]

write.csv(week_tot_rx, here::here("output", "clockstops", "total_rx_wait.csv"),
          row.names = FALSE)
