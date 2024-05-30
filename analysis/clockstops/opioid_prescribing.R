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
  
  dplyr::select(c(patient_id, starts_with(c("opioid_", "short_opioid", "long_opioid", "moderate_opioid",
                                            "weak_opioid", "strong_opioid")), 
                  age_group, sex, imd10, ethnicity6, region, prior_opioid_rx, prior_opioid_rx2, wait_gp,
                  censor_before_study_end, num_weeks, oa, hip_hrg, knee_hrg)) %>%
  
  dplyr::select(!c(ends_with(c("_any1","_any2","_any")))) %>%
  
  # Transpose to long
  reshape2::melt(id = c("patient_id","age_group","sex","imd10","ethnicity6", "region",
                        "prior_opioid_rx","wait_gp","censor_before_study_end",
                        "num_weeks", "oa", "hip_hrg", "knee_hrg")) %>%
  
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
                      grepl("strong_opioid1", variable) ~ "Strong opioid",
                      grepl("strong_opioid2", variable) ~ "Strong opioid 2",
                      grepl("moderate_opioid", variable) ~ "Moderate opioid",
                      TRUE ~ "Any opioid"),
         time = case_when(
                      grepl("_count1", variable) ~ "180 days",
                      grepl("_count2", variable) ~ "90 days"),
         
         # Variables for any prescribing, and >=3 prescriptions
         med_any = ifelse(value >= 1, 1, 0),
         med_6mos_3plus = ifelse(value >= 3 & time == "180 days", 1, 0),
         med_3mos_3plus = ifelse(value >= 3 & time == "90 days", 1, 0)) 

# Split out person-time and merge it back in
# person_time <- ortho_routine_final %>%
#     subset(measure == "Person time") %>%
#     dplyr::select(c(patient_id, period, value, censor_before_study_end, num_weeks, oa, hip_hrg, knee_hrg)) %>%
#     rename(person_time = value)

ortho_routine_final_2 <- ortho_routine_final %>%
  subset(measure != "Person time")

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
  
meds <- meds[,c("cohort", "variable", "category", "period", "measure", "count_any", "count_6mos_3plus", 
                "count_3mos_3plus", "total") ]

write.csv(meds, here::here("output", "clockstops", "med_by_period.csv"), row.names = FALSE)


########### Medicine Rx counts and person time - overall and stratified ########
# 
# meds_ptime <- rbind(
#     ptime(full, "Full cohort"),
#     ptime(age_group, "Age"),
#     ptime(imd10, "IMD"),
#     ptime(region, "Region"),
#     ptime(ethnicity6, "Ethnicity"),
#     ptime(sex, "Sex"),
#     ptime(prior_opioid_rx, "Prior opioid Rx"),
#     ptime(wait_gp, "Time on waiting list"),
#     ptime(oa, "OA diagnosis"),
#     ptime(hip_hrg, "Hip HRG"),
#     ptime(knee_hrg, "Knee HRG")
#   ) %>%
#   arrange(cohort, variable, category, period, measure) %>%
#   subset(!(variable == "Region" & is.na(category))) %>%
#   subset(!(variable == "IMD" & category == "Unknown"))
# 
# meds_ptime <- meds_ptime[,c("cohort","period","measure","variable","category","person_days","count_rx")]


## Combine into one file
# all_meds <- merge(meds, meds_ptime, by = c( "cohort", "period", "variable","category", "measure")) %>%
#   arrange(cohort, variable, category, period, measure) 




#########################################################################
############ By prior opioid prescribing and wait time ##################
#########################################################################

####### Medicine prescribing frequencies - by prior opioid Rx, wait time, OA diagnosis, hip/knee HRG ####

prior_wait_1 <- ortho_routine_final_2 %>% 
  mutate(prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
  group_by(wait_gp, prior_opioid_rx, measure, period) %>%
  summarise(count_any = rounding(sum(med_any)),
            count_6mos_3plus = rounding(sum(med_6mos_3plus)),
            count_3mos_3plus = rounding(sum(med_3mos_3plus)),
            total = rounding(n()),
            total_post = rounding(sum(censor_before_study_end == FALSE))) %>%
  ungroup() %>%
  mutate(cohort = "Orthopaedic - Routine/Admitted",
         total = ifelse(period == "Pre-WL", total, total_post),
         oa = "Total",
         hip_hrg = "Total",
         knee_hrg = "Total")

prior_wait_2  <- ortho_routine_final_2 %>% 
  mutate(prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
  group_by(wait_gp, prior_opioid_rx, measure, period, oa) %>%
  summarise(count_any = rounding(sum(med_any)),
            count_6mos_3plus = rounding(sum(med_6mos_3plus)),
            count_3mos_3plus = rounding(sum(med_3mos_3plus)),
            total = rounding(n()),
            total_post = rounding(sum(censor_before_study_end == FALSE))) %>%
  ungroup() %>%
  mutate(cohort = "Orthopaedic - Routine/Admitted",
         total = ifelse(period == "Pre-WL", total, total_post),
         hip_hrg = "Total",
         knee_hrg = "Total")


prior_wait_3  <- ortho_routine_final_2 %>% 
  mutate(prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
  group_by(wait_gp, prior_opioid_rx, measure, period, hip_hrg) %>%
  summarise(count_any = rounding(sum(med_any)),
            count_6mos_3plus = rounding(sum(med_6mos_3plus)),
            count_3mos_3plus = rounding(sum(med_3mos_3plus)),
            total = rounding(n()),
            total_post = rounding(sum(censor_before_study_end == FALSE))) %>%
  ungroup() %>%
  mutate(cohort = "Orthopaedic - Routine/Admitted",
         total = ifelse(period == "Pre-WL", total, total_post),
         oa = "Total",
         knee_hrg = "Total")


prior_wait_4  <- ortho_routine_final_2 %>% 
  mutate(prior_opioid_rx = ifelse(prior_opioid_rx == TRUE, "Yes", "No")) %>%
  group_by(wait_gp, prior_opioid_rx, measure, period, knee_hrg) %>%
  summarise(count_any = rounding(sum(med_any)),
            count_6mos_3plus = rounding(sum(med_6mos_3plus)),
            count_3mos_3plus = rounding(sum(med_3mos_3plus)),
            total = rounding(n()),
            total_post = rounding(sum(censor_before_study_end == FALSE))) %>%
  ungroup() %>%
  mutate(cohort = "Orthopaedic - Routine/Admitted",
         total = ifelse(period == "Pre-WL", total, total_post),
         oa = "Total",
         hip_hrg = "Total")


prior_wait_all <- rbind(prior_wait_1, prior_wait_2, prior_wait_3, prior_wait_4)
prior_wait_all <- prior_wait_all[,c("cohort", "prior_opioid_rx", "oa", "hip_hrg", "knee_hrg",
                            "wait_gp", "period", "measure", "count_any", "count_6mos_3plus", 
                            "count_3mos_3plus", "total") ]

write.csv(prior_wait_all, here::here("output", "clockstops", "med_by_period_wait.csv"),
          row.names = FALSE)



# ##############
# 
# 
# week_tot_rx <- ortho_routine_final %>%
#   subset(period == "During WL" & num_weeks <= 52 & measure!= "Person time" & num_weeks > 0) %>%
#   group_by(num_weeks, measure, prior_opioid_rx) %>%
#   summarise(opioid_rx = rounding(sum(value)),
#          denominator = rounding(n())) %>%
#   rename(weeks_on_wait_list = num_weeks) %>%
#   ungroup() %>% 
#   arrange(prior_opioid_rx, weeks_on_wait_list, measure, opioid_rx, denominator) %>%
#   subset(prior_opioid_rx == TRUE | (prior_opioid_rx == FALSE & measure == "Any opioid"))
# 
# week_tot_rx <- week_tot_rx[,c("prior_opioid_rx", "measure", "weeks_on_wait_list", "opioid_rx", "denominator")]
# 
# write.csv(week_tot_rx, here::here("output", "clockstops", "total_rx_wait.csv"),
#           row.names = FALSE)
