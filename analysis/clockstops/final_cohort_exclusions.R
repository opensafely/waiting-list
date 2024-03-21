###############################################################
# This script creates final cohorts for analysis 
# for: 1. all people with a closed RTT pathway (May21-Apr22)
# and 2. all people with a closed RTT pathway for trauma/orthopaedic surgery
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
library('arrow')

## Rounding function
source(here("analysis", "custom_functions.R"))

## Create directories if needed
dir_create(here::here("output", "clockstops"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)


## Load data ##
full <- arrow::read_feather(here::here("output", "data", "dataset_full.arrow")) %>%
                # Create new variables
                mutate(start_before_end = ifelse((rtt_start_date > rtt_end_date) |
                                                  is.na(rtt_start_date), TRUE, FALSE))


# Number of people with start date before end date/missing        
exclusions_1 <- full %>% 
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(start_before_end = rounding(sum(start_before_end))) %>%
  ungroup() %>%
  mutate(cohort = "Step 1") %>%
  reshape2::melt(id = c("cohort", "total")) 

# Final full cohort
exclusions_2 <- full %>% 
  subset(start_before_end == FALSE) %>%
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(final = max(total)) %>%
  ungroup() %>%
  mutate(cohort = "Step 2") %>%
  reshape2::melt(id = c("cohort", "total")) 

# Save as final
full_final <- full %>%
  subset(start_before_end == FALSE)


write.csv(full_final, file = here::here("output", "data", "cohort_full_clockstops.csv.gz"),
          row.names = FALSE)


####################################################

# Restrict to people with trauma/orthopaedic surgery

ortho <- arrow::read_feather(here::here("output", "data", "dataset_ortho.arrow")) %>%
  # Create new variables
  mutate(
    # Month of WL start/end
    rtt_start_month = floor_date(rtt_start_date, "month"),
    rtt_end_month = floor_date(rtt_end_date, "month"),
    
    # Were on multiple WL during study period
    rtt_multiple = (count_rtt_start_date > 1),
    
    routine = ifelse(priority_type %in% c("urgent", "two week wait"), "Urgent", 
                     ifelse(priority_type %in% c("routine"), "Routine", 
                            "Missing")),
    
    priority_type = ifelse(is.na(priority_type), "Missing", priority_type),
    admitted = (waiting_list_type %in% c("IRTT")),
      
    missing_priority = (priority_type == "Missing" | is.na(priority_type)),
    missing_admission = (!(waiting_list_type %in% c("IRTT","ORTT")) | is.na(waiting_list_type)),
    
    prior_opioid_rx = (opioid_pre_count >= 3),
    
    # Died while on WL
    died_during_wl = (!is.na(dod) & dod < rtt_end_date),
    died_during_post = (!is.na(dod) & dod <= (rtt_end_date + 182)),
    
    # Time on WL, censored at death/deregistration
    wait_time_adj = as.numeric(
      pmin(rtt_end_date, end_date, na.rm = FALSE) - rtt_start_date + 1),
    
    # Time post-WL, censored at death/deregistration (max 182 days)
    #    If study end date before RTT end date, set to zero
    post_time_adj = ifelse(
      end_date >= rtt_end_date,
      as.numeric(pmin((rtt_end_date + 182), end_date, na.rm = FALSE) - rtt_end_date + 1),
      0),
    
    # Time pre-WL (182 days for everyone)
    pre_time = 182,
    
    # Any HRG
    any_nontrauma_hrg = (knee_hrg | hip_hrg | shoulder_hrg | elbow_hrg | foot_hrg | hand_hrg | complex_hrg),

    # Week variable capped at one year (for some analyses)
    week52 =  ifelse(num_weeks > 52, 52, num_weeks),
    
    # Waiting time category
    wait_gp = ifelse(num_weeks <= 18, "<=18 weeks", 
                     ifelse(num_weeks > 18 & num_weeks <= 52, "19-52 weeks", 
                            "52+ weeks")),
    
    covid_timing = ifelse(rtt_start_date < as.Date("2020-03-01"), "Pre-COVID",
                          ifelse(rtt_start_date >= as.Date("2020-03-01") & 
                                   rtt_start_date < as.Date("2021-04-01"), 
                                 "Restriction period",
                                 "Recovery period")),
    
    age_not_18_110 = ifelse((age<18 | age >=110), TRUE, FALSE),
    sex_missing = ifelse(sex == "unknown", TRUE, FALSE),
    sex_not_m_f = ifelse(!is.na(sex) & !(sex %in% c("unknown", "male", "female")),
                         TRUE, FALSE),
    
    not_routine_admitted = (!(routine == "Routine" & admitted == TRUE))
  )


# Number of people with >2.5 years wait (99.9%)
exclusions_3 <- ortho %>% 
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(more_130weeks = rounding(sum(num_weeks > 130))) %>%
  ungroup() %>%
  mutate(cohort = "Step 3") %>%
  reshape2::melt(id = c("cohort", "total")) 

# Number of people excluded due to non-M/F sex
exclusions_4 <- ortho %>% 
  subset(num_weeks <= 130) %>%
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(sex_not_m_f = rounding(sum(sex_missing | sex_not_m_f))) %>%
  ungroup() %>%
  mutate(cohort = "Step 4") %>%
  reshape2::melt(id = c("cohort", "total")) 

# Number of people excluded due to outside age range
exclusions_5 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE) %>%
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(age_not_18_110 = rounding(sum(age_not_18_110))) %>%
  ungroup() %>%
  mutate(cohort = "Step 5" ) %>%
  reshape2::melt(id = c("total", "cohort"))

# Number of people excluded due to missing values 
exclusions_6 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE) %>%
    mutate(total = rounding(n())) %>%
    group_by(total) %>%
    summarise(missing_priority = rounding(sum(missing_priority)),
              missing_admission = rounding(sum(missing_admission))) %>%
    ungroup() %>%
    mutate(cohort = "Step 6") %>%
    reshape2::melt(id = c("total", "cohort"))

# Number of people excluded due to not being routine/admitted
exclusions_7 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE & 
           missing_priority == FALSE & missing_admission == FALSE) %>%
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(not_routine_admitted = rounding(sum(not_routine_admitted))) %>%
  ungroup() %>%
  mutate(cohort = "Step 7") %>%
  reshape2::melt(id = c("total", "cohort"))

# Number of people excluded due to dying before end of waiting list
exclusions_8 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE & 
           routine == "Routine" & admitted == TRUE) %>%
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(died_before_wl_end = rounding(sum(died_during_wl))) %>%
  ungroup() %>%
  mutate(cohort = "Step 8") %>%
  reshape2::melt(id = c("total", "cohort"))
  
# Number of people excluded due to having history of cancer
exclusions_9 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE & 
           routine == "Routine" & admitted == TRUE &
           died_during_wl == FALSE) %>%
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(cancer = rounding(sum(cancer))) %>%
  ungroup() %>%
  mutate(cohort = "Step 9") %>%
  reshape2::melt(id = c("total", "cohort"))

# Number of people in final cohort
exclusions_10 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE & 
           routine == "Routine" & admitted == TRUE &
           died_during_wl == FALSE & 
           cancer == FALSE) %>%
  mutate(total = rounding(n())) %>%
  group_by(total) %>%
  summarise(final = max(total)) %>%
  ungroup() %>%
  mutate(cohort = "Step 10") %>%
  reshape2::melt(id = c("total", "cohort"))

all_exclusions <- rbind(exclusions_1, exclusions_2, exclusions_3, exclusions_4,
                        exclusions_5, exclusions_6, exclusions_7, exclusions_8,
                        exclusions_9, exclusions_10) %>%
  rename(reason = variable, count = value)

all_exclusions <- all_exclusions[,c("cohort", "total", "reason", "count")]

write.csv(all_exclusions, here::here("output", "clockstops", "exclude_ortho.csv"),
          row.names = FALSE)



######## FINAL ORTHOPAEDIC COHORT #########

ortho_final <- ortho %>%
  subset(cancer == FALSE & died_during_wl == FALSE & age_not_18_110 == FALSE &
           sex_missing == FALSE & sex_not_m_f == FALSE & num_weeks <= 130)

## Save as final
write.csv(ortho_final, file = here::here("output", "data", "cohort_ortho_clockstops.csv.gz"),
          row.names = FALSE)


ortho_routine_final <- ortho %>%
  subset(cancer == FALSE & died_during_wl == FALSE & routine == "Routine" & admitted == TRUE
         & age_not_18_110 == FALSE & sex_missing == FALSE & sex_not_m_f == FALSE & num_weeks <= 130)

## Save as final
write.csv(ortho_routine_final, file = here::here("output", "data", "cohort_ortho_routine_clockstops.csv.gz"),
          row.names = FALSE)
