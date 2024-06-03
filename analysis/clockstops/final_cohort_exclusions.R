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
                mutate(end_before_start = ifelse((rtt_start_date > rtt_end_date) |
                                                  is.na(rtt_start_date), TRUE, FALSE))


# Number of people with start date before end date/missing        
exclusions_1 <- full %>% 
  mutate(count = rounding(n()), 
         step = "Step 1", 
         exclusion = "Full cohort") %>%
  distinct(count, step, exclusion)

# Final full cohort
exclusions_2 <- full %>% 
  subset(end_before_start == FALSE) %>%
  mutate(count = rounding(n()),
         step = "Step 2",
         exclusion = "End date before start date")%>%
  distinct(count, step, exclusion)

# Save as final
full_final <- full %>%
  subset(end_before_start == FALSE)


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
    
    # Died while on WL
    died_during_wl = (!is.na(dod) & dod < rtt_end_date),
    died_during_post = (!is.na(dod) & dod <= (rtt_end_date + 182)),
    
    # Long-term use
    no_opioid = (opioid_pre_count1 == 0),
    prior_opioid_rx = (opioid_pre_count1 >= 3),
    long_term_opioid = (opioid_pre_count2 >= 3),
    
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
  mutate(count = rounding(n()),
         step = "Step 3",
         exclusion = "Not orthopaedic") %>%
  distinct(count, step, exclusion)

# Number of people excluded due to non-M/F sex
exclusions_4 <- ortho %>% 
  subset(num_weeks <= 130) %>% 
  mutate(count = rounding(n()),
         step = "Step 4",
         exclusion = "Wait <130 weeks") %>%
  distinct(count, step, exclusion)

# Number of people excluded due to outside age range
exclusions_5 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE) %>% 
  mutate(count = rounding(n()),
         step = "Step 5",
         exclusion = "Sex not male/female or missing") %>%
  distinct(count, step, exclusion)

# Number of people excluded due to missing values 
exclusions_6 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE) %>% 
  mutate(count = rounding(n()),
         step = "Step 6",
         exclusion = "Age not 18-110 years") %>%
  distinct(count, step, exclusion)

# Number of people excluded due to not being routine/admitted
exclusions_7 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE & 
           missing_priority == FALSE & missing_admission == FALSE) %>% 
  mutate(count = rounding(n()),
         step = "Step 7",
         exclusion = "Missing priority/admission type") %>%
  distinct(count, step, exclusion)

# Number of people excluded due to dying before end of waiting list
exclusions_8 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE & 
           routine == "Routine" & admitted == TRUE) %>% 
  mutate(count = rounding(n()),
         step = "Step 8",
         exclusion = "Not routine/admitted") %>%
  distinct(count, step, exclusion)

# Number of people in final cohort
exclusions_9 <- ortho %>%
  subset(num_weeks <= 130 & 
           sex_missing == FALSE & sex_not_m_f == FALSE & 
           age_not_18_110 == FALSE & 
           routine == "Routine" & admitted == TRUE &
           cancer == FALSE) %>% 
  mutate(count = rounding(n()),
         step = "Step 9",
         exclusion = "History of cancer") %>%
  distinct(count, step, exclusion)

all_exclusions <- rbind(exclusions_1, exclusions_2, exclusions_3, exclusions_4,
                        exclusions_5, exclusions_6, exclusions_7, exclusions_8,
                        exclusions_9)

all_exclusions <- all_exclusions[,c("step", "exclusion", "count")]

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
