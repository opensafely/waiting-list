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
full <- arrow::read_feather(here::here("output", "data", "dataset_clockstops.arrow")) %>%
                
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
                    prior_opioid_rx = (opioid_pre_count >=3),
                    
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
                    
                    # Week of study period
                    week = ceiling(wait_time / 7),
                    
                    # Week variable capped at one year (for some analyses)
                    week52 =  ifelse(week > 52, 52, week),
                    
                    # Waiting time category
                    wait_gp = ifelse(week <= 18, "<=18 weeks", 
                                     ifelse(week > 18 & week <= 52, "19-52 weeks", 
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
                    
                    not_ortho = (is.na(ortho_surgery) | ortho_surgery == FALSE)
                )
                    
                        
exclusions_1 <- full %>% 
  mutate(total = n()) %>%
  group_by(total) %>%
  summarise(age_not_18_110 = sum(age_not_18_110),
            sex_missing = sum(sex_missing),
            sex_not_m_f = sum(sex_not_m_f)) %>%
  ungroup() %>%
  mutate(age_not_18_110 = rounding(age_not_18_110),
         sex_missing = rounding(sex_missing),
         sex_not_m_f = rounding(sex_not_m_f),
         total = rounding(total),
         cohort = "1. Full WL population") %>%
  reshape2::melt(id = c("cohort", "total")) 

exclusions_2 <- full %>%
  subset(age_not_18_110 == FALSE & sex_missing == FALSE & sex_not_m_f == FALSE) %>%
  mutate(total = n()) %>%
  group_by(total) %>%
  summarise(not_ortho = sum(not_ortho)) %>%
  ungroup() %>%
  mutate(not_ortho = rounding(not_ortho),
         total = rounding(total),
         cohort = "2. Full WL pop with no missing age/sex") %>%
  reshape2::melt(id = c("total", "cohort"))

# Save as final
full_final <- full %>%
  subset(age_not_18_110 == FALSE & sex_missing == FALSE & sex_not_m_f == FALSE)

write.csv(full_final, file = here::here("output", "data", "cohort_full_clockstops.csv.gz"),
          row.names = FALSE)


####################################################

# Restrict to people with trauma/orthopaedic surgery
ortho <- full_final %>%
  subset(ortho_surgery == TRUE) %>%
  mutate(not_routine_admitted =  (routine != "Routine" | admitted == FALSE)) %>%
  dplyr::select(!c(age_not_18_110, sex_missing, sex_not_m_f))

# Number of people excluded due to cancer 
exclusions_3 <- ortho %>%
  mutate(total = n()) %>%
  group_by(total) %>%
  summarise(not_routine_admitted = sum(not_routine_admitted)) %>%
  ungroup() %>%
  mutate(total = rounding(total),
         not_routine_admitted = rounding(not_routine_admitted),
         cohort = "3. All orthopaedic") %>%
  reshape2::melt(id = c("total", "cohort"))
  
exclusions_4 <- ortho %>%
  subset(not_routine_admitted == FALSE) %>%
  mutate(total = n()) %>%
  group_by(total) %>%
  summarise(cancer = sum(cancer),
            died_during_wl = sum(died_during_wl)) %>%
  ungroup() %>%
  mutate(total = rounding(total),
         cancer = rounding(cancer),
         died_during_wl = rounding(died_during_wl),
         cohort = "4. Orthopaedic routine/admitted only"
  ) %>%
  reshape2::melt(id = c("total", "cohort"))

all_exclusions <- rbind(exclusions_1, exclusions_2, exclusions_3, exclusions_4) %>%
  rename(reason = variable, count = value)

all_exclusions <- all_exclusions[,c("cohort", "total", "reason", "count")]

write.csv(all_exclusions, here::here("output", "clockstops", "exclude_ortho.csv"),
          row.names = FALSE)



######## FINAL ORTHOPAEDIC COHORT #########

ortho_final <- ortho

## Save as final
write.csv(ortho_final, file = here::here("output", "data", "cohort_ortho_clockstops.csv.gz"),
          row.names = FALSE)
