###############################################################
# This script creates final cohorts for analysis 
# for: 1. all people with a closed RTT pathway (May21-May22)
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

## Rounding function
source(here("analysis", "custom_functions.R"))

## Create directories if needed
dir_create(here::here("output", "clockstops"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)


## Load data ##
full <- read_csv(here::here("output", "data", "dataset_clockstops.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    rtt_end_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"),
                    first_opioid_date = col_date(format="%Y-%m-%d"))) %>%
              
                # Create new variables
                mutate(
                    # Month of WL start/end
                    rtt_start_month = floor_date(rtt_start_date, "month"),
                    rtt_end_month = floor_date(rtt_end_date, "month"),
                       
                    # Were on multiple WL during study period
                    rtt_multiple = (count_rtt_start_date > 1),
                    
                    routine = if_else(priority_type %in% c("urgent", "two week wait"), "Urgent", "Routine", "Missing"),
                    
                    # Admitted
                    admitted = (waiting_list_type %in% c("IRTT","PTLI","RTTI")),

                    # Orthopaedic surgery
                    ortho_surgery = (treatment_function %in% c("110", "111")),
                    
                    # Died while on WL
                    died_during_wl = (!is.na(dod) & dod <= rtt_end_date),
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
                    
                    # Week category
                    week_gp = ifelse(week <= 18, "<=18 weeks", 
                                     ifelse(week > 18 & week <= 52, "19-52 weeks", 
                                            "52+ weeks")),
                    
                    age_missing = (is.na(age)),
                    age_not_18_110 = (!is.na(age) & age<18 & age >=110),
                    sex_missing = is.na(sex),
                    sex_not_m_f = (!is.na(sex) & sex == "intersex")
                )
                    
                        
full_exclusions <- full %>% 
  mutate(total = n()) %>%
  group_by(total) %>%
  summarise(age_missing = sum(age_missing),
            age_not_18_110 = sum(age_not_18_110),
            sex_missing = sum(sex_missing),
            sex_not_m_f = sum(sex_not_m_f)) %>%
  ungroup() %>%
  mutate(age_missing = rounding(age_missing),
         age_not_18_110 = rounding(age_not_18_110),
         sex_missing = rounding(sex_missing),
         sex_not_m_f = rounding(sex_not_m_f))

write.csv(full_exclusions, file = here::here("output", "clockstops", "cohort_full_exclusions.csv"),
          row.names = FALSE)


full_final <- full %>%
  subset(!is.na(age) & age >= 18 & age < 110 
         & !is.na(sex) & (sex != "intersex"))

## Save as final
write.csv(full_final, file = here::here("output", "data", "cohort_full_clockstops.csv.gz"),
          row.names = FALSE)


####################################################

# Restrict to people with trauma/orthopaedic surgery
ortho <- full_final %>%
  subset(ortho_surgery == TRUE) %>%
  dplyr::select(!c(age_missing, age_not_18_110, sex_missing, sex_not_m_f))


# Number of people excluded due to cancer 
exclude <- ortho %>%
  mutate(total = n()) %>%
  group_by(cancer, total) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(var = "Cancer",
         count = rounding(count),
         total = rounding(total),
         source = "clockstops", 
         cohort = "ortho"
  ) %>%
  rename(category = cancer)

exclude <- exclude[,c("source", "cohort", "var","category","count","total")]

write.csv(exclude, here::here("output", "clockstops", "exclude_ortho.csv"),
          row.names = FALSE)


######## FINAL ORTHOPAEDIC COHORT #########

ortho_final <- ortho %>%
  subset(cancer == FALSE)


## Save as final
write.csv(ortho_final, file = here::here("output", "data", "cohort_ortho_clockstops.csv.gz"),
          row.names = FALSE)
