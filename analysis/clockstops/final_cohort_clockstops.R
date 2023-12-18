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
                    end_date = col_date(format="%Y-%m-%d"))) %>%
              
                # Create new variables
                mutate(
                    # Month of WL start/end
                    rtt_start_month = floor_date(rtt_start_date, "month"),
                    rtt_end_month = floor_date(rtt_end_date, "month"),
                       
                    # Were on multiple WL during study period
                    rtt_multiple = (count_rtt_start_date > 1),
                    
                    routine = case_when(
                      priority_type %in% c("urgent", "two week wait") ~ "Urgent",
                      priority_type == "routine" ~ "Routine",
                      .default = "Missing"
                    ),
                    
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
                    
                    any_opioid_pre = (opioid_pre_count > 0),
                    any_opioid_wait = (opioid_wait_count > 0),
                    any_opioid_post = (opioid_post_count > 0),
                    
                    hi_opioid_pre = (hi_opioid_pre_count > 0),
                    hi_opioid_wait = (hi_opioid_wait_count > 0),
                    hi_opioid_post = (hi_opioid_post_count > 0),
                    
                    long_opioid_pre = (long_opioid_pre_count > 0),
                    long_opioid_wait = (long_opioid_wait_count > 0),
                    long_opioid_post = (long_opioid_post_count > 0),
                    
                    short_opioid_pre = (short_opioid_pre_count > 0),
                    short_opioid_wait = (short_opioid_wait_count > 0),
                    short_opioid_post = (short_opioid_post_count > 0),
                    
                    weak_opioid_pre = (weak_opioid_pre_count > 0),
                    weak_opioid_wait = (weak_opioid_wait_count > 0),
                    weak_opioid_post = (weak_opioid_post_count > 0),
                    
                    strong_opioid_pre = (strong_opioid_pre_count > 0),
                    strong_opioid_wait = (strong_opioid_wait_count > 0),
                    strong_opioid_post = (strong_opioid_post_count > 0),
                    
                    codeine_pre = (codeine_pre_count > 0),
                    codeine_wait = (codeine_wait_count > 0),
                    codeine_post = (codeine_post_count > 0),
                    
                    tramadol_pre = (tramadol_pre_count > 0),
                    tramadol_wait = (tramadol_wait_count > 0),
                    tramadol_post = (tramadol_post_count > 0),
                    
                    oxycodone_pre = (oxycodone_pre_count > 0),
                    oxycodone_wait = (oxycodone_wait_count > 0),
                    oxycodone_post = (oxycodone_post_count > 0),
                    
                    gabapentinoid_pre = (gabapentinoid_pre_count > 0),
                    gabapentinoid_wait = (gabapentinoid_wait_count > 0),
                    gabapentinoid_post = (gabapentinoid_post_count > 0),
                    
                    nsaid_pre = (nsaid_pre_count > 0),
                    nsaid_wait = (nsaid_wait_count > 0),
                    nsaid_post = (nsaid_post_count > 0),
                    
                    antidepressant_pre = (antidepressant_pre_count > 0),
                    antidepressant_wait = (antidepressant_wait_count > 0),
                    antidepressant_post = (antidepressant_post_count > 0),
                    
                    week = ceiling(wait_time / 7),
                    week52 =  ifelse(week > 52, 52, week),
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
  subset(ortho_surgery == TRUE)

## Save as final
write.csv(ortho, file = here::here("output", "data", "cohort_ortho_clockstops.csv.gz"),
          row.names = FALSE)
