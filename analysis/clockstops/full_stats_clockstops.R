###############################################################
# This script creates summary statistics for all key variables
# for all people with a closed RTT pathway (May21-May22)
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
dat <- read_csv(here::here("output", "data", "cohort_full_clockstops.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    rtt_end_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"),
                    rtt_start_month =  col_date(format="%Y-%m-%d"),
                    rtt_end_month =  col_date(format="%Y-%m-%d")))
                       

############## Plot start/end dates by month #################

rtt_dates("clockstops", "full")

############### Waiting time distribution #################

wait("clockstops", "full")

# Stratified by demographics
wait_by_group <- rbind(
    wait_gp(age_group, "Age group"),
    wait_gp(sex, "Sex"),
    wait_gp(ethnicity6, "Ethnicity"),
    wait_gp(imd10, "IMD decile"),
    wait_gp(region, "Region")
  ) %>%
  mutate(source = "clockstops", cohort = "full") 

write.csv(wait_by_group, here::here("output", "clockstops", "wait_by_group_full.csv"),
          row.names = FALSE) 


############ Categorical variable relative frequency distributions #############

categorical_dist <- rbind(
    cat_dist(waiting_list_type, "Waiting list type"),
    cat_dist(treatment_function, "Treatment function"),
    cat_dist(priority_type, "Priority type"),
    cat_dist(censor_before_rtt_end, "Censor before WL end"),
    cat_dist(censor_before_study_end, "Censor before study end"),
    cat_dist(admitted, "Admitted"),
    cat_dist(rtt_multiple, "Multiple RTT pathways"),

    cat_dist(age_group, "Age group"),
    cat_dist(sex, "Sex"),
    cat_dist(imd10, "IMD decile"),
    cat_dist(ethnicity6, "Ethnicity (6 groups)"),
    cat_dist(ethnicity16, "Ethnicity (16 groups)"),
    cat_dist(region, "Region"),
    
    cat_dist(cancer, "Cancer"),
    cat_dist(diabetes, "Diabetes"),
    cat_dist(cardiac, "Cardiac"),
    cat_dist(copd, "COPD"),
    cat_dist(liver, "Liver"),
    cat_dist(ckd, "CKD"),
    cat_dist(osteoarthritis, "Osteoarthritis"),
    cat_dist(depress_or_gad, "Depression/GAD"),
    cat_dist(ra, "Rheumatoid arthritis"),
    
    cat_dist(died_during_wl, "Died while on WL"),
    cat_dist(died_during_post, "Died during post-WL follow-up"),
    
    cat_dist(any_opioid_pre, "Any opioid (pre-WL)"),
    cat_dist(any_opioid_wait, "Any opioid (during WL)"),
    cat_dist(any_opioid_post, "Any opioid (post-WL)"),
    
    cat_dist(hi_opioid_pre, "High dose opioid (pre-WL)"),
    cat_dist(hi_opioid_wait, "High dose opioid (during WL)"),
    cat_dist(hi_opioid_post, "High dose opioid (post-WL)"),
    
    cat_dist(gabapentinoid_pre, "Gabapentinoid (pre-WL)"),
    cat_dist(gabapentinoid_wait, "Gabapentinoid (during WL)"),
    cat_dist(gabapentinoid_post, "Gabapentinoid (post-WL)"),
    
    cat_dist(nsaid_pre, "NSAID (pre-WL)"),
    cat_dist(nsaid_wait, "NSAID (during WL)"),
    cat_dist(nsaid_post, "NSAID (post-WL)"),
    
    cat_dist(antidepressant_pre, "Antidepressant (pre-WL)"),
    cat_dist(antidepressant_wait, "Antidepressant (during WL)"),
    cat_dist(antidepressant_post, "Antidepressant (post-WL)")
  ) %>%
  mutate(source = "clockstops", cohort = "full") 
  
write.csv(categorical_dist, here::here("output", "clockstops", "cat_var_dist_full.csv"),
          row.names = FALSE)

################## Medicine Rx count variables ####################

# Count total number of Rx, total person-days, and p25/median/75
#   for each period and each medicine group
med_count <- rbind(
    summ(pre_time, opioid_pre_count, "Any opioid", "Pre-WL"),
    summ(wait_time_adj, opioid_wait_count, "Any opioid", "During WL"),
    summ(post_time_adj, opioid_post_count, "Any opioid", "Post-WL"),
    
    summ(pre_time, hi_opioid_pre_count, "High dose opioid", "Pre-WL"),
    summ(wait_time_adj, hi_opioid_wait_count, "High dose opioid", "During WL"),
    summ(post_time_adj, hi_opioid_post_count, "High dose opioid", "Post-WL"),
    
    summ(pre_time, gabapentinoid_pre_count, "Gabapentinoid", "Pre-WL"),
    summ(wait_time_adj, gabapentinoid_wait_count, "Gabapentinoid", "During WL"),
    summ(post_time_adj, gabapentinoid_post_count, "Gabapentinoid", "Post-WL"),
    
    summ(pre_time, nsaid_pre_count, "NSAID", "Pre-WL"),
    summ(wait_time_adj, nsaid_wait_count, "NSAID", "During WL"),
    summ(post_time_adj, nsaid_post_count, "NSAID", "Post-WL"),
    
    summ(pre_time, antidepressant_pre_count, "Antidepressant", "Pre-WL"),
    summ(wait_time_adj, antidepressant_wait_count, "Antidepressant", "During WL"),
    summ(post_time_adj, antidepressant_post_count, "Antidepressant", "Post-WL")
  ) %>%
  mutate(source = "clockstops", cohort = "full") 
  
write.csv(med_count, here::here("output", "clockstops", "med_by_period_full.csv"),
          row.names = FALSE)
