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
dir_create(here::here("output", "openpathways"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "data"), showWarnings = FALSE, recurse = TRUE)


## Load data ##
dat <- read_csv(here::here("output", "data", "cohort_full_openpathways.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"),
                    rtt_start_month =  col_date(format="%Y-%m-%d")))
                       

############## Plot start/end dates by month #################

rtt_dates("openpathways", "full")

############### Waiting time distribution #################

wait("openpathways", "full")

############ Categorical variable relative frequency distributions #############

categorical_dist <- rbind(
    cat_dist(waiting_list_type, "Waiting list type"),
    cat_dist(treatment_function, "Treatment function"),
    cat_dist(priority_type, "Priority type"),
    cat_dist(censor_before_study_end, "Censor before study end"),
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
    
    cat_dist(died_during_wl, "Died while on WL")
  ) %>%
  mutate(source = "openpathways", cohort = "full") 
  
write.csv(categorical_dist, here::here("output", "openpathways", "cat_var_dist_full.csv"),
          row.names = FALSE)
