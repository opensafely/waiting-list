###############################################################
# This script creates summary statistics for all people
# with/without an admission
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


## Load data - orthopaedic routine only ##
ortho_routine_final <- read_csv(here::here("output", "data", "cohort_ortho_routine_clockstops.csv.gz"),
                                col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                                                 rtt_end_date = col_date(format="%Y-%m-%d"),
                                                 reg_end_date = col_date(format="%Y-%m-%d"),
                                                 dod = col_date(format="%Y-%m-%d"),
                                                 end_date = col_date(format="%Y-%m-%d"),
                                                 rtt_start_month =  col_date(format="%Y-%m-%d"),
                                                 rtt_end_month =  col_date(format="%Y-%m-%d"))) 


# No admission only
dat <- ortho_routine_final %>%
  subset(any_admission == FALSE)
      
no_admit <- cat_dist_combined() %>%
              rbind(cat_dist(waiting_list_type, "WL type"),
                    cat_dist(wait_gp, "Wait group")) %>%
              rename(count_noadmit = count, total_noadmit = total)

# Admission only
dat <- ortho_routine_final %>%
  subset(any_admission == TRUE)

admit <- cat_dist_combined() %>%
  rbind(cat_dist(waiting_list_type, "WL type"),
        cat_dist(wait_gp, "Wait group")) %>%
  rename(count_admit = count, total_admit = total)


# Merge 
cat_dist <- list(no_admit, admit) %>% 
  reduce(full_join, by=c("category","var","cohort")) %>%
  filter(category != FALSE) %>%
  arrange(var, category) 

cat_dist <- cat_dist[,c("cohort", "var", "category", 
                        "count_noadmit", "total_noadmit", 
                        "count_admit", "total_admit")]

write.csv(cat_dist, here::here("output", "clockstops",  "check_no_admission.csv"),
          row.names = FALSE) 

  