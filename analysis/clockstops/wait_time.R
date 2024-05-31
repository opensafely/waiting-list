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
library('readr')

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
                    rtt_end_month =  col_date(format="%Y-%m-%d"))) 
  

############# Plot start/end dates by month #################

rtt_month <- ortho_routine_final %>%
  mutate(month = pmax(rtt_start_month, as.Date("2020-01-01")),
         type = "RTT start date") %>%
  group_by(month, type) %>%
  summarise(count = n()) %>%
  bind_rows(
    ortho_routine_final %>%
      rename(month = rtt_end_month) %>%
      mutate(type = "RTT end date") %>%
      group_by(month, type) %>%
      summarise(count = n())
  ) %>%
  mutate(count = rounding(count), cohort = "Orthopaedic - Routine/Admitted") 

write.csv(rtt_month, here::here("output", "clockstops", "rtt_dates.csv"),
          row.names = FALSE)


############ Wait time stratified by demographics #########################

dat <- ortho_routine_final %>%
  mutate(full = "Full cohort")

wait_by_group <- rbind(
  wait_gp(full, "Full cohort"),
  wait_gp(age_group, "Age group"),
  wait_gp(sex, "Sex"),
  wait_gp(imd10, "IMD decile"),
  wait_gp(ethnicity6, "Ethnicity"),
  wait_gp(region, "Region"),
  wait_gp(prior_opioid_rx, "Prior opioid Rx"),
  wait_gp(long_term_opioid, "Long-term opioid"),
  wait_gp(oa, "OA diagnosis"),
  wait_gp(hip_hrg, "Hip procedure"),
  wait_gp(knee_hrg, "Knee procedure")
  ) %>% 
  arrange(var, category) %>%
  subset(!(var == "Region" & is.na(category)))

wait_by_group <- wait_by_group[,c( "cohort", "var", "category",
                                   "wait_gp1", "wait_gp2", "wait_gp3", "total",
                                  "p25", "p50", "p75")]

write.csv(wait_by_group, here::here("output", "clockstops", "wait_by_group.csv"),
  row.names = FALSE) 



########### Check min/max for errors ########################################

# By week
wait_time <- ortho_routine_final %>%
  mutate(total = n()) %>%
  group_by(num_weeks, any_admission, total) %>%
  summarise(count = n()) %>%
  arrange(num_weeks, any_admission, count, total)

write.csv(wait_time, here::here("output", "clockstops", "num_weeks.csv"),
          row.names = FALSE) 

