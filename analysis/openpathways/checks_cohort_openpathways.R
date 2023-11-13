###############################################################
# This script checks for incorrect values in dates
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
dir_create(here::here("output", "measures"), showWarnings = FALSE, recurse = TRUE)


## Load data ##
openpathways <- read_csv(here::here("output", "data", "dataset_openpathways.csv.gz"),
  col_types = cols(rtt_start_date = col_date(format="%Y-%m-%d"),
                    reg_end_date = col_date(format="%Y-%m-%d"),
                    dod = col_date(format="%Y-%m-%d"),
                    end_date = col_date(format="%Y-%m-%d"))) %>%
  mutate(rtt_end_date = as.Date("2022-05-01"))

#### Check dates for problems ##

check_dates <- openpathways %>%
  mutate(# These should all be 0 
         rtt_end_before_start = ifelse(rtt_end_date < rtt_start_date, 1, 0),
         rtt_end_after_dod = ifelse(!is.na(dod) & (rtt_end_date > dod), 1, 0),
         dereg_before_rtt_start = ifelse(!is.na(reg_end_date) & (reg_end_date < rtt_start_date), 1, 0),
         rtt_start_missing = ifelse(is.na(rtt_start_date), 1, 0),
         end_before_start = ifelse(end_date < rtt_start_date, 1, 0)
         ) %>%
  summarise(rtt_end_before_start = sum(rtt_end_before_start),
            rtt_end_after_dod = sum(rtt_end_after_dod),
            dereg_before_rtt_start = sum(dereg_before_rtt_start),
            rtt_start_missing = sum(rtt_start_missing),
            end_before_start = sum(end_before_start),
            # Check max wait time for developing code
            max_wait_time = max(wait_time),
            )

write.csv(check_dates, here::here("output", "openpathways", "check_dates.csv"),
          row.names = FALSE)


# Check time between death/ RTT end date
dod_end_time <- openpathways %>%
  subset(!is.na(dod) & (rtt_end_date > dod)) %>%
  mutate(time_dod_end = as.numeric(rtt_end_date - dod),
         time_dod_end_gp = ifelse(time_dod_end >= 30, 30, time_dod_end)) %>%
  group_by(time_dod_end_gp) %>%
  summarise(count = n()) 

write.csv(dod_end_time, here::here("output", "openpathways", "check_end_dod_time.csv"),
          row.names = FALSE)


# Number of rows/pathways/etc.
num_per_person <- openpathways %>%
  mutate(count_rtt_rows = ifelse(count_rtt_rows >= 10, 10, count_rtt_rows),
         count_rtt_start_date = ifelse(count_rtt_start_date >= 10, 10, count_rtt_start_date),
         count_patient_id = ifelse(count_patient_id >= 10, 10, count_patient_id),
         count_organisation_id = ifelse(count_organisation_id >= 10, 10, count_organisation_id),
         count_referral_id = ifelse(count_rtt_rows >= 10, 10, count_referral_id)) 

group_summ <- function(variable, name){
  num_per_person %>%
    mutate(total_count = sum({{variable}})) %>%
    group_by({{variable}}, total_count) %>%
    summarise(count = n()) %>%
    mutate(var = name) %>%
    rename(num_per_person = {{variable}})
}

all <- rbind(
  group_summ(count_rtt_rows, "RTT rows"),
  group_summ(count_rtt_start_date, "RTT start dates"),
  group_summ(count_patient_id, "Patient IDs"),
  group_summ(count_organisation_id, "Organisation IDs"),
  group_summ(count_referral_id, "Referral IDs")
)

write.csv(all,  here::here("output", "openpathways", "check_num_per_person.csv"), row.names = FALSE)

