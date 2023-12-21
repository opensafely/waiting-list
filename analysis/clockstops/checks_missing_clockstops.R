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
library(stringr)

## Rounding function
source(here("analysis", "custom_functions.R"))

## Create directories if needed
dir_create(here::here("output", "clockstops"), showWarnings = FALSE, recurse = TRUE)


# Check number missing end dates 
missing <- read_csv(here::here("output", "data", "dataset_clockstops_missing.csv.gz")) %>%
  summarise(count_null = sum(count_null_ref_id),
            count_not_null = sum(count_not_null_ref_id))

write.csv(missing, here::here("output", "clockstops", "check_missing_dates.csv"), row.names = FALSE)

